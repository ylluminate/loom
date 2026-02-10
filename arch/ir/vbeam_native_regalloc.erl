-module(vbeam_native_regalloc).
-compile({no_auto_import, [min/2]}).

%% Linear scan register allocator for vbeam native backend.
%%
%% Algorithm:
%% 1. Number all instructions sequentially
%% 2. Compute live intervals for each virtual register
%% 3. Sort intervals by start position
%% 4. Walk intervals; allocate physical registers greedily
%% 5. On conflict: spill the longest-lived interval to stack
%% 6. Rewrite IR: replace vreg with preg or stack operands
%% 7. Insert loads/stores for spilled registers

-export([
    allocate/3,
    allocate/4,
    compute_live_intervals/1,
    rewrite_instructions/2,
    used_callee_saved/2,
    needs_heap/1,
    available_regs_no_heap/1
]).

-type target() :: x86_64 | arm64.

-record(interval, {
    vreg :: non_neg_integer(),
    start :: non_neg_integer(),
    stop :: non_neg_integer(),
    preg :: atom() | undefined,
    spill_slot :: integer() | undefined
}).

-record(alloc_state, {
    active :: [#interval{}],
    free_regs :: [atom()],
    used_regs :: [atom()],
    next_spill :: non_neg_integer(),
    assignments :: #{non_neg_integer() => atom() | {stack, integer()}}
}).

%% Main entry point: allocate physical registers for a function's IR body.
%% Returns rewritten body with vreg replaced by preg/stack,
%% plus the number of spill slots used (for stack frame sizing).
-spec allocate([term()], target(), non_neg_integer()) ->
    {[term()], non_neg_integer()}.
allocate(Body, Target, NumParams) ->
    allocate(Body, Target, NumParams, #{}).

-spec allocate([term()], target(), non_neg_integer(), map()) ->
    {[term()], non_neg_integer()}.
allocate(Body, Target, NumParams, Opts) ->
    Intervals0 = compute_live_intervals(Body),
    %% Ensure parameter vregs have live intervals starting at 0.
    %% Parameters are live from function entry even if first explicit use is later.
    ParamAssign0 = param_assignments(Target, NumParams),
    Intervals = extend_param_intervals(Intervals0, ParamAssign0),

    %% Detect call positions and vregs that span calls
    CallPositions = find_call_positions(Body),
    CallerSaved = caller_saved_regs(Target),
    CalleeSaved = callee_saved_regs(Target),

    %% Check which parameter vregs span calls.  If a parameter is live across
    %% a call it must be moved from its caller-saved arg register to a
    %% callee-saved register.  We reassign those parameters upfront.
    ParamAssign = reassign_params_for_calls(
        ParamAssign0, Intervals, CallPositions, CallerSaved, CalleeSaved),

    Sorted = lists:sort(fun(A, B) -> A#interval.start =< B#interval.start end, Intervals),

    %% Build the free pool, giving callee-saved priority to intervals
    %% that span calls.
    AllRegs0 = available_regs_call_aware(Target, Intervals, CallPositions),
    %% If heap allocation is needed, exclude the heap register (x28/r15)
    HeapReg = vbeam_native_alloc:heap_reg(Target),
    AllRegs1 = case maps:get(reserve_heap, Opts, false) of
        true -> [R || R <- AllRegs0, R =/= HeapReg];
        false -> AllRegs0
    end,
    %% If complex pseudo-ops are present (string_concat, string_cmp, print_str,
    %% print_int, array_append, etc.), exclude scratch registers they clobber
    %% internally. These instructions expand to long sequences that use x9-x15
    %% (ARM64) as temporaries. Without this exclusion, a vreg assigned to x9
    %% would be silently destroyed by the expanded code.
    ScratchRegs = scratch_regs_for_pseudo_ops(Target),
    AllRegs = case needs_scratch_exclusion(Body) of
        true -> [R || R <- AllRegs1, not lists:member(R, ScratchRegs)];
        false -> AllRegs1
    end,

    %% Remove pre-assigned parameter registers from the free pool
    ParamRegs = [Reg || {_, Reg} <- maps:to_list(ParamAssign)],
    FreeRegs = [R || R <- AllRegs, not lists:member(R, ParamRegs)],
    %% Add parameter intervals as active (they're already assigned)
    ParamActive = [#interval{vreg = V, start = 0,
                             stop = param_interval_end(V, Intervals),
                             preg = Reg}
                   || {V, Reg} <- maps:to_list(ParamAssign)],
    SortedActive = lists:sort(
        fun(A, B) -> A#interval.stop =< B#interval.stop end, ParamActive),
    %% Remove param vregs from the intervals to scan (they're pre-assigned)
    NonParamIntervals = [I || I <- Sorted,
                              not maps:is_key(I#interval.vreg, ParamAssign)],
    State0 = #alloc_state{
        active = SortedActive,
        free_regs = FreeRegs,
        used_regs = ParamRegs,
        next_spill = 0,
        assignments = ParamAssign
    },
    State1 = linear_scan(NonParamIntervals, State0),
    RewrittenBody0 = rewrite_instructions(Body, State1#alloc_state.assignments),
    %% Insert parameter copies where params were reassigned to callee-saved regs
    RewrittenBody = insert_param_copies(RewrittenBody0, ParamAssign0, ParamAssign),
    {RewrittenBody, State1#alloc_state.next_spill}.

%% Return the list of callee-saved registers used by the function body.
%% The lowering pass uses this to emit save/restore in prologue/epilogue.
-spec used_callee_saved([term()], target()) -> [atom()].
used_callee_saved(Body, Target) ->
    Callee = callee_saved_regs(Target),
    Used = lists:foldl(fun(Inst, Acc) ->
        Regs = extract_pregs(Inst),
        lists:foldl(fun(R, A) ->
            case lists:member(R, Callee) andalso not lists:member(R, A) of
                true -> [R | A];
                false -> A
            end
        end, Acc, Regs)
    end, [], Body),
    %% Return in register order for consistent save/restore
    [R || R <- Callee, lists:member(R, Used)].

%% Extract all physical register atoms from an instruction.
extract_pregs(Inst) when is_tuple(Inst) ->
    extract_pregs_from_list(tuple_to_list(Inst));
extract_pregs(_) -> [].

extract_pregs_from_list([]) -> [];
extract_pregs_from_list([{preg, R} | Rest]) ->
    [R | extract_pregs_from_list(Rest)];
extract_pregs_from_list([_ | Rest]) ->
    extract_pregs_from_list(Rest).

%% Compute live intervals for each virtual register in the instruction list.
-spec compute_live_intervals([term()]) -> [#interval{}].
compute_live_intervals(Body) ->
    Numbered = lists:zip(lists:seq(0, length(Body) - 1), Body),
    %% Collect first-use and last-use for each vreg
    Uses = lists:foldl(fun({Pos, Inst}, Acc) ->
        Vregs = extract_vregs(Inst),
        lists:foldl(fun(V, A) ->
            case maps:find(V, A) of
                {ok, {Start, _}} -> maps:put(V, {Start, Pos}, A);
                error -> maps:put(V, {Pos, Pos}, A)
            end
        end, Acc, Vregs)
    end, #{}, Numbered),
    [#interval{vreg = V, start = S, stop = E}
     || {V, {S, E}} <- maps:to_list(Uses)].

%% Extend parameter vreg intervals to start at 0.
%% Parameters are live from function entry even before first explicit use.
extend_param_intervals(Intervals, ParamAssign) ->
    [case maps:is_key(I#interval.vreg, ParamAssign) of
         true -> I#interval{start = 0};
         false -> I
     end || I <- Intervals].

%% Find the end of a parameter vreg's live interval.
param_interval_end(Vreg, Intervals) ->
    case [I#interval.stop || I <- Intervals, I#interval.vreg =:= Vreg] of
        [Stop] -> Stop;
        [] -> 0  %% unused parameter
    end.

%% Extract all virtual register numbers from an instruction.
extract_vregs(Inst) when is_tuple(Inst) ->
    extract_vregs_from_list(tuple_to_list(Inst));
extract_vregs(ret) -> [];
extract_vregs(syscall) -> [];
extract_vregs(nop) -> [];
extract_vregs(_) -> [].

extract_vregs_from_list([]) -> [];
extract_vregs_from_list([{vreg, N} | Rest]) ->
    [N | extract_vregs_from_list(Rest)];
extract_vregs_from_list([{mem, {vreg, N}, _} | Rest]) ->
    [N | extract_vregs_from_list(Rest)];
extract_vregs_from_list([L | Rest]) when is_list(L) ->
    extract_vregs_from_list(L) ++ extract_vregs_from_list(Rest);
extract_vregs_from_list([_ | Rest]) ->
    extract_vregs_from_list(Rest).

%% Linear scan allocation core.
linear_scan([], State) -> State;
linear_scan([Interval | Rest], State) ->
    %% Expire old intervals
    State1 = expire_old(Interval#interval.start, State),
    case State1#alloc_state.free_regs of
        [] ->
            %% No free registers — spill
            State2 = spill_at_interval(Interval, State1),
            linear_scan(Rest, State2);
        [Reg | FreeRest] ->
            %% Assign a free register
            Assigned = Interval#interval{preg = Reg},
            Active2 = insert_active(Assigned, State1#alloc_state.active),
            Assignments2 = maps:put(Interval#interval.vreg, Reg,
                                    State1#alloc_state.assignments),
            State2 = State1#alloc_state{
                active = Active2,
                free_regs = FreeRest,
                used_regs = [Reg | State1#alloc_state.used_regs],
                assignments = Assignments2
            },
            linear_scan(Rest, State2)
    end.

%% Remove intervals that have expired (stop < current position).
expire_old(Pos, State = #alloc_state{active = Active, free_regs = Free}) ->
    {Expired, Remaining} = lists:partition(
        fun(I) -> I#interval.stop < Pos end, Active),
    FreedRegs = [I#interval.preg || I <- Expired, I#interval.preg =/= undefined],
    State#alloc_state{
        active = Remaining,
        free_regs = Free ++ FreedRegs
    }.

%% Spill: if current interval is longer than the longest active, spill active.
%% Otherwise spill the current interval.
spill_at_interval(Interval, State = #alloc_state{active = Active}) ->
    case Active of
        [] ->
            %% No active intervals to spill, spill current
            do_spill_current(Interval, State);
        _ ->
            %% Find the active interval with the latest end point
            Longest = lists:foldl(
                fun(I, Best) ->
                    if I#interval.stop > Best#interval.stop -> I;
                       true -> Best
                    end
                end, hd(Active), tl(Active)),
            if Longest#interval.stop > Interval#interval.stop ->
                %% Spill the longest active, give its register to current
                Reg = Longest#interval.preg,
                Slot = State#alloc_state.next_spill,
                Active2 = lists:delete(Longest, Active),
                Assigned = Interval#interval{preg = Reg},
                Active3 = insert_active(Assigned, Active2),
                Assignments = maps:put(Longest#interval.vreg, {stack, Slot},
                              maps:put(Interval#interval.vreg, Reg,
                                       State#alloc_state.assignments)),
                State#alloc_state{
                    active = Active3,
                    next_spill = Slot + 1,
                    assignments = Assignments
                };
               true ->
                do_spill_current(Interval, State)
            end
    end.

do_spill_current(Interval, State) ->
    Slot = State#alloc_state.next_spill,
    Assignments = maps:put(Interval#interval.vreg, {stack, Slot},
                           State#alloc_state.assignments),
    State#alloc_state{
        next_spill = Slot + 1,
        assignments = Assignments
    }.

%% Insert into active list, sorted by end point.
insert_active(Interval, []) -> [Interval];
insert_active(Interval, [H | T]) ->
    if Interval#interval.stop =< H#interval.stop ->
        [Interval, H | T];
       true ->
        [H | insert_active(Interval, T)]
    end.

%% ============================================================================
%% Call-aware register allocation helpers
%% ============================================================================

%% Find the instruction positions that are calls (clobber all caller-saved regs).
-spec find_call_positions([term()]) -> [non_neg_integer()].
find_call_positions(Body) ->
    Numbered = lists:zip(lists:seq(0, length(Body) - 1), Body),
    [Pos || {Pos, Inst} <- Numbered, is_call_instruction(Inst)].

%% Detect call-like instructions that clobber caller-saved registers.
is_call_instruction({call, _}) -> true;
is_call_instruction({call_indirect, _}) -> true;
is_call_instruction(_) -> false.

%% Check if a vreg's live interval spans any call position.
-spec spans_call(#interval{}, [non_neg_integer()]) -> boolean().
spans_call(#interval{start = Start, stop = Stop}, CallPositions) ->
    lists:any(fun(Pos) -> Pos >= Start andalso Pos =< Stop end, CallPositions).

%% Callee-saved and caller-saved register sets.
callee_saved_regs(arm64) ->
    [x19, x20, x21, x22, x23, x24, x25, x26, x27, x28];
callee_saved_regs(x86_64) ->
    [rbx, r12, r13, r14, r15].

caller_saved_regs(arm64) ->
    [x0, x1, x2, x3, x4, x5, x6, x7,
     x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18];
caller_saved_regs(x86_64) ->
    [rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11].

%% Build available register list with callee-saved first.
%% When vregs span calls, callee-saved regs should be preferred so the
%% linear scan picks them up first for call-spanning intervals.
%% When heap opcodes are present, exclude the heap register (x28/r15).
available_regs_call_aware(Target, Intervals, CallPositions) ->
    HasCallSpanners = lists:any(
        fun(I) -> spans_call(I, CallPositions) end, Intervals),
    BaseRegs = case HasCallSpanners of
        true ->
            %% Put callee-saved FIRST so call-spanning vregs get them
            callee_saved_regs(Target) ++ caller_saved_regs(Target);
        false ->
            %% No calls to worry about — use original order
            available_regs(Target)
    end,
    %% Heap reg reservation is handled by the allocate/3 caller
    BaseRegs.

%% Reassign parameter vregs from caller-saved arg regs to callee-saved regs
%% when the parameter is live across a call.
reassign_params_for_calls(ParamAssign, Intervals, CallPositions,
                          CallerSaved, CalleeSaved) ->
    %% Find which params span calls
    SpanningParams = maps:filter(
        fun(Vreg, Reg) ->
            lists:member(Reg, CallerSaved) andalso
            case [I || I <- Intervals, I#interval.vreg =:= Vreg] of
                [Iv] -> spans_call(Iv, CallPositions);
                _ -> false
            end
        end, ParamAssign),
    case maps:size(SpanningParams) of
        0 -> ParamAssign;
        _ ->
            %% Assign callee-saved regs to spanning params
            %% Collect used callee-saved regs
            UsedCallee = [R || {_, R} <- maps:to_list(ParamAssign),
                              lists:member(R, CalleeSaved)],
            FreeCallee = [R || R <- CalleeSaved,
                              not lists:member(R, UsedCallee)],
            reassign_params(maps:to_list(SpanningParams), FreeCallee, ParamAssign)
    end.

reassign_params([], _FreeCallee, Assign) -> Assign;
reassign_params([{Vreg, _OldReg} | Rest], [NewReg | FreeRest], Assign) ->
    reassign_params(Rest, FreeRest, maps:put(Vreg, NewReg, Assign));
reassign_params([{Vreg, _OldReg} | Rest], [], Assign) ->
    %% No more callee-saved regs — remove the caller-saved assignment
    %% so linear scan will handle it (likely spilling to stack)
    reassign_params(Rest, [], maps:remove(Vreg, Assign)).

%% Insert MOV instructions at function entry to copy parameter values
%% from their original arg registers to their reassigned callee-saved registers.
insert_param_copies(Body, OrigAssign, NewAssign) ->
    Copies = maps:fold(
        fun(Vreg, OrigReg, Acc) ->
            case maps:find(Vreg, NewAssign) of
                {ok, NewReg} when NewReg =/= OrigReg ->
                    [{mov, {preg, NewReg}, {preg, OrigReg}} | Acc];
                _ -> Acc
            end
        end, [], OrigAssign),
    case Copies of
        [] -> Body;
        _ -> Copies ++ Body
    end.

%% Available registers for allocation (excluding reserved ones).
%% Note: x28 (ARM64) and r15 (x86_64) are reserved for the heap pointer
%% when heap allocation opcodes are present. The needs_heap flag is
%% checked by the caller (allocate/3 passes the full body).
available_regs(x86_64) ->
    %% Exclude rsp (stack pointer), rbp (frame pointer)
    %% Put callee-saved last (prefer caller-saved to minimize saves)
    [rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11,
     rbx, r12, r13, r14, r15];
available_regs(arm64) ->
    %% Exclude x29 (frame pointer), x30 (link register), sp (stack pointer)
    %% Exclude x16, x17 (scratch/IP0/IP1 - used by lowering pseudo-ops)
    %% Exclude x18 (platform register on macOS)
    %% Put callee-saved last
    [x0, x1, x2, x3, x4, x5, x6, x7,
     x8, x9, x10, x11, x12, x13, x14, x15,
     x19, x20, x21, x22, x23, x24, x25, x26, x27, x28].

%% Available registers excluding the heap pointer register.
available_regs_no_heap(x86_64) ->
    [R || R <- available_regs(x86_64), R =/= r15];
available_regs_no_heap(arm64) ->
    [R || R <- available_regs(arm64), R =/= x28].

%% Check if the IR body uses heap-allocating opcodes.
-spec needs_heap([term()]) -> boolean().
needs_heap([]) -> false;
needs_heap([Inst | Rest]) ->
    case is_heap_instruction(Inst) of
        true -> true;
        false -> needs_heap(Rest)
    end.

is_heap_instruction({string_lit, _, _, _}) -> true;
is_heap_instruction({string_concat, _, _, _}) -> true;
is_heap_instruction({array_new, _, _, _}) -> true;
is_heap_instruction({array_append, _, _, _}) -> true;
is_heap_instruction({array_append, _, _, _, _}) -> true;
is_heap_instruction({struct_new, _, _}) -> true;
is_heap_instruction({map_new, _}) -> true;
is_heap_instruction({map_put, _, _, _, _}) -> true;
is_heap_instruction({int_to_str, _, _}) -> true;
is_heap_instruction({float_to_str, _, _}) -> true;
is_heap_instruction(_) -> false.

%% Check if body contains pseudo-ops that expand to code using scratch registers.
needs_scratch_exclusion([]) -> false;
needs_scratch_exclusion([Inst | Rest]) ->
    case is_scratch_instruction(Inst) of
        true -> true;
        false -> needs_scratch_exclusion(Rest)
    end.

is_scratch_instruction({print_int, _}) -> true;
is_scratch_instruction({print_str, _}) -> true;
is_scratch_instruction({string_lit, _, _, _}) -> true;
is_scratch_instruction({string_lit, _, _}) -> true;
is_scratch_instruction({string_cmp, _, _, _}) -> true;
is_scratch_instruction({string_concat, _, _, _}) -> true;
is_scratch_instruction({array_new, _, _, _}) -> true;
is_scratch_instruction({array_get, _, _, _, _}) -> true;
is_scratch_instruction({array_set, _, _, _, _}) -> true;
is_scratch_instruction({array_append, _, _, _, _}) -> true;
is_scratch_instruction({array_append, _, _, _}) -> true;
is_scratch_instruction({map_new, _}) -> true;
is_scratch_instruction({map_get, _, _, _}) -> true;
is_scratch_instruction({map_put, _, _, _, _}) -> true;
is_scratch_instruction({map_delete, _, _, _}) -> true;
is_scratch_instruction({int_to_float, _, _}) -> true;
is_scratch_instruction({float_to_int, _, _}) -> true;
is_scratch_instruction({int_to_str, _, _}) -> true;
is_scratch_instruction({float_to_str, _, _}) -> true;
is_scratch_instruction({method_call, _, _, _, _}) -> true;
is_scratch_instruction(_) -> false.

%% Scratch registers used by pseudo-ops during lowering.
%% These must NOT be allocated to vregs when scratch instructions are present.
scratch_regs_for_pseudo_ops(arm64) ->
    [x9, x10, x11, x12, x13, x14, x15];
scratch_regs_for_pseudo_ops(x86_64) ->
    %% x86_64 pseudo-ops may use rax/rcx/rdx as scratch
    [r10, r11].

%% Pre-assign parameter registers.
param_assignments(x86_64, NumParams) ->
    ArgRegs = [rdi, rsi, rdx, rcx, r8, r9],
    maps:from_list([{I, lists:nth(I + 1, ArgRegs)}
                    || I <- lists:seq(0, min(NumParams, 6) - 1)]);
param_assignments(arm64, NumParams) ->
    ArgRegs = [x0, x1, x2, x3, x4, x5, x6, x7],
    maps:from_list([{I, lists:nth(I + 1, ArgRegs)}
                    || I <- lists:seq(0, min(NumParams, 8) - 1)]).

%% Rewrite instructions: replace {vreg, N} with assigned physical reg or stack.
-spec rewrite_instructions([term()], #{non_neg_integer() => atom() | {stack, integer()}}) ->
    [term()].
rewrite_instructions(Body, Assignments) ->
    [rewrite_inst(I, Assignments) || I <- Body].

rewrite_inst(Inst, Assignments) when is_tuple(Inst) ->
    list_to_tuple([rewrite_operand(E, Assignments) || E <- tuple_to_list(Inst)]);
rewrite_inst(Inst, _) -> Inst.  % atoms like ret, syscall, nop

rewrite_operand({vreg, N}, Assignments) ->
    case maps:find(N, Assignments) of
        {ok, {stack, Slot}} -> {stack, Slot};
        {ok, Reg} -> {preg, Reg};
        error -> {vreg, N}  % unassigned (dead code?)
    end;
rewrite_operand({mem, {vreg, N}, Off}, Assignments) ->
    case maps:find(N, Assignments) of
        {ok, {stack, Slot}} -> {mem, {stack, Slot}, Off};
        {ok, Reg} -> {mem, {preg, Reg}, Off};
        error -> {mem, {vreg, N}, Off}
    end;
rewrite_operand(List, Assignments) when is_list(List) ->
    [rewrite_operand(E, Assignments) || E <- List];
rewrite_operand(Other, _) -> Other.

min(A, B) when A < B -> A;
min(_, B) -> B.
