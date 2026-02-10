-module(vbeam_native_alloc).

%% Bump allocator helper routines for vbeam native code generation.
%%
%% Provides runtime allocation for strings, arrays, structs, and maps.
%% Uses a simple bump allocator: a heap pointer is maintained in a
%% reserved callee-saved register, advanced on each allocation.
%%
%% Memory layout:
%%   - Heap base is obtained via mmap at program start
%%   - Heap pointer advances monotonically (bump allocation)
%%   - No free/GC in this version (programs are short-lived)
%%
%% ARM64:
%%   x28 = heap pointer (callee-saved, survives calls)
%%   Init via mmap syscall (197 macOS, 222 Linux)
%%
%% x86_64:
%%   r15 = heap pointer (callee-saved, survives calls)
%%   Init via mmap syscall (9)

-export([
    emit_alloc_init/1,
    emit_alloc_init/2,
    emit_alloc/3,
    emit_alloc_reg/3,
    heap_reg/1
]).

-define(ARM64_ENC, vbeam_native_arm64).
-define(X86_ENC, vbeam_native_x86_64).

%% Default heap size: 1MB
-define(HEAP_SIZE, (1024 * 1024)).

%% @doc Return the physical register used as the heap pointer for a target.
-spec heap_reg(atom()) -> atom().
heap_reg(arm64)  -> x28;
heap_reg(x86_64) -> r15.

%% @doc Emit code to initialize the bump allocator (auto-detect format).
-spec emit_alloc_init(atom()) -> [term()].
emit_alloc_init(arm64)  -> emit_alloc_init(arm64, macho);
emit_alloc_init(x86_64) -> emit_alloc_init(x86_64, elf64).

%% @doc Emit code to initialize the bump allocator with explicit format.
%% Must be called once at program start (before any alloc calls).
%% Returns a list of code parts (binaries).
-spec emit_alloc_init(atom(), atom()) -> [term()].
emit_alloc_init(arm64, Format) ->
    %% mmap syscall args:
    %%   x0 = addr (NULL = 0)
    %%   x1 = length (1MB)
    %%   x2 = prot (PROT_READ|PROT_WRITE = 3)
    %%   x3 = flags (MAP_ANON|MAP_PRIVATE)
    %%   x4 = fd (-1)
    %%   x5 = offset (0)
    {SysNumReg, MmapNum, SvcImm, Flags} = case Format of
        macho -> {x16, 197, 16#80, 16#1002};
        _     -> {x8, 222, 0, 16#22}
    end,
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    OkLbl = <<"__alloc_init_ok_", Uid/binary>>,
    FailLbl = <<"__alloc_init_fail_", Uid/binary>>,
    lists:flatten([
        ?ARM64_ENC:encode_mov_imm64(x0, 0),
        ?ARM64_ENC:encode_mov_imm64(x1, ?HEAP_SIZE),
        ?ARM64_ENC:encode_mov_imm64(x2, 3),
        ?ARM64_ENC:encode_mov_imm64(x3, Flags),
        ?ARM64_ENC:encode_mov_imm64(x4, -1),
        ?ARM64_ENC:encode_mov_imm64(x5, 0),
        ?ARM64_ENC:encode_mov_imm64(SysNumReg, MmapNum),
        ?ARM64_ENC:encode_svc(SvcImm),
        %% Check for mmap failure: result < 0 or result > -4096 (error sentinel range)
        ?ARM64_ENC:encode_cmp_imm(x0, 0),
        ?ARM64_ENC:encode_b_cond(ge, 0),
        {reloc, arm64_cond_branch19, OkLbl, -4},
        %% Failure path: mmap returned error
        {label, FailLbl},
        ?ARM64_ENC:encode_mov_imm64(x0, 1),            %% exit code 1
        %% sys_exit: macOS=1, Linux=93
        case Format of
            macho -> ?ARM64_ENC:encode_mov_imm64(x16, 1);
            _     -> ?ARM64_ENC:encode_mov_imm64(x8, 93)
        end,
        ?ARM64_ENC:encode_svc(SvcImm),                 %% exit(1)
        %% Success path: store heap base in x28 and heap end in x27
        {label, OkLbl},
        ?ARM64_ENC:encode_mov_rr(x28, x0),             %% x28 = heap_base
        ?ARM64_ENC:encode_mov_rr(x27, x0),
        %% HEAP_SIZE is 1MB, too large for add_imm (max 4095), use movz+add_rrr
        ?ARM64_ENC:encode_mov_imm64(x17, ?HEAP_SIZE),
        ?ARM64_ENC:encode_add_rrr(x27, x27, x17)       %% x27 = heap_end
    ]);
emit_alloc_init(x86_64, _Format) ->
    %% x86_64 Linux mmap syscall:
    %%   rax = 9 (sys_mmap)
    %%   rdi = addr (0), rsi = length, rdx = prot,
    %%   r10 = flags, r8 = fd, r9 = offset
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    OkLbl = <<"__alloc_init_x64_ok_", Uid/binary>>,
    FailLbl = <<"__alloc_init_x64_fail_", Uid/binary>>,
    lists:flatten([
        ?X86_ENC:encode_mov_imm64(rdi, 0),
        ?X86_ENC:encode_mov_imm64(rsi, ?HEAP_SIZE),
        ?X86_ENC:encode_mov_imm64(rdx, 3),
        ?X86_ENC:encode_mov_imm64(r10, 16#22),
        ?X86_ENC:encode_mov_imm64(r8, -1),
        ?X86_ENC:encode_mov_imm64(r9, 0),
        ?X86_ENC:encode_mov_imm64(rax, 9),
        ?X86_ENC:encode_syscall(),
        %% Validate mmap result: negative rax means error
        ?X86_ENC:encode_test_rr(rax, rax),
        ?X86_ENC:encode_jns(0),
        {reloc, rel32, OkLbl, -4},
        %% Failure path
        {label, FailLbl},
        ?X86_ENC:encode_mov_imm64(rdi, 1),     %% exit code 1
        ?X86_ENC:encode_mov_imm64(rax, 60),    %% sys_exit
        ?X86_ENC:encode_syscall(),
        %% Success path: store heap base in r15 and heap end in r14
        {label, OkLbl},
        ?X86_ENC:encode_mov_rr(r15, rax),      %% r15 = heap_base
        ?X86_ENC:encode_mov_rr(r14, rax),
        ?X86_ENC:encode_add_imm(r14, ?HEAP_SIZE) %% r14 = heap_end
    ]).

%% @doc Emit code to allocate N bytes (compile-time constant) from the
%% bump allocator. Returns pointer in DstReg, advances heap pointer.
%% N is rounded up to 8-byte alignment.
%% Includes bounds check to prevent heap overflow.
-spec emit_alloc(atom(), atom(), non_neg_integer()) -> [term()].
emit_alloc(arm64, DstReg, N) ->
    AlignedN = align8(N),
    HeapReg = heap_reg(arm64),
    HeapEndReg = x27,  %% Initialized to heap_base + HEAP_SIZE in emit_alloc_init
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    OkLbl = <<"__alloc_ok_", Uid/binary>>,
    OomLbl = <<"__alloc_oom_", Uid/binary>>,
    %% CRITICAL FIX (Finding 1): Choose temp register distinct from DstReg
    %% to avoid overwriting the new pointer before committing to heap register.
    TmpReg = if DstReg =:= x16 -> x17; true -> x16 end,
    %% Calculate new heap pointer in TmpReg
    AdvanceCode = if AlignedN >= 0, AlignedN < 4096 ->
            ?ARM64_ENC:encode_add_imm(TmpReg, HeapReg, AlignedN);
           true ->
            [?ARM64_ENC:encode_mov_imm64(TmpReg, AlignedN),
             ?ARM64_ENC:encode_add_rrr(TmpReg, HeapReg, TmpReg)]
        end,
    lists:flatten([
        AdvanceCode,
        %% CRITICAL FIX (Finding 10): Add wraparound check before bounds check
        %% If new_ptr < old_ptr, unsigned overflow occurred
        ?ARM64_ENC:encode_cmp_rr(TmpReg, HeapReg),
        ?ARM64_ENC:encode_b_cond(lo, 0),  % unsigned: new < old
        {reloc, arm64_cond_branch19, OomLbl, -4},
        %% Bounds check: new_ptr > heap_end?
        ?ARM64_ENC:encode_cmp_rr(TmpReg, HeapEndReg),
        ?ARM64_ENC:encode_b_cond(ls, 0),  % unsigned: new <= end (FIXED from 'le')
        {reloc, arm64_cond_branch19, OkLbl, -4},
        %% OOM path: trap
        {label, OomLbl},
        ?ARM64_ENC:encode_brk(16#DEAD),
        %% Success path
        {label, OkLbl},
        ?ARM64_ENC:encode_mov_rr(DstReg, HeapReg),     %% return current ptr
        ?ARM64_ENC:encode_mov_rr(HeapReg, TmpReg)      %% advance heap pointer
    ]);
emit_alloc(x86_64, DstReg, N) ->
    AlignedN = align8(N),
    HeapReg = heap_reg(x86_64),
    HeapEndReg = r14,  %% Initialized to heap_base + HEAP_SIZE in emit_alloc_init
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    OkLbl = <<"__alloc_x64_ok_", Uid/binary>>,
    OomLbl = <<"__alloc_x64_oom_", Uid/binary>>,
    lists:flatten([
        ?X86_ENC:encode_mov_rr(DstReg, HeapReg),       %% return current ptr
        ?X86_ENC:encode_add_imm(HeapReg, AlignedN),    %% advance heap pointer
        %% CRITICAL FIX (Finding 10): Add wraparound check before bounds check
        %% If heap_ptr < dst_reg after add, unsigned overflow occurred
        ?X86_ENC:encode_cmp_rr(HeapReg, DstReg),
        ?X86_ENC:encode_jb(0),  % unsigned: new < old
        {reloc, rel32, OomLbl, -4},
        %% Bounds check: heap_ptr > heap_end?
        ?X86_ENC:encode_cmp_rr(HeapReg, HeapEndReg),
        ?X86_ENC:encode_jbe(0),  % FIXED: unsigned below-or-equal
        {reloc, rel32, OkLbl, -4},
        %% OOM path: trap via int3
        {label, OomLbl},
        ?X86_ENC:encode_int3(),
        %% Success path
        {label, OkLbl}
    ]).

%% @doc Emit code to allocate a dynamic number of bytes (in SizeReg)
%% from the bump allocator. Returns pointer in DstReg.
%% SizeReg is rounded up to 8-byte alignment at runtime.
-spec emit_alloc_reg(atom(), atom(), atom()) -> [term()].
emit_alloc_reg(arm64, DstReg, SizeReg) ->
    HeapReg = heap_reg(arm64),
    HeapEndReg = x27,
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    OkLbl = <<"__alloc_reg_ok_", Uid/binary>>,
    OomLbl = <<"__alloc_reg_oom_", Uid/binary>>,
    OverflowLbl = <<"__alloc_reg_ovfl_", Uid/binary>>,
    lists:flatten([
        %% Align SizeReg to 8: size = (size + 7) & ~7
        ?ARM64_ENC:encode_add_imm(x16, SizeReg, 7),
        %% CRITICAL FIX (Finding 4): Use unsigned condition 'lo' (lower-than) instead of 'lt'
        %% Overflow after add: x16 < SizeReg means unsigned carry
        ?ARM64_ENC:encode_cmp_rr(x16, SizeReg),
        ?ARM64_ENC:encode_b_cond(lo, 0),  % unsigned: below (carry set)
        {reloc, arm64_cond_branch19, OverflowLbl, -4},
        ?ARM64_ENC:encode_mov_imm64(x17, -8),         %% ~7 = 0xFFF...F8
        ?ARM64_ENC:encode_and_rrr(x16, x16, x17),     %% x16 = aligned size
        %% Calculate new heap pointer
        ?ARM64_ENC:encode_add_rrr(x17, HeapReg, x16), %% x17 = new_heap_ptr
        %% CRITICAL FIX (Finding 4): Wraparound check uses unsigned 'lo'
        ?ARM64_ENC:encode_cmp_rr(x17, HeapReg),
        ?ARM64_ENC:encode_b_cond(lo, 0),  % unsigned: new_ptr < old_ptr
        {reloc, arm64_cond_branch19, OverflowLbl, -4},
        %% CRITICAL FIX (Finding 4): Bounds check uses unsigned 'ls' (lower-or-same)
        ?ARM64_ENC:encode_cmp_rr(x17, HeapEndReg),
        ?ARM64_ENC:encode_b_cond(ls, 0),  % unsigned: new_ptr <= heap_end
        {reloc, arm64_cond_branch19, OkLbl, -4},
        %% OOM path
        {label, OomLbl},
        ?ARM64_ENC:encode_brk(16#DEAD),
        %% Overflow path
        {label, OverflowLbl},
        ?ARM64_ENC:encode_brk(16#DEAD),
        %% Success path
        {label, OkLbl},
        ?ARM64_ENC:encode_mov_rr(DstReg, HeapReg),     %% return current ptr
        ?ARM64_ENC:encode_mov_rr(HeapReg, x17)         %% advance
    ]);
emit_alloc_reg(x86_64, DstReg, SizeReg) ->
    HeapReg = heap_reg(x86_64),
    HeapEndReg = r14,
    %% CRITICAL FIX (Finding 9): Choose temp distinct from both DstReg and SizeReg.
    %% If temp aliases SizeReg, self-compare disables overflow check.
    %% Use r11 (reserved scratch) if both DstReg and SizeReg conflict with rax/rcx.
    TmpReg = case {DstReg, SizeReg} of
        {rax, rcx} -> r11;  % both taken
        {rcx, rax} -> r11;  % both taken
        {rax, _} -> rcx;    % rax taken by dst
        {_, rax} -> rcx;    % rax taken by size
        {rcx, _} -> rax;    % rcx taken by dst
        {_, rcx} -> rax;    % rcx taken by size
        _ -> rax            % default
    end,
    Uid = integer_to_binary(erlang:unique_integer([positive])),
    OkLbl = <<"__alloc_reg_x64_ok_", Uid/binary>>,
    OomLbl = <<"__alloc_reg_x64_oom_", Uid/binary>>,
    OverflowLbl = <<"__alloc_reg_x64_ovfl_", Uid/binary>>,
    lists:flatten([
        %% Save original SizeReg to check for overflow after alignment
        ?X86_ENC:encode_mov_rr(TmpReg, SizeReg),
        %% Align: tmp = (size + 7) & ~7
        ?X86_ENC:encode_add_imm(TmpReg, 7),
        %% CRITICAL FIX (Finding 4): Use unsigned 'jb' (below) instead of 'jl' (less)
        %% Overflow after add: tmp < SizeReg means unsigned carry
        ?X86_ENC:encode_cmp_rr(TmpReg, SizeReg),
        ?X86_ENC:encode_jb(0),  % unsigned: below (carry set)
        {reloc, rel32, OverflowLbl, -4},
        encode_and_imm64(TmpReg, -8),
        %% Save current heap pointer in DstReg
        ?X86_ENC:encode_mov_rr(DstReg, HeapReg),
        %% Advance heap pointer
        ?X86_ENC:encode_add_rr(HeapReg, TmpReg),
        %% CRITICAL FIX (Finding 4): Wraparound check uses unsigned 'jb'
        ?X86_ENC:encode_cmp_rr(HeapReg, DstReg),
        ?X86_ENC:encode_jb(0),  % unsigned: new < old
        {reloc, rel32, OverflowLbl, -4},
        %% CRITICAL FIX (Finding 4): Bounds check uses unsigned 'jbe' (below-or-equal)
        ?X86_ENC:encode_cmp_rr(HeapReg, HeapEndReg),
        ?X86_ENC:encode_jbe(0),  % unsigned: heap_ptr <= heap_end
        {reloc, rel32, OkLbl, -4},
        %% OOM path
        {label, OomLbl},
        ?X86_ENC:encode_int3(),
        %% Overflow path
        {label, OverflowLbl},
        ?X86_ENC:encode_int3(),
        %% Success path
        {label, OkLbl}
    ]).

%% Internal: encode AND r64, imm32 (sign-extended)
%% Encoding: REX.W + 81 /4 id
encode_and_imm64(Dst, Imm) ->
    Rex = ?X86_ENC:rex(1, 0, 0, ?X86_ENC:reg_hi(Dst)),
    ModRM = ?X86_ENC:modrm(2#11, 4, ?X86_ENC:reg_lo(Dst)),
    <<Rex:8, 16#81:8, ModRM:8, Imm:32/little-signed>>.

%% Round up to 8-byte alignment.
-spec align8(non_neg_integer()) -> non_neg_integer().
align8(N) ->
    case N rem 8 of
        0 -> N;
        R -> N + (8 - R)
    end.
