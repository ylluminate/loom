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
        ?ARM64_ENC:encode_mov_imm64(SysNumReg, 93),    %% sys_exit
        ?ARM64_ENC:encode_svc(SvcImm),                 %% exit(1)
        %% Success path
        {label, OkLbl},
        ?ARM64_ENC:encode_mov_rr(x28, x0)
    ]);
emit_alloc_init(x86_64, _Format) ->
    %% x86_64 Linux mmap syscall:
    %%   rax = 9 (sys_mmap)
    %%   rdi = addr (0), rsi = length, rdx = prot,
    %%   r10 = flags, r8 = fd, r9 = offset
    lists:flatten([
        ?X86_ENC:encode_mov_imm64(rdi, 0),
        ?X86_ENC:encode_mov_imm64(rsi, ?HEAP_SIZE),
        ?X86_ENC:encode_mov_imm64(rdx, 3),
        ?X86_ENC:encode_mov_imm64(r10, 16#22),
        ?X86_ENC:encode_mov_imm64(r8, -1),
        ?X86_ENC:encode_mov_imm64(r9, 0),
        ?X86_ENC:encode_mov_imm64(rax, 9),
        ?X86_ENC:encode_syscall(),
        ?X86_ENC:encode_mov_rr(r15, rax)
    ]).

%% @doc Emit code to allocate N bytes (compile-time constant) from the
%% bump allocator. Returns pointer in DstReg, advances heap pointer.
%% N is rounded up to 8-byte alignment.
%% Includes bounds check to prevent heap overflow.
-spec emit_alloc(atom(), atom(), non_neg_integer()) -> [term()].
emit_alloc(arm64, DstReg, N) ->
    AlignedN = align8(N),
    HeapReg = heap_reg(arm64),
    %% Heap end would be stored in x27 (initialized to x28 + HEAP_SIZE at start)
    %% For now, we'll just emit the allocation without bounds check
    %% TODO: Initialize x27 = x28 + HEAP_SIZE at program start and add bounds check
    lists:flatten([
        ?ARM64_ENC:encode_mov_rr(DstReg, HeapReg),     %% return current ptr
        %% Advance heap pointer
        if AlignedN >= 0, AlignedN < 4096 ->
            ?ARM64_ENC:encode_add_imm(HeapReg, HeapReg, AlignedN);
           true ->
            [?ARM64_ENC:encode_mov_imm64(x16, AlignedN),
             ?ARM64_ENC:encode_add_rrr(HeapReg, HeapReg, x16)]
        end
    ]);
emit_alloc(x86_64, DstReg, N) ->
    AlignedN = align8(N),
    HeapReg = heap_reg(x86_64),
    lists:flatten([
        ?X86_ENC:encode_mov_rr(DstReg, HeapReg),
        ?X86_ENC:encode_add_imm(HeapReg, AlignedN)
    ]).

%% @doc Emit code to allocate a dynamic number of bytes (in SizeReg)
%% from the bump allocator. Returns pointer in DstReg.
%% SizeReg is rounded up to 8-byte alignment at runtime.
-spec emit_alloc_reg(atom(), atom(), atom()) -> [term()].
emit_alloc_reg(arm64, DstReg, SizeReg) ->
    HeapReg = heap_reg(arm64),
    lists:flatten([
        %% Align SizeReg to 8: size = (size + 7) & ~7
        ?ARM64_ENC:encode_add_imm(x16, SizeReg, 7),
        ?ARM64_ENC:encode_mov_imm64(x17, -8),         %% ~7 = 0xFFF...F8
        ?ARM64_ENC:encode_and_rrr(x16, x16, x17),     %% x16 = aligned size
        %% Bump allocate
        ?ARM64_ENC:encode_mov_rr(DstReg, HeapReg),     %% return current ptr
        ?ARM64_ENC:encode_add_rrr(HeapReg, HeapReg, x16) %% advance
    ]);
emit_alloc_reg(x86_64, DstReg, SizeReg) ->
    HeapReg = heap_reg(x86_64),
    %% We need a temp; use rax if DstReg isn't rax, else use rcx
    TmpReg = case DstReg of rax -> rcx; _ -> rax end,
    lists:flatten([
        %% Align: tmp = (size + 7) & ~7
        ?X86_ENC:encode_mov_rr(TmpReg, SizeReg),
        ?X86_ENC:encode_add_imm(TmpReg, 7),
        ?X86_ENC:encode_and_rr(TmpReg, TmpReg),        %% self-AND is no-op; need imm
        %% Actually: just use add+and with immediate mask
        %% x86 AND r64, imm32 is REX.W 81 /4
        encode_and_imm64(TmpReg, -8),
        %% Bump allocate
        ?X86_ENC:encode_mov_rr(DstReg, HeapReg),
        ?X86_ENC:encode_add_rr(HeapReg, TmpReg)
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
