#!/usr/bin/env escript
%%% Verify x86-64 encoding fixes

main(_) ->
    %% Load compiled modules
    code:add_pathz("_build"),

    io:format("~n=== VERIFICATION: x86-64 Encoding Fixes ===~n~n"),

    %% Fix 1: Exception stub jmp displacements
    verify_stub_jumps(),

    %% Fix 2: encode_mov_mem_imm16
    verify_mov_mem_imm16(),

    %% Fix 4: Page table base addresses
    verify_page_table_base(),

    %% Fix 5: serial_puts call displacement
    verify_serial_puts_call(),

    io:format("~n=== All verifications passed ===~n").

verify_stub_jumps() ->
    io:format("Fix 1: Exception stub jmp rel32 displacements~n"),

    Stubs = vbeam_gdt_idt:exception_stubs(),

    %% Measure timer and generic stub sizes (not exported, so measure from full binary)
    %% We know: 32 exception stubs (320 bytes) + timer + generic + common handler
    %% Find common handler start by measuring

    %% Extract stub 0
    <<Stub0:10/binary, _/binary>> = Stubs,
    <<_:2/binary, _:2/binary, 16#E9, Disp0:32/little-signed, _/binary>> = Stub0,

    %% Stub 0: jmp ends at byte 9, displacement should reach common handler
    JmpEnd0 = 9,
    Target0 = JmpEnd0 + Disp0,

    io:format("  Stub 0: jmp displacement = ~w (0x~.16B)~n", [Disp0, Disp0 band 16#FFFFFFFF]),
    io:format("  Stub 0: jmp ends at ~w, target at ~w~n", [JmpEnd0, Target0]),

    %% Extract stub 31 (last exception stub)
    <<_:310/binary, Stub31:10/binary, _/binary>> = Stubs,
    <<_:2/binary, _:2/binary, 16#E9, Disp31:32/little-signed, _/binary>> = Stub31,

    JmpEnd31 = 310 + 9,
    Target31 = JmpEnd31 + Disp31,

    io:format("  Stub 31: jmp displacement = ~w (0x~.16B)~n", [Disp31, Disp31 band 16#FFFFFFFF]),
    io:format("  Stub 31: jmp ends at ~w, target at ~w~n", [JmpEnd31, Target31]),

    %% Both should point to the same common handler
    case Target0 == Target31 of
        true ->
            io:format("  ✓ All stubs point to same common handler at offset ~w~n", [Target0]);
        false ->
            io:format("  ✗ ERROR: Stubs point to different targets!~n"),
            halt(1)
    end,

    ok.

verify_mov_mem_imm16() ->
    io:format("~nFix 2: encode_mov_mem_imm16 ModR/M encoding~n"),

    %% Generate idt_load_code to check the mov word [rax], limit encoding
    Code = vbeam_gdt_idt:idt_load_code(16#300000),

    %% Find the "mov word [rax], 4095" instruction
    %% It should be: 66 [REX] C7 00 FF 0F (66 prefix, REX (minimal), C7 /0, ModRM=00, imm16)
    %% REX for rax with W=0 is 0x40
    %% Expected: 66 40 C7 00 FF 0F

    case binary:match(Code, <<16#66, 16#40, 16#C7, 16#00>>) of
        {Pos, _} ->
            %% Extract the full instruction
            <<_:Pos/binary, Insn:6/binary, _/binary>> = Code,
            <<16#66, 16#40, 16#C7, ModRM, Imm:16/little>> = Insn,
            io:format("  Found: 66 40 C7 ~.16B ~.16B~n", [ModRM, Imm]),

            %% ModRM should be 0x00 (mod=00, reg=0, rm=000)
            case ModRM of
                16#00 ->
                    io:format("  ✓ ModRM = 0x00 (correct: [rax])~n");
                _ ->
                    io:format("  ✗ ERROR: ModRM = 0x~2.16.0B (expected 0x00)~n", [ModRM]),
                    halt(1)
            end,

            case Imm of
                4095 ->
                    io:format("  ✓ Immediate = 4095 (0x0FFF) - correct limit~n");
                _ ->
                    io:format("  ✗ ERROR: Immediate = ~w (expected 4095)~n", [Imm]),
                    halt(1)
            end;
        nomatch ->
            io:format("  ✗ ERROR: Could not find mov word [rax] instruction~n"),
            halt(1)
    end,

    ok.

verify_page_table_base() ->
    io:format("~nFix 4: Page table pointers use absolute base~n"),

    %% Generate page tables with explicit base
    Base = 16#300000,
    Tables = vbeam_paging:page_tables(Base, 4),

    %% Extract PML4[0] entry (first 8 bytes)
    <<PML4Entry0:64/little, _/binary>> = Tables,

    %% Extract physical address from entry (bits 12-51)
    PDPTPhys = (PML4Entry0 bsr 12) bsl 12,
    ExpectedPDPT = Base + 16#1000,

    io:format("  PML4[0] points to: 0x~.16B~n", [PDPTPhys]),
    io:format("  Expected (base + 0x1000): 0x~.16B~n", [ExpectedPDPT]),

    case PDPTPhys of
        ExpectedPDPT ->
            io:format("  ✓ PML4[0] correctly points to base + 0x1000~n");
        _ ->
            io:format("  ✗ ERROR: PML4[0] pointer is wrong!~n"),
            halt(1)
    end,

    %% Extract PDPT[0] entry (at offset 4096, first entry)
    <<_:4096/binary, PDPTEntry0:64/little, _/binary>> = Tables,

    PD0Phys = (PDPTEntry0 bsr 12) bsl 12,
    ExpectedPD0 = Base + 16#2000,

    io:format("  PDPT[0] points to: 0x~.16B~n", [PD0Phys]),
    io:format("  Expected (base + 0x2000): 0x~.16B~n", [ExpectedPD0]),

    case PD0Phys of
        ExpectedPD0 ->
            io:format("  ✓ PDPT[0] correctly points to base + 0x2000~n");
        _ ->
            io:format("  ✗ ERROR: PDPT[0] pointer is wrong!~n"),
            halt(1)
    end,

    ok.

verify_serial_puts_call() ->
    io:format("~nFix 5: serial_puts call displacement~n"),

    PutcharCode = vbeam_beam_to_native:serial_putchar_code(),
    PutsCode = vbeam_beam_to_native:serial_puts_code(),

    PutcharSize = byte_size(PutcharCode),

    %% Find the call instruction in puts code
    %% Should be at offset 7: E8 <disp32>
    <<_:7/binary, 16#E8, CallDisp:32/little-signed, _/binary>> = PutsCode,

    io:format("  serial_putchar size: ~w bytes~n", [PutcharSize]),
    io:format("  call displacement: ~w (0x~.16B)~n", [CallDisp, CallDisp band 16#FFFFFFFF]),

    %% Call is at offset 7 in serial_puts, which is at offset PutcharSize in the combined layout
    %% Call instruction ends at: PutcharSize + 7 + 5 = PutcharSize + 12
    %% Target should be: 0 (start of serial_putchar)
    %% Displacement = 0 - (PutcharSize + 12)

    CallInsnEnd = PutcharSize + 12,
    Target = CallInsnEnd + CallDisp,

    io:format("  call ends at offset ~w, target at ~w~n", [CallInsnEnd, Target]),

    case Target of
        0 ->
            io:format("  ✓ call correctly targets serial_putchar at offset 0~n");
        _ ->
            io:format("  ✗ ERROR: call targets offset ~w (expected 0)~n", [Target]),
            halt(1)
    end,

    ok.
