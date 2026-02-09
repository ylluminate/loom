%%% @doc UEFI Boot Nucleus for Loom OS.
%%% Generates a bootable UEFI .efi file with serial console output.
%%% @end
-module(vbeam_nucleus_boot).

-export([build/0, build/1]).

build() ->
    {ok, Cwd} = file:get_cwd(),
    OutputPath = filename:join([Cwd, "os", "nucleus", "nucleus.efi"]),
    build(OutputPath).

build(OutputPath) ->
    io:format("Building Loom OS nucleus...~n"),

    Code = build_boot_code(),
    Data = build_boot_data(),

    PEBinary = vbeam_native_pe:emit(Code, Data, 0, x86_64, efi),

    case file:write_file(OutputPath, PEBinary) of
        ok ->
            io:format("~c Nucleus written to: ~s (~w bytes)~n", [16#2713, OutputPath, byte_size(PEBinary)]),
            ok;
        {error, Reason} ->
            io:format("Failed to write nucleus: ~p~n", [Reason]),
            {error, Reason}
    end.

build_boot_code() ->
    %% === Data strings ===
    Banner1 = <<"Loom Kernel v0.2.0 booting...\r\n", 0>>,
    Banner2 = <<"[HAL] Serial: COM1 @ 0x3F8 (115200 baud)\r\n", 0>>,
    Banner3 = <<"[BEAM] V native code output:\r\n", 0>>,

    %% === UCS-2 strings for UEFI ConOut ===
    %% Each ASCII char becomes 2 bytes (char, 0x00), terminated by 0x00, 0x00
    ConOutBanner1 = ucs2_encode("Loom OS v0.2.0\r\n"),
    ConOutBanner2 = ucs2_encode("> Hello from V on BEAM!\r\n"),

    %% === Framebuffer display strings (null-terminated) ===
    SplashStr1 = <<"LOOM OS v0.2.0", 0>>,
    SplashStr2 = <<"V -> BEAM -> x86_64", 0>>,
    SplashStr3 = <<"> Hello from V on BEAM!", 0>>,
    SplashStr4 = <<"L O O M", 0>>,
    SplashStr5 = <<"Built with Opus 4.6", 0>>,

    %% === Build each code section as a binary ===
    SerialInit = build_serial_init(),
    PrintString = build_print_string(),
    ConOutPrint = build_conout_print(),
    VHelloNative = build_v_hello_native(),
    GopInit = build_gop_init(),
    DrawChar = vbeam_font_8x16:draw_char_code(),
    FontData = vbeam_font_8x16:font_binary(),  %% standard VGA 8x16 — clean bitmap font

    %% === Compute layout from actual sizes ===
    %% Entry section: reduced to 100 bytes without ConOut calls
    ExpectedEntrySize = 100,

    SerialInitPos = ExpectedEntrySize,
    PrintStringPos = SerialInitPos + byte_size(SerialInit),
    ConOutPrintPos = PrintStringPos + byte_size(PrintString),
    VHelloNativePos = ConOutPrintPos + byte_size(ConOutPrint),
    GopInitPos = VHelloNativePos + byte_size(VHelloNative),

    %% FbRenderStr needs to know DrawCharPos and FontDataPos
    %% Build a temporary version to get the size
    TempFbRenderStr = build_fb_render_str_template(),
    FbRenderStrPos = GopInitPos + byte_size(GopInit),
    DrawCharPos = FbRenderStrPos + byte_size(TempFbRenderStr),

    %% FbRenderStr2x and DrawChar2x come after DrawChar
    TempFbRenderStr2x = build_fb_render_str_2x_template(),
    FbRenderStr2xPos = DrawCharPos + byte_size(DrawChar),
    DrawChar2x = vbeam_font_8x16:draw_char_2x_code(),
    DrawChar2xPos = FbRenderStr2xPos + byte_size(TempFbRenderStr2x),

    %% FillRect comes after DrawChar2x
    FillRect = build_fill_rect(),
    FillRectPos = DrawChar2xPos + byte_size(DrawChar2x),

    %% BootSplash comes after FillRect
    TempBootSplash = build_boot_splash_template(),
    BootSplashPos = FillRectPos + byte_size(FillRect),

    %% FontData comes after BootSplash
    FontDataPos = BootSplashPos + byte_size(TempBootSplash),

    %% Now build the real versions with correct offsets
    FbRenderStr = build_fb_render_str_final(FbRenderStrPos, DrawCharPos, FontDataPos),
    FbRenderStr2x = build_fb_render_str_2x_final(FbRenderStr2xPos, DrawChar2xPos, FontDataPos),

    %% Data starts after all code
    DataStart = FontDataPos + byte_size(FontData),
    Banner1Pos = DataStart,
    Banner2Pos = Banner1Pos + byte_size(Banner1),
    Banner3Pos = Banner2Pos + byte_size(Banner2),
    ConOutBanner1Pos = Banner3Pos + byte_size(Banner3),
    ConOutBanner2Pos = ConOutBanner1Pos + byte_size(ConOutBanner1),
    SplashStr1Pos = ConOutBanner2Pos + byte_size(ConOutBanner2),
    SplashStr2Pos = SplashStr1Pos + byte_size(SplashStr1),
    SplashStr3Pos = SplashStr2Pos + byte_size(SplashStr2),
    SplashStr4Pos = SplashStr3Pos + byte_size(SplashStr3),
    SplashStr5Pos = SplashStr4Pos + byte_size(SplashStr4),

    %% Build final BootSplash with correct offsets — uses FbRenderStr2x for readable text
    BootSplash = build_boot_splash_final(BootSplashPos, FillRectPos, FbRenderStr2xPos,
                                         [SplashStr1Pos, SplashStr2Pos, SplashStr3Pos,
                                          SplashStr4Pos, SplashStr5Pos]),

    %% === Build entry section ===
    %% Instruction positions (cumulative byte count from start of entry):
    %%  0: push rbp                    (1)
    %%  1: mov rbp, rsp                (3)
    %%  4: sub rsp, 64                 (4)
    %%  8: mov [rbp-8], rcx            (4)  -- ImageHandle
    %% 12: mov [rbp-16], rdx           (4)  -- SystemTable
    %% 16: call init_serial            (5)
    %% 21: lea rdi, [banner1]          (7)
    %% 28: call print_string           (5)
    %% 33: lea rdi, [banner2]          (7)
    %% 40: call print_string           (5)
    %% 45: lea rdi, [banner3]          (7)
    %% 52: call print_string           (5)
    %% 57: call v_hello_native         (5)  -- Call V native code!
    %% 62: mov rcx, [rbp-16]           (4)  -- Load SystemTable for GOP
    %% 66: call gop_init               (5)
    %% 71: mov rdi, 0x100000           (10) -- Load fb_base address
    %% 81: mov rax, [rdi]              (3)  -- Load fb_base value
    %% 84: test rax, rax               (3)  -- Check if GOP found
    %% 87: jz skip_fb                  (2)  -- Skip if no framebuffer (offset=8 to 97)
    %% 89: mov rcx, rax                (3)  -- fb_base as arg1
    %% 92: call boot_splash            (5)  -- Render Loom grid + text
    %% 97: skip_fb:
    %% 97: cli                         (1)
    %% 98: jmp $                       (2)
    %%100: [END]

    Entry = iolist_to_binary([
        <<16#55>>,                                              %% push rbp
        <<16#48, 16#89, 16#E5>>,                                %% mov rbp, rsp
        <<16#48, 16#83, 16#EC, 16#40>>,                         %% sub rsp, 64
        <<16#48, 16#89, 16#4D, 16#F8>>,                         %% mov [rbp-8], rcx (ImageHandle)
        <<16#48, 16#89, 16#55, 16#F0>>,                         %% mov [rbp-16], rdx (SystemTable)

        %% call init_serial (at pos 16, size 5)
        <<16#E8, (SerialInitPos - 21):32/little-signed>>,

        %% lea rdi, [banner1] (at pos 21, size 7)
        <<16#48, 16#8D, 16#3D, (Banner1Pos - 28):32/little-signed>>,
        %% call print_string (at pos 28, size 5)
        <<16#E8, (PrintStringPos - 33):32/little-signed>>,

        %% lea rdi, [banner2] (at pos 33, size 7)
        <<16#48, 16#8D, 16#3D, (Banner2Pos - 40):32/little-signed>>,
        %% call print_string (at pos 40, size 5)
        <<16#E8, (PrintStringPos - 45):32/little-signed>>,

        %% lea rdi, [banner3] (at pos 45, size 7)
        <<16#48, 16#8D, 16#3D, (Banner3Pos - 52):32/little-signed>>,
        %% call print_string (at pos 52, size 5)
        <<16#E8, (PrintStringPos - 57):32/little-signed>>,

        %% call v_hello_native (at pos 57, size 5) -- THE MONEY SHOT
        <<16#E8, (VHelloNativePos - 62):32/little-signed>>,

        %% === GOP Initialization & Framebuffer Rendering (at pos 62) ===
        %% mov rcx, [rbp-16] (load SystemTable for GOP) (at pos 62, size 4)
        <<16#48, 16#8B, 16#4D, 16#F0>>,
        %% call gop_init (at pos 66, size 5)
        <<16#E8, (GopInitPos - 71):32/little-signed>>,

        %% Load fb_base and check if GOP found (at pos 71)
        %% mov rdi, 0x100000 (10 bytes: REX.W movabs)
        <<16#48, 16#BF, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00>>,
        %% mov rax, [rdi] (3 bytes)
        <<16#48, 16#8B, 16#07>>,
        %% test rax, rax (at pos 84, size 3)
        <<16#48, 16#85, 16#C0>>,
        %% jz skip_fb (at pos 87, size 2)
        %% skip_fb is at 97 (CLI), so offset = 97 - 89 = 8
        <<16#74, 8>>,  %% jz +8 bytes to pos 97

        %% Render boot splash (at pos 89)
        %% mov rcx, rax (fb_base as arg1) (at pos 89, size 3)
        <<16#48, 16#89, 16#C1>>,
        %% call boot_splash (at pos 92, size 5, ends at 97)
        <<16#E8, (BootSplashPos - 97):32/little-signed>>,

        %% skip_fb: (at pos 97)
        %% Disable interrupts + spin (prevents UEFI watchdog/timer from crashing)
        <<16#FA>>,                                              %% cli
        <<16#EB, 16#FE>>                                        %% jmp $ (infinite spin)
    ]),

    %% Verify entry size matches expectation
    ActualEntrySize = byte_size(Entry),
    case ActualEntrySize of
        ExpectedEntrySize -> ok;
        _ -> error({entry_size_mismatch, expected, ExpectedEntrySize, got, ActualEntrySize})
    end,

    iolist_to_binary([
        Entry,
        SerialInit,
        PrintString,
        ConOutPrint,
        VHelloNative,
        GopInit,
        FbRenderStr,
        DrawChar,
        FbRenderStr2x,
        DrawChar2x,
        FillRect,
        BootSplash,
        FontData,
        Banner1,
        Banner2,
        Banner3,
        ConOutBanner1,
        ConOutBanner2,
        SplashStr1,
        SplashStr2,
        SplashStr3,
        SplashStr4,
        SplashStr5
    ]).

%% === Serial Init (57 bytes) ===
%% Initializes COM1 at 115200 baud
build_serial_init() ->
    iolist_to_binary([
        <<16#BA, 16#F9, 16#03, 16#00, 16#00>>, <<16#B0, 16#00>>, <<16#EE>>,  %% IER: disable interrupts
        <<16#BA, 16#FB, 16#03, 16#00, 16#00>>, <<16#B0, 16#80>>, <<16#EE>>,  %% LCR: enable DLAB
        <<16#BA, 16#F8, 16#03, 16#00, 16#00>>, <<16#B0, 16#01>>, <<16#EE>>,  %% DLL: divisor 1 (115200)
        <<16#BA, 16#F9, 16#03, 16#00, 16#00>>, <<16#B0, 16#00>>, <<16#EE>>,  %% DLM: 0
        <<16#BA, 16#FB, 16#03, 16#00, 16#00>>, <<16#B0, 16#03>>, <<16#EE>>,  %% LCR: 8N1
        <<16#BA, 16#FA, 16#03, 16#00, 16#00>>, <<16#B0, 16#C7>>, <<16#EE>>,  %% FCR: enable FIFO
        <<16#BA, 16#FC, 16#03, 16#00, 16#00>>, <<16#B0, 16#0B>>, <<16#EE>>,  %% MCR: RTS/DTR
        <<16#C3>>                                                              %% ret
    ]).

%% === Print String (31 bytes) ===
%% Input: RDI = null-terminated string pointer
%% Outputs each character to COM1 (0x3F8)
build_print_string() ->
    iolist_to_binary([
        <<16#48, 16#89, 16#FE>>,                %% mov rsi, rdi
        <<16#AC>>,                               %% lodsb
        <<16#84, 16#C0>>,                        %% test al, al
        <<16#74, 16#16>>,                        %% jz done (22 bytes ahead)
        <<16#88, 16#C3>>,                        %% mov bl, al (save char)
        <<16#BA, 16#FD, 16#03, 16#00, 16#00>>,  %% mov edx, 0x3FD (LSR)
        <<16#EC>>,                               %% in al, dx
        <<16#A8, 16#20>>,                        %% test al, 0x20 (TX ready?)
        <<16#74, 16#F6>>,                        %% jnz wait_loop
        <<16#88, 16#D8>>,                        %% mov al, bl (restore char)
        <<16#BA, 16#F8, 16#03, 16#00, 16#00>>,  %% mov edx, 0x3F8 (THR)
        <<16#EE>>,                               %% out dx, al
        <<16#EB, 16#E5>>,                        %% jmp next_char
        <<16#C3>>                                %% ret
    ]).

%% === UEFI ConOut Print (30 bytes) ===
%% Input: RCX = SystemTable pointer, RDX = UCS-2 string pointer
%% Calls SystemTable->ConOut->OutputString
%% UEFI MS x64 calling convention: RCX, RDX, R8, R9, then stack
build_conout_print() ->
    iolist_to_binary([
        <<16#48, 16#83, 16#EC, 16#28>>,          %% sub rsp, 40 (shadow space + alignment)
        <<16#48, 16#89, 16#D0>>,                 %% mov rax, rdx (save string ptr)
        <<16#48, 16#8B, 16#49, 16#40>>,          %% mov rcx, [rcx+0x40] (ConOut)
        <<16#48, 16#89, 16#C2>>,                 %% mov rdx, rax (string ptr)
        <<16#48, 16#8B, 16#41, 16#08>>,          %% mov rax, [rcx+0x08] (OutputString)
        <<16#FF, 16#D0>>,                        %% call rax
        <<16#48, 16#83, 16#C4, 16#28>>,          %% add rsp, 40
        <<16#C3>>                                %% ret
    ]).

%% === V Hello World Native (59 bytes) ===
%% This is V-compiled BEAM code translated to x86_64 native!
%% Outputs "Hello from V on BEAM!\r\n" to COM1
build_v_hello_native() ->
    %% The string that will be output
    VString = <<"Hello from V on BEAM!\r\n", 0>>,

    %% String starts after the code, calculate offset from end of LEA instruction
    %% LEA instruction is 7 bytes, starts at offset 1, ends at offset 8
    %% String starts at end of all code (just before ret at end)
    %% Total code before string: push(1) + lea(7) + loop(27) + pop(1) + ret(1) = 37
    %% String offset from end of LEA = 37 - 8 = 29
    StringOffset = 29,

    %% Code section
    Code = iolist_to_binary([
        <<16#53>>,                               %% push rbx (save callee-saved)

        %% lea rsi, [rip + string_offset]
        <<16#48, 16#8D, 16#35, StringOffset:32/little-signed>>,  %% lea rsi, [rip+offset]

        %% .loop: (at offset 8)
        <<16#AC>>,                               %% lodsb (AL = [RSI++])
        <<16#84, 16#C0>>,                        %% test al, al
        <<16#74, 16#16>>,                        %% jz .done (+22 bytes to pop rbx)
        <<16#88, 16#C3>>,                        %% mov bl, al (save char)

        %% .wait: (at offset 13)
        <<16#BA, 16#FD, 16#03, 16#00, 16#00>>,  %% mov edx, 0x3FD (LSR port)
        <<16#EC>>,                               %% in al, dx
        <<16#A8, 16#20>>,                        %% test al, 0x20 (TX empty?)
        <<16#74, 16#F6>>,                        %% jz .wait (-10 bytes)

        <<16#88, 16#D8>>,                        %% mov al, bl (restore char)
        <<16#BA, 16#F8, 16#03, 16#00, 16#00>>,  %% mov edx, 0x3F8 (THR port)
        <<16#EE>>,                               %% out dx, al
        <<16#EB, 16#E5>>,                        %% jmp .loop (-27 bytes)

        %% .done: (at offset 35)
        <<16#5B>>,                               %% pop rbx (restore)
        <<16#C3>>                                %% ret
    ]),

    %% Combine code + string data
    iolist_to_binary([Code, VString]).

%% === GOP Init (discovers framebuffer via UEFI Graphics Output Protocol) ===
%% Input: RCX = SystemTable pointer
%% Output: Stores at known memory addresses:
%%   [0x100000] = FrameBufferBase (8 bytes)
%%   [0x100008] = HorizontalResolution (4 bytes)
%%   [0x10000C] = VerticalResolution (4 bytes)
%%   [0x100010] = PixelsPerScanLine (4 bytes)
%% If GOP not found, sets fb_base=0 to signal skip framebuffer rendering
build_gop_init() ->
    iolist_to_binary([
        %% Prologue
        <<16#55>>,                                      %% push rbp
        <<16#48, 16#89, 16#E5>>,                        %% mov rbp, rsp
        <<16#53>>,                                      %% push rbx
        <<16#48, 16#83, 16#EC, 16#38>>,                 %% sub rsp, 56 (shadow + locals)

        %% Load BootServices
        <<16#48, 16#8B, 16#59, 16#60>>,                 %% mov rbx, [rcx+0x60] ; BootServices

        %% Set up GOP GUID on stack (16 bytes at rbp-0x20)
        <<16#C7, 16#45, 16#E0, 16#DE, 16#A9, 16#42, 16#90>>,  %% mov dword [rbp-0x20], 0x9042A9DE
        <<16#C7, 16#45, 16#E4, 16#DC, 16#23, 16#38, 16#4A>>,  %% mov dword [rbp-0x1C], 0x4A3823DC
        <<16#C7, 16#45, 16#E8, 16#96, 16#FB, 16#7A, 16#DE>>,  %% mov dword [rbp-0x18], 0xDE7AFB96
        <<16#C7, 16#45, 16#EC, 16#D0, 16#80, 16#51, 16#6A>>,  %% mov dword [rbp-0x14], 0x6A5180D0

        %% Set up LocateProtocol call (MS x64: RCX, RDX, R8)
        <<16#48, 16#8D, 16#4D, 16#E0>>,                 %% lea rcx, [rbp-0x20] ; &GUID
        <<16#31, 16#D2>>,                               %% xor edx, edx ; NULL (Registration)
        <<16#4C, 16#8D, 16#45, 16#D0>>,                 %% lea r8, [rbp-0x30] ; &gop_out

        %% Call LocateProtocol
        <<16#FF, 16#93, 16#40, 16#01, 16#00, 16#00>>,   %% call [rbx+0x140]

        %% Check if success (EFI_SUCCESS = 0)
        <<16#48, 16#85, 16#C0>>,                        %% test rax, rax
        <<16#75, 16#31>>,                               %% jnz gop_fail (skip 49 bytes)

        %% Load GOP protocol and extract framebuffer info
        <<16#48, 16#8B, 16#45, 16#D0>>,                 %% mov rax, [rbp-0x30] ; GOP
        <<16#48, 16#8B, 16#40, 16#18>>,                 %% mov rax, [rax+0x18] ; Mode
        <<16#48, 16#8B, 16#48, 16#08>>,                 %% mov rcx, [rax+0x08] ; Info
        <<16#48, 16#8B, 16#50, 16#18>>,                 %% mov rdx, [rax+0x18] ; FrameBufferBase

        %% Store FrameBufferBase at 0x100000
        <<16#48, 16#BF, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00>>,  %% mov rdi, 0x100000
        <<16#48, 16#89, 16#17>>,                        %% mov [rdi], rdx

        %% Store HorizontalResolution at 0x100008
        <<16#8B, 16#41, 16#04>>,                        %% mov eax, [rcx+0x04] ; HorizontalResolution
        <<16#89, 16#47, 16#08>>,                        %% mov [rdi+0x08], eax

        %% Store VerticalResolution at 0x10000C
        <<16#8B, 16#41, 16#08>>,                        %% mov eax, [rcx+0x08] ; VerticalResolution
        <<16#89, 16#47, 16#0C>>,                        %% mov [rdi+0x0C], eax

        %% Store PixelsPerScanLine at 0x100010
        <<16#8B, 16#41, 16#20>>,                        %% mov eax, [rcx+0x20] ; PixelsPerScanLine
        <<16#89, 16#47, 16#10>>,                        %% mov [rdi+0x10], eax

        <<16#EB, 16#10>>,                               %% jmp done (skip 16 bytes)

        %% gop_fail: Set fb_base = 0
        <<16#48, 16#BF, 16#00, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00>>,  %% mov rdi, 0x100000
        <<16#48, 16#31, 16#C0>>,                        %% xor rax, rax
        <<16#48, 16#89, 16#07>>,                        %% mov [rdi], rax ; fb_base = 0

        %% done: Epilogue
        <<16#48, 16#83, 16#C4, 16#38>>,                 %% add rsp, 56
        <<16#5B>>,                                      %% pop rbx
        <<16#5D>>,                                      %% pop rbp
        <<16#C3>>                                       %% ret
    ]).

%% Build template to get size (with placeholders)
build_fb_render_str_template() ->
    build_fb_render_str_final(0, 0, 0).

%% Build final version with correct offsets
build_fb_render_str_final(FbRenderStrPos, DrawCharPos, FontDataPos) ->
    %% Position of LEA instruction for font_data: 85 bytes into function
    LeaFontDataInstr = 85,
    LeaFontDataEnd = LeaFontDataInstr + 7,
    FontDataOffset = FontDataPos - (FbRenderStrPos + LeaFontDataEnd),

    %% Position of CALL draw_char instruction: 100 bytes into function
    CallDrawCharInstr = 100,
    CallDrawCharEnd = CallDrawCharInstr + 5,
    DrawCharOffset = DrawCharPos - (FbRenderStrPos + CallDrawCharEnd),

    iolist_to_binary([
        %% Prologue
        <<16#55>>,                                      %% push rbp (1)
        <<16#48, 16#89, 16#E5>>,                        %% mov rbp, rsp (3)
        <<16#41, 16#57>>,                               %% push r15 (2)
        <<16#41, 16#56>>,                               %% push r14 (2)
        <<16#41, 16#55>>,                               %% push r13 (2)
        <<16#41, 16#54>>,                               %% push r12 (2)
        <<16#53>>,                                      %% push rbx (1)
        <<16#48, 16#83, 16#EC, 16#48>>,                 %% sub rsp, 72 (4) [total: 17]

        %% Save parameters
        <<16#49, 16#89, 16#CC>>,                        %% mov r12, rcx ; fb_base (3)
        <<16#41, 16#89, 16#D5>>,                        %% mov r13d, edx ; x (3)
        <<16#45, 16#89, 16#C6>>,                        %% mov r14d, r8d ; y (3) [REX.RB]
        <<16#4D, 16#89, 16#CF>>,                        %% mov r15, r9 ; string_ptr (3) [REX.WRB] [total: 29]

        %% Load stride from known location [0x100010]
        <<16#48, 16#BF, 16#10, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00>>,  %% mov rdi, 0x100010 (10)
        <<16#8B, 16#1F>>,                               %% mov ebx, [rdi] ; stride (2) [total: 41]

        %% string_loop: (at offset 41)
        <<16#41, 16#0F, 16#B6, 16#07>>,                 %% movzx eax, byte [r15] ; load char (4)
        <<16#84, 16#C0>>,                               %% test al, al (2)
        <<16#0F, 16#84, 16#40, 16#00, 16#00, 16#00>>,   %% jz done (6 bytes, near jump to +64) [total: 53]

        %% Set up draw_char call (MS x64)
        <<16#4C, 16#89, 16#E1>>,                        %% mov rcx, r12 ; fb_base (3)
        <<16#44, 16#89, 16#EA>>,                        %% mov edx, r13d ; x (3)
        <<16#45, 16#89, 16#F0>>,                        %% mov r8d, r14d ; y (3)
        <<16#41, 16#89, 16#C1>>,                        %% mov r9d, eax ; char (3) [total: 65]

        %% Set up stack parameters for draw_char
        %% fg_color (white) at [rsp+0x20] (5th arg, MS x64)
        <<16#C7, 16#44, 16#24, 16#20, 16#FF, 16#FF, 16#FF, 16#00>>,  %% mov dword [rsp+0x20], 0x00FFFFFF (8)
        %% bg_color (dark navy, matches screen fill) at [rsp+0x28] (6th arg)
        <<16#C7, 16#44, 16#24, 16#28, 16#18, 16#00, 16#0C, 16#00>>,  %% mov dword [rsp+0x28], 0x000C0018 (8)
        %% stride at [rsp+0x30] (7th arg)
        <<16#89, 16#5C, 16#24, 16#30>>,                 %% mov [rsp+0x30], ebx (4) [total: 85]

        %% font_data base at [rsp+0x38] (8th arg) - use RIP-relative LEA
        %% Current position: 85 bytes into function
        %% LEA instruction is 7 bytes
        <<16#48, 16#8D, 16#3D, FontDataOffset:32/little-signed>>,  %% lea rdi, [rip+offset] (7)
        <<16#48, 16#89, 16#7C, 16#24, 16#38>>,          %% mov [rsp+0x38], rdi (5) [total: 97]

        %% Adjust for actual instruction count (was 100, now 97 due to LEA vs movabs)
        %% Add 3 nops to keep alignment
        <<16#90, 16#90, 16#90>>,                        %% nop x3 (3) [total: 100]

        %% Call draw_char - RIP-relative
        <<16#E8, DrawCharOffset:32/little-signed>>,     %% call draw_char (5) [total: 105]

        %% Advance x by 8 pixels
        <<16#41, 16#83, 16#C5, 16#08>>,                 %% add r13d, 8 (4)
        %% Move to next character
        <<16#49, 16#FF, 16#C7>>,                        %% inc r15 (3)
        %% Loop back to string_loop (offset 41)
        %% Current pos: 112, next = 117, offset = 41 - 117 = -76 = 0xFFFFFFB4
        <<16#E9, 16#B4, 16#FF, 16#FF, 16#FF>>,          %% jmp string_loop (5) [total: 117]

        %% done: (at offset 117)
        <<16#48, 16#83, 16#C4, 16#48>>,                 %% add rsp, 72 (4)
        <<16#5B>>,                                      %% pop rbx (1)
        <<16#41, 16#5C>>,                               %% pop r12 (2)
        <<16#41, 16#5D>>,                               %% pop r13 (2)
        <<16#41, 16#5E>>,                               %% pop r14 (2)
        <<16#41, 16#5F>>,                               %% pop r15 (2)
        <<16#5D>>,                                      %% pop rbp (1)
        <<16#C3>>                                       %% ret (1) [total: 132]
    ]).

%% Build 2x render string template to get size
build_fb_render_str_2x_template() ->
    build_fb_render_str_2x_final(0, 0, 0).

%% Build 2x render string - same as FbRenderStr but calls DrawChar2x and advances x by 16
build_fb_render_str_2x_final(FbRenderStr2xPos, DrawChar2xPos, FontDataPos) ->
    %% Position of LEA instruction for font_data: 85 bytes into function
    LeaFontDataInstr = 85,
    LeaFontDataEnd = LeaFontDataInstr + 7,
    FontDataOffset = FontDataPos - (FbRenderStr2xPos + LeaFontDataEnd),

    %% Position of CALL draw_char_2x instruction: 100 bytes into function
    CallDrawCharInstr = 100,
    CallDrawCharEnd = CallDrawCharInstr + 5,
    DrawChar2xOffset = DrawChar2xPos - (FbRenderStr2xPos + CallDrawCharEnd),

    iolist_to_binary([
        %% Prologue (17 bytes)
        <<16#55>>,                                      %% push rbp
        <<16#48, 16#89, 16#E5>>,                        %% mov rbp, rsp
        <<16#41, 16#57>>,                               %% push r15
        <<16#41, 16#56>>,                               %% push r14
        <<16#41, 16#55>>,                               %% push r13
        <<16#41, 16#54>>,                               %% push r12
        <<16#53>>,                                      %% push rbx
        <<16#48, 16#83, 16#EC, 16#48>>,                 %% sub rsp, 72

        %% Save parameters (12 bytes)
        <<16#49, 16#89, 16#CC>>,                        %% mov r12, rcx ; fb_base
        <<16#41, 16#89, 16#D5>>,                        %% mov r13d, edx ; x
        <<16#45, 16#89, 16#C6>>,                        %% mov r14d, r8d ; y
        <<16#4D, 16#89, 16#CF>>,                        %% mov r15, r9 ; string_ptr

        %% Load stride from [0x100010] (12 bytes)
        <<16#48, 16#BF, 16#10, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00>>,
        <<16#8B, 16#1F>>,                               %% mov ebx, [rdi]

        %% string_loop: (offset 41)
        <<16#41, 16#0F, 16#B6, 16#07>>,                 %% movzx eax, byte [r15]
        <<16#84, 16#C0>>,                               %% test al, al
        <<16#0F, 16#84, 16#40, 16#00, 16#00, 16#00>>,   %% jz done

        %% Set up draw_char_2x call
        <<16#4C, 16#89, 16#E1>>,                        %% mov rcx, r12
        <<16#44, 16#89, 16#EA>>,                        %% mov edx, r13d
        <<16#45, 16#89, 16#F0>>,                        %% mov r8d, r14d
        <<16#41, 16#89, 16#C1>>,                        %% mov r9d, eax

        %% Stack parameters
        <<16#C7, 16#44, 16#24, 16#20, 16#FF, 16#FF, 16#FF, 16#00>>,  %% fg = white
        <<16#C7, 16#44, 16#24, 16#28, 16#18, 16#00, 16#0C, 16#00>>,  %% bg = dark navy
        <<16#89, 16#5C, 16#24, 16#30>>,                 %% stride

        %% font_data via RIP-relative LEA (offset 85)
        <<16#48, 16#8D, 16#3D, FontDataOffset:32/little-signed>>,
        <<16#48, 16#89, 16#7C, 16#24, 16#38>>,

        %% Padding to keep alignment (offset 97)
        <<16#90, 16#90, 16#90>>,

        %% Call draw_char_2x (offset 100)
        <<16#E8, DrawChar2xOffset:32/little-signed>>,

        %% Advance x by 16 (2x char width) — THIS IS THE KEY DIFFERENCE
        <<16#41, 16#83, 16#C5, 16#10>>,                 %% add r13d, 16
        <<16#49, 16#FF, 16#C7>>,                        %% inc r15
        <<16#E9, 16#B4, 16#FF, 16#FF, 16#FF>>,          %% jmp string_loop

        %% done: (offset 117)
        <<16#48, 16#83, 16#C4, 16#48>>,                 %% add rsp, 72
        <<16#5B>>,
        <<16#41, 16#5C>>,
        <<16#41, 16#5D>>,
        <<16#41, 16#5E>>,
        <<16#41, 16#5F>>,
        <<16#5D>>,
        <<16#C3>>                                       %% [total: 132 bytes]
    ]).

%% === Fill Rectangle (53 bytes) ===
%% Draws a filled rectangle on the framebuffer
%% Input: RCX=fb_base, EDX=x, R8D=y, R9D=width, R10D=height, R11D=color
%% Uses stride from [0x100010]
build_fill_rect() ->
    <<
    16#57,                                              %% push rdi                    ; (1) = 1
    16#53,                                              %% push rbx                    ; (1) = 2
    16#48, 16#BF, 16#10, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00,  %% mov rdi, 0x100010 ; (10) = 12
    16#8B, 16#1F,                                       %% mov ebx, [rdi]    ; stride  ; (2) = 14
    %% .row: (offset 14)
    16#44, 16#89, 16#C0,                                %% mov eax, r8d      ; y       ; (3) = 17
    16#0F, 16#AF, 16#C3,                                %% imul eax, ebx     ; y*stride; (3) = 20
    16#01, 16#D0,                                       %% add eax, edx      ; +x      ; (2) = 22
    16#48, 16#C1, 16#E0, 16#02,                         %% shl rax, 2        ; *4      ; (4) = 26
    16#48, 16#8D, 16#3C, 16#01,                         %% lea rdi, [rcx+rax]; addr    ; (4) = 30
    16#44, 16#89, 16#D8,                                %% mov eax, r11d      ; color  ; (3) = 33
    16#90, 16#90,                                        %% nop; nop           ; pad    ; (2) = 35
    16#51,                                              %% push rcx          ; save    ; (1) = 36
    16#44, 16#89, 16#C9,                                %% mov ecx, r9d      ; count=w ; (3) = 39
    16#F3, 16#AB,                                       %% rep stosd         ; fill!   ; (2) = 41
    16#59,                                              %% pop rcx           ; restore ; (1) = 42
    16#41, 16#FF, 16#C0,                                %% inc r8d           ; y++     ; (3) = 45
    16#41, 16#FF, 16#CA,                                %% dec r10d          ; h--     ; (3) = 48
    16#75, 16#DC,                                       %% jnz .row  ; -36 (14-50=-36=0xDC) ; (2) = 50
    16#5B,                                              %% pop rbx                    ; (1) = 51
    16#5F,                                              %% pop rdi                    ; (1) = 52
    16#C3                                               %% ret                        ; (1) = 53
    >>.

%% === Boot Splash Template (for size calculation) ===
build_boot_splash_template() ->
    build_boot_splash_final(0, 0, 0, [0, 0, 0, 0, 0]).

%% === Boot Splash Final (291 bytes) ===
%% Renders full-screen dark fill, Loom grid logo, and text lines
%% Input: RCX = fb_base
build_boot_splash_final(BootSplashPos, FillRectPos, FbRenderStrPos, StrPositions) ->
    [S1P, S2P, S3P, S4P, S5P] = StrPositions,

    %% Screen fill CALL offset: call at offset 48, ends at 53
    ScreenFillCallOff = FillRectPos - (BootSplashPos + 53),

    %% Grid FillRect CALL offset: call at offset 95, ends at 100
    GridFillCallOff = FillRectPos - (BootSplashPos + 100),

    %% Grid data at offset 251, LEA ends at offset 66
    GridDataOff = 251 - 66,  %% = 185

    %% FbRenderStr CALL offsets (each text render is 26 bytes)
    %% Renders start at 109, 135, 161, 187, 213
    FbRS1 = FbRenderStrPos - (BootSplashPos + 109 + 26),
    FbRS2 = FbRenderStrPos - (BootSplashPos + 135 + 26),
    FbRS3 = FbRenderStrPos - (BootSplashPos + 161 + 26),
    FbRS4 = FbRenderStrPos - (BootSplashPos + 187 + 26),
    FbRS5 = FbRenderStrPos - (BootSplashPos + 213 + 26),

    %% String LEA offsets (LEA at +14 in each render, 7 bytes, ends at +21)
    SL1 = S1P - (BootSplashPos + 109 + 21),
    SL2 = S2P - (BootSplashPos + 135 + 21),
    SL3 = S3P - (BootSplashPos + 161 + 21),
    SL4 = S4P - (BootSplashPos + 187 + 21),
    SL5 = S5P - (BootSplashPos + 213 + 21),

    iolist_to_binary([
        %% === Prologue (17 bytes, offset 0-16) ===
        <<16#55>>,                                      %% push rbp
        <<16#48, 16#89, 16#E5>>,                        %% mov rbp, rsp
        <<16#41, 16#54>>,                               %% push r12
        <<16#41, 16#55>>,                               %% push r13
        <<16#41, 16#56>>,                               %% push r14
        <<16#48, 16#83, 16#EC, 16#08>>,                 %% sub rsp, 8
        <<16#49, 16#89, 16#CC>>,                        %% mov r12, rcx (save fb_base)

        %% === Screen fill (36 bytes, offset 17-52) ===
        %% Load screen dimensions from known memory locations
        <<16#48, 16#BF, 16#08, 16#00, 16#10, 16#00, 16#00, 16#00, 16#00, 16#00>>,  %% mov rdi, 0x100008
        <<16#44, 16#8B, 16#0F>>,                        %% mov r9d, [rdi]   ; width
        <<16#44, 16#8B, 16#57, 16#04>>,                 %% mov r10d, [rdi+4] ; height
        <<16#4C, 16#89, 16#E1>>,                        %% mov rcx, r12     ; fb_base
        <<16#31, 16#D2>>,                               %% xor edx, edx     ; x=0
        <<16#45, 16#31, 16#C0>>,                        %% xor r8d, r8d     ; y=0
        <<16#41, 16#BB, 16#18, 16#00, 16#0C, 16#00>>,  %% mov r11d, 0x000C0018 ; dark navy
        <<16#E8, ScreenFillCallOff:32/little-signed>>,  %% call FillRect

        %% === Set white for grid lines (6 bytes, offset 53-58) ===
        <<16#41, 16#BB, 16#FF, 16#FF, 16#FF, 16#00>>,  %% mov r11d, 0x00FFFFFF

        %% === Grid loop (50 bytes, offset 59-108) ===
        <<16#4C, 16#8D, 16#2D, GridDataOff:32/little-signed>>,  %% lea r13, [rip+grid_data]
        <<16#41, 16#BE, 16#06, 16#00, 16#00, 16#00>>,           %% mov r14d, 6
        %% .grid_loop: (offset 72)
        <<16#4C, 16#89, 16#E1>>,                        %% mov rcx, r12
        <<16#41, 16#0F, 16#B6, 16#55, 16#00>>,          %% movzx edx, byte [r13+0]
        <<16#45, 16#0F, 16#B6, 16#45, 16#01>>,          %% movzx r8d, byte [r13+1]
        <<16#45, 16#0F, 16#B6, 16#4D, 16#02>>,          %% movzx r9d, byte [r13+2]
        <<16#45, 16#0F, 16#B6, 16#55, 16#03>>,          %% movzx r10d, byte [r13+3]
        <<16#E8, GridFillCallOff:32/little-signed>>,     %% call FillRect
        <<16#49, 16#83, 16#C5, 16#04>>,                 %% add r13, 4
        <<16#41, 16#FF, 16#CE>>,                        %% dec r14d
        <<16#75, 16#DB>>,                               %% jnz .grid_loop (-37)

        %% === Text render 1: "LOOM OS v0.2.0" at (240, 50) — offset 109 ===
        <<16#4C, 16#89, 16#E1>>,                        %% mov rcx, r12
        <<16#BA, 240:32/little>>,                        %% mov edx, 240
        <<16#41, 16#B8, 50:32/little>>,                  %% mov r8d, 50
        <<16#4C, 16#8D, 16#0D, SL1:32/little-signed>>,  %% lea r9, [rip+str1]
        <<16#E8, FbRS1:32/little-signed>>,               %% call FbRenderStr

        %% === Text render 2: "V -> BEAM -> x86_64" at (240, 90) — offset 135 ===
        <<16#4C, 16#89, 16#E1>>,
        <<16#BA, 240:32/little>>,
        <<16#41, 16#B8, 90:32/little>>,
        <<16#4C, 16#8D, 16#0D, SL2:32/little-signed>>,
        <<16#E8, FbRS2:32/little-signed>>,

        %% === Text render 3: "> Hello from V on BEAM!" at (240, 135) — offset 161 ===
        <<16#4C, 16#89, 16#E1>>,
        <<16#BA, 240:32/little>>,
        <<16#41, 16#B8, 135:32/little>>,
        <<16#4C, 16#8D, 16#0D, SL3:32/little-signed>>,
        <<16#E8, FbRS3:32/little-signed>>,

        %% === Text render 4: "L O O M" at (85, 200) — offset 187 ===
        <<16#4C, 16#89, 16#E1>>,
        <<16#BA, 85:32/little>>,
        <<16#41, 16#B8, 200:32/little>>,
        <<16#4C, 16#8D, 16#0D, SL4:32/little-signed>>,
        <<16#E8, FbRS4:32/little-signed>>,

        %% === Text render 5: "Built with Opus 4.6" at (240, 185) — offset 213 ===
        <<16#4C, 16#89, 16#E1>>,
        <<16#BA, 240:32/little>>,
        <<16#41, 16#B8, 185:32/little>>,
        <<16#4C, 16#8D, 16#0D, SL5:32/little-signed>>,
        <<16#E8, FbRS5:32/little-signed>>,

        %% === Epilogue (12 bytes, offset 239) ===
        <<16#48, 16#83, 16#C4, 16#08>>,                %% add rsp, 8
        <<16#41, 16#5E>>,                               %% pop r14
        <<16#41, 16#5D>>,                               %% pop r13
        <<16#41, 16#5C>>,                               %% pop r12
        <<16#5D>>,                                      %% pop rbp
        <<16#C3>>,                                      %% ret

        %% === Grid data table (24 bytes) ===
        %% 6 entries: (x, y, width, height) as bytes
        %% Uncapped 3x3 loom mesh — threads flow freely past intersections
        %% 3 horizontal threads (open-ended, 20px overhang each side)
        <<70, 90, 120, 3>>,
        <<70, 130, 120, 3>>,
        <<70, 170, 120, 3>>,
        %% 3 vertical threads (open-ended, 20px overhang each side)
        <<90, 70, 3, 120>>,
        <<130, 70, 3, 120>>,
        <<170, 70, 3, 120>>
    ]).

%% === UCS-2 String Encoding Helper ===
%% Converts ASCII string to UCS-2 (each char becomes 2 bytes: char, 0x00)
%% Terminated by 0x00, 0x00
ucs2_encode(Str) ->
    Chars = [<<C, 0>> || C <- Str],
    iolist_to_binary([Chars, <<0, 0>>]).

build_boot_data() ->
    <<>>.
