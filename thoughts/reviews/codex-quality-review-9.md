Baseline: `make compile && make check && make test` passed (`exit 0`).

HIGH|compat/syscall/vbeam_linux_syscall.erl:277|stub_syscall retries recursively on badarg; protected ETS from non-owner inserts can fail forever, causing stack exhaustion.|Replace recursive retry with single fallback return.
HIGH|vm/interp/vbeam_beam_interp_v2.erl:547|Fallback BIF uses apply(Mod,Fun,Args) with guest-controlled values, enabling arbitrary host-function execution.|Enforce strict BIF allowlist, return undef for everything else.
HIGH|kernel/io/vbeam_io_server.erl:217|is_safe_mfa allows all erlang functions; io_requests can invoke halt etc.|Whitelist exact safe MFA tuples needed for formatting only.
HIGH|arch/ir/vbeam_native_alloc.erl:71|Allocator init stores raw mmap syscall result without checking for failure.|Check syscall return for failure sentinel and branch to error path.
HIGH|arch/ir/vbeam_native_alloc.erl:111|Bump allocator advances heap pointer without end-of-heap bound check.|Track heap_end and emit per-allocation bounds checks.
HIGH|arch/x86_64/vbeam_native_lower_x86_64.erl:832|Map capacity overflow emits ud2 causing hard crash.|Implement growth/reallocation or recoverable error instead of trap.
HIGH|arch/arm64/vbeam_native_lower_arm64.erl:948|Map capacity overflow emits svc 0xFFFF causing fatal trap.|Implement growth/reallocation or safe error return.
HIGH|vm/parser/vbeam_beam_parser.erl:146|zlib:uncompress executed before enforcing limits — compressed-bomb DoS.|Use streaming inflate with hard output cap during decompression.
HIGH|vm/parser/vbeam_beam_standalone.erl:167|Standalone parser same pre-limit zlib:uncompress behavior.|Use bounded streaming decompression.

MEDIUM|compat/kpi/vbeam_linuxkpi.erl:408|Timer ref updates use non-atomic persistent_term read-modify-write.|Move timer state to ETS with atomic operations.
MEDIUM|compat/kpi/vbeam_linuxkpi.erl:287|mod_timer passes ExpiresJiffies to send_after without validating type/range.|Validate timeout as bounded non-negative integer.
MEDIUM|compat/kpi/vbeam_linuxkpi.erl:288|Timer refs only removed by del_timer; one-shot timers never auto-pruned.|Remove timer refs on timeout delivery.
MEDIUM|compat/kpi/vbeam_linuxkpi.erl:83|dma_alloc_coherent pattern-matches successful kmalloc and crashes on error.|Handle kmalloc error tuples explicitly.
MEDIUM|vm/interp/vbeam_beam_interp_v2.erl:475|Y-register index N not validated before lists:sublist/nthtail.|Guard N with is_integer and >= 0.
MEDIUM|kernel/boot/vbeam_boot_sequence.erl:134|Padding lengths from config addresses are unbounded.|Validate layout invariants and cap padding.
MEDIUM|compat/syscall/vbeam_linux_syscall.erl:35|dispatch only catches function_clause and badarg.|Catch broadly, map to errno, isolate handler failures.
MEDIUM|arch/x86_64/vbeam_native_lower_x86_64.erl:238|Signed division emits idiv without zero-divisor check.|Emit runtime zero-divisor guard.

LOW|vm/jit/vbeam_beam_to_native.erl:299|Unknown opcodes silently translated to NOP.|Fail closed by rejecting unknown opcode shapes.
