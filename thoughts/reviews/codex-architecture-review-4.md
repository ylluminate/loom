`make compile && make check && make test` passed cleanly (exit 0).  
Review was read-only (no file edits), and I did not run preflight scripts.

1. `CRITICAL` — `kernel/arch/vbeam_gdt_idt.erl:172` / `kernel/arch/vbeam_gdt_idt.erl:183` / `kernel/arch/vbeam_gdt_idt.erl:294` / `kernel/arch/vbeam_gdt_idt.erl:311`  
Problem: IDT/exceptions assume fixed 10-byte stubs, but actual stub block is variable-size (`exception_stubs` is 374 bytes). IDT entry 33 points into timer stub bytes, and exception `jmp rel32` targets land at timer `iretq` instead of common handler.  
Fix: build stubs first, compute real offsets/labels, then emit IDT entries and `jmp` displacements from actual byte positions.

2. `CRITICAL` — `kernel/arch/vbeam_gdt_idt.erl:440` / `kernel/arch/vbeam_gdt_idt.erl:444` / `kernel/arch/vbeam_gdt_idt.erl:233`  
Problem: `encode_mov_mem_imm16/3` uses ModR/M `mod=11` for `Offset=0`, encoding register form (`mov ax, imm16`) instead of memory form (`mov word [rax], imm16`). `idt_load_code/1` therefore fails to write IDTR limit to memory.  
Fix: use memory ModR/M (`00/01/10`), with `rbp/r13` disp8=0 handling and SIB for `rsp/r12`.

3. `HIGH` — `kernel/boot/vbeam_boot_sequence.erl:49` / `kernel/boot/vbeam_boot_sequence.erl:51` / `kernel/boot/vbeam_boot_sequence.erl:54` / `kernel/boot/vbeam_boot_sequence.erl:57`  
Problem: boot sequence concatenates helpers that each end in `ret`; execution returns early after first helper block.  
Fix: either strip helper `ret`s when inlining, or place helpers separately and `call` them.

4. `HIGH` — `kernel/boot/vbeam_boot_sequence.erl:72` / `kernel/boot/vbeam_boot_sequence.erl:73` / `kernel/boot/vbeam_boot_sequence.erl:74` / `kernel/boot/vbeam_boot_sequence.erl:98`  
Problem: `boot_data/1` ignores configured base addresses and emits a tightly packed blob, while `boot_code/1` uses absolute base addresses from config. Address invariants are not enforced.  
Fix: honor configured base addresses in data layout (with padding/placement), or derive all runtime pointers from emitted layout only.

5. `HIGH` — `kernel/arch/vbeam_gdt_idt.erl:223` / `kernel/boot/vbeam_boot_sequence.erl:98`  
Problem: IDTR is written at `IDTBase + 4096` without reserved storage; with contiguous layouts this can overwrite the next structure.  
Fix: reserve dedicated IDTR memory and pass its explicit address.

6. `CRITICAL` — `vm/jit/vbeam_beam_to_native.erl:86` / `vm/jit/vbeam_beam_to_native.erl:89`  
Problem: `serial_puts_code/0` has incorrect rel8 branch displacements (`jz` and loop `jmp` land at wrong instructions).  
Fix: recompute offsets from instruction-end addresses or use label-based relocation.

7. `CRITICAL` — `vm/jit/vbeam_beam_to_native.erl:319` / `vm/jit/vbeam_beam_to_native.erl:329`  
Problem: `translate_serial_output/0` has broken rel8 displacements (`jz` lands in immediate bytes; back jump targets invalid location).  
Fix: use label resolution for all short branches.

8. `HIGH` — `kernel/io/vbeam_irq_bridge.erl:181` / `kernel/io/vbeam_irq_bridge.erl:201` / `kernel/io/vbeam_irq_bridge.erl:204` / `kernel/io/vbeam_irq_bridge.erl:207`  
Problem: ISR keeps ring-buffer base in `rax`, then `rdtsc` clobbers `eax/edx`; later memory writes still use `rax` as base.  
Fix: preserve base in a non-clobbered register (e.g. `r11`/`r12`) or save/restore `rax` around `rdtsc`.

9. `MEDIUM` — `kernel/arch/vbeam_gdt_idt.erl:42` / `kernel/arch/vbeam_gdt_idt.erl:52`  
Problem: data-segment descriptors use flags nibble `0xA` (L-bit set). In IA-32e, L is for 64-bit code segments; for data descriptors it should be 0.  
Fix: use data flags with L=0 (typically `0x8` or `0xC` depending desired D/B semantics).

10. `MEDIUM` — `kernel/mm/vbeam_paging.erl:48` / `kernel/mm/vbeam_paging.erl:157` / `kernel/mm/vbeam_paging.erl:200` / `kernel/mm/vbeam_paging.erl:138`  
Problem: page table builder allows unconstrained `MaxPhysicalGB`, silently ignores unknown flags, and masks addresses instead of rejecting invalid/reserved bits.  
Fix: enforce `1..512` PDPT range, validate flags per level, and fail on masked-off address bits.

11. `MEDIUM` — `kernel/mm/vbeam_page_alloc.erl:84` / `kernel/mm/vbeam_page_alloc.erl:261`  
Problem: `free_page/2` lacks alignment/bounds checks; bad addresses can free wrong page or crash in `get_bit/2` binary match.  
Fix: require page alignment and `PageNum < total_pages` before bit operations.

12. `HIGH` — `arch/x86_64/vbeam_native_lower_x86_64.erl:905` / `arch/x86_64/vbeam_native_lower_x86_64.erl:940` / `arch/x86_64/vbeam_native_lower_x86_64.erl:948`  
Problem: `int_to_str` stores sign flag in `r13` then reuses `r13` as pointer scratch before sign check.  
Fix: keep sign flag in separate register/stack slot.

13. `HIGH` — `arch/x86_64/vbeam_native_lower_x86_64.erl:419` / `arch/x86_64/vbeam_native_lower_x86_64.erl:422` / `arch/x86_64/vbeam_native_lower_x86_64.erl:436` / `arch/x86_64/vbeam_native_lower_x86_64.erl:440`  
Problem: string-concat loop advances by 1 byte but does 64-bit loads/stores, causing overread/overwrite.  
Fix: use byte load/store sequence (`movzx` + byte store) for per-byte copy.

14. `MEDIUM` — `vm/jit/vbeam_beam_to_native.erl:283`  
Problem: `movabs` immediate uses unsigned binary construction (`<<Val:64/little>>`), unsafe for large negative immediates.  
Fix: use signed encoding (`<<Val:64/little-signed>>`) or explicit two’s-complement conversion.

15. `LOW` — `kernel/arch/vbeam_gdt_idt.erl:12` / `kernel/arch/vbeam_gdt_idt.erl:417`  
Problem: `timer_isr/0` is exported but not used in runtime paths and returns `<<>>`.  
Fix: remove export/function or implement and wire it.

16. `MEDIUM` — `kernel/sched/vbeam_scheduler.erl:346` / `kernel/sched/vbeam_scheduler.erl:367`  
Problem: mailbox stores raw Erlang terms without per-process heap copy semantics; this violates strict process-heap isolation assumptions.  
Fix: copy/serialize into recipient heap on send (or explicitly adopt/document shared-heap semantics and enforce consistently).

Gen_server return contracts: no concrete callback tuple contract violations found in `kernel/io/vbeam_io_server.erl`, `kernel/io/vbeam_irq_bridge.erl`, or `kernel/sched/vbeam_scheduler.erl`.