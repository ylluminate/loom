Baseline: `make compile && make check && make test` passed.

CRITICAL|arch/x86_64/vbeam_native_lower_x86_64.erl:269|Shift lowering unconditionally moves count into rcx; when Dst is rcx, the value being shifted is overwritten before shl/shr/sar.|Use dedicated scratch for CL-based shifts (spill/swap when Dst=rcx) and model RCX as implicit clobber.
CRITICAL|arch/x86_64/vbeam_native_lower_x86_64.erl:238|sdiv/srem hardcodes rax/rdx and runs idiv(B) without operand constraints; if divisor or live values are in rax/rdx, operands are destroyed.|Add fixed-register constraints for div/rem, force divisor into safe temp, preserve/forbid rax/rdx.
HIGH|arch/x86_64/vbeam_native_lower_x86_64.erl:68|Prologue does sub rsp then pushes callee-saved; odd counts leave rsp misaligned at call sites.|Push callee-saved first then subtract aligned spill space so rsp is 16-byte aligned before call.
HIGH|arch/x86_64/vbeam_native_lower_x86_64.erl:133|Stack-slot addressing inconsistent: load adds UsedCalleeSaved offset but mov does not.|Unify one stack-frame layout formula for all {stack,Slot} accesses.
HIGH|arch/ir/vbeam_native_regalloc.erl:296|Call-site detection ignores method_call; live ranges spanning method calls not treated as call-clobbered.|Treat method_call as call in liveness/call-aware allocation.
HIGH|arch/x86_64/vbeam_native_lower_x86_64.erl:1282|Argument cycle breaker handles only first cycle; additional cycles can overwrite sources.|Implement full cycle decomposition and break every cycle.
HIGH|arch/arm64/vbeam_native_lower_arm64.erl:1364|ARM64 argument cycle breaker has same single-cycle logic flaw.|Use full cycle decomposition and break each cycle independently with x16 temp.
HIGH|arch/arm64/vbeam_native_lower_arm64.erl:615|print_str writes syscall number into SysNumReg (x8) without save/restore; x8 can hold live allocator-assigned values.|Save/restore SysNumReg in print_str or reserve x8 in scratch exclusion.
MEDIUM|arch/x86_64/vbeam_native_lower_x86_64.erl:332|Immediate push lowering calls encode_push_imm32/1 which doesn't exist in vbeam_native_x86_64.|Add and export encode_push_imm32/1 in vbeam_native_x86_64.erl.
MEDIUM|kernel/boot/vbeam_boot_sequence.erl:54|Boot code reprograms GDT/IDT/PIC/PIT without clearing IF first; interrupts during transient state can triple-fault.|Emit cli at boot entry, only sti after full setup complete.
