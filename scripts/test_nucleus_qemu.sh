#!/bin/bash
# Test BEAM kernel nucleus in QEMU
# Requires: qemu-system-x86_64, OVMF firmware (UEFI)
#
# Usage:
#   ./scripts/test_nucleus_qemu.sh          # Non-interactive (auto-verify)
#   ./scripts/test_nucleus_qemu.sh --interactive  # Interactive serial console

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
NUCLEUS_DIR="$PROJECT_ROOT/os/nucleus"
BOOT_DIR="/tmp/beam_os_boot"
SERIAL_LOG="/tmp/nucleus_serial.log"
INTERACTIVE=false

if [ "${1:-}" = "--interactive" ] || [ "${1:-}" = "-i" ]; then
    INTERACTIVE=true
fi

echo "=== BEAM Kernel Nucleus Test ==="
echo

# Compile PE emitter (dependency)
echo "[1/5] Compiling PE emitter..."
mkdir -p "$PROJECT_ROOT/vbeam_rt/ebin"
erlc -o "$PROJECT_ROOT/vbeam_rt/ebin" "$PROJECT_ROOT/vbeam_rt/src/vbeam_native_pe.erl"
echo "✓ PE emitter compiled"

# Build the nucleus
echo "[2/5] Building nucleus.efi..."
cd "$PROJECT_ROOT"
erlc -o "$NUCLEUS_DIR" "$NUCLEUS_DIR/vbeam_font_8x16.erl" 2>/dev/null
erlc -o "$NUCLEUS_DIR" "$NUCLEUS_DIR/vbeam_nucleus_boot.erl" 2>/dev/null
erl -noshell -pa "$NUCLEUS_DIR" -pa vbeam_rt/ebin \
    -eval "vbeam_nucleus_boot:build(\"$NUCLEUS_DIR/nucleus.efi\")" -s init stop

if [ ! -f "$NUCLEUS_DIR/nucleus.efi" ]; then
    echo "✗ Build failed - nucleus.efi not found"
    exit 1
fi

SIZE=$(stat -f%z "$NUCLEUS_DIR/nucleus.efi" 2>/dev/null || stat -c%s "$NUCLEUS_DIR/nucleus.efi")
echo "✓ Built: nucleus.efi ($SIZE bytes)"
echo

# Create FAT disk image with EFI boot structure
echo "[3/5] Creating bootable disk image..."
rm -rf "$BOOT_DIR"
mkdir -p "$BOOT_DIR/EFI/BOOT"
cp "$NUCLEUS_DIR/nucleus.efi" "$BOOT_DIR/EFI/BOOT/BOOTX64.EFI"
printf 'FS0:\\EFI\\BOOT\\BOOTX64.EFI\n' > "$BOOT_DIR/startup.nsh"

# Create FAT32 disk image using hdiutil (macOS) or dd+mkfs (Linux)
DISK_IMG="/tmp/beam_os_boot.img"
rm -f /tmp/beam_os_boot_raw.dmg /tmp/beam_os_boot_rw.dmg /tmp/beam_os_boot_final.cdr "$DISK_IMG"

if command -v hdiutil &>/dev/null; then
    # macOS
    hdiutil create -size 33m -fs FAT32 -volname BEAMOS -layout NONE -o /tmp/beam_os_boot_raw >/dev/null 2>&1
    hdiutil convert /tmp/beam_os_boot_raw.dmg -format UDRW -o /tmp/beam_os_boot_rw >/dev/null 2>&1
    hdiutil attach /tmp/beam_os_boot_rw.dmg -nobrowse -mountpoint /tmp/beam_os_mnt >/dev/null 2>&1
    mkdir -p /tmp/beam_os_mnt/EFI/BOOT
    cp "$BOOT_DIR/EFI/BOOT/BOOTX64.EFI" /tmp/beam_os_mnt/EFI/BOOT/
    cp "$BOOT_DIR/startup.nsh" /tmp/beam_os_mnt/
    hdiutil detach /tmp/beam_os_mnt >/dev/null 2>&1
    hdiutil convert /tmp/beam_os_boot_rw.dmg -format UDTO -o /tmp/beam_os_boot_final >/dev/null 2>&1
    mv /tmp/beam_os_boot_final.cdr "$DISK_IMG"
    rm -f /tmp/beam_os_boot_raw.dmg /tmp/beam_os_boot_rw.dmg
else
    # Linux — use dd + mtools
    dd if=/dev/zero of="$DISK_IMG" bs=1M count=33 2>/dev/null
    mkfs.fat -F 32 "$DISK_IMG" >/dev/null 2>&1
    mmd -i "$DISK_IMG" ::EFI ::EFI/BOOT
    mcopy -i "$DISK_IMG" "$BOOT_DIR/EFI/BOOT/BOOTX64.EFI" ::EFI/BOOT/
    mcopy -i "$DISK_IMG" "$BOOT_DIR/startup.nsh" ::
fi
echo "✓ Disk image: $DISK_IMG"
echo

# Find OVMF firmware
echo "[4/5] Locating UEFI firmware..."
OVMF=""
OVMF_VARS_SRC=""

OVMF_PATHS=(
    "/opt/homebrew/share/qemu/edk2-x86_64-code.fd"
    "/opt/homebrew/Cellar/qemu/*/share/qemu/edk2-x86_64-code.fd"
    "/usr/share/qemu/OVMF.fd"
    "/usr/share/OVMF/OVMF_CODE.fd"
    "/usr/share/edk2/ovmf/OVMF_CODE.fd"
)
VARS_PATHS=(
    "/opt/homebrew/share/qemu/edk2-i386-vars.fd"
    "/usr/share/OVMF/OVMF_VARS.fd"
    "/usr/share/edk2/ovmf/OVMF_VARS.fd"
)

for path in "${OVMF_PATHS[@]}"; do
    for file in $path; do
        if [ -f "$file" ]; then OVMF="$file"; break 2; fi
    done
done
for path in "${VARS_PATHS[@]}"; do
    for file in $path; do
        if [ -f "$file" ]; then OVMF_VARS_SRC="$file"; break 2; fi
    done
done

if [ -z "$OVMF" ]; then
    echo "✗ OVMF firmware not found."
    echo "  Install with: brew install qemu (macOS) or apt install ovmf (Linux)"
    exit 1
fi

# Create writable vars copy
cp "$OVMF_VARS_SRC" /tmp/ovmf_vars.fd 2>/dev/null || cp "$OVMF" /tmp/ovmf_vars.fd

echo "✓ UEFI firmware: $OVMF"
echo

# Run in QEMU
echo "[5/5] Launching QEMU..."
echo "────────────────────────────────────────"

if $INTERACTIVE; then
    echo "Interactive mode — Ctrl+A, X to exit"
    echo "────────────────────────────────────────"
    echo
    qemu-system-x86_64 \
        -machine q35 \
        -m 256M \
        -drive if=pflash,format=raw,readonly=on,file="$OVMF" \
        -drive if=pflash,format=raw,file=/tmp/ovmf_vars.fd \
        -drive format=raw,file="$DISK_IMG" \
        -nographic \
        -no-reboot
else
    # Non-interactive: capture serial to file, timeout, verify
    rm -f "$SERIAL_LOG"
    echo "Non-interactive mode — 10s timeout, serial → $SERIAL_LOG"
    echo "────────────────────────────────────────"

    timeout 10 qemu-system-x86_64 \
        -machine q35 \
        -m 256M \
        -drive if=pflash,format=raw,readonly=on,file="$OVMF" \
        -drive if=pflash,format=raw,file=/tmp/ovmf_vars.fd \
        -drive format=raw,file="$DISK_IMG" \
        -display none \
        -serial file:"$SERIAL_LOG" \
        -no-reboot \
        2>/dev/null || true

    echo
    echo "=== Serial Output ==="
    if [ -f "$SERIAL_LOG" ]; then
        # Strip ANSI escape codes and show readable text
        strings "$SERIAL_LOG" | grep -v '^\[' | grep -v '^$'
        echo

        # Verify expected output
        if strings "$SERIAL_LOG" | grep -q "Loom Kernel"; then
            echo "✓ PASS: Nucleus booted and printed to serial"
            if strings "$SERIAL_LOG" | grep -q "HAL"; then
                echo "✓ PASS: HAL initialization confirmed"
            fi
            exit 0
        else
            echo "✗ FAIL: Expected 'Loom Kernel' in serial output"
            echo "  Raw log: $SERIAL_LOG"
            exit 1
        fi
    else
        echo "✗ FAIL: No serial output captured"
        exit 1
    fi
fi
