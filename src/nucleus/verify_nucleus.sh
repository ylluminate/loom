#!/bin/bash
# Quick verification script for nucleus.efi

set -e

NUCLEUS="os/nucleus/nucleus.efi"

if [ ! -f "$NUCLEUS" ]; then
    echo "✗ $NUCLEUS not found. Run: make nucleus"
    exit 1
fi

echo "=== Nucleus Verification ==="
echo

echo "[1/3] File info:"
file "$NUCLEUS"
echo "Size: $(stat -f%z "$NUCLEUS" 2>/dev/null || stat -c%s "$NUCLEUS") bytes"
echo

echo "[2/3] PE header check:"
if xxd -l 2 "$NUCLEUS" | grep -q "4d5a"; then
    echo "✓ DOS header magic (MZ) found"
else
    echo "✗ Invalid DOS header"
    exit 1
fi

if xxd -s 64 -l 4 "$NUCLEUS" | grep -q "5045 0000"; then
    echo "✓ PE signature (PE\\0\\0) found at offset 64"
else
    echo "✗ Invalid PE signature"
    exit 1
fi
echo

echo "[3/3] Machine type:"
# Machine type is at offset 68, 2 bytes, little-endian
# For x86-64, should be 0x8664 (stored as 64 86 in little-endian)
MACHINE_BYTES=$(xxd -s 68 -l 2 "$NUCLEUS" | awk '{print $2$3}')
# Swap bytes for little-endian reading
MACHINE="${MACHINE_BYTES:2:2}${MACHINE_BYTES:0:2}"
if [ "$MACHINE" = "8664" ]; then
    echo "✓ Machine type: 0x8664 (x86-64)"
else
    echo "✗ Unexpected machine type: 0x$MACHINE (raw: $MACHINE_BYTES)"
    exit 1
fi
echo

echo "=== Verification passed! ==="
echo
echo "To test in QEMU:"
echo "  ./scripts/test_nucleus_qemu.sh"
