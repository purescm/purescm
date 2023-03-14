#!/usr/bin/env sh

SRC_DIR="src"
OUT_DIR="out"

set -e

if [[ -z "${SCHEME}" ]]; then
    SCHEME="chez"
else
    SCHEME="${SCHEME}"
fi

if [ ! -d "${OUT_DIR}" ]; then
  mkdir -p "${OUT_DIR}"
fi

# Building

echo | $SCHEME --libdirs $SRC_DIR -q << EOF
; setup compiler flags...
(generate-covin-files #f)
(compile-profile #f)
(debug-level 0)
(cp0-effort-limit 1)
(cp0-score-limit 1)
(cp0-outer-unroll-limit 1)
(compile-interpret-simple #f)
(enable-cross-library-optimization #t)
(generate-inspector-information #f)
(generate-procedure-source-information #t)
(generate-interrupt-trap #t)

; compile the program
(compile-library "${SRC_DIR}/fac.ss" "${OUT_DIR}/fac.so")
(compile-program "${SRC_DIR}/main.ss" "${OUT_DIR}/main.so")
EOF

# Running

$SCHEME --libdirs $OUT_DIR --program ./out/main.so

# Note how in both building and running, we specified the libdirs parameter.
#
# For distributing binaries, we would want to do the following:
#
# 1. Install built objects somewhere, usually this would be `$PREFIX/lib/`
# 2. Generate a shell script that passes libdirs, this would be in `$PREFIX/bin`
