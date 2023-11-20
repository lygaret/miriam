#!/usr/bin/env scheme-script
#!r7rs

(import (miriam assembler))

(assemble '(mov.ne r0 r1 (lsl 8)))
