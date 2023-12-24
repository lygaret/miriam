target remote localhost:1234
layout asm
layout reg

display/4xw $sp
break _start
