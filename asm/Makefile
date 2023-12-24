ARMTARGET ?= arm-none-eabi
ASMOPTS   = -Iinc

all: kernel7.img

clean:
	rm -f *.elf
	rm -f *.img
	rm -f *.o

%.s.o: %.s
	$(ARMTARGET)-as $(ASMOPTS) -g -MD $@.d -c $< -o $@

ASM_FILES = $(wildcard *.s)
OBJ_FILES = $(ASM_FILES:%.s=%.s.o)
DEP_FILES = $(OBJ_FILES:%.o=%.o.d)
-include $(DEP_FILES)

kernel7.img: linker.ld $(OBJ_FILES)
	$(ARMTARGET)-ld -T linker.ld -static -g -o kernel7.elf $(OBJ_FILES)
	$(ARMTARGET)-objcopy kernel7.elf -O binary kernel7.img
