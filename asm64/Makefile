BUILD_DIR = build
SRC_DIR   = src

CFLAGS = -W -mcpu=cortex-a53+nosimd -I $(SRC_DIR) -g --gstabs

S_FILES = $(wildcard $(SRC_DIR)/**/*.s)
C_FILES = $(wildcard $(SRC_DIR)/**/*.c)

OBJ_FILES  = $(S_FILES:$(SRC_DIR)/%.s=$(BUILD_DIR)/%.s.o)
OBJ_FILES += $(C_FILES:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.c.o)

DEP_FILES  = $(OBJ_FILES:%.o=%.o.d)

all: kernel8.img

$(BUILD_DIR)/%.s.o: $(SRC_DIR)/%.s
	mkdir -p $(@D)
	aarch64-linux-gnu-as $(CFLAGS) -MD $@.d -o $@ $<

$(BUILD_DIR)/%.c.o: $(SRC_DIR)/%.c
	mkdir -p $(@D)
	aarch64-linux-gnu-as $(CFLAGS) -MD $@.d -o $@ $<

clean:
	rm -rf $(BUILD_DIR) *.img

-include $(DEP_FILES)

kernel8.img: $(OBJ_FILES) $(SRC_DIR)/link.ld
	ld.lld -m aarch64elf -nostdlib $(OBJ_FILES) -T $(SRC_DIR)/link.ld -g -o $(BUILD_DIR)/kernel8.elf
	aarch64-linux-gnu-objcopy -O binary $(BUILD_DIR)/kernel8.elf $@

run: kernel8.img
	qemu-system-aarch64 -S -s \
		-M raspi3b -m 1G \
		-kernel $< \
	        -serial null -serial mon:stdio

debugger: kernel8.img
	aarch64-linux-gnu-gdb -f $(BUILD_DIR)/kernel8.elf

.PHONY: all clean run debugger
