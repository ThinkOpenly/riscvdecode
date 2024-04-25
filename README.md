# THIS PROJECT IS DEPRECATED

> [!NOTE]
> This project is deprecated.
> 
> Please see https://github.com/ThinkOpenly/sail/tree/json instead.

# Sail RISC-V extractor

A tool to extract information from the sail-riscv spec.

## Build

```
make
```
This will build a `riscv_decode.cmxs` for use as a `sail` plugin.
Example use below.

## Usage

In general:
```
sail -plugin riscv_decode.cmxs -riscv_decode <files>
```

A specific example follows. To ensure the various RISC-V Sail files are
all included and in the correct order, leverage the `sail-riscv/Makefile`:

1. Clone the sail-riscv repository:
   ```
   $ git clone https://github.com/riscv/sail-riscv.git
   ```

1. Add a `decode` target to the `Makefile`, providing the path to the plugin
   built above:
   ```
   @@ -178,6 +178,10 @@ endif
     
    .PHONY:
     
    +decode: $(SAIL_SRCS) model/main.sail Makefile
    +       sail -dno_cast -no_warn -plugin $(replace with path to)/riscvdecode/riscv_decode.cmxs -riscv_decode $(SAIL_SRCS)
    +
   ```

1. Run it:
   ```
   $ make decode
   ```
