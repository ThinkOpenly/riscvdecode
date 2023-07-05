.PHONY: riscv_decode clean

all: riscv_decode

riscv_decode:
	dune build --release
	cp _build/default/riscvdecode.cmxs riscv_decode.cmxs
	chmod +rwx riscv_decode.cmxs

clean:
	-dune clean
