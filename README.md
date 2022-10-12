# RISC-V simulator written in OCaml

<img width="682" alt="image" src="https://user-images.githubusercontent.com/15457/194853471-938b0105-579a-41c7-81b8-e8a103710179.png">

## Usage

```sh
$ git clone https://github.com/thata/rvsim-ocaml.git
$ cd rvsim-ocaml/
$ dune exec rvsim sample/hello.rom
HELLO WORLD!!
--------------------------------------------------------------------------------
x00 = 0x0 (0)	x01 = 0x0 (0)	x02 = 0x0 (0)	x03 = 0x10000000 (268435456)
x04 = 0x0 (0)	x05 = 0xa (10)	x06 = 0x0 (0)	x07 = 0x0 (0)
x08 = 0x0 (0)	x09 = 0x0 (0)	x10 = 0x0 (0)	x11 = 0x0 (0)
x12 = 0x0 (0)	x13 = 0x0 (0)	x14 = 0x0 (0)	x15 = 0x0 (0)
x16 = 0x0 (0)	x17 = 0x0 (0)	x18 = 0x0 (0)	x19 = 0x0 (0)
x20 = 0x0 (0)	x21 = 0x0 (0)	x22 = 0x0 (0)	x23 = 0x0 (0)
x24 = 0x0 (0)	x25 = 0x0 (0)	x26 = 0x0 (0)	x27 = 0x0 (0)
x28 = 0x0 (0)	x29 = 0x0 (0)	x30 = 0x0 (0)	x31 = 0x0 (0)
--------------------------------------------------------------------------------
pc = 0x88 (136)
$
```

Run tests

```sh
$ dune test && echo ok
ok
$
```

## How to make a RISC-V simulator in OCaml
https://scrapbox.io/htkymtks/OCaml%E3%81%A7%E3%82%82%E3%81%A7%E3%81%8D%E3%82%8BRISC-V%E3%82%B7%E3%83%9F%E3%83%A5%E3%83%AC%E3%83%BC%E3%82%BF%E3%81%AE%E4%BD%9C%E3%82%8A%E6%96%B9

## License
This software is released under the MIT License, see LICENSE.

## Supported Instructions
- RV32I Subset
  - add (ADD)
  - sub (SUB)
  - or (OR)
  - and (AND)
  - addi (ADD Immediate)
  - beq (Branch ==)
  - slli (Shift Left Logical Immediate)
  - lw (Load Word)
  - sw (Store Word)

## How to build .rom file

```sh
# Pull Docker image
$ docker pull kamiyaowl/riscv-gnu-toolchain-docker

# Build
$ cd ~/src/rvsim-ocaml/sample/
$ docker run --rm \
  -v $PWD:/usr/src/myapp \
  -w /usr/src/myapp kamiyaowl/riscv-gnu-toolchain-docker \
  /opt/riscv/bin/riscv32-unknown-elf-gcc \
  -march=rv32i -mabi=ilp32 -Wl,-Ttext=0x00 -nostdlib -o hello.elf hello.S
$ docker run --rm \
  -v $PWD:/usr/src/myapp \
  -w /usr/src/myapp kamiyaowl/riscv-gnu-toolchain-docker \
  /opt/riscv/bin/riscv32-unknown-elf-objcopy -O binary hello.elf hello.rom

# Run
$ cd ..
$ dune exec rvsim sample/hello.rom
HELLO WORLD!!
--------------------------------------------------------------------------------
x00 = 0x0 (0)	x01 = 0x0 (0)	x02 = 0x0 (0)	x03 = 0x10000000 (268435456)
x04 = 0x0 (0)	x05 = 0xa (10)	x06 = 0x0 (0)	x07 = 0x0 (0)
x08 = 0x0 (0)	x09 = 0x0 (0)	x10 = 0x0 (0)	x11 = 0x0 (0)
x12 = 0x0 (0)	x13 = 0x0 (0)	x14 = 0x0 (0)	x15 = 0x0 (0)
x16 = 0x0 (0)	x17 = 0x0 (0)	x18 = 0x0 (0)	x19 = 0x0 (0)
x20 = 0x0 (0)	x21 = 0x0 (0)	x22 = 0x0 (0)	x23 = 0x0 (0)
x24 = 0x0 (0)	x25 = 0x0 (0)	x26 = 0x0 (0)	x27 = 0x0 (0)
x28 = 0x0 (0)	x29 = 0x0 (0)	x30 = 0x0 (0)	x31 = 0x0 (0)
--------------------------------------------------------------------------------
pc = 0x88 (136)
```
