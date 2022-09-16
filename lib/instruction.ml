type t = {
  opcode : int;
  rd : int;
  funct3 : int;
  rs1 : int;
  rs2 : int;
  funct7 : int;
  i_imm : int;
  s_imm : int;
  b_imm : int;
}

let decode word =
  let wd = Int32.to_int word in
  let opcode = wd land 0x0000007f
  and rd = (wd land 0x00000f80) lsr 7
  and funct3 = (wd land 0x00007000) lsr 12
  and rs1 = (wd land 0x000f8000) lsr 15
  and rs2 = (wd land 0x01f00000) lsr 20
  and funct7 = (wd land 0xfe000000) lsr 25
  and i_imm = (wd land 0xfff00000) lsr 20
  and s_imm = ((wd land 0xfe000000) lsr 20) lor ((wd land 0x00000f80) lsr 7)
  and b_imm =
    ((wd land 0x80000000) lsr 19)
    lor ((wd land 0x00000080) lsl 4)
    lor ((wd land 0x7e000000) lsr 20)
    lor ((wd land 0x00000f00) lsr 7)
  in
  { opcode; rd; funct3; rs1; rs2; funct7; i_imm; s_imm; b_imm }

(* ADD命令 *)
let add rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0x0
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0x00 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* SUB命令 *)
let sub rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0x0
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0x20 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* AND命令 *)
let _and rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0x7
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0x00 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* OR命令 *)
let _or rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0x6
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0b0000000 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* ADDI命令 *)
let addi rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0b000
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0b0000000 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* SLLI命令 *)
let slli rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0b000
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0b0000000 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* BEQ命令 *)
let beq rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0b000
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0b0000000 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* LW命令 *)
let lw rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0b000
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0b0000000 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

(* SW命令 *)
let sw rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0b000
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0b0000000 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)
