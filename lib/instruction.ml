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
