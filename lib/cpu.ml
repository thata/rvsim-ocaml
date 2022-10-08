type t = {
  mutable pc : int32;
  memory : Memory.t;
  x_registers : int32 array;
  mutable nop_count : int;
}

let create =
  {
    pc = 0l;
    memory = Memory.make 512 '\x00';
    x_registers = Array.make 32 0l;
    nop_count = 0;
  }

(* レジスタの値を取得する *)
(* x0は常に0を返す *)
let get_x_register cpu n = if n = 0 then 0l else Array.get cpu.x_registers n

(* 指定したレジスタに値をセットする *)
(* x0への値のセットは無視される *)
let set_x_register cpu n v = if n = 0 then () else Array.set cpu.x_registers n v
let init_memory cpu data = Memory.init cpu.memory data

(* ADD命令 *)
let exec_add cpu rd rs1 rs2 =
  let rs1v = Array.get cpu.x_registers rs1
  and rs2v = Array.get cpu.x_registers rs2 in
  Array.set cpu.x_registers rd (Int32.add rs1v rs2v);
  cpu.pc <- Int32.add cpu.pc 4l

(* SUB命令 *)
let exec_sub cpu rd rs1 rs2 =
  let rs1v = Array.get cpu.x_registers rs1
  and rs2v = Array.get cpu.x_registers rs2 in
  Array.set cpu.x_registers rd (Int32.sub rs1v rs2v);
  cpu.pc <- Int32.add cpu.pc 4l

(* OR命令 *)
let exec_or cpu rd rs1 rs2 =
  let rs1v = Int32.to_int (Array.get cpu.x_registers rs1)
  and rs2v = Int32.to_int (Array.get cpu.x_registers rs2) in
  Array.set cpu.x_registers rd (Int32.of_int (rs1v lor rs2v));
  cpu.pc <- Int32.add cpu.pc 4l

(* AND命令 *)
let exec_and cpu rd rs1 rs2 =
  let rs1v = Int32.to_int (Array.get cpu.x_registers rs1)
  and rs2v = Int32.to_int (Array.get cpu.x_registers rs2) in
  Array.set cpu.x_registers rd (Int32.of_int (rs1v land rs2v));
  cpu.pc <- Int32.add cpu.pc 4l

(* ADDI命令 *)
let exec_addi cpu rd rs1 i_imm =
  (* 12ビット符号付き整数を32ビットに符号拡張 *)
  let sign_ext i = if i land 0x800 = 0x800 then 0xFFFFF000 lor i else i in
  let rs1v = Array.get cpu.x_registers rs1
  and imm = Int32.of_int (sign_ext i_imm) in
  Array.set cpu.x_registers rd (Int32.add rs1v imm);
  cpu.pc <- Int32.add cpu.pc 4l

(* SLLI命令 *)
let exec_slli cpu rd rs1 i_imm =
  let rs1v = Array.get cpu.x_registers rs1 in
  Array.set cpu.x_registers rd (Int32.shift_left rs1v i_imm);
  cpu.pc <- Int32.add cpu.pc 4l

(* BEQ命令 *)
let exec_beq cpu rs1 rs2 b_imm =
  (* 13ビット符号付き整数を32ビットに符号拡張 *)
  let sign_ext i = if i land 0x1000 = 0x1000 then 0xFFFFE000 lor i else i in
  let rs1v = Int32.to_int (Array.get cpu.x_registers rs1)
  and rs2v = Int32.to_int (Array.get cpu.x_registers rs2) in
  let imm = sign_ext b_imm in
  if rs1v = rs2v then cpu.pc <- Int32.add cpu.pc (Int32.of_int imm)
  else cpu.pc <- Int32.add cpu.pc 4l

(* LW命令 *)
let exec_lw cpu rd rs1 i_imm =
  (* 12ビット符号付き整数を32ビットに符号拡張 *)
  let sign_ext i = if i land 0x800 = 0x800 then 0xFFFFF000 lor i else i in
  let rs1val = Int32.to_int (Array.get cpu.x_registers rs1) in
  let imm = sign_ext i_imm in
  let addr = rs1val + imm in
  let data = Memory.read_word cpu.memory addr in
  Array.set cpu.x_registers rd data;
  cpu.pc <- Int32.add cpu.pc 4l

(* SW命令 *)
let exec_sw cpu rs1 rs2 s_imm =
  (* 12ビット符号付き整数を32ビットに符号拡張 *)
  let sign_ext i = if i land 0x800 = 0x800 then 0xFFFFF000 lor i else i in
  let rs1val = Int32.to_int (Array.get cpu.x_registers rs1) in
  let rs2val = Array.get cpu.x_registers rs2 in
  let imm = sign_ext s_imm in
  let addr = rs1val + imm in
  if addr = 0x10000000 then print_char (Char.chr (Int32.to_int rs2val))
  else Memory.write_word cpu.memory addr rs2val;
  cpu.pc <- Int32.add cpu.pc 4l

let fetch cpu =
  let word = Memory.read_word cpu.memory (Int32.to_int cpu.pc) in
  let inst = Instruction.decode word in
  (* NOPの場合はnop_countをインクリメント *)
  if Instruction.is_nop inst then cpu.nop_count <- cpu.nop_count + 1;
  inst

let exec cpu (inst : Instruction.t) =
  match inst with
  | { opcode = 0b0110011; funct3 = 0x0; funct7 = 0x00; _ } ->
      exec_add cpu inst.rd inst.rs1 inst.rs2
  | { opcode = 0b0110011; funct3 = 0x0; funct7 = 0x20; _ } ->
      exec_sub cpu inst.rd inst.rs1 inst.rs2
  | { opcode = 0b0110011; funct3 = 0x6; funct7 = 0x00; _ } ->
      exec_or cpu inst.rd inst.rs1 inst.rs2
  | { opcode = 0b0110011; funct3 = 0x7; funct7 = 0x00; _ } ->
      exec_and cpu inst.rd inst.rs1 inst.rs2
  | { opcode = 0b0010011; funct3 = 0x0; _ } ->
      exec_addi cpu inst.rd inst.rs1 inst.i_imm
  | { opcode = 0b0010011; funct3 = 0x1; _ } ->
      exec_slli cpu inst.rd inst.rs1 inst.i_imm
  | { opcode = 0b1100011; funct3 = 0x0; _ } ->
      exec_beq cpu inst.rs1 inst.rs2 inst.b_imm
  | { opcode = 0b0000011; funct3 = 0x2; _ } ->
      exec_lw cpu inst.rd inst.rs1 inst.i_imm
  | { opcode = 0b0100011; funct3 = 0x2; _ } ->
      exec_sw cpu inst.rs1 inst.rs2 inst.s_imm
  | _ -> failwith "invalid instruction"

let run cpu =
  let inst = fetch cpu in
  (* NOPが5回来たら実行を終了するためにfalseを返す *)
  if cpu.nop_count >= 5 then false
  else (
    exec cpu inst;
    true)
