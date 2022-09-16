type t = { mutable pc : int32; memory : Memory.t; x_registers : int32 array }

let create =
  { pc = 0l; memory = Memory.make 512 '\x00'; x_registers = Array.make 32 0l }

(* レジスタの値を取得する *)
(* x0は常に0を返す *)
let get_x_register cpu n = if n = 0 then 0l else Array.get cpu.x_registers n

(* 指定したレジスタに値をセットする *)
(* x0への値のセットは無視される *)
let set_x_register cpu n v = if n = 0 then () else Array.set cpu.x_registers n v
let init_memory cpu data = Memory.init cpu.memory data

(* ADD命令 *)
let exec_add cpu (inst : Instruction.t) =
  let v1 = Array.get cpu.x_registers inst.rs1
  and v2 = Array.get cpu.x_registers inst.rs2 in
  cpu.pc <- Int32.add cpu.pc 4l;
  Array.set cpu.x_registers inst.rd (Int32.add v1 v2)

(* SUB命令 *)
let exec_sub cpu (inst : Instruction.t) =
  let v1 = Array.get cpu.x_registers inst.rs1
  and v2 = Array.get cpu.x_registers inst.rs2 in
  cpu.pc <- Int32.add cpu.pc 4l;
  Array.set cpu.x_registers inst.rd (Int32.sub v1 v2)

let fetch cpu =
  let word = Memory.read_word cpu.memory (Int32.to_int cpu.pc) in
  Instruction.decode word

let exec cpu (inst : Instruction.t) =
  match inst with
  | { opcode = 0b0110011; funct3 = 0x0; funct7 = 0x00; _ } -> exec_add cpu inst
  | { opcode = 0b0110011; funct3 = 0x0; funct7 = 0x20; _ } -> exec_sub cpu inst
  | _ -> failwith "invalid instruction"

let run cpu = exec cpu (fetch cpu)
