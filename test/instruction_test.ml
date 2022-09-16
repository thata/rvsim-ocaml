(* R形式 *)
let _ =
  let word = 0b1000001_10111_10011_101_10001_0110011_l in
  let inst = Rvsim.Instruction.decode word in
  Printf.printf "%d\n" inst.opcode;
  (* => 51 *)
  Printf.printf "%d\n" inst.rd;
  (* => 17 *)
  Printf.printf "%d\n" inst.funct3;
  (* => 5 *)
  Printf.printf "%d\n" inst.rs1;
  (* => 19 *)
  Printf.printf "%d\n" inst.rs2;
  (* => 23 *)
  Printf.printf "%d\n" inst.funct7
(* => 65 *)

(* I形式 *)
let _ =
  let word = 0b100000000001_10011_101_10001_0010011_l in
  let inst = Rvsim.Instruction.decode word in
  Printf.printf "%d\n" inst.opcode;
  (* => 19 *)
  Printf.printf "%d\n" inst.rd;
  (* => 17 *)
  Printf.printf "%d\n" inst.funct3;
  (* => 5 *)
  Printf.printf "%d\n" inst.rs1;
  (* => 19 *)
  Printf.printf "%d\n" inst.i_imm
(* => 2049 *)

(* S形式 *)
let _ =
  let word = 0b1000001_10111_10011_101_10001_0100011_l in
  let inst = Rvsim.Instruction.decode word in
  Printf.printf "%d\n" inst.opcode;
  (* => 35 *)
  Printf.printf "%d\n" inst.funct3;
  (* => 5 *)
  Printf.printf "%d\n" inst.rs1;
  (* => 19 *)
  Printf.printf "%d\n" inst.rs2;
  (* => 23 *)
  Printf.printf "%d\n" inst.s_imm
(* => 2097 *)

(* B形式 *)
let _ =
  let word = 0b1_100001_10111_10011_101_1001_1_1100011_l in
  let inst = Rvsim.Instruction.decode word in
  Printf.printf "%d\n" inst.opcode;
  (* => 99 *)
  Printf.printf "%d\n" inst.funct3;
  (* => 5 *)
  Printf.printf "%d\n" inst.rs1;
  (* => 19 *)
  Printf.printf "%d\n" inst.rs2;
  (* => 23 *)
  Printf.printf "%d\n" inst.b_imm
(* => 7218 *)
