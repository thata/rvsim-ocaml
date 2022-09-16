(* ADD命令組み立てのためのユーティリティメソッド *)
let _add rd rs1 rs2 =
  let _op = 0b0110011
  and _rd = rd lsl 7
  and _f3 = 0b000
  and _rs1 = rs1 lsl 15
  and _rs2 = rs2 lsl 20
  and _f7 = 0b0000000 in
  Int32.of_int (_op lor _rd lor _f3 lor _rs1 lor _rs2 lor _f7)

let print_int32 i = print_int (Int32.to_int i)
let cpu = Rvsim.Cpu.create

(* 簡単な命令を実行してみる *)
let _ =
  let data =
    let data = Bytes.create 8 in
    Bytes.set_int32_le data 0 (_add 4 1 2);
    (* x4 = x1 + x2 *)
    Bytes.set_int32_le data 4 (_add 5 4 3);
    (* x5 = x4 + x3 *)
    data
  in
  (* 初期化 *)
  Array.set cpu.x_registers 1 10l;
  (* x1 = 10 *)
  Array.set cpu.x_registers 2 20l;
  (* x2 = 20 *)
  Array.set cpu.x_registers 3 30l;
  (* x3 = 30 *)
  Rvsim.Cpu.init_memory cpu data;
  (* 実行前 *)
  print_int32 cpu.pc;
  print_newline ();
  (* => 0 *)
  print_int32 (Array.get cpu.x_registers 1);
  print_newline ();
  (* => 10 *)
  print_int32 (Array.get cpu.x_registers 2);
  print_newline ();
  (* => 20 *)
  print_int32 (Array.get cpu.x_registers 3);
  print_newline ();
  (* => 30 *)
  print_int32 (Array.get cpu.x_registers 4);
  print_newline ();
  (* => 0 *)
  print_int32 (Array.get cpu.x_registers 5);
  print_newline ();
  (* => 0 *)
  Rvsim.Cpu.run cpu;
  (* 1つめの命令を実行 *)
  Rvsim.Cpu.run cpu;
  (* 2つめの命令を実行 *)
  (* 実行後 *)
  print_int32 cpu.pc;
  print_newline ();
  (* => 8 *)
  print_int32 (Array.get cpu.x_registers 4);
  print_newline ();
  (* => 30 *)
  print_int32 (Array.get cpu.x_registers 5);
  print_newline ()
(* => 60 *)
