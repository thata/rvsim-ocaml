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

(* 簡単な実行のテスト *)
let test_run () =
  let data =
    let data = Bytes.create 8 in
    Bytes.set_int32_le data 0 (_add 4 1 2);
    (* x4 = x1 + x2 *)
    Bytes.set_int32_le data 4 (_add 5 4 3);
    (* x5 = x4 + x3 *)
    data
  in
  let cpu = Rvsim.Cpu.create in
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
  (* => 0 *)
  print_newline ();
  print_int32 (Array.get cpu.x_registers 1);
  (* => 10 *)
  print_newline ();
  print_int32 (Array.get cpu.x_registers 2);
  (* => 20 *)
  print_newline ();
  print_int32 (Array.get cpu.x_registers 3);
  (* => 30 *)
  print_newline ();
  print_int32 (Array.get cpu.x_registers 4);
  (* => 0 *)
  print_newline ();
  print_int32 (Array.get cpu.x_registers 5);
  (* => 0 *)
  print_newline ();
  (* 1つめの命令を実行 *)
  Rvsim.Cpu.run cpu;
  (* 2つめの命令を実行 *)
  Rvsim.Cpu.run cpu;
  (* 実行後 *)
  print_int32 cpu.pc;
  (* => 8 *)
  print_newline ();
  print_int32 (Array.get cpu.x_registers 4);
  (* => 30 *)
  print_newline ();
  print_int32 (Array.get cpu.x_registers 5);
  (* => 60 *)
  print_newline ()

let test_exec_add () =
  let cpu = Rvsim.Cpu.create in
  Array.set cpu.x_registers 1 10l;
  (* x1 = 10 *)
  Array.set cpu.x_registers 2 20l;
  (* x2 = 20 *)
  Array.set cpu.x_registers 3 (-10l);
  (* x3 = -10 *)
  Rvsim.Cpu.exec_add cpu 4 1 2;
  print_int32 (Array.get cpu.x_registers 4);
  (* => 30 *)
  print_newline ();
  Rvsim.Cpu.exec_add cpu 5 0 3;
  (* x5 = x0 + x3 *)
  print_int32 (Array.get cpu.x_registers 5);
  (* => -10 *)
  print_newline ()

let test_exec_sub () =
  let cpu = Rvsim.Cpu.create in
  Array.set cpu.x_registers 1 10l;
  (* x1 = 10 *)
  Array.set cpu.x_registers 2 20l;
  (* x2 = 20 *)
  Array.set cpu.x_registers 3 (-10l);
  (* x3 = -10 *)
  Rvsim.Cpu.exec_sub cpu 4 2 1; (* x4 = x2 - x1 *)
  print_int32 (Array.get cpu.x_registers 4);
  (* => 10 *)
  print_newline ();
  Rvsim.Cpu.exec_sub cpu 5 2 3;
  (* x5 = x2 - x3 *)
  print_int32 (Array.get cpu.x_registers 5);
  (* => 30 *)
  print_newline ()

let _ =
  test_run ();
  test_exec_add ();
  test_exec_sub ()
