type t = { cpu : Cpu.t }

let create = { cpu = Cpu.create }
let init_with_bytes sim data = Cpu.init_memory sim data

let init_with_file sim f =
  let ic = open_in_bin f in
  let len = in_channel_length ic in
  let b = Buffer.create len in
  Buffer.add_channel b ic len;
  Cpu.init_memory sim.cpu (Buffer.to_bytes b)

let start (sim : t) =
  Cpu.run sim.cpu;
  Cpu.run sim.cpu;
  Cpu.run sim.cpu

let dump_register label i32 =
  let i = Int32.to_int i32 in
  Printf.printf "%s = 0x%x (%d)" label i i

let dump_registers (sim : t) =
  let cpu = sim.cpu in
  print_endline "--------------------------------------------------------------------------------";
  for i = 0 to 7 do
    for j = 0 to 3 do
      let n = (i * 4) + j in
      let label = Printf.sprintf "x%02d" n in
      let v = cpu.x_registers.(n) in
      if j != 0 then print_string "\t";
      dump_register label v
    done;
    print_newline ()
  done;
  print_endline "--------------------------------------------------------------------------------";
  dump_register "pc" cpu.pc;
  print_newline ()
