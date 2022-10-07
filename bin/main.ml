open Rvsim

let usage () = print_endline "usage: rvsim path/to/rom_file"

let () =
  let sim = Simulator.create in
  if Array.length Sys.argv != 2 then usage ()
  else (
    Simulator.init_with_file sim Sys.argv.(1);
    Simulator.start sim;
    Simulator.dump_registers sim)
