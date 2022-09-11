let () = print_endline "Hi"

(* メモリを初期化 *)
let memory = Rvsim.Memory.of_string "\x93\x00\x01\x01\x94\x00\x01\x01"

(* アドレスを指定して読み込み *)
let test = Rvsim.Memory.read_word memory 0
let _ = Printf.printf "%08x\n" (Int32.to_int test)
let test = Rvsim.Memory.read_word memory 4
let _ = Printf.printf "%08x\n" (Int32.to_int test)

(* アドレス0番地へ書き込み *)
let test =
  Rvsim.Memory.write_word memory 0 0x00ABCDEFl;
  Rvsim.Memory.read_word memory 0

let _ = Printf.printf "%08x\n" (Int32.to_int test)

(* アドレス4番地へ書き込み *)
let test =
  Rvsim.Memory.write_word memory 4 (-1l);
  Rvsim.Memory.read_word memory 4

let _ = Printf.printf "%d\n" (Int32.to_int test)
