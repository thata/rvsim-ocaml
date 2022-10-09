let write word = (
  print_char (Char.chr (Int32.to_int word));
  flush stdout)

let read () = Int32.of_int (input_byte stdin)
