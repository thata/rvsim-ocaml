type t = { data : bytes }

let create n = { data = Bytes.create n }
let make n c = { data = Bytes.make n c }
let of_string str = { data = Bytes.of_string str }
let init memory data = Bytes.blit data 0 memory.data 0 (Bytes.length data)

let read_word memory addr =
  let b0 = Bytes.get_uint8 memory.data (addr + 0)
  and b1 = Bytes.get_uint8 memory.data (addr + 1)
  and b2 = Bytes.get_uint8 memory.data (addr + 2)
  and b3 = Bytes.get_uint8 memory.data (addr + 3) in
  let word = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
  Int32.of_int word

let write_word memory addr word =
  let wd = Int32.to_int word in
  let b0 = wd land 0xff
  and b1 = (wd land 0xff00) lsr 8
  and b2 = (wd land 0xff0000) lsr 16
  and b3 = (wd land 0xff000000) lsr 24 in
  ignore (Bytes.set_uint8 memory.data (addr + 3) b3);
  ignore (Bytes.set_uint8 memory.data (addr + 2) b2);
  ignore (Bytes.set_uint8 memory.data (addr + 1) b1);
  ignore (Bytes.set_uint8 memory.data (addr + 0) b0)
