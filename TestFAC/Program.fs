open BitRoutines
open ImmutableArray

[<EntryPoint>]
let main argv = 
   let addr1 = Byte_address 1 in
    let bytes_a = ImmutableArray.make "Hello world" in
    let bytes_b = ImmutableArray.write_byte bytes_a addr1 65 in
    let b_a = ImmutableArray.read_byte bytes_a addr1 in
    let b_b = ImmutableArray.read_byte bytes_b addr1 in
    Printf.printf "%d %d\n" b_a b_b;
    0
