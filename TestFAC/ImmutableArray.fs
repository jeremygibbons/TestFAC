module ImmutableArray

open BitRoutines

type t = {
  original_bytes: string;
  edits: Map<int, char>
}

let make bytes =
  { original_bytes = bytes; edits = Map.empty }

let size bytes =
  String.length bytes.original_bytes

let read_byte bytes address =
  if is_out_of_range address (size bytes) then
    failwith "address is out of range"
  else
    let (Byte_address addr) = address in
    let c =
      if  bytes.edits.ContainsKey addr then bytes.edits.[addr]
      else bytes.original_bytes.[addr] in
        int c

let write_byte bytes address value =
  if is_out_of_range address (size bytes) then
    failwith "address is out of range"
  else
    let (Byte_address addr) = address in
    let b = char (byte_of_int value) in
    { bytes with edits = Map.add addr b bytes.edits }

let original bytes =
  { bytes with edits = Map.empty }