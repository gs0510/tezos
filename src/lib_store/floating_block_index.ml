(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Invariant: a maximum of [max_predecessors] predecessors at all
   time: if a block has fewer than 12 predecessors then it is padded
   so its size remain constant. *)
module Block_info = struct
  type t = {offset : int; predecessors : Block_key.t list} [@@deriving repr]

  let max_predecessors = 12

  let (offset_len, offset_encoding) = (4, Data_encoding.int31)

  let variable_encoded_list_size = Block_hash.size * max_predecessors

  let ( predecessors_size_len,
        predecessors_size_kind,
        predecessors_size_encoding ) =
    if variable_encoded_list_size <= 0xff then (1, `Uint8, Data_encoding.int8)
    else if variable_encoded_list_size <= 0xffff then
      (2, `Uint16, Data_encoding.uint16)
    else (4, `Uint30, Data_encoding.int31)

  let encoded_list_size = predecessors_size_len + variable_encoded_list_size

  let encoded_size = offset_len + encoded_list_size

  let encoding =
    let open Data_encoding in
    conv
      (fun {offset; predecessors} -> (offset, predecessors))
      (fun (offset, predecessors) -> {offset; predecessors})
      (obj2
         (req "offset" offset_encoding)
         (req
            "predecessors"
            (dynamic_size
               ~kind:predecessors_size_kind
               (Variable.list ~max_length:max_predecessors Block_hash.encoding))))

  let encode v =
    let bytes = Data_encoding.Binary.to_bytes_exn encoding v in
    let padding = encoded_size - Bytes.length bytes in
    Bytes.unsafe_to_string (Bytes.cat bytes (Bytes.create padding))

  let decode str i =
    let bytes = Bytes.sub (Bytes.unsafe_of_string str) i encoded_size in
    (* Remove the padding if need be *)
    let unpadded_bytes =
      let len_list = Bytes.sub bytes offset_len predecessors_size_len in
      let len =
        Data_encoding.(Binary.of_bytes_exn predecessors_size_encoding len_list)
      in
      Bytes.sub bytes 0 (offset_len + predecessors_size_len + len)
    in
    Data_encoding.Binary.of_bytes_exn encoding unpadded_bytes

  let pp fmt v =
    let open Format in
    fprintf
      fmt
      "@[offset: %d, predecessors : [ @[<hov>%a @]]@]"
      v.offset
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " ;@,") Block_hash.pp)
      v.predecessors
end

(* Hashmap from block's hashes to location *)
include Index_unix.Make (Block_key) (Block_info) (Index.Cache.Unbounded)
