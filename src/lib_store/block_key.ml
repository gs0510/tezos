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

include Block_hash

(* FIXME the block_hash should be the hash itself *)
(* Overrides [hash] into [Hashtbl.hash] for compliance with index *)
let hash = Hashtbl.hash

let hash_size = 30

let encode bh = Block_hash.to_string bh

let encoded_size = Block_hash.size (* in bytes *)

let decode str off =
  let str = String.sub str off encoded_size in
  Block_hash.of_string_exn str

let of_string_result : string -> (t, [`Msg of string]) result =
 fun s ->
  match of_string s with
  | Ok x ->
      Ok x
  | Error err ->
      Error
        (`Msg
          (Format.asprintf
             "Failed to read b58check_encoding data: %a"
             Error_monad.pp_print_error
             err))

let t : t Irmin.Type.t =
  Irmin.Type.map
    ~pp
    ~of_string:of_string_result
    Irmin.Type.(string_of (`Fixed size))
    of_string_exn
    to_string
