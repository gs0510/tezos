(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module IO = Layers_IO.Unix

module type S = sig
  include Inode.S

  module U : Pack.S

  module L : Pack.S

  type 'a upper = 'a U.t

  val v :
    'a upper ->
    'a upper ->
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    Lwt_mutex.t ->
    bool ->
    IO.t ->
    int64 ->
    int option ->
    int option ->
    'a t Lwt.t

  val batch : unit -> 'a Lwt.t

  val project :
    'a t ->
    [ `Read | `Write ] upper ->
    [ `Read | `Write ] upper ->
    [ `Read | `Write ] t

  val layer_id : [ `Read ] t -> key -> int Lwt.t

  type 'a layer_type =
    | Upper : [ `Read | `Write ] U.t layer_type
    | Lower : [ `Read | `Write ] L.t layer_type

  val check_and_copy :
    'l layer_type * 'l ->
    [ `Read ] t ->
    aux:(value -> unit Lwt.t) ->
    key ->
    unit Lwt.t

  val batch_lower : 'a t -> ([ `Read | `Write ] L.t -> 'b Lwt.t) -> 'b Lwt.t

  val mem_lower : 'a t -> key -> bool Lwt.t

  val mem_current : [> `Read ] t -> key -> bool Lwt.t

  val flip_upper : 'a t -> unit

  val current_upper : 'a t -> 'a U.t

  val ro_sync : 'a t -> bool -> int64 -> unit

  val end_freeze : 'a t -> unit
end

module Make
    (Conf : Inode.CONFIG)
    (H : Irmin.Hash.S)
    (P : Layered.LAYERED_MAKER
           with type key = H.t
            and type index = Pack_index.Make(H).t)
    (Node : Irmin.Private.Node.S with type hash = H.t) :
  S
    with type key = H.t
     and type Val.metadata = Node.metadata
     and type Val.step = Node.step
     and type index = Pack_index.Make(H).t
     and type U.index = Pack_index.Make(H).t
