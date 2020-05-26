let src = Logs.Src.create "irmin.layers.io" ~doc:"IO for irmin-layers"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type t

  val v : string -> t

  val close : t -> unit

  val read_flip : t -> bool

  val write_flip : bool -> t -> unit

  val read_generation : t -> int64

  val write_generation : int64 -> t -> unit
end

module Unix : S = struct
  type t = { file : string; fd : Unix.file_descr }

  let write ~offset t buf =
    let off = Unix.lseek t.fd offset Unix.SEEK_SET in
    assert (off = offset);
    let len = Bytes.length buf in
    let n = Unix.write t.fd buf 0 len in
    assert (n = len)

  let read ~offset t buf =
    let off = Unix.lseek t.fd offset Unix.SEEK_SET in
    assert (off = offset);
    let len = Bytes.length buf in
    let n = Unix.read t.fd buf 0 len in
    assert (n = len)

  let close t = Unix.close t.fd

  let write_flip flip t =
    let buf =
      if flip then Bytes.make 1 (char_of_int 1)
      else Bytes.make 1 (char_of_int 0)
    in
    write ~offset:0 t buf

  let read_flip t =
    let buf = Bytes.create 1 in
    read ~offset:0 t buf;
    let ch = Bytes.get buf 0 in
    match int_of_char ch with
    | 0 -> false
    | 1 -> true
    | d -> failwith ("corrupted flip file " ^ string_of_int d)

  let read_generation t =
    let buf = Bytes.create 8 in
    read ~offset:8 t buf;
    match Irmin.Type.(of_bin_string int64) (Bytes.unsafe_to_string buf) with
    | Ok t -> t
    | Error (`Msg e) -> Fmt.failwith "get_generation: %s" e

  let write_generation v t =
    let gen = Irmin.Type.(to_bin_string int64) v in
    let buf = Bytes.unsafe_of_string gen in
    write ~offset:8 t buf

  let v file =
    match Sys.file_exists file with
    | false ->
        let fd = Unix.openfile file Unix.[ O_CREAT; O_RDWR; O_CLOEXEC ] 0o644 in
        let t = { file; fd } in
        write_flip true t;
        write_generation 1L t;
        t
    | true ->
        let fd = Unix.openfile file Unix.[ O_EXCL; O_RDWR; O_CLOEXEC ] 0o644 in
        { file; fd }
end
