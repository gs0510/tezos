(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Tarides <contact@tarides.com>                          *)
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

let term_name = "storage"

let ( // ) = Filename.concat

module Term = struct
  open Cmdliner
  open Node_shared_arg.Term

  (* [Cmdliner] terms are not nestable, so we implement an ad-hoc mechanism for
     delegating to one of several "subcommand"s by parsing a single positional
     argument and then calling [Term.eval] again with the remaining
     arguments. *)

  type subcommand = {
    name : string;
    description : string;
    term : [`Error of bool * string | `Ok of unit] Term.t;
  }

  let auto_repair =
    let open Cmdliner.Arg in
    value
    & flag
      @@ info
           ~doc:"Automatically repair issues; option for integrity-check"
           ["auto-repair"]

  let read_config_file config_file =
    match config_file with
    | Some config_file when Sys.file_exists config_file ->
        Node_config_file.read config_file
    | _ ->
        return Node_config_file.default_config

  let ensure_context_dir context_dir =
    Lwt.catch
      (fun () ->
        Lwt_unix.file_exists context_dir
        >>= function
        | false ->
            fail (Node_data_version.Invalid_data_dir context_dir)
        | true -> (
            let pack = context_dir // "store.pack" in
            Lwt_unix.file_exists pack
            >>= function
            | false ->
                fail (Node_data_version.Invalid_data_dir context_dir)
            | true ->
                return_unit ))
      (function
        | Unix.Unix_error _ ->
            fail (Node_data_version.Invalid_data_dir context_dir)
        | exc ->
            raise exc)

  let root config_file data_dir =
    read_config_file config_file
    >>=? fun cfg ->
    let data_dir = Option.value ~default:cfg.data_dir data_dir in
    let context_dir = Node_data_version.context_dir data_dir in
    ensure_context_dir context_dir >>=? fun () -> return context_dir

  let run main_promise =
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit main_promise with
    | Ok () ->
        `Ok ()
    | Error err ->
        `Error (false, Format.asprintf "%a" pp_print_error err)

  let integrity_check =
    let open Term in
    const (fun config_file data_dir auto_repair ->
        let main =
          root config_file data_dir
          >>=? fun root ->
          Context.Checks.Pack.Integrity_check.run ~root ~auto_repair
          >>= fun () -> return_unit
        in
        run main)
    $ config_file $ data_dir $ auto_repair

  let stat_index =
    let open Term in
    const (fun config_file data_dir ->
        let main =
          root config_file data_dir
          >>=? fun root ->
          Context.Checks.Index.Stat.run ~root ;
          return_unit
        in
        run main)
    $ config_file $ data_dir

  let stat_pack =
    let open Term in
    const (fun config_file data_dir ->
        let main =
          root config_file data_dir
          >>=? fun root ->
          Context.Checks.Pack.Stat.run ~root >>= fun () -> return_unit
        in
        run main)
    $ config_file $ data_dir

  let dest =
    let open Cmdliner.Arg in
    value
    & opt (some string) None
      @@ info
           ~doc:"Path to the new index file; option for reconstruct-index"
           ~docv:"DEST"
           ["output"; "o"]

  let reconstruct_index =
    let open Term in
    const (fun config_file data_dir output ->
        let main =
          root config_file data_dir
          >>=? fun root ->
          Context.Checks.Pack.Reconstruct_index.run ~root ~output ;
          return_unit
        in
        run main)
    $ config_file $ data_dir $ dest

  let heads =
    let open Cmdliner.Arg in
    value
    & opt (some string) None
      @@ info
           ~doc:"Heads; option for integrity-check-inodes"
           ~docv:"HEADS"
           ["heads"; "h"]

  let head_hash config data_dir block =
    let context_root = Node_data_version.context_dir data_dir in
    let store_root = Node_data_version.store_dir data_dir in
    Store.init ~mapsize:40_960_000_000L store_root
    >>=? fun store ->
    let genesis = config.Node_config_file.blockchain_network.genesis in
    State.init ~context_root ~store_root genesis
    >>=? fun (state, chain_state, _context_index, _history_mode) ->
    let chain_id = Chain_id.of_block_hash genesis.block in
    let chain_store = Store.Chain.get store chain_id in
    let chain_data_store = Store.Chain_data.get chain_store in
    Snapshots.parse_block chain_state chain_data_store genesis block
    >>= fun b -> let str = Block_hash.to_string b in Printf.printf "context hash %s\n%!" str;
    Store.close store ;
    State.close state >>= fun () -> return str
(*    let store_dir = Node_data_version.store_dir data_dir in
    let context_dir = Node_data_version.context_dir data_dir in
    Store.init ~store_dir ~context_dir ~allow_testchains:true genesis
    >>= fun store ->
    let store = Result.get_ok store in
    let chain_store = Store.main_chain_store store in
    Store.Chain.current_head chain_store
    >|= fun b -> Block_hash.to_hex (Store.Block.hash b) |> Hex.show*)

  let integrity_check_inodes =
    let open Term in
    const (fun config_file data_dir ->
        let main =
          root config_file data_dir
          >>=? fun root ->
          let heads =
            Some ["CoV5KDUMDSxogp6QWTMxhBAw3Bjp2LfWcA95qLLusfQUA3neKC3y"]
          in
          Context.Checks.Pack.Integrity_check_inodes.run ~root ~heads
          >>= fun () -> return_unit
        in
        run main)
    $ config_file $ data_dir

  let terms =
    [ {
        name = "integrity-check";
        description = "Search the store for integrity faults and corruption.";
        term = integrity_check;
      };
      {
        name = "stat-index";
        description = "Print high-level statistics about the index store.";
        term = stat_index;
      };
      {
        name = "stat-pack";
        description = "Print high-level statistics about the pack file.";
        term = stat_pack;
      };
      {
        name = "reconstruct-index";
        description = "Reconstruct index from pack file.";
        term = reconstruct_index;
      };
      {
        name = "integrity-check-inodes";
        description = "Search the store for corrupted inodes.";
        term = integrity_check_inodes;
      } ]

  let dispatch_subcommand _ _ _ _ _ = function
    | None ->
        `Help (`Auto, Some term_name)
    | Some n -> (
      match List.find_opt (fun {name; _} -> name = n) terms with
      | None ->
          let msg =
            let pp_ul = Fmt.(list ~sep:cut (const string "- " ++ string)) in
            terms
            |> List.map (fun {name; _} -> name)
            |> Fmt.str
                 "@[<v 0>Unrecognized command: %s@,\
                  @,\
                  Available commands:@,\
                  %a@,\
                  @]"
                 n
                 pp_ul
          in
          `Error (false, msg)
      | Some command -> (
          let (binary_name, argv) =
            (* Get remaining arguments for subcommand evaluation *)
            ( Sys.argv.(0),
              Array.init
                (Array.length Sys.argv - 2)
                (function 0 -> Sys.argv.(0) | i -> Sys.argv.(i + 2)) )
          in
          let noop_formatter =
            Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())
          in
          Term.eval
            ~argv
            ~err:noop_formatter (* Defaults refer to non-existent help *)
            ~catch:false (* Will be caught by parent [Term.eval_choice] *)
            ( command.term,
              Term.info (binary_name ^ " " ^ term_name ^ " " ^ command.name) )
          |> function
          | `Ok (`Ok ()) ->
              `Ok ()
          | `Ok (`Error e) ->
              `Error e
          | `Help | `Version ->
              (* Parent term evaluation intercepts [--help] and [--version] *)
              assert false
          | `Error _ -> (
              (* We want to display the usage information for the selected
                 subcommand, but [Cmdliner] will only do this at evaluation
                 time *)
              Term.eval
                ~argv:[|""; "--help=plain"|]
                ( command.term,
                  Term.info (binary_name ^ " " ^ term_name ^ " " ^ command.name)
                )
              |> function `Help -> `Ok () | _ -> assert false ) ) )

  let term =
    let subcommand =
      (* NOTE: [Cmdliner] doesn't have a wildcard argument or mechanism for
         deferring the parsing of arguments, so this term must explicitly
         support any options required by the subcommands *)
      Arg.(value @@ (pos 0) (some string) None (info ~docv:"COMMAND" []))
    in
    Term.(
      ret
        ( const dispatch_subcommand $ config_file $ data_dir $ auto_repair
        $ dest $ heads $ subcommand ))
end

module Manpage = struct
  let command_description =
    "The $(b,storage) command provides tools for introspecting and debugging \
     the storage layer."

  let commands =
    [ `S Cmdliner.Manpage.s_commands;
      `P "The following subcommands are available:";
      `Blocks
        (List.map
           (fun Term.{name; description; _} ->
             `I (Printf.sprintf " $(b,%s)" name, description))
           Term.terms);
      `P
        "$(b,WARNING): this API is experimental and may change in future \
         versions." ]

  let man = commands @ Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Query the storage layer (EXPERIMENTAL)"
      ~man
      term_name
end

let cmd = (Term.term, Manpage.info)
