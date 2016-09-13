open Protocol_9p
open Infix
open Result
open Lwt.Infix

module Log = (val Logs.src_log Logs.default)
module Client = Client9p_unix.Make (Log)

let () = Logs.set_reporter (Logs_fmt.reporter ())

type 'a error_lwt = 'a Error.t Lwt.t

let return_error msg = Error (`Msg msg) |> Lwt.return

let return_ok v = Lwt.return (Ok v)

module State : sig
  val is_connected : unit -> bool

  val cwd : unit -> string list

  val connect : username:string -> string -> string -> unit error_lwt

  val disconnect : unit -> unit Lwt.t

  val client : unit -> Client.t error_lwt

  val is_dir : string list -> bool error_lwt

  val set_cwd : string list -> unit error_lwt
end = struct
  type t = {
    client_o : Client.t option;
    cwd : string list;
  }

  let empty = {
    client_o = None;
    cwd = [];
  }

  let r = ref empty

  let cwd () = !r.cwd

  let is_connected () = !r.client_o <> None

  let disconnect () =
    match !r.client_o with
    | None -> Lwt.return_unit
    | Some t -> r := empty; Client.disconnect t

  let connect ~username proto address =
    let thread () =
      Client.connect proto address ~username () >>*= fun client ->
      disconnect () >>= fun () ->
      r := {client_o = Some client; cwd = []};
      return_ok ()
    in
    Lwt.catch thread (function
        | Unix.Unix_error (e, _, _) -> Unix.error_message e |> return_error
        | Failure s -> return_error s
        | e -> Lwt.fail e
      )

  let client () =
    match !r.client_o with
    | None -> "Not connected." |> return_error
    | Some client -> return_ok client

  let is_dir path =
    client () >>*= fun client ->
    Client.stat client path >>*= fun stat ->
    Types.(stat.Stat.mode.FileMode.is_directory) |> return_ok

  let set_cwd dir =
    is_dir dir >>*= function
    | true -> r := {!r with cwd = dir}; return_ok ()
    | false -> "Not a directory." |> return_error
end

let cut c s =
  if not (String.contains s c) then None
  else
    let i = String.index s c in
    let left = String.sub s 0 i in
    let right = String.sub s (i + 1) (String.length s - i - 1) in
    Some (left, right)

let rec cuts c s =
  if s = "" then []
  else
    match s |> cut c with
    | None -> [s]
    | Some (hd, tl) -> hd :: cuts c tl

let connect username address =
  let proto, address =
    match address |> cut ':' with
    | Some (proto, address) -> proto, address
    | None -> address, "5640"
  in
  State.connect ~username proto address

let parse_path path ~cwd =
  let tl_or_empty = function [] as tl | _::tl -> tl in
  let rec handle_dots acc = function
    | [] -> List.rev acc
    | ""::tl | "."::tl -> handle_dots acc tl
    | ".."::tl -> handle_dots (tl_or_empty acc) tl
    | hd::tl -> handle_dots (hd::acc) tl
  in
  match path |> cuts '/' with
  | ""::tl -> handle_dots [] tl
  | l -> handle_dots (List.rev cwd) l

let client_and_path raw_path =
  State.client () >>*= fun client ->
  if raw_path = "" then "No path given." |> return_error
  else return_ok (client, parse_path raw_path ~cwd:(State.cwd ()))

let client_and_dir raw_path =
  client_and_path raw_path >>*= fun (client, path) ->
  State.is_dir path >>*= function
  | false -> "Not a directory." |> return_error
  | true -> return_ok (client, path)

let client_and_filepath raw_path =
  client_and_path raw_path >>*= fun (client, path) ->
  State.is_dir path >>*= function
  | true -> "Is a directory." |> return_error
  | false -> return_ok (client, path)

let print_stats stats =
  let module Stat = Types.Stat in
  let open Types.FileMode in
  let row_of_stat x =
    let permissions p =
      (if List.mem `Read p then "r" else "-")
      ^ (if List.mem `Write p then "w" else "-")
      ^ (if List.mem `Execute p then "x" else "-") in
    let filemode = x.Stat.mode in
    let owner = permissions filemode.owner in
    let group = permissions filemode.group in
    let other = permissions filemode.other in
    let kind =
      if filemode.is_directory then "d"
      else if filemode.is_symlink then "l"
      else if filemode.is_device then "c"
      else if filemode.is_socket then "s"
      else "-" in
    let perms = kind ^ owner ^ group ^ other in
    let links = "?" in
    let uid = x.Stat.uid in
    let gid = x.Stat.gid in
    let length = Int64.to_string x.Stat.length in
    let tm = Unix.gmtime (Int32.to_float x.Stat.mtime) in
    let month = match tm.Unix.tm_mon with
      | 0 -> "Jan"  | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr" | 4 -> "May"
      | 5 -> "Jun"  | 6 -> "Jul" | 7 -> "Aug" | 8 -> "Sep" | 9 -> "Oct"
      | 10 -> "Nov" | 11 -> "Dec"
      | x -> string_of_int x
    in
    let day = string_of_int tm.Unix.tm_mday in
    let year = string_of_int (1900 + tm.Unix.tm_year) in
    let name =
     let name = x.Stat.name in
     if filemode.is_symlink
     then match x.Stat.u with
       | Some {Stat.extension = e; _} -> name ^ " -> " ^ e
       | None -> name
     else name in
    Array.of_list [
      perms; links; uid; gid; length; month; day; year; name;
    ] in
  let rows = Array.of_list (List.map row_of_stat stats) in
  let padto n x = x ^ String.make (max 0 @@ n - String.length x) ' ' in
  Array.iter (fun row ->
    Array.iteri (fun i txt ->
      let column = Array.map (fun row -> row.(i)) rows in
      let biggest = Array.fold_left (fun acc x ->
        max acc (String.length x)
      ) 0 column in
      Printf.printf "%s " (padto biggest txt)
    ) row;
    Printf.printf "\n";
  ) rows;
  Printf.printf "%!"

let ls raw_path =
  let raw_path = if raw_path = "" then "./" else raw_path in
  client_and_dir raw_path >>*= fun (client, dir) ->
  Client.readdir client dir >>*= fun stats ->
  print_stats stats;
  return_ok ()

let cd raw_path =
  let raw_path = if raw_path = "" then "/" else raw_path in
  client_and_dir raw_path >>*= fun (_, path) ->
  State.set_cwd path

let separate_last_name path =
  match List.rev path with
  | [] -> "No path given." |> return_error
  | name::rev_dir ->
    State.is_dir (List.rev rev_dir) >>*= function
    | false -> "Not a directory." |> return_error
    | true -> (List.rev rev_dir, name) |> return_ok

let mkdir raw_path =
  client_and_path raw_path >>*= fun (client, path) ->
  separate_last_name path >>*= fun (dir, name) ->
  let mode = Protocol_9p_types.FileMode.make ~is_directory:true
      ~owner:[`Read; `Write; `Execute] ~group:[`Read; `Execute]
      ~other:[`Read; `Execute ] () in
  Client.mkdir client dir name mode

let touch raw_path =
  client_and_path raw_path >>*= fun (client, path) ->
  separate_last_name path >>*= fun (dir, name) ->
  let mode = Protocol_9p_types.FileMode.make ~is_directory:false
      ~owner:[`Read; `Write] ~group:[`Read] ~other:[`Read] () in
  Client.create client dir name mode

let rm raw_path =
  client_and_path raw_path >>*= fun (client, path) ->
  Client.remove client path

let read raw_path =
  client_and_filepath raw_path >>*= fun (client, filepath) ->
  let rec copy ofs =
    let requested = 1024l in
    Client.read client filepath ofs requested >>*= fun bufs ->
    let len = List.fold_left (+) 0 (List.map Cstruct.len bufs) in
    List.iter (fun x -> output_string stdout (Cstruct.to_string x)) bufs;
    flush stdout;
    if Int32.of_int len < requested
    then return_ok ()
    else copy Int64.(add ofs (of_int len))
  in
  copy 0L >>*= fun () ->
  print_endline "";
  return_ok ()

let write_to_fid client fid buf =
  let maximum_payload =
    min 0x100000l (Client.LowLevel.maximum_write_payload client) |>
    Int32.to_int
  in
  let rec loop ~offset remaining =
    let len = Cstruct.len remaining in
    if len = 0 then return_ok ()
    else begin
      let to_request = min len maximum_payload in
      Client.LowLevel.write client fid offset
        (Cstruct.sub remaining 0 to_request) >>*=
      fun {Protocol_9p_response.Write.count} ->
      let count = Int32.to_int count in
      let remaining = Cstruct.shift remaining count in
      loop ~offset:Int64.(add offset (of_int count)) remaining
    end in
  loop ~offset:0L buf

let write raw_path data =
  client_and_filepath raw_path >>*= fun (client, filepath) ->
  Client.with_fid client (fun fid ->
      Client.walk_from_root client fid filepath >>*= fun _qids ->
      let open Client.LowLevel in
      openfid client fid Types.OpenMode.write_only >>*= fun _ ->
      update client fid ~length:0L >>*= fun () ->
      Cstruct.of_string data |> write_to_fid client fid
    )

open Shell_commands

type cache = {
  valid : bool;
  dir : string list;
  names : string list;
}

let cache = ref {
    valid = false;
    dir = [];
    names = [];
  }

let prompt_address = ref ""

let connect_cmd =
  let username = Arg.opt ["username"; "u"] in
  let predict _ = ["tcp:"; "tcp:127.0.0.1:5640"] in
  let address = Arg.pos_required ~predict () in
  let connect username address =
    connect username address >>*= fun () ->
    prompt_address := (if username = "" then "" else username ^ "@") ^ address;
    return_ok ()
  in
  Command.(create "connect" connect $ username $ address)

let disconnect_cmd =
  let disconnect () =
    State.disconnect () >>= fun () ->
    cache := {!cache with valid = false};
    return_ok ()
  in
  Command.(create "disconnect" disconnect $ const ())

let reload_cache dir =
  State.client () >>*= fun (client) ->
  Client.readdir client dir >>*= fun stats ->
  let names = stats |> List.map (fun stat ->
      let open Types in
      stat.Stat.name ^ (if stat.Stat.mode.FileMode.is_directory then "/" else "")
    ) in
  cache := {valid = true; dir; names};
  return_ok ()

(* TODO This needs to be improved a lot, and requires some support in the shell library to work fully. *)
(* May also want to store file names and directory names separately for better completion. *)
let predict_path p =
  if not (State.is_connected ()) then []
  else
    let raw_dir =
      if not (String.contains p '/') then ""
      else String.sub p 0 (String.rindex p '/' + 1)
    in
    let dir = parse_path raw_dir ~cwd:(State.cwd ()) in
    if !cache.valid && dir = !cache.dir then
      !cache.names |> List.map (fun name -> raw_dir ^ name)
    else begin
      Lwt.async (fun () -> reload_cache dir);
      []
    end

let path = Arg.pos ~predict:predict_path ()

let path_required = Arg.pos_required ~predict:predict_path ()

let ls_cmd =
  Command.(create "ls" ls $ path)

let cd_cmd =
  Command.(create "cd" cd $ path)

let map_invalidate_cache command =
  command |> Command.map (fun error_lwt ->
      error_lwt >>*= fun () ->
      cache := {!cache with valid = false};
      return_ok ()
    )

let mkdir_cmd =
  Command.(create "mkdir" mkdir $ path_required) |> map_invalidate_cache

let touch_cmd =
  Command.(create "touch" touch $ path_required) |> map_invalidate_cache

let rm_cmd =
  Command.(create "rm" rm $ path_required) |> map_invalidate_cache

let read_cmd =
  Command.(create "read" read $ path_required)

let write_cmd =
  let data = Arg.pos () in
  Command.(create "write" write $ path_required $ data)

let map_print_error command =
  let print_error result_lwt =
    result_lwt >>= function
    | Ok () -> Lwt.return_unit
    | Error (`Msg s) ->
      Command.name command ^ ": " ^ s |> Shell_unix.print_error
  in
  command |> Command.map print_error

let commands =
  [
    connect_cmd;
    disconnect_cmd;
    ls_cmd;
    cd_cmd;
    mkdir_cmd;
    touch_cmd;
    touch_cmd |> Command.rename "create";
    rm_cmd;
    read_cmd;
    write_cmd;
  ] |>
  List.map map_print_error |>
  List.fold_left (fun cs c -> cs |> Map.add c) Map.empty

let prompt () =
  let open Notty in
  if not (State.is_connected ()) then I.string A.(fg red) "Disconnected"
  else
    let open Infix in
    I.string A.empty (!prompt_address ^ " ") <|>
    I.string A.(fg green) ("/" ^ (State.cwd () |> String.concat "/"))

let () =
  Lwt_main.run (
    Shell_unix.run commands ~prompt >>=
    State.disconnect
  )
