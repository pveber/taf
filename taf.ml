type status = Todo | Done
[@@deriving yojson]

type task = {
  text         : string;
  status       : status;
  created_at   : float;
  completed_at : float option;
  children     : task list;
}
[@@deriving yojson]

type db = (string * task) list
[@@deriving yojson]

let now () = Unix.gettimeofday ()

let dief fmt =
  let die msg =
    prerr_endline msg ;
    exit 1
  in
  Printf.ksprintf die fmt

let mk_task text = {
  text ;
  status = Todo;
  created_at = now ();
  completed_at = None;
  children = [];
}

let children_complete t =
  List.for_all (fun x -> x.status = Done) t.children

let is_continuation_byte c =
  Char.code c land 0b1100_0000 = 0b1000_0000

let remove_last_utf8 = function
  | "" -> ""
  | s ->
    let rec loop i =
      if i < 0 then ""
      else if is_continuation_byte s.[i]
      then loop (i - 1) (* continuation byte *)
      else String.sub s 0 i
    in
    loop (String.length s - 1)

module List_zipper = struct
  type 'a t = 'a list * 'a list

  let make xs = ([], xs)

  let singleton x = ([], [ x ])

  let next = function
    | ((_, []) | (_, _ :: [])) as lz -> lz
    | (ls, h :: t) -> (h :: ls, t)

  let prev = function
    | ([], _) as lz -> lz
    | (h :: t, rs) -> (t, h :: rs)

  let shift_left = function
    | ((_, []) | ([], _))as lz -> lz
    | (lh :: lt, rh :: rt) -> (lt, rh :: lh :: rt)

  let shift_right = function
    | ((_, []) | (_, _ :: []))as lz -> lz
    | (ls, e1 :: e2 :: t) -> (e2 :: ls, e1 :: t)

  let head = function
    | (_, []) -> None
    | (_, h :: _) -> Some h

  let replace_head (ls, rs) x = match rs with
    | [] -> (ls, [ x ])
    | _ :: t -> (ls, x :: t)

  let update_head ((ls, rs) as lz) f = match rs with
    | [] -> lz
    | h :: t -> (ls, f h :: t)

  let insert (ls, rs) x =
    match rs with
    | [] -> (ls, x :: [])
    | h :: t -> (h :: ls, x :: t)

  let fold (lr, rs) ~init ~f =
    let acc = List.fold_right (fun x acc -> f ~head:false acc x) lr init in
    match rs with
    | [] -> acc
    | h :: t ->
      let acc' = f ~head:true acc h in
      List.fold_left (fun acc x -> f ~head:false acc x) acc' t

  let fold_right (lr, rs) ~init ~f =
    let acc = match rs with
      | [] -> init
      | h :: t ->
        let acc' = List.fold_right (fun x acc -> f ~head:false acc x) t init in
        f ~head:true acc' h
    in
    List.fold_left (f ~head:false) acc lr

  let remove_head (ls, rs) =
    (ls,
     match rs with
     | [] -> []
     | _ :: t -> t)

  let to_list (ls, rs) = List.rev_append ls rs
end

module Task_zipper = struct
  type t = {
    focus  : task ;
    items  : task List_zipper.t ;
    parent : t option ;
  }

  let make t = {
    focus = t ;
    items = List_zipper.make t.children ;
    parent = None ;
  }

  let upd_items z f = { z with items = f z.items }

  let prev z = upd_items z List_zipper.prev
  let next z = upd_items z List_zipper.next

  let move_up z = upd_items z List_zipper.shift_left
  let move_down z = upd_items z List_zipper.shift_right

  let zoom_in z = match List_zipper.head z.items with
    | None -> z
    | Some t -> {
        focus = t ;
        items = List_zipper.make t.children ;
        parent = Some z ;
      }

  let zoom_out z = match z.parent with
    | None -> z
    | Some parent ->
      let t = { z.focus with children = List_zipper.to_list z.items } in
      { parent with items = List_zipper.replace_head parent.items t }

  let current_path z =
    let rec loop z acc =
      let acc' = z.focus :: acc in
      match z.parent with
      | None -> acc'
      | Some p -> loop p acc'
    in
    loop z []

  let insert_task z t =
    { z with items = List_zipper.insert z.items t }

  let set_cursor_text z text =
    match List_zipper.head z.items with
    | None -> z
    | Some t -> { z with items = List_zipper.replace_head z.items { t with text } }

  let is_cursor_on_task z = Option.is_some (List_zipper.head z.items)

  let cursor z = List_zipper.head z.items

  let toggle_done z =
    match List_zipper.head z.items with
    | None -> z
    | Some t ->
      let t =
        match t.status with
        | Todo ->
          if children_complete t then
            { t with status = Done ; completed_at = Some (now ()) }
          else t
        | Done -> { t with status = Todo ; completed_at = None }
      in
      { z with items = List_zipper.replace_head z.items t }

  let at_root z = Option.is_none z.parent

  let rec root z =
    match z.parent with
    | None -> { z.focus with children = List_zipper.to_list z.items }
    | Some _ -> root (zoom_out z)

  let suppr_current z =
    { z with items = List_zipper.remove_head z.items }
end

let utf8_string_length s =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun i _ -> i + 1) 0 s

let utf8_add_char s p c =
  let buf = Buffer.create (2 * String.length s) in
  let add = function
    | `ASCII c -> Buffer.add_char buf c
    | `Uchar u -> Buffer.add_utf_8_uchar buf u
  in
  let f i u =
    if i = p then add c ;
    Buffer.add_string buf u ;
    i + 1
  in
  let last_pos = Uuseg_string.fold_utf_8 `Grapheme_cluster f 0 s in
  if p = last_pos then add c ;
  Buffer.contents buf

let utf8_remove_prec_char s p =
  let buf = Buffer.create (String.length s) in
  let f i u =
    if i <> p - 1 then Buffer.add_string buf u ;
    i + 1
  in
  let _ = Uuseg_string.fold_utf_8 `Grapheme_cluster f 0 s in
  Buffer.contents buf

(* module Hist_zipper = struct *)
(*   (\* invariant: List_zipper not empty *\) *)
(*   type t = Zipper.t List_zipper.t *)

(*   let make z = List_zipper.singleton z   *)
(* end *)

module State = struct
  open Notty

  type mode =
    | Command
    | Edit of string * int

  type t = {
    contexts : (string * Task_zipper.t) List_zipper.t ; (* invariant: cannot be empty *)
    mode : mode ;
  }

  let init db =
    if db = [] then invalid_arg "there should be at least one context" ;
    let contexts =
      List.map (fun (name, task) -> (name, Task_zipper.make task)) db
      |> List_zipper.make
    in
    { contexts ; mode = Command }

  let to_db state =
    List_zipper.to_list state.contexts
    |> List.map (fun (ctx, tz) -> ctx, Task_zipper.root tz)

  let ftz state = snd (Option.get (List_zipper.head state.contexts))

  let upd_ctx contexts f =
    List_zipper.update_head contexts (fun (name, tz) -> name, f tz)

  (* update focused task zipper *)
  let upd_ftz state f =
    { state with contexts = upd_ctx state.contexts f }

  let insert_empty_task state =
    let t = mk_task "" in
    {
      contexts = upd_ctx state.contexts (fun tz -> Task_zipper.insert_task tz t) ;
      mode = Edit ("", 0) ;
    }

  let leave_edit_mode state =
    match state.mode with
    | Command -> state
    | Edit (s, _) ->
      { contexts = upd_ctx state.contexts (fun tz -> Task_zipper.set_cursor_text tz s) ;
        mode = Command }

  let task_cursor state =
    List_zipper.head state.contexts
    |> Option.map snd
    |> Fun.flip Option.bind Task_zipper.cursor

  let enter_edit_mode state =
    match state.mode with
    | Edit _ -> state
    | Command ->
      match task_cursor state with
      | None -> state
      | Some t -> { state with mode = Edit (t.text, utf8_string_length t.text) }

  let add_char state c =
    match state.mode with
    | Command -> state
    | Edit (s, p) ->
      let s' = utf8_add_char s p c in
      { state with mode = Edit (s', p + 1) }

  let remove_prec_char state =
    match state.mode with
    | Command -> state
    | Edit (s, p) -> { state with mode = Edit (utf8_remove_prec_char s p, p - 1) }

  let previous_context state =
    { state with contexts = List_zipper.prev state.contexts }

  let next_context state =
    { state with contexts = List_zipper.next state.contexts }

  let render_breadcrumb state =
    let context, tz = Option.get (List_zipper.head state.contexts) in
    let path_elts = List.map (fun t -> t.text) (Task_zipper.current_path tz) in
    let context = I.string A.(fg lightblue ++ st bold) context in
    let breadcrumb = match path_elts with
      | [] -> failwith "should not happen: there is always a root task"
      | _ :: [] -> ""
      | _root :: t -> " :: " ^ String.concat " > " t
    in
    let breadcrumb = I.string A.(fg lightblue) breadcrumb in
    I.hcat [ context ; breadcrumb ]

  let edited_text_attr = A.(bg green ++ fg black)

  let render_edited_field s p =
    let bef = Buffer.create (String.length s) in
    let und = Buffer.create (String.length s) in
    let aft = Buffer.create (String.length s) in
    let f i u =
      let buf = if i < p then bef else if i = p then und else aft in
      Buffer.add_string buf u ;
      i + 1
    in
    let last_pos = Uuseg_string.fold_utf_8 `Grapheme_cluster f 0 s in
    if last_pos = p then Buffer.add_char und ' ' ;
    I.hcat [
      I.string edited_text_attr (Buffer.contents bef) ;
      I.string A.(bg lightgreen ++ fg black) (Buffer.contents und) ;
      I.string edited_text_attr (Buffer.contents aft) ;
    ]

  let render_task ~has_focus ~mode task =
    let text_attr =
      match has_focus, mode with
      | true, Command -> A.(bg blue ++ fg white)
      | true, Edit _ -> edited_text_attr
      | false, _ -> A.empty
    in
    let checkbox =
      I.string text_attr (
        match task.status with
        | Todo -> "- [ ] "
        | Done -> "- [X] "
      )
    in
    let field = match has_focus, mode with
      | true, Edit (s, p) -> render_edited_field s p
      | _ -> I.string text_attr task.text
    in
    let line = I.(checkbox <|> field) in
    if task.children <> [] then
      I.(line <|> string text_attr " ▶ ")
    else line

  let render_tasks state =
    let task_images =
      let tz = ftz state in
      List_zipper.fold_right tz.items ~init:[] ~f:(fun ~head acc t ->
          render_task ~has_focus:head ~mode:state.mode t :: acc
        )
    in
    match task_images with
    | [] -> I.string A.(fg (gray 8)) "(no tasks - press 'i' to insert)"
    | imgs -> I.vcat imgs

  let render_help () =
    I.string
      A.(fg (gray 12))
      "i:insert d:delete Enter:edit Space:toggle ←→:zoom ↑↓:move u:undo q:quit"

  let render state =
    let breadcrumb = render_breadcrumb state in
    let tasks = render_tasks state in
    let help = render_help () in
    I.vcat [breadcrumb; I.void 0 1; tasks; I.void 0 1; help]
end

let appname = "taf"

module Dirs = struct
  type t = {
    config : string ;
    data : string ;
  }

  let mksubdir path dir =
    let path = Filename.concat path dir in
    if not (Sys.file_exists path) then Unix.mkdir path 0o700 ;
    path

  let create () =
    let xdg = Xdg.create ~env:Sys.getenv_opt () in
    let config = mksubdir (Xdg.config_dir xdg) appname in
    let data = mksubdir (Xdg.data_dir xdg) appname in
    { config ; data }

  let config_path dirs =
    Filename.concat dirs.config "config.toml"

  let data_path dirs =
    Filename.concat dirs.data "tasks.json"
end

(* module Config = struct *)
(*   type t = { *)
(*     contexts : string list ; *)
(*   } *)

(*   let load dirs = *)
(*     let path = Dirs.config_path dirs in *)
(*     if not (Sys.file_exists path) then [] *)
(*     else *)
(*       let conf = Otoml.Parser.from_file path in *)
(*       Otoml.(find conf (get_array get_string) ["contexts"]) *)
(* end *)

let load_from_file filename =
  In_channel.with_open_text filename (fun ic ->
      Yojson.Safe.from_channel ic
      |> db_of_yojson
      |> Result.get_ok
    )

let load_task_tree dirs =
  let filename = Dirs.data_path dirs in
  if Sys.file_exists filename then
    load_from_file filename
  else dief "Please run `taf init` first!"

let save_db db dirs =
  let filename = Dirs.data_path dirs in
  let json = db_to_yojson db in
  Out_channel.with_open_text filename (fun oc ->
      Yojson.Safe.to_channel oc json
    )

let save_task_tree state dirs =
  save_db (State.to_db state) dirs

let main () =
  let dirs = Dirs.create () in
  let db = load_task_tree dirs in
  let state = State.init db in
  let term = Notty_unix.Term.create () in

  let rec loop state =
    let k_update_zip f = loop (State.upd_ftz state f) in
    let img = State.render state in
    Notty_unix.Term.image term img ;

    match state.mode, Notty_unix.Term.event term with
    | Edit _, `Key (`Enter, _) -> loop (State.leave_edit_mode state)
    | Edit _, `Key (`Backspace, _) -> loop (State.remove_prec_char state)
    | Edit _, `Key ((`ASCII _ | `Uchar _) as c, []) -> loop (State.add_char state c)
    | Command, `Key (`ASCII 'q', []) -> state
    | Command, `Key (`ASCII 'i', []) -> loop (State.insert_empty_task state)
    | Command, `Key (`ASCII 'd', []) -> k_update_zip Task_zipper.toggle_done
    | Command, `Key (`Arrow `Down, []) -> k_update_zip Task_zipper.next
    | Command, `Key (`Arrow `Up, []) -> k_update_zip Task_zipper.prev
    | Command, `Key (`Arrow `Down, [`Meta]) -> k_update_zip Task_zipper.move_down
    | Command, `Key (`Arrow `Up, [`Meta]) -> k_update_zip Task_zipper.move_up
    | Command, `Key (`Arrow `Left, []) -> k_update_zip Task_zipper.zoom_out
    | Command, `Key (`Arrow `Right, []) -> k_update_zip Task_zipper.zoom_in
    | Command, `Key (`Page `Up, []) -> loop (State.previous_context state)
    | Command, `Key (`Page `Down, []) -> loop (State.next_context state)
    | Command, `Key (`Delete, []) -> k_update_zip Task_zipper.suppr_current
    | Command, `Key (`Enter, []) -> loop (State.enter_edit_mode state)
    | _ -> loop state
  in
  let final_state = loop state in
  save_task_tree final_state dirs

let mk_root () = mk_task "•"

let init ~contexts =
  let db = List.map (fun c -> c, mk_root ()) contexts in
  let dirs = Dirs.create () in
  save_db db dirs

let context_add ~context_name =
  let dirs = Dirs.create () in
  let db = load_task_tree dirs in
  let db' = db @ [ context_name, mk_root () ] in
  save_db db' dirs

open Cmdliner
open Cmdliner.Term.Syntax

let init_command =
  let doc = {|Initializes configuration files to use taf. It is necessary to define the initial contexts for your tasks.|}
  in
  let info = Cmd.info ~doc "init" in
  let term =
    let+ contexts =
      let doc = "A list of strings separated by commas, each string is a name for a context" in
      let i = Arg.info [] ~doc ~docv:"CONTEXTS" in
      Arg.(required & pos 0 (some (list string)) None i)
    in
    init ~contexts
  in
  Cmd.v info term

let context_add_cmd =
  let info = Cmd.info "add" in
  let term =
    let+ context_name =
      let doc = "Name of context to add" in
      Arg.(required & pos 0 (some string) None & info [] ~doc)
    in
    context_add ~context_name
  in
  Cmd.v info term

let context_command =
  let info = Cmd.info "context" in
  Cmd.group info [context_add_cmd]

let command =
  let info = Cmd.info appname in
  let default =
    let+ unit = Term.const () in
    main unit
  in
  Cmd.group ~default:default info [ init_command ; context_command ]

let () = exit @@ Cmd.eval command
