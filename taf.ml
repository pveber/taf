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

let now () = Unix.gettimeofday ()

let mk_task text = {
  text ;
  status = Todo;
  created_at = now ();
  completed_at = None;
  children = [];
}

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
    | (_, []) as lz -> lz
    | (ls, h :: t) -> (h :: ls, t)

  let prev = function
    | ([], _) as lz -> lz
    | (h :: t, rs) -> (t, h :: rs)

  let head = function
    | (_, []) -> None
    | (_, h :: _) -> Some h

  let replace_head (ls, rs) x = match rs with
    | [] -> (ls, [ x ])
    | _ :: t -> (ls, x :: t)

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

module Zipper = struct
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

  let prev z = { z with items = List_zipper.prev z.items }
  let next z = { z with items = List_zipper.next z.items }

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
        | Todo -> { t with status = Done ; completed_at = Some (now ()) }
        | Done -> { t with status = Todo ; completed_at = None } in
      { z with items = List_zipper.replace_head z.items t }

  let at_root z = Option.is_none z.parent

  let rec root z =
    match z.parent with
    | None -> { z.focus with children = List_zipper.to_list z.items }
    | Some _ -> root (zoom_out z)

  let suppr_current z =
    { z with items = List_zipper.remove_head z.items }
end

(* module Hist_zipper = struct *)
(*   (\* invariant: List_zipper not empty *\) *)
(*   type t = Zipper.t List_zipper.t *)

(*   let make z = List_zipper.singleton z   *)
(* end *)

module State = struct
  open Notty

  type mode =
    | Command
    | Edit of string

  type t = {
    zip : Zipper.t ;
    mode : mode ;
  }

  let init zip = { zip ; mode = Command }

  let insert_empty_task { zip ; _ } =
    let t = mk_task "" in
    {
      zip = Zipper.insert_task zip t ;
      mode = Edit "" ;
    }

  let leave_edit_mode state =
    match state.mode with
    | Command -> state
    | Edit s ->
      { zip = Zipper.set_cursor_text state.zip s ;
        mode = Command }

  let enter_edit_mode state =
    match state.mode with
    | Edit _ -> state
    | Command ->
      match Zipper.cursor state.zip with
      | None -> state
      | Some t -> { state with mode = Edit t.text }

  let add_char state c =
    match state.mode with
    | Command -> state
    | Edit s ->
      let s' =
        let b = Buffer.create (4 + String.length s) in
        Buffer.add_string b s ;
        (
          match c with
          | `ASCII c -> Buffer.add_char b c
          | `Uchar u -> Buffer.add_utf_8_uchar b u
        ) ;
        Buffer.contents b
      in
      { state with mode = Edit s' }

  let remove_char state =
    match state.mode with
    | Command -> state
    | Edit s -> { state with mode = Edit (remove_last_utf8 s) }
  
  let update_zip f state = { state with zip = f state.zip }

  let render_breadcrumb { zip ; _ } =
    let path_elts = List.map (fun t -> t.text) (Zipper.current_path zip) in
    let breadcrumb = String.concat " > " path_elts in
    I.string A.(fg lightblue) breadcrumb
    
  let render_task ~has_focus ~mode task =
    let checkbox = match task.status with
      | Todo -> "[ ]"
      | Done -> "[X]"
    in
    let text = match task.text, mode with
      | "", Command -> "(empty)"
      | s,  Command -> s
      | s,  Edit field ->
        if has_focus then field else s
    in
    let line = checkbox ^ " " ^ text in
    let attr =
      if has_focus then
        match mode with
        | Edit _ -> A.(bg green ++ fg black)
        | Command -> A.(bg blue ++ fg white)
      else A.empty
    in
    I.string attr line

  let render_tasks state =
    let task_images =
      List_zipper.fold_right state.zip.items ~init:[] ~f:(fun ~head acc t ->
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

let load_from_file filename =
  In_channel.with_open_text filename (fun ic ->
      Yojson.Safe.from_channel ic
      |> task_of_yojson
      |> Result.get_ok
    )

let data_dir () =
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  let path = Filename.concat (Xdg.data_dir xdg) "taf" in
  if not (Sys.file_exists path) then Unix.mkdir path 0o700 ;
  path

let backup_path () =
  Filename.concat (data_dir ()) "tasks.json"

let load_task_tree () =
  let filename = backup_path () in
  let root_task =
    if Sys.file_exists filename then
      load_from_file filename
    else mk_task "•"
  in
  Zipper.make root_task

let save_task_tree (state : State.t) =
  let filename = backup_path () in
  let json = task_to_yojson (Zipper.root state.zip) in
  Out_channel.with_open_text filename (fun oc ->
      Yojson.Safe.to_channel oc json
    )

let main () =
  let zip = load_task_tree () in
  let state = State.init zip in
  let term = Notty_unix.Term.create () in

  let rec loop state =
    let k_update_zip f = loop (State.update_zip f state) in
    let img = State.render state in
    Notty_unix.Term.image term img ;

    match state.mode, Notty_unix.Term.event term with
    | Edit _, `Key (`Enter, _) -> loop (State.leave_edit_mode state)
    | Edit _, `Key (`Backspace, _) -> loop (State.remove_char state)
    | Edit _, `Key ((`ASCII _ | `Uchar _) as c, []) -> loop (State.add_char state c)
    | Command, `Key (`ASCII 'q', []) -> state
    | Command, `Key (`ASCII 'i', []) -> loop (State.insert_empty_task state)
    | Command, `Key (`ASCII 'd', []) -> k_update_zip Zipper.toggle_done
    | Command, `Key (`Arrow `Down, []) -> k_update_zip Zipper.next
    | Command, `Key (`Arrow `Up, []) -> k_update_zip Zipper.prev
    | Command, `Key (`Arrow `Left, []) -> k_update_zip Zipper.zoom_out
    | Command, `Key (`Arrow `Right, []) -> k_update_zip Zipper.zoom_in
    | Command, `Key (`Delete, []) -> k_update_zip Zipper.suppr_current
    | Command, `Key (`Enter, []) -> loop (State.enter_edit_mode state)
    | _ -> loop state
  in
  let final_state = loop state in
  save_task_tree final_state

let () = main ()
