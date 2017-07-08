open Base
open Vdom

type date = string
[@@deriving sexp]

module Task = struct
  type t = {
    id : id ;
    descr : string ;
    note : string ;
    status : status ;
    due : date option ;
    tags : string list ;
    steps : t list ;
    estimated_duration : float option ; (* in hours *)
    duration : float option ;
    deps : id list ;
    history : (date * event) list ;
  }
  and id = string
  and event =
    | Created
    | Started
    | Canceled
    | Done
  and status = TODO | DONE | CANCELED
  [@@deriving sexp]

  let make ?(descr = "") ?(steps = []) () = {
    id = "" ;
    due = None ;
    tags = [] ;
    history = [] ;
    descr ;
    note = "" ;
    steps ;
    deps = [] ;
    estimated_duration = None ;
    duration = None ;
    status = TODO ;
  }


end

module Project = struct
  type t = {
    name : string ;
    description : string ;
    milestones : Task.t list ;
  }
  [@@deriving sexp]
end

module List_zipper = struct
  type 'a t = {
    prev : 'a list ;
    next : 'a list ;
  }
  [@@deriving sexp]

  let make xs = {
    prev = [] ;
    next = xs ;
  }

  let singleton x = make [ x ]

  let current xs = match xs.next with
    | [] -> None
    | h :: _ -> Some h

  let set_current xs x = {
    xs with next = x :: (
      match xs.next with
      | [] -> []
      | _ :: t -> t
    )
  }

  let next z =
    match z.next with
    | [] -> z
    | h :: t ->
      { prev = h :: z.prev ;
        next = t }

  let prev z =
    match z.prev with
    | [] -> z
    | h :: t ->
      { prev = t ;
        next = h :: z.next ; }

  let contents z = List.rev z.prev @ z.next

  let is_empty z = List.is_empty z.prev && List.is_empty z.next

  let is_at_end z = List.is_empty z.next
end

module Task_zipper = struct
  type t = {
    current_task : Task.t ; (* invariant: the real children of current_task are given by cursor *)
    cursor : Task.t List_zipper.t ;
    editing : bool ;
    parent : t option ;
  }
  [@@deriving sexp]

  let of_task t =
    {
      current_task = t ;
      cursor = List_zipper.make t.Task.steps ;
      editing = false ;
      parent = None ;
    }

  let next z =
    { z with editing = false ;
             cursor = List_zipper.next z.cursor }

  let prev z =
    { z with editing = false ;
             cursor = List_zipper.prev z.cursor }

  let cursor z = List_zipper.current z.cursor

  let set_cursor z u =
    { z with cursor = List_zipper.set_current z.cursor u }

  let enter z =
    match cursor z with
    | None -> z
    | Some t ->
      let res = of_task t in
      { res with parent = Some z }

  let leave z =
    match z.parent with
    | None -> z
    | Some p ->
      let z_u = { z.current_task with Task.steps = List_zipper.contents z.cursor } in
      { p with editing = false ;
               cursor = List_zipper.set_current p.cursor z_u }

  let start_edit z =
    { z with editing = true ;
             cursor = (
               if List_zipper.is_at_end z.cursor then
                 List_zipper.set_current z.cursor (Task.make ())
               else
                 z.cursor
             ) ; }

  let stop_edit z eol =
    let r = { z with editing = false } in
    if eol then next r else r

  let set_descr z descr =
    let u = match cursor z with
      | None -> Task.make ~descr ()
      | Some u -> { u with Task.descr }
    in
    set_cursor z u

  let rec contents z =
    match z.parent with
    | None ->
      { z.current_task with Task.steps = List_zipper.contents z.cursor }
    | Some _ -> contents (leave z)
end

module Db = struct
  type t = {
    projects : Project.t list ;
  }
  [@@deriving sexp]

  let make () = { projects  = [] }
end

(* Definition of the vdom application *)

type model =
  | Project_list_browser of {
      db : Db.t ;
      cursor : Project.t List_zipper.t ;
    }
  | Task_tree_browser of {
      db : Db.t ;
      project : Project.t ;
      zipper : Task_zipper.t ;
    }

type 'msg Vdom.Cmd.t +=
  | Focus of string
  | Save of Task.t

let rec update m ev =
  match m with
  | Task_tree_browser ({ zipper } as b) -> (
      let return_ttb ?c z = return ?c (Task_tree_browser { b with zipper = z }) in
      match ev with
      | `Keydown k ->
        if not zipper.Task_zipper.editing then
          let msg = match k with
            | `Left -> `TTB_leave
            | `Right -> `TTB_enter
            | `Up -> `TTB_prev
            | `Down -> `TTB_next
            | `Enter -> `TTB_toggle_edit
            | `S -> `Save
          in
          update m msg
        else (
          match k with
          | `Enter -> update m `TTB_toggle_edit
          | _ -> return m
        )
      | `TTB_enter -> return_ttb (Task_zipper.enter zipper)
      | `TTB_leave -> return_ttb (Task_zipper.leave zipper)
      | `TTB_next  -> return_ttb (Task_zipper.next  zipper)
      | `TTB_prev  -> return_ttb (Task_zipper.prev  zipper)
      | `TTB_toggle_edit ->
        if zipper.Task_zipper.editing then
          return_ttb (Task_zipper.stop_edit zipper true)
        else
          return_ttb ~c:[ Focus "task-edit" ] (Task_zipper.start_edit zipper)
      | `TTB_set_descr descr ->
        return_ttb (Task_zipper.set_descr zipper descr)
      | `Save ->
        return_ttb ~c:[ Save (Task_zipper.contents zipper) ] zipper
    )
  | Project_list_browser _ ->
    return m

module View = struct
  let ul = elt "ul"
  let li = elt "li"
  let input = elt "input"
  let strong = elt "strong"
  let br () = elt "br" []
  let pre code = elt "pre" [ text code ]

  let rec view_ttb_context z =
    let open Task_zipper in
    match z.parent with
    | None -> [ text z.current_task.Task.descr ]
    | Some z' ->
      view_ttb_context z' @ [ text " > " ; text z.current_task.Task.descr ]

  let view_ttb_current_level z =
    let open Task_zipper in
    let line ?(highlight = false) t =
      let f x = if highlight then strong [ x ] else x in
      li [ f (text t.Task.descr) ]
    in
    let prev = List.map ~f:line (List.rev z.cursor.List_zipper.prev) in
    let next = match z.cursor.List_zipper.next with
      | [] -> [ li [ strong [ text "+" ] ] ]
      | h :: t ->
        let current =
          if z.editing then (
            let input =
              input ~a:[
                str_prop "id" "task-edit" ;
                str_prop "value" h.Task.descr ;
                str_prop "placeholder" (
                  if String.(h.Task.descr = "") then
                    "Enter a task description"
                  else ""
                ) ;
                oninput (fun s -> `TTB_set_descr s)
              ] []
            in
            li [ input ]
          )
          else line ~highlight:true h
        in
        current :: List.map ~f:line t @ [ li [ text "+" ] ]
    in
    [ ul (prev @ next) ]

  let view_ttb z =
    view_ttb_context z @ br () :: view_ttb_current_level z


  let debug_task_ttb z =
    Task_zipper.sexp_of_t z
    |> Sexp.to_string_hum
    |> pre

  let debug_task_ttb2 z =
    Task_zipper.contents z
    |> Task.sexp_of_t
    |> Sexp.to_string_hum
    |> pre

  let view = function
    | Task_tree_browser ttb ->
      div (view_ttb ttb.zipper @ [ debug_task_ttb2 ttb.zipper ])
    | Project_list_browser _ -> div []

  let d = Js_browser.document
end

open Js_browser

let cmd_handler ctx = function
  | Focus id ->
    (
      match Document.get_element_by_id document id with
      | None -> ()
      | Some e -> Element.focus e
    ) ;
    true
  | Save u -> (
      match Window.local_storage window with
      | None -> Window.alert window "no local storage !"
      | Some storage ->
        let serialized =
          Task.sexp_of_t u
          |> Sexp.to_string_hum
        in
        Storage.set_item storage "task" serialized
    ) ;
    true
  | _ -> false

let () = Vdom_blit.(register (cmd {Cmd.f = cmd_handler}))


let set_keydown_handler app =
  let keydown_handler ev =
    match Event.which ev with
    | 39 -> Vdom_blit.process app (`Keydown `Right)
    | 37 -> Vdom_blit.process app (`Keydown `Left)
    | 38 -> Vdom_blit.process app (`Keydown `Up)
    | 40 -> Vdom_blit.process app (`Keydown `Down)
    | 13 -> Vdom_blit.process app (`Keydown `Enter)
    | 83 -> Vdom_blit.process app (`Keydown `S)
    | _ -> ()
  in
  Window.add_event_listener window "keydown" keydown_handler false

let initialize_db () =
  match Window.local_storage window with
  | None ->
    let msg = "No local storage, stopping program" in
    Window.alert window msg ;
    failwith msg
  | Some storage ->
    match Storage.get_item storage "db" with
    | None -> Db.make ()
    | Some serialized ->
      serialized
      |> Sexplib.Sexp.of_string
      |> Db.t_of_sexp

let init db =
  Project_list_browser {
    db ;
    cursor = List_zipper.make db.Db.projects
  },
  Cmd.batch []

let run () =
  let db = initialize_db () in
  let init = init db in
  let app = app ~init ~update ~view:View.view () in
  let app = Vdom_blit.run app in
  set_keydown_handler app ;
  app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let () = Window.set_onload window run
