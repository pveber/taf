open Base
open Vdom

let local_storage_key = "taf-db"

let ul = elt "ul"
let li = elt "li"
let input = elt "input"
let strong = elt "strong"
let br () = elt "br" []
let pre code = elt "pre" [ text code ]
let tr = elt "tr"
let td = elt "td"
let table = elt "table"
let h3 = elt "h3"
let label = elt "label"
let textarea = elt "textarea"
let button = elt "button"

let strong_if b x = if b then strong [ x ] else x

type date = float (* number of milliseconds since epoch *)
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
    history = [ Js_browser.Date.now (), Created ] ;
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
    created : date ;
    archived : bool ;
    milestones : Task.t list ;
  }
  [@@deriving sexp]

  let make ~name ~description () = {
    name ;
    description ;
    created = Js_browser.Date.now () ;
    archived = false ;
    milestones = [] ;
  }
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

  let is_at_last z =
    match z.next with
    | [ _ ] -> true
    | _ -> false

  let positional_map z ~f =
    let l1 = List.rev_filter_map z.prev ~f:(fun x -> f (`Prev x)) in
    let l2 = match z.next with
      | [] -> List.filter_opt [f `Cursor_at_end]
      | h :: t ->
        f (`Cursor h) :: List.map t ~f:(fun x -> f (`Next x)) @ [ f `End ]
        |> List.filter_opt
    in
    l1 @ l2
end

module Task_zipper = struct
  type t = {
    current_task : Task.t ; (* invariant: the real children of
                               current_task are given by cursor *)
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

  let is_at_root z = match z.parent with
    | None -> true
    | Some _ -> false
end

module Db = struct
  type t = {
    projects : Project.t list ;
  }
  [@@deriving sexp]

  let make () = { projects  = [] }

  let add_project db p = { projects = db.projects @ [ p ] }

  let update_project db p =
    let projects =
      List.map db.projects ~f:(fun q ->
          if String.(q.Project.name = p.Project.name)
          then p else q
        )
    in
    { projects }
end

(* Definition of the vdom application *)
type 'msg Vdom.Cmd.t +=
  | Focus of string
  | Save of Db.t


module Task_tree_browser = struct
  type model = {
      db : Db.t ;
      project : Project.t ; (* invariant: the current milestones of project
                               are represented by zipper *)
      zipper : Task_zipper.t ;
      event : [`Leave of Db.t] option ;
    }

  let init db project = {
    db ;
    project ;
    zipper = (
      let root =
        Task.make
          ~descr:project.Project.name
          ~steps:project.Project.milestones
          ()
      in
      Task_zipper.of_task root
    ) ;
    event = None ;
  }

  let contents m =
    let milestones = (Task_zipper.contents m.zipper).Task.steps in
    let project = { m.project with Project.milestones } in
    Db.update_project m.db project

  let rec update ({ zipper } as m) =
    let retz ?c zipper = return ?c { m with zipper } in
    function
    | `Keydown k ->
      if not zipper.Task_zipper.editing then
        let msg = match k with
          | `Left -> `TTB_leave
          | `Right -> `TTB_enter
          | `Up -> `TTB_prev
          | `Down -> `TTB_next
          | `Enter -> `TTB_toggle_edit
          | `S -> `Save
          | _ -> `No_op
        in
        update m msg
      else (
        match k with
        | `Enter -> update m `TTB_toggle_edit
        | _ -> return m
      )
    | `TTB_enter -> retz (Task_zipper.enter zipper)
    | `TTB_leave ->
      if Task_zipper.is_at_root zipper then
        return { m with event = Some (`Leave (contents m))}
      else
        retz (Task_zipper.leave zipper)
    | `TTB_next  -> retz (Task_zipper.next  zipper)
    | `TTB_prev  -> retz (Task_zipper.prev  zipper)
    | `TTB_toggle_edit ->
      if zipper.Task_zipper.editing then
        retz (Task_zipper.stop_edit zipper true)
      else
        retz ~c:[ Focus "task-edit" ] (Task_zipper.start_edit zipper)
    | `TTB_set_descr descr ->
      retz (Task_zipper.set_descr zipper descr)
    | `Save ->
      return ~c:[ Save (contents m) ] m
    | _ -> return m

  let rec view_context z =
    let open Task_zipper in
    match z.parent with
    | None -> [ text z.current_task.Task.descr ]
    | Some z' ->
      view_context z' @ [ text " > " ; text z.current_task.Task.descr ]

  let view_current_level z =
    let open Task_zipper in
    let line ?(highlight = false) txt =
      li [ strong_if highlight (text txt) ]
    in
    let task_line ?highlight u = line ?highlight u.Task.descr in
    let f = function
      | `Prev x | `Next x -> task_line x
      | `Cursor x -> (
          if z.editing then (
            let input =
              input ~a:[
                str_prop "id" "task-edit" ;
                str_prop "value" x.Task.descr ;
                str_prop "placeholder" (
                  if String.(x.Task.descr = "") then
                    "Enter a task description"
                  else ""
                ) ;
                oninput (fun s -> `TTB_set_descr s)
              ] []
            in
            li [ input ]
          )
          else task_line ~highlight:true x
        )
      | `Cursor_at_end -> line ~highlight:true "+"
      | `End -> line "+"
    in
    [ ul (List_zipper.positional_map z.cursor ~f:(fun x -> Some (f x))) ]

  let debug_task z =
    Task_zipper.sexp_of_t z
    |> Sexp.to_string_hum
    |> pre

  let debug_task2 z =
    Task_zipper.contents z
    |> Task.sexp_of_t
    |> Sexp.to_string_hum
    |> pre

  let view ttb =
    view_context ttb.zipper @ br () :: view_current_level ttb.zipper

end

module Project_list_browser = struct
  type model = {
    db : Db.t ;
    cursor : Project.t List_zipper.t ;
    event : [ `Switch_to_new_project_form
            | `Browse_project of Project.t ] option ;
  }

  let update m =
    function
    | `Keydown `C -> return { m with event = Some `Switch_to_new_project_form }
    | `Keydown `Down ->
      if List_zipper.is_at_last m.cursor then
        return m
      else
        return { m with cursor = List_zipper.next m.cursor }
    | `Keydown `Up  -> return { m with cursor = List_zipper.prev m.cursor }
    | `Keydown `Right -> (
        match List_zipper.current m.cursor with
        | None -> return m
        | Some p -> return { m with event = Some (`Browse_project p) }
      )
    | _ -> return m

  let view { cursor } =
    let module Date = Js_browser.Date in
    let line ?(highlight = false) { Project.name ; created } =
      let created = Date.to_date_string (Date.new_date created) in
      tr [
        td [ strong_if highlight (text name) ] ;
        td [ text created ] ;
      ]
    in
    let f = function
      | `Prev x | `Next x -> Some (line x)
      | `Cursor x -> Some (line ~highlight:true x)
      | `Cursor_at_end -> None
      | `End -> None
    in
    div [
      h3 [ text "List of projects" ] ;
      table (List_zipper.positional_map cursor ~f) ;
    ]

  let init db = {
    db ;
    cursor = List_zipper.make db.Db.projects ;
    event = None ;
  }
end

module New_project_form = struct
  type model = {
    db : Db.t ;
    name : string ;
    description : string ;
    submitted : bool ;
  }

  let update model = function
    | `NPF_set_name name -> return { model with name }
    | `NPF_set_description description -> return { model with description }
    | `NPF_submit -> return { model with submitted = true }
    | _ -> return model

  let view m = div [
      h3 [ text "New project" ] ;
      br () ; br () ;
      label [
        text "Name" ;
        input ~a:[ str_prop "value" m.name ;
                   str_prop "placeholder" "Enter a short name for your project" ;
                   oninput (fun s -> `NPF_set_name s) ] [] ;
      ] ;
      br () ;
      label [
        text "Description" ;
        textarea ~a:[ str_prop "value" m.description ;
                      str_prop "placeholder" "Enter a description for your project" ;
                      oninput (fun s -> `NPF_set_description s) ] [] ;
      ] ;
      br () ;
      button
        ~a:[ onclick `NPF_submit ; ]
        [ text "Create project" ] ;
    ]

  let init db = {
    db ;
    name = "" ;
    description = "" ;
    submitted = false ;
  }

end

type model =
  | Project_list_browser of Project_list_browser.model
  | Task_tree_browser of Task_tree_browser.model
  | New_project_form of New_project_form.model

let update m ev =
  match m with
  | Task_tree_browser ttb ->
    let open Task_tree_browser in
    let ttb, cmd = update ttb ev in
    let m = match ttb.event with
      | None -> Task_tree_browser ttb
      | Some (`Leave db) ->
        Project_list_browser (
          Project_list_browser.init db
        )
    in
    m, cmd

  | Project_list_browser plb ->
    let open Project_list_browser in
    let plb, cmd = update plb ev in
    let m = match plb.event with
      | None -> Project_list_browser plb
      | Some `Switch_to_new_project_form ->
        New_project_form (New_project_form.init plb.db)
      | Some (`Browse_project p) ->
        Task_tree_browser (
          Task_tree_browser.init plb.db p
        )
    in
    m, cmd
  | New_project_form npf ->
    let open New_project_form in
    let npf, cmd = update npf ev in
    let m =
      if npf.submitted then
        Project_list_browser (
          Project.make ~name:npf.name ~description:npf.description ()
          |> Db.add_project npf.db
          |> Project_list_browser.init
        )
      else
        New_project_form npf
    in
    m, cmd

let view = function
  | Task_tree_browser ttb ->
    div (  Task_tree_browser.view ttb
         @ [ Task_tree_browser.debug_task2 ttb.Task_tree_browser.zipper ])
  | Project_list_browser plb ->
    Project_list_browser.view plb
  | New_project_form npf ->
    New_project_form.view npf

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
          Db.sexp_of_t u
          |> Sexp.to_string_hum
        in
        Storage.set_item storage local_storage_key serialized
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
    | 67 -> Vdom_blit.process app (`Keydown `C)
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
    match Storage.get_item storage local_storage_key with
    | None -> Db.make ()
    | Some serialized ->
      serialized
      |> Sexplib.Sexp.of_string
      |> Db.t_of_sexp

let init db =
  Project_list_browser (Project_list_browser.init db),
  Cmd.batch []

let run () =
  let db = initialize_db () in
  let init = init db in
  let app = app ~init ~update ~view () in
  let app = Vdom_blit.run app in
  set_keydown_handler app ;
  app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let () = Window.set_onload window run
