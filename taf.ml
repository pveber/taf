open Base
open Vdom

type date = string
[@@deriving sexp]

module Task = struct
  type t = {
    id : id ;
    descr : string ;
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
    steps ;
    deps = [] ;
    estimated_duration = None ;
    duration = None ;
    status = TODO ;
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
end


let root_task =
  let x = Task.make ~descr:"X" () in
  let y = Task.make ~descr:"Y" () in
  Task.make ~descr:"Root" ~steps:[x ; y] ()

(* Definition of the vdom application *)

type model = {
  zipper : Task_zipper.t ;
}

type 'msg Vdom.Cmd.t +=
  | Focus of string

let rec update m = function
  | `Keydown k ->
    if not m.zipper.Task_zipper.editing then
      let msg = match k with
        | `Left -> `Zipper_leave
        | `Right -> `Zipper_enter
        | `Up -> `Zipper_prev
        | `Down -> `Zipper_next
        | `Enter -> `Zipper_toggle_edit
      in
      update m msg
    else (
      match k with
      | `Enter -> update m `Zipper_toggle_edit
      | _ -> return m
    )
  | `Zipper_enter -> return { zipper = Task_zipper.enter m.zipper }
  | `Zipper_leave -> return { zipper = Task_zipper.leave m.zipper }
  | `Zipper_next -> return { zipper = Task_zipper.next m.zipper }
  | `Zipper_prev -> return { zipper = Task_zipper.prev m.zipper }
  | `Zipper_toggle_edit ->
    if m.zipper.Task_zipper.editing then
      return { zipper = Task_zipper.stop_edit m.zipper true }
    else
      return ~c:[ Focus "task-edit" ] { zipper = Task_zipper.start_edit m.zipper }
  | `Zipper_set_descr descr ->
    return { zipper = Task_zipper.set_descr m.zipper descr }

let init = { zipper = Task_zipper.of_task root_task }, Cmd.batch []


module View = struct
  let ul = elt "ul"
  let li = elt "li"
  let input = elt "input"
  let strong = elt "strong"
  let br () = elt "br" []

  let rec zipper_context z =
    let open Task_zipper in
    match z.parent with
    | None -> [ text z.current_task.Task.descr ]
    | Some z' ->
      zipper_context z' @ [ text " > " ; text z.current_task.Task.descr ]

  let zipper_current_level z =
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
                oninput (fun s -> `Zipper_set_descr s)
              ] []
            in
            li [ input ]
          )
          else line ~highlight:true h
        in
        current :: List.map ~f:line t @ [ li [ text "+" ] ]
    in
    [ ul (prev @ next) ]

  let zipper z =
    zipper_context z @ br () :: zipper_current_level z

  let debug_task_zipper z =
    Task_zipper.sexp_of_t z
    |> Sexp.to_string_hum
    |> text
    |> (fun x -> [ x ])
    |> elt "pre"

  let view m = div (zipper m.zipper @ [ debug_task_zipper m.zipper ])

  let d = Js_browser.document
end

let app = app ~init ~update ~view:View.view ()


(* Driver *)

open Js_browser

let cmd_handler ctx = function
  | Focus id ->
    (
      match Document.get_element_by_id document id with
      | None -> ()
      | Some e -> Element.focus e
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
    | _ -> ()
  in
  Window.add_event_listener window "keydown" keydown_handler false

let run () =
  let app = Vdom_blit.run app in
  set_keydown_handler app ;
  app
  |> Vdom_blit.dom
  |> Element.append_child (Document.body document)

let () = Window.set_onload window run
