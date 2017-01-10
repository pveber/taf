open Vdom


module Task = struct
  type t = {
    descr : string ;
    estimated_duration : float option ; (* in hours *)
    status : status ;
    deps : t list ;
  }

  and status = TODO | DONE
  [@@deriving sexp]
end

module Zipper = struct
  type t = {
    descr : string ;
    estimated_duration : float option ; (* in hours *)
    status : Task.status ;
    editing : bool ;
    prev_deps : Task.t list ;
    next_deps : Task.t list ;
    parent : t option ;
  }

  let of_task t =
    {
      descr = t.Task.descr ;
      estimated_duration = t.Task.estimated_duration ;
      status = t.Task.status ;
      editing = false ;
      prev_deps = [] ;
      next_deps = t.Task.deps ;
      parent = None ;
    }

  let next z =
    match z.next_deps with
    | [] -> z
    | h :: t ->
      { z with editing = false ;
               prev_deps = h :: z.prev_deps ;
               next_deps = t }

  let prev z =
    match z.prev_deps with
    | [] -> z
    | h :: t ->
      { z with editing = false ;
               prev_deps = t ;
               next_deps = h :: z.next_deps ; }

  let enter z =
    match z.next_deps with
    | [] -> z
    | h :: _ ->
      let res = of_task h in
      { res with parent = Some z }

  let leave z =
    match z.parent with
    | None -> z
    | Some p ->
      match p.next_deps with
      | [] -> assert false (* otherwise we couldn't have entered [z] in the first place *)
      | h :: t ->
        { p with editing = false ;
                 next_deps = Task.{ descr = z.descr ;
                                    deps = List.rev z.prev_deps @ z.next_deps ;
                                    estimated_duration = z.estimated_duration ;
                                    status = z.status } :: t }

  let toggle_edit z =
    if z.editing then
      { z with editing = false }
    else
      { z with editing = true ;
               next_deps =
                 match z.next_deps with
                 | [] -> [ Task.{ descr = "" ;
                                  deps = [] ;
                                  estimated_duration = None ;
                                  status = TODO } ]
                 | _ :: _ -> z.next_deps }

  let set_descr z descr =
    { z with next_deps = (
          match z.next_deps with
          | [] -> [ Task.{ descr ; deps = [] ; estimated_duration = None ; status = TODO } ]
          | h :: t -> Task.{ h with descr } :: t
        ) }
end


let root_task = Task.{
  descr = "Root" ;
  deps = [
    { descr = "X"  ; deps = [] ; estimated_duration = None ; status = TODO } ;
    { descr = "Y" ; deps = [] ; estimated_duration = None ; status = TODO } ;
  ] ;
  estimated_duration = None ;
  status = TODO ;
}

(* Definition of the vdom application *)

type model = {
  zipper : Zipper.t ;
}

type 'msg Vdom.Cmd.t +=
  | Focus of string

let rec update m = function
  | `Keydown k ->
    if not m.zipper.Zipper.editing then
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

  | `Zipper_enter -> return { zipper = Zipper.enter m.zipper }
  | `Zipper_leave -> return { zipper = Zipper.leave m.zipper }
  | `Zipper_next -> return { zipper = Zipper.next m.zipper }
  | `Zipper_prev -> return { zipper = Zipper.prev m.zipper }
  | `Zipper_toggle_edit ->
    return
      ~c:(if not m.zipper.Zipper.editing then [ Focus "task-edit" ] else [])
      { zipper = Zipper.toggle_edit m.zipper }
  | `Zipper_set_descr descr ->
    return { zipper = Zipper.set_descr m.zipper descr }

let init = { zipper = Zipper.of_task root_task }, Cmd.batch []


module View = struct
  let ul = elt "ul"
  let li = elt "li"
  let input = elt "input"
  let strong = elt "strong"

  let rec zipper_in_context z contents =
    let open Zipper in
    match z.parent with
    | None -> contents
    | Some z' ->
      zipper_in_context z' [ ul [ li (text z'.descr :: contents) ] ]

  let zipper_current_level z =
    let open Zipper in
    let line ?(highlight = false) t =
      let f x = if highlight then strong [ x ] else x in
      li [ f (text t.Task.descr) ]
    in
    let prev = List.map line (List.rev z.prev_deps) in
    let next = match z.next_deps with
      | [] -> []
      | h :: t ->
        let current =
          if z.editing then (
            let input =
              input ~a:[
                str_prop "id" "task-edit" ;
                str_prop "value" h.Task.descr ;
                str_prop "placeholder" (
                  if h.Task.descr = "" then
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
        current :: List.map line t
    in
    [ul [ li [ text z.descr ; ul (prev @ next) ] ] ]

  let zipper z =
    zipper_current_level z
    |> zipper_in_context z

  let view m = div (zipper m.zipper)

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
