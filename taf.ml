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

type msg =
  | Zipper_enter
  | Zipper_leave
  | Zipper_next
  | Zipper_prev
  | Zipper_toggle_edit
  | Zipper_set_descr of string

(* this is nasty *)
let editing = ref false

let update m = function
  | Zipper_enter -> { zipper = Zipper.enter m.zipper }
  | Zipper_leave -> { zipper = Zipper.leave m.zipper }
  | Zipper_next -> { zipper = Zipper.next m.zipper }
  | Zipper_prev -> { zipper = Zipper.prev m.zipper }
  | Zipper_toggle_edit ->
    editing := not !editing ;
    { zipper = Zipper.toggle_edit m.zipper }
  | Zipper_set_descr descr -> { zipper = Zipper.set_descr m.zipper descr }

let init = { zipper = Zipper.of_task root_task }


module View = struct
  let ul = elt "ul"
  let li = elt "li"
  let input = elt "input"

  let rec zipper_in_context z contents =
    let open Zipper in
    match z.parent with
    | None -> contents
    | Some z' ->
      zipper_in_context z' [ elt "ul" [ elt "li" (text z'.descr :: contents) ] ]

  let zipper_current_level z =
    let open Zipper in
    let line ?(highlight = false) t =
      let f x = if highlight then elt "strong" [ x ] else x in
      elt "li" [ f (text t.Task.descr) ]
    in
    let prev = List.map line (List.rev z.prev_deps) in
    let next = match z.next_deps with
      | [] -> []
      | h :: t ->
        let current =
          if z.editing then (
            let input =
              input ~a:[
                str_prop "id" "task-descr" ;
                str_prop "value" h.Task.descr ;
                str_prop "placeholder" (
                  if h.Task.descr = "" then
                    "Enter a task description"
                  else ""
                ) ;
                oninput (fun s -> Zipper_set_descr s)
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

let my_app = simple_app ~init ~update ~view:View.view ()


(* Driver *)

open Js_browser

let run () =
  let app = Vdom_blit.run my_app   (* run the application *) in
  let keydown_handler ev =
    if not !editing then (
      match Event.which ev with
      | 39 -> Vdom_blit.process app Zipper_enter
      | 37 -> Vdom_blit.process app Zipper_leave
      | 38 -> Vdom_blit.process app Zipper_prev
      | 40 -> Vdom_blit.process app Zipper_next
      | _ -> ()
    ) ;
    if Event.which ev = 13 then (
      Vdom_blit.process app Zipper_toggle_edit ;
      (
        match Document.get_element_by_id document "task-descr" with
        | None -> ()
        | Some e -> Element.focus e
      )
    )


  in
  Window.add_event_listener window "keydown" keydown_handler false ;
  app
  |> Vdom_blit.dom    (* get its root DOM container *)
  |> Element.append_child (Document.body document)   (* insert the DOM in the document *)

let () = Window.set_onload window run
