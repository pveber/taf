type t = private Ojs.t

type library = [`auth2 | `client] [@js.enum]

type load_config = private Ojs.t

val load_config :
  callback:(unit -> unit) ->
  onerror:(unit -> unit) ->
  timeout:float ->
  ontimeout:(unit -> unit) ->
  load_config
[@@js.builder]

val load :
  library list ->
  [`Callback of (unit -> unit) | `Config of load_config] ->
  unit
[@@js.custom
  val load_internal :
    string ->
    ([`Callback of (unit -> unit) | `Config of load_config] [@js.union]) ->
    unit
  [@@js.global "gapi.load"]

  let string_of_library = function
    | `client -> "client"
    | `auth2 -> "auth2"

  let load libs =
    List.map string_of_library libs
    |> String.concat ":"
    |> load_internal
]


[@@@js.stop]
module Promise0 : sig
  type t = private Ojs.t
  val t_to_js: t -> Ojs.t
  val t_of_js: Ojs.t -> t
  val then_final : (unit -> unit) -> t -> unit
end

module Promise : sig
  type 'a t = private Ojs.t
  val t_to_js: ('a -> Ojs.t) -> 'a t -> Ojs.t
  val t_of_js: (Ojs.t -> 'a) -> Ojs.t -> 'a t
  val then_final : ('a -> unit) -> 'a t -> unit
end
[@@@js.start]

[@@@js.implem
  module Promise0 = struct
    type t = Ojs.t
    let t_to_js x = x
    let t_of_js x = x
    let then_final f x =
      Ojs.call x "then" [| Ojs.fun_to_js 1 (fun _ -> f ()) |]
      |> ignore
  end

  module Promise = struct
    type 'a t = Ojs.t
    let t_to_js f x = x
    let t_of_js f x = x
    let then_final f x =
      Js.Unsafe.meth_call x "then" [|Js.Unsafe.inject f|]
  end
]

module Client : sig
  val init :
    ?apiKey:string ->
    discoveryDocs:string list ->
    clientId:string ->
    scope:string ->
    unit -> Promise0.t
  [@@js.custom
    val init_arg :
      ?apiKey:string ->
      discoveryDocs:string list ->
      clientId:string ->
      scope:string ->
      unit ->
      Ojs.t
    [@@js.builder]

    let init ?apiKey ~discoveryDocs ~clientId ~scope () =
      Ojs.call (Ojs.get (Ojs.get Ojs.global "gapi") "client") "init" [|
        init_arg ?apiKey ~discoveryDocs ~clientId ~scope ()
      |]
  ]
end

module SignStatus : sig
  type t
  val get : t -> bool
  [@@js.call]
end

module GoogleAuth : sig
  type t

  val isSignedIn : t -> SignStatus.t

  val signIn : t -> Promise0.t
  [@@js.call]
end

module Auth2 : sig
  val getAuthInstance : unit -> GoogleAuth.t
  [@@js.global "gapi.auth2.getAuthInstance"]
end
