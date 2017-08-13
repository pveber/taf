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

module Client : sig
  val init : unit -> unit [@@js.global]
end [@js.scope "gapi.client"]
