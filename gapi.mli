type library = [`auth2 | `client] [@js.enum]

val load :
  library list ->
  (unit -> unit) ->
  unit
[@@js.custom
  val load_internal :
    string ->
    (unit -> unit) ->
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
