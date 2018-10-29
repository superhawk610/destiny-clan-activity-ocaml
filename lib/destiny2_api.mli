val __version : string

(*===== constants =====*)
val api_root : string
type platform = Destiny2 | GroupV2
type character = {
  id : string;
  level : int;
  light : int;
  race : int;
  gender : int;
  character_class : int
}
type player_info = {
  id : string;
  name : string
}
type player = {
  id : string;
  name : string;
  characters: character list
}
type activity = {
  id : string;
  mode : int;
  players : player_info list
}
type score = {
  composite : int;
  games : int;
  clan_teammates : string list;
  modes: (string, int) Hashtbl.t;
  completions: (string, int) Hashtbl.t
}

(*===== exceptions =====*)
exception Invalid_username
(* Username doesn't include "#" or "%23" and thus isn't
 * a valid Battle.net username *)

exception Private_account
(* Account is private and not allowed to list activities *)

exception No_activities
(* Character has no recorded activities *)

exception Out_of_retries
(* Request failed the maximum allowable number of times *)

(*===== utilities =====*)
val auth : string -> unit
(** [auth api_key] set the X-API-Key header for all subsequent requests *)

val string_of_activity : activity -> string

val string_of_character : character -> string

(*===== endpoints =====*)
val get_json : platform -> string -> Yojson.Basic.json
(** [get_json platform path] Get the JSON response from the given API endpoint.
 ** Only use this if a wrapper doesn't exist for that endpoint. *)

val group_id_from_clan_name : string -> string

val members_of_clan : string -> (string, player) Hashtbl.t

val player_from_username : string -> player
(** [player_from_username username] Get the membershipId for the given user *)

val characters_for_player : string -> character list
(** [characters_for_player id] Get a list of characters for the given user *)

val activities_for_character : player -> character -> Date.date -> Date.date -> activity list

val score_for_character : player -> character -> Date.date -> Date.date -> score

val score_for_player : player -> Date.date -> Date.date -> score