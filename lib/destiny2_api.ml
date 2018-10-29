open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson

let __version = "0.0.1"

(*===== constants =====*)

let api_root = "https://www.bungie.net/Platform/"
let api_key = ref ""

type platform =
  | Destiny2
  | GroupV2

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

exception Invalid_username
exception Private_account
exception No_activities
exception Out_of_retries

(*===== utilities =====*)

module Activity_cache = Cache.Make (
  struct
    type k = string
    type v = activity
    let cache_file = "activity_cache.bin"
  end
  )

module Character_cache = Cache.Make (
  struct
    type k = string
    type v = character list
    let cache_file = "character_cache.bin"
  end
  )

let cache_a = Activity_cache.load ()

let cache_c = Character_cache.load ()

let string_of_platform = function
  | Destiny2 -> "Destiny2"
  | GroupV2 -> "GroupV2"

let race_of_int = function
  | 0 -> "Human"
  | 1 -> "Awoken"
  | 2 -> "Exo"
  | _ -> "Unknown Race"

let gender_of_int = function
  | 0 -> "Male"
  | 1 -> "Female"
  | _ -> "Unknown Gender"

let character_class_of_int = function
  | 0 -> "Titan"
  | 1 -> "Hunter"
  | 2 -> "Warlock"
  | _ -> "Unknown Class"

let game_mode_of_int = function
  | 0 -> "None"
  | 2 -> "Story"
  | 3 -> "Strike"
  | 4 -> "RaidEncounter"
  | 5 -> "AllPvP"
  | 6 -> "Patrol"
  | 7 -> "AllPvE"
  | 9 -> "Reserved9"
  | 10 -> "Control"
  | 11 -> "Reserved11"
  | 12 -> "Clash"
  | 13 -> "Reserved13"
  | 15 -> "CrimsonDoubles"
  | 16 -> "Nightfall"
  | 17 -> "HeroicNightfall"
  | 18 -> "AllStrikes"
  | 19 -> "IronBanner"
  | 20 -> "Reserved20"
  | 21 -> "Reserved21"
  | 22 -> "Reserved22"
  | 24 -> "Reserved24"
  | 25 -> "AllMayhem"
  | 26 -> "Reserved26"
  | 27 -> "Reserved27"
  | 28 -> "Reserved28"
  | 29 -> "Reserved29"
  | 30 -> "Reserved30"
  | 31 -> "Supremacy"
  | 32 -> "PrivateMatchesAll"
  | 37 -> "Survival"
  | 38 -> "Countdown"
  | 39 -> "TrialsOfTheNine"
  | 40 -> "Social"
  | 41 -> "TrialsCountdown"
  | 42 -> "TrialsSurvival"
  | 43 -> "IronBannerControl"
  | 44 -> "IronBannerClash"
  | 45 -> "IronBannerSupremacy"
  | 46 -> "ScoredNightfall"
  | 47 -> "ScoredHeroicNightfall"
  | 48 -> "Rumble"
  | 49 -> "AllDoubles"
  | 50 -> "Doubles"
  | 51 -> "PrivateMatchesClash"
  | 52 -> "PrivateMatchesControl"
  | 53 -> "PrivateMatchesSupremacy"
  | 54 -> "PrivateMatchesCountdown"
  | 55 -> "PrivateMatchesSurvival"
  | 56 -> "PrivateMatchesMayhem"
  | 57 -> "PrivateMatchesRumble"
  | 58 -> "HeroicAdventure"
  | 59 -> "Showdown"
  | 60 -> "Lockdown"
  | 61 -> "Scorched"
  | 62 -> "ScorchedTeam"
  | 63 -> "Gambit"
  | 64 -> "AllPvECompetitive"
  | 65 -> "Breakthrough"
  | _ -> "UNRECOGNIZED"

let string_of_list l string_of_element line_prefix =
  let add a b = a ^ "\n" ^ line_prefix ^ "  " ^ (string_of_element b) ^ "," in
  match l with
  | [] -> "[]"
  | h::t ->
    "[\n" ^ line_prefix ^ "  "
    ^ List.fold_left add ((string_of_element h) ^ ",") t
    ^ "\n" ^ line_prefix ^ "]"

let string_of_activity a =
  "Destiny2_api.activity {\n  "
  ^ "id=" ^ a.id
  ^ ";\n  mode=" ^ game_mode_of_int a.mode
  ^ ";\n  players=" ^ string_of_list a.players (fun x -> x.name) "  "
  ^ ";\n}"

let string_of_character c =
  gender_of_int c.gender
  ^ " " ^ race_of_int c.race
  ^ " " ^ character_class_of_int c.character_class
  ^ " (Lv. " ^ string_of_int c.level
  ^ ") (" ^ string_of_int c.light
  ^ " PL)"

let trim_quotes str =
  let r = Str.regexp {|"|} in
  Str.global_replace r "" str

let auth key = api_key := key

let with_api _platform path =
  api_root ^ (string_of_platform _platform) ^ "/" ^ path

let get_detail json_string =
  let json = Basic.from_string json_string in

  let open Basic.Util in

  let account_is_private =
    json |> member "ErrorCode" |> to_int = 1665
  in
  if account_is_private then
    raise Private_account
  else
    json |> member "Response"

let get_json _platform path =
  let headers = Header.of_list [ ("X-API-Key", !api_key) ] in
  let uri = Uri.of_string (with_api _platform path) in

  print_endline ("\n" ^ Uri.to_string uri);

  let rec attempt retries_left =
    if (retries_left = 0) then
      raise Out_of_retries
    else
      let req =
        Client.get ~headers:headers uri >>= fun (_, body) ->
        body |> Cohttp_lwt.Body.to_string >|= fun body ->
        (* print_endline body; *)
        get_detail body
      in

      try Lwt_main.run req with
        _ -> attempt (retries_left - 1)
  in

  attempt 5

(*===== endpoints =====*)

let group_id_from_clan_name clan_name =
  let uri = "Name/" ^ Uri.pct_encode clan_name ^ "/1/" in
  let json = get_json GroupV2 uri in

  let open Basic.Util in
  json |> member "detail" |> member "groupId" |> to_string

let characters_for_player id =
  try
    Character_cache.read cache_c id
  with
    Not_found ->
    let uri = "4/Profile/" ^ id ^ "/?components=100,200" in
    let json = get_json Destiny2 uri in

    let open Basic.Util in
    let characters =
      List.map (
        fun x ->
          let character_id = trim_quotes (Basic.to_string x) in
          let character_json =
            json |> member "characters" |> member "data" |> member character_id
          in

          {
            id=character_id;
            level=character_json |> member "baseCharacterLevel" |> to_int;
            light=character_json |> member "light" |> to_int;
            race=character_json |> member "raceType" |> to_int;
            gender=character_json |> member "genderType" |> to_int;
            character_class=character_json |> member "classType" |> to_int
          }
      ) (
        json
        |> member "profile"
        |> member "data"
        |> member "characterIds"
        |> to_list
      )
    in

    let () = Character_cache.write cache_c id characters in
    characters


let members_of_clan group_id =
  let uri = group_id ^ "/Members/" in
  let json = get_json GroupV2 uri in

  let open Basic.Util in
  let players : (string, player) Hashtbl.t = Hashtbl.create 100 in
  let () = List.iter (
      fun x ->
        let id =
          x |> member "destinyUserInfo" |> member "membershipId" |> to_string
        in

        Hashtbl.replace players id {
          id=id;
          name=x |> member "destinyUserInfo" |> member "displayName" |> to_string;
          characters=characters_for_player id
        }
    ) (json |> member "results" |> to_list) in

  players

let player_from_username username =
  let re = Str.regexp {|\(#\|%23\)|} in
  let _ =
    try Str.search_forward re username 0
    with Not_found -> raise Invalid_username
  in

  let uri = "SearchDestinyPlayer/4/" ^ Uri.pct_encode username ^ "/" in
  let json = get_json Destiny2 uri in

  let open Basic.Util in
  let user = List.nth (json |> to_list) 0 in
  let id = user |> member "membershipId" |> to_string in

  {
    id=id;
    name=user |> member "displayName" |> to_string;
    characters=characters_for_player id
  }

let postgame_carnage_report_for_activity id =
  let uri = "Stats/PostGameCarnageReport/" ^ id ^ "/" in
  let json = get_json Destiny2 uri in

  let open Basic.Util in
  json |> member "entries" |> to_list

let activities_for_character (p : player) (c : character) from_date to_date =
  let uri_for_page page =
    "4/Account/"
    ^ p.id ^ "/Character/"
    ^ c.id ^ "/Stats/Activities/?count=100&modes='5,7'&page="
    ^ string_of_int page
  in

  let rec get_activities acc page =
    let uri = uri_for_page page in
    let json = get_json Destiny2 uri in

    let open Basic.Util in
    let activities =
      try json |> member "activities" |> to_list with
        Type_error _ -> [] 
    in

    if (List.length activities = 0) then
      raise No_activities
    else
      let recurse = true in
      let formatted = List.fold_right (
          fun x a ->
            if (not recurse) then a
            else
              let id =
                x |> member "activityDetails" |> member "instanceId" |> to_string
              in

              let activity_date = Date.from_iso (x |> member "period" |> to_string) in
              if (Date.is_before activity_date from_date)
              then let _ = recurse = false in a
              else if (Date.is_after activity_date to_date)
              then a
              else
                let cached =
                  try Activity_cache.read cache_a id with
                    Not_found ->
                    let activity_entry = {
                      id=id;
                      mode=x |> member "activityDetails" |> member "mode" |> to_int;
                      players=List.map (
                          fun entry ->
                            let user_info =
                              entry |> member "player" |> member "destinyUserInfo"
                            in

                            {
                              id=user_info |> member "membershipId" |> to_string;
                              name=user_info |> member "displayName" |> to_string
                            }
                        ) (postgame_carnage_report_for_activity id)
                    } in
                    let () = Activity_cache.write cache_a id activity_entry in
                    activity_entry
                in

                cached :: a
        ) activities acc in

      if (recurse && (List.length activities = 100))
      then get_activities formatted (page + 1)
      else (List.rev formatted)
  in

  get_activities [] 0

let merge_hashtables_and_add_collisions (h1 : (string, int) Hashtbl.t) (h2 : (string, int) Hashtbl.t) =
  let open Hashtbl in
  let h = create (length h1) in
  let () = iter (
      fun k v ->
        if (mem h2 k) then
          let h2v = find h2 k in
          replace h k (v + h2v)
        else replace h k v
    ) h1
  in
  let () = iter (
      fun k v ->
        if (mem h1 k) then ()
        else replace h k v
    ) h2
  in
  h

let score_for_character (p : player) (c : character) from_date to_date =
  let activities = activities_for_character p c from_date to_date in

  let () =
    print_endline (
      "scoring "
      ^ p.name
      ^ "'s " ^ (string_of_character c)
    )
  in

  List.fold_left (
    fun acc _a ->
      {
        composite=acc.composite + 1;
        games=acc.games + 1;
        clan_teammates="foo" :: acc.clan_teammates;
        modes=acc.modes;
        completions=acc.completions
      }
  ) {
    composite=0;
    games=0;
    clan_teammates=[];
    modes=Hashtbl.create 10;
    completions=Hashtbl.create 10
  } activities

let score_for_player (p : player) from_date to_date =
  List.fold_left (
    fun a c ->
      let character_score = score_for_character p c from_date to_date in

      {
        composite=a.composite + character_score.composite;
        games=a.games + character_score.games;
        clan_teammates=a.clan_teammates @ character_score.clan_teammates;
        modes=merge_hashtables_and_add_collisions a.modes character_score.modes;
        completions=merge_hashtables_and_add_collisions a.completions character_score.completions
      }
  ) {
    composite=0;
    games=0;
    clan_teammates=[];
    modes=Hashtbl.create 10;
    completions=Hashtbl.create 10
  } p.characters

let _ = game_mode_of_int 0
