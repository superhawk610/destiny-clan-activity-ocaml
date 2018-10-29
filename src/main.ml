open Destiny2_api

let api_key = "b747cceae0e04032ba5b6a8051624f09"

let () = auth api_key
let group_id = group_id_from_clan_name "Charlie Company 337"
let clan_members = members_of_clan group_id
let end_date = Date.now ()
let start_date = Date.start_of_month end_date

let () = print_endline (
    "From: " ^ Date.to_string start_date
    ^ "\n  To: " ^ Date.to_string end_date
  )

let () = Hashtbl.iter (
    fun _ (p : player) ->
      let characters = characters_for_player p.id in

      List.iter (
        fun (c : character) ->
          let _ =
            activities_for_character p c start_date end_date
          in ()
      ) characters
  ) clan_members
