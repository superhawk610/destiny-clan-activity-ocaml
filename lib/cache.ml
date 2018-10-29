module type S = sig
  type k
  type v
  type cache_type
  val cache_file : string
  val load : unit -> cache_type
  val write : cache_type -> k -> v -> unit
  val read : cache_type -> k -> v
end
module type Config = sig
  type k
  type v
  val cache_file : string
end
module Make (Config : Config) : (S with type k = Config.k with type v = Config.v) = struct
  type k = Config.k
  type v = Config.v
  type cache_type = (k, v) Hashtbl.t

  let cache_file = Config.cache_file

  let open_cache_file () =
    let chan = open_out cache_file in
    let () = close_out chan in
    open_in_bin cache_file

  let persist c =
    let chan = open_out_bin cache_file in
    let () = Marshal.to_channel chan c [] in
    close_out chan

  let write c key value =
    Hashtbl.replace c key value;
    persist c

  let read c key =
    Hashtbl.find c key

  let load () =
    let chan =
      try open_in_bin cache_file with
        Sys_error _ -> open_cache_file ()
    in

    let cached : cache_type =
      try
        Marshal.from_channel chan
      with
        End_of_file -> Hashtbl.create 5000
    in

    close_in chan;
    cached
end
