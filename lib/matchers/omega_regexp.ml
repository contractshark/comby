open Angstrom

module type Regexp_engine_intf = sig
  type t
  type substrings

  val make: string -> t

  val get_substring: substrings -> int -> string option

  val get_all_substrings: substrings -> string array

  val exec: rex:t -> pos:int -> Bytes.t -> substrings option
end

type t =
  { buffer_pos : int
  ; buffer : bytes
  }

(* I think I should just implement the analog of string_ for regex with some bounded buffer size. *)

module Make (Regexp: Regexp_engine_intf) = struct
  (* https://sourcegraph.com/github.com/comby-tools/mparser/-/blob/src/mParser_Char_Stream.ml#L231:8 *)
  let match_regexp s pos rex =
    Regexp.exec ~rex ~pos:(pos - s.buffer_pos) s.buffer

  let make_regexp pat =
    Regexp.make pat

  (* FIXME: size. FIXME advance => want to use internal unsafe_apply_opt
     actually. cf. string_ in angstrom.ml. actually, do peek, then
     advance/commit. *)
  let regexp rex : string Angstrom.t =
    let match_option =
      Unsafe.peek 10000 (fun buffer ~off ~len:_ ->
          Format.printf "Got string %s@." @@ Bigstringaf.to_string buffer;
          let bytes = Bigstringaf.to_string buffer |> Bytes.of_string in
          match Regexp.exec ~rex ~pos:off bytes with
          | None -> None
          | Some substrings ->
            match Regexp.get_substring substrings 0 with
            | None -> None
            | Some result ->
              Some (result, String.length result))
    in
    match_option >>= function
    | Some (result, n) ->
      Unsafe.take n (fun _ ~off:_ ~len:_ -> ()) >>= fun _ ->
      return result
    | None -> fail "No match" (* this will not backtrack I think *)

end

module PCRE = struct
  module Engine : Regexp_engine_intf = struct
    type t = Pcre.regexp
    type substrings = Pcre.substrings

    let compile_flags =
      Pcre.cflags [ `ANCHORED ]

    let make pattern =
      Pcre.regexp ~iflags:compile_flags pattern

    let get_substring s idx =
      match Pcre.get_substring s idx with
      | result -> Some result
      | exception Not_found
      | exception Invalid_argument _ -> None

    let get_all_substrings s =
      Pcre.get_substrings s

    let exec ~rex ~pos b =
      match Pcre.exec ~pos ~rex (Bytes.unsafe_to_string b) with
      | result -> Some result
      | exception Not_found -> None
  end

  include Make(Engine)
end

module RE = struct
  module Engine : Regexp_engine_intf = struct
    type t = Re.re
    type substrings = Re.substrings

    let compile_flags =
      [ `Anchored ]

    let make pattern =
      Re.Perl.(compile (re ~opts:compile_flags pattern))

    let get_substring s idx =
      match Re.get s idx with
      | result -> Some result
      | exception Not_found -> None

    let get_all_substrings s =
      Re.get_all s

    let exec ~rex ~pos b =
      match Re.exec ~pos rex (Bytes.unsafe_to_string b) with
      | result -> Some result
      | exception Not_found -> None
  end

  include Make(Engine)
end
