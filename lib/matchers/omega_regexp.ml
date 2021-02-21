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

module Make (R: Regexp_engine_intf) = struct
  (* https://sourcegraph.com/github.com/comby-tools/mparser/-/blob/src/mParser_Char_Stream.ml#L231:8 *)
  let match_regexp s pos rex =
    R.exec ~rex ~pos:(pos - s.buffer_pos) s.buffer
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
