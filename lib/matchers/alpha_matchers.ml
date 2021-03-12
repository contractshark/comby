open Core
open Languages

module Matcher = Alpha

module Metasyntax : Types.Metasyntax.S = struct
  let everything  = Some (Some ":[", Some "]")
  let expression  = Some (Some ":[", Some ":e]")
  let alphanum  = Some (Some ":[[", Some "]]")
  let non_space = Some (Some ":[", Some ".]")
  let line  = Some (Some ":[", Some "\\n]")
  let blank  = Some (Some ":[ ", Some "]")
  let regex  = Some (":[", '~', "]")
end

module Text = Matcher.Make (Text) (Metasyntax)
module Paren = Matcher.Make (Paren) (Metasyntax)
module Dyck = Matcher.Make (Dyck) (Metasyntax)
module JSON = Matcher.Make (JSON) (Metasyntax)
module JSONC = Matcher.Make (JSONC) (Metasyntax)
module GraphQL = Matcher.Make (GraphQL) (Metasyntax)
module Dhall = Matcher.Make (Dhall) (Metasyntax)
module Latex = Matcher.Make (Latex) (Metasyntax)
module Assembly = Matcher.Make (Assembly) (Metasyntax)
module Clojure = Matcher.Make (Clojure) (Metasyntax)
module Lisp = Matcher.Make (Lisp) (Metasyntax)
module Generic = Matcher.Make (Generic) (Metasyntax)
module Bash = Matcher.Make (Bash) (Metasyntax)
module Ruby = Matcher.Make (Ruby) (Metasyntax)
module Elixir = Matcher.Make (Elixir) (Metasyntax)
module Python = Matcher.Make (Python) (Metasyntax)
module Html = Matcher.Make (Html) (Metasyntax)
module Xml = Matcher.Make (Xml) (Metasyntax)
module SQL = Matcher.Make (SQL) (Metasyntax)
module Erlang = Matcher.Make (Erlang) (Metasyntax)
module C = Matcher.Make (C) (Metasyntax)
module Csharp = Matcher.Make (Csharp) (Metasyntax)
module Java = Matcher.Make (Java) (Metasyntax)
module CSS = Matcher.Make (CSS) (Metasyntax)
module Kotlin = Matcher.Make (Kotlin) (Metasyntax)
module Scala = Matcher.Make (Scala) (Metasyntax)
module Nim = Matcher.Make (Nim) (Metasyntax)
module Dart = Matcher.Make (Dart) (Metasyntax)
module Php = Matcher.Make (Php) (Metasyntax)
module Go = Matcher.Make (Go) (Metasyntax)
module Javascript = Matcher.Make (Javascript) (Metasyntax)
module Jsx = Matcher.Make (Jsx) (Metasyntax)
module Typescript = Matcher.Make (Typescript) (Metasyntax)
module Tsx = Matcher.Make (Tsx) (Metasyntax)
module Swift = Matcher.Make (Swift) (Metasyntax)
module Rust = Matcher.Make (Rust) (Metasyntax)
module OCaml = Matcher.Make (OCaml) (Metasyntax)
module Reason = Matcher.Make (Reason) (Metasyntax)
module Fsharp = Matcher.Make (Fsharp) (Metasyntax)
module Pascal = Matcher.Make (Pascal) (Metasyntax)
module Julia = Matcher.Make (Julia) (Metasyntax)
module Fortran = Matcher.Make (Fortran) (Metasyntax)
module Haskell = Matcher.Make (Haskell) (Metasyntax)
module Elm = Matcher.Make (Elm) (Metasyntax)
module Zig = Matcher.Make (Zig) (Metasyntax)
module Coq  = Matcher.Make (Coq) (Metasyntax)
module Move = Matcher.Make (Move) (Metasyntax)
module Solidity = Matcher.Make (Solidity) (Metasyntax)
module C_nested_comments = Matcher.Make (C_nested_comments) (Metasyntax)

let all : (module Types.Matcher.S) list =
  [ (module Assembly)
  ; (module Bash)
  ; (module C)
  ; (module Csharp)
  ; (module CSS)
  ; (module Dart)
  ; (module Dyck)
  ; (module Clojure)
  ; (module Coq)
  ; (module Elm)
  ; (module Erlang)
  ; (module Elixir)
  ; (module Fortran)
  ; (module Fsharp)
  ; (module Go)
  ; (module Html)
  ; (module Haskell)
  ; (module Java)
  ; (module Javascript)
  ; (module Jsx)
  ; (module JSON)
  ; (module JSONC)
  ; (module GraphQL)
  ; (module Dhall)
  ; (module Julia)
  ; (module Kotlin)
  ; (module Latex)
  ; (module Lisp)
  ; (module Move)
  ; (module Nim)
  ; (module OCaml)
  ; (module Paren)
  ; (module Pascal)
  ; (module Php)
  ; (module Python)
  ; (module Reason)
  ; (module Ruby)
  ; (module Rust)
  ; (module Scala)
  ; (module Solidity)
  ; (module SQL)
  ; (module Swift)
  ; (module Text)
  ; (module Typescript)
  ; (module Tsx)
  ; (module Xml)
  ; (module Zig)
  ; (module Generic)
  ]

let select_with_extension extension : (module Types.Matcher.S) option =
  List.find all ~f:(fun (module M) -> List.exists M.extensions ~f:(String.(=) extension))

let with_metasyntax
    (module Language : Types.Language.S)
    (definitions : Types.Metasyntax.t) =
  let open Polymorphic_compare in
  let hole sort =
    List.find_map definitions ~f:(function
        | Delimited (sort', left, right) when sort' = sort -> Some (left, right)
        | Delimited (sort', Some left, None)
        | Prefix (sort', left) when sort' = sort -> Some (Some left, None)
        | _ -> Some (Some "AA3BEDC4-8D36-4368-8372-0E475A4A2F7D", Some "AA3BEDC4-8D36-4368-8372-0E475A4A2F7D"))
  in
  let regex =
    List.find_map definitions ~f:(function
        | Regex (left, separator, right) -> Some (left, separator, right)
        | _ -> Some ("AA3BEDC4-8D36-4368-8372-0E475A4A2F7D", '~',"AA3BEDC4-8D36-4368-8372-0E475A4A2F7D"))
  in
  let module Metasyntax = struct
    let everything  = hole Everything
    let expression = hole Expression
    let alphanum = hole Alphanum
    let non_space  = hole Non_space
    let line = hole Line
    let blank = hole Blank
    let regex = regex
  end
  in
  (module Alpha.Make (Language) (Metasyntax) : Types.Matcher.S)

let create
    Types.Syntax.
      { user_defined_delimiters
      ; escapable_string_literals
      ; raw_string_literals
      ; comments
      } =
  let module Info = struct
    let name = "User_defined_language"
    let extensions = []
  end
  in
  let module Syntax = struct
    let user_defined_delimiters = user_defined_delimiters
    let escapable_string_literals = escapable_string_literals
    let raw_string_literals = raw_string_literals
    let comments = comments
  end
  in
  let module User_language = struct
    module Info = Info
    module Syntax = Syntax
  end
  in
  (module Alpha.Make (User_language) (Metasyntax) : Types.Matcher.S)
