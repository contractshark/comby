open Core
open Matchers

let configuration = Configuration.create ~match_kind:Fuzzy ()

let create = Matchers.Alpha.with_metasyntax (module Matchers.Languages.Go)

let run (module M : Matchers.Matcher) source match_template _rewrite_template =
  M.all ~configuration ~template:match_template ~source ()
  |> function
  | [] -> print_string "No matches."
  | results -> print_string (Format.asprintf "%a" Match.pp_json_lines (None, results))

let%expect_test "custom_metasyntax_everything" =
  let matcher = create
      [ Delimited (Everything, Some "$", None)
      ]
  in

  let source = "simple(test)" in
  run matcher source "simple($a)" "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":12,"line":1,"column":13}},"environment":[{"variable":"a","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"simple(test)"}]}
|}];

  let source = "(nested(test))" in
  run matcher source "($a)" "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":14,"line":1,"column":15}},"environment":[{"variable":"a","value":"nested(test)","range":{"start":{"offset":1,"line":1,"column":2},"end":{"offset":13,"line":1,"column":14}}}],"matched":"(nested(test))"}]}
|}];

  let source = "flat stuff yeah" in
  run matcher source "flat $a yeah" "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":15,"line":1,"column":16}},"environment":[{"variable":"a","value":"stuff","range":{"start":{"offset":5,"line":1,"column":6},"end":{"offset":10,"line":1,"column":11}}}],"matched":"flat stuff yeah"}]}
|}]

let%expect_test "custom_metasyntax_regex" =
  let matcher = create
      [ Regex ("$", ':', " ")
      ]
  in
  let source = "simple(test)" in
  run matcher source {|$a:\w+ |} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}},"environment":[{"variable":"a","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}}],"matched":"simple"},{"range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}},"environment":[{"variable":"a","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"test"}]}
|}]

let%expect_test "custom_metasyntax_multiple_holes" =
  let matcher = create
      [ Delimited (Everything, Some "$", None)
      ; Delimited (Alphanum, Some "$$", None)
      ; Regex ("$", ':', " ")
      ]
  in
  let source = "simple(test)after" in

  run matcher source {|$a($$b)|} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":12,"line":1,"column":13}},"environment":[{"variable":"","value":"","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":7,"line":1,"column":8}}},{"variable":"a","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"b","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"simple(test)"}]}
|}];

  run matcher source {|$$a($b)|} "";
  [%expect_exact {|{"uri":null,"matches":[{"range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":12,"line":1,"column":13}},"environment":[{"variable":"","value":"","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":0,"line":1,"column":1}}},{"variable":"a","value":"simple","range":{"start":{"offset":0,"line":1,"column":1},"end":{"offset":6,"line":1,"column":7}}},{"variable":"b","value":"test","range":{"start":{"offset":7,"line":1,"column":8},"end":{"offset":11,"line":1,"column":12}}}],"matched":"simple(test)"}]}
|}];

  (* FIXME: $c not interpreted as regex. Changing the ordering in the definition causes previous cases to fail. *)
  run matcher source {|$$a($b)$c:\w+ |} "";
  [%expect_exact {|No matches.|}]
