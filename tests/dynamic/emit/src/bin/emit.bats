#include "share/atspre_staload.hats"
#use array as A
#use builder as B
#use css as C
#use str as S

(* Emits a rule list with each selector form (class, id, tag, pseudo,
   child, descendant), a nested @media rule, and values (rgb, a length,
   zero with no unit, a keyword, a string), and prints the CSS. *)

fn t2 (a: char, b: char): @($A.text(2), int 2) = let
  var c = @[char][2](a, b)
in @($S.text_of_chars(c, 2), 2) end

fn t3 (a: char, b: char, c: char): @($A.text(3), int 3) = let
  var x = @[char][3](a, b, c)
in @($S.text_of_chars(x, 3), 3) end

fn t4 (a: char, b: char, c: char, d: char): @($A.text(4), int 4) = let
  var x = @[char][4](a, b, c, d)
in @($S.text_of_chars(x, 4), 4) end

fn t5 (a: char, b: char, c: char, d: char, e: char): @($A.text(5), int 5) = let
  var x = @[char][5](a, b, c, d, e)
in @($S.text_of_chars(x, 5), 5) end

fn t6 (a: char, b: char, c: char, d: char, e: char, f: char): @($A.text(6), int 6) = let
  var x = @[char][6](a, b, c, d, e, f)
in @($S.text_of_chars(x, 6), 6) end

fn t7 (a: char, b: char, c: char, d: char, e: char, f: char, g: char): @($A.text(7), int 7) = let
  var x = @[char][7](a, b, c, d, e, f, g)
in @($S.text_of_chars(x, 7), 7) end

(* Prints arr[i, n) *)
fun show {l:agz}{i,n:nat | i <= n; n <= $B.BUILDER_CAP} .<n - i>.
  (a: !$A.arr(byte, l, $B.BUILDER_CAP), i: int i, n: int n): void =
  if i >= n then ()
  else let
    val () = print_char(int2char0(byte2int0($A.get<byte>(a, i))))
  in show(a, i + 1, n) end

implement main0 () = let
  (* .card { color: rgb(1, 2, 3); } *)
  val @(card, cardn) = t4('c', 'a', 'r', 'd')
  val @(color, colorn) = t5('c', 'o', 'l', 'o', 'r')
  val r1 = $C.Rule($C.Class(card, cardn), $C.Decl(color, colorn, $C.Color($C.RGB(1, 2, 3))))
  (* div > #x:hover { margin: 15px; } *)
  val @(dv, dvn) = t3('d', 'i', 'v')
  var xc = @[char][1]('x')
  val @(hover, hovern) = t5('h', 'o', 'v', 'e', 'r')
  val @(margin, marginn) = t6('m', 'a', 'r', 'g', 'i', 'n')
  val r2 = $C.Rule($C.Child($C.Tag(dv, dvn), $C.Pseudo($C.Id($S.text_of_chars(xc, 1), 1), hover, hovern)),
                   $C.Decl(margin, marginn, $C.Number_scaled(15, 0, $C.PX())))
  (* b { padding: 0; } *)
  var bc = @[char][1]('b')
  val @(padding, paddingn) = t7('p', 'a', 'd', 'd', 'i', 'n', 'g')
  val r3 = $C.Rule($C.Tag($S.text_of_chars(bc, 1), 1), $C.Decl(padding, paddingn, $C.Number_scaled(0, 0, $C.EM())))
  (* p { content: "hi"; } *)
  var pc = @[char][1]('p')
  val @(content, contentn) = t7('c', 'o', 'n', 't', 'e', 'n', 't')
  val @(hi, hin) = t2('h', 'i')
  val r4 = $C.Rule($C.Tag($S.text_of_chars(pc, 1), 1), $C.Decl(content, contentn, $C.Str(hi, hin)))
  (* @media screen { ul li { display: none; } } *)
  val @(screen, screenn) = t6('s', 'c', 'r', 'e', 'e', 'n')
  val @(ul, uln) = t2('u', 'l')
  val @(li, lin) = t2('l', 'i')
  val @(display, displayn) = t7('d', 'i', 's', 'p', 'l', 'a', 'y')
  val @(none, nonen) = t4('n', 'o', 'n', 'e')
  val inner = $C.Rule($C.Descendant($C.Tag(ul, uln), $C.Tag(li, lin)),
                      $C.Decl(display, displayn, $C.Keyword(none, nonen)))
  val r5 = $C.MediaQuery(screen, screenn, $C.RuleCons(inner, $C.RuleNil()))
  val rules = $C.RuleCons(r1, $C.RuleCons(r2, $C.RuleCons(r3, $C.RuleCons(r4, $C.RuleCons(r5, $C.RuleNil())))))
  val b = $B.create()
  val () = $C.emit_rule_list(b, rules)
  val () = $C.css_rule_list_free(rules)
  val @(arr, n) = $B.to_arr(b)
  val () = show(arr, 0, n)
in $A.free<byte>(arr) end
