#include "share/atspre_staload.hats"
#use array as A
#use builder as B
#use css as C

(* Run under valgrind (the no-leaks marker): every class name and a
   stylesheet using each constructor, built and emitted again and
   again, must leave no block lost. *)

fun classes {i:nat | i <= 676} .<676 - i>. (i: int i, acc: int): int =
  if i >= 676 then acc
  else let
    val @(t, n) = $C.class_text(i)
  in classes(i + 1, acc + byte2int0($A.text_get(t, 2)) + n) end

fn sheet (): int = let
  val r1 = $C.Rule($C.Class($A.text_lit("card"), 4),
    $C.Decl($A.text_lit("color"), 5, $C.Color($C.RGB(1, 2, 3))))
  val r2 = $C.Rule($C.Child($C.Tag($A.text_lit("div"), 3),
      $C.Pseudo($C.Id($A.text_lit("x"), 1), $A.text_lit("hover"), 5)),
    $C.Decl($A.text_lit("margin"), 6, $C.Number_scaled(15, 0, $C.PX())))
  val r3 = $C.Rule($C.Tag($A.text_lit("b"), 1),
    $C.Decl($A.text_lit("color"), 5, $C.Color($C.RGBA(1, 2, 3, 4))))
  val r4 = $C.Rule($C.Tag($A.text_lit("p"), 1),
    $C.Decl($A.text_lit("content"), 7, $C.Str($A.text_lit("hi"), 2)))
  val r5 = $C.Rule($C.Tag($A.text_lit("i"), 1),
    $C.Decl($A.text_lit("color"), 5, $C.Color($C.Named($A.text_lit("red"), 3))))
  val r6 = $C.Rule($C.Tag($A.text_lit("q"), 1),
    $C.Decl($A.text_lit("width"), 5, $C.Var_ref($A.text_lit("w"), 1)))
  val r7 = $C.Rule($C.Tag($A.text_lit("s"), 1),
    $C.Decl($A.text_lit("z-index"), 7, $C.Number_bare(2)))
  val inner = $C.Rule($C.Descendant($C.Tag($A.text_lit("ul"), 2), $C.Tag($A.text_lit("li"), 2)),
    $C.Decl($A.text_lit("display"), 7, $C.Keyword($A.text_lit("none"), 4)))
  val r8 = $C.MediaQuery($A.text_lit("screen"), 6, $C.RuleCons(inner, $C.RuleNil()))
  val rules = $C.RuleCons(r1, $C.RuleCons(r2, $C.RuleCons(r3, $C.RuleCons(r4,
    $C.RuleCons(r5, $C.RuleCons(r6, $C.RuleCons(r7, $C.RuleCons(r8, $C.RuleNil()))))))))
  val b = $B.create()
  val () = $C.emit_rule_list(b, rules)
  val @(arr, n) = $B.to_arr(b)
  val () = $A.free<byte>(arr)
in n end

fun sheets {i:nat} .<i>. (i: int i, acc: int): int =
  if i <= 0 then acc else sheets(i - 1, acc + sheet())

implement main0 () = let
  val c = classes(0, 0)
  val s = sheets(100, 0)
in println!(c, " ", s) end
