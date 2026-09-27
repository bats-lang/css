#include "share/atspre_staload.hats"
#use array as A
#use css as C

(* class_text maps 0 -> caa, 25 -> caz, 26 -> cba, 675 -> czz. *)
fn show {i:nat | i < 676} (i: int i): void = let
  val @(t, _) = $C.class_text(i)
  val () = print_char(int2char0(byte2int0($A.text_get(t, 0))))
  val () = print_char(int2char0(byte2int0($A.text_get(t, 1))))
  val () = print_char(int2char0(byte2int0($A.text_get(t, 2))))
in print_newline() end

implement main0 () = let
  val () = show(0)
  val () = show(25)
  val () = show(26)
in show(675) end
