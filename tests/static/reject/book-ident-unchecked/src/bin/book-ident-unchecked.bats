(* A container id whose first byte is a digit (48) is not an identifier: the start
   of an identifier is a letter, a hyphen or an underscore, and the proof of the
   start is made only for those *)
#include "share/atspre_staload.hats"
#use css as C
#use array as A
staload B = "css/src/book.sats"

prfn digit_first {cs:$A.cells} (nth: $A.NTH(cs, 0, 48)): $B.CSS_IDENT(cs, 0, 1) =
  $B.CSS_IDENT_mk(nth, $B.IDENT_START_lower(), $B.IDENT_BODY_nil())

implement main0 () = ()
