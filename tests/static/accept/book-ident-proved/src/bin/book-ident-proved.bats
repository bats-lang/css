(* A container id of one letter is an identifier when the byte at 0 is known to be
   a lower-case letter (97): the proof is made from the array's NTH fact *)
#include "share/atspre_staload.hats"
#use css as C
#use array as A
staload B = "css/src/book.sats"

prfn one_letter {cs:$A.cells} (nth: $A.NTH(cs, 0, 97)): $B.CSS_IDENT(cs, 0, 1) =
  $B.CSS_IDENT_mk(nth, $B.IDENT_START_lower(), $B.IDENT_BODY_nil())

implement main0 () = ()
