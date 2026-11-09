(* A declaration of the sheet cs1 cannot carry the VALUE_SAFE proof about the bytes of
   another array cs2: the proof names its cells *)
#include "share/atspre_staload.hats"
#use css as C
#use array as A
staload B = "css/src/book.sats"

fn forge {n:nat}{cs1,cs2:$A.cells}{o,k:nat | o + k <= n}{g,p:int}
  (pf: $B.VALUE_SAFE(cs2, o, k, g, p) | prop: int p, o: int o, k: int k)
  : $B.book_decls(cs1, n, g, 1) =
  $B.BDecl(pf | prop, o, k, $B.BDeclsNil())

implement main0 () = ()
