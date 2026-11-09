(* The declarations of a block are a tree that is freed; an empty one is made with
   no proof, and one with a declaration needs the VALUE_SAFE proof of its own
   cells (see reject/book-declaration-forged) *)
#include "share/atspre_staload.hats"
#use css as C
#use array as A
staload B = "css/src/book.sats"

fn free_empty {n:nat}{cs:$A.cells}{g:int} (): void =
  $B.book_decls_free{n}{cs}{g}($B.BDeclsNil())

implement main0 () = ()
