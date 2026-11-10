(* A driver of the pieces of a sheet (bats-lang/css#90): it writes each piece into
   bridge's book stream with bridge's own encoders, book_sheet_span with the walk's
   cs and n and book_sheet_lit with a literal, with no $UNSAFE, praxi or assume. The
   span's OUTSIDE_READ(cs) is the driver's parameter, for the same cs that types the
   pieces; the bounds o + k <= n, k <= 65535 and m <= 1024 are in the types of the
   pieces. BPContainer stands for the container's id: bridge has no encoder that
   writes the id into the text of a sheet, so the driver writes nothing for it. *)
#include "share/atspre_staload.hats"
#use css as C
#use array as A
#use wasm.bats-packages.dev/bridge as B
staload CB = "css/src/book.sats"
staload BC = "wasm.bats-packages.dev/bridge/src/book_content.sats"
staload OU = "wasm.bats-packages.dev/bridge/src/outside.sats"

(* The digit of v, a number below 10 *)
fn digit (v: int): [m:nat | m <= 1024] string(m) =
  if v = 1 then "1" else if v = 2 then "2" else if v = 3 then "3"
  else if v = 4 then "4" else if v = 5 then "5" else if v = 6 then "6"
  else if v = 7 then "7" else if v = 8 then "8" else if v = 9 then "9"
  else "0"

fn put_number {lc:agz}{nc:pos | nc <= 64}
  (s: !$BC.book_stream, container: !$A.borrow(byte, lc, nc), container_len: int nc, v: int): void = let
  val () =
    (if v >= 10 then $BC.book_sheet_lit(s, container, container_len, digit(v / 10)) else ())
in $BC.book_sheet_lit(s, container, container_len, digit(v mod 10)) end

fun drive {l:agz}{n:nat}{cs:$A.cells}{lc:agz}{nc:pos | nc <= 64}{t:nat} .<t>.
  (from: $OU.OUTSIDE_READ(cs) |
   s: !$BC.book_stream, container: !$A.borrow(byte, lc, nc), container_len: int nc,
   src: !$A.cborrow(l, n, cs), xs: $CB.book_pieces(cs, n, t)): void =
  case+ xs of
  | ~$CB.BPNil() => ()
  | ~$CB.BPLit(lit, rest) => let
      val () = $BC.book_sheet_lit(s, container, container_len, lit)
    in drive(from | s, container, container_len, src, rest) end
  | ~$CB.BPNum(v, rest) => let
      val () = put_number(s, container, container_len, v)
    in drive(from | s, container, container_len, src, rest) end
  | ~$CB.BPSpan(o, k, rest) => let
      val () = $BC.book_sheet_span(from | s, container, container_len, src, o, k)
    in drive(from | s, container, container_len, src, rest) end
  | ~$CB.BPContainer(rest) => drive(from | s, container, container_len, src, rest)
