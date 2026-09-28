(* put_rgb writes a colour as #rrggbb *)
#include "share/atspre_staload.hats"
#use builder as B
#use array as A
#use css as C
staload CT = "css/src/contrast.sats"

fun show {l:agz}{i,n:nat | i <= n; n <= $B.BUILDER_CAP} .<n - i>.
  (a: !$A.arr(byte, l, $B.BUILDER_CAP), i: int i, n: int n): void =
  if i >= n then ()
  else let
    val () = print_char(int2char0(byte2int0($A.get<byte>(a, i))))
  in show(a, i + 1, n) end

implement main0 () = let
  val b = $B.create()
  val () = $CT.put_rgb(b, 0x2f6f4f)
  val () = $B.put_char(b, 10)
  val () = $CT.put_rgb(b, 0)
  val () = $B.put_char(b, 10)
  val () = $CT.put_rgb(b, 0xffffff)
  val () = $B.put_char(b, 10)
  val @(a, n) = $B.to_arr(b)
  val () = show(a, 0, n)
in $A.free<byte>(a) end
