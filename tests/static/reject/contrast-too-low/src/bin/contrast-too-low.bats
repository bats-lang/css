(* Grey #6b6b6b on #dddddd is about 3.9:1: short of 4.5 *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"

prval muted_on_line = $CT.CONTRAST_lighter_second{0x6b6b6b, 0xdddddd, 45}(
  $CT.LUMc($CT.LIN_6b(), $CT.LIN_6b(), $CT.LIN_6b()),
  $CT.LUMc($CT.LIN_dd(), $CT.LIN_dd(), $CT.LIN_dd()))

implement main0 () = ()
