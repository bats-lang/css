(* The luminance of #2a2a2a cannot be built from another channel's
   entry: the colour a LUM is about is fixed by the entries it holds *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"

prval text_on_page = $CT.CONTRAST_lighter_second{0x2a2a2a, 0xfaf8f5, 45}(
  $CT.LUMc($CT.LIN_00(), $CT.LIN_2a(), $CT.LIN_2a()),
  $CT.LUMc($CT.LIN_fa(), $CT.LIN_f8(), $CT.LIN_f5()))

implement main0 () = ()
