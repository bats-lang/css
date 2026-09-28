(* Quire's light theme: body text on the page, and the primary
   button's label on its fill, are at least 4.5:1 *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"

prval text_on_page = $CT.CONTRAST_lighter_second{0x2a2a2a, 0xfaf8f5, 45}(
  $CT.LUMc($CT.LIN_2a(), $CT.LIN_2a(), $CT.LIN_2a()),
  $CT.LUMc($CT.LIN_fa(), $CT.LIN_f8(), $CT.LIN_f5()))

prval label_on_button = $CT.CONTRAST_lighter_first{0xffffff, 0x2f6f4f, 45}(
  $CT.LUMc($CT.LIN_ff(), $CT.LIN_ff(), $CT.LIN_ff()),
  $CT.LUMc($CT.LIN_2f(), $CT.LIN_6f(), $CT.LIN_4f()))

implement main0 () = ()
