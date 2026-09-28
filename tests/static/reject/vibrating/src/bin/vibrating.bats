(* Saturated red text on saturated blue vibrates: neither is calm *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval v: $H.NOVIB(0xff0000, 0x0000ff) = $H.NOVIB_text($H.CALMc($H.MXMN_rgb($H.RGBc{0xff,0x00,0x00}())))

implement main0 () = ()
