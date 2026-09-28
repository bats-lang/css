(* A dark theme's text is not pure white *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval t: $H.PEAK(0xffffff, 0, 232) = $H.PEAKc($H.MXMN_rgb($H.RGBc{0xff,0xff,0xff}()))

implement main0 () = ()
