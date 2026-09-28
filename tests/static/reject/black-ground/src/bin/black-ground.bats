(* A dark theme's ground is not pure black *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval g: $H.PEAK(0x000000, 18, 255) = $H.PEAKc($H.MXMN_rgb($H.RGBc{0x00,0x00,0x00}()))

implement main0 () = ()
