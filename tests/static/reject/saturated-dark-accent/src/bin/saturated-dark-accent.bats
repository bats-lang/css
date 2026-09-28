(* A fully saturated accent on a dark theme is not calm *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval a: $H.CALM(0x00c853) = $H.CALMc($H.MXMN_gbr($H.RGBc{0x00,0xc8,0x53}()))

implement main0 () = ()
