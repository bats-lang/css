(* A neon accent beside a muted danger colour: saturations 1.0 and 0.33 *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval s: $H.SATNEAR(0x00ff66, 0xffb4ab, 30) = $H.SATNEARc($H.MXMN_gbr($H.RGBc{0x00,0xff,0x66}()), $H.MXMN_rgb($H.RGBc{0xff,0xb4,0xab}()))

implement main0 () = ()
