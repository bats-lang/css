(* A purple accent in a theme whose families are warm, green and red *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval fams = $H.FAMILIESc{25, 50, 135, 165, ~15, 15}()
prval p: $H.IN3(0x7b4fd0, 25, 50, 135, 165, ~15, 15) = $H.IN3_2(fams, $H.HUE_b_r_g($H.RGBc{0x7b,0x4f,0xd0}()))

implement main0 () = ()
