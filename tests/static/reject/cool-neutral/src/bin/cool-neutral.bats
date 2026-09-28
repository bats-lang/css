(* A cool blue-grey card (hue 213) is not a neutral of a warm theme *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval n: $H.NEUTRAL(0xe8eef5, 48, 25, 50) = $H.NEUTRAL_tint($H.CHROMAc($H.MXMN_bgr($H.RGBc{0xe8,0xee,0xf5}())), $H.HUE_b_g_r($H.RGBc{0xe8,0xee,0xf5}()))

implement main0 () = ()
