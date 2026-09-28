(* A card darker than the page it sits on *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval c: $H.LIGHTER(0xf0f0f0, 0xfaf8f5) = $H.LIGHTERc($CT.LUMc($CT.LIN_f0(), $CT.LIN_f0(), $CT.LIN_f0()), $CT.LUMc($CT.LIN_fa(), $CT.LIN_f8(), $CT.LIN_f5()))

implement main0 () = ()
