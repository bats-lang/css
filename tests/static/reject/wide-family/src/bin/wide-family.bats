(* A hue family 60 degrees wide is two families *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

prval f = $H.FAMILIESc{0, 60, 135, 165, ~15, 15}()

implement main0 () = ()
