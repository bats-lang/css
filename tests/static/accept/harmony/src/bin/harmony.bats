(* Colours that follow the harmony rules *)
#include "share/atspre_staload.hats"
#use css as C
staload CT = "css/src/contrast.sats"
staload H = "css/src/harmony.sats"

(* Quire's light theme: the page's ground is a warm neutral *)
prval page: $H.NEUTRAL(0xfaf8f5, 48, 25, 50) = $H.NEUTRAL_tint($H.CHROMAc($H.MXMN_rgb($H.RGBc{0xfa,0xf8,0xf5}())), $H.HUE_r_g_b{0xfaf8f5,0xfa,0xf8,0xf5,25,50}($H.RGBc{0xfa,0xf8,0xf5}()))

(* its text is grey *)
prval text: $H.NEUTRAL(0x2a2a2a, 48, 25, 50) = $H.NEUTRAL_grey($H.CHROMAc($H.MXMN_rgb($H.RGBc{0x2a,0x2a,0x2a}())))

(* the accent (green) and the danger colour (red) are in two of its three
   hue families, and the highlight (yellow) in the page's *)
prval fams = $H.FAMILIESc{25, 50, 135, 165, ~15, 15}()
prval accent: $H.IN3(0x2f6f4f, 25, 50, 135, 165, ~15, 15) = $H.IN3_2(fams, $H.HUE_g_b_r{0x2f6f4f,0x2f,0x6f,0x4f,135,165}($H.RGBc{0x2f,0x6f,0x4f}()))
prval danger: $H.IN3(0xb3261e, 25, 50, 135, 165, ~15, 15) = $H.IN3_3(fams, $H.HUE_r_g_b{0xb3261e,0xb3,0x26,0x1e,~15,15}($H.RGBc{0xb3,0x26,0x1e}()))
prval hl: $H.IN3(0xfde59a, 25, 50, 135, 165, ~15, 15) = $H.IN3_1(fams, $H.HUE_r_g_b{0xfde59a,0xfd,0xe5,0x9a,25,50}($H.RGBc{0xfd,0xe5,0x9a}()))

(* the accent and the danger colour are as saturated, within 0.3 *)
prval voices: $H.SATNEAR(0x2f6f4f, 0xb3261e, 30) = $H.SATNEARc($H.MXMN_gbr($H.RGBc{0x2f,0x6f,0x4f}()), $H.MXMN_rgb($H.RGBc{0xb3,0x26,0x1e}()))

(* white on the accent does not vibrate: white is calm *)
prval label: $H.NOVIB(0xffffff, 0x2f6f4f) = $H.NOVIB_text($H.CALMc($H.MXMN_rgb($H.RGBc{0xff,0xff,0xff}())))

(* a card is lighter than the page *)
prval card: $H.LIGHTER(0xffffff, 0xfaf8f5) = $H.LIGHTERc($CT.LUMc($CT.LIN_ff(), $CT.LIN_ff(), $CT.LIN_ff()), $CT.LUMc($CT.LIN_fa(), $CT.LIN_f8(), $CT.LIN_f5()))

(* the dark theme: its ground is not black, its text not white, and its
   accent is calm *)
prval ground: $H.PEAK(0x1e1e1e, 18, 255) = $H.PEAKc($H.MXMN_rgb($H.RGBc{0x1e,0x1e,0x1e}()))
prval dtext: $H.PEAK(0xe2e2e2, 0, 232) = $H.PEAKc($H.MXMN_rgb($H.RGBc{0xe2,0xe2,0xe2}()))
prval daccent: $H.CALM(0x7fc49b) = $H.CALMc($H.MXMN_gbr($H.RGBc{0x7f,0xc4,0x9b}()))

(* a red past 0 degrees: #e0103a is at about 350 *)
prval rose: $H.HUE(0xe0103a, ~15, 15) = $H.HUE_r_b_g_down{0xe0103a,0xe0,0x10,0x3a,~15,15}($H.RGBc{0xe0,0x10,0x3a}())

implement main0 () = ()
