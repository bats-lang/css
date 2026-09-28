(* harmony -- rules for a harmonious theme, as proofs over sRGB channels

   A theme's colours are proven to follow these rules, as CONTRAST
   (contrast.bats) proves they can be read. Each is stated on a colour's
   channels (RGB), so the constraint solver checks it for the colours a
   theme names; a theme that breaks one does not type-check.

   What the rules come from:

   * Colours harmonize when they share a hue, have similar saturation
     and contrast in lightness (the trends lab studies agree on, as
     O'Donovan, Agarwala and Hertzmann summarise them in "Color
     Compatibility From Large Datasets", SIGGRAPH 2011). The same study
     found no evidence that hue templates (Matsuda's sectors) predict
     which colours go together, but that themes of 2 to 3 hues are
     preferred to one hue or more. So hues come in families (IN3: at
     most three arcs, none wider than 30 degrees, FAMILIES), rather than
     in template positions.
   * The large surfaces are neutrals, grey or tinted with one hue, so
     they do not clash with each other or with the accents (NEUTRAL).
   * Two saturated colours as text and ground vibrate (chromostereopsis)
     whatever their contrast: one of a text/ground pair is calm (NOVIB).
   * A dark theme's ground is not pure black and its text not pure
     white, and its colours are desaturated (Material Design's dark
     theme: #121212 ground, white text at 87%, accents from the light
     200 tones): PEAK bounds a colour's brightest channel, CALM its
     saturation.
   * A raised surface is lighter than what it sits on, in light and
     dark themes alike (LIGHTER).

   Saturation is HSV's: (max - min) / max over the channels; chroma is
   max - min; hue is HSV's, in degrees. *)

#include "share/atspre_staload.hats"

staload "./contrast.bats"

(* RGB(c, r, g, b): the colour 0xRRGGBB c has channels r, g, b *)
#pub dataprop RGB(int, int, int, int) =
  | {r,g,b:nat | r < 256; g < 256; b < 256} RGBc(r * 65536 + g * 256 + b, r, g, b)

(* MXMN(c, mx, mn): c's brightest and darkest channels *)
#pub dataprop MXMN(int, int, int) =
  | {c,r,g,b:int | r >= g; g >= b} MXMN_rgb(c, r, b) of RGB(c, r, g, b)
  | {c,r,g,b:int | r >= b; b >= g} MXMN_rbg(c, r, g) of RGB(c, r, g, b)
  | {c,r,g,b:int | g >= r; r >= b} MXMN_grb(c, g, b) of RGB(c, r, g, b)
  | {c,r,g,b:int | g >= b; b >= r} MXMN_gbr(c, g, r) of RGB(c, r, g, b)
  | {c,r,g,b:int | b >= r; r >= g} MXMN_brg(c, b, g) of RGB(c, r, g, b)
  | {c,r,g,b:int | b >= g; g >= r} MXMN_bgr(c, b, r) of RGB(c, r, g, b)

(* CHROMA(c, k): c's chroma is at most k (of 255) *)
#pub dataprop CHROMA(int, int) =
  | {c,mx,mn,k:int | mx - mn <= k} CHROMAc(c, k) of MXMN(c, mx, mn)

(* CALM(c): c's saturation is at most 1/2 *)
#pub dataprop CALM(int) =
  | {c,mx,mn:int | 2 * (mx - mn) <= mx} CALMc(c) of MXMN(c, mx, mn)

(* SATNEAR(a, b, d): a's and b's saturations are within d / 100 *)
#pub dataprop SATNEAR(int, int, int) =
  | {a,b,d,amx,amn,bmx,bmn:int | amx > 0; bmx > 0;
      100 * ((amx - amn) * bmx - (bmx - bmn) * amx) <= d * amx * bmx;
      100 * ((bmx - bmn) * amx - (amx - amn) * bmx) <= d * amx * bmx}
    SATNEARc(a, b, d) of (MXMN(a, amx, amn), MXMN(b, bmx, bmn))

(* PEAK(c, lo, hi): c's brightest channel is in [lo, hi] *)
#pub dataprop PEAK(int, int, int) =
  | {c,mx,mn,lo,hi:int | lo <= mx; mx <= hi} PEAKc(c, lo, hi) of MXMN(c, mx, mn)

(* LIGHTER(a, b): a's luminance is more than b's *)
#pub dataprop LIGHTER(int, int) =
  | {a,b,alo,ahi,blo,bhi:int | alo > bhi} LIGHTERc(a, b) of (LUM(a, alo, ahi), LUM(b, blo, bhi))

(* HUE(c, lo, hi): c has a hue, and it is in [lo, hi] degrees. An arc
   may reach past 0 or 360 (a red is in [~15, 15]): the _down and _up
   forms take the hue as h - 360 and h + 360 *)
#pub dataprop HUE(int, int, int) =
  | {c,r,g,b,lo,hi:int | r >= g; g >= b; r > b;
      lo * (r - b) <= 60 * (g - b); 60 * (g - b) <= hi * (r - b)}
    HUE_r_g_b(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | r >= g; g >= b; r > b;
      lo * (r - b) <= 60 * (g - b) - 360 * (r - b); 60 * (g - b) - 360 * (r - b) <= hi * (r - b)}
    HUE_r_g_b_down(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | r >= g; g >= b; r > b;
      lo * (r - b) <= 60 * (g - b) + 360 * (r - b); 60 * (g - b) + 360 * (r - b) <= hi * (r - b)}
    HUE_r_g_b_up(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | g >= r; r >= b; g > b;
      lo * (g - b) <= 120 * (g - b) - 60 * (r - b); 120 * (g - b) - 60 * (r - b) <= hi * (g - b)}
    HUE_g_r_b(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | g >= r; r >= b; g > b;
      lo * (g - b) <= 120 * (g - b) - 60 * (r - b) - 360 * (g - b); 120 * (g - b) - 60 * (r - b) - 360 * (g - b) <= hi * (g - b)}
    HUE_g_r_b_down(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | g >= r; r >= b; g > b;
      lo * (g - b) <= 120 * (g - b) - 60 * (r - b) + 360 * (g - b); 120 * (g - b) - 60 * (r - b) + 360 * (g - b) <= hi * (g - b)}
    HUE_g_r_b_up(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | g >= b; b >= r; g > r;
      lo * (g - r) <= 120 * (g - r) + 60 * (b - r); 120 * (g - r) + 60 * (b - r) <= hi * (g - r)}
    HUE_g_b_r(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | g >= b; b >= r; g > r;
      lo * (g - r) <= 120 * (g - r) + 60 * (b - r) - 360 * (g - r); 120 * (g - r) + 60 * (b - r) - 360 * (g - r) <= hi * (g - r)}
    HUE_g_b_r_down(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | g >= b; b >= r; g > r;
      lo * (g - r) <= 120 * (g - r) + 60 * (b - r) + 360 * (g - r); 120 * (g - r) + 60 * (b - r) + 360 * (g - r) <= hi * (g - r)}
    HUE_g_b_r_up(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | b >= g; g >= r; b > r;
      lo * (b - r) <= 240 * (b - r) - 60 * (g - r); 240 * (b - r) - 60 * (g - r) <= hi * (b - r)}
    HUE_b_g_r(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | b >= g; g >= r; b > r;
      lo * (b - r) <= 240 * (b - r) - 60 * (g - r) - 360 * (b - r); 240 * (b - r) - 60 * (g - r) - 360 * (b - r) <= hi * (b - r)}
    HUE_b_g_r_down(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | b >= g; g >= r; b > r;
      lo * (b - r) <= 240 * (b - r) - 60 * (g - r) + 360 * (b - r); 240 * (b - r) - 60 * (g - r) + 360 * (b - r) <= hi * (b - r)}
    HUE_b_g_r_up(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | b >= r; r >= g; b > g;
      lo * (b - g) <= 240 * (b - g) + 60 * (r - g); 240 * (b - g) + 60 * (r - g) <= hi * (b - g)}
    HUE_b_r_g(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | b >= r; r >= g; b > g;
      lo * (b - g) <= 240 * (b - g) + 60 * (r - g) - 360 * (b - g); 240 * (b - g) + 60 * (r - g) - 360 * (b - g) <= hi * (b - g)}
    HUE_b_r_g_down(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | b >= r; r >= g; b > g;
      lo * (b - g) <= 240 * (b - g) + 60 * (r - g) + 360 * (b - g); 240 * (b - g) + 60 * (r - g) + 360 * (b - g) <= hi * (b - g)}
    HUE_b_r_g_up(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | r >= b; b >= g; r > g;
      lo * (r - g) <= 360 * (r - g) - 60 * (b - g); 360 * (r - g) - 60 * (b - g) <= hi * (r - g)}
    HUE_r_b_g(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | r >= b; b >= g; r > g;
      lo * (r - g) <= 360 * (r - g) - 60 * (b - g) - 360 * (r - g); 360 * (r - g) - 60 * (b - g) - 360 * (r - g) <= hi * (r - g)}
    HUE_r_b_g_down(c, lo, hi) of RGB(c, r, g, b)
  | {c,r,g,b,lo,hi:int | r >= b; b >= g; r > g;
      lo * (r - g) <= 360 * (r - g) - 60 * (b - g) + 360 * (r - g); 360 * (r - g) - 60 * (b - g) + 360 * (r - g) <= hi * (r - g)}
    HUE_r_b_g_up(c, lo, hi) of RGB(c, r, g, b)

(* FAMILIES(l1, h1, l2, h2, l3, h3): three hue arcs (which may be the
   same), none wider than 30 degrees *)
#pub dataprop FAMILIES(int, int, int, int, int, int) =
  | {l1,h1,l2,h2,l3,h3:int | l1 <= h1; h1 - l1 <= 30; l2 <= h2; h2 - l2 <= 30; l3 <= h3; h3 - l3 <= 30}
    FAMILIESc(l1, h1, l2, h2, l3, h3)

(* IN3(c, l1, h1, l2, h2, l3, h3): c is grey (chroma at most 4), or its
   hue is in one of the three families *)
#pub dataprop IN3(int, int, int, int, int, int, int) =
  | {c,l1,h1,l2,h2,l3,h3:int} IN3_grey(c, l1, h1, l2, h2, l3, h3) of CHROMA(c, 4)
  | {c,l1,h1,l2,h2,l3,h3:int} IN3_1(c, l1, h1, l2, h2, l3, h3) of (FAMILIES(l1, h1, l2, h2, l3, h3), HUE(c, l1, h1))
  | {c,l1,h1,l2,h2,l3,h3:int} IN3_2(c, l1, h1, l2, h2, l3, h3) of (FAMILIES(l1, h1, l2, h2, l3, h3), HUE(c, l2, h2))
  | {c,l1,h1,l2,h2,l3,h3:int} IN3_3(c, l1, h1, l2, h2, l3, h3) of (FAMILIES(l1, h1, l2, h2, l3, h3), HUE(c, l3, h3))

(* NEUTRAL(c, k, lo, hi): c is a neutral: grey, or tinted with a hue in
   [lo, hi] and chroma at most k *)
#pub dataprop NEUTRAL(int, int, int, int) =
  | {c,k,lo,hi:int} NEUTRAL_grey(c, k, lo, hi) of CHROMA(c, 4)
  | {c,k,lo,hi:int} NEUTRAL_tint(c, k, lo, hi) of (CHROMA(c, k), HUE(c, lo, hi))

(* NOVIB(a, b): text a on ground b does not vibrate: one of them is
   calm *)
#pub dataprop NOVIB(int, int) =
  | {a,b:int} NOVIB_text(a, b) of CALM(a)
  | {a,b:int} NOVIB_ground(a, b) of CALM(b)
