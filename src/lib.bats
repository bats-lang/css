(* css -- typed CSS generation library *)
(* No $UNSAFE. Structured datatypes + emitter. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as B
#use str as S

(* ============================================================
   Units -- exhaustive enumeration
   ============================================================ *)

#pub datatype css_unit =
  (* Length - absolute *)
  | PX | PT | PC | CM | MM | IN_unit
  (* Length - relative *)
  | EM | REM | VW | VH | VMIN | VMAX | PERCENT
  (* Angle *)
  | DEG | RAD | TURN
  (* Time *)
  | S_unit | MS
  (* Resolution *)
  | DPI | DPCM

(* ============================================================
   Colors
   ============================================================ *)

#pub datatype css_color =
  | RGB of (int, int, int)
  | RGBA of (int, int, int, int)  (* alpha 0-255 *)
  | Named of (string)

(* ============================================================
   Separator
   ============================================================ *)

#pub datatype css_separator =
  | Space | Comma | Slash

(* ============================================================
   Values
   ============================================================ *)

#pub datatype css_value =
  | Keyword of (string)
  | Number_int of (int, css_unit)
  | Number_bare of (int)
  | Color of (css_color)
  | Str of (string)
  | Var_ref of (string)

(* ============================================================
   Selectors
   ============================================================ *)

#pub datatype css_selector =
  | Class of (string)
  | Id of (string)
  | Tag of (string)
  | Pseudo of (css_selector, string)
  | Child of (css_selector, css_selector)

(* ============================================================
   Declarations and Rules
   ============================================================ *)

#pub datatype css_declaration =
  | Decl of (string, css_value)

#pub datatype css_rule =
  | Rule of (css_selector, css_declaration)

(* ============================================================
   Emit helpers
   ============================================================ *)

fn bput {sn:nat} (b: !$B.builder, s: string sn): void = let
  fun loop {sn:nat}{fuel:nat} .<fuel>.
    (b: !$B.builder, s: string sn, slen: int sn, i: int, fuel: int fuel): void =
    if fuel <= 0 then ()
    else let val ii = g1ofg0(i) in
      if ii >= 0 then
        if $AR.lt1_int_int(ii, slen) then let
          val c = char2int0(string_get_at(s, ii))
          val () = $B.put_byte(b, c)
        in loop(b, s, slen, i + 1, fuel - 1) end
        else ()
      else ()
    end
  val slen = g1u2i(string1_length(s))
in loop(b, s, slen, 0, $AR.checked_nat(g0ofg1(slen) + 1)) end

fn put_int(b: !$B.builder, v: int): void = let
  val is_neg = v < 0
  val abs_v = (if is_neg then 0 - v else v): int
  fun count {k:nat} .<k>. (v: int, r: int(k)): int =
    if r <= 0 then 1
    else if v < 10 then 1
    else 1 + count(v / 10, r - 1)
  val nd = count(abs_v, $AR.checked_nat(abs_v + 1))
  fun write {fuel:nat} .<fuel>.
    (b: !$B.builder, v: int, pos: int, fuel: int fuel): void =
    if fuel <= 0 then ()
    else if v < 10 then $B.put_byte(b, v + 48)
    else let
      val () = write(b, v / 10, pos - 1, fuel - 1)
    in $B.put_byte(b, (v mod 10) + 48) end
in
  if is_neg then $B.put_byte(b, 45);
  write(b, abs_v, nd - 1, $AR.checked_nat(nd + 1))
end

(* ============================================================
   Emit: unit
   ============================================================ *)

#pub fn emit_unit(b: !$B.builder, u: css_unit): void =
  case+ u of
  | PX() => bput(b, "px")
  | PT() => bput(b, "pt")
  | PC() => bput(b, "pc")
  | CM() => bput(b, "cm")
  | MM() => bput(b, "mm")
  | IN_unit() => bput(b, "in")
  | EM() => bput(b, "em")
  | REM() => bput(b, "rem")
  | VW() => bput(b, "vw")
  | VH() => bput(b, "vh")
  | VMIN() => bput(b, "vmin")
  | VMAX() => bput(b, "vmax")
  | PERCENT() => bput(b, "%")
  | DEG() => bput(b, "deg")
  | RAD() => bput(b, "rad")
  | TURN() => bput(b, "turn")
  | S_unit() => bput(b, "s")
  | MS() => bput(b, "ms")
  | DPI() => bput(b, "dpi")
  | DPCM() => bput(b, "dpcm")

(* ============================================================
   Emit: color
   ============================================================ *)

#pub fn emit_color(b: !$B.builder, c: css_color): void =
  case+ c of
  | RGB(r, g, bb) => let
      val () = bput(b, "rgb(")
      val () = put_int(b, r)
      val () = bput(b, ", ")
      val () = put_int(b, g)
      val () = bput(b, ", ")
      val () = put_int(b, bb)
    in bput(b, ")") end
  | RGBA(r, g, bb, a) => let
      val () = bput(b, "rgba(")
      val () = put_int(b, r)
      val () = bput(b, ", ")
      val () = put_int(b, g)
      val () = bput(b, ", ")
      val () = put_int(b, bb)
      val () = bput(b, ", ")
      val () = put_int(b, a)
    in bput(b, ")") end
  | Named(name) => bput(b, name)

(* ============================================================
   Emit: value
   ============================================================ *)

#pub fn emit_value(b: !$B.builder, v: css_value): void =
  case+ v of
  | Keyword(kw) => bput(b, kw)
  | Number_int(n, u) => let
      val () = put_int(b, n)
      val () = (if n = 0 then () else emit_unit(b, u))
    in end
  | Number_bare(n) => put_int(b, n)
  | Color(c) => emit_color(b, c)
  | Str(s) => let
      val () = $B.put_byte(b, 34) (* " *)
      val () = bput(b, s)
    in $B.put_byte(b, 34) end
  | Var_ref(name) => let
      val () = bput(b, "var(--")
      val () = bput(b, name)
    in bput(b, ")") end

(* ============================================================
   Emit: selector
   ============================================================ *)

#pub fun emit_selector(b: !$B.builder, s: css_selector): void =
  case+ s of
  | Class(name) => let
      val () = $B.put_byte(b, 46) (* . *)
    in bput(b, name) end
  | Id(name) => let
      val () = $B.put_byte(b, 35) (* # *)
    in bput(b, name) end
  | Tag(name) => bput(b, name)
  | Pseudo(base, pseudo) => let
      val () = emit_selector(b, base)
      val () = $B.put_byte(b, 58) (* : *)
    in bput(b, pseudo) end
  | Child(parent, child) => let
      val () = emit_selector(b, parent)
      val () = bput(b, " > ")
    in emit_selector(b, child) end

(* ============================================================
   Emit: declaration
   ============================================================ *)

#pub fn emit_declaration(b: !$B.builder, d: css_declaration): void =
  case+ d of
  | Decl(prop, val_) => let
      val () = bput(b, "  ")
      val () = bput(b, prop)
      val () = bput(b, ": ")
      val () = emit_value(b, val_)
    in bput(b, ";\n") end

(* ============================================================
   Emit: rule
   ============================================================ *)

#pub fn emit_rule(b: !$B.builder, r: css_rule): void =
  case+ r of
  | Rule(sel, decl) => let
      val () = emit_selector(b, sel)
      val () = bput(b, " {\n")
      val () = emit_declaration(b, decl)
    in bput(b, "}\n") end
