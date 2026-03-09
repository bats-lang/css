(* css -- typed CSS generation library *)
(* No $UNSAFE. Structured datatypes + emitter. *)
(* Size-indexed selectors and rules: emit has zero runtime bounds checks. *)

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
  | RGBA of (int, int, int, int)
  | {n:pos | n < 256} Named of ($A.text(n), int(n))

(* ============================================================
   Separator
   ============================================================ *)

#pub datatype css_separator =
  | Space | Comma | Slash

(* ============================================================
   Values
   ============================================================ *)

#pub datatype css_value =
  | {n:pos | n < 256} Keyword of ($A.text(n), int(n))
  | Number_scaled of (int, int, css_unit)
  | Number_bare of (int)
  | Color of (css_color)
  | {ns:pos | ns < 256} Str of (string ns)
  | {n:pos | n < 256} Var_ref of ($A.text(n), int(n))

(* ============================================================
   Selectors -- size-indexed by max emitted bytes
   ============================================================ *)

#pub datatype css_selector(int) =
  | {n:pos | n < 256} Class(n + 1) of ($A.text(n), int(n))
  | {n:pos | n < 256} Id(n + 1) of ($A.text(n), int(n))
  | {n:pos | n < 256} Tag(n) of ($A.text(n), int(n))
  | {n:pos | n < 256}{ssz:nat} Pseudo(ssz + n + 1) of (css_selector(ssz), $A.text(n), int(n))
  | {ssz1:nat}{ssz2:nat} Child(ssz1 + ssz2 + 3) of (css_selector(ssz1), css_selector(ssz2))
  | {ssz1:nat}{ssz2:nat} Descendant(ssz1 + ssz2 + 1) of (css_selector(ssz1), css_selector(ssz2))

(* ============================================================
   Declarations and Rules -- size-indexed
   ============================================================ *)

#pub datatype css_declaration =
  | {n:pos | n < 256} Decl of ($A.text(n), int(n), css_value)

#pub datatype css_rule_list(int) =
  | RuleNil(0) of ()
  | {rsz:nat}{rlsz:nat} RuleCons(rsz + rlsz) of (css_rule(rsz), css_rule_list(rlsz))

and css_rule(int) =
  | {ssz:nat} Rule(ssz + 705) of (css_selector(ssz), css_declaration)
  | {nq:pos | nq < 256}{rlsz:nat} MediaQuery(nq + rlsz + 12) of (string nq, css_rule_list(rlsz))

(* ============================================================
   Emit helpers
   ============================================================ *)

fn put_text {n:pos | n < 256}{p:nat | p + n <= $B.BUILDER_CAP}
  (b: !$B.builder(p) >> $B.builder(p + n), t: $A.text(n), len: int n): void = let
  fun loop {i:nat | i <= n}{q:nat | q + n - i <= $B.BUILDER_CAP} .<n - i>.
    (b: !$B.builder(q) >> $B.builder(q + n - i),
     t: $A.text(n), len: int n, i: int i): void =
    if i >= len then ()
    else let
      val c = $A.text_get(t, i)
      val () = $B.put_byte(b, byte2int0(c))
    in loop(b, t, len, i + 1) end
in loop(b, t, len, 0) end

(* ============================================================
   Emit: unit -- max 5 bytes
   ============================================================ *)

#pub fn emit_unit {n:nat | n + 5 <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 5] $B.builder(m), u: css_unit): void

implement emit_unit(b, u) =
  case+ u of
  | PX() => $B.bput(b, "px") | PT() => $B.bput(b, "pt") | PC() => $B.bput(b, "pc")
  | CM() => $B.bput(b, "cm") | MM() => $B.bput(b, "mm") | IN_unit() => $B.bput(b, "in")
  | EM() => $B.bput(b, "em") | REM() => $B.bput(b, "rem")
  | VW() => $B.bput(b, "vw") | VH() => $B.bput(b, "vh")
  | VMIN() => $B.bput(b, "vmin") | VMAX() => $B.bput(b, "vmax") | PERCENT() => $B.bput(b, "%")
  | DEG() => $B.bput(b, "deg") | RAD() => $B.bput(b, "rad") | TURN() => $B.bput(b, "turn")
  | S_unit() => $B.bput(b, "s") | MS() => $B.bput(b, "ms")
  | DPI() => $B.bput(b, "dpi") | DPCM() => $B.bput(b, "dpcm")

(* ============================================================
   Emit: color -- max 300 bytes
   ============================================================ *)

#pub fn emit_color {n:nat | n + 300 <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 300] $B.builder(m), c: css_color): void

implement emit_color(b, c) =
  case+ c of
  | RGB(r, g, bb) => let
      val () = $B.bput(b, "rgb(") val () = $B.put_int(b, r) val () = $B.bput(b, ", ")
      val () = $B.put_int(b, g) val () = $B.bput(b, ", ") val () = $B.put_int(b, bb)
    in $B.bput(b, ")") end
  | RGBA(r, g, bb, a) => let
      val () = $B.bput(b, "rgba(") val () = $B.put_int(b, r) val () = $B.bput(b, ", ")
      val () = $B.put_int(b, g) val () = $B.bput(b, ", ") val () = $B.put_int(b, bb)
      val () = $B.bput(b, ", ") val () = $B.put_int(b, a)
    in $B.bput(b, ")") end
  | Named(name, len) => put_text(b, name, len)

(* ============================================================
   Emit: value -- max 400 bytes
   ============================================================ *)

#pub fn emit_value {n:nat | n + 400 <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 400] $B.builder(m), v: css_value): void

implement emit_value(b, v) =
  case+ v of
  | Keyword(kw, len) => put_text(b, kw, len)
  | Number_scaled(num, dp, u) => let
      val () = $B.put_int(b, num)
    in (if num = 0 then () else emit_unit(b, u)) end
  | Number_bare(num) => $B.put_int(b, num)
  | Color(c) => emit_color(b, c)
  | Str(s) => let
      val () = $B.put_char(b, 34) val () = $B.bput(b, s)
    in $B.put_char(b, 34) end
  | Var_ref(name, len) => let
      val () = $B.bput(b, "var(--") val () = put_text(b, name, len)
    in $B.bput(b, ")") end

(* ============================================================
   Emit: selector -- compile-time bounds from size index
   ============================================================ *)

#pub fun emit_selector {ssz:nat}{n:nat | n + ssz <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + ssz] $B.builder(m),
   s: css_selector(ssz)): void

implement emit_selector(b, s) =
  case+ s of
  | Class(name, len) => let val () = $B.put_char(b, 46) in put_text(b, name, len) end
  | Id(name, len) => let val () = $B.put_char(b, 35) in put_text(b, name, len) end
  | Tag(name, len) => put_text(b, name, len)
  | Pseudo(base, pseudo, len) => let
      val () = emit_selector(b, base)
      val () = $B.put_char(b, 58)
    in put_text(b, pseudo, len) end
  | Child(parent, child) => let
      val () = emit_selector(b, parent)
      val () = $B.bput(b, " > ")
    in emit_selector(b, child) end
  | Descendant(parent, child) => let
      val () = emit_selector(b, parent)
      val () = $B.put_char(b, 32)
    in emit_selector(b, child) end

(* ============================================================
   Emit: declaration -- max 700 bytes (prop < 256 + value < 400 + formatting)
   ============================================================ *)

#pub fn emit_declaration {n:nat | n + 700 <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 700] $B.builder(m), d: css_declaration): void

implement emit_declaration(b, d) =
  case+ d of
  | Decl(prop, len, val_) => let
      val () = $B.bput(b, "  ") val () = put_text(b, prop, len)
      val () = $B.bput(b, ": ") val () = emit_value(b, val_)
    in $B.bput(b, ";\n") end

(* ============================================================
   Emit: rule -- compile-time bounds from size index
   ============================================================ *)

#pub fun emit_rule_list {rlsz:nat}{n:nat | n + rlsz <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + rlsz] $B.builder(m),
   lst: css_rule_list(rlsz)): void

#pub fn emit_rule {rsz:nat}{n:nat | n + rsz <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + rsz] $B.builder(m),
   r: css_rule(rsz)): void

implement emit_rule(b, r) =
  case+ r of
  | Rule(sel, decl) => let
      val () = emit_selector(b, sel) val () = $B.bput(b, " {\n")
      val () = emit_declaration(b, decl)
    in $B.bput(b, "}\n") end
  | MediaQuery(query, rules) => let
      val () = $B.bput(b, "@media ")
      val () = $B.bput(b, query)
      val () = $B.bput(b, " {\n")
      val () = emit_rule_list(b, rules)
    in $B.bput(b, "}\n") end

implement emit_rule_list(b, lst) =
  case+ lst of
  | RuleNil() => ()
  | RuleCons(r, rest) => let
      val () = emit_rule(b, r)
    in emit_rule_list(b, rest) end

(* ============================================================
   class_text -- generate a class name from an integer index
   Maps 0 -> caa, 1 -> cab, ..., 25 -> caz, 26 -> cba, ..., 675 -> czz
   ============================================================ *)

#pub fn class_text(idx: int): @($A.text(3), int(3))

implement class_text(idx) = let
  val i1 = $AR.mod_int_int($AR.div_int_int(idx, 26), 26)
  val i2 = $AR.mod_int_int(idx, 26)
  val c1 = $AR.add_int_int(97, i1)
  val c2 = $AR.add_int_int(97, i2)
  var chars = @[char][3](int2char0(99), int2char0(c1), int2char0(c2))
in @($S.text_of_chars(chars, 3), 3) end
