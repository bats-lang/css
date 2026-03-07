(* css -- typed CSS generation library *)
(* No $UNSAFE. Structured datatypes + emitter. *)
(* Uses text from array for safe string handling. *)

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
   Selectors
   ============================================================ *)

#pub datatype css_selector =
  | {n:pos | n < 256} Class of ($A.text(n), int(n))
  | {n:pos | n < 256} Id of ($A.text(n), int(n))
  | {n:pos | n < 256} Tag of ($A.text(n), int(n))
  | {n:pos | n < 256} Pseudo of (css_selector, $A.text(n), int(n))
  | Child of (css_selector, css_selector)
  | Descendant of (css_selector, css_selector)

(* ============================================================
   Declarations and Rules
   ============================================================ *)

#pub datatype css_declaration =
  | {n:pos | n < 256} Decl of ($A.text(n), int(n), css_value)

#pub datatype css_rule_list =
  | RuleNil of ()
  | RuleCons of (css_rule, css_rule_list)

and css_rule =
  | Rule of (css_selector, css_declaration)
  | {nq:pos | nq < 256} MediaQuery of (string nq, css_rule_list)

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
   Emit: selector -- uses room parameter for recursion
   Room must be >= 260 (max single-level selector)
   ============================================================ *)

#pub fun emit_selector {n:nat}{room:nat | n + room <= $B.BUILDER_CAP; room >= 260}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + room] $B.builder(m),
   s: css_selector, room: int room): void

implement emit_selector(b, s, room) =
  case+ s of
  | Class(name, len) => let val () = $B.put_char(b, 46) in put_text(b, name, len) end
  | Id(name, len) => let val () = $B.put_char(b, 35) in put_text(b, name, len) end
  | Tag(name, len) => put_text(b, name, len)
  | Pseudo(base, pseudo, len) =>
    if room >= 520 then let
      val () = emit_selector(b, base, room - 260)
      val () = $B.put_char(b, 58)
    in put_text(b, pseudo, len) end
    else let val () = $B.put_char(b, 58) in put_text(b, pseudo, len) end
  | Child(parent, child) =>
    if room >= 526 then let
      val () = emit_selector(b, parent, room / 2)
      val () = $B.bput(b, " > ")
    in emit_selector(b, child, room / 2 - 3) end
    else ()
  | Descendant(parent, child) =>
    if room >= 522 then let
      val () = emit_selector(b, parent, room / 2)
      val () = $B.put_char(b, 32)
    in emit_selector(b, child, room / 2 - 1) end
    else ()

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
   Emit: rule -- uses room parameter for recursion
   ============================================================ *)

#pub fun emit_rule_list {n:nat}{room:nat | n + room <= $B.BUILDER_CAP; room >= 2000}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + room] $B.builder(m),
   lst: css_rule_list, room: int room): void

#pub fn emit_rule {n:nat}{room:nat | n + room <= $B.BUILDER_CAP; room >= 2000}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + room] $B.builder(m),
   r: css_rule, room: int room): void

implement emit_rule(b, r, room) =
  case+ r of
  | Rule(sel, decl) => let
      val () = emit_selector(b, sel, room / 2) val () = $B.bput(b, " {\n")
      val () = emit_declaration(b, decl)
    in $B.bput(b, "}\n") end
  | MediaQuery(query, rules) =>
    if room >= 4270 then let
      val () = $B.bput(b, "@media ")
      val () = $B.bput(b, query)
      val () = $B.bput(b, " {\n")
      val () = emit_rule_list(b, rules, room - 270)
    in $B.bput(b, "}\n") end
    else ()

implement emit_rule_list(b, lst, room) =
  case+ lst of
  | RuleNil() => ()
  | RuleCons(r, rest) =>
    if room >= 4000 then let
      val () = emit_rule(b, r, room / 2)
    in emit_rule_list(b, rest, room / 2) end
    else ()
