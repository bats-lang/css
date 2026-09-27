(* css -- typed CSS generation library *)
(* No $UNSAFE. Structured linear datatypes + emitter. *)
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
   There is no GC: every value below that holds data is a linear
   cell, freed by its *_free (or consumed by a constructor that holds
   it). The emitters only borrow what they emit.
   ============================================================ *)

#pub datavtype css_color =
  | RGB of (int, int, int)
  | RGBA of (int, int, int, int)
  | {n:pos | n < 256} Named of ($A.text(n), int(n))

#pub fn css_color_free (c: css_color): void

implement css_color_free(c) =
  case+ c of
  | ~RGB(_, _, _) => ()
  | ~RGBA(_, _, _, _) => ()
  | ~Named(_, _) => ()

(* ============================================================
   Separator
   ============================================================ *)

#pub datatype css_separator =
  | Space | Comma | Slash

(* ============================================================
   Values
   ============================================================ *)

#pub datavtype css_value =
  | {n:pos | n < 256} Keyword of ($A.text(n), int(n))
  | Number_scaled of (int, int, css_unit)
  | Number_bare of (int)
  | Color of (css_color)
  | {ns:pos | ns < 256} Str of ($A.text(ns), int(ns))
  | {n:pos | n < 256} Var_ref of ($A.text(n), int(n))

#pub fn css_value_free (v: css_value): void

implement css_value_free(v) =
  case+ v of
  | ~Keyword(_, _) => ()
  | ~Number_scaled(_, _, _) => ()
  | ~Number_bare(_) => ()
  | ~Color(c) => css_color_free(c)
  | ~Str(_, _) => ()
  | ~Var_ref(_, _) => ()

(* ============================================================
   Selectors -- size-indexed by max emitted bytes
   ============================================================ *)

#pub datavtype css_selector(int) =
  | {n:pos | n < 256} Class(n + 1) of ($A.text(n), int(n))
  | {n:pos | n < 256} Id(n + 1) of ($A.text(n), int(n))
  | {n:pos | n < 256} Tag(n) of ($A.text(n), int(n))
  | {n:pos | n < 256}{ssz:nat} Pseudo(ssz + n + 1) of (css_selector(ssz), $A.text(n), int(n))
  | {ssz1:nat}{ssz2:nat} Child(ssz1 + ssz2 + 3) of (css_selector(ssz1), css_selector(ssz2))
  | {ssz1:nat}{ssz2:nat} Descendant(ssz1 + ssz2 + 1) of (css_selector(ssz1), css_selector(ssz2))

#pub fn css_selector_free {ssz:nat} (s: css_selector(ssz)): void

implement css_selector_free(s) = let
  (* A selector's parts are smaller than it *)
  fun free {ssz:nat} .<ssz>. (s: css_selector(ssz)): void =
    case+ s of
    | ~Class(_, _) => ()
    | ~Id(_, _) => ()
    | ~Tag(_, _) => ()
    | ~Pseudo(base, _, _) => free(base)
    | ~Child(parent, child) => let val () = free(parent) in free(child) end
    | ~Descendant(parent, child) => let val () = free(parent) in free(child) end
in free(s) end

(* ============================================================
   Declarations and Rules -- size-indexed
   ============================================================ *)

#pub datavtype css_declaration =
  | {n:pos | n < 256} Decl of ($A.text(n), int(n), css_value)

#pub fn css_declaration_free (d: css_declaration): void

implement css_declaration_free(d) =
  case+ d of
  | ~Decl(_, _, v) => css_value_free(v)

#pub datavtype css_rule_list(int) =
  | RuleNil(0) of ()
  | {rsz:pos}{rlsz:nat} RuleCons(rsz + rlsz) of (css_rule(rsz), css_rule_list(rlsz))

and css_rule(int) =
  | {ssz:nat} Rule(ssz + 705) of (css_selector(ssz), css_declaration)
  | {nq:pos | nq < 256}{rlsz:nat} MediaQuery(nq + rlsz + 12) of ($A.text(nq), int(nq), css_rule_list(rlsz))

#pub fn css_rule_list_free {rlsz:nat} (lst: css_rule_list(rlsz)): void

#pub fn css_rule_free {rsz:nat} (r: css_rule(rsz)): void

(* As in _emit_rule: a rule's list is smaller than the rule, a list's
   first rule no larger than the list *)
fun _free_rule {rsz:nat} .<rsz, 0>. (r: css_rule(rsz)): void =
  case+ r of
  | ~Rule(sel, decl) => let
      val () = css_selector_free(sel)
    in css_declaration_free(decl) end
  | ~MediaQuery(_, _, rules) => _free_rule_list(rules)

and _free_rule_list {rlsz:nat} .<rlsz, 1>. (lst: css_rule_list(rlsz)): void =
  case+ lst of
  | ~RuleNil() => ()
  | ~RuleCons(r, rest) => let
      val () = _free_rule(r)
    in _free_rule_list(rest) end

implement css_rule_free(r) = _free_rule(r)

implement css_rule_list_free(lst) = _free_rule_list(lst)

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
      val () = $B.put_byte(b, $AR.low_byte(byte2int0(c)))
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
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 300] $B.builder(m), c: !css_color): void

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
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 400] $B.builder(m), v: !css_value): void

implement emit_value(b, v) =
  case+ v of
  | Keyword(kw, len) => put_text(b, kw, len)
  | Number_scaled(num, dp, u) => let
      val () = $B.put_int(b, num)
    in (if num = 0 then () else emit_unit(b, u)) end
  | Number_bare(num) => $B.put_int(b, num)
  | @Color(c) => let
      val () = emit_color(b, c)
      prval () = fold@(v)
    in () end
  | Str(t, tlen) => let
      val () = $B.put_char(b, 34) val () = put_text(b, t, tlen)
    in $B.put_char(b, 34) end
  | Var_ref(name, len) => let
      val () = $B.bput(b, "var(--") val () = put_text(b, name, len)
    in $B.bput(b, ")") end

(* ============================================================
   Emit: selector -- compile-time bounds from size index
   ============================================================ *)

#pub fun emit_selector {ssz:nat}{n:nat | n + ssz <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + ssz] $B.builder(m),
   s: !css_selector(ssz)): void

implement emit_selector(b, s) = let
  (* A selector's parts are smaller than it *)
  fun emit {ssz:nat}{n:nat | n + ssz <= $B.BUILDER_CAP} .<ssz>.
    (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + ssz] $B.builder(m),
     s: !css_selector(ssz)): void =
  case+ s of
  | Class(name, len) => let val () = $B.put_char(b, 46) in put_text(b, name, len) end
  | Id(name, len) => let val () = $B.put_char(b, 35) in put_text(b, name, len) end
  | Tag(name, len) => put_text(b, name, len)
  | @Pseudo(base, pseudo, len) => let
      val () = emit(b, base)
      val () = $B.put_char(b, 58)
      val () = put_text(b, pseudo, len)
      prval () = fold@(s)
    in () end
  | @Child(parent, child) => let
      val () = emit(b, parent)
      val () = $B.bput(b, " > ")
      val () = emit(b, child)
      prval () = fold@(s)
    in () end
  | @Descendant(parent, child) => let
      val () = emit(b, parent)
      val () = $B.put_char(b, 32)
      val () = emit(b, child)
      prval () = fold@(s)
    in () end
in emit(b, s) end

(* ============================================================
   Emit: declaration -- max 700 bytes (prop < 256 + value < 400 + formatting)
   ============================================================ *)

#pub fn emit_declaration {n:nat | n + 700 <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + 700] $B.builder(m), d: !css_declaration): void

implement emit_declaration(b, d) =
  case+ d of
  | @Decl(prop, len, val_) => let
      val () = $B.bput(b, "  ") val () = put_text(b, prop, len)
      val () = $B.bput(b, ": ") val () = emit_value(b, val_)
      val () = $B.bput(b, ";\n")
      prval () = fold@(d)
    in () end

(* ============================================================
   Emit: rule -- compile-time bounds from size index
   ============================================================ *)

#pub fun emit_rule_list {rlsz:nat}{n:nat | n + rlsz <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + rlsz] $B.builder(m),
   lst: !css_rule_list(rlsz)): void

#pub fn emit_rule {rsz:nat}{n:nat | n + rsz <= $B.BUILDER_CAP}
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + rsz] $B.builder(m),
   r: !css_rule(rsz)): void

(* A rule's list is smaller than the rule, and a list's first rule is
   no larger than the list, whose rules are never empty *)
fun _emit_rule {rsz:nat}{n:nat | n + rsz <= $B.BUILDER_CAP} .<rsz, 0>.
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + rsz] $B.builder(m),
   r: !css_rule(rsz)): void =
  case+ r of
  | @Rule(sel, decl) => let
      val () = emit_selector(b, sel) val () = $B.bput(b, " {\n")
      val () = emit_declaration(b, decl)
      val () = $B.bput(b, "}\n")
      prval () = fold@(r)
    in () end
  | @MediaQuery(query, qlen, rules) => let
      val () = $B.bput(b, "@media ")
      val () = put_text(b, query, qlen)
      val () = $B.bput(b, " {\n")
      val () = _emit_rule_list(b, rules)
      val () = $B.bput(b, "}\n")
      prval () = fold@(r)
    in () end

and _emit_rule_list {rlsz:nat}{n:nat | n + rlsz <= $B.BUILDER_CAP} .<rlsz, 1>.
  (b: !$B.builder(n) >> [m:nat | n <= m; m <= n + rlsz] $B.builder(m),
   lst: !css_rule_list(rlsz)): void =
  case+ lst of
  | RuleNil() => ()
  | @RuleCons(r, rest) => let
      val () = _emit_rule(b, r)
      val () = _emit_rule_list(b, rest)
      prval () = fold@(lst)
    in () end

implement emit_rule(b, r) = _emit_rule(b, r)

implement emit_rule_list(b, lst) = _emit_rule_list(b, lst)

(* ============================================================
   class_text -- generate a class name from an integer index
   Maps 0 -> caa, 1 -> cab, ..., 25 -> caz, 26 -> cba, ..., 675 -> czz
   ============================================================ *)

#pub fn class_text {i:nat | i < 676} (idx: int i): @($A.text(3), int(3))

(* A literal each: text_lit allocates nothing, where text_build
   allocated a cell per call that nothing ever freed. The last case,
   675, is the only index left once the others are taken. *)
fn _class_lit {i:nat | i < 676} (idx: int i): $A.text(3) =
  case+ idx of
  | 0 => $A.text_lit("caa")
  | 1 => $A.text_lit("cab")
  | 2 => $A.text_lit("cac")
  | 3 => $A.text_lit("cad")
  | 4 => $A.text_lit("cae")
  | 5 => $A.text_lit("caf")
  | 6 => $A.text_lit("cag")
  | 7 => $A.text_lit("cah")
  | 8 => $A.text_lit("cai")
  | 9 => $A.text_lit("caj")
  | 10 => $A.text_lit("cak")
  | 11 => $A.text_lit("cal")
  | 12 => $A.text_lit("cam")
  | 13 => $A.text_lit("can")
  | 14 => $A.text_lit("cao")
  | 15 => $A.text_lit("cap")
  | 16 => $A.text_lit("caq")
  | 17 => $A.text_lit("car")
  | 18 => $A.text_lit("cas")
  | 19 => $A.text_lit("cat")
  | 20 => $A.text_lit("cau")
  | 21 => $A.text_lit("cav")
  | 22 => $A.text_lit("caw")
  | 23 => $A.text_lit("cax")
  | 24 => $A.text_lit("cay")
  | 25 => $A.text_lit("caz")
  | 26 => $A.text_lit("cba")
  | 27 => $A.text_lit("cbb")
  | 28 => $A.text_lit("cbc")
  | 29 => $A.text_lit("cbd")
  | 30 => $A.text_lit("cbe")
  | 31 => $A.text_lit("cbf")
  | 32 => $A.text_lit("cbg")
  | 33 => $A.text_lit("cbh")
  | 34 => $A.text_lit("cbi")
  | 35 => $A.text_lit("cbj")
  | 36 => $A.text_lit("cbk")
  | 37 => $A.text_lit("cbl")
  | 38 => $A.text_lit("cbm")
  | 39 => $A.text_lit("cbn")
  | 40 => $A.text_lit("cbo")
  | 41 => $A.text_lit("cbp")
  | 42 => $A.text_lit("cbq")
  | 43 => $A.text_lit("cbr")
  | 44 => $A.text_lit("cbs")
  | 45 => $A.text_lit("cbt")
  | 46 => $A.text_lit("cbu")
  | 47 => $A.text_lit("cbv")
  | 48 => $A.text_lit("cbw")
  | 49 => $A.text_lit("cbx")
  | 50 => $A.text_lit("cby")
  | 51 => $A.text_lit("cbz")
  | 52 => $A.text_lit("cca")
  | 53 => $A.text_lit("ccb")
  | 54 => $A.text_lit("ccc")
  | 55 => $A.text_lit("ccd")
  | 56 => $A.text_lit("cce")
  | 57 => $A.text_lit("ccf")
  | 58 => $A.text_lit("ccg")
  | 59 => $A.text_lit("cch")
  | 60 => $A.text_lit("cci")
  | 61 => $A.text_lit("ccj")
  | 62 => $A.text_lit("cck")
  | 63 => $A.text_lit("ccl")
  | 64 => $A.text_lit("ccm")
  | 65 => $A.text_lit("ccn")
  | 66 => $A.text_lit("cco")
  | 67 => $A.text_lit("ccp")
  | 68 => $A.text_lit("ccq")
  | 69 => $A.text_lit("ccr")
  | 70 => $A.text_lit("ccs")
  | 71 => $A.text_lit("cct")
  | 72 => $A.text_lit("ccu")
  | 73 => $A.text_lit("ccv")
  | 74 => $A.text_lit("ccw")
  | 75 => $A.text_lit("ccx")
  | 76 => $A.text_lit("ccy")
  | 77 => $A.text_lit("ccz")
  | 78 => $A.text_lit("cda")
  | 79 => $A.text_lit("cdb")
  | 80 => $A.text_lit("cdc")
  | 81 => $A.text_lit("cdd")
  | 82 => $A.text_lit("cde")
  | 83 => $A.text_lit("cdf")
  | 84 => $A.text_lit("cdg")
  | 85 => $A.text_lit("cdh")
  | 86 => $A.text_lit("cdi")
  | 87 => $A.text_lit("cdj")
  | 88 => $A.text_lit("cdk")
  | 89 => $A.text_lit("cdl")
  | 90 => $A.text_lit("cdm")
  | 91 => $A.text_lit("cdn")
  | 92 => $A.text_lit("cdo")
  | 93 => $A.text_lit("cdp")
  | 94 => $A.text_lit("cdq")
  | 95 => $A.text_lit("cdr")
  | 96 => $A.text_lit("cds")
  | 97 => $A.text_lit("cdt")
  | 98 => $A.text_lit("cdu")
  | 99 => $A.text_lit("cdv")
  | 100 => $A.text_lit("cdw")
  | 101 => $A.text_lit("cdx")
  | 102 => $A.text_lit("cdy")
  | 103 => $A.text_lit("cdz")
  | 104 => $A.text_lit("cea")
  | 105 => $A.text_lit("ceb")
  | 106 => $A.text_lit("cec")
  | 107 => $A.text_lit("ced")
  | 108 => $A.text_lit("cee")
  | 109 => $A.text_lit("cef")
  | 110 => $A.text_lit("ceg")
  | 111 => $A.text_lit("ceh")
  | 112 => $A.text_lit("cei")
  | 113 => $A.text_lit("cej")
  | 114 => $A.text_lit("cek")
  | 115 => $A.text_lit("cel")
  | 116 => $A.text_lit("cem")
  | 117 => $A.text_lit("cen")
  | 118 => $A.text_lit("ceo")
  | 119 => $A.text_lit("cep")
  | 120 => $A.text_lit("ceq")
  | 121 => $A.text_lit("cer")
  | 122 => $A.text_lit("ces")
  | 123 => $A.text_lit("cet")
  | 124 => $A.text_lit("ceu")
  | 125 => $A.text_lit("cev")
  | 126 => $A.text_lit("cew")
  | 127 => $A.text_lit("cex")
  | 128 => $A.text_lit("cey")
  | 129 => $A.text_lit("cez")
  | 130 => $A.text_lit("cfa")
  | 131 => $A.text_lit("cfb")
  | 132 => $A.text_lit("cfc")
  | 133 => $A.text_lit("cfd")
  | 134 => $A.text_lit("cfe")
  | 135 => $A.text_lit("cff")
  | 136 => $A.text_lit("cfg")
  | 137 => $A.text_lit("cfh")
  | 138 => $A.text_lit("cfi")
  | 139 => $A.text_lit("cfj")
  | 140 => $A.text_lit("cfk")
  | 141 => $A.text_lit("cfl")
  | 142 => $A.text_lit("cfm")
  | 143 => $A.text_lit("cfn")
  | 144 => $A.text_lit("cfo")
  | 145 => $A.text_lit("cfp")
  | 146 => $A.text_lit("cfq")
  | 147 => $A.text_lit("cfr")
  | 148 => $A.text_lit("cfs")
  | 149 => $A.text_lit("cft")
  | 150 => $A.text_lit("cfu")
  | 151 => $A.text_lit("cfv")
  | 152 => $A.text_lit("cfw")
  | 153 => $A.text_lit("cfx")
  | 154 => $A.text_lit("cfy")
  | 155 => $A.text_lit("cfz")
  | 156 => $A.text_lit("cga")
  | 157 => $A.text_lit("cgb")
  | 158 => $A.text_lit("cgc")
  | 159 => $A.text_lit("cgd")
  | 160 => $A.text_lit("cge")
  | 161 => $A.text_lit("cgf")
  | 162 => $A.text_lit("cgg")
  | 163 => $A.text_lit("cgh")
  | 164 => $A.text_lit("cgi")
  | 165 => $A.text_lit("cgj")
  | 166 => $A.text_lit("cgk")
  | 167 => $A.text_lit("cgl")
  | 168 => $A.text_lit("cgm")
  | 169 => $A.text_lit("cgn")
  | 170 => $A.text_lit("cgo")
  | 171 => $A.text_lit("cgp")
  | 172 => $A.text_lit("cgq")
  | 173 => $A.text_lit("cgr")
  | 174 => $A.text_lit("cgs")
  | 175 => $A.text_lit("cgt")
  | 176 => $A.text_lit("cgu")
  | 177 => $A.text_lit("cgv")
  | 178 => $A.text_lit("cgw")
  | 179 => $A.text_lit("cgx")
  | 180 => $A.text_lit("cgy")
  | 181 => $A.text_lit("cgz")
  | 182 => $A.text_lit("cha")
  | 183 => $A.text_lit("chb")
  | 184 => $A.text_lit("chc")
  | 185 => $A.text_lit("chd")
  | 186 => $A.text_lit("che")
  | 187 => $A.text_lit("chf")
  | 188 => $A.text_lit("chg")
  | 189 => $A.text_lit("chh")
  | 190 => $A.text_lit("chi")
  | 191 => $A.text_lit("chj")
  | 192 => $A.text_lit("chk")
  | 193 => $A.text_lit("chl")
  | 194 => $A.text_lit("chm")
  | 195 => $A.text_lit("chn")
  | 196 => $A.text_lit("cho")
  | 197 => $A.text_lit("chp")
  | 198 => $A.text_lit("chq")
  | 199 => $A.text_lit("chr")
  | 200 => $A.text_lit("chs")
  | 201 => $A.text_lit("cht")
  | 202 => $A.text_lit("chu")
  | 203 => $A.text_lit("chv")
  | 204 => $A.text_lit("chw")
  | 205 => $A.text_lit("chx")
  | 206 => $A.text_lit("chy")
  | 207 => $A.text_lit("chz")
  | 208 => $A.text_lit("cia")
  | 209 => $A.text_lit("cib")
  | 210 => $A.text_lit("cic")
  | 211 => $A.text_lit("cid")
  | 212 => $A.text_lit("cie")
  | 213 => $A.text_lit("cif")
  | 214 => $A.text_lit("cig")
  | 215 => $A.text_lit("cih")
  | 216 => $A.text_lit("cii")
  | 217 => $A.text_lit("cij")
  | 218 => $A.text_lit("cik")
  | 219 => $A.text_lit("cil")
  | 220 => $A.text_lit("cim")
  | 221 => $A.text_lit("cin")
  | 222 => $A.text_lit("cio")
  | 223 => $A.text_lit("cip")
  | 224 => $A.text_lit("ciq")
  | 225 => $A.text_lit("cir")
  | 226 => $A.text_lit("cis")
  | 227 => $A.text_lit("cit")
  | 228 => $A.text_lit("ciu")
  | 229 => $A.text_lit("civ")
  | 230 => $A.text_lit("ciw")
  | 231 => $A.text_lit("cix")
  | 232 => $A.text_lit("ciy")
  | 233 => $A.text_lit("ciz")
  | 234 => $A.text_lit("cja")
  | 235 => $A.text_lit("cjb")
  | 236 => $A.text_lit("cjc")
  | 237 => $A.text_lit("cjd")
  | 238 => $A.text_lit("cje")
  | 239 => $A.text_lit("cjf")
  | 240 => $A.text_lit("cjg")
  | 241 => $A.text_lit("cjh")
  | 242 => $A.text_lit("cji")
  | 243 => $A.text_lit("cjj")
  | 244 => $A.text_lit("cjk")
  | 245 => $A.text_lit("cjl")
  | 246 => $A.text_lit("cjm")
  | 247 => $A.text_lit("cjn")
  | 248 => $A.text_lit("cjo")
  | 249 => $A.text_lit("cjp")
  | 250 => $A.text_lit("cjq")
  | 251 => $A.text_lit("cjr")
  | 252 => $A.text_lit("cjs")
  | 253 => $A.text_lit("cjt")
  | 254 => $A.text_lit("cju")
  | 255 => $A.text_lit("cjv")
  | 256 => $A.text_lit("cjw")
  | 257 => $A.text_lit("cjx")
  | 258 => $A.text_lit("cjy")
  | 259 => $A.text_lit("cjz")
  | 260 => $A.text_lit("cka")
  | 261 => $A.text_lit("ckb")
  | 262 => $A.text_lit("ckc")
  | 263 => $A.text_lit("ckd")
  | 264 => $A.text_lit("cke")
  | 265 => $A.text_lit("ckf")
  | 266 => $A.text_lit("ckg")
  | 267 => $A.text_lit("ckh")
  | 268 => $A.text_lit("cki")
  | 269 => $A.text_lit("ckj")
  | 270 => $A.text_lit("ckk")
  | 271 => $A.text_lit("ckl")
  | 272 => $A.text_lit("ckm")
  | 273 => $A.text_lit("ckn")
  | 274 => $A.text_lit("cko")
  | 275 => $A.text_lit("ckp")
  | 276 => $A.text_lit("ckq")
  | 277 => $A.text_lit("ckr")
  | 278 => $A.text_lit("cks")
  | 279 => $A.text_lit("ckt")
  | 280 => $A.text_lit("cku")
  | 281 => $A.text_lit("ckv")
  | 282 => $A.text_lit("ckw")
  | 283 => $A.text_lit("ckx")
  | 284 => $A.text_lit("cky")
  | 285 => $A.text_lit("ckz")
  | 286 => $A.text_lit("cla")
  | 287 => $A.text_lit("clb")
  | 288 => $A.text_lit("clc")
  | 289 => $A.text_lit("cld")
  | 290 => $A.text_lit("cle")
  | 291 => $A.text_lit("clf")
  | 292 => $A.text_lit("clg")
  | 293 => $A.text_lit("clh")
  | 294 => $A.text_lit("cli")
  | 295 => $A.text_lit("clj")
  | 296 => $A.text_lit("clk")
  | 297 => $A.text_lit("cll")
  | 298 => $A.text_lit("clm")
  | 299 => $A.text_lit("cln")
  | 300 => $A.text_lit("clo")
  | 301 => $A.text_lit("clp")
  | 302 => $A.text_lit("clq")
  | 303 => $A.text_lit("clr")
  | 304 => $A.text_lit("cls")
  | 305 => $A.text_lit("clt")
  | 306 => $A.text_lit("clu")
  | 307 => $A.text_lit("clv")
  | 308 => $A.text_lit("clw")
  | 309 => $A.text_lit("clx")
  | 310 => $A.text_lit("cly")
  | 311 => $A.text_lit("clz")
  | 312 => $A.text_lit("cma")
  | 313 => $A.text_lit("cmb")
  | 314 => $A.text_lit("cmc")
  | 315 => $A.text_lit("cmd")
  | 316 => $A.text_lit("cme")
  | 317 => $A.text_lit("cmf")
  | 318 => $A.text_lit("cmg")
  | 319 => $A.text_lit("cmh")
  | 320 => $A.text_lit("cmi")
  | 321 => $A.text_lit("cmj")
  | 322 => $A.text_lit("cmk")
  | 323 => $A.text_lit("cml")
  | 324 => $A.text_lit("cmm")
  | 325 => $A.text_lit("cmn")
  | 326 => $A.text_lit("cmo")
  | 327 => $A.text_lit("cmp")
  | 328 => $A.text_lit("cmq")
  | 329 => $A.text_lit("cmr")
  | 330 => $A.text_lit("cms")
  | 331 => $A.text_lit("cmt")
  | 332 => $A.text_lit("cmu")
  | 333 => $A.text_lit("cmv")
  | 334 => $A.text_lit("cmw")
  | 335 => $A.text_lit("cmx")
  | 336 => $A.text_lit("cmy")
  | 337 => $A.text_lit("cmz")
  | 338 => $A.text_lit("cna")
  | 339 => $A.text_lit("cnb")
  | 340 => $A.text_lit("cnc")
  | 341 => $A.text_lit("cnd")
  | 342 => $A.text_lit("cne")
  | 343 => $A.text_lit("cnf")
  | 344 => $A.text_lit("cng")
  | 345 => $A.text_lit("cnh")
  | 346 => $A.text_lit("cni")
  | 347 => $A.text_lit("cnj")
  | 348 => $A.text_lit("cnk")
  | 349 => $A.text_lit("cnl")
  | 350 => $A.text_lit("cnm")
  | 351 => $A.text_lit("cnn")
  | 352 => $A.text_lit("cno")
  | 353 => $A.text_lit("cnp")
  | 354 => $A.text_lit("cnq")
  | 355 => $A.text_lit("cnr")
  | 356 => $A.text_lit("cns")
  | 357 => $A.text_lit("cnt")
  | 358 => $A.text_lit("cnu")
  | 359 => $A.text_lit("cnv")
  | 360 => $A.text_lit("cnw")
  | 361 => $A.text_lit("cnx")
  | 362 => $A.text_lit("cny")
  | 363 => $A.text_lit("cnz")
  | 364 => $A.text_lit("coa")
  | 365 => $A.text_lit("cob")
  | 366 => $A.text_lit("coc")
  | 367 => $A.text_lit("cod")
  | 368 => $A.text_lit("coe")
  | 369 => $A.text_lit("cof")
  | 370 => $A.text_lit("cog")
  | 371 => $A.text_lit("coh")
  | 372 => $A.text_lit("coi")
  | 373 => $A.text_lit("coj")
  | 374 => $A.text_lit("cok")
  | 375 => $A.text_lit("col")
  | 376 => $A.text_lit("com")
  | 377 => $A.text_lit("con")
  | 378 => $A.text_lit("coo")
  | 379 => $A.text_lit("cop")
  | 380 => $A.text_lit("coq")
  | 381 => $A.text_lit("cor")
  | 382 => $A.text_lit("cos")
  | 383 => $A.text_lit("cot")
  | 384 => $A.text_lit("cou")
  | 385 => $A.text_lit("cov")
  | 386 => $A.text_lit("cow")
  | 387 => $A.text_lit("cox")
  | 388 => $A.text_lit("coy")
  | 389 => $A.text_lit("coz")
  | 390 => $A.text_lit("cpa")
  | 391 => $A.text_lit("cpb")
  | 392 => $A.text_lit("cpc")
  | 393 => $A.text_lit("cpd")
  | 394 => $A.text_lit("cpe")
  | 395 => $A.text_lit("cpf")
  | 396 => $A.text_lit("cpg")
  | 397 => $A.text_lit("cph")
  | 398 => $A.text_lit("cpi")
  | 399 => $A.text_lit("cpj")
  | 400 => $A.text_lit("cpk")
  | 401 => $A.text_lit("cpl")
  | 402 => $A.text_lit("cpm")
  | 403 => $A.text_lit("cpn")
  | 404 => $A.text_lit("cpo")
  | 405 => $A.text_lit("cpp")
  | 406 => $A.text_lit("cpq")
  | 407 => $A.text_lit("cpr")
  | 408 => $A.text_lit("cps")
  | 409 => $A.text_lit("cpt")
  | 410 => $A.text_lit("cpu")
  | 411 => $A.text_lit("cpv")
  | 412 => $A.text_lit("cpw")
  | 413 => $A.text_lit("cpx")
  | 414 => $A.text_lit("cpy")
  | 415 => $A.text_lit("cpz")
  | 416 => $A.text_lit("cqa")
  | 417 => $A.text_lit("cqb")
  | 418 => $A.text_lit("cqc")
  | 419 => $A.text_lit("cqd")
  | 420 => $A.text_lit("cqe")
  | 421 => $A.text_lit("cqf")
  | 422 => $A.text_lit("cqg")
  | 423 => $A.text_lit("cqh")
  | 424 => $A.text_lit("cqi")
  | 425 => $A.text_lit("cqj")
  | 426 => $A.text_lit("cqk")
  | 427 => $A.text_lit("cql")
  | 428 => $A.text_lit("cqm")
  | 429 => $A.text_lit("cqn")
  | 430 => $A.text_lit("cqo")
  | 431 => $A.text_lit("cqp")
  | 432 => $A.text_lit("cqq")
  | 433 => $A.text_lit("cqr")
  | 434 => $A.text_lit("cqs")
  | 435 => $A.text_lit("cqt")
  | 436 => $A.text_lit("cqu")
  | 437 => $A.text_lit("cqv")
  | 438 => $A.text_lit("cqw")
  | 439 => $A.text_lit("cqx")
  | 440 => $A.text_lit("cqy")
  | 441 => $A.text_lit("cqz")
  | 442 => $A.text_lit("cra")
  | 443 => $A.text_lit("crb")
  | 444 => $A.text_lit("crc")
  | 445 => $A.text_lit("crd")
  | 446 => $A.text_lit("cre")
  | 447 => $A.text_lit("crf")
  | 448 => $A.text_lit("crg")
  | 449 => $A.text_lit("crh")
  | 450 => $A.text_lit("cri")
  | 451 => $A.text_lit("crj")
  | 452 => $A.text_lit("crk")
  | 453 => $A.text_lit("crl")
  | 454 => $A.text_lit("crm")
  | 455 => $A.text_lit("crn")
  | 456 => $A.text_lit("cro")
  | 457 => $A.text_lit("crp")
  | 458 => $A.text_lit("crq")
  | 459 => $A.text_lit("crr")
  | 460 => $A.text_lit("crs")
  | 461 => $A.text_lit("crt")
  | 462 => $A.text_lit("cru")
  | 463 => $A.text_lit("crv")
  | 464 => $A.text_lit("crw")
  | 465 => $A.text_lit("crx")
  | 466 => $A.text_lit("cry")
  | 467 => $A.text_lit("crz")
  | 468 => $A.text_lit("csa")
  | 469 => $A.text_lit("csb")
  | 470 => $A.text_lit("csc")
  | 471 => $A.text_lit("csd")
  | 472 => $A.text_lit("cse")
  | 473 => $A.text_lit("csf")
  | 474 => $A.text_lit("csg")
  | 475 => $A.text_lit("csh")
  | 476 => $A.text_lit("csi")
  | 477 => $A.text_lit("csj")
  | 478 => $A.text_lit("csk")
  | 479 => $A.text_lit("csl")
  | 480 => $A.text_lit("csm")
  | 481 => $A.text_lit("csn")
  | 482 => $A.text_lit("cso")
  | 483 => $A.text_lit("csp")
  | 484 => $A.text_lit("csq")
  | 485 => $A.text_lit("csr")
  | 486 => $A.text_lit("css")
  | 487 => $A.text_lit("cst")
  | 488 => $A.text_lit("csu")
  | 489 => $A.text_lit("csv")
  | 490 => $A.text_lit("csw")
  | 491 => $A.text_lit("csx")
  | 492 => $A.text_lit("csy")
  | 493 => $A.text_lit("csz")
  | 494 => $A.text_lit("cta")
  | 495 => $A.text_lit("ctb")
  | 496 => $A.text_lit("ctc")
  | 497 => $A.text_lit("ctd")
  | 498 => $A.text_lit("cte")
  | 499 => $A.text_lit("ctf")
  | 500 => $A.text_lit("ctg")
  | 501 => $A.text_lit("cth")
  | 502 => $A.text_lit("cti")
  | 503 => $A.text_lit("ctj")
  | 504 => $A.text_lit("ctk")
  | 505 => $A.text_lit("ctl")
  | 506 => $A.text_lit("ctm")
  | 507 => $A.text_lit("ctn")
  | 508 => $A.text_lit("cto")
  | 509 => $A.text_lit("ctp")
  | 510 => $A.text_lit("ctq")
  | 511 => $A.text_lit("ctr")
  | 512 => $A.text_lit("cts")
  | 513 => $A.text_lit("ctt")
  | 514 => $A.text_lit("ctu")
  | 515 => $A.text_lit("ctv")
  | 516 => $A.text_lit("ctw")
  | 517 => $A.text_lit("ctx")
  | 518 => $A.text_lit("cty")
  | 519 => $A.text_lit("ctz")
  | 520 => $A.text_lit("cua")
  | 521 => $A.text_lit("cub")
  | 522 => $A.text_lit("cuc")
  | 523 => $A.text_lit("cud")
  | 524 => $A.text_lit("cue")
  | 525 => $A.text_lit("cuf")
  | 526 => $A.text_lit("cug")
  | 527 => $A.text_lit("cuh")
  | 528 => $A.text_lit("cui")
  | 529 => $A.text_lit("cuj")
  | 530 => $A.text_lit("cuk")
  | 531 => $A.text_lit("cul")
  | 532 => $A.text_lit("cum")
  | 533 => $A.text_lit("cun")
  | 534 => $A.text_lit("cuo")
  | 535 => $A.text_lit("cup")
  | 536 => $A.text_lit("cuq")
  | 537 => $A.text_lit("cur")
  | 538 => $A.text_lit("cus")
  | 539 => $A.text_lit("cut")
  | 540 => $A.text_lit("cuu")
  | 541 => $A.text_lit("cuv")
  | 542 => $A.text_lit("cuw")
  | 543 => $A.text_lit("cux")
  | 544 => $A.text_lit("cuy")
  | 545 => $A.text_lit("cuz")
  | 546 => $A.text_lit("cva")
  | 547 => $A.text_lit("cvb")
  | 548 => $A.text_lit("cvc")
  | 549 => $A.text_lit("cvd")
  | 550 => $A.text_lit("cve")
  | 551 => $A.text_lit("cvf")
  | 552 => $A.text_lit("cvg")
  | 553 => $A.text_lit("cvh")
  | 554 => $A.text_lit("cvi")
  | 555 => $A.text_lit("cvj")
  | 556 => $A.text_lit("cvk")
  | 557 => $A.text_lit("cvl")
  | 558 => $A.text_lit("cvm")
  | 559 => $A.text_lit("cvn")
  | 560 => $A.text_lit("cvo")
  | 561 => $A.text_lit("cvp")
  | 562 => $A.text_lit("cvq")
  | 563 => $A.text_lit("cvr")
  | 564 => $A.text_lit("cvs")
  | 565 => $A.text_lit("cvt")
  | 566 => $A.text_lit("cvu")
  | 567 => $A.text_lit("cvv")
  | 568 => $A.text_lit("cvw")
  | 569 => $A.text_lit("cvx")
  | 570 => $A.text_lit("cvy")
  | 571 => $A.text_lit("cvz")
  | 572 => $A.text_lit("cwa")
  | 573 => $A.text_lit("cwb")
  | 574 => $A.text_lit("cwc")
  | 575 => $A.text_lit("cwd")
  | 576 => $A.text_lit("cwe")
  | 577 => $A.text_lit("cwf")
  | 578 => $A.text_lit("cwg")
  | 579 => $A.text_lit("cwh")
  | 580 => $A.text_lit("cwi")
  | 581 => $A.text_lit("cwj")
  | 582 => $A.text_lit("cwk")
  | 583 => $A.text_lit("cwl")
  | 584 => $A.text_lit("cwm")
  | 585 => $A.text_lit("cwn")
  | 586 => $A.text_lit("cwo")
  | 587 => $A.text_lit("cwp")
  | 588 => $A.text_lit("cwq")
  | 589 => $A.text_lit("cwr")
  | 590 => $A.text_lit("cws")
  | 591 => $A.text_lit("cwt")
  | 592 => $A.text_lit("cwu")
  | 593 => $A.text_lit("cwv")
  | 594 => $A.text_lit("cww")
  | 595 => $A.text_lit("cwx")
  | 596 => $A.text_lit("cwy")
  | 597 => $A.text_lit("cwz")
  | 598 => $A.text_lit("cxa")
  | 599 => $A.text_lit("cxb")
  | 600 => $A.text_lit("cxc")
  | 601 => $A.text_lit("cxd")
  | 602 => $A.text_lit("cxe")
  | 603 => $A.text_lit("cxf")
  | 604 => $A.text_lit("cxg")
  | 605 => $A.text_lit("cxh")
  | 606 => $A.text_lit("cxi")
  | 607 => $A.text_lit("cxj")
  | 608 => $A.text_lit("cxk")
  | 609 => $A.text_lit("cxl")
  | 610 => $A.text_lit("cxm")
  | 611 => $A.text_lit("cxn")
  | 612 => $A.text_lit("cxo")
  | 613 => $A.text_lit("cxp")
  | 614 => $A.text_lit("cxq")
  | 615 => $A.text_lit("cxr")
  | 616 => $A.text_lit("cxs")
  | 617 => $A.text_lit("cxt")
  | 618 => $A.text_lit("cxu")
  | 619 => $A.text_lit("cxv")
  | 620 => $A.text_lit("cxw")
  | 621 => $A.text_lit("cxx")
  | 622 => $A.text_lit("cxy")
  | 623 => $A.text_lit("cxz")
  | 624 => $A.text_lit("cya")
  | 625 => $A.text_lit("cyb")
  | 626 => $A.text_lit("cyc")
  | 627 => $A.text_lit("cyd")
  | 628 => $A.text_lit("cye")
  | 629 => $A.text_lit("cyf")
  | 630 => $A.text_lit("cyg")
  | 631 => $A.text_lit("cyh")
  | 632 => $A.text_lit("cyi")
  | 633 => $A.text_lit("cyj")
  | 634 => $A.text_lit("cyk")
  | 635 => $A.text_lit("cyl")
  | 636 => $A.text_lit("cym")
  | 637 => $A.text_lit("cyn")
  | 638 => $A.text_lit("cyo")
  | 639 => $A.text_lit("cyp")
  | 640 => $A.text_lit("cyq")
  | 641 => $A.text_lit("cyr")
  | 642 => $A.text_lit("cys")
  | 643 => $A.text_lit("cyt")
  | 644 => $A.text_lit("cyu")
  | 645 => $A.text_lit("cyv")
  | 646 => $A.text_lit("cyw")
  | 647 => $A.text_lit("cyx")
  | 648 => $A.text_lit("cyy")
  | 649 => $A.text_lit("cyz")
  | 650 => $A.text_lit("cza")
  | 651 => $A.text_lit("czb")
  | 652 => $A.text_lit("czc")
  | 653 => $A.text_lit("czd")
  | 654 => $A.text_lit("cze")
  | 655 => $A.text_lit("czf")
  | 656 => $A.text_lit("czg")
  | 657 => $A.text_lit("czh")
  | 658 => $A.text_lit("czi")
  | 659 => $A.text_lit("czj")
  | 660 => $A.text_lit("czk")
  | 661 => $A.text_lit("czl")
  | 662 => $A.text_lit("czm")
  | 663 => $A.text_lit("czn")
  | 664 => $A.text_lit("czo")
  | 665 => $A.text_lit("czp")
  | 666 => $A.text_lit("czq")
  | 667 => $A.text_lit("czr")
  | 668 => $A.text_lit("czs")
  | 669 => $A.text_lit("czt")
  | 670 => $A.text_lit("czu")
  | 671 => $A.text_lit("czv")
  | 672 => $A.text_lit("czw")
  | 673 => $A.text_lit("czx")
  | 674 => $A.text_lit("czy")
  | _ => $A.text_lit("czz")

implement class_text(idx) = @(_class_lit(idx), 3)
