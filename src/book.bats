(* book -- a book's CSS, narrowed into proved spans of the bytes it was read
   from (keyword: book css; bats-lang/quire#1063, design section 3.4 to 3.6,
   bats-lang/css#79)

   The reader takes a sheet that is a borrowed array of known cells
   (cborrow) and gives a tree whose leaves are spans (an offset and a length)
   of those cells. A span is never a copy. Each span that the walk will hand
   to a sink comes with a proof built from NTH facts that cborrow_get gives:
   CSS_IDENT (an identifier), STRING (a quoted string), VALUE_SAFE (a value
   of a declaration). A proof is about the cells cs of one array, so a span
   of another array does not type-check as one of this array's (the static
   reject fixture book-declaration-forged), and the constructors of a proof
   take the NTH fact of each byte, so a byte is not claimed safe unchecked
   (book-ident-unchecked).

   What a proof says is about bytes: no backslash, at-sign, brace,
   semicolon, exclamation mark, angle bracket, control or non-ASCII byte
   outside a string; a string with no backslash, brace, angle bracket, or
   its own quote, and no way to spell one of book_refused's substrings; a
   parenthesis only after an identifier (a function) or inside one; numbers
   of at most 6 integer digits and 4 decimals. Which function, unit or
   keyword a declaration may use is policy: the tables below (book_prop_row,
   the function and unit and keyword lists) decide it, and a name that is not
   in them drops the declaration.

   The reader only drops: a declaration, a selector, a rule, an at-rule, the
   important flag, a comment. A value's bytes reach a sink exactly as the book
   wrote them (the walk emits spans of them). Nothing here is unsafe and
   nothing is checked at run time that a type could prove: the loops end by
   a metric on the bytes left, and every index is in range by its type. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as B

staload "./contrast.bats"

(* ============================================================
   Byte classes
   ============================================================ *)

(* The first byte of an identifier: a letter, a hyphen or an underscore *)
#pub dataprop IDENT_START(int) =
  | {c:int | 65 <= c; c <= 90} IDENT_START_upper(c)
  | {c:int | 97 <= c; c <= 122} IDENT_START_lower(c)
  | IDENT_START_hyphen(45)
  | IDENT_START_underscore(95)

(* Any byte of an identifier *)
#pub dataprop IDENT_CH(int) =
  | {c:int} IDENT_CH_start(c) of IDENT_START(c)
  | {c:int | 48 <= c; c <= 57} IDENT_CH_digit(c)

(* White space *)
#pub dataprop WS_CH(int) =
  | WS_CH_tab(9)
  | WS_CH_lf(10)
  | WS_CH_cr(13)
  | WS_CH_space(32)

#pub dataprop DIGIT_CH(int) =
  | {c:int | 48 <= c; c <= 57} DIGIT_CH_mk(c)

(* A hexadecimal digit, with its value *)
#pub dataprop HEX_CH(int, int) =
  | {c:int | 48 <= c; c <= 57} HEX_CH_digit(c, c - 48)
  | {c:int | 65 <= c; c <= 70} HEX_CH_upper(c, c - 55)
  | {c:int | 97 <= c; c <= 102} HEX_CH_lower(c, c - 87)

(* ============================================================
   Runs of bytes, proved from NTH facts
   ============================================================ *)

(* k bytes from o, each an identifier byte *)
#pub dataprop IDENT_BODY(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int} IDENT_BODY_nil(cs, o, 0)
  | {cs:$A.cells}{o:int}{k:nat}{c:int}
    IDENT_BODY_snoc(cs, o, k + 1) of (IDENT_BODY(cs, o, k), $A.NTH(cs, o + k, c), IDENT_CH(c))

(* The k bytes from o are an identifier (CSS_IDENT: the proof that a
   leaf of the tree is an identifier, no escape, no non-ASCII byte) *)
#pub dataprop CSS_IDENT(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int}{k:nat}{c:int}
    CSS_IDENT_mk(cs, o, k + 1) of ($A.NTH(cs, o, c), IDENT_START(c), IDENT_BODY(cs, o + 1, k))

(* k white space bytes from o *)
#pub dataprop SPACES_RUN(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int} SPACES_RUN_nil(cs, o, 0)
  | {cs:$A.cells}{o:int}{k:nat}{c:int}
    SPACES_RUN_snoc(cs, o, k + 1) of (SPACES_RUN(cs, o, k), $A.NTH(cs, o + k, c), WS_CH(c))

(* k decimal digits from o *)
#pub dataprop DIGITS_RUN(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int} DIGITS_RUN_nil(cs, o, 0)
  | {cs:$A.cells}{o:int}{k:nat}{c:int}
    DIGITS_RUN_snoc(cs, o, k + 1) of (DIGITS_RUN(cs, o, k), $A.NTH(cs, o + k, c), DIGIT_CH(c))

(* k hexadecimal digits from o *)
#pub dataprop HEX_RUN(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int} HEX_RUN_nil(cs, o, 0)
  | {cs:$A.cells}{o:int}{k:nat}{c,v:int}
    HEX_RUN_snoc(cs, o, k + 1) of (HEX_RUN(cs, o, k), $A.NTH(cs, o + k, c), HEX_CH(c, v))

(* ============================================================
   Numbers (P3): at most 6 integer digits and 4 decimals
   ============================================================ *)

#pub dataprop NUMBER_UNSIGNED(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int}{i:int | 1 <= i; i <= 6}
    NUMBER_whole(cs, o, i) of DIGITS_RUN(cs, o, i)
  | {cs:$A.cells}{o:int}{i,f:int | 1 <= i; i <= 6; 1 <= f; f <= 4}
    NUMBER_fraction(cs, o, i + 1 + f) of (DIGITS_RUN(cs, o, i), $A.NTH(cs, o + i, 46), DIGITS_RUN(cs, o + i + 1, f))
  | {cs:$A.cells}{o:int}{f:int | 1 <= f; f <= 4}
    NUMBER_dot(cs, o, 1 + f) of ($A.NTH(cs, o, 46), DIGITS_RUN(cs, o + 1, f))

#pub dataprop NUMBER(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int}{m:int} NUMBER_plain(cs, o, m) of NUMBER_UNSIGNED(cs, o, m)
  | {cs:$A.cells}{o:int}{m:int} NUMBER_plus(cs, o, m + 1) of ($A.NTH(cs, o, 43), NUMBER_UNSIGNED(cs, o + 1, m))
  | {cs:$A.cells}{o:int}{m:int} NUMBER_minus(cs, o, m + 1) of ($A.NTH(cs, o, 45), NUMBER_UNSIGNED(cs, o + 1, m))

(* ============================================================
   Hexadecimal colours
   ============================================================ *)

(* #rgb, #rgba, #rrggbb or #rrggbbaa *)
#pub dataprop HEX_COLOR(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int} HEX_COLOR_3(cs, o, 4) of ($A.NTH(cs, o, 35), HEX_RUN(cs, o + 1, 3))
  | {cs:$A.cells}{o:int} HEX_COLOR_4(cs, o, 5) of ($A.NTH(cs, o, 35), HEX_RUN(cs, o + 1, 4))
  | {cs:$A.cells}{o:int} HEX_COLOR_6(cs, o, 7) of ($A.NTH(cs, o, 35), HEX_RUN(cs, o + 1, 6))
  | {cs:$A.cells}{o:int} HEX_COLOR_8(cs, o, 9) of ($A.NTH(cs, o, 35), HEX_RUN(cs, o + 1, 8))

(* The colour #rgb or #rrggbb has the channels r, g, b *)
#pub dataprop HEX_RGB(cs: $A.cells, o: int, m: int, r: int, g: int, b: int) =
  | {cs:$A.cells}{o:int}{c1,c2,c3,h1,h2,h3:int}
    HEX_RGB_3(cs, o, 4, h1 * 17, h2 * 17, h3 * 17) of
      ($A.NTH(cs, o, 35), $A.NTH(cs, o + 1, c1), HEX_CH(c1, h1),
       $A.NTH(cs, o + 2, c2), HEX_CH(c2, h2), $A.NTH(cs, o + 3, c3), HEX_CH(c3, h3))
  | {cs:$A.cells}{o:int}{c1,c2,c3,c4,c5,c6,h1,h2,h3,h4,h5,h6:int}
    HEX_RGB_6(cs, o, 7, h1 * 16 + h2, h3 * 16 + h4, h5 * 16 + h6) of
      ($A.NTH(cs, o, 35),
       $A.NTH(cs, o + 1, c1), HEX_CH(c1, h1), $A.NTH(cs, o + 2, c2), HEX_CH(c2, h2),
       $A.NTH(cs, o + 3, c3), HEX_CH(c3, h3), $A.NTH(cs, o + 4, c4), HEX_CH(c4, h4),
       $A.NTH(cs, o + 5, c5), HEX_CH(c5, h5), $A.NTH(cs, o + 6, c6), HEX_CH(c6, h6))

(* ============================================================
   Strings (P1)
   A string has no backslash, no brace, no angle bracket, no at-sign, no
   exclamation mark, no control byte and not its own quote. An opening
   parenthesis in it follows no identifier byte, and an asterisk follows no
   slash, so no substring of book_refused (url(, image-set(, var(, attr(,
   a slash-asterisk ...) can be spelled in one.
   ============================================================ *)

(* A byte that is not an identifier byte, a slash, a parenthesis or an
   asterisk, and can be in a string *)
#pub dataprop STR_CH_PLAIN(int) =
  | STR_CH_PLAIN_space(32)
  | {c:int | 35 <= c; c <= 38} STR_CH_PLAIN_symbol(c)
  | STR_CH_PLAIN_close(41)
  | {c:int | 43 <= c; c <= 44} STR_CH_PLAIN_comma(c)
  | STR_CH_PLAIN_dot(46)
  | {c:int | 58 <= c; c <= 59} STR_CH_PLAIN_colon(c)
  | {c:int | 61 <= c; c <= 63} STR_CH_PLAIN_compare(c)
  | STR_CH_PLAIN_open_square(91)
  | {c:int | 93 <= c; c <= 94} STR_CH_PLAIN_square(c)
  | STR_CH_PLAIN_tick(96)
  | STR_CH_PLAIN_bar(124)
  | STR_CH_PLAIN_tilde(126)
  | {c:int | 128 <= c; c <= 255} STR_CH_PLAIN_high(c)

(* The body of a string: k bytes from o inside the quote q; last is 0 when
   the last byte is any other byte, 1 when it is an identifier byte, 2 when it is
   a slash (the quote q itself counts as any other byte) *)
#pub dataprop STR_BODY(cs: $A.cells, o: int, k: int, q: int, last: int) =
  | {cs:$A.cells}{o:int}{q:int} STR_BODY_nil(cs, o, 0, q, 0)
  | {cs:$A.cells}{o:int}{k:nat}{q,last,c:int}
    STR_BODY_plain(cs, o, k + 1, q, 0) of (STR_BODY(cs, o, k, q, last), $A.NTH(cs, o + k, c), STR_CH_PLAIN(c))
  | {cs:$A.cells}{o:int}{k:nat}{q,last,c:int}
    STR_BODY_ident(cs, o, k + 1, q, 1) of (STR_BODY(cs, o, k, q, last), $A.NTH(cs, o + k, c), IDENT_CH(c))
  | {cs:$A.cells}{o:int}{k:nat}{q,last:int}
    STR_BODY_slash(cs, o, k + 1, q, 2) of (STR_BODY(cs, o, k, q, last), $A.NTH(cs, o + k, 47))
  | {cs:$A.cells}{o:int}{k:nat}{q:int}
    STR_BODY_paren_after_other(cs, o, k + 1, q, 0) of (STR_BODY(cs, o, k, q, 0), $A.NTH(cs, o + k, 40))
  | {cs:$A.cells}{o:int}{k:nat}{q:int}
    STR_BODY_paren_after_slash(cs, o, k + 1, q, 0) of (STR_BODY(cs, o, k, q, 2), $A.NTH(cs, o + k, 40))
  | {cs:$A.cells}{o:int}{k:nat}{q:int}
    STR_BODY_star_after_other(cs, o, k + 1, q, 0) of (STR_BODY(cs, o, k, q, 0), $A.NTH(cs, o + k, 42))
  | {cs:$A.cells}{o:int}{k:nat}{q:int}
    STR_BODY_star_after_ident(cs, o, k + 1, q, 0) of (STR_BODY(cs, o, k, q, 1), $A.NTH(cs, o + k, 42))
  | {cs:$A.cells}{o:int}{k:nat}{last:int}
    STR_BODY_single_in_double(cs, o, k + 1, 34, 0) of (STR_BODY(cs, o, k, 34, last), $A.NTH(cs, o + k, 39))
  | {cs:$A.cells}{o:int}{k:nat}{last:int}
    STR_BODY_double_in_single(cs, o, k + 1, 39, 0) of (STR_BODY(cs, o, k, 39, last), $A.NTH(cs, o + k, 34))

(* The m bytes from o are a quoted string *)
#pub dataprop STRING(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int}{k:nat}{last:int}
    STRING_double(cs, o, k + 2) of ($A.NTH(cs, o, 34), STR_BODY(cs, o + 1, k, 34, last), $A.NTH(cs, o + 1 + k, 34))
  | {cs:$A.cells}{o:int}{k:nat}{last:int}
    STRING_single(cs, o, k + 2) of ($A.NTH(cs, o, 39), STR_BODY(cs, o + 1, k, 39, last), $A.NTH(cs, o + 1 + k, 39))

(* ============================================================
   Values (P1 to P5)
   A value is a run of tokens, each proved from NTH facts. d is the depth
   of parentheses: a value is safe only at depth 0, so a closing parenthesis
   without its opener, which would end a condition early, is not one.
   ============================================================ *)

(* url(#id): the one use of url( that is kept, in the SVG paint grammar:
   a reference to an element of the same document *)
#pub dataprop FRAGMENT(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int}{a:pos}
    FRAGMENT_mk(cs, o, a + 6) of
      ($A.NTH(cs, o, 117), $A.NTH(cs, o + 1, 114), $A.NTH(cs, o + 2, 108), $A.NTH(cs, o + 3, 40),
       $A.NTH(cs, o + 4, 35), CSS_IDENT(cs, o + 5, a), $A.NTH(cs, o + 5 + a, 41))

#pub dataprop VALUE_PARTIAL(cs: $A.cells, o: int, k: int, g: int, p: int, d: int) =
  | {cs:$A.cells}{o:int}{g,p:int} VP_nil(cs, o, 0, g, p, 0)
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}{g,p,d:int}
    VP_space(cs, o, k + m, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), SPACES_RUN(cs, o + k, m))
  | {cs:$A.cells}{o:int}{k:nat}{g,p,d:int}
    VP_comma(cs, o, k + 1, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), $A.NTH(cs, o + k, 44))
  | {cs:$A.cells}{o:int}{k:nat}{g,p,d:int}
    VP_slash(cs, o, k + 1, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), $A.NTH(cs, o + k, 47))
  | {cs:$A.cells}{o:int}{k:nat}{g,p,d:int}
    VP_plus(cs, o, k + 1, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), $A.NTH(cs, o + k, 43))
  | {cs:$A.cells}{o:int}{k:nat}{g,p,d:int}
    VP_star(cs, o, k + 1, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), $A.NTH(cs, o + k, 42))
  | {cs:$A.cells}{o:int}{k:nat}{g,p,d:int}
    VP_percent(cs, o, k + 1, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), $A.NTH(cs, o + k, 37))
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}{g,p,d:int}
    VP_ident(cs, o, k + m, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), CSS_IDENT(cs, o + k, m))
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}{g,p,d:int}
    VP_number(cs, o, k + m, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), NUMBER(cs, o + k, m))
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}{g,p,d:int}
    VP_hex(cs, o, k + m, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), HEX_COLOR(cs, o + k, m))
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}{g,p,d:int}
    VP_string(cs, o, k + m, g, p, d) of (VALUE_PARTIAL(cs, o, k, g, p, d), STRING(cs, o + k, m))
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}{g,p,d:int}
    VP_function(cs, o, k + m + 1, g, p, d + 1) of
      (VALUE_PARTIAL(cs, o, k, g, p, d), CSS_IDENT(cs, o + k, m), $A.NTH(cs, o + k + m, 40))
  | {cs:$A.cells}{o:int}{k:nat}{g,p,d:int}
    VP_paren(cs, o, k + 1, g, p, d + 1) of (VALUE_PARTIAL(cs, o, k, g, p, d), $A.NTH(cs, o + k, 40))
  | {cs:$A.cells}{o:int}{k:nat}{g,p:int}{d:pos}
    VP_close(cs, o, k + 1, g, p, d - 1) of (VALUE_PARTIAL(cs, o, k, g, p, d), $A.NTH(cs, o + k, 41))
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}{p:int}{d:int}
    VP_fragment(cs, o, k + m, 3, p, d) of (VALUE_PARTIAL(cs, o, k, 3, p, d), FRAGMENT(cs, o + k, m))

(* The k bytes from o are a value of the property p (the row of the table of
   properties) in the grammar g: 1 for reflow, 2 for fixed layout, 3 for SVG
   paint. A property in the table has p >= 1; the reflow text colour, p = 0, is
   made only by its own constructor, which holds the proof of the contrast
   (P5) of a hexadecimal colour against two grounds. *)
#pub dataprop VALUE_SAFE(cs: $A.cells, o: int, k: int, g: int, p: int) =
  | {cs:$A.cells}{o:int}{k:nat}{g:int}{p:int | p >= 1}
    VALUE_SAFE_general(cs, o, k, g, p) of VALUE_PARTIAL(cs, o, k, g, p, 0)
  | {cs:$A.cells}{o:int}{m:pos}{r,gr,b,c1,c2:int}
    VALUE_SAFE_text_color(cs, o, m, 1, 0) of
      (HEX_RGB(cs, o, m, r, gr, b), CONTRAST(r * 65536 + gr * 256 + b, c1, 45),
       CONTRAST(r * 65536 + gr * 256 + b, c2, 45))

(* ============================================================
   Media and supports conditions
   ============================================================ *)

#pub dataprop QUERY_PARTIAL(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int} QP_nil(cs, o, 0)
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}
    QP_space(cs, o, k + m) of (QUERY_PARTIAL(cs, o, k), SPACES_RUN(cs, o + k, m))
  | {cs:$A.cells}{o:int}{k:nat}{m:pos}
    QP_ident(cs, o, k + m) of (QUERY_PARTIAL(cs, o, k), CSS_IDENT(cs, o + k, m))
  | {cs:$A.cells}{o:int}{k:nat}{a:pos}{s:nat}{v:pos}{g,p:int}
    QP_condition(cs, o, k + a + s + v + 3) of
      (QUERY_PARTIAL(cs, o, k), $A.NTH(cs, o + k, 40), CSS_IDENT(cs, o + k + 1, a),
       $A.NTH(cs, o + k + 1 + a, 58), SPACES_RUN(cs, o + k + 2 + a, s),
       VALUE_SAFE(cs, o + k + 2 + a + s, v, g, p), $A.NTH(cs, o + k + 2 + a + s + v, 41))

(* The k bytes from o are a media query or a supports condition: names and
   parenthesized (feature: value) *)
#pub dataprop QUERY_SAFE(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int}{k:nat} QUERY_SAFE_mk(cs, o, k) of QUERY_PARTIAL(cs, o, k)

(* ============================================================
   Reading bytes
   ============================================================ *)

(* Byte i of the sheet, with the proof of it *)
fn _byte {l:agz}{n:nat}{cs:$A.cells}{i:nat | i < n}
  (src: !$A.cborrow(l, n, cs), i: int i): [v:int | 0 <= v; v < 256] ($A.NTH(cs, i, v) | int v) =
  $A.cborrow_get(src, i)

(* ============================================================
   Scanning: identifiers, spaces, digits, hexadecimal digits
   ============================================================ *)

dataprop IDENT_CLASS(c: int, b: bool) =
  | {c:int} IDENT_CLASS_yes(c, true) of IDENT_CH(c)
  | {c:int} IDENT_CLASS_no(c, false)

fn _ident_class {c:int | 0 <= c; c < 256} (c: int c): [b:bool] (IDENT_CLASS(c, b) | bool b) =
  if c >= 97 then
    (if c <= 122 then (IDENT_CLASS_yes(IDENT_CH_start(IDENT_START_lower())) | true)
     else if c = 95 then (IDENT_CLASS_yes(IDENT_CH_start(IDENT_START_underscore())) | true)
     else (IDENT_CLASS_no() | false))
  else if c >= 65 then
    (if c <= 90 then (IDENT_CLASS_yes(IDENT_CH_start(IDENT_START_upper())) | true)
     else if c = 95 then (IDENT_CLASS_yes(IDENT_CH_start(IDENT_START_underscore())) | true)
     else (IDENT_CLASS_no() | false))
  else if c >= 48 then
    (if c <= 57 then (IDENT_CLASS_yes(IDENT_CH_digit()) | true) else (IDENT_CLASS_no() | false))
  else if c = 45 then (IDENT_CLASS_yes(IDENT_CH_start(IDENT_START_hyphen())) | true)
  else (IDENT_CLASS_no() | false)

dataprop START_CLASS(c: int, b: bool) =
  | {c:int} START_CLASS_yes(c, true) of IDENT_START(c)
  | {c:int} START_CLASS_no(c, false)

fn _start_class {c:int | 0 <= c; c < 256} (c: int c): [b:bool] (START_CLASS(c, b) | bool b) =
  if c >= 97 then
    (if c <= 122 then (START_CLASS_yes(IDENT_START_lower()) | true)
     else if c = 95 then (START_CLASS_yes(IDENT_START_underscore()) | true)
     else (START_CLASS_no() | false))
  else if c >= 65 then
    (if c <= 90 then (START_CLASS_yes(IDENT_START_upper()) | true)
     else if c = 95 then (START_CLASS_yes(IDENT_START_underscore()) | true)
     else (START_CLASS_no() | false))
  else if c = 45 then (START_CLASS_yes(IDENT_START_hyphen()) | true)
  else (START_CLASS_no() | false)

dataprop WS_CLASS(c: int, b: bool) =
  | {c:int} WS_CLASS_yes(c, true) of WS_CH(c)
  | {c:int} WS_CLASS_no(c, false)

fn _ws_class {c:int | 0 <= c; c < 256} (c: int c): [b:bool] (WS_CLASS(c, b) | bool b) =
  if c = 32 then (WS_CLASS_yes(WS_CH_space()) | true)
  else if c = 10 then (WS_CLASS_yes(WS_CH_lf()) | true)
  else if c = 9 then (WS_CLASS_yes(WS_CH_tab()) | true)
  else if c = 13 then (WS_CLASS_yes(WS_CH_cr()) | true)
  else (WS_CLASS_no() | false)

dataprop DIGIT_CLASS(c: int, b: bool) =
  | {c:int} DIGIT_CLASS_yes(c, true) of DIGIT_CH(c)
  | {c:int} DIGIT_CLASS_no(c, false)

fn _digit_class {c:int | 0 <= c; c < 256} (c: int c): [b:bool] (DIGIT_CLASS(c, b) | bool b) =
  if c >= 48 then
    (if c <= 57 then (DIGIT_CLASS_yes(DIGIT_CH_mk()) | true) else (DIGIT_CLASS_no() | false))
  else (DIGIT_CLASS_no() | false)

dataprop HEX_CLASS(c: int, v: int, b: bool) =
  | {c,v:int} HEX_CLASS_yes(c, v, true) of HEX_CH(c, v)
  | {c,v:int} HEX_CLASS_no(c, v, false)

fn _hex_class {c:int | 0 <= c; c < 256} (c: int c): [v:int][b:bool] (HEX_CLASS(c, v, b) | bool b) =
  if c >= 97 then
    (if c <= 102 then (HEX_CLASS_yes(HEX_CH_lower()) | true) else (HEX_CLASS_no() | false))
  else if c >= 65 then
    (if c <= 70 then (HEX_CLASS_yes(HEX_CH_upper()) | true) else (HEX_CLASS_no() | false))
  else if c >= 48 then
    (if c <= 57 then (HEX_CLASS_yes(HEX_CH_digit()) | true) else (HEX_CLASS_no() | false))
  else (HEX_CLASS_no() | false)

(* The run of identifier bytes that goes on from o + k *)
fun _ident_body {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat}{k:nat | o + k <= len} .<len - o - k>.
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o, k: int k, pf: IDENT_BODY(cs, o, k))
  : [k2:nat | k <= k2; o + k2 <= len] (IDENT_BODY(cs, o, k2) | int k2) =
  if o + k >= len then (pf | k)
  else let
    val @(pn | c) = _byte(src, o + k)
    val @(pc | b) = _ident_class(c)
  in
    if b then let
      prval IDENT_CLASS_yes(ch) = pc
    in _ident_body(src, len, o, k + 1, IDENT_BODY_snoc(pf, pn, ch)) end
    else let
      prval IDENT_CLASS_no() = pc
    in (pf | k) end
  end

#pub dataprop IDENT_OPT(cs: $A.cells, o: int, k: int) =
  | {cs:$A.cells}{o:int} IDENT_OPT_none(cs, o, 0)
  | {cs:$A.cells}{o:int}{k:pos} IDENT_OPT_some(cs, o, k) of CSS_IDENT(cs, o, k)

(* The identifier at o, if there is one *)
fn _scan_ident {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat | o <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o)
  : [k:nat | o + k <= len] (IDENT_OPT(cs, o, k) | int k) =
  if o >= len then (IDENT_OPT_none() | 0)
  else let
    val @(pn | c) = _byte(src, o)
    val @(pc | b) = _start_class(c)
  in
    if b then let
      prval START_CLASS_yes(st) = pc
      val @(body | k) = _ident_body(src, len, o + 1, 0, IDENT_BODY_nil())
    in (IDENT_OPT_some(CSS_IDENT_mk(pn, st, body)) | k + 1) end
    else let
      prval START_CLASS_no() = pc
    in (IDENT_OPT_none() | 0) end
  end

(* The identifier at o, if there is one: for the app, which proves a container's
   id an identifier (CSS_IDENT) before it is handed to the walk *)
#pub fun book_ident_scan {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat | o <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o): [k:nat | o + k <= len] (IDENT_OPT(cs, o, k) | int k)

implement book_ident_scan {l}{n}{cs}{len}{o} (src, len, o) = _scan_ident(src, len, o)

(* The white space that goes on from o + k *)
fun _spaces_from {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat}{k:nat | o + k <= len} .<len - o - k>.
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o, k: int k, pf: SPACES_RUN(cs, o, k))
  : [k2:nat | k <= k2; o + k2 <= len] (SPACES_RUN(cs, o, k2) | int k2) =
  if o + k >= len then (pf | k)
  else let
    val @(pn | c) = _byte(src, o + k)
    val @(pc | b) = _ws_class(c)
  in
    if b then let
      prval WS_CLASS_yes(ch) = pc
    in _spaces_from(src, len, o, k + 1, SPACES_RUN_snoc(pf, pn, ch)) end
    else let
      prval WS_CLASS_no() = pc
    in (pf | k) end
  end

fn _scan_spaces {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat | o <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o)
  : [k:nat | o + k <= len] (SPACES_RUN(cs, o, k) | int k) =
  _spaces_from(src, len, o, 0, SPACES_RUN_nil())

fun _digits_from {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat}{k:nat | o + k <= len} .<len - o - k>.
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o, k: int k, pf: DIGITS_RUN(cs, o, k))
  : [k2:nat | k <= k2; o + k2 <= len] (DIGITS_RUN(cs, o, k2) | int k2) =
  if o + k >= len then (pf | k)
  else let
    val @(pn | c) = _byte(src, o + k)
    val @(pc | b) = _digit_class(c)
  in
    if b then let
      prval DIGIT_CLASS_yes(ch) = pc
    in _digits_from(src, len, o, k + 1, DIGITS_RUN_snoc(pf, pn, ch)) end
    else let
      prval DIGIT_CLASS_no() = pc
    in (pf | k) end
  end

fn _scan_digits {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat | o <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o)
  : [k:nat | o + k <= len] (DIGITS_RUN(cs, o, k) | int k) =
  _digits_from(src, len, o, 0, DIGITS_RUN_nil())

fun _hex_from {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat}{k:nat | o + k <= len} .<len - o - k>.
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o, k: int k, pf: HEX_RUN(cs, o, k))
  : [k2:nat | k <= k2; o + k2 <= len] (HEX_RUN(cs, o, k2) | int k2) =
  if o + k >= len then (pf | k)
  else let
    val @(pn | c) = _byte(src, o + k)
    val @(pc | b) = _hex_class(c)
  in
    if b then let
      prval HEX_CLASS_yes(ch) = pc
    in _hex_from(src, len, o, k + 1, HEX_RUN_snoc(pf, pn, ch)) end
    else let
      prval HEX_CLASS_no() = pc
    in (pf | k) end
  end

fn _scan_hex {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat | o <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o)
  : [k:nat | o + k <= len] (HEX_RUN(cs, o, k) | int k) =
  _hex_from(src, len, o, 0, HEX_RUN_nil())

(* ============================================================
   Scanning: numbers and strings
   ============================================================ *)

dataprop UNSIGNED_MAYBE(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int} UNSIGNED_MAYBE_none(cs, o, 0)
  | {cs:$A.cells}{o:int}{m:pos} UNSIGNED_MAYBE_some(cs, o, m) of NUMBER_UNSIGNED(cs, o, m)

(* The unsigned number at p: at most 6 integer digits and 4 decimals *)
fn _scan_unsigned {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{p:nat | p <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, p: int p)
  : [m:nat | p + m <= len] (UNSIGNED_MAYBE(cs, p, m) | int m) = let
  val @(pi | i) = _scan_digits(src, len, p)
in
  if i >= 1 then
    (if i <= 6 then
       (if p + i >= len then (UNSIGNED_MAYBE_some(NUMBER_whole(pi)) | i)
        else let
          val @(pd | c) = _byte(src, p + i)
        in
          if c = 46 then let
            val @(pf | f) = _scan_digits(src, len, p + i + 1)
          in
            if f >= 1 then
              (if f <= 4 then (UNSIGNED_MAYBE_some(NUMBER_fraction(pi, pd, pf)) | i + 1 + f)
               else (UNSIGNED_MAYBE_none() | 0))
            else (UNSIGNED_MAYBE_none() | 0)
          end
          else (UNSIGNED_MAYBE_some(NUMBER_whole(pi)) | i)
        end)
     else (UNSIGNED_MAYBE_none() | 0))
  else if p >= len then (UNSIGNED_MAYBE_none() | 0)
  else let
    val @(pd | c) = _byte(src, p)
  in
    if c = 46 then let
      val @(pf | f) = _scan_digits(src, len, p + 1)
    in
      if f >= 1 then
        (if f <= 4 then (UNSIGNED_MAYBE_some(NUMBER_dot(pd, pf)) | 1 + f)
         else (UNSIGNED_MAYBE_none() | 0))
      else (UNSIGNED_MAYBE_none() | 0)
    end
    else (UNSIGNED_MAYBE_none() | 0)
  end
end

dataprop NUMBER_MAYBE(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int} NUMBER_MAYBE_none(cs, o, 0)
  | {cs:$A.cells}{o:int}{m:pos} NUMBER_MAYBE_some(cs, o, m) of NUMBER(cs, o, m)

(* The number at o, signed or not *)
fn _scan_number {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat | o <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o)
  : [m:nat | o + m <= len] (NUMBER_MAYBE(cs, o, m) | int m) =
  if o >= len then (NUMBER_MAYBE_none() | 0)
  else let
    val @(ps | c) = _byte(src, o)
  in
    if c = 43 then let
      val @(pu | m) = _scan_unsigned(src, len, o + 1)
    in
      if m >= 1 then let
        prval UNSIGNED_MAYBE_some(u) = pu
      in (NUMBER_MAYBE_some(NUMBER_plus(ps, u)) | m + 1) end
      else let prval UNSIGNED_MAYBE_none() = pu in (NUMBER_MAYBE_none() | 0) end
    end
    else if c = 45 then let
      val @(pu | m) = _scan_unsigned(src, len, o + 1)
    in
      if m >= 1 then let
        prval UNSIGNED_MAYBE_some(u) = pu
      in (NUMBER_MAYBE_some(NUMBER_minus(ps, u)) | m + 1) end
      else let prval UNSIGNED_MAYBE_none() = pu in (NUMBER_MAYBE_none() | 0) end
    end
    else let
      val @(pu | m) = _scan_unsigned(src, len, o)
    in
      if m >= 1 then let
        prval UNSIGNED_MAYBE_some(u) = pu
      in (NUMBER_MAYBE_some(NUMBER_plain(u)) | m) end
      else let prval UNSIGNED_MAYBE_none() = pu in (NUMBER_MAYBE_none() | 0) end
    end
  end

dataprop PLAIN_CLASS(c: int, b: bool) =
  | {c:int} PLAIN_CLASS_yes(c, true) of STR_CH_PLAIN(c)
  | {c:int} PLAIN_CLASS_no(c, false)

fn _plain_class {c:int | 0 <= c; c < 256} (c: int c): [b:bool] (PLAIN_CLASS(c, b) | bool b) =
  if c = 32 then (PLAIN_CLASS_yes(STR_CH_PLAIN_space()) | true)
  else if c >= 128 then (PLAIN_CLASS_yes(STR_CH_PLAIN_high()) | true)
  else if c >= 124 then
    (if c = 124 then (PLAIN_CLASS_yes(STR_CH_PLAIN_bar()) | true)
     else if c = 126 then (PLAIN_CLASS_yes(STR_CH_PLAIN_tilde()) | true)
     else (PLAIN_CLASS_no() | false))
  else if c >= 93 then
    (if c <= 94 then (PLAIN_CLASS_yes(STR_CH_PLAIN_square()) | true)
     else if c = 96 then (PLAIN_CLASS_yes(STR_CH_PLAIN_tick()) | true)
     else (PLAIN_CLASS_no() | false))
  else if c = 91 then (PLAIN_CLASS_yes(STR_CH_PLAIN_open_square()) | true)
  else if c >= 61 then
    (if c <= 63 then (PLAIN_CLASS_yes(STR_CH_PLAIN_compare()) | true) else (PLAIN_CLASS_no() | false))
  else if c >= 58 then
    (if c <= 59 then (PLAIN_CLASS_yes(STR_CH_PLAIN_colon()) | true) else (PLAIN_CLASS_no() | false))
  else if c = 46 then (PLAIN_CLASS_yes(STR_CH_PLAIN_dot()) | true)
  else if c >= 43 then
    (if c <= 44 then (PLAIN_CLASS_yes(STR_CH_PLAIN_comma()) | true) else (PLAIN_CLASS_no() | false))
  else if c = 41 then (PLAIN_CLASS_yes(STR_CH_PLAIN_close()) | true)
  else if c >= 35 then
    (if c <= 38 then (PLAIN_CLASS_yes(STR_CH_PLAIN_symbol()) | true) else (PLAIN_CLASS_no() | false))
  else (PLAIN_CLASS_no() | false)

dataprop STRING_MAYBE(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int} STRING_MAYBE_none(cs, o, 0)
  | {cs:$A.cells}{o:int}{m:pos} STRING_MAYBE_some(cs, o, m) of STRING(cs, o, m)

(* The body of the string whose opening quote q is at o, from byte o + 1 + k:
   last is how the byte before ended (STR_BODY) *)
fun _string_body {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat}{q:int | q == 34 || q == 39}
  {k:nat | o + 1 + k <= len}{last:int} .<len - o - k>.
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o, q: int q, po: $A.NTH(cs, o, q), k: int k, last: int last,
   pf: STR_BODY(cs, o + 1, k, q, last))
  : [m:nat | o + m <= len] (STRING_MAYBE(cs, o, m) | int m) =
  if o + 1 + k >= len then (STRING_MAYBE_none() | 0)
  else let
    val @(pn | c) = _byte(src, o + 1 + k)
  in
    if c = q then
      (if q = 34 then (STRING_MAYBE_some(STRING_double(po, pf, pn)) | k + 2)
       else (STRING_MAYBE_some(STRING_single(po, pf, pn)) | k + 2))
    else let
      val @(pi | bi) = _ident_class(c)
    in
      if bi then let
        prval IDENT_CLASS_yes(ch) = pi
      in _string_body(src, len, o, q, po, k + 1, 1, STR_BODY_ident(pf, pn, ch)) end
      else if c = 47 then
        _string_body(src, len, o, q, po, k + 1, 2, STR_BODY_slash(pf, pn))
      else if c = 40 then
        (if last = 1 then (STRING_MAYBE_none() | 0)
         else if last = 0 then _string_body(src, len, o, q, po, k + 1, 0, STR_BODY_paren_after_other(pf, pn))
         else if last = 2 then _string_body(src, len, o, q, po, k + 1, 0, STR_BODY_paren_after_slash(pf, pn))
         else (STRING_MAYBE_none() | 0))
      else if c = 42 then
        (if last = 2 then (STRING_MAYBE_none() | 0)
         else if last = 0 then _string_body(src, len, o, q, po, k + 1, 0, STR_BODY_star_after_other(pf, pn))
         else if last = 1 then _string_body(src, len, o, q, po, k + 1, 0, STR_BODY_star_after_ident(pf, pn))
         else (STRING_MAYBE_none() | 0))
      else if c = 39 then
        (if q = 34 then _string_body(src, len, o, q, po, k + 1, 0, STR_BODY_single_in_double(pf, pn))
         else (STRING_MAYBE_none() | 0))
      else if c = 34 then
        (if q = 39 then _string_body(src, len, o, q, po, k + 1, 0, STR_BODY_double_in_single(pf, pn))
         else (STRING_MAYBE_none() | 0))
      else let
        val @(pp | bp) = _plain_class(c)
      in
        if bp then let
          prval PLAIN_CLASS_yes(cl) = pp
        in _string_body(src, len, o, q, po, k + 1, 0, STR_BODY_plain(pf, pn, cl)) end
        else let
          prval PLAIN_CLASS_no() = pp
        in (STRING_MAYBE_none() | 0) end
      end
    end
  end

(* The string at o, if there is one that is safe *)
fn _scan_string {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{o:nat | o <= len}
  (src: !$A.cborrow(l, n, cs), len: int len, o: int o)
  : [m:nat | o + m <= len] (STRING_MAYBE(cs, o, m) | int m) =
  if o >= len then (STRING_MAYBE_none() | 0)
  else let
    val @(po | c) = _byte(src, o)
  in
    if c = 34 then _string_body(src, len, o, 34, po, 0, 0, STR_BODY_nil())
    else if c = 39 then _string_body(src, len, o, 39, po, 0, 0, STR_BODY_nil())
    else (STRING_MAYBE_none() | 0)
  end

(* ============================================================
   Names: comparing a span with a literal
   ============================================================ *)

fn _slen {m:nat} (s: string(m)): int(m) = g1u2i(string1_length(s))

fn _lower {c:int | 0 <= c; c < 256} (c: int c): [v:int | 0 <= v; v < 256] int v =
  if c >= 65 then (if c <= 90 then c + 32 else c) else c

fun _span_is_from {l:agz}{n:nat}{cs:$A.cells}{o:nat}{m:nat | o + m <= n}{i:nat | i <= m} .<m - i>.
  (src: !$A.cborrow(l, n, cs), o: int o, lit: string(m), m: int m, i: int i): bool =
  if i >= m then true
  else let
    val @(pn | c) = _byte(src, o + i)
  in
    if _lower(c) = char2int0(string_get_at(lit, i)) then _span_is_from(src, o, lit, m, i + 1)
    else false
  end

(* Whether the k bytes from o are the literal (lower case), without regard to case *)
fn _span_is {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}{m:nat}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, lit: string(m)): bool =
  if k = _slen(lit) then _span_is_from(src, o, lit, k, 0) else false

(* ============================================================
   The allow-lists: the closed tables of properties, functions, units and
   keywords. A value token is kept only when its class is in the mask of the
   property in the layout (the grammar g: 1 reflow, 2 fixed, 3 SVG paint).
   ============================================================ *)

(* The classes of a value's tokens, as bits of a mask *)
#define M_IDENT 1
#define M_NUM 2
#define M_ABS 4
#define M_EM 8
#define M_VIEW 16
#define M_PCT 32
#define M_ANGLE 64
#define M_TIME 128
#define M_FR 256
#define M_STRING 512
#define M_HEX 1024
#define M_CALC 2048
#define M_FN_COLOR 4096
#define M_FN_GRAD 8192
#define M_FN_COUNTER 16384
#define M_FN_TRANSFORM 32768
#define M_FRAGMENT 65536
#define M_RES 131072
(* The checks of a property that is more than its tokens *)
#define M_DISPLAY 1048576
#define M_POSITION 2097152
#define M_ZINDEX 4194304
#define M_FAMILY 8388608
(* The reflow text colour, which only the proof of its contrast keeps (P5) *)
#define M_TEXT_COLOR 16777216

(* Keywords only *)
#define K_KW 1
(* Keywords and numbers *)
#define K_NUMKW 3
(* Keywords and percentages *)
#define K_PCTKW 33
(* Lengths: keywords, numbers, absolute and relative units, percentages, calc *)
#define K_LEN 2111
(* A font size in reflow: no px, no viewport units *)
#define K_EMSIZE 2091
(* Keywords and strings *)
#define K_STR 513
#define K_CONTENT 16897
(* Colours: keywords, hexadecimal, rgb/hsl *)
#define K_COLOR 5121
#define K_BGIMAGE 15487
#define K_SHADOW 7199
#define K_GRID 2367
#define K_TRANSFORM 34943
#define K_OPACITY 35
#define K_PAINT 70657
#define K_MARKER 65537
#define K_DASH 47

(* The number of rows of the table of properties *)
#define PROP_ROWS 162

(* The name of the property of row p *)
fn _prop_name (p: int): [m:nat] string(m) =
  case+ p of
  | 0 => "color"
  | 1 => "font-style"
  | 2 => "font-weight"
  | 3 => "font-variant"
  | 4 => "font-variant-caps"
  | 5 => "font-variant-numeric"
  | 6 => "font-variant-east-asian"
  | 7 => "font-stretch"
  | 8 => "font-size"
  | 9 => "font-family"
  | 10 => "line-height"
  | 11 => "letter-spacing"
  | 12 => "word-spacing"
  | 13 => "text-align"
  | 14 => "text-align-last"
  | 15 => "text-indent"
  | 16 => "text-transform"
  | 17 => "text-decoration-line"
  | 18 => "text-decoration-style"
  | 19 => "text-decoration-thickness"
  | 20 => "text-underline-offset"
  | 21 => "white-space"
  | 22 => "word-break"
  | 23 => "overflow-wrap"
  | 24 => "line-break"
  | 25 => "hyphens"
  | 26 => "vertical-align"
  | 27 => "unicode-bidi"
  | 28 => "direction"
  | 29 => "quotes"
  | 30 => "content"
  | 31 => "text-combine-upright"
  | 32 => "text-emphasis-style"
  | 33 => "text-emphasis-position"
  | 34 => "ruby-position"
  | 35 => "writing-mode"
  | 36 => "text-orientation"
  | 37 => "display"
  | 38 => "margin-top"
  | 39 => "margin-bottom"
  | 40 => "margin-left"
  | 41 => "margin-right"
  | 42 => "margin-block-start"
  | 43 => "margin-block-end"
  | 44 => "margin-inline-start"
  | 45 => "margin-inline-end"
  | 46 => "margin"
  | 47 => "padding-top"
  | 48 => "padding-bottom"
  | 49 => "padding-left"
  | 50 => "padding-right"
  | 51 => "padding"
  | 52 => "border-width"
  | 53 => "border-top-width"
  | 54 => "border-right-width"
  | 55 => "border-bottom-width"
  | 56 => "border-left-width"
  | 57 => "border-style"
  | 58 => "border-top-style"
  | 59 => "border-right-style"
  | 60 => "border-bottom-style"
  | 61 => "border-left-style"
  | 62 => "border-radius"
  | 63 => "width"
  | 64 => "height"
  | 65 => "min-width"
  | 66 => "min-height"
  | 67 => "max-width"
  | 68 => "max-height"
  | 69 => "box-sizing"
  | 70 => "float"
  | 71 => "clear"
  | 72 => "overflow"
  | 73 => "visibility"
  | 74 => "list-style-type"
  | 75 => "list-style-position"
  | 76 => "border-collapse"
  | 77 => "border-spacing"
  | 78 => "caption-side"
  | 79 => "empty-cells"
  | 80 => "table-layout"
  | 81 => "break-before"
  | 82 => "break-after"
  | 83 => "break-inside"
  | 84 => "page-break-before"
  | 85 => "page-break-after"
  | 86 => "page-break-inside"
  | 87 => "orphans"
  | 88 => "widows"
  | 89 => "position"
  | 90 => "top"
  | 91 => "left"
  | 92 => "right"
  | 93 => "bottom"
  | 94 => "z-index"
  | 95 => "flex"
  | 96 => "flex-direction"
  | 97 => "flex-wrap"
  | 98 => "flex-grow"
  | 99 => "flex-shrink"
  | 100 => "flex-basis"
  | 101 => "gap"
  | 102 => "row-gap"
  | 103 => "column-gap"
  | 104 => "justify-content"
  | 105 => "justify-items"
  | 106 => "justify-self"
  | 107 => "align-content"
  | 108 => "align-items"
  | 109 => "align-self"
  | 110 => "order"
  | 111 => "grid-template-columns"
  | 112 => "grid-template-rows"
  | 113 => "grid-area"
  | 114 => "grid-row"
  | 115 => "grid-column"
  | 116 => "object-fit"
  | 117 => "object-position"
  | 118 => "aspect-ratio"
  | 119 => "inset"
  | 120 => "transform"
  | 121 => "transform-origin"
  | 122 => "opacity"
  | 123 => "column-count"
  | 124 => "column-width"
  | 125 => "column-rule-width"
  | 126 => "column-rule-style"
  | 127 => "color"
  | 128 => "background-color"
  | 129 => "background-image"
  | 130 => "background-position"
  | 131 => "background-size"
  | 132 => "background-repeat"
  | 133 => "border-color"
  | 134 => "border-top-color"
  | 135 => "border-right-color"
  | 136 => "border-bottom-color"
  | 137 => "border-left-color"
  | 138 => "outline-color"
  | 139 => "outline-style"
  | 140 => "outline-width"
  | 141 => "text-decoration-color"
  | 142 => "text-emphasis-color"
  | 143 => "column-rule-color"
  | 144 => "box-shadow"
  | 145 => "text-shadow"
  | 146 => "fill"
  | 147 => "stroke"
  | 148 => "stroke-width"
  | 149 => "fill-opacity"
  | 150 => "fill-rule"
  | 151 => "stroke-opacity"
  | 152 => "stroke-dasharray"
  | 153 => "stroke-dashoffset"
  | 154 => "stroke-linecap"
  | 155 => "stroke-linejoin"
  | 156 => "stroke-miterlimit"
  | 157 => "stop-color"
  | 158 => "stop-opacity"
  | 159 => "marker-start"
  | 160 => "marker-mid"
  | 161 => "marker-end"
  | _ => ""

(* The mask of row p in the grammar g (0 when the layout's list does not
   have the property): reflow, fixed layout, SVG paint. Every reflow entry is
   a fixed entry too, except font-size (no px) and color (only P5). *)
fn _prop_mask (p: int, g: int): int =
  case+ p of
  | 0 => (if g = 1 then M_TEXT_COLOR else 0)
  | 1 => (if g = 3 then 0 else K_KW)
  | 2 => (if g = 3 then 0 else K_NUMKW)
  | 3 => (if g = 3 then 0 else K_KW)
  | 4 => (if g = 3 then 0 else K_KW)
  | 5 => (if g = 3 then 0 else K_KW)
  | 6 => (if g = 3 then 0 else K_KW)
  | 7 => (if g = 3 then 0 else K_PCTKW)
  | 8 => (if g = 1 then K_EMSIZE else if g = 2 then K_LEN else 0)
  | 9 => (if g = 3 then 0 else K_STR + M_FAMILY)
  | 10 => (if g = 3 then 0 else K_LEN)
  | 11 => (if g = 3 then 0 else K_LEN)
  | 12 => (if g = 3 then 0 else K_LEN)
  | 13 => (if g = 3 then 0 else K_KW)
  | 14 => (if g = 3 then 0 else K_KW)
  | 15 => (if g = 3 then 0 else K_LEN)
  | 16 => (if g = 3 then 0 else K_KW)
  | 17 => (if g = 3 then 0 else K_KW)
  | 18 => (if g = 3 then 0 else K_KW)
  | 19 => (if g = 3 then 0 else K_LEN)
  | 20 => (if g = 3 then 0 else K_LEN)
  | 21 => (if g = 3 then 0 else K_KW)
  | 22 => (if g = 3 then 0 else K_KW)
  | 23 => (if g = 3 then 0 else K_KW)
  | 24 => (if g = 3 then 0 else K_KW)
  | 25 => (if g = 3 then 0 else K_KW)
  | 26 => (if g = 3 then 0 else K_LEN)
  | 27 => (if g = 3 then 0 else K_KW)
  | 28 => (if g = 3 then 0 else K_KW)
  | 29 => (if g = 3 then 0 else K_STR)
  | 30 => (if g = 3 then 0 else K_CONTENT)
  | 31 => (if g = 3 then 0 else K_NUMKW)
  | 32 => (if g = 3 then 0 else K_STR)
  | 33 => (if g = 3 then 0 else K_KW)
  | 34 => (if g = 3 then 0 else K_KW)
  | 35 => (if g = 2 then K_KW else 0)
  | 36 => (if g = 2 then K_KW else 0)
  | 37 => K_KW + M_DISPLAY
  | 38 => (if g = 3 then 0 else K_LEN)
  | 39 => (if g = 3 then 0 else K_LEN)
  | 40 => (if g = 3 then 0 else K_LEN)
  | 41 => (if g = 3 then 0 else K_LEN)
  | 42 => (if g = 3 then 0 else K_LEN)
  | 43 => (if g = 3 then 0 else K_LEN)
  | 44 => (if g = 3 then 0 else K_LEN)
  | 45 => (if g = 3 then 0 else K_LEN)
  | 46 => (if g = 3 then 0 else K_LEN)
  | 47 => (if g = 3 then 0 else K_LEN)
  | 48 => (if g = 3 then 0 else K_LEN)
  | 49 => (if g = 3 then 0 else K_LEN)
  | 50 => (if g = 3 then 0 else K_LEN)
  | 51 => (if g = 3 then 0 else K_LEN)
  | 52 => (if g = 3 then 0 else K_LEN)
  | 53 => (if g = 3 then 0 else K_LEN)
  | 54 => (if g = 3 then 0 else K_LEN)
  | 55 => (if g = 3 then 0 else K_LEN)
  | 56 => (if g = 3 then 0 else K_LEN)
  | 57 => (if g = 3 then 0 else K_KW)
  | 58 => (if g = 3 then 0 else K_KW)
  | 59 => (if g = 3 then 0 else K_KW)
  | 60 => (if g = 3 then 0 else K_KW)
  | 61 => (if g = 3 then 0 else K_KW)
  | 62 => (if g = 3 then 0 else K_LEN)
  | 63 => (if g = 3 then 0 else K_LEN)
  | 64 => (if g = 3 then 0 else K_LEN)
  | 65 => (if g = 3 then 0 else K_LEN)
  | 66 => (if g = 3 then 0 else K_LEN)
  | 67 => (if g = 3 then 0 else K_LEN)
  | 68 => (if g = 3 then 0 else K_LEN)
  | 69 => (if g = 3 then 0 else K_KW)
  | 70 => (if g = 3 then 0 else K_KW)
  | 71 => (if g = 3 then 0 else K_KW)
  | 72 => (if g = 3 then 0 else K_KW)
  | 73 => K_KW
  | 74 => (if g = 3 then 0 else K_STR)
  | 75 => (if g = 3 then 0 else K_KW)
  | 76 => (if g = 3 then 0 else K_KW)
  | 77 => (if g = 3 then 0 else K_LEN)
  | 78 => (if g = 3 then 0 else K_KW)
  | 79 => (if g = 3 then 0 else K_KW)
  | 80 => (if g = 3 then 0 else K_KW)
  | 81 => (if g = 3 then 0 else K_KW)
  | 82 => (if g = 3 then 0 else K_KW)
  | 83 => (if g = 3 then 0 else K_KW)
  | 84 => (if g = 3 then 0 else K_KW)
  | 85 => (if g = 3 then 0 else K_KW)
  | 86 => (if g = 3 then 0 else K_KW)
  | 87 => (if g = 3 then 0 else K_NUMKW)
  | 88 => (if g = 3 then 0 else K_NUMKW)
  | 89 => (if g = 3 then 0 else K_KW + M_POSITION)
  | 90 => (if g = 3 then 0 else K_LEN)
  | 91 => (if g = 3 then 0 else K_LEN)
  | 92 => (if g = 3 then 0 else K_LEN)
  | 93 => (if g = 3 then 0 else K_LEN)
  | 94 => (if g = 3 then 0 else K_NUMKW + M_ZINDEX)
  | 95 => (if g = 3 then 0 else K_LEN)
  | 96 => (if g = 3 then 0 else K_KW)
  | 97 => (if g = 3 then 0 else K_KW)
  | 98 => (if g = 3 then 0 else K_NUMKW)
  | 99 => (if g = 3 then 0 else K_NUMKW)
  | 100 => (if g = 3 then 0 else K_LEN)
  | 101 => (if g = 3 then 0 else K_LEN)
  | 102 => (if g = 3 then 0 else K_LEN)
  | 103 => (if g = 3 then 0 else K_LEN)
  | 104 => (if g = 3 then 0 else K_KW)
  | 105 => (if g = 3 then 0 else K_KW)
  | 106 => (if g = 3 then 0 else K_KW)
  | 107 => (if g = 3 then 0 else K_KW)
  | 108 => (if g = 3 then 0 else K_KW)
  | 109 => (if g = 3 then 0 else K_KW)
  | 110 => (if g = 3 then 0 else K_NUMKW)
  | 111 => (if g = 3 then 0 else K_GRID)
  | 112 => (if g = 3 then 0 else K_GRID)
  | 113 => (if g = 3 then 0 else K_NUMKW)
  | 114 => (if g = 3 then 0 else K_NUMKW)
  | 115 => (if g = 3 then 0 else K_NUMKW)
  | 116 => (if g = 3 then 0 else K_KW)
  | 117 => (if g = 3 then 0 else K_LEN)
  | 118 => (if g = 3 then 0 else K_NUMKW)
  | 119 => (if g = 2 then K_LEN else 0)
  | 120 => (if g = 2 then K_TRANSFORM else 0)
  | 121 => (if g = 2 then K_LEN else 0)
  | 122 => (if g = 1 then 0 else K_OPACITY)
  | 123 => (if g = 2 then K_NUMKW else 0)
  | 124 => (if g = 2 then K_LEN else 0)
  | 125 => (if g = 2 then K_LEN else 0)
  | 126 => (if g = 2 then K_KW else 0)
  | 127 => (if g = 2 then K_COLOR else 0)
  | 128 => (if g = 2 then K_COLOR else 0)
  | 129 => (if g = 2 then K_BGIMAGE else 0)
  | 130 => (if g = 2 then K_LEN else 0)
  | 131 => (if g = 2 then K_LEN else 0)
  | 132 => (if g = 2 then K_KW else 0)
  | 133 => (if g = 2 then K_COLOR else 0)
  | 134 => (if g = 2 then K_COLOR else 0)
  | 135 => (if g = 2 then K_COLOR else 0)
  | 136 => (if g = 2 then K_COLOR else 0)
  | 137 => (if g = 2 then K_COLOR else 0)
  | 138 => (if g = 2 then K_COLOR else 0)
  | 139 => (if g = 2 then K_KW else 0)
  | 140 => (if g = 2 then K_LEN else 0)
  | 141 => (if g = 2 then K_COLOR else 0)
  | 142 => (if g = 2 then K_COLOR else 0)
  | 143 => (if g = 2 then K_COLOR else 0)
  | 144 => (if g = 2 then K_SHADOW else 0)
  | 145 => (if g = 2 then K_SHADOW else 0)
  | 146 => (if g = 2 then K_COLOR else if g = 3 then K_PAINT else 0)
  | 147 => (if g = 2 then K_COLOR else if g = 3 then K_PAINT else 0)
  | 148 => (if g = 1 then 0 else K_LEN)
  | 149 => (if g = 3 then K_OPACITY else 0)
  | 150 => (if g = 3 then K_KW else 0)
  | 151 => (if g = 3 then K_OPACITY else 0)
  | 152 => (if g = 3 then K_DASH else 0)
  | 153 => (if g = 3 then K_LEN else 0)
  | 154 => (if g = 3 then K_KW else 0)
  | 155 => (if g = 3 then K_KW else 0)
  | 156 => (if g = 3 then K_NUMKW else 0)
  | 157 => (if g = 3 then K_COLOR else 0)
  | 158 => (if g = 3 then K_OPACITY else 0)
  | 159 => (if g = 3 then K_MARKER else 0)
  | 160 => (if g = 3 then K_MARKER else 0)
  | 161 => (if g = 3 then K_MARKER else 0)
  | _ => 0

fn _has (mask: int, bit: int): bool = $AR.band_int_int(mask, bit) > 0

fun _str_eq_from {a:nat}{i:nat | i <= a} .<a - i>. (x: string(a), y: string(a), a: int a, i: int i): bool =
  if i >= a then true
  else if char2int0(string_get_at(x, i)) = char2int0(string_get_at(y, i)) then _str_eq_from(x, y, a, i + 1)
  else false

fn _str_eq {a,b:nat} (x: string(a), y: string(b)): bool =
  if _slen(x) = _slen(y) then _str_eq_from(x, y, _slen(x), 0) else false

(* The row of the property that the k bytes from o name in the grammar g, or
   ~1. -epub- names are mapped to the unprefixed property (the mapped ones
   only) and then looked for in the layout's list. *)
fun _find_from {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}{i:nat | i <= 162} .<162 - i>.
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, g: int, i: int i): [r:int | r >= ~1] int r =
  if i >= 162 then ~1
  else if _prop_mask(i, g) > 0 then
    (if _span_is(src, o, k, _prop_name(i)) then i else _find_from(src, o, k, g, i + 1))
  else _find_from(src, o, k, g, i + 1)

fun _find_str_from {a:nat}{i:nat | i <= 162} .<162 - i>. (name: string(a), g: int, i: int i): [r:int | r >= ~1] int r =
  if i >= 162 then ~1
  else if _prop_mask(i, g) > 0 then
    (if _str_eq(name, _prop_name(i)) then i else _find_str_from(name, g, i + 1))
  else _find_str_from(name, g, i + 1)

(* The property that the -epub- name whose rest is the k bytes from o maps to *)
fn _epub_target {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): [m:nat] string(m) =
  if _span_is(src, o, k, "hyphens") then "hyphens"
  else if _span_is(src, o, k, "line-break") then "line-break"
  else if _span_is(src, o, k, "text-align-last") then "text-align-last"
  else if _span_is(src, o, k, "text-combine") then "text-combine-upright"
  else if _span_is(src, o, k, "text-emphasis-style") then "text-emphasis-style"
  else if _span_is(src, o, k, "text-emphasis-position") then "text-emphasis-position"
  else if _span_is(src, o, k, "text-emphasis-color") then "text-emphasis-color"
  else if _span_is(src, o, k, "text-orientation") then "text-orientation"
  else if _span_is(src, o, k, "text-transform") then "text-transform"
  else if _span_is(src, o, k, "word-break") then "word-break"
  else if _span_is(src, o, k, "writing-mode") then "writing-mode"
  else ""

fn _prop_find {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, g: int): [r:int | r >= ~1] int r =
  if k > 6 then
    (if _span_is(src, o, 6, "-epub-") then
       (let val name = _epub_target(src, o + 6, k - 6) in _find_str_from(name, g, 0) end)
     else _find_from(src, o, k, g, 0))
  else _find_from(src, o, k, g, 0)

(* The class of a unit, as a mask bit (0 when the book's unit is not one) *)
fn _unit_bit {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): int =
  if _span_is(src, o, k, "px") then M_ABS
  else if _span_is(src, o, k, "pt") then M_ABS
  else if _span_is(src, o, k, "pc") then M_ABS
  else if _span_is(src, o, k, "cm") then M_ABS
  else if _span_is(src, o, k, "mm") then M_ABS
  else if _span_is(src, o, k, "in") then M_ABS
  else if _span_is(src, o, k, "em") then M_EM
  else if _span_is(src, o, k, "rem") then M_EM
  else if _span_is(src, o, k, "ex") then M_EM
  else if _span_is(src, o, k, "ch") then M_EM
  else if _span_is(src, o, k, "vw") then M_VIEW
  else if _span_is(src, o, k, "vh") then M_VIEW
  else if _span_is(src, o, k, "vmin") then M_VIEW
  else if _span_is(src, o, k, "vmax") then M_VIEW
  else if _span_is(src, o, k, "deg") then M_ANGLE
  else if _span_is(src, o, k, "rad") then M_ANGLE
  else if _span_is(src, o, k, "turn") then M_ANGLE
  else if _span_is(src, o, k, "s") then M_TIME
  else if _span_is(src, o, k, "ms") then M_TIME
  else if _span_is(src, o, k, "fr") then M_FR
  else if _span_is(src, o, k, "dpi") then M_RES
  else if _span_is(src, o, k, "dpcm") then M_RES
  else 0

(* The class of a function, as a mask bit (0 when the function is refused:
   url, image-set, image, src, cross-fade, element, paint, env, var, attr,
   expression, and every other that is not here) *)
fn _func_bit {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): int =
  if _span_is(src, o, k, "calc") then M_CALC
  else if _span_is(src, o, k, "rgb") then M_FN_COLOR
  else if _span_is(src, o, k, "rgba") then M_FN_COLOR
  else if _span_is(src, o, k, "hsl") then M_FN_COLOR
  else if _span_is(src, o, k, "hsla") then M_FN_COLOR
  else if _span_is(src, o, k, "counter") then M_FN_COUNTER
  else if _span_is(src, o, k, "counters") then M_FN_COUNTER
  else if _span_is(src, o, k, "linear-gradient") then M_FN_GRAD
  else if _span_is(src, o, k, "radial-gradient") then M_FN_GRAD
  else if _span_is(src, o, k, "repeating-linear-gradient") then M_FN_GRAD
  else if _span_is(src, o, k, "repeating-radial-gradient") then M_FN_GRAD
  else if _span_is(src, o, k, "translate") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "translatex") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "translatey") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "scale") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "scalex") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "scaley") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "rotate") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "skew") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "skewx") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "skewy") then M_FN_TRANSFORM
  else if _span_is(src, o, k, "matrix") then M_FN_TRANSFORM
  else 0

(* The displays that a book may ask for *)
fn _display_ok {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): bool =
  if _span_is(src, o, k, "block") then true
  else if _span_is(src, o, k, "inline") then true
  else if _span_is(src, o, k, "inline-block") then true
  else if _span_is(src, o, k, "list-item") then true
  else if _span_is(src, o, k, "none") then true
  else if _span_is(src, o, k, "contents") then true
  else if _span_is(src, o, k, "flow-root") then true
  else if _span_is(src, o, k, "table") then true
  else if _span_is(src, o, k, "inline-table") then true
  else if _span_is(src, o, k, "table-row") then true
  else if _span_is(src, o, k, "table-cell") then true
  else if _span_is(src, o, k, "table-row-group") then true
  else if _span_is(src, o, k, "table-header-group") then true
  else if _span_is(src, o, k, "table-footer-group") then true
  else if _span_is(src, o, k, "table-column") then true
  else if _span_is(src, o, k, "table-column-group") then true
  else if _span_is(src, o, k, "table-caption") then true
  else if _span_is(src, o, k, "flex") then true
  else if _span_is(src, o, k, "inline-flex") then true
  else if _span_is(src, o, k, "grid") then true
  else if _span_is(src, o, k, "inline-grid") then true
  else false

(* position is static or relative in reflow, and also absolute in fixed layout (P4) *)
fn _position_ok {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, g: int): bool =
  if _span_is(src, o, k, "static") then true
  else if _span_is(src, o, k, "relative") then true
  else if g = 2 then _span_is(src, o, k, "absolute")
  else false

fn _zindex_digits {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}{s:nat | s <= k}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, s: int s): bool =
  if k - s >= 1 then
    (if k - s <= 4 then let
       val @(pr | m) = _scan_digits(src, o + k, o + s)
     in m = k - s end
     else false)
  else false

(* z-index has at most 4 digits (P4), or is auto *)
fn _zindex_ok {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): bool =
  if _span_is(src, o, k, "auto") then true
  else if k = 0 then false
  else let
    val @(pd | c) = _byte(src, o)
  in
    if c = 45 then _zindex_digits(src, o, k, 1)
    else if c = 43 then _zindex_digits(src, o, k, 1)
    else _zindex_digits(src, o, k, 0)
  end

dataprop FRAGMENT_MAYBE(cs: $A.cells, o: int, m: int) =
  | {cs:$A.cells}{o:int} FRAGMENT_MAYBE_none(cs, o, 0)
  | {cs:$A.cells}{o:int}{m:pos} FRAGMENT_MAYBE_some(cs, o, m) of FRAGMENT(cs, o, m)

(* The url(#id) at o *)
fn _scan_fragment {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat | o <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o)
  : [m:nat | o + m <= e] (FRAGMENT_MAYBE(cs, o, m) | int m) =
  if o + 6 >= e then (FRAGMENT_MAYBE_none() | 0)
  else let
    val @(p0 | c0) = _byte(src, o)
    val @(p1 | c1) = _byte(src, o + 1)
    val @(p2 | c2) = _byte(src, o + 2)
    val @(p3 | c3) = _byte(src, o + 3)
    val @(p4 | c4) = _byte(src, o + 4)
  in
    if c0 = 117 then
      (if c1 = 114 then
         (if c2 = 108 then
            (if c3 = 40 then
               (if c4 = 35 then let
                  val @(pi | a) = _scan_ident(src, e, o + 5)
                in
                  if a >= 1 then
                    (if o + 5 + a < e then let
                       val @(p5 | c5) = _byte(src, o + 5 + a)
                     in
                       if c5 = 41 then let
                         prval IDENT_OPT_some(id) = pi
                       in (FRAGMENT_MAYBE_some(FRAGMENT_mk(p0, p1, p2, p3, p4, id, p5)) | a + 6) end
                       else (FRAGMENT_MAYBE_none() | 0)
                     end
                     else (FRAGMENT_MAYBE_none() | 0))
                  else (FRAGMENT_MAYBE_none() | 0)
                end
                else (FRAGMENT_MAYBE_none() | 0))
             else (FRAGMENT_MAYBE_none() | 0))
          else (FRAGMENT_MAYBE_none() | 0))
       else (FRAGMENT_MAYBE_none() | 0))
    else (FRAGMENT_MAYBE_none() | 0)
  end

fn _is_letter (c: int): bool =
  if c >= 97 then c <= 122 else if c >= 65 then c <= 90 else false

(* A number has been read (m bytes at o + k, pf1 holds it): its unit, if it has
   one, as the mask allows (a unitless number is M_NUM, % is M_PCT, a unit
   word is its class). The bool is false when the value is to be dropped. *)
fn _after_number {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat}{m:pos | o + k + m <= e}
  {g,p,d:int}
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, m: int m, mask: int,
   pf1: VALUE_PARTIAL(cs, o, k + m, g, p, d))
  : [k2:nat | k + m <= k2; o + k2 <= e] (VALUE_PARTIAL(cs, o, k2, g, p, d) | int k2, bool) =
  if o + k + m >= e then (pf1 | k + m, _has(mask, M_NUM))
  else let
    val @(pn | c) = _byte(src, o + k + m)
  in
    if c = 37 then
      (if _has(mask, M_PCT) then (VP_percent(pf1, pn) | k + m + 1, true) else (pf1 | k + m, false))
    else if _is_letter(c) then let
      val @(pi | m2) = _scan_ident(src, e, o + k + m)
    in
      if m2 >= 1 then let
        prval IDENT_OPT_some(id) = pi
        val bit = _unit_bit(src, o + k + m, m2)
      in
        if bit > 0 then
          (if _has(mask, bit) then (VP_ident(pf1, id) | k + m + m2, true) else (pf1 | k + m, false))
        else (pf1 | k + m, false)
      end
      else let prval IDENT_OPT_none() = pi in (pf1 | k + m, false) end
    end
    else if c = 46 then (pf1 | k + m, false)
    else (pf1 | k + m, _has(mask, M_NUM))
  end

(* The tokens of a value from o + k to e, as the mask allows. The bool is false
   when a byte or a token is not allowed, and then the value is dropped; d is
   the depth of parentheses (at most 4). *)
(* The loop's answer: the whole value from o to e was read, holding d2 open parentheses, or
   it is dropped *)
dataprop LOOP_DONE(cs: $A.cells, o: int, e: int, g: int, p: int, ok: bool, d: int) =
  | {cs:$A.cells}{o,e:int}{g,p,d:int} LOOP_DONE_ok(cs, o, e, g, p, true, d) of VALUE_PARTIAL(cs, o, e - o, g, p, d)
  | {cs:$A.cells}{o,e:int}{g,p,d:int} LOOP_DONE_bad(cs, o, e, g, p, false, d)

fnx _value_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat | o + k <= e}
  {g:int}{p:int | p >= 1}{d:nat} .<e - o - k, 5>.
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, g: int g, p: int p, d: int d, mask: int,
   pf: VALUE_PARTIAL(cs, o, k, g, p, d))
  : [ok:bool][d2:nat] (LOOP_DONE(cs, o, e, g, p, ok, d2) | int d2, bool ok) =
  if o + k >= e then (LOOP_DONE_ok(pf) | d, true)
  else let
    val @(pn | c) = _byte(src, o + k)
    val @(pw | bw) = _ws_class(c)
  in
    if bw then let
      val @(ps | m) = _scan_spaces(src, e, o + k)
    in
      if m >= 1 then _value_loop(src, e, o, k + m, g, p, d, mask, VP_space(pf, ps))
      else (LOOP_DONE_bad() | d, false)
    end
    else if c = 44 then _value_loop(src, e, o, k + 1, g, p, d, mask, VP_comma(pf, pn))
    else if c = 47 then let
      (* a comment in a value would have to be cut out of its middle *)
      val comment = (if o + k + 1 < e then let val @(pc | c1) = _byte(src, o + k + 1) in c1 = 42 end else false)
    in
      if comment then (LOOP_DONE_bad() | d, false)
      else _value_loop(src, e, o, k + 1, g, p, d, mask, VP_slash(pf, pn))
    end
    else if c = 42 then _value_loop(src, e, o, k + 1, g, p, d, mask, VP_star(pf, pn))
    else if c = 40 then
      (if d >= 1 then
         (if d < 4 then _value_loop(src, e, o, k + 1, g, p, d + 1, mask, VP_paren(pf, pn))
          else (LOOP_DONE_bad() | d, false))
       else (LOOP_DONE_bad() | d, false))
    else if c = 41 then
      (if d >= 1 then _value_loop(src, e, o, k + 1, g, p, d - 1, mask, VP_close(pf, pn))
       else (LOOP_DONE_bad() | d, false))
    else if c = 35 then let
      val @(ph | h) = _scan_hex(src, e, o + k + 1)
      val glued = (if o + k + 1 + h < e then let
                     val @(pg | c1) = _byte(src, o + k + 1 + h)
                     val @(pgc | bg) = _ident_class(c1)
                   in bg end
                   else false)
    in
      if glued then (LOOP_DONE_bad() | d, false)
      else if _has(mask, M_HEX) then
        (if h = 3 then _value_loop(src, e, o, k + 4, g, p, d, mask, VP_hex(pf, HEX_COLOR_3(pn, ph)))
         else if h = 4 then _value_loop(src, e, o, k + 5, g, p, d, mask, VP_hex(pf, HEX_COLOR_4(pn, ph)))
         else if h = 6 then _value_loop(src, e, o, k + 7, g, p, d, mask, VP_hex(pf, HEX_COLOR_6(pn, ph)))
         else if h = 8 then _value_loop(src, e, o, k + 9, g, p, d, mask, VP_hex(pf, HEX_COLOR_8(pn, ph)))
         else (LOOP_DONE_bad() | d, false))
      else (LOOP_DONE_bad() | d, false)
    end
    else if c = 34 then _value_string(src, e, o, k, g, p, d, mask, pf)
    else if c = 39 then _value_string(src, e, o, k, g, p, d, mask, pf)
    else if c = 43 then _value_number(src, e, o, k, g, p, d, mask, pf)
    else if c = 45 then _value_number(src, e, o, k, g, p, d, mask, pf)
    else if c = 46 then _value_number(src, e, o, k, g, p, d, mask, pf)
    else if c >= 48 then
      (if c <= 57 then _value_number(src, e, o, k, g, p, d, mask, pf)
       else _value_ident(src, e, o, k, g, p, d, mask, pf))
    else (LOOP_DONE_bad() | d, false)
  end

(* A string at o + k *)
and _value_string {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat | o + k < e}
  {g:int}{p:int | p >= 1}{d:nat} .<e - o - k, 1>.
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, g: int g, p: int p, d: int d, mask: int,
   pf: VALUE_PARTIAL(cs, o, k, g, p, d))
  : [ok:bool][d2:nat] (LOOP_DONE(cs, o, e, g, p, ok, d2) | int d2, bool ok) = let
  val @(ps | m) = _scan_string(src, e, o + k)
in
  if m >= 1 then
    (if _has(mask, M_STRING) then let
       prval STRING_MAYBE_some(s) = ps
     in _value_loop(src, e, o, k + m, g, p, d, mask, VP_string(pf, s)) end
     else (LOOP_DONE_bad() | d, false))
  else (LOOP_DONE_bad() | d, false)
end

(* A number at o + k (a sign or a dot or a digit); a sign that no number follows
   is an operator or the start of an identifier *)
and _value_number {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat | o + k < e}
  {g:int}{p:int | p >= 1}{d:nat} .<e - o - k, 4>.
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, g: int g, p: int p, d: int d, mask: int,
   pf: VALUE_PARTIAL(cs, o, k, g, p, d))
  : [ok:bool][d2:nat] (LOOP_DONE(cs, o, e, g, p, ok, d2) | int d2, bool ok) = let
  val @(pm | m) = _scan_number(src, e, o + k)
in
  if m >= 1 then let
    prval NUMBER_MAYBE_some(num) = pm
    val @(pf2 | k2, ok) = _after_number(src, e, o, k, m, mask, VP_number(pf, num))
  in
    if ok then _value_loop(src, e, o, k2, g, p, d, mask, pf2) else (LOOP_DONE_bad() | d, false)
  end
  else let
    prval NUMBER_MAYBE_none() = pm
    val @(pp | cc) = _byte(src, o + k)
  in
    if cc = 43 then _value_loop(src, e, o, k + 1, g, p, d, mask, VP_plus(pf, pp))
    else _value_ident(src, e, o, k, g, p, d, mask, pf)
  end
end

(* An identifier or a function at o + k; a lone + is an operator *)
and _value_ident {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat | o + k < e}
  {g:int}{p:int | p >= 1}{d:nat} .<e - o - k, 3>.
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, g: int g, p: int p, d: int d, mask: int,
   pf: VALUE_PARTIAL(cs, o, k, g, p, d))
  : [ok:bool][d2:nat] (LOOP_DONE(cs, o, e, g, p, ok, d2) | int d2, bool ok) = let
  val @(pi | m) = _scan_ident(src, e, o + k)
in
  if m >= 1 then let
    prval IDENT_OPT_some(id) = pi
  in
    if o + k + m < e then let
      val @(pp | c) = _byte(src, o + k + m)
    in
      if c = 40 then
        (if _span_is(src, o + k, m, "url") then _value_fragment(src, e, o, k, g, p, d, mask, pf)
         else let
           val bit = _func_bit(src, o + k, m)
         in
           if bit > 0 then
             (if _has(mask, bit) then
                (if d < 4 then _value_loop(src, e, o, k + m + 1, g, p, d + 1, mask, VP_function(pf, id, pp))
                 else (LOOP_DONE_bad() | d, false))
              else (LOOP_DONE_bad() | d, false))
           else (LOOP_DONE_bad() | d, false)
         end)
      else if _has(mask, M_IDENT) then _value_loop(src, e, o, k + m, g, p, d, mask, VP_ident(pf, id))
      else (LOOP_DONE_bad() | d, false)
    end
    else if _has(mask, M_IDENT) then _value_loop(src, e, o, k + m, g, p, d, mask, VP_ident(pf, id))
    else (LOOP_DONE_bad() | d, false)
  end
  else let prval IDENT_OPT_none() = pi in (LOOP_DONE_bad() | d, false) end
end

(* url( at o + k: only url(#id) in the SVG paint grammar *)
and _value_fragment {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat | o + k < e}
  {g:int}{p:int | p >= 1}{d:nat} .<e - o - k, 2>.
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, g: int g, p: int p, d: int d, mask: int,
   pf: VALUE_PARTIAL(cs, o, k, g, p, d))
  : [ok:bool][d2:nat] (LOOP_DONE(cs, o, e, g, p, ok, d2) | int d2, bool ok) =
  if g = 3 then
    (if _has(mask, M_FRAGMENT) then let
       val @(pf1 | m) = _scan_fragment(src, e, o + k)
     in
       if m >= 1 then let
         prval FRAGMENT_MAYBE_some(fr) = pf1
       in _value_loop(src, e, o, k + m, 3, p, d, mask, VP_fragment(pf, fr)) end
       else (LOOP_DONE_bad() | d, false)
     end
     else (LOOP_DONE_bad() | d, false))
  else (LOOP_DONE_bad() | d, false)

(* ============================================================
   Proved text colour (P5)
   In reflow, a book's text colour is kept only when its contrast against
   both grounds the app passes (the Light and the Sepia ground) is at least 4.5:
   the proof is CONTRAST, from the luminance bounds that contrast.bats reads
   from its table at run time. The grounds are the app's, as channels.
   ============================================================ *)

#pub typedef rgb256 = [r,g,b:nat | r < 256; g < 256; b < 256] @(int r, int g, int b)

dataprop VALUE_MAYBE(cs: $A.cells, o: int, k: int, g: int, p: int, ok: bool) =
  | {cs:$A.cells}{o,k,g,p:int} VALUE_MAYBE_none(cs, o, k, g, p, false)
  | {cs:$A.cells}{o,k,g,p:int} VALUE_MAYBE_some(cs, o, k, g, p, true) of VALUE_SAFE(cs, o, k, g, p)

dataprop HEX_AT(cs: $A.cells, i: int, v: int, ok: bool) =
  | {cs:$A.cells}{i,v:int} HEX_AT_none(cs, i, v, false)
  | {cs:$A.cells}{i:int}{c,v:int} HEX_AT_some(cs, i, v, true) of ($A.NTH(cs, i, c), HEX_CH(c, v))

fn _hex_at {l:agz}{n:nat}{cs:$A.cells}{i:nat | i < n}
  (src: !$A.cborrow(l, n, cs), i: int i)
  : [v:int | 0 <= v; v <= 15][ok:bool] (HEX_AT(cs, i, v, ok) | int v, bool ok) = let
  val @(pn | c) = _byte(src, i)
in
  if c >= 97 then
    (if c <= 102 then (HEX_AT_some(pn, HEX_CH_lower()) | c - 87, true) else (HEX_AT_none() | 0, false))
  else if c >= 65 then
    (if c <= 70 then (HEX_AT_some(pn, HEX_CH_upper()) | c - 55, true) else (HEX_AT_none() | 0, false))
  else if c >= 48 then
    (if c <= 57 then (HEX_AT_some(pn, HEX_CH_digit()) | c - 48, true) else (HEX_AT_none() | 0, false))
  else (HEX_AT_none() | 0, false)
end

dataprop HEX_PAIR(cs: $A.cells, i: int, hi: int, lo: int, ok: bool) =
  | {cs:$A.cells}{i,hi,lo:int} HEX_PAIR_none(cs, i, hi, lo, false)
  | {cs:$A.cells}{i:int}{c1,c2,hi,lo:int}
    HEX_PAIR_some(cs, i, hi, lo, true) of ($A.NTH(cs, i, c1), HEX_CH(c1, hi), $A.NTH(cs, i + 1, c2), HEX_CH(c2, lo))

fn _hex_pair {l:agz}{n:nat}{cs:$A.cells}{i:nat | i + 1 < n}
  (src: !$A.cborrow(l, n, cs), i: int i)
  : [hi,lo:int | 0 <= hi; hi <= 15; 0 <= lo; lo <= 15][ok:bool]
    (HEX_PAIR(cs, i, hi, lo, ok) | int hi, int lo, bool ok) = let
  val @(d1 | h1, b1) = _hex_at(src, i)
in
  if b1 then let
    val @(d2 | h2, b2) = _hex_at(src, i + 1)
  in
    if b2 then let
      prval HEX_AT_some(n1, x1) = d1
      prval HEX_AT_some(n2, x2) = d2
    in (HEX_PAIR_some(n1, x1, n2, x2) | h1, h2, true) end
    else (HEX_PAIR_none() | 0, 0, false)
  end
  else (HEX_PAIR_none() | 0, 0, false)
end

(* #rrggbb at o *)
fn _text_color_6 {l:agz}{n:nat}{cs:$A.cells}{o:nat | o + 7 <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, light: rgb256, sepia: rgb256)
  : [ok:bool] (VALUE_MAYBE(cs, o, 7, 1, 0, ok) | bool ok) = let
  val @(p0 | c0) = _byte(src, o)
  val @(lr, lg, lb) = light
  val @(sr, sg, sb) = sepia
in
  if c0 = 35 then let
    val @(d1 | h1, h2, b1) = _hex_pair(src, o + 1)
  in
    if b1 then let
      val @(d2 | h3, h4, b2) = _hex_pair(src, o + 3)
    in
      if b2 then let
        val @(d3 | h5, h6, b3) = _hex_pair(src, o + 5)
      in
        if b3 then let
          prval HEX_PAIR_some(n1, x1, n2, x2) = d1
          prval HEX_PAIR_some(n3, x3, n4, x4) = d2
          prval HEX_PAIR_some(n5, x5, n6, x6) = d3
          val r = h1 * 16 + h2
          val g = h3 * 16 + h4
          val b = h5 * 16 + h6
          val @(pl | okl) = contrast_45_of(r, g, b, lr, lg, lb)
        in
          if okl then let
            val @(ps | oks) = contrast_45_of(r, g, b, sr, sg, sb)
          in
            if oks then let
              prval CONTRAST_YES(cl) = pl
              prval CONTRAST_YES(cs2) = ps
            in
              (VALUE_MAYBE_some(VALUE_SAFE_text_color(HEX_RGB_6(p0, n1, x1, n2, x2, n3, x3, n4, x4, n5, x5, n6, x6), cl, cs2)) | true)
            end
            else (VALUE_MAYBE_none() | false)
          end
          else (VALUE_MAYBE_none() | false)
        end
        else (VALUE_MAYBE_none() | false)
      end
      else (VALUE_MAYBE_none() | false)
    end
    else (VALUE_MAYBE_none() | false)
  end
  else (VALUE_MAYBE_none() | false)
end

(* #rgb at o *)
fn _text_color_3 {l:agz}{n:nat}{cs:$A.cells}{o:nat | o + 4 <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, light: rgb256, sepia: rgb256)
  : [ok:bool] (VALUE_MAYBE(cs, o, 4, 1, 0, ok) | bool ok) = let
  val @(p0 | c0) = _byte(src, o)
  val @(lr, lg, lb) = light
  val @(sr, sg, sb) = sepia
in
  if c0 = 35 then let
    val @(d1 | h1, b1) = _hex_at(src, o + 1)
  in
    if b1 then let
      val @(d2 | h2, b2) = _hex_at(src, o + 2)
    in
      if b2 then let
        val @(d3 | h3, b3) = _hex_at(src, o + 3)
      in
        if b3 then let
          prval HEX_AT_some(n1, x1) = d1
          prval HEX_AT_some(n2, x2) = d2
          prval HEX_AT_some(n3, x3) = d3
          val r = h1 * 17
          val g = h2 * 17
          val b = h3 * 17
          val @(pl | okl) = contrast_45_of(r, g, b, lr, lg, lb)
        in
          if okl then let
            val @(ps | oks) = contrast_45_of(r, g, b, sr, sg, sb)
          in
            if oks then let
              prval CONTRAST_YES(cl) = pl
              prval CONTRAST_YES(cs2) = ps
            in
              (VALUE_MAYBE_some(VALUE_SAFE_text_color(HEX_RGB_3(p0, n1, x1, n2, x2, n3, x3), cl, cs2)) | true)
            end
            else (VALUE_MAYBE_none() | false)
          end
          else (VALUE_MAYBE_none() | false)
        end
        else (VALUE_MAYBE_none() | false)
      end
      else (VALUE_MAYBE_none() | false)
    end
    else (VALUE_MAYBE_none() | false)
  end
  else (VALUE_MAYBE_none() | false)
end

(* The value of a declaration: the k bytes from o, as the property p of the
   grammar g (the mask is the table's), proved or dropped *)
fn _read_value {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}{g:int}{p:int | p >= 0}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, g: int g, p: int p, mask: int,
   light: rgb256, sepia: rgb256)
  : [ok:bool] (VALUE_MAYBE(cs, o, k, g, p, ok) | bool ok) =
  if k = 0 then (VALUE_MAYBE_none() | false)
  else if p = 0 then
    (if g = 1 then
       (if k = 7 then _text_color_6(src, o, light, sepia)
        else if k = 4 then _text_color_3(src, o, light, sepia)
        else (VALUE_MAYBE_none() | false))
     else (VALUE_MAYBE_none() | false))
  else let
    val @(done | d, ok) = _value_loop(src, o + k, o, 0, g, p, 0, mask, VP_nil())
  in
    if ok then
      (if d = 0 then let
         prval LOOP_DONE_ok(pf) = done
       in (VALUE_MAYBE_some(VALUE_SAFE_general(pf)) | true) end
       else (VALUE_MAYBE_none() | false))
    else (VALUE_MAYBE_none() | false)
  end

(* ============================================================
   The tree
   Linear (nothing is a GC's), every list indexed by its size, which is the
   metric of the functions that free, reverse and walk it.
   ============================================================ *)

(* Every span is in range of the n cells it is a span of: the constructors that
   hold one say o + k <= n, which is what lets the walk hand a span to a sink. *)

(* The items of a font-family value: a generic family or CSS-wide keyword, kept
   as the proved identifier it is, or a named family, a span of its text that is
   only compared with the book's faces and never emitted *)
#pub datavtype book_family(cs: $A.cells, n: int, sz: int) =
  | BFamilyNil(cs, n, 0)
  | {o,k:nat | o + k <= n}{r:nat}
    BFamilyGeneric(cs, n, r + 1) of (CSS_IDENT(cs, o, k) | int o, int k, book_family(cs, n, r))
  | {o,k:nat | o + k <= n}{r:nat} BFamilyNamed(cs, n, r + 1) of (int o, int k, book_family(cs, n, r))

(* The declarations of a block, each with the proof that its value is safe for
   its property p in the grammar g *)
#pub datavtype book_decls(cs: $A.cells, n: int, g: int, sz: int) =
  | BDeclsNil(cs, n, g, 0)
  | {o,k:nat | o + k <= n}{p:int}{r:nat}
    BDecl(cs, n, g, r + 1) of (VALUE_SAFE(cs, o, k, g, p) | int p, int o, int k, book_decls(cs, n, g, r))
  | {f,r:nat} BDeclFamily(cs, n, g, f + r + 1) of (book_family(cs, n, f), book_decls(cs, n, g, r))

(* A compound selector's simple selectors: r is 1 at the top and 0 inside :not() *)
#pub datavtype book_simples(cs: $A.cells, n: int, r: int, sz: int) =
  | BSimplesNil(cs, n, r, 0)
  | {o,k:nat | o + k <= n}{s:nat}
    BSType(cs, n, r, s + 1) of (CSS_IDENT(cs, o, k) | int o, int k, book_simples(cs, n, r, s))
  | {s:nat} BSUniversal(cs, n, r, s + 1) of book_simples(cs, n, r, s)
  | {o,k:nat | o + k <= n}{s:nat}
    BSClass(cs, n, r, s + 1) of (CSS_IDENT(cs, o, k) | int o, int k, book_simples(cs, n, r, s))
  | {o,k:nat | o + k <= n}{s:nat}
    BSId(cs, n, r, s + 1) of (CSS_IDENT(cs, o, k) | int o, int k, book_simples(cs, n, r, s))
  | {o,k:nat | o + k <= n}{s:nat}
    BSLang(cs, n, r, s + 1) of (CSS_IDENT(cs, o, k) | int o, int k, book_simples(cs, n, r, s))
  | {o,k:nat | o + k <= n}{s:nat}
    BSAttrIdent(cs, n, r, s + 1) of (CSS_IDENT(cs, o, k) | int, int, int o, int k, book_simples(cs, n, r, s))
  | {o,k:nat | o + k <= n}{s:nat}
    BSAttrString(cs, n, r, s + 1) of (STRING(cs, o, k) | int, int, int o, int k, book_simples(cs, n, r, s))
  | {s:nat} BSAttrExists(cs, n, r, s + 1) of (int, book_simples(cs, n, r, s))
  | {s:nat} BSPseudo(cs, n, r, s + 1) of (int, book_simples(cs, n, r, s))
  | {s:nat} BSNth(cs, n, r, s + 1) of (int, int, int, book_simples(cs, n, r, s))
  | {s,t:nat} BSNot(cs, n, 1, s + t + 1) of (book_simples(cs, n, 0, t), book_simples(cs, n, 1, s))

(* A complex selector: its compounds with the combinator before each (0 for the
   first, 1 descendant, 2 child, 3 next sibling, 4 later sibling) and the
   pseudo-element at the end of each (~1 for none) *)
#pub datavtype book_steps(cs: $A.cells, n: int, sz: int) =
  | BStepsNil(cs, n, 0)
  | {t,s:nat} BStep(cs, n, s + t + 1) of (int, int, book_simples(cs, n, 1, t), book_steps(cs, n, s))

#pub datavtype book_sels(cs: $A.cells, n: int, sz: int) =
  | BSelsNil(cs, n, 0)
  | {t,s:nat} BSel(cs, n, s + t + 1) of (book_steps(cs, n, t), book_sels(cs, n, s))

(* The rules of a sheet, or of the block of an @media or @supports (kind 0 and
   1) whose condition is proved *)
#pub datavtype book_rules(cs: $A.cells, n: int, g: int, sz: int) =
  | BRulesNil(cs, n, g, 0)
  | {a,b,c:nat}
    BRule(cs, n, g, a + b + c + 1) of (book_sels(cs, n, a), book_decls(cs, n, g, b), book_rules(cs, n, g, c))
  | {o,k:nat | o + k <= n}{a,c:nat}
    BCondition(cs, n, g, a + c + 1) of
      (QUERY_SAFE(cs, o, k) | int, int o, int k, book_rules(cs, n, g, a), book_rules(cs, n, g, c))

(* The target of an @import, a span of the path in the sheet *)
#pub datavtype book_imports(n: int, sz: int) =
  | BImportsNil(n, 0)
  | {o,k:nat | o + k <= n}{r:nat} BImportsCons(n, r + 1) of (int o, int k, book_imports(n, r))

(* The source paths of a font face *)
#pub datavtype book_srcs(n: int, sz: int) =
  | BSrcsNil(n, 0)
  | {o,k:nat | o + k <= n}{r:nat} BSrcsCons(n, r + 1) of (int o, int k, book_srcs(n, r))

(* The weight or the style of a face, proved as the value of that property *)
#pub datavtype book_descriptor(cs: $A.cells, n: int, g: int) =
  | BDescriptorNone(cs, n, g)
  | {o,k:nat | o + k <= n}{p:int} BDescriptor(cs, n, g) of (VALUE_SAFE(cs, o, k, g, p) | int o, int k)

(* A face the book declares: the text of its family, its sources, its weight and its
   style; its number is its place in the list *)
#pub datavtype book_faces(cs: $A.cells, n: int, g: int, sz: int) =
  | BFacesNil(cs, n, g, 0)
  | {o,k:nat | o + k <= n}{s,r:nat} BFace(cs, n, g, r + 1) of
      (int o, int k, book_srcs(n, s), book_descriptor(cs, n, g), book_descriptor(cs, n, g), book_faces(cs, n, g, r))

(* What reading a sheet gives *)
#pub datavtype book_read(cs: $A.cells, n: int, g: int) =
  | {a,b,c:nat} BookRead(cs, n, g) of (book_rules(cs, n, g, a), book_imports(n, b), book_faces(cs, n, g, c))

fun _free_family {n:nat}{cs:$A.cells}{sz:nat} .<sz>. (xs: book_family(cs, n, sz)): void =
  case+ xs of
  | ~BFamilyNil() => ()
  | ~BFamilyGeneric(_ | _, _, rest) => _free_family(rest)
  | ~BFamilyNamed(_, _, rest) => _free_family(rest)

fun _free_decls {n:nat}{cs:$A.cells}{g:int}{sz:nat} .<sz>. (xs: book_decls(cs, n, g, sz)): void =
  case+ xs of
  | ~BDeclsNil() => ()
  | ~BDecl(_ | _, _, _, rest) => _free_decls(rest)
  | ~BDeclFamily(fam, rest) => let val () = _free_family(fam) in _free_decls(rest) end

fun _free_simples {n:nat}{cs:$A.cells}{r:nat}{sz:nat} .<r, sz>. (xs: book_simples(cs, n, r, sz)): void =
  case+ xs of
  | ~BSimplesNil() => ()
  | ~BSType(_ | _, _, rest) => _free_simples(rest)
  | ~BSUniversal(rest) => _free_simples(rest)
  | ~BSClass(_ | _, _, rest) => _free_simples(rest)
  | ~BSId(_ | _, _, rest) => _free_simples(rest)
  | ~BSLang(_ | _, _, rest) => _free_simples(rest)
  | ~BSAttrIdent(_ | _, _, _, _, rest) => _free_simples(rest)
  | ~BSAttrString(_ | _, _, _, _, rest) => _free_simples(rest)
  | ~BSAttrExists(_, rest) => _free_simples(rest)
  | ~BSPseudo(_, rest) => _free_simples(rest)
  | ~BSNth(_, _, _, rest) => _free_simples(rest)
  | ~BSNot(inner, rest) => let val () = _free_simples(inner) in _free_simples(rest) end

fun _free_steps {n:nat}{cs:$A.cells}{sz:nat} .<sz>. (xs: book_steps(cs, n, sz)): void =
  case+ xs of
  | ~BStepsNil() => ()
  | ~BStep(_, _, simples, rest) => let val () = _free_simples(simples) in _free_steps(rest) end

fun _free_sels {n:nat}{cs:$A.cells}{sz:nat} .<sz>. (xs: book_sels(cs, n, sz)): void =
  case+ xs of
  | ~BSelsNil() => ()
  | ~BSel(steps, rest) => let val () = _free_steps(steps) in _free_sels(rest) end

fun _free_rules {n:nat}{cs:$A.cells}{g:int}{sz:nat} .<sz>. (xs: book_rules(cs, n, g, sz)): void =
  case+ xs of
  | ~BRulesNil() => ()
  | ~BRule(sels, decls, rest) => let
      val () = _free_sels(sels)
      val () = _free_decls(decls)
    in _free_rules(rest) end
  | ~BCondition(_ | _, _, _, inner, rest) => let
      val () = _free_rules(inner)
    in _free_rules(rest) end

fun _free_imports {n:nat}{sz:nat} .<sz>. (xs: book_imports(n, sz)): void =
  case+ xs of
  | ~BImportsNil() => ()
  | ~BImportsCons(_, _, rest) => _free_imports(rest)

fun _free_srcs {n:nat}{sz:nat} .<sz>. (xs: book_srcs(n, sz)): void =
  case+ xs of
  | ~BSrcsNil() => ()
  | ~BSrcsCons(_, _, rest) => _free_srcs(rest)

fn _free_descriptor {n:nat}{cs:$A.cells}{g:int} (d: book_descriptor(cs, n, g)): void =
  case+ d of
  | ~BDescriptorNone() => ()
  | ~BDescriptor(_ | _, _) => ()

fun _free_faces {n:nat}{cs:$A.cells}{g:int}{sz:nat} .<sz>. (xs: book_faces(cs, n, g, sz)): void =
  case+ xs of
  | ~BFacesNil() => ()
  | ~BFace(_, _, srcs, weight, style, rest) => let
      val () = _free_srcs(srcs)
      val () = _free_descriptor(weight)
      val () = _free_descriptor(style)
    in _free_faces(rest) end

(* Frees what a reading gave *)
#pub fn book_read_free {n:nat}{cs:$A.cells}{g:int} (r: book_read(cs, n, g)): void

implement book_read_free(r) =
  case+ r of
  | ~BookRead(rules, imports, faces) => let
      val () = _free_rules(rules)
      val () = _free_imports(imports)
    in _free_faces(faces) end

(* The lists, reversed: they are built on the way in and put in order at the end *)
fun _family_rev {n:nat}{cs:$A.cells}{a,b:nat} .<a>.
  (xs: book_family(cs, n, a), acc: book_family(cs, n, b)): book_family(cs, n, a + b) =
  case+ xs of
  | ~BFamilyNil() => acc
  | ~BFamilyGeneric(pf | o, k, rest) => _family_rev(rest, BFamilyGeneric(pf | o, k, acc))
  | ~BFamilyNamed(o, k, rest) => _family_rev(rest, BFamilyNamed(o, k, acc))

fun _decls_rev {n:nat}{cs:$A.cells}{g:int}{a,b:nat} .<a>.
  (xs: book_decls(cs, n, g, a), acc: book_decls(cs, n, g, b)): book_decls(cs, n, g, a + b) =
  case+ xs of
  | ~BDeclsNil() => acc
  | ~BDecl(pf | p, o, k, rest) => _decls_rev(rest, BDecl(pf | p, o, k, acc))
  | ~BDeclFamily(fam, rest) => _decls_rev(rest, BDeclFamily(fam, acc))

fun _rules_rev {n:nat}{cs:$A.cells}{g:int}{a,b:nat} .<a>.
  (xs: book_rules(cs, n, g, a), acc: book_rules(cs, n, g, b)): book_rules(cs, n, g, a + b) =
  case+ xs of
  | ~BRulesNil() => acc
  | ~BRule(sels, decls, rest) => _rules_rev(rest, BRule(sels, decls, acc))
  | ~BCondition(pf | kind, o, k, inner, rest) => _rules_rev(rest, BCondition(pf | kind, o, k, inner, acc))

fun _imports_rev {n:nat}{a,b:nat} .<a>. (xs: book_imports(n, a), acc: book_imports(n, b)): book_imports(n, a + b) =
  case+ xs of
  | ~BImportsNil() => acc
  | ~BImportsCons(o, k, rest) => _imports_rev(rest, BImportsCons(o, k, acc))

fun _srcs_rev {n:nat}{a,b:nat} .<a>. (xs: book_srcs(n, a), acc: book_srcs(n, b)): book_srcs(n, a + b) =
  case+ xs of
  | ~BSrcsNil() => acc
  | ~BSrcsCons(o, k, rest) => _srcs_rev(rest, BSrcsCons(o, k, acc))

fun _faces_rev {n:nat}{cs:$A.cells}{g:int}{a,b:nat} .<a>.
  (xs: book_faces(cs, n, g, a), acc: book_faces(cs, n, g, b)): book_faces(cs, n, g, a + b) =
  case+ xs of
  | ~BFacesNil() => acc
  | ~BFace(o, k, srcs, weight, style, rest) => _faces_rev(rest, BFace(o, k, srcs, weight, style, acc))

(* ============================================================
   Skipping: comments, strings, blocks, what is dropped
   ============================================================ *)

(* The end of a comment whose body starts at i *)
fun _comment_body {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] int j =
  if i + 1 >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 42 then let
      val @(p1 | c1) = _byte(src, i + 1)
    in
      if c1 = 47 then i + 2 else _comment_body(src, e, i + 1)
    end
    else _comment_body(src, e, i + 1)
  end

(* White space and comments *)
fun _skip_trivia {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] int j =
  if i >= e then i
  else let
    val @(p0 | c) = _byte(src, i)
    val @(pw | bw) = _ws_class(c)
  in
    if bw then _skip_trivia(src, e, i + 1)
    else if c = 47 then
      (if i + 1 < e then let
         val @(p1 | c1) = _byte(src, i + 1)
       in
         if c1 = 42 then _skip_trivia(src, e, _comment_body(src, e, i + 2)) else i
       end
       else i)
    else i
  end

(* White space, comments and semicolons *)
fun _skip_semis {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] int j = let
  val j = _skip_trivia(src, e, i)
in
  if j >= e then j
  else let
    val @(p0 | c) = _byte(src, j)
  in
    if c = 59 then _skip_semis(src, e, j + 1) else j
  end
end

(* The end of the string with the quote q whose body starts at i; a string
   that a line ends is over at the line *)
fun _string_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, q: int, i: int i): [j:nat | i <= j; j <= e] int j =
  if i >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = q then i + 1
    else if c = 92 then (if i + 1 < e then _string_end(src, e, q, i + 2) else e)
    else if c = 10 then i
    else _string_end(src, e, q, i + 1)
  end

(* The end of the block whose opening brace is before i, d blocks being open *)
fun _block_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{d:pos} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, d: int d): [j:nat | i <= j; j <= e] int j =
  if i >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 125 then (if d = 1 then i + 1 else _block_end(src, e, i + 1, d - 1))
    else if c = 123 then _block_end(src, e, i + 1, d + 1)
    else if c = 34 then _block_end(src, e, _string_end(src, e, 34, i + 1), d)
    else if c = 39 then _block_end(src, e, _string_end(src, e, 39, i + 1), d)
    else if c = 47 then
      (if i + 1 < e then let
         val @(p1 | c1) = _byte(src, i + 1)
       in
         if c1 = 42 then _block_end(src, e, _comment_body(src, e, i + 2), d)
         else _block_end(src, e, i + 1, d)
       end
       else _block_end(src, e, i + 1, d))
    else _block_end(src, e, i + 1, d)
  end

(* The end of a declaration from i: its semicolon or the closing brace of its
   block (not consumed), or the end of a nested block (consumed, and then true) *)
fun _decl_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{pd:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, pd: int pd)
  : [j:nat | i <= j; j <= e] @(int j, bool) =
  if i >= e then @(e, false)
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 59 then (if pd = 0 then @(i, false) else _decl_end(src, e, i + 1, pd))
    else if c = 125 then @(i, false)
    else if c = 123 then let val j = _block_end(src, e, i + 1, 1) in @(j, true) end
    else if c = 40 then _decl_end(src, e, i + 1, pd + 1)
    else if c = 41 then (if pd = 0 then _decl_end(src, e, i + 1, 0) else _decl_end(src, e, i + 1, pd - 1))
    else if c = 34 then _decl_end(src, e, _string_end(src, e, 34, i + 1), pd)
    else if c = 39 then _decl_end(src, e, _string_end(src, e, 39, i + 1), pd)
    else if c = 47 then
      (if i + 1 < e then let
         val @(p1 | c1) = _byte(src, i + 1)
       in
         if c1 = 42 then _decl_end(src, e, _comment_body(src, e, i + 2), pd)
         else _decl_end(src, e, i + 1, pd)
       end
       else _decl_end(src, e, i + 1, pd))
    else _decl_end(src, e, i + 1, pd)
  end

(* The end of the declaration that starts at i < e: it takes the byte at i
   whatever it is, so a loop over declarations always moves *)
fn _decl_first {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i < e}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i + 1 <= j; j <= e] @(int j, bool) = let
  val @(p0 | c) = _byte(src, i)
in
  if c = 123 then let val j = _block_end(src, e, i + 1, 1) in @(j, true) end
  else if c = 40 then _decl_end(src, e, i + 1, 1)
  else if c = 34 then let val j = _string_end(src, e, 34, i + 1) in @(j, false) end
  else if c = 39 then let val j = _string_end(src, e, 39, i + 1) in @(j, false) end
  else _decl_end(src, e, i + 1, 0)
end

(* The end of what comes before a block or a semicolon (a selector list, an
   at-rule's prelude) from i: the byte that ends it, not consumed *)
fun _prelude_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{pd:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, pd: int pd): [j:nat | i <= j; j <= e] int j =
  if i >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 123 then i
    else if c = 125 then i
    else if c = 59 then (if pd = 0 then i else _prelude_end(src, e, i + 1, pd))
    else if c = 40 then _prelude_end(src, e, i + 1, pd + 1)
    else if c = 91 then _prelude_end(src, e, i + 1, pd + 1)
    else if c = 41 then (if pd = 0 then _prelude_end(src, e, i + 1, 0) else _prelude_end(src, e, i + 1, pd - 1))
    else if c = 93 then (if pd = 0 then _prelude_end(src, e, i + 1, 0) else _prelude_end(src, e, i + 1, pd - 1))
    else if c = 34 then _prelude_end(src, e, _string_end(src, e, 34, i + 1), pd)
    else if c = 39 then _prelude_end(src, e, _string_end(src, e, 39, i + 1), pd)
    else if c = 47 then
      (if i + 1 < e then let
         val @(p1 | c1) = _byte(src, i + 1)
       in
         if c1 = 42 then _prelude_end(src, e, _comment_body(src, e, i + 2), pd)
         else _prelude_end(src, e, i + 1, pd)
       end
       else _prelude_end(src, e, i + 1, pd))
    else _prelude_end(src, e, i + 1, pd)
  end

(* Back from b over white space, not before a *)
fun _trim_end {l:agz}{n:nat}{cs:$A.cells}{a:nat}{b:nat | a <= b; b <= n} .<b - a>.
  (src: !$A.cborrow(l, n, cs), a: int a, b: int b): [j:nat | a <= j; j <= b] int j =
  if b <= a then a
  else let
    val @(p0 | c) = _byte(src, b - 1)
    val @(pw | bw) = _ws_class(c)
  in
    if bw then _trim_end(src, a, b - 1) else b
  end

(* The end of a value from a to b without its important flag: `!` and `important`
   cut off, and the white space before them *)
fn _strip_important {l:agz}{n:nat}{cs:$A.cells}{a:nat}{b:nat | a <= b; b <= n}
  (src: !$A.cborrow(l, n, cs), a: int a, b: int b): [j:nat | a <= j; j <= b] int j = let
  val t = _trim_end(src, a, b)
in
  if t - a >= 10 then
    (if _span_is(src, t - 9, 9, "important") then let
       val w = _trim_end(src, a, t - 9)
     in
       if w > a then let
         val @(p0 | c) = _byte(src, w - 1)
       in
         if c = 33 then _trim_end(src, a, w - 1) else t
       end
       else t
     end
     else t)
  else t
end

(* ============================================================
   Declarations
   ============================================================ *)

(* The generic families, and the CSS-wide keywords, which a font-family keeps *)
fn _generic_family {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): bool =
  if _span_is(src, o, k, "serif") then true
  else if _span_is(src, o, k, "sans-serif") then true
  else if _span_is(src, o, k, "monospace") then true
  else if _span_is(src, o, k, "cursive") then true
  else if _span_is(src, o, k, "fantasy") then true
  else if _span_is(src, o, k, "system-ui") then true
  else if _span_is(src, o, k, "ui-serif") then true
  else if _span_is(src, o, k, "ui-sans-serif") then true
  else if _span_is(src, o, k, "ui-monospace") then true
  else if _span_is(src, o, k, "ui-rounded") then true
  else if _span_is(src, o, k, "math") then true
  else if _span_is(src, o, k, "emoji") then true
  else if _span_is(src, o, k, "fangsong") then true
  else if _span_is(src, o, k, "inherit") then true
  else if _span_is(src, o, k, "initial") then true
  else if _span_is(src, o, k, "unset") then true
  else false

(* The index after the next comma from i *)
fun _to_comma {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] int j =
  if i >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 44 then i + 1 else _to_comma(src, e, i + 1)
  end

(* The end of the identifiers, separated by white space, that go on from i *)
fun _words_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] int j = let
  val @(ps | s) = _scan_spaces(src, e, i)
in
  if s >= 1 then let
    val @(pi | m) = _scan_ident(src, e, i + s)
  in
    if m >= 1 then _words_end(src, e, i + s + m) else i
  end
  else i
end

(* After an item of a list: past the comma that follows it; false when something
   else does and the item is not one *)
fn _item_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] @(int j, bool) = let
  val t = _skip_trivia(src, e, i)
in
  if t >= e then @(t, true)
  else let
    val @(p0 | c) = _byte(src, t)
  in
    if c = 44 then @(t + 1, true)
    else let val j = _to_comma(src, e, t + 1) in @(j, false) end
  end
end

(* The text of a quoted string whose quote is at s: where it starts and how long
   it is, 0 when the string is not closed *)
fn _quoted_span {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{s:nat | s < e}
  (src: !$A.cborrow(l, n, cs), e: int e, s: int s, q: int)
  : [o:nat][k:nat | o + k <= e] @(int o, int k) = let
  val j = _string_end(src, e, q, s + 1)
in
  if j >= s + 2 then let
    val @(p0 | c) = _byte(src, j - 1)
  in
    if c = q then @(s + 1, j - s - 2) else @(s, 0)
  end
  else @(s, 0)
end

(* The items of a font-family from i, in order; cnt counts the ones in acc *)
fnx _family_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{s:nat} .<e - i, 1>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, cnt: int s, acc: book_family(cs, n, s))
  : [r:nat] @(book_family(cs, n, r), int r) = let
  val i1 = _skip_trivia(src, e, i)
in
  if i1 >= e then @(_family_rev(acc, BFamilyNil()), cnt)
  else let
    val @(p0 | c) = _byte(src, i1)
  in
    if c = 44 then _family_loop(src, e, i1 + 1, cnt, acc)
    else if c = 34 then _family_string(src, e, i1, 34, cnt, acc)
    else if c = 39 then _family_string(src, e, i1, 39, cnt, acc)
    else let
      val @(pi | m) = _scan_ident(src, e, i1)
    in
      if m >= 1 then let
        prval IDENT_OPT_some(id) = pi
        val wend = _words_end(src, e, i1 + m)
        val @(j, ok) = _item_end(src, e, wend)
      in
        if ok then
          (if wend = i1 + m then
             (if _generic_family(src, i1, m) then _family_loop(src, e, j, cnt + 1, BFamilyGeneric(id | i1, m, acc))
              else _family_loop(src, e, j, cnt + 1, BFamilyNamed(i1, m, acc)))
           else _family_loop(src, e, j, cnt + 1, BFamilyNamed(i1, wend - i1, acc)))
        else _family_loop(src, e, j, cnt, acc)
      end
      else let prval IDENT_OPT_none() = pi in _family_loop(src, e, _to_comma(src, e, i1 + 1), cnt, acc) end
    end
  end
end

and _family_string {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i < e}{s:nat} .<e - i, 0>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, q: int, cnt: int s, acc: book_family(cs, n, s))
  : [r:nat] @(book_family(cs, n, r), int r) = let
  val @(ps | m) = _scan_string(src, e, i)
in
  if m >= 1 then let
    val @(j, ok) = _item_end(src, e, i + m)
    val @(qo, qk) = _quoted_span(src, e, i, q)
  in
    if ok then _family_loop(src, e, j, cnt + 1, BFamilyNamed(qo, qk, acc))
    else _family_loop(src, e, j, cnt, acc)
  end
  else _family_loop(src, e, _to_comma(src, e, i + 1), cnt, acc)
end

(* One declaration, from a to b: added to acc when it is one that the grammar g
   keeps and its value is proved, and acc otherwise *)
fn _decl_add {l:agz}{n:nat}{cs:$A.cells}{a:nat}{b:nat | a <= b; b <= n}{g:int}{s:nat}
  (src: !$A.cborrow(l, n, cs), a: int a, b: int b, g: int g, light: rgb256, sepia: rgb256,
   acc: book_decls(cs, n, g, s)): [r:nat] book_decls(cs, n, g, r) = let
  val @(pi | m) = _scan_ident(src, b, a)
in
  if m >= 1 then let
    val c1 = _skip_trivia(src, b, a + m)
  in
    if c1 >= b then acc
    else let
      val @(pc | c) = _byte(src, c1)
    in
      if c = 58 then let
        val p = _prop_find(src, a, m, g)
      in
        if p >= 0 then let
          val vs = _skip_trivia(src, b, c1 + 1)
          val ve = _strip_important(src, vs, b)
          val mask = _prop_mask(p, g)
        in
          if _has(mask, M_FAMILY) then let
            val @(fam, cnt) = _family_loop(src, ve, vs, 0, BFamilyNil())
          in
            if cnt = 0 then let val () = _free_family(fam) in acc end
            else BDeclFamily(fam, acc)
          end
          else let
            val @(pv | ok) = _read_value(src, vs, ve - vs, g, p, mask, light, sepia)
          in
            if ok then let
              prval VALUE_MAYBE_some(pf) = pv
              val allowed =
                (if _has(mask, M_DISPLAY) then _display_ok(src, vs, ve - vs)
                 else if _has(mask, M_POSITION) then _position_ok(src, vs, ve - vs, g)
                 else if _has(mask, M_ZINDEX) then _zindex_ok(src, vs, ve - vs)
                 else true)
            in
              if allowed then BDecl(pf | p, vs, ve - vs, acc) else acc
            end
            else let prval VALUE_MAYBE_none() = pv in acc end
          end
        end
        else acc
      end
      else acc
    end
  end
  else let prval IDENT_OPT_none() = pi in acc end
end

(* The declarations of a block that starts at i, up to its closing brace, in order *)
fun _decls_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{g:int}{s:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, g: int g, light: rgb256, sepia: rgb256,
   acc: book_decls(cs, n, g, s))
  : [j:nat | i <= j; j <= e][r:nat] @(book_decls(cs, n, g, r), int j) = let
  val i1 = _skip_semis(src, e, i)
in
  if i1 >= e then @(_decls_rev(acc, BDeclsNil()), e)
  else let
    val @(p0 | c) = _byte(src, i1)
  in
    if c = 125 then @(_decls_rev(acc, BDeclsNil()), i1 + 1)
    else let
      val @(d, nested) = _decl_first(src, e, i1)
    in
      if nested then _decls_loop(src, e, d, g, light, sepia, acc)
      else let
        val acc2 = _decl_add(src, i1, d, g, light, sepia, acc)
      in _decls_loop(src, e, d, g, light, sepia, acc2) end
    end
  end
end

(* ============================================================
   Selectors (design 3.5)
   ============================================================ *)

(* The code of an attribute a selector may test, or ~1:
   1 lang, 2 dir, 3 title, 4 colspan, 5 rowspan, 6 hidden, 7 epub|type *)
fn _attr_code {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): int =
  if _span_is(src, o, k, "lang") then 1
  else if _span_is(src, o, k, "dir") then 2
  else if _span_is(src, o, k, "title") then 3
  else if _span_is(src, o, k, "colspan") then 4
  else if _span_is(src, o, k, "rowspan") then 5
  else if _span_is(src, o, k, "hidden") then 6
  else ~1

(* The code of a pseudo-class or of a pseudo-element with one colon, or ~1:
   1 first-child, 2 last-child, 3 only-child, 4 first-of-type, 5 last-of-type,
   6 only-of-type, 7 link, 8 empty, 9 root; 21 to 24 before, after,
   first-letter and first-line (one colon); 31 not, 32 lang, 33 nth-child,
   34 nth-last-child, 35 nth-of-type, 36 nth-last-of-type *)
fn _pseudo_code {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): [c:int | c >= ~1] int c =
  if _span_is(src, o, k, "first-child") then 1
  else if _span_is(src, o, k, "last-child") then 2
  else if _span_is(src, o, k, "only-child") then 3
  else if _span_is(src, o, k, "first-of-type") then 4
  else if _span_is(src, o, k, "last-of-type") then 5
  else if _span_is(src, o, k, "only-of-type") then 6
  else if _span_is(src, o, k, "link") then 7
  else if _span_is(src, o, k, "empty") then 8
  else if _span_is(src, o, k, "root") then 9
  else if _span_is(src, o, k, "before") then 21
  else if _span_is(src, o, k, "after") then 22
  else if _span_is(src, o, k, "first-letter") then 23
  else if _span_is(src, o, k, "first-line") then 24
  else if _span_is(src, o, k, "not") then 31
  else if _span_is(src, o, k, "lang") then 32
  else if _span_is(src, o, k, "nth-child") then 33
  else if _span_is(src, o, k, "nth-last-child") then 34
  else if _span_is(src, o, k, "nth-of-type") then 35
  else if _span_is(src, o, k, "nth-last-of-type") then 36
  else ~1

(* The pseudo-element of two colons: 0 before, 1 after, 2 first-letter,
   3 first-line, 4 marker, or ~1 (::selection among them) *)
fn _pe_code {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): [c:int | c >= ~1] int c =
  if _span_is(src, o, k, "before") then 0
  else if _span_is(src, o, k, "after") then 1
  else if _span_is(src, o, k, "first-letter") then 2
  else if _span_is(src, o, k, "first-line") then 3
  else if _span_is(src, o, k, "marker") then 4
  else ~1

(* Whether the bar at b is the start of the operator |= (and not a namespace bar) *)
fn _bar_is_operator {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{b:nat | b < e}
  (src: !$A.cborrow(l, n, cs), e: int e, b: int b): bool =
  if b + 1 < e then let
    val @(p0 | c) = _byte(src, b + 1)
  in c = 61 end
  else false

(* The attribute's name from a: its code (~1 for none) and where it ends *)
fn _attr_name {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{a:nat | a <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, a: int a): [j:nat | a <= j; j <= e] @(int, int j) = let
  val @(pn | m) = _scan_ident(src, e, a)
in
  if m >= 1 then
    (if a + m < e then let
       val @(p0 | c) = _byte(src, a + m)
     in
       if c = 124 then
         (if _bar_is_operator(src, e, a + m) then @(_attr_code(src, a, m), a + m)
          else if _span_is(src, a, m, "epub") then let
            val @(pn2 | m2) = _scan_ident(src, e, a + m + 1)
          in
            if m2 >= 1 then
              (if _span_is(src, a + m + 1, m2, "type") then @(7, a + m + 1 + m2) else @(~1, a))
            else @(~1, a)
          end
          else @(~1, a))
       else @(_attr_code(src, a, m), a + m)
     end
     else @(_attr_code(src, a, m), a + m))
  else @(~1, a)
end

(* The value of the k digits from o *)
fun _digits_value {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}{i:nat | i <= k} .<k - i>.
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, i: int i, acc: int): int =
  if i >= k then acc
  else let
    val @(p0 | c) = _byte(src, o + i)
  in _digits_value(src, o, k, i + 1, acc * 10 + (c - 48)) end

(* The operator of an attribute selector at t, whose byte is c: its code (0 for =,
   1 ~=, 2 |=, 3 ^=, 4 $=, 5 *=, ~1 for none) and where the value starts *)
fn _attr_op {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{t:nat | t < e}
  (src: !$A.cborrow(l, n, cs), e: int e, t: int t, c: int): [v:nat | t <= v; v <= e] @(int, int v) =
  if c = 61 then @(0, t + 1)
  else if t + 1 < e then let
    val @(p1 | c1) = _byte(src, t + 1)
  in
    if c1 = 61 then
      (if c = 126 then @(1, t + 2)
       else if c = 124 then @(2, t + 2)
       else if c = 94 then @(3, t + 2)
       else if c = 36 then @(4, t + 2)
       else if c = 42 then @(5, t + 2)
       else @(~1, t))
    else @(~1, t)
  end
  else @(~1, t)

(* The attribute selector's quoted value at v, and the bracket after it *)
fn _attr_string {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{v:nat | i < v; v < e}{r:int}{s:nat}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, v: int v, code: int, oper: int, r: int r,
   acc: book_simples(cs, n, r, s))
  : [t:nat][j:nat | i + 1 <= j; j <= e] @(book_simples(cs, n, r, t), int j, bool) = let
  val @(ps | m) = _scan_string(src, e, v)
in
  if m >= 1 then let
    prval STRING_MAYBE_some(sp) = ps
    val w = _skip_trivia(src, e, v + m)
  in
    if w >= e then @(acc, i + 1, false)
    else let
      val @(p2 | cw) = _byte(src, w)
    in
      if cw = 93 then @(BSAttrString(sp | code, oper, v, m, acc), w + 1, true)
      else @(acc, i + 1, false)
    end
  end
  else let prval STRING_MAYBE_none() = ps in @(acc, i + 1, false) end
end

(* The attribute selector whose bracket is at i: added to acc; the end is past
   the bracket whatever happens, and false says it is not one that is kept *)
fn _attr_read{l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i < e}{r:int}{s:nat}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, r: int r, acc: book_simples(cs, n, r, s))
  : [t:nat][j:nat | i + 1 <= j; j <= e] @(book_simples(cs, n, r, t), int j, bool) = let
  val a = _skip_trivia(src, e, i + 1)
  val @(code, jn) = _attr_name(src, e, a)
in
  if code >= 1 then let
    val t = _skip_trivia(src, e, jn)
  in
    if t >= e then @(acc, i + 1, false)
    else let
      val @(p0 | c) = _byte(src, t)
    in
      if c = 93 then @(BSAttrExists(code, acc), t + 1, true)
      else let
        val @(oper, vs) = _attr_op(src, e, t, c)
      in
        if oper >= 0 then let
          val v = _skip_trivia(src, e, vs)
        in
          if v >= e then @(acc, i + 1, false)
          else let
            val @(pq | q) = _byte(src, v)
          in
            if q = 34 then _attr_string(src, e, i, v, code, oper, r, acc)
            else if q = 39 then _attr_string(src, e, i, v, code, oper, r, acc)
            else let
              val @(pi | m) = _scan_ident(src, e, v)
            in
              if m >= 1 then let
                prval IDENT_OPT_some(id) = pi
                val w = _skip_trivia(src, e, v + m)
              in
                if w >= e then @(acc, i + 1, false)
                else let
                  val @(p2 | cw) = _byte(src, w)
                in
                  if cw = 93 then @(BSAttrIdent(id | code, oper, v, m, acc), w + 1, true)
                  else @(acc, i + 1, false)
                end
              end
              else let prval IDENT_OPT_none() = pi in @(acc, i + 1, false) end
            end
          end
        end
        else @(acc, i + 1, false)
      end
    end
  end
  else @(acc, i + 1, false)
end

(* The signed number of at most 4 digits at d (its sign at d - 1 if s is 1):
   its value, or false *)
fn _signed_digits {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{d:nat | d <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, d: int d, neg: bool): [j:nat | d <= j; j <= e] @(int, int j, int) = let
  val @(pd | m) = _scan_digits(src, e, d)
in
  if m >= 1 then
    (if m <= 4 then let
       val x = _digits_value(src, d, m, 0, 0)
     in @((if neg then 0 - x else x), d + m, m) end
     else @(0, d + m, ~1))
  else @(0, d, 0)
end

(* The digits of the B of An+B, which start at u *)
fn _nth_b_digits {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{t:nat | i <= t; t <= e}{u:nat | t <= u; u <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, a: int, t: int t, u: int u, neg: bool)
  : [j:nat | i <= j; j <= e] @(int, int, int j, bool) = let
  val @(y, w, m) = _signed_digits(src, e, u, neg)
in
  if m >= 1 then @(a, y, w, true) else @(0, 0, i, false)
end

(* The part of An+B after the n, from t *)
fn _nth_b {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{t:nat | i <= t; t <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, a: int, t: int t)
  : [j:nat | i <= j; j <= e] @(int, int, int j, bool) =
  if t >= e then @(a, 0, t, true)
  else let
    val @(p0 | c) = _byte(src, t)
  in
    if c = 43 then _nth_b_digits(src, e, i, a, t, _skip_trivia(src, e, t + 1), false)
    else if c = 45 then _nth_b_digits(src, e, i, a, t, _skip_trivia(src, e, t + 1), true)
    else @(a, 0, t, true)
  end

(* The digits of An+B from d, whose sign is before d *)
fn _nth_num {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{d:nat | i <= d; d <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, d: int d, neg: bool)
  : [j:nat | i <= j; j <= e] @(int, int, int j, bool) = let
  val @(x, q, m) = _signed_digits(src, e, d, neg)
in
  if m < 0 then @(0, 0, i, false)
  else if q < e then let
    val @(p0 | c) = _byte(src, q)
  in
    if c = 110 then _nth_b(src, e, i, (if m = 0 then (if neg then ~1 else 1) else x), _skip_trivia(src, e, q + 1))
    else if c = 78 then _nth_b(src, e, i, (if m = 0 then (if neg then ~1 else 1) else x), _skip_trivia(src, e, q + 1))
    else if m >= 1 then @(0, x, q, true)
    else @(0, 0, i, false)
  end
  else if m >= 1 then @(0, x, q, true)
  else @(0, 0, i, false)
end

(* odd or even at s: A, B and the end, which is s when it is neither *)
fn _nth_kw {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{s:nat | s <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, s: int s): [j:nat | s <= j; j <= e] @(int, int, int j) =
  if s + 4 <= e then
    (if _span_is(src, s, 4, "even") then @(2, 0, s + 4)
     else if _span_is(src, s, 3, "odd") then @(2, 1, s + 3)
     else @(0, 0, s))
  else if s + 3 <= e then
    (if _span_is(src, s, 3, "odd") then @(2, 1, s + 3) else @(0, 0, s))
  else @(0, 0, s)

(* An+B from i: A, B, the end, and whether it is one (odd, even, a number,
   An, An+B, with signs and white space; at most 4 digits each) *)
fn _nth_read {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] @(int, int, int j, bool) = let
  val s = _skip_trivia(src, e, i)
  val @(ka, kb, kj) = _nth_kw(src, e, s)
in
  if kj > s then @(ka, kb, kj, true)
  else if s >= e then @(0, 0, i, false)
  else let
    val @(p0 | c) = _byte(src, s)
  in
    if c = 43 then _nth_num(src, e, i, s + 1, false)
    else if c = 45 then _nth_num(src, e, i, s + 1, true)
    else _nth_num(src, e, i, s, false)
  end
end

(* The end of the item that starts at i, past the next comma that is not inside
   brackets, parentheses or a string; e when there is none *)
fun _to_top_comma {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{pd:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, pd: int pd): [j:nat | i <= j; j <= e] int j =
  if i >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 44 then (if pd = 0 then i + 1 else _to_top_comma(src, e, i + 1, pd))
    else if c = 40 then _to_top_comma(src, e, i + 1, pd + 1)
    else if c = 91 then _to_top_comma(src, e, i + 1, pd + 1)
    else if c = 41 then (if pd = 0 then _to_top_comma(src, e, i + 1, 0) else _to_top_comma(src, e, i + 1, pd - 1))
    else if c = 93 then (if pd = 0 then _to_top_comma(src, e, i + 1, 0) else _to_top_comma(src, e, i + 1, pd - 1))
    else if c = 34 then _to_top_comma(src, e, _string_end(src, e, 34, i + 1), pd)
    else if c = 39 then _to_top_comma(src, e, _string_end(src, e, 39, i + 1), pd)
    else _to_top_comma(src, e, i + 1, pd)
  end

fun _simples_rev {n:nat}{cs:$A.cells}{r:int}{a,b:nat} .<a>.
  (xs: book_simples(cs, n, r, a), acc: book_simples(cs, n, r, b)): book_simples(cs, n, r, a + b) =
  case+ xs of
  | ~BSimplesNil() => acc
  | ~BSType(pf | o, k, rest) => _simples_rev(rest, BSType(pf | o, k, acc))
  | ~BSUniversal(rest) => _simples_rev(rest, BSUniversal(acc))
  | ~BSClass(pf | o, k, rest) => _simples_rev(rest, BSClass(pf | o, k, acc))
  | ~BSId(pf | o, k, rest) => _simples_rev(rest, BSId(pf | o, k, acc))
  | ~BSLang(pf | o, k, rest) => _simples_rev(rest, BSLang(pf | o, k, acc))
  | ~BSAttrIdent(pf | a1, a2, o, k, rest) => _simples_rev(rest, BSAttrIdent(pf | a1, a2, o, k, acc))
  | ~BSAttrString(pf | a1, a2, o, k, rest) => _simples_rev(rest, BSAttrString(pf | a1, a2, o, k, acc))
  | ~BSAttrExists(a1, rest) => _simples_rev(rest, BSAttrExists(a1, acc))
  | ~BSPseudo(a1, rest) => _simples_rev(rest, BSPseudo(a1, acc))
  | ~BSNth(a1, a2, a3, rest) => _simples_rev(rest, BSNth(a1, a2, a3, acc))
  | ~BSNot(inner, rest) => _simples_rev(rest, BSNot(inner, acc))

(* A compound selector from i, one simple selector at a time. first says that a
   type selector or * may come (it is the start); ty that a type selector
   is there. r is 1 at the top and 0 inside :not(). The result: the simple
   selectors in the order they were met, reversed; the end; the pseudo-element
   that ended it (~1 if none); and whether it is a kept selector *)
fnx _compound_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{r:int}{s:nat} .<e - i, 1>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, r: int r, first: bool, ty: bool,
   acc: book_simples(cs, n, r, s))
  : [t:nat][j:nat | i <= j; j <= e] @(book_simples(cs, n, r, t), int j, int, bool) =
  if i >= e then @(acc, i, ~1, true)
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 46 then let
      val @(pi | m) = _scan_ident(src, e, i + 1)
    in
      if m >= 1 then let
        prval IDENT_OPT_some(id) = pi
      in _compound_loop(src, e, i + 1 + m, r, false, ty, BSClass(id | i + 1, m, acc)) end
      else let prval IDENT_OPT_none() = pi in @(acc, i, ~1, false) end
    end
    else if c = 35 then let
      val @(pi | m) = _scan_ident(src, e, i + 1)
    in
      if m >= 1 then let
        prval IDENT_OPT_some(id) = pi
      in _compound_loop(src, e, i + 1 + m, r, false, ty, BSId(id | i + 1, m, acc)) end
      else let prval IDENT_OPT_none() = pi in @(acc, i, ~1, false) end
    end
    else if c = 91 then let
      val @(acc2, j, ok) = _attr_read(src, e, i, r, acc)
    in
      if ok then _compound_loop(src, e, j, r, false, ty, acc2) else @(acc2, i, ~1, false)
    end
    else if c = 58 then _pseudo(src, e, i, r, ty, acc)
    else if c = 42 then
      (if first then _compound_loop(src, e, i + 1, r, false, false, BSUniversal(acc))
       else @(acc, i, ~1, false))
    else if first then let
      val @(pi | m) = _scan_ident(src, e, i)
    in
      if m >= 1 then let
        prval IDENT_OPT_some(id) = pi
      in _compound_loop(src, e, i + m, r, false, true, BSType(id | i, m, acc)) end
      else let prval IDENT_OPT_none() = pi in @(acc, i, ~1, true) end
    end
    else @(acc, i, ~1, true)
  end

(* A pseudo-class or a pseudo-element, whose colon is at i *)
and _pseudo {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i < e}{r:int}{s:nat} .<e - i, 0>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, r: int r, ty: bool, acc: book_simples(cs, n, r, s))
  : [t:nat][j:nat | i <= j; j <= e] @(book_simples(cs, n, r, t), int j, int, bool) =
  if i + 1 >= e then @(acc, i, ~1, false)
  else let
    val @(p1 | c1) = _byte(src, i + 1)
  in
    if c1 = 58 then let
      val @(pi | m) = _scan_ident(src, e, i + 2)
      val code = _pe_code(src, i + 2, m)
    in
      if code >= 0 then (if r = 1 then @(acc, i + 2 + m, code, true) else @(acc, i, ~1, false))
      else @(acc, i, ~1, false)
    end
    else let
      val @(pi | m) = _scan_ident(src, e, i + 1)
      val code = _pseudo_code(src, i + 1, m)
    in
      if code < 0 then @(acc, i, ~1, false)
      else if code >= 21 then
        (if code <= 24 then (if r = 1 then @(acc, i + 1 + m, code - 21, true) else @(acc, i, ~1, false))
         else _pseudo_args(src, e, i, i + 1 + m, code, r, ty, acc))
      else if code >= 4 then
        (if code <= 6 then (if ty then _compound_loop(src, e, i + 1 + m, r, false, ty, BSPseudo(code, acc))
                            else @(acc, i, ~1, false))
         else _compound_loop(src, e, i + 1 + m, r, false, ty, BSPseudo(code, acc)))
      else _compound_loop(src, e, i + 1 + m, r, false, ty, BSPseudo(code, acc))
    end
  end

(* The arguments of a functional pseudo-class, whose parenthesis may be at q *)
and _pseudo_args {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{q:nat | i < q; q <= e}{r:int}{s:nat} .<e - q, 0>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, q: int q, code: int, r: int r, ty: bool,
   acc: book_simples(cs, n, r, s))
  : [t:nat][j:nat | i <= j; j <= e] @(book_simples(cs, n, r, t), int j, int, bool) =
  if q >= e then @(acc, i, ~1, false)
  else let
    val @(p0 | c) = _byte(src, q)
  in
    if c = 40 then let
      val a = _skip_trivia(src, e, q + 1)
    in
      if code = 31 then
        (if r = 1 then _pseudo_not(src, e, i, a, ty, acc) else @(acc, i, ~1, false))
      else if code = 32 then let
        val @(pi | m) = _scan_ident(src, e, a)
      in
        if m >= 1 then let
          prval IDENT_OPT_some(id) = pi
          val w = _skip_trivia(src, e, a + m)
        in
          if w >= e then @(acc, i, ~1, false)
          else let
            val @(p2 | cw) = _byte(src, w)
          in
            if cw = 41 then _compound_loop(src, e, w + 1, r, false, ty, BSLang(id | a, m, acc))
            else @(acc, i, ~1, false)
          end
        end
        else let prval IDENT_OPT_none() = pi in @(acc, i, ~1, false) end
      end
      else let
        val @(na, nb, j, ok) = _nth_read(src, e, a)
        val w = _skip_trivia(src, e, j)
      in
        if ok then
          (if w >= e then @(acc, i, ~1, false)
           else let
             val @(p2 | cw) = _byte(src, w)
           in
             if cw = 41 then
               (if code >= 35 then
                  (if ty then _compound_loop(src, e, w + 1, r, false, ty, BSNth(code - 33, na, nb, acc))
                   else @(acc, i, ~1, false))
                else _compound_loop(src, e, w + 1, r, false, ty, BSNth(code - 33, na, nb, acc)))
             else @(acc, i, ~1, false)
           end)
        else @(acc, i, ~1, false)
      end
    end
    else @(acc, i, ~1, false)
  end

(* :not( a compound ) from a: the compound is read as the top of a selector is,
   without a pseudo-element or a nested :not *)
and _pseudo_not {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{a:nat | i < a; a <= e}{s:nat} .<e - a, 2>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, a: int a, ty: bool, acc: book_simples(cs, n, 1, s))
  : [t:nat][j:nat | i <= j; j <= e] @(book_simples(cs, n, 1, t), int j, int, bool) = let
  val @(inner0, j, pe, ok) = _compound_loop(src, e, a, 0, true, false, BSimplesNil())
  val inner = _simples_rev(inner0, BSimplesNil())
in
  if ok then
    (if pe < 0 then
       (if j > a then let
          val w = _skip_trivia(src, e, j)
        in
          if w >= e then let val () = _free_simples(inner) in @(acc, i, ~1, false) end
          else let
            val @(p2 | cw) = _byte(src, w)
          in
            if cw = 41 then _compound_loop(src, e, w + 1, 1, false, ty, BSNot(inner, acc))
            else let val () = _free_simples(inner) in @(acc, i, ~1, false) end
          end
        end
        else let val () = _free_simples(inner) in @(acc, i, ~1, false) end)
     else let val () = _free_simples(inner) in @(acc, i, ~1, false) end)
  else let val () = _free_simples(inner) in @(acc, i, ~1, false) end
end

fun _steps_rev {n:nat}{cs:$A.cells}{a,b:nat} .<a>.
  (xs: book_steps(cs, n, a), acc: book_steps(cs, n, b)): book_steps(cs, n, a + b) =
  case+ xs of
  | ~BStepsNil() => acc
  | ~BStep(c, pe, simples, rest) => _steps_rev(rest, BStep(c, pe, simples, acc))

fun _sels_rev {n:nat}{cs:$A.cells}{a,b:nat} .<a>.
  (xs: book_sels(cs, n, a), acc: book_sels(cs, n, b)): book_sels(cs, n, a + b) =
  case+ xs of
  | ~BSelsNil() => acc
  | ~BSel(steps, rest) => _sels_rev(rest, BSel(steps, acc))

(* The steps of a complex selector from i (the compound there is the one that the
   combinator comb, 0 for the first, comes before; plus counts the + combinators
   met: a selector with two is dropped, design 3.5). The result: the steps in
   reverse order, where the selector ends (a comma, or the end), and whether it
   is a kept one *)
fun _steps_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{s:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, comb: int, plus: int, acc: book_steps(cs, n, s))
  : [t:nat][j:nat | i <= j; j <= e] @(book_steps(cs, n, t), int j, bool) = let
  val @(simples0, j, pe, ok) = _compound_loop(src, e, i, 1, true, false, BSimplesNil())
  val simples = _simples_rev(simples0, BSimplesNil())
in
  if ok then
    (if j > i then let
       val t = _skip_trivia(src, e, j)
     in
       if t >= e then @(BStep(comb, pe, simples, acc), t, true)
       else let
         val @(p0 | c) = _byte(src, t)
       in
         if pe >= 0 then
           (if c = 44 then @(BStep(comb, pe, simples, acc), t, true)
            else let val () = _free_simples(simples) in @(acc, i, false) end)
         else if c = 44 then @(BStep(comb, pe, simples, acc), t, true)
         else if c = 62 then
           _steps_loop(src, e, _skip_trivia(src, e, t + 1), 2, plus, BStep(comb, pe, simples, acc))
         else if c = 126 then
           _steps_loop(src, e, _skip_trivia(src, e, t + 1), 4, plus, BStep(comb, pe, simples, acc))
         else if c = 43 then
           (if plus = 0 then
              _steps_loop(src, e, _skip_trivia(src, e, t + 1), 3, 1, BStep(comb, pe, simples, acc))
            else let val () = _free_simples(simples) in @(acc, i, false) end)
         else if t > j then _steps_loop(src, e, t, 1, plus, BStep(comb, pe, simples, acc))
         else let val () = _free_simples(simples) in @(acc, i, false) end
       end
     end
     else let val () = _free_simples(simples) in @(acc, i, false) end)
  else let val () = _free_simples(simples) in @(acc, i, false) end
end

(* The selector list from i up to e, in order: the selectors that are kept *)
fun _sels_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{s:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, acc: book_sels(cs, n, s))
  : [t:nat] book_sels(cs, n, t) = let
  val i1 = _skip_trivia(src, e, i)
in
  if i1 >= e then _sels_rev(acc, BSelsNil())
  else let
    val @(steps, j, ok) = _steps_loop(src, e, i1, 0, 0, BStepsNil())
  in
    if ok then let
      val acc2 = BSel(_steps_rev(steps, BStepsNil()), acc)
    in
      if j >= e then _sels_rev(acc2, BSelsNil())
      else _sels_loop(src, e, j + 1, acc2)
    end
    else let
      val () = _free_steps(steps)
      val k = _to_top_comma(src, e, i1 + 1, 0)
    in _sels_loop(src, e, k, acc) end
  end
end

(* ============================================================
   At-rules: conditions, imports, font faces
   ============================================================ *)

dataprop QMAYBE(cs: $A.cells, o: int, k: int, ok: bool) =
  | {cs:$A.cells}{o,k:int} QMAYBE_none(cs, o, k, false)
  | {cs:$A.cells}{o,k:int} QMAYBE_some(cs, o, k, true) of QUERY_PARTIAL(cs, o, k)

(* The ) that closes the parenthesis that is open before i, or e *)
fun _paren_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{pd:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, pd: int pd): [j:nat | i <= j; j <= e] int j =
  if i >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 41 then (if pd = 0 then i else _paren_end(src, e, i + 1, pd - 1))
    else if c = 40 then _paren_end(src, e, i + 1, pd + 1)
    else if c = 34 then _paren_end(src, e, _string_end(src, e, 34, i + 1), pd)
    else if c = 39 then _paren_end(src, e, _string_end(src, e, 39, i + 1), pd)
    else _paren_end(src, e, i + 1, pd)
  end

(* The words a query may have outside a condition: the media types that apply
   to a screen and the logical words (kind 0 is @media, 1 is @supports) *)
fn _query_word_ok {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, kind: int): bool =
  if _span_is(src, o, k, "and") then true
  else if _span_is(src, o, k, "not") then true
  else if kind = 0 then
    (if _span_is(src, o, k, "screen") then true
     else if _span_is(src, o, k, "all") then true
     else _span_is(src, o, k, "only"))
  else _span_is(src, o, k, "or")

(* The row whose grammar reads the value of a media feature, or ~1: min- and
   max- of width, height and aspect-ratio, and orientation (resolution is not
   kept: its units are not) *)
fn _media_row {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, g: int): [r:int | r >= ~1] int r =
  if _span_is(src, o, k, "width") then _find_str_from("width", g, 0)
  else if _span_is(src, o, k, "min-width") then _find_str_from("width", g, 0)
  else if _span_is(src, o, k, "max-width") then _find_str_from("width", g, 0)
  else if _span_is(src, o, k, "height") then _find_str_from("height", g, 0)
  else if _span_is(src, o, k, "min-height") then _find_str_from("height", g, 0)
  else if _span_is(src, o, k, "max-height") then _find_str_from("height", g, 0)
  else if _span_is(src, o, k, "aspect-ratio") then _find_str_from("aspect-ratio", g, 0)
  else if _span_is(src, o, k, "min-aspect-ratio") then _find_str_from("aspect-ratio", g, 0)
  else if _span_is(src, o, k, "max-aspect-ratio") then _find_str_from("aspect-ratio", g, 0)
  else if _span_is(src, o, k, "orientation") then _find_str_from("font-style", g, 0)
  else ~1

(* The row of a feature's value: a media feature (kind 0) or a property of the
   grammar g (@supports) *)
fn _query_row {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, kind: int, g: int): [r:int | r >= ~1] int r =
  if kind = 0 then _media_row(src, o, k, g) else _prop_find(src, o, k, g)

(* A query from o + k to e: words, white space and (feature: value) conditions *)
fnx _query_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat | o + k <= e}{g:int} .<e - o - k, 1>.
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, kind: int, g: int g,
   light: rgb256, sepia: rgb256, pf: QUERY_PARTIAL(cs, o, k))
  : [m:nat | o + m <= e][ok:bool] (QMAYBE(cs, o, m, ok) | int m, bool ok) =
  if o + k >= e then (QMAYBE_some(pf) | k, true)
  else let
    val @(ps | m) = _scan_spaces(src, e, o + k)
  in
    if m >= 1 then _query_loop(src, e, o, k + m, kind, g, light, sepia, QP_space(pf, ps))
    else let
      val @(p0 | c) = _byte(src, o + k)
    in
      if c = 40 then _query_cond(src, e, o, k, kind, g, light, sepia, pf, p0)
      else let
        val @(pi | w) = _scan_ident(src, e, o + k)
      in
        if w >= 1 then let
          prval IDENT_OPT_some(id) = pi
        in
          if _query_word_ok(src, o + k, w, kind) then
            _query_loop(src, e, o, k + w, kind, g, light, sepia, QP_ident(pf, id))
          else (QMAYBE_none() | k, false)
        end
        else let prval IDENT_OPT_none() = pi in (QMAYBE_none() | k, false) end
      end
    end
  end

(* A condition whose parenthesis is at o + k *)
and _query_cond {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{o:nat}{k:nat | o + k < e}{g:int}{c:int} .<e - o - k, 0>.
  (src: !$A.cborrow(l, n, cs), e: int e, o: int o, k: int k, kind: int, g: int g,
   light: rgb256, sepia: rgb256, pf: QUERY_PARTIAL(cs, o, k), p40: $A.NTH(cs, o + k, 40))
  : [m:nat | o + m <= e][ok:bool] (QMAYBE(cs, o, m, ok) | int m, bool ok) = let
  val @(pi | a) = _scan_ident(src, e, o + k + 1)
in
  if a >= 1 then let
    prval IDENT_OPT_some(id) = pi
  in
    if o + k + 1 + a < e then let
      val @(p58 | c) = _byte(src, o + k + 1 + a)
    in
      if c = 58 then let
        val @(ps | s) = _scan_spaces(src, e, o + k + 2 + a)
      in
        if o + k + 2 + a + s < e then let
          val j = _paren_end(src, e, o + k + 2 + a + s, 0)
        in
          if j < e then
            (if j > o + k + 2 + a + s then let
               val row = _query_row(src, o + k + 1, a, kind, g)
             in
               if row >= 0 then let
                 val @(pv | ok) = _read_value(src, o + k + 2 + a + s, j - (o + k + 2 + a + s), g, row,
                                              _prop_mask(row, g), light, sepia)
               in
                 if ok then let
                   prval VALUE_MAYBE_some(vs) = pv
                   val @(p41 | c41) = _byte(src, j)
                 in
                   if c41 = 41 then
                     _query_loop(src, e, o, k + a + s + (j - (o + k + 2 + a + s)) + 3, kind, g, light, sepia,
                                 QP_condition(pf, p40, id, p58, ps, vs, p41))
                   else (QMAYBE_none() | k, false)
                 end
                 else let prval VALUE_MAYBE_none() = pv in (QMAYBE_none() | k, false) end
               end
               else (QMAYBE_none() | k, false)
             end
             else (QMAYBE_none() | k, false))
          else (QMAYBE_none() | k, false)
        end
        else (QMAYBE_none() | k, false)
      end
      else (QMAYBE_none() | k, false)
    end
    else (QMAYBE_none() | k, false)
  end
  else let prval IDENT_OPT_none() = pi in (QMAYBE_none() | k, false) end
end

(* The end of a bare url( ... ) target from i: a parenthesis, a space or a semicolon *)
fun _bare_end {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [j:nat | i <= j; j <= e] int j =
  if i >= e then e
  else let
    val @(p0 | c) = _byte(src, i)
  in
    if c = 41 then i
    else if c = 59 then i
    else if c <= 32 then i
    else _bare_end(src, e, i + 1)
  end

(* One past the closing parenthesis at j, or e when there is none *)
fn _past {e:nat}{j:nat | j <= e} (j: int j, e: int e): [r:nat | j <= r; r <= e] int r =
  if j < e then j + 1 else e

(* The path of the url( at s, with the end of the function: (o, k, j) *)
fn _url_at {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{s:nat | s + 4 <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, s: int s)
  : [o:nat][k:nat][j:nat | s + 4 <= j; j <= e; o + k <= e] @(int o, int k, int j) = let
  val t = _skip_trivia(src, e, s + 4)
in
  if t >= e then @(t, 0, e)
  else let
    val @(p0 | c) = _byte(src, t)
  in
    if c = 34 then let
      val @(o, k) = _quoted_span(src, e, t, 34)
      val j = _paren_end(src, e, t + 1, 0)
    in let val r = _past(j, e) in @(o, k, r) end end
    else if c = 39 then let
      val @(o, k) = _quoted_span(src, e, t, 39)
      val j = _paren_end(src, e, t + 1, 0)
    in let val r = _past(j, e) in @(o, k, r) end end
    else let
      val b = _bare_end(src, e, t)
      val j = _paren_end(src, e, b, 0)
    in let val r = _past(j, e) in @(t, b - t, r) end end
  end
end

(* The target of an @import from i: a string or a url(), the span of its path
   (k is 0 when there is none) *)
fn _import_target {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i): [o:nat][k:nat | o + k <= e] @(int o, int k) = let
  val s = _skip_trivia(src, e, i)
in
  if s >= e then @(s, 0)
  else let
    val @(p0 | c) = _byte(src, s)
  in
    if c = 34 then _quoted_span(src, e, s, 34)
    else if c = 39 then _quoted_span(src, e, s, 39)
    else if s + 4 <= e then
      (if _span_is(src, s, 4, "url(") then let
         val @(o, k, j) = _url_at(src, e, s)
       in @(o, k) end
       else @(s, 0))
    else @(s, 0)
  end
end

(* After a statement whose prelude ended at pe: a semicolon is past, a block
   is skipped, a closing brace is left *)
fn _after_stmt {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{pe:nat | pe <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, pe: int pe): [j:nat | pe <= j; j <= e] int j =
  if pe >= e then e
  else let
    val @(p0 | c) = _byte(src, pe)
  in
    if c = 123 then _block_end(src, e, pe + 1, 1)
    else if c = 59 then pe + 1
    else pe
  end

fn _import_add {n:nat}{b:nat}{o,k:nat | o + k <= n} (o: int o, k: int k, xs: book_imports(n, b)): [r:nat] book_imports(n, r) =
  if k >= 1 then BImportsCons(o, k, xs) else xs

fn _src_add {n:nat}{b:nat}{o,k:nat | o + k <= n} (o: int o, k: int k, xs: book_srcs(n, b)): [r:nat] book_srcs(n, r) =
  if k >= 1 then BSrcsCons(o, k, xs) else xs

(* The paths of the url() functions in a value from i to e *)
fun _srcs_collect {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{s:nat} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, acc: book_srcs(n, s)): [t:nat] book_srcs(n, t) =
  if i + 4 > e then acc
  else if _span_is(src, i, 4, "url(") then let
    val @(o, k, j) = _url_at(src, e, i)
  in _srcs_collect(src, e, j, _src_add(o, k, acc)) end
  else _srcs_collect(src, e, i + 1, acc)

fn _srcs_nonempty {n:nat}{t:nat} (xs: !book_srcs(n, t)): bool =
  case+ xs of
  | BSrcsNil() => false
  | BSrcsCons(_, _, _) => true

(* The first family a @font-face names: the text of a string, or the words *)
fn _face_family {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{vs:nat | vs <= e}
  (src: !$A.cborrow(l, n, cs), e: int e, vs: int vs): [o:nat][k:nat | o + k <= e] @(int o, int k) =
  if vs >= e then @(vs, 0)
  else let
    val @(p0 | c) = _byte(src, vs)
  in
    if c = 34 then _quoted_span(src, e, vs, 34)
    else if c = 39 then _quoted_span(src, e, vs, 39)
    else let
      val @(pi | m) = _scan_ident(src, e, vs)
    in
      if m >= 1 then let val w = _words_end(src, e, vs + m) in @(vs, w - vs) end
      else @(vs, 0)
    end
  end

(* A weight or a style descriptor: proved as the value of the property of row p *)
fn _descriptor {l:agz}{n:nat}{cs:$A.cells}{vs:nat}{ve:nat | vs <= ve; ve <= n}{g:int}{p:nat}
  (src: !$A.cborrow(l, n, cs), vs: int vs, ve: int ve, g: int g, p: int p, light: rgb256, sepia: rgb256,
   old: book_descriptor(cs, n, g)): book_descriptor(cs, n, g) = let
  val () = _free_descriptor(old)
  val @(pv | ok) = _read_value(src, vs, ve - vs, g, p, _prop_mask(p, g), light, sepia)
in
  if ok then let
    prval VALUE_MAYBE_some(pf) = pv
  in BDescriptor(pf | vs, ve - vs) end
  else let prval VALUE_MAYBE_none() = pv in BDescriptorNone() end
end

(* The declarations of a @font-face from i up to its closing brace: the family,
   the paths of the sources, the weight and the style *)
fun _face_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{g:int}{s:nat}{fo,fk:nat | fo + fk <= n} .<e - i>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, g: int g, light: rgb256, sepia: rgb256,
   fo: int fo, fk: int fk, srcs: book_srcs(n, s), w: book_descriptor(cs, n, g), st: book_descriptor(cs, n, g))
  : [j:nat | i <= j; j <= e][t:nat][fo2,fk2:nat | fo2 + fk2 <= n] @(int fo2, int fk2, book_srcs(n, t), book_descriptor(cs, n, g), book_descriptor(cs, n, g), int j) = let
  val i1 = _skip_semis(src, e, i)
in
  if i1 >= e then @(fo, fk, srcs, w, st, e)
  else let
    val @(p0 | c) = _byte(src, i1)
  in
    if c = 125 then @(fo, fk, srcs, w, st, i1 + 1)
    else let
      val @(d, nested) = _decl_first(src, e, i1)
    in
      if nested then _face_loop(src, e, d, g, light, sepia, fo, fk, srcs, w, st)
      else let
        val @(pi | m) = _scan_ident(src, d, i1)
        val c1 = _skip_trivia(src, d, i1 + m)
      in
        if m >= 1 then
          (if c1 < d then let
             val @(pc | cc) = _byte(src, c1)
           in
             if cc = 58 then let
               val vs = _skip_trivia(src, d, c1 + 1)
               val ve = _strip_important(src, vs, d)
             in
               if _span_is(src, i1, m, "font-family") then let
                 val @(o, k) = _face_family(src, ve, vs)
               in _face_loop(src, e, d, g, light, sepia, o, k, srcs, w, st) end
               else if _span_is(src, i1, m, "src") then let
                 val srcs2 = _srcs_collect(src, ve, vs, srcs)
               in _face_loop(src, e, d, g, light, sepia, fo, fk, srcs2, w, st) end
               else if _span_is(src, i1, m, "font-weight") then
                 _face_loop(src, e, d, g, light, sepia, fo, fk, srcs, _descriptor(src, vs, ve, g, 2, light, sepia, w), st)
               else if _span_is(src, i1, m, "font-style") then
                 _face_loop(src, e, d, g, light, sepia, fo, fk, srcs, w, _descriptor(src, vs, ve, g, 1, light, sepia, st))
               else _face_loop(src, e, d, g, light, sepia, fo, fk, srcs, w, st)
             end
             else _face_loop(src, e, d, g, light, sepia, fo, fk, srcs, w, st)
           end
           else _face_loop(src, e, d, g, light, sepia, fo, fk, srcs, w, st))
        else _face_loop(src, e, d, g, light, sepia, fo, fk, srcs, w, st)
      end
    end
  end
end

(* A face is listed when it has a family and a source *)
fn _face_add {n:nat}{cs:$A.cells}{g:int}{s,c:nat}{fo,fk:nat | fo + fk <= n}
  (fo: int fo, fk: int fk, srcs: book_srcs(n, s), w: book_descriptor(cs, n, g), st: book_descriptor(cs, n, g),
   faces: book_faces(cs, n, g, c)): [r:nat] book_faces(cs, n, g, r) =
  if fk >= 1 then
    (if _srcs_nonempty(srcs) then BFace(fo, fk, srcs, w, st, faces)
     else let
       val () = _free_srcs(srcs)
       val () = _free_descriptor(w)
       val () = _free_descriptor(st)
     in faces end)
  else let
    val () = _free_srcs(srcs)
    val () = _free_descriptor(w)
    val () = _free_descriptor(st)
  in faces end

fn _sels_nonempty {n:nat}{cs:$A.cells}{s:nat} (xs: !book_sels(cs, n, s)): bool =
  case+ xs of
  | BSelsNil() => false
  | BSel(_, _) => true

fn _decls_nonempty {n:nat}{cs:$A.cells}{g:int}{s:nat} (xs: !book_decls(cs, n, g, s)): bool =
  case+ xs of
  | BDeclsNil() => false
  | BDecl(_ | _, _, _, _) => true
  | BDeclFamily(_, _) => true

fn _rule_add {n:nat}{cs:$A.cells}{g:int}{a,b,c:nat}
  (sels: book_sels(cs, n, a), decls: book_decls(cs, n, g, b), rules: book_rules(cs, n, g, c))
  : [r:nat] book_rules(cs, n, g, r) =
  if _sels_nonempty(sels) then
    (if _decls_nonempty(decls) then BRule(sels, decls, rules)
     else let
       val () = _free_sels(sels)
       val () = _free_decls(decls)
     in rules end)
  else let
    val () = _free_sels(sels)
    val () = _free_decls(decls)
  in rules end

(* The three lists put in order, with the end *)
fn _rules_done {n:nat}{cs:$A.cells}{g:int}{a,b,c:nat}{j:nat}
  (rules: book_rules(cs, n, g, a), imps: book_imports(n, b), faces: book_faces(cs, n, g, c), j: int j)
  : [a2,b2,c2:nat] @(book_rules(cs, n, g, a2), book_imports(n, b2), book_faces(cs, n, g, c2), int j) =
  @(_rules_rev(rules, BRulesNil()), _imports_rev(imps, BImportsNil()), _faces_rev(faces, BFacesNil()), j)

(* The rules of a sheet (d is 0), or of the block of an @media or @supports (d is 1,
   which ends at the closing brace) from i, with the targets of the @imports
   and the @font-face rules. Only a sheet's own rules take at-rules. *)
fnx _rules_loop {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i <= e}{g:int}{a,b,c:nat} .<e - i, 3>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, g: int g, d: int, light: rgb256, sepia: rgb256,
   rules: book_rules(cs, n, g, a), imps: book_imports(n, b), faces: book_faces(cs, n, g, c))
  : [j:nat | i <= j; j <= e][a2,b2,c2:nat]
    @(book_rules(cs, n, g, a2), book_imports(n, b2), book_faces(cs, n, g, c2), int j) = let
  val i1 = _skip_trivia(src, e, i)
in
  if i1 >= e then _rules_done(rules, imps, faces, i1)
  else let
    val @(p0 | c) = _byte(src, i1)
  in
    if c = 125 then
      (if d = 1 then _rules_done(rules, imps, faces, i1 + 1)
       else _rules_loop(src, e, i1 + 1, g, d, light, sepia, rules, imps, faces))
    else if c = 59 then _rules_loop(src, e, i1 + 1, g, d, light, sepia, rules, imps, faces)
    else if c = 60 then
      (if i1 + 4 <= e then
         (if _span_is(src, i1, 4, "<!--") then _rules_loop(src, e, i1 + 4, g, d, light, sepia, rules, imps, faces)
          else _rule(src, e, i1, g, d, light, sepia, rules, imps, faces))
       else _rule(src, e, i1, g, d, light, sepia, rules, imps, faces))
    else if c = 45 then
      (if i1 + 3 <= e then
         (if _span_is(src, i1, 3, "-->") then _rules_loop(src, e, i1 + 3, g, d, light, sepia, rules, imps, faces)
          else _rule(src, e, i1, g, d, light, sepia, rules, imps, faces))
       else _rule(src, e, i1, g, d, light, sepia, rules, imps, faces))
    else if c = 64 then _at_rule(src, e, i1, g, d, light, sepia, rules, imps, faces)
    else _rule(src, e, i1, g, d, light, sepia, rules, imps, faces)
  end
end

(* A qualified rule that starts at i < e *)
and _rule {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i < e}{g:int}{a,b,c:nat} .<e - i, 2>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, g: int g, d: int, light: rgb256, sepia: rgb256,
   rules: book_rules(cs, n, g, a), imps: book_imports(n, b), faces: book_faces(cs, n, g, c))
  : [j:nat | i <= j; j <= e][a2,b2,c2:nat]
    @(book_rules(cs, n, g, a2), book_imports(n, b2), book_faces(cs, n, g, c2), int j) = let
  val pe = _prelude_end(src, e, i, 0)
in
  if pe >= e then _rules_done(rules, imps, faces, e)
  else let
    val @(p0 | c) = _byte(src, pe)
  in
    if c = 123 then let
      val sels = _sels_loop(src, pe, i, BSelsNil())
      val @(decls, j) = _decls_loop(src, e, pe + 1, g, light, sepia, BDeclsNil())
      val rules2 = _rule_add(sels, decls, rules)
    in _rules_loop(src, e, j, g, d, light, sepia, rules2, imps, faces) end
    else if c = 59 then _rules_loop(src, e, pe + 1, g, d, light, sepia, rules, imps, faces)
    else if pe > i then _rules_loop(src, e, pe, g, d, light, sepia, rules, imps, faces)
    else _rules_loop(src, e, pe + 1, g, d, light, sepia, rules, imps, faces)
  end
end

(* An at-rule whose at sign is at i *)
and _at_rule {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat | i < e}{g:int}{a,b,c:nat} .<e - i, 2>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, g: int g, d: int, light: rgb256, sepia: rgb256,
   rules: book_rules(cs, n, g, a), imps: book_imports(n, b), faces: book_faces(cs, n, g, c))
  : [j:nat | i <= j; j <= e][a2,b2,c2:nat]
    @(book_rules(cs, n, g, a2), book_imports(n, b2), book_faces(cs, n, g, c2), int j) = let
  val @(pi | m) = _scan_ident(src, e, i + 1)
  val q = i + 1 + m
in
  if d = 0 then
    (if _span_is(src, i + 1, m, "import") then let
       val @(io, ik) = _import_target(src, e, q)
       val pe = _prelude_end(src, e, q, 0)
       val j = _after_stmt(src, e, pe)
     in _rules_loop(src, e, j, g, d, light, sepia, rules, _import_add(io, ik, imps), faces) end
     else if _span_is(src, i + 1, m, "font-face") then _font_face(src, e, i, q, g, d, light, sepia, rules, imps, faces)
     else if _span_is(src, i + 1, m, "media") then _condition(src, e, i, q, 0, g, d, light, sepia, rules, imps, faces)
     else if _span_is(src, i + 1, m, "supports") then _condition(src, e, i, q, 1, g, d, light, sepia, rules, imps, faces)
     else _at_skip(src, e, i, q, g, d, light, sepia, rules, imps, faces))
  else _at_skip(src, e, i, q, g, d, light, sepia, rules, imps, faces)
end

(* An at-rule that is dropped with its block or up to its semicolon *)
and _at_skip {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{q:nat | i < q; q <= e}{g:int}{a,b,c:nat} .<e - i, 1>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, q: int q, g: int g, d: int, light: rgb256, sepia: rgb256,
   rules: book_rules(cs, n, g, a), imps: book_imports(n, b), faces: book_faces(cs, n, g, c))
  : [j:nat | i <= j; j <= e][a2,b2,c2:nat]
    @(book_rules(cs, n, g, a2), book_imports(n, b2), book_faces(cs, n, g, c2), int j) = let
  val pe = _prelude_end(src, e, q, 0)
  val j = _after_stmt(src, e, pe)
in _rules_loop(src, e, j, g, d, light, sepia, rules, imps, faces) end

(* @font-face: a block of descriptors *)
and _font_face {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{q:nat | i < q; q <= e}{g:int}{a,b,c:nat} .<e - i, 1>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, q: int q, g: int g, d: int, light: rgb256, sepia: rgb256,
   rules: book_rules(cs, n, g, a), imps: book_imports(n, b), faces: book_faces(cs, n, g, c))
  : [j:nat | i <= j; j <= e][a2,b2,c2:nat]
    @(book_rules(cs, n, g, a2), book_imports(n, b2), book_faces(cs, n, g, c2), int j) = let
  val pe = _prelude_end(src, e, q, 0)
in
  if pe >= e then _rules_done(rules, imps, faces, e)
  else let
    val @(p0 | c) = _byte(src, pe)
  in
    if c = 123 then let
      val @(fo, fk, srcs, w, st, j) =
        _face_loop(src, e, pe + 1, g, light, sepia, 0, 0, BSrcsNil(), BDescriptorNone(), BDescriptorNone())
      val faces2 = _face_add(fo, fk, srcs, w, st, faces)
    in _rules_loop(src, e, j, g, d, light, sepia, rules, imps, faces2) end
    else let val j = _after_stmt(src, e, pe)
    in _rules_loop(src, e, j, g, d, light, sepia, rules, imps, faces) end
  end
end

(* @media and @supports: the rules of the block, kept when the condition is *)
and _condition {l:agz}{n:nat}{cs:$A.cells}{e:nat | e <= n}{i:nat}{q:nat | i < q; q <= e}{g:int}{a,b,c:nat} .<e - i, 1>.
  (src: !$A.cborrow(l, n, cs), e: int e, i: int i, q: int q, kind: int, g: int g, d: int,
   light: rgb256, sepia: rgb256,
   rules: book_rules(cs, n, g, a), imps: book_imports(n, b), faces: book_faces(cs, n, g, c))
  : [j:nat | i <= j; j <= e][a2,b2,c2:nat]
    @(book_rules(cs, n, g, a2), book_imports(n, b2), book_faces(cs, n, g, c2), int j) = let
  val a0 = _skip_trivia(src, e, q)
  val pe = _prelude_end(src, e, a0, 0)
in
  if pe >= e then _rules_done(rules, imps, faces, e)
  else let
    val @(p0 | c) = _byte(src, pe)
  in
    if c = 123 then let
      val @(pq | qm, qok) = _query_loop(src, pe, a0, 0, kind, g, light, sepia, QP_nil())
    in
      if qok then
        (if qm >= 1 then let
           prval QMAYBE_some(qp) = pq
           val @(inner, imps0, faces0, j) =
             _rules_loop(src, e, pe + 1, g, 1, light, sepia, BRulesNil(), BImportsNil(), BFacesNil())
           val () = _free_imports(imps0)
           val () = _free_faces(faces0)
         in
           _rules_loop(src, e, j, g, d, light, sepia,
                       BCondition(QUERY_SAFE_mk(qp) | kind, a0, qm, inner, rules), imps, faces)
         end
         else let prval QMAYBE_some(qp) = pq
              in _rules_loop(src, e, _block_end(src, e, pe + 1, 1), g, d, light, sepia, rules, imps, faces) end)
      else let prval QMAYBE_none() = pq
           in _rules_loop(src, e, _block_end(src, e, pe + 1, 1), g, d, light, sepia, rules, imps, faces) end
    end
    else let val j = _after_stmt(src, e, pe)
    in _rules_loop(src, e, j, g, d, light, sepia, rules, imps, faces) end
  end
end

(* ============================================================
   The reader's entry points
   ============================================================ *)

(* Reads a book's style sheet of len bytes (of the n of the borrow) in the grammar g (1 reflow, 2 fixed,
   3 SVG paint). light and sepia are the app's two reading grounds: a text
   colour is kept in reflow only if its contrast with both is at least 4.5.
   Nothing is copied: the tree holds spans of the bytes. *)
#pub fun book_sheet_read {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{g:int | 1 <= g; g <= 3}
  (src: !$A.cborrow(l, n, cs), len: int len, g: int g, light: rgb256, sepia: rgb256): book_read(cs, n, g)

implement book_sheet_read {l}{n}{cs}{len}{g} (src, len, g, light, sepia) = let
  val @(rules, imps, faces, j) =
    _rules_loop(src, len, 0, g, 0, light, sepia, BRulesNil(), BImportsNil(), BFacesNil())
in BookRead(rules, imps, faces) end

(* Reads the declarations of a style attribute of n bytes *)
#pub fun book_style_read {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{g:int | 1 <= g; g <= 3}
  (src: !$A.cborrow(l, n, cs), len: int len, g: int g, light: rgb256, sepia: rgb256)
  : [r:nat] book_decls(cs, n, g, r)

implement book_style_read {l}{n}{cs}{len}{g} (src, len, g, light, sepia) = let
  val @(decls, j) = _decls_loop(src, len, 0, g, light, sepia, BDeclsNil())
in decls end

#pub fun book_decls_free {n:nat}{cs:$A.cells}{g:int}{r:nat} (xs: book_decls(cs, n, g, r)): void

implement book_decls_free {n}{cs}{g}{r} (xs) = _free_decls(xs)

(* ============================================================
   The refused list (book_refused): what bridge's check refuses in a sheet
   ============================================================ *)

fn _refused_lit {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{i:nat | i <= len}{m:nat}
  (src: !$A.cborrow(l, n, cs), len: int len, i: int i, lit: string(m)): bool =
  if i + _slen(lit) <= len then _span_is(src, i, _slen(lit), lit) else false

(* The table of the refused entries (keyword: refused table, bats-lang/css#88): the one
   list. book_refused reads it, and book_refused_count and book_refused_entry export it,
   so a consumer generates its check from the entries the predicate uses. An entry is
   lower case (the match is without regard to case) and at most 11 bytes. The count 17
   is in the type of book_refused_entry, in _refused_from_entry and in the getter's
   result: a row added here changes all three. *)
#pub fn book_refused_count (): int 17

implement book_refused_count () = 17

#pub fn book_refused_entry {i:nat | i < 17} (i: int i): [m:int | 1 <= m; m <= 11] string(m)

implement book_refused_entry {i} (i) =
  case+ i of
  | 0 => "\\"
  | 1 => "<"
  | 2 => "/*"
  | 3 => "!important"
  | 4 => "@import"
  | 5 => "@font-face"
  | 6 => "url("
  | 7 => "image-set("
  | 8 => "image("
  | 9 => "src("
  | 10 => "cross-fade("
  | 11 => "element("
  | 12 => "paint("
  | 13 => "env("
  | 14 => "var("
  | 15 => "attr("
  | _ => "expression("

(* Whether entry k of the table or a later one starts at i *)
fun _refused_from_entry {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{i:nat | i <= len}{k:nat | k <= 17} .<17 - k>.
  (src: !$A.cborrow(l, n, cs), len: int len, i: int i, k: int k): bool =
  if k >= 17 then false
  else if _refused_lit(src, len, i, book_refused_entry(k)) then true
  else _refused_from_entry(src, len, i, k + 1)

fun _refused_from {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}{i:nat | i <= len} .<len - i>.
  (src: !$A.cborrow(l, n, cs), len: int len, i: int i): bool =
  if i >= len then false
  else if _refused_from_entry(src, len, i, 0) then true
  else _refused_from(src, len, i + 1)

(* Whether the len bytes hold something that a book's sheet or style never may:
   url(, image-set(, image(, src(, cross-fade(, element(, paint(, env(, var(,
   attr(, expression(, @import, @font-face, !important, a backslash, a left
   angle bracket or a comment opener, without regard to case. Bridge's check
   is made from this list; the sheets the walk emits are tested against it. *)
#pub fun book_refused {l:agz}{n:nat}{cs:$A.cells}{len:nat | len <= n}
  (src: !$A.cborrow(l, n, cs), len: int len): bool

implement book_refused {l}{n}{cs}{len} (src, len) = _refused_from(src, len, 0)

(* ============================================================
   Element names: the HTML tags a book element can be made as (dom's book_tag,
   which must agree: any other name is made as a div or a span)
   ============================================================ *)

fn _tag_known_a {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): bool =
  if _span_is(src, o, k, "a") then true
  else if _span_is(src, o, k, "abbr") then true
  else if _span_is(src, o, k, "address") then true
  else if _span_is(src, o, k, "article") then true
  else if _span_is(src, o, k, "aside") then true
  else if _span_is(src, o, k, "audio") then true
  else if _span_is(src, o, k, "b") then true
  else if _span_is(src, o, k, "bdi") then true
  else if _span_is(src, o, k, "bdo") then true
  else if _span_is(src, o, k, "blockquote") then true
  else if _span_is(src, o, k, "br") then true
  else if _span_is(src, o, k, "caption") then true
  else if _span_is(src, o, k, "cite") then true
  else if _span_is(src, o, k, "code") then true
  else if _span_is(src, o, k, "col") then true
  else if _span_is(src, o, k, "colgroup") then true
  else if _span_is(src, o, k, "data") then true
  else if _span_is(src, o, k, "dd") then true
  else if _span_is(src, o, k, "del") then true
  else if _span_is(src, o, k, "details") then true
  else if _span_is(src, o, k, "dfn") then true
  else if _span_is(src, o, k, "div") then true
  else if _span_is(src, o, k, "dl") then true
  else if _span_is(src, o, k, "dt") then true
  else if _span_is(src, o, k, "em") then true
  else if _span_is(src, o, k, "figcaption") then true
  else if _span_is(src, o, k, "figure") then true
  else if _span_is(src, o, k, "footer") then true
  else if _span_is(src, o, k, "h1") then true
  else if _span_is(src, o, k, "h2") then true
  else if _span_is(src, o, k, "h3") then true
  else if _span_is(src, o, k, "h4") then true
  else if _span_is(src, o, k, "h5") then true
  else if _span_is(src, o, k, "h6") then true
  else if _span_is(src, o, k, "header") then true
  else if _span_is(src, o, k, "hgroup") then true
  else if _span_is(src, o, k, "hr") then true
  else false

fn _tag_known_b {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): bool =
  if _span_is(src, o, k, "i") then true
  else if _span_is(src, o, k, "img") then true
  else if _span_is(src, o, k, "ins") then true
  else if _span_is(src, o, k, "kbd") then true
  else if _span_is(src, o, k, "li") then true
  else if _span_is(src, o, k, "main") then true
  else if _span_is(src, o, k, "mark") then true
  else if _span_is(src, o, k, "nav") then true
  else if _span_is(src, o, k, "ol") then true
  else if _span_is(src, o, k, "p") then true
  else if _span_is(src, o, k, "picture") then true
  else if _span_is(src, o, k, "pre") then true
  else if _span_is(src, o, k, "q") then true
  else if _span_is(src, o, k, "rp") then true
  else if _span_is(src, o, k, "rt") then true
  else if _span_is(src, o, k, "rtc") then true
  else if _span_is(src, o, k, "ruby") then true
  else if _span_is(src, o, k, "s") then true
  else if _span_is(src, o, k, "samp") then true
  else if _span_is(src, o, k, "section") then true
  else if _span_is(src, o, k, "small") then true
  else if _span_is(src, o, k, "source") then true
  else if _span_is(src, o, k, "span") then true
  else if _span_is(src, o, k, "strong") then true
  else if _span_is(src, o, k, "sub") then true
  else if _span_is(src, o, k, "summary") then true
  else if _span_is(src, o, k, "sup") then true
  else if _span_is(src, o, k, "table") then true
  else if _span_is(src, o, k, "tbody") then true
  else if _span_is(src, o, k, "td") then true
  else if _span_is(src, o, k, "tfoot") then true
  else if _span_is(src, o, k, "th") then true
  else if _span_is(src, o, k, "thead") then true
  else if _span_is(src, o, k, "time") then true
  else if _span_is(src, o, k, "tr") then true
  else if _span_is(src, o, k, "u") then true
  else if _span_is(src, o, k, "ul") then true
  else if _span_is(src, o, k, "var") then true
  else if _span_is(src, o, k, "video") then true
  else if _span_is(src, o, k, "wbr") then true
  else false

(* Whether the k bytes from o name a tag that a book element is made as itself *)
#pub fun book_tag_known {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k): bool

implement book_tag_known {l}{n}{cs}{o}{k} (src, o, k) =
  if _tag_known_a(src, o, k) then true else _tag_known_b(src, o, k)

(* ============================================================
   The walk
   The sheet is handed to a sink as pieces: literals from a closed set of this
   file (punctuation, property names, the rewritten parts of a selector), numbers,
   and spans of the sheet's bytes that carry a proof in the tree. dom supplies the
   sink and writes each piece into its stream; css never builds a byte buffer.
   ============================================================ *)

#pub fun{sink:vt@ype} book_lit {m:nat} (s: !sink, lit: string m): void

#pub fun{sink:vt@ype} book_span {l:agz}{n:nat}{cs:$A.cells}{o,k:nat | o + k <= n}
  (s: !sink, src: !$A.cborrow(l, n, cs), o: int o, k: int k): void

#pub fun{sink:vt@ype} book_num (s: !sink, v: int): void

#pub fun book_name_is {l:agz}{n:nat}{cs:$A.cells}{o:nat}{k:nat | o + k <= n}{m:nat}
  (src: !$A.cborrow(l, n, cs), o: int o, k: int k, lit: string m): bool

implement book_name_is {l}{n}{cs}{o}{k}{m} (src, o, k, lit) = _span_is(src, o, k, lit)

#pub fun book_prop_name (p: int): [m:nat] string(m)

implement book_prop_name (p) = _prop_name(p)

fn{sink:vt@ype} _w_pe (s: !sink, pe: int): void =
  if pe = 0 then book_lit<sink>(s, "::before")
  else if pe = 1 then book_lit<sink>(s, "::after")
  else if pe = 2 then book_lit<sink>(s, "::first-letter")
  else if pe = 3 then book_lit<sink>(s, "::first-line")
  else if pe = 4 then book_lit<sink>(s, "::marker")
  else ()

fn{sink:vt@ype} _w_comb (s: !sink, comb: int): void =
  if comb = 1 then book_lit<sink>(s, " ")
  else if comb = 2 then book_lit<sink>(s, " > ")
  else if comb = 4 then book_lit<sink>(s, " ~ ")
  else ()

fn{sink:vt@ype} _w_attr (s: !sink, code: int): void =
  if code = 1 then book_lit<sink>(s, "[lang")
  else if code = 2 then book_lit<sink>(s, "[dir")
  else if code = 3 then book_lit<sink>(s, "[title")
  else if code = 4 then book_lit<sink>(s, "[colspan")
  else if code = 5 then book_lit<sink>(s, "[rowspan")
  else if code = 6 then book_lit<sink>(s, "[hidden")
  else book_lit<sink>(s, "[data-book-epub-type")

fn{sink:vt@ype} _w_oper (s: !sink, oper: int): void =
  if oper = 0 then book_lit<sink>(s, "=")
  else if oper = 1 then book_lit<sink>(s, "~=")
  else if oper = 2 then book_lit<sink>(s, "|=")
  else if oper = 3 then book_lit<sink>(s, "^=")
  else if oper = 4 then book_lit<sink>(s, "$=")
  else book_lit<sink>(s, "*=")

(* The span of the type selector of a compound, with k = 0 when it has none *)
fun _type_span {cs:$A.cells}{n:nat}{r:nat}{t:nat} .<r, t>.
  (xs: !book_simples(cs, n, r, t)): [o,k:nat | o + k <= n] @(int o, int k) =
  case+ xs of
  | BSimplesNil() => @(0, 0)
  | BSType(_ | o, k, _) => @(o, k)
  | BSUniversal(rest) => _type_span(rest)
  | BSClass(_ | _, _, rest) => _type_span(rest)
  | BSId(_ | _, _, rest) => _type_span(rest)
  | BSLang(_ | _, _, rest) => _type_span(rest)
  | BSAttrIdent(_ | _, _, _, _, rest) => _type_span(rest)
  | BSAttrString(_ | _, _, _, _, rest) => _type_span(rest)
  | BSAttrExists(_, rest) => _type_span(rest)
  | BSPseudo(_, rest) => _type_span(rest)
  | BSNth(_, _, _, rest) => _type_span(rest)
  | BSNot(_, rest) => _type_span(rest)

fun _has_pseudo {cs:$A.cells}{n:nat}{r:nat}{t:nat} .<r, t>.
  (xs: !book_simples(cs, n, r, t), code: int): bool =
  case+ xs of
  | BSimplesNil() => false
  | BSType(_ | _, _, rest) => _has_pseudo(rest, code)
  | BSUniversal(rest) => _has_pseudo(rest, code)
  | BSClass(_ | _, _, rest) => _has_pseudo(rest, code)
  | BSId(_ | _, _, rest) => _has_pseudo(rest, code)
  | BSLang(_ | _, _, rest) => _has_pseudo(rest, code)
  | BSAttrIdent(_ | _, _, _, _, rest) => _has_pseudo(rest, code)
  | BSAttrString(_ | _, _, _, _, rest) => _has_pseudo(rest, code)
  | BSAttrExists(_, rest) => _has_pseudo(rest, code)
  | BSPseudo(c, rest) => if c = code then true else _has_pseudo(rest, code)
  | BSNth(_, _, _, rest) => _has_pseudo(rest, code)
  | BSNot(_, rest) => _has_pseudo(rest, code)

#pub fun book_type_span {cs:$A.cells}{n:nat}{r:nat}{t:nat}
  (xs: !book_simples(cs, n, r, t)): [o,k:nat | o + k <= n] @(int o, int k)

implement book_type_span {cs}{n}{r}{t} (xs) = _type_span(xs)

#pub fun book_has_pseudo {cs:$A.cells}{n:nat}{r:nat}{t:nat} (xs: !book_simples(cs, n, r, t), code: int): bool

implement book_has_pseudo {cs}{n}{r}{t} (xs, code) = _has_pseudo(xs, code)

(* The name that a book element of this type has: html and body are written as
   such, so that their case is not the book's *)
fn{sink:vt@ype} _w_name {l:agz}{n:nat}{cs:$A.cells}{o,k:nat | o + k <= n}
  (s: !sink, src: !$A.cborrow(l, n, cs), o: int o, k: int k): void =
  if book_name_is(src, o, k, "html") then book_lit<sink>(s, "html")
  else if book_name_is(src, o, k, "body") then book_lit<sink>(s, "body")
  else book_span<sink>(s, src, o, k)

(* The type selector of a compound; marker says that it is the compound of the
   top (every compound of the top is of a book element) and not of a :not() *)
fn{sink:vt@ype} _w_type {l:agz}{n:nat}{cs:$A.cells}{r:nat}{t:nat}
  (s: !sink, src: !$A.cborrow(l, n, cs), xs: !book_simples(cs, n, r, t), marker: bool): void = let
  val @(o, k) = book_type_span(xs)
in
  if k >= 1 then
    (if book_name_is(src, o, k, "html") then book_lit<sink>(s, "div:where([data-book-element=\"html\"])")
     else if book_name_is(src, o, k, "body") then book_lit<sink>(s, "div:where([data-book-element=\"body\"])")
     else if book_tag_known(src, o, k) then let
       val () = book_span<sink>(s, src, o, k)
     in if marker then book_lit<sink>(s, ":where([data-book-element])") else () end
     else let
       val () = book_lit<sink>(s, ":is(div, span):where([data-book-element=\"")
       val () = book_span<sink>(s, src, o, k)
     in book_lit<sink>(s, "\"])") end)
  else if marker then
    (if book_has_pseudo(xs, 9) then book_lit<sink>(s, "div:where([data-book-element=\"html\"])")
     else book_lit<sink>(s, ":where([data-book-element])"))
  else ()
end

(* An+B *)
fn{sink:vt@ype} _w_anb (s: !sink, a: int, b: int): void =
  if a = 0 then
    (if b < 0 then let val () = book_lit<sink>(s, "-") in book_num<sink>(s, 0 - b) end
     else book_num<sink>(s, b))
  else let
    val () =
      (if a = 1 then ()
       else if a = ~1 then book_lit<sink>(s, "-")
       else if a < 0 then let val () = book_lit<sink>(s, "-") in book_num<sink>(s, 0 - a) end
       else book_num<sink>(s, a))
    val () = book_lit<sink>(s, "n")
  in
    if b > 0 then let val () = book_lit<sink>(s, "+") in book_num<sink>(s, b) end
    else if b < 0 then let val () = book_lit<sink>(s, "-") in book_num<sink>(s, 0 - b) end
    else ()
  end

(* The set of the book's elements that a child index counts: all of them, or
   (to is the span of the type) those of the type *)
fn{sink:vt@ype} _w_of {l:agz}{n:nat}{cs:$A.cells}{to,tk:nat | to + tk <= n}
  (s: !sink, src: !$A.cborrow(l, n, cs), of_type: bool, to: int to, tk: int tk): void =
  if of_type then let
    val () = book_lit<sink>(s, " of :where([data-book-element=\"")
    val () = _w_name<sink>(s, src, to, tk)
  in book_lit<sink>(s, "\"]))") end
  else book_lit<sink>(s, " of :where([data-book-element]))")

fn{sink:vt@ype} _w_pseudo {l:agz}{n:nat}{cs:$A.cells}{to,tk:nat | to + tk <= n}
  (s: !sink, src: !$A.cborrow(l, n, cs), code: int, to: int to, tk: int tk): void =
  if code = 1 then let
    val () = book_lit<sink>(s, ":nth-child(1")
  in _w_of<sink>(s, src, false, to, tk) end
  else if code = 2 then let
    val () = book_lit<sink>(s, ":nth-last-child(1")
  in _w_of<sink>(s, src, false, to, tk) end
  else if code = 3 then let
    val () = book_lit<sink>(s, ":nth-child(1")
    val () = _w_of<sink>(s, src, false, to, tk)
    val () = book_lit<sink>(s, ":nth-last-child(1")
  in _w_of<sink>(s, src, false, to, tk) end
  else if code = 4 then let
    val () = book_lit<sink>(s, ":nth-child(1")
  in _w_of<sink>(s, src, true, to, tk) end
  else if code = 5 then let
    val () = book_lit<sink>(s, ":nth-last-child(1")
  in _w_of<sink>(s, src, true, to, tk) end
  else if code = 6 then let
    val () = book_lit<sink>(s, ":nth-child(1")
    val () = _w_of<sink>(s, src, true, to, tk)
    val () = book_lit<sink>(s, ":nth-last-child(1")
  in _w_of<sink>(s, src, true, to, tk) end
  else if code = 7 then book_lit<sink>(s, ":link")
  else if code = 8 then book_lit<sink>(s, ":empty")
  else if code = 9 then
    (if tk >= 1 then book_lit<sink>(s, ":where([data-book-element=\"html\"])") else ())
  else ()

(* The simple selectors of a compound except its type; (to, tk) is the type's span *)
fun{sink:vt@ype} _w_rest {l:agz}{n:nat}{cs:$A.cells}{l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}
  {r:nat}{t:nat}{to,tk:nat | to + tk <= n} .<r, t>.
  (s: !sink, src: !$A.cborrow(l, n, cs), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   xs: !book_simples(cs, n, r, t), to: int to, tk: int tk): void =
  case+ xs of
  | BSimplesNil() => ()
  | BSType(_ | _, _, rest) => _w_rest<sink>(s, src, cid, co, ck, rest, to, tk)
  | BSUniversal(rest) => _w_rest<sink>(s, src, cid, co, ck, rest, to, tk)
  | BSClass(_ | o, k, rest) => let
      val () = book_lit<sink>(s, "[data-book-class~=\"")
      val () = book_span<sink>(s, src, o, k)
      val () = book_lit<sink>(s, "\"]")
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSId(_ | o, k, rest) => let
      val () = book_lit<sink>(s, ":is([data-book-id=\"")
      val () = book_span<sink>(s, src, o, k)
      val () = book_lit<sink>(s, "\"], #")
      val () = book_span<sink>(s, cid, co, ck)
      val () = book_lit<sink>(s, ")")
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSLang(_ | o, k, rest) => let
      val () = book_lit<sink>(s, ":lang(")
      val () = book_span<sink>(s, src, o, k)
      val () = book_lit<sink>(s, ")")
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSAttrIdent(_ | code, oper, o, k, rest) => let
      val () = _w_attr<sink>(s, code)
      val () = _w_oper<sink>(s, oper)
      val () = book_lit<sink>(s, "\"")
      val () = book_span<sink>(s, src, o, k)
      val () = book_lit<sink>(s, "\"]")
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSAttrString(_ | code, oper, o, k, rest) => let
      val () = _w_attr<sink>(s, code)
      val () = _w_oper<sink>(s, oper)
      val () = book_span<sink>(s, src, o, k)
      val () = book_lit<sink>(s, "]")
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSAttrExists(code, rest) => let
      val () = _w_attr<sink>(s, code)
      val () = book_lit<sink>(s, "]")
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSPseudo(code, rest) => let
      val () = _w_pseudo<sink>(s, src, code, to, tk)
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSNth(kind, a, b, rest) => let
      val () = (if kind = 0 then book_lit<sink>(s, ":nth-child(")
                else if kind = 1 then book_lit<sink>(s, ":nth-last-child(")
                else if kind = 2 then book_lit<sink>(s, ":nth-child(")
                else book_lit<sink>(s, ":nth-last-child("))
      val () = _w_anb<sink>(s, a, b)
      val () = _w_of<sink>(s, src, kind >= 2, to, tk)
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end
  | BSNot(inner, rest) => let
      val () = book_lit<sink>(s, ":not(")
      val @(io, ik) = book_type_span(inner)
      val () = _w_type<sink>(s, src, inner, false)
      val () = _w_rest<sink>(s, src, cid, co, ck, inner, io, ik)
      val () = book_lit<sink>(s, ")")
    in _w_rest<sink>(s, src, cid, co, ck, rest, to, tk) end

(* A compound of the top: its type, always limited to the book's elements, and the rest *)
fn{sink:vt@ype} _w_compound {l:agz}{n:nat}{cs:$A.cells}{l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}{t:nat}
  (s: !sink, src: !$A.cborrow(l, n, cs), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   xs: !book_simples(cs, n, 1, t)): void = let
  val @(to, tk) = book_type_span(xs)
  val () = _w_type<sink>(s, src, xs, true)
in _w_rest<sink>(s, src, cid, co, ck, xs, to, tk) end

(* A complex selector's steps. A step after + is written with A + B as
   :is(A + B, A + :where(:not([data-book-element])) + B): app nodes between
   A and B do not break the match. The pseudo-element, which :is() cannot hold,
   follows the closing parenthesis. *)
fun{sink:vt@ype} _w_steps {l:agz}{n:nat}{cs:$A.cells}{l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}{t:nat} .<t>.
  (s: !sink, src: !$A.cborrow(l, n, cs), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   xs: !book_steps(cs, n, t)): void =
  case+ xs of
  | BStepsNil() => ()
  | BStep(comb, pe, sm, rest) => let
      val () = _w_comb<sink>(s, comb)
    in
      case+ rest of
      | BStep(3, pe2, sm2, rest2) => let
          val () = book_lit<sink>(s, ":is(")
          val () = _w_compound<sink>(s, src, cid, co, ck, sm)
          val () = book_lit<sink>(s, " + ")
          val () = _w_compound<sink>(s, src, cid, co, ck, sm2)
          val () = book_lit<sink>(s, ", ")
          val () = _w_compound<sink>(s, src, cid, co, ck, sm)
          val () = book_lit<sink>(s, " + :where(:not([data-book-element])) + ")
          val () = _w_compound<sink>(s, src, cid, co, ck, sm2)
          val () = book_lit<sink>(s, ")")
          val () = _w_pe<sink>(s, pe2)
        in _w_steps<sink>(s, src, cid, co, ck, rest2) end
      | _ => let
          val () = _w_compound<sink>(s, src, cid, co, ck, sm)
          val () = _w_pe<sink>(s, pe)
        in _w_steps<sink>(s, src, cid, co, ck, rest) end
    end

(* A selector list: each complex selector starts with the container's prefix *)
fun{sink:vt@ype} _w_sels {l:agz}{n:nat}{cs:$A.cells}{l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}{t:nat} .<t>.
  (s: !sink, src: !$A.cborrow(l, n, cs), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   xs: !book_sels(cs, n, t)): void =
  case+ xs of
  | BSelsNil() => ()
  | BSel(steps, rest) => let
      val () = book_lit<sink>(s, "[data-book-container=\"")
      val () = book_span<sink>(s, cid, co, ck)
      val () = book_lit<sink>(s, "\"] ")
      val () = _w_steps<sink>(s, src, cid, co, ck, steps)
      val () = (case+ rest of
                | BSelsNil() => ()
                | BSel(_, _) => book_lit<sink>(s, ","))
    in _w_sels<sink>(s, src, cid, co, ck, rest) end

(* ============================================================
   Faces: a family's number is its place among the book's distinct families
   (the order the book declared them in); at 32 or more it has no face
   ============================================================ *)

fun _bytes_eq {l:agz}{n:nat}{cs:$A.cells}{o1,o2:nat}{k:nat | o1 + k <= n; o2 + k <= n}{i:nat | i <= k} .<k - i>.
  (src: !$A.cborrow(l, n, cs), o1: int o1, o2: int o2, k: int k, i: int i): bool =
  if i >= k then true
  else let
    val @(p1 | c1) = _byte(src, o1 + i)
    val @(p2 | c2) = _byte(src, o2 + i)
  in
    if c1 = c2 then _bytes_eq(src, o1, o2, k, i + 1) else false
  end

(* Whether two spans of the sheet hold the same bytes (exactly) *)
fn _spans_eq {l:agz}{n:nat}{cs:$A.cells}{o1,k1,o2,k2:nat | o1 + k1 <= n; o2 + k2 <= n}
  (src: !$A.cborrow(l, n, cs), o1: int o1, k1: int k1, o2: int o2, k2: int k2): bool =
  if k1 = k2 then _bytes_eq(src, o1, o2, k1, 0) else false

(* Whether the spans seen so far have the text (o, k) *)
fun _seen_has {l:agz}{n:nat}{cs:$A.cells}{sz:nat}{o,k:nat | o + k <= n} .<sz>.
  (src: !$A.cborrow(l, n, cs), xs: !book_srcs(n, sz), o: int o, k: int k): bool =
  case+ xs of
  | BSrcsNil() => false
  | BSrcsCons(so, sk, rest) =>
      if _spans_eq(src, so, sk, o, k) then true else _seen_has(src, rest, o, k)

(* Where the text (o, k) is among the spans seen, counted from the latest, or ~1 *)
fun _seen_pos {l:agz}{n:nat}{cs:$A.cells}{sz:nat}{o,k:nat | o + k <= n} .<sz>.
  (src: !$A.cborrow(l, n, cs), xs: !book_srcs(n, sz), o: int o, k: int k): [r:int | r >= ~1] int r =
  case+ xs of
  | BSrcsNil() => ~1
  | BSrcsCons(so, sk, rest) =>
      if _spans_eq(src, so, sk, o, k) then 0
      else let
        val r = _seen_pos(src, rest, o, k)
      in if r >= 0 then r + 1 else ~1 end

(* The number of the family (o, k): the faces from cur on, c distinct families
   having been met before them (seen); ~1 when no face has it or when the number
   is 32 or more *)
fun _face_count {l:agz}{n:nat}{cs:$A.cells}{g:int}{sz,s:nat}{o,k:nat | o + k <= n}{c:nat} .<sz>.
  (src: !$A.cborrow(l, n, cs), cur: !book_faces(cs, n, g, sz), seen: book_srcs(n, s), c: int c,
   o: int o, k: int k): [r:int | r >= ~1] int r =
  case+ cur of
  | BFacesNil() => let val () = _free_srcs(seen) in ~1 end
  | BFace(fo, fk, _, _, _, rest) =>
      if _spans_eq(src, fo, fk, o, k) then let
        val () = _free_srcs(seen)
      in if c < 32 then c else ~1 end
      else if _seen_has(src, seen, fo, fk) then _face_count(src, rest, seen, c, o, k)
      else _face_count(src, rest, BSrcsCons(fo, fk, seen), c + 1, o, k)

fn _face_number {l:agz}{n:nat}{cs:$A.cells}{g:int}{a:nat}{o,k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), faces: !book_faces(cs, n, g, a), o: int o, k: int k): [r:int | r >= ~1] int r =
  _face_count(src, faces, BSrcsNil(), 0, o, k)

(* Whether a font-family value has an item that is written: a generic family
   or a family the book declared *)
fun _family_any {l:agz}{n:nat}{cs:$A.cells}{g:int}{a,t:nat} .<t>.
  (src: !$A.cborrow(l, n, cs), faces: !book_faces(cs, n, g, a), xs: !book_family(cs, n, t)): bool =
  case+ xs of
  | BFamilyNil() => false
  | BFamilyGeneric(_ | _, _, _) => true
  | BFamilyNamed(o, k, rest) =>
      if _face_number(src, faces, o, k) >= 0 then true else _family_any(src, faces, rest)

#pub fun book_face_number {l:agz}{n:nat}{cs:$A.cells}{g:int}{a:nat}{o,k:nat | o + k <= n}
  (src: !$A.cborrow(l, n, cs), faces: !book_faces(cs, n, g, a), o: int o, k: int k): [r:int | r >= ~1] int r

implement book_face_number {l}{n}{cs}{g}{a}{o,k} (src, faces, o, k) = _face_number(src, faces, o, k)

#pub fun book_family_any {l:agz}{n:nat}{cs:$A.cells}{g:int}{a,t:nat}
  (src: !$A.cborrow(l, n, cs), faces: !book_faces(cs, n, g, a), xs: !book_family(cs, n, t)): bool

implement book_family_any {l}{n}{cs}{g}{a,t} (src, faces, xs) = _family_any(src, faces, xs)

(* The items of a font-family value that are written, separated by commas; a
   declared family is written as its generated name book-<container>-<number> *)
fun{sink:vt@ype} _w_family {l:agz}{n:nat}{cs:$A.cells}{l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}
  {g:int}{a,t:nat} .<t>.
  (s: !sink, src: !$A.cborrow(l, n, cs), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   faces: !book_faces(cs, n, g, a), xs: !book_family(cs, n, t), first: bool): void =
  case+ xs of
  | BFamilyNil() => ()
  | BFamilyGeneric(_ | o, k, rest) => let
      val () = (if first then () else book_lit<sink>(s, ","))
      val () = book_span<sink>(s, src, o, k)
    in _w_family<sink>(s, src, cid, co, ck, faces, rest, false) end
  | BFamilyNamed(o, k, rest) => let
      val num = book_face_number(src, faces, o, k)
    in
      if num >= 0 then let
        val () = (if first then () else book_lit<sink>(s, ","))
        val () = book_lit<sink>(s, "book-")
        val () = book_span<sink>(s, cid, co, ck)
        val () = book_lit<sink>(s, "-")
        val () = book_num<sink>(s, num)
      in _w_family<sink>(s, src, cid, co, ck, faces, rest, false) end
      else _w_family<sink>(s, src, cid, co, ck, faces, rest, first)
    end

(* The declarations of a block *)
fun{sink:vt@ype} _w_decls {l:agz}{n:nat}{cs:$A.cells}{l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}
  {g:int}{a,t:nat} .<t>.
  (s: !sink, src: !$A.cborrow(l, n, cs), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   faces: !book_faces(cs, n, g, a), xs: !book_decls(cs, n, g, t)): void =
  case+ xs of
  | BDeclsNil() => ()
  | BDecl(_ | p, o, k, rest) => let
      val () = book_lit<sink>(s, book_prop_name(p))
      val () = book_lit<sink>(s, ":")
      val () = book_span<sink>(s, src, o, k)
      val () = book_lit<sink>(s, ";")
    in _w_decls<sink>(s, src, cid, co, ck, faces, rest) end
  | BDeclFamily(fam, rest) => let
      val () =
        (if book_family_any(src, faces, fam) then let
           val () = book_lit<sink>(s, "font-family:")
           val () = _w_family<sink>(s, src, cid, co, ck, faces, fam, true)
         in book_lit<sink>(s, ";") end
         else ())
    in _w_decls<sink>(s, src, cid, co, ck, faces, rest) end

(* The rules of a sheet or of a condition's block *)
fun{sink:vt@ype} _w_rules {l:agz}{n:nat}{cs:$A.cells}{l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}
  {g:int}{a,t:nat} .<t>.
  (s: !sink, src: !$A.cborrow(l, n, cs), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   faces: !book_faces(cs, n, g, a), xs: !book_rules(cs, n, g, t)): void =
  case+ xs of
  | BRulesNil() => ()
  | BRule(sels, decls, rest) => let
      val () = _w_sels<sink>(s, src, cid, co, ck, sels)
      val () = book_lit<sink>(s, "{")
      val () = _w_decls<sink>(s, src, cid, co, ck, faces, decls)
      val () = book_lit<sink>(s, "}")
    in _w_rules<sink>(s, src, cid, co, ck, faces, rest) end
  | BCondition(_ | kind, o, k, inner, rest) => let
      val () = (if kind = 0 then book_lit<sink>(s, "@media ") else book_lit<sink>(s, "@supports "))
      val () = book_span<sink>(s, src, o, k)
      val () = book_lit<sink>(s, "{")
      val () = _w_rules<sink>(s, src, cid, co, ck, faces, inner)
      val () = book_lit<sink>(s, "}")
    in _w_rules<sink>(s, src, cid, co, ck, faces, rest) end

(* Hands a sink the text of a sheet that was read: every selector starts with the
   prefix of the container cid (an identifier: pf), every compound is limited to
   the book's elements, !important is not there, and a font-family names only
   generic families and the generated names of the book's own faces. @import
   and @font-face are not written: book_refs_walk lists them. *)
#pub fun{sink:vt@ype} book_sheet_walk {l:agz}{n:nat}{cs:$A.cells}{g:int}
  {l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}
  (s: !sink, src: !$A.cborrow(l, n, cs), read: !book_read(cs, n, g),
   cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck, pf: CSS_IDENT(cs2, co, ck)): void

implement{sink} book_sheet_walk {l}{n}{cs}{g}{l2}{n2}{cs2}{co,ck} (s, src, read, cid, co, ck, pf) =
  case+ read of
  | BookRead(rules, _, faces) => _w_rules<sink>(s, src, cid, co, ck, faces, rules)

(* Hands a sink the declarations of a style attribute (read with book_style_read);
   the font-family is mapped by the faces of the sheet read *)
#pub fun{sink:vt@ype} book_style_walk {l:agz}{n:nat}{cs:$A.cells}{g:int}{t:nat}
  {l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}
  (s: !sink, src: !$A.cborrow(l, n, cs), read: !book_read(cs, n, g), decls: !book_decls(cs, n, g, t),
   cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck, pf: CSS_IDENT(cs2, co, ck)): void

implement{sink} book_style_walk {l}{n}{cs}{g}{t}{l2}{n2}{cs2}{co,ck} (s, src, read, decls, cid, co, ck, pf) =
  case+ read of
  | BookRead(_, _, faces) => _w_decls<sink>(s, src, cid, co, ck, faces, decls)

(* ============================================================
   What the app resolves: the targets of @import and the sources of faces
   ============================================================ *)

(* The references of a sheet, in the order of the sheet: an @import target, and a
   source path, the weight or the style of the face whose family has the
   number num (below 32; a face of the same family as an earlier one has the same
   number), each a span of the sheet *)
#pub datavtype book_refs(n: int, sz: int) =
  | BRefsNil(n, 0)
  | {o,k:nat | o + k <= n}{r:nat} BRefImport(n, r + 1) of (int o, int k, book_refs(n, r))
  | {o,k:nat | o + k <= n}{r:nat} BRefFaceSrc(n, r + 1) of (int, int o, int k, book_refs(n, r))
  | {o,k:nat | o + k <= n}{r:nat} BRefFaceWeight(n, r + 1) of (int, int o, int k, book_refs(n, r))
  | {o,k:nat | o + k <= n}{r:nat} BRefFaceStyle(n, r + 1) of (int, int o, int k, book_refs(n, r))

fun _refs_rev {n:nat}{a,b:nat} .<a>.
  (xs: book_refs(n, a), acc: book_refs(n, b)): book_refs(n, a + b) =
  case+ xs of
  | ~BRefsNil() => acc
  | ~BRefImport(o, k, rest) => _refs_rev(rest, BRefImport(o, k, acc))
  | ~BRefFaceSrc(num, o, k, rest) => _refs_rev(rest, BRefFaceSrc(num, o, k, acc))
  | ~BRefFaceWeight(num, o, k, rest) => _refs_rev(rest, BRefFaceWeight(num, o, k, acc))
  | ~BRefFaceStyle(num, o, k, rest) => _refs_rev(rest, BRefFaceStyle(num, o, k, acc))

fun _refs_free {n:nat}{r:nat} .<r>. (xs: book_refs(n, r)): void =
  case+ xs of
  | ~BRefsNil() => ()
  | ~BRefImport(_, _, rest) => _refs_free(rest)
  | ~BRefFaceSrc(_, _, _, rest) => _refs_free(rest)
  | ~BRefFaceWeight(_, _, _, rest) => _refs_free(rest)
  | ~BRefFaceStyle(_, _, _, rest) => _refs_free(rest)

#pub fun book_refs_free {n:nat}{r:nat} (xs: book_refs(n, r)): void

implement book_refs_free {n}{r} (xs) = _refs_free(xs)

fun _refs_imports {n:nat}{t,a:nat} .<t>.
  (xs: !book_imports(n, t), acc: book_refs(n, a)): [r:nat] book_refs(n, r) =
  case+ xs of
  | BImportsNil() => acc
  | BImportsCons(o, k, rest) => _refs_imports(rest, BRefImport(o, k, acc))

fun _refs_srcs {n:nat}{t,a:nat} .<t>.
  (xs: !book_srcs(n, t), num: int, acc: book_refs(n, a)): [r:nat] book_refs(n, r) =
  case+ xs of
  | BSrcsNil() => acc
  | BSrcsCons(o, k, rest) => _refs_srcs(rest, num, BRefFaceSrc(num, o, k, acc))

fn _refs_descriptor {n:nat}{cs:$A.cells}{g:int}{a:nat}
  (d: !book_descriptor(cs, n, g), num: int, weight: bool, acc: book_refs(n, a)): [r:nat] book_refs(n, r) =
  case+ d of
  | BDescriptorNone() => acc
  | BDescriptor(_ | o, k) =>
      if weight then BRefFaceWeight(num, o, k, acc) else BRefFaceStyle(num, o, k, acc)

(* A face's references, when its family has a number below 32 *)
fn _refs_face {n:nat}{cs:$A.cells}{g:int}{s,a:nat}
  (srcs: !book_srcs(n, s), w: !book_descriptor(cs, n, g), st: !book_descriptor(cs, n, g), num: int,
   acc: book_refs(n, a)): [r:nat] book_refs(n, r) =
  if num >= 0 then
    (if num < 32 then let
       val a1 = _refs_srcs(srcs, num, acc)
       val a2 = _refs_descriptor(w, num, true, a1)
     in _refs_descriptor(st, num, false, a2) end
     else acc)
  else acc

fn _face_num {c:nat}{pos:int} (has: bool, c: int c, pos: int pos): [r:int] int r =
  if has then c - 1 - pos else c

fun _refs_faces {l:agz}{n:nat}{cs:$A.cells}{g:int}{t,s,a:nat}{c:nat} .<t>.
  (src: !$A.cborrow(l, n, cs), xs: !book_faces(cs, n, g, t), seen: book_srcs(n, s), c: int c,
   acc: book_refs(n, a)): [r:nat] book_refs(n, r) =
  case+ xs of
  | BFacesNil() => let val () = _free_srcs(seen) in acc end
  | BFace(fo, fk, srcs, w, st, rest) => let
      val pos = _seen_pos(src, seen, fo, fk)
      val num = _face_num(pos >= 0, c, pos)
      val acc2 = _refs_face(srcs, w, st, num, acc)
    in
      if pos >= 0 then _refs_faces(src, rest, seen, c, acc2)
      else _refs_faces(src, rest, BSrcsCons(fo, fk, seen), c + 1, acc2)
    end

(* The references of a sheet that was read, for the app to resolve *)
#pub fun book_refs_of {l:agz}{n:nat}{cs:$A.cells}{g:int}
  (src: !$A.cborrow(l, n, cs), read: !book_read(cs, n, g)): [r:nat] book_refs(n, r)

implement book_refs_of {l}{n}{cs}{g} (src, read) =
  case+ read of
  | BookRead(_, imps, faces) => let
      val a1 = _refs_imports(imps, BRefsNil())
      val a2 = _refs_faces(src, faces, BSrcsNil(), 0, a1)
    in _refs_rev(a2, BRefsNil()) end
