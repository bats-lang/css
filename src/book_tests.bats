(* book_tests -- the unit tests of book.bats (keyword: book css, bats-lang/css#79),
   run by `bats test`. A case reads a sheet that it builds in memory into a tree, walks
   the tree into a test sink (a rope), and compares the rope with a literal: each
   allow-list entry and each refusal of the design has a case. *)

#include "share/atspre_staload.hats"

#use array as A
#use arith as AR
#use builder as B

staload "./book.bats"
staload "./book.dats"
staload "./contrast.bats"

(* ============================================================
   The test sink: a rope that the walk's pieces are put into
   ============================================================ *)

datavtype tsink = TSink of $B.sized_rope

fun _copy_span {l:agz}{n:nat}{cs:$A.cells}{o,k:nat | o + k <= n}{i:nat | i <= k} .<k - i>.
  (r: !$B.sized_rope, src: !$A.cborrow(l, n, cs), o: int o, k: int k, i: int i): void =
  if i >= k then ()
  else let
    val @(p | c) = $A.cborrow_get(src, o + i)
    val () = $B.sized_rope_put(r, c)
  in _copy_span(r, src, o, k, i + 1) end

implement book_lit<tsink> {m} (s, lit) =
  case+ s of
  | TSink(r) => $B.sized_rope_bput(r, lit)

implement book_span<tsink> {l}{n}{cs}{o,k} (s, src, o, k) =
  case+ s of
  | TSink(r) => _copy_span(r, src, o, k, 0)

implement book_num<tsink> (s, v) =
  case+ s of
  | TSink(r) => let
      var b : $B.builder(0) = $B.create()
      val () = $B.put_int(b, v)
    in $B.sized_rope_append(r, b) end

(* ============================================================
   Comparing a rope with a literal
   ============================================================ *)

fn text_length {text_len:nat} (text: string text_len): int text_len = g1u2i(string1_length(text))

fun _equal_from {l:agz}{n:pos}{used:nat | used <= n}{i:nat | i <= used}{literal_len:nat}
  {at:nat | at + (used - i) <= literal_len} .<used - i>.
  (bytes: !$A.borrow(byte, l, n), used: int used, i: int i, literal: string literal_len,
   at: int at): bool =
  if i >= used then true
  else if byte2int0($A.read<byte>(bytes, i)) = char2int0(string_get_at(literal, at)) then
    _equal_from(bytes, used, i + 1, literal, at + 1)
  else false

fun _chunks_are {chunk_count:nat}{literal_len:nat}{at:nat | at <= literal_len} .<chunk_count>.
  (chunks: $B.rope_sized_list(chunk_count), literal: string literal_len,
   literal_len: int literal_len,
   at: int at): bool =
  case+ chunks of
  | ~$B.rope_sized_nil() => at = literal_len
  | ~$B.rope_sized_cons(chunk, used, rest) =>
    if at + used <= literal_len then let
      val @(frozen, view) = $A.freeze<byte>(chunk)
      val same = _equal_from(view, used, 0, literal, at)
      val () = $A.drop<byte>(frozen, view)
      val () = $A.free<byte>($A.thaw<byte>(frozen))
    in
      if same then _chunks_are(rest, literal, literal_len, at + used)
      else let val () = $B.rope_sized_list_free(rest) in false end
    end
    else let
      val () = $A.free<byte>(chunk)
      val () = $B.rope_sized_list_free(rest)
    in false end

fn _rope_is {literal_len:nat} (text: $B.sized_rope, literal: string literal_len): bool =
  _chunks_are($B.sized_rope_chunks(text), literal, text_length(literal), 0)

(* ============================================================
   Reading a sheet and walking it
   ============================================================ *)

(* The grounds that the app passes: the Light page and the Sepia page *)
fn _light (): rgb256 = @(250, 248, 245)
fn _sepia (): rgb256 = @(244, 236, 216)

(* Reads css in the grammar g, walks it with the container id c1 and gives what the
   sink got *)
fn _walk {css_len:nat | css_len < 16384}{g:int | 1 <= g; g <= 3}
  (css: string css_len, g: int g): $B.sized_rope = let
  var sheet : $B.builder(0) = $B.create()
  val () = $B.bput(sheet, css)
  val @(sheet_arr, sheet_len) = $B.to_arr(sheet)
  val @(sheet_len_pf | sheet_barr) = $A.barr_of_arr(sheet_arr)
  val @(sheet_frozen, sheet_view) = $A.barr_freeze(sheet_barr)
  var ident : $B.builder(0) = $B.create()
  val () = $B.bput(ident, "c1")
  val @(ident_arr, ident_len) = $B.to_arr(ident)
  val @(ident_len_pf | ident_barr) = $A.barr_of_arr(ident_arr)
  val @(ident_frozen, ident_view) = $A.barr_freeze(ident_barr)
  val sink = TSink($B.sized_rope_create())
  val @(ident_pf | ident_k) = book_ident_scan(ident_view, ident_len, 0)
  val read = book_sheet_read(sheet_view, sheet_len, g, _light(), _sepia())
  val () =
    (if ident_k >= 1 then let
       prval IDENT_OPT_some(id) = ident_pf
     in book_sheet_walk<tsink>(sink, sheet_view, read, ident_view, 0, ident_k, id) end
     else let prval IDENT_OPT_none() = ident_pf in () end)
  val () = book_read_free(read)
  val () = $A.cborrow_drop(sheet_frozen, sheet_view)
  val () = $A.barr_free($A.barr_thaw(sheet_frozen))
  val () = $A.cborrow_drop(ident_frozen, ident_view)
  val () = $A.barr_free($A.barr_thaw(ident_frozen))
in
  case+ sink of
  | ~TSink(r) => r
end

(* Whether the bytes of x then y hold a refused substring (book_refused) *)
fn _refused_cat {a,b:nat | a + b < 16384} (x: string a, y: string b): bool = let
  var sheet : $B.builder(0) = $B.create()
  val () = $B.bput(sheet, x)
  val () = $B.bput(sheet, y)
  val @(sheet_arr, sheet_len) = $B.to_arr(sheet)
  val @(sheet_len_pf | sheet_barr) = $A.barr_of_arr(sheet_arr)
  val @(sheet_frozen, sheet_view) = $A.barr_freeze(sheet_barr)
  val answer = book_refused(sheet_view, sheet_len)
  val () = $A.cborrow_drop(sheet_frozen, sheet_view)
  val () = $A.barr_free($A.barr_thaw(sheet_frozen))
in answer end

(* Whether a sheet's bytes hold a refused substring (book_refused) *)
fn _refused {css_len:nat | css_len < 16384} (css: string css_len): bool = _refused_cat(css, "")

(* The golden output of a walk: the rope is the literal, and the literal is not refused,
   so what the walk emits passes bridge's check *)
fn _is {css_len:nat | css_len < 16384}{g:int | 1 <= g; g <= 3}{gold_len:nat | gold_len < 16384}
  (css: string css_len, g: int g, gold: string gold_len): bool =
  if _rope_is(_walk(css, g), gold) then not(_refused(gold)) else false

(* The same, for the SVG paint grammar, whose url(#fragment) is kept on purpose and so
   is on the refused list *)
fn _is_paint {css_len:nat | css_len < 16384}{gold_len:nat | gold_len < 16384}
  (css: string css_len, gold: string gold_len): bool = _rope_is(_walk(css, 3), gold)

fun _refs_count {n:nat}{r:nat} .<r>. (xs: book_refs(n, r), acc: int): int =
  case+ xs of
  | ~BRefsNil() => acc
  | ~BRefImport(_, _, rest) => _refs_count(rest, acc + 1)
  | ~BRefFaceSrc(_, _, _, rest) => _refs_count(rest, acc + 1)
  | ~BRefFaceWeight(_, _, _, rest) => _refs_count(rest, acc + 1)
  | ~BRefFaceStyle(_, _, _, rest) => _refs_count(rest, acc + 1)

(* How many references (imports, face sources, weights, styles) a sheet lists *)
fn _refs_of {css_len:nat | css_len < 16384} (css: string css_len): int = let
  var sheet : $B.builder(0) = $B.create()
  val () = $B.bput(sheet, css)
  val @(sheet_arr, sheet_len) = $B.to_arr(sheet)
  val @(sheet_len_pf | sheet_barr) = $A.barr_of_arr(sheet_arr)
  val @(sheet_frozen, sheet_view) = $A.barr_freeze(sheet_barr)
  val read = book_sheet_read(sheet_view, sheet_len, 1, _light(), _sepia())
  val count = _refs_count(book_refs_of(sheet_view, read), 0)
  val () = book_read_free(read)
  val () = $A.cborrow_drop(sheet_frozen, sheet_view)
  val () = $A.barr_free($A.barr_thaw(sheet_frozen))
in count end

(* Walks the declarations and frees them (they are passed straight from the read, which
   opens their size) *)
fn _style_run {l:agz}{n:nat}{cs:$A.cells}{g:int}{t:nat}
  {l2:agz}{n2:nat}{cs2:$A.cells}{co,ck:nat | co + ck <= n2}
  (sink: !tsink, src: !$A.cborrow(l, n, cs), read: !book_read(cs, n, g),
   decls: book_decls(cs, n, g, t), cid: !$A.cborrow(l2, n2, cs2), co: int co, ck: int ck,
   pf: CSS_IDENT(cs2, co, ck)): void = let
  val () = book_style_walk<tsink>(sink, src, read, decls, cid, co, ck, pf)
in book_decls_free(decls) end

(* The rope of a style attribute read in the grammar g and walked with the faces of an
   empty sheet *)
fn _style_walk {css_len:nat | css_len < 16384}{g:int | 1 <= g; g <= 3}
  (css: string css_len, g: int g): $B.sized_rope = let
  var sheet : $B.builder(0) = $B.create()
  val () = $B.bput(sheet, css)
  val @(sheet_arr, sheet_len) = $B.to_arr(sheet)
  val @(sheet_len_pf | sheet_barr) = $A.barr_of_arr(sheet_arr)
  val @(sheet_frozen, sheet_view) = $A.barr_freeze(sheet_barr)
  var ident : $B.builder(0) = $B.create()
  val () = $B.bput(ident, "c1")
  val @(ident_arr, ident_len) = $B.to_arr(ident)
  val @(ident_len_pf | ident_barr) = $A.barr_of_arr(ident_arr)
  val @(ident_frozen, ident_view) = $A.barr_freeze(ident_barr)
  val sink = TSink($B.sized_rope_create())
  val @(ident_pf | ident_k) = book_ident_scan(ident_view, ident_len, 0)
  val read = book_sheet_read(sheet_view, 0, g, _light(), _sepia())
  val () =
    (if ident_k >= 1 then let
       prval IDENT_OPT_some(id) = ident_pf
     in _style_run(sink, sheet_view, read,
          book_style_read(sheet_view, sheet_len, g, _light(), _sepia()),
          ident_view, 0, ident_k, id) end
     else let prval IDENT_OPT_none() = ident_pf in () end)
  val () = book_read_free(read)
  val () = $A.cborrow_drop(sheet_frozen, sheet_view)
  val () = $A.barr_free($A.barr_thaw(sheet_frozen))
  val () = $A.cborrow_drop(ident_frozen, ident_view)
  val () = $A.barr_free($A.barr_thaw(ident_frozen))
in
  case+ sink of
  | ~TSink(r) => r
end

fn _style_is {css_len:nat | css_len < 16384}{g:int | 1 <= g; g <= 3}{gold_len:nat | gold_len < 16384}
  (css: string css_len, g: int g, gold: string gold_len): bool =
  if _rope_is(_style_walk(css, g), gold) then not(_refused(gold)) else false

$UNITTEST.run begin

(* ---- the walk: one case for each entry of the allow-list and each refusal ---- *)

fn test_text_allowed (): bool =
  _is("p{font-style:italic;font-weight:bold;font-size:1.2em;line-height:1.5;text-align:center;text-indent:1em;margin:0 1em;padding-left:2px;display:block;float:left;width:50%}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){font-style:italic;font-weight:bold;font-size:1.2em;line-height:1.5;text-align:center;text-indent:1em;margin:0 1em;padding-left:2px;display:block;float:left;width:50%;}")

fn test_refuse_url (): bool =
  _is("p{width:url(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_image_set (): bool =
  _is("p{width:image-set(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_image (): bool =
  _is("p{width:image(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_src (): bool =
  _is("p{width:src(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_cross_fade (): bool =
  _is("p{width:cross-fade(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_element (): bool =
  _is("p{width:element(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_paint (): bool =
  _is("p{width:paint(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_env (): bool =
  _is("p{width:env(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_var (): bool =
  _is("p{width:var(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_attr (): bool =
  _is("p{width:attr(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_expression (): bool =
  _is("p{width:expression(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_URL_upper (): bool =
  _is("p{width:URL(x);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_refuse_calc_kept (): bool =
  _is("p{line-height:calc(1px + 2px);margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){line-height:calc(1px + 2px);margin:0;}")

fn test_escape_value (): bool =
  _is("p{margin:\\30;color:#333}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){color:#333;}")

fn test_escape_selector (): bool =
  _is("\\70{margin:0} q{margin:0}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){margin:0;}")

fn test_escape_string (): bool =
  _is("p{content:\"a\\\"b\";quotes:\"\\201C\"} q{margin:0}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){margin:0;}")

fn test_import_listed (): bool =
  _is("@import \"a.css\"; p{margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_font_face_listed (): bool =
  _is("@font-face{font-family:\"X\";src:url(x.otf)} p{margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_important_dropped (): bool =
  _is("p{margin:0 !important;color:#333 ! important;padding:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;color:#333;padding:0;}")

fn test_comments (): bool =
  _is("/* c */ p /* d */ { margin /* e */ : 0; color:#333 }", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;color:#333;}")

fn test_nesting (): bool =
  _is("p{ color:#333; &.x{margin:0} margin-top:1em }", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){color:#333;margin-top:1em;}")

fn test_has_dropped (): bool =
  _is("p:has(img){margin:0} q{margin:0}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){margin:0;}")

fn test_selection_dropped (): bool =
  _is("p::selection{margin:0} q{margin:0}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){margin:0;}")

fn test_position_fixed (): bool =
  _is("p{position:fixed} p{position:relative} p{position:absolute}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){position:relative;}")

fn test_px_reflow (): bool =
  _is("p{font-size:12px} q{font-size:1em}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){font-size:1em;}")

fn test_px_fixed (): bool =
  _is("p{font-size:12px} q{font-size:1em}", 2,
    "[data-book-container=\"c1\"] p:where([data-book-element]){font-size:12px;}[data-book-container=\"c1\"] q:where([data-book-element]){font-size:1em;}")

fn test_colour_reflow (): bool =
  _is("p{color:red;background-color:red;border-color:red}", 1,
    "")

fn test_colour_fixed (): bool =
  _is("p{color:red;background-color:red}", 2,
    "[data-book-container=\"c1\"] p:where([data-book-element]){color:red;background-color:red;}")

fn test_contrast_dropped (): bool =
  _is("p{color:#777} q{color:#333}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){color:#333;}")

fn test_svg_paint_kept (): bool =
  _is_paint(".s{fill:url(#star)} .t{fill:url(a.svg)} .u{fill:red}",
    "[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"s\"]{fill:url(#star);}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"u\"]{fill:red;}")

fn test_svg_paint_reflow (): bool =
  _is(".s{fill:url(#star)} .u{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"u\"]{margin:0;}")

fn test_spec_type (): bool =
  _is("p{margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_spec_class (): bool =
  _is(".x{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"x\"]{margin:0;}")

fn test_spec_id (): bool =
  _is("#a{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element]):is([data-book-id=\"a\"], #c1){margin:0;}")

fn test_spec_two_classes (): bool =
  _is(".x.y{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"x\"][data-book-class~=\"y\"]{margin:0;}")

fn test_spec_body (): bool =
  _is("body{margin:0}", 1,
    "[data-book-container=\"c1\"] div:where([data-book-element=\"body\"]){margin:0;}")

fn test_spec_html (): bool =
  _is("html{margin:0}", 1,
    "[data-book-container=\"c1\"] div:where([data-book-element=\"html\"]){margin:0;}")

fn test_child_forms (): bool =
  _is("p:first-child, span:first-of-type, li:nth-child(2n+1), li:nth-last-of-type(odd), p:only-child{margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]):nth-child(1 of :where([data-book-element])),[data-book-container=\"c1\"] span:where([data-book-element]):nth-child(1 of :where([data-book-element=\"span\"])),[data-book-container=\"c1\"] li:where([data-book-element]):nth-child(2n+1 of :where([data-book-element])),[data-book-container=\"c1\"] li:where([data-book-element]):nth-last-child(2n+1 of :where([data-book-element=\"li\"])),[data-book-container=\"c1\"] p:where([data-book-element]):nth-child(1 of :where([data-book-element])):nth-last-child(1 of :where([data-book-element])){margin:0;}")

fn test_sibling_adjacent (): bool =
  _is("p + p{margin:0}", 1,
    "[data-book-container=\"c1\"] :is(p:where([data-book-element]) + p:where([data-book-element]), p:where([data-book-element]) + :where(:not([data-book-element])) + p:where([data-book-element])){margin:0;}")

fn test_sibling_general (): bool =
  _is("* ~ p{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element]) ~ p:where([data-book-element]){margin:0;}")

fn test_child_descendant (): bool =
  _is("p > span{margin:0} a b{margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]) > span:where([data-book-element]){margin:0;}[data-book-container=\"c1\"] a:where([data-book-element]) b:where([data-book-element]){margin:0;}")

fn test_universal (): bool =
  _is("*{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element]){margin:0;}")

fn test_attr_selector (): bool =
  _is("a[title=\"x\"]:not(.y){margin:0} [lang|=en]{margin:0}", 1,
    "[data-book-container=\"c1\"] a:where([data-book-element])[title=\"x\"]:not([data-book-class~=\"y\"]){margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[lang|=\"en\"]{margin:0;}")

fn test_not_pseudo (): bool =
  _is("p:not(.x){margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]):not([data-book-class~=\"x\"]){margin:0;}")

fn test_foo_unknown (): bool =
  _is("foo{margin:0} foo:first-of-type{margin:0}", 1,
    "[data-book-container=\"c1\"] :is(div, span):where([data-book-element=\"foo\"]){margin:0;}[data-book-container=\"c1\"] :is(div, span):where([data-book-element=\"foo\"]):nth-child(1 of :where([data-book-element=\"foo\"])){margin:0;}")

fn test_pseudo_elements (): bool =
  _is("p::before{content:\"x\"} p::after{content:counter(c)} p:before{content:none} p::first-letter{float:left}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element])::before{content:\"x\";}[data-book-container=\"c1\"] p:where([data-book-element])::after{content:counter(c);}[data-book-container=\"c1\"] p:where([data-book-element])::before{content:none;}[data-book-container=\"c1\"] p:where([data-book-element])::first-letter{float:left;}")

fn test_epub_prefix (): bool =
  _is("p{-epub-hyphens:auto;-epub-text-transform:uppercase;-webkit-x:1}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){hyphens:auto;text-transform:uppercase;}")

fn test_media_supports (): bool =
  _is("@media (min-width: 600px) { p{margin:0} } @media print { p{margin:0} } @supports (display: flex) { p{display:flex} }", 1,
    "@media (min-width: 600px) {[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}}@supports (display: flex) {[data-book-container=\"c1\"] p:where([data-book-element]){display:flex;}}")

fn test_faces (): bool =
  _is("p{font-family:\"Foo\", Georgia, serif} @font-face{font-family:Foo;src:url('a.otf')} @font-face{font-family:Foo;font-weight:bold;src:url(b.otf)} @font-face{font-family:\"Bar\";src:url(c.otf)} q{font-family:Bar,Foo,Baz}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){font-family:book-c1-0,serif;}[data-book-container=\"c1\"] q:where([data-book-element]){font-family:book-c1-1,book-c1-0;}")

fn test_selector_list (): bool =
  _is("h1, h2 , h3{margin:0}", 1,
    "[data-book-container=\"c1\"] h1:where([data-book-element]),[data-book-container=\"c1\"] h2:where([data-book-element]),[data-book-container=\"c1\"] h3:where([data-book-element]){margin:0;}")

fn test_empty (): bool =
  _is("", 1,
    "")

fn test_unbalanced (): bool =
  _is("p{margin:0", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}")

fn test_angle (): bool =
  _is("p{margin:0} <!-- q{margin:0} -->", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]){margin:0;}[data-book-container=\"c1\"] q:where([data-book-element]){margin:0;}")

fn test_fixture_fixed_reflow (): bool =
  _is("@font-face {\n  font-family: \"StixTwoText\";\n  font-weight: normal;\n  font-style: normal;\n  src: url('fonts/STIXTwoText-Regular.otf');\n}\n\nh1 {\n  position: absolute;\n  top: 10px;\n  left: 10px;\n  margin: 0;\n  padding: 0;\n  font-size: 30px;\n  font-family: \"StixTwoText\";\n}\n\np {\n  position: absolute;\n  top: 80px;\n  left: 40px;\n  margin: 0;\n  padding: 0;\n  font-size: 20px;\n  font-family: \"StixTwoText\";\n}\n\n.pattern {\n  background-color: white;\n  background-image: linear-gradient(currentColor 1px, transparent 1px), linear-gradient(to right, currentColor 1px, transparent 1px);\n  background-size: 50px 50px;\n}\n\n.solid {\n  background-color: red;\n}\n\n", 1,
    "[data-book-container=\"c1\"] h1:where([data-book-element]){top:10px;left:10px;margin:0;padding:0;font-family:book-c1-0;}[data-book-container=\"c1\"] p:where([data-book-element]){top:80px;left:40px;margin:0;padding:0;font-family:book-c1-0;}")

fn test_fixture_fixed_fixed (): bool =
  _is("@font-face {\n  font-family: \"StixTwoText\";\n  font-weight: normal;\n  font-style: normal;\n  src: url('fonts/STIXTwoText-Regular.otf');\n}\n\nh1 {\n  position: absolute;\n  top: 10px;\n  left: 10px;\n  margin: 0;\n  padding: 0;\n  font-size: 30px;\n  font-family: \"StixTwoText\";\n}\n\np {\n  position: absolute;\n  top: 80px;\n  left: 40px;\n  margin: 0;\n  padding: 0;\n  font-size: 20px;\n  font-family: \"StixTwoText\";\n}\n\n.pattern {\n  background-color: white;\n  background-image: linear-gradient(currentColor 1px, transparent 1px), linear-gradient(to right, currentColor 1px, transparent 1px);\n  background-size: 50px 50px;\n}\n\n.solid {\n  background-color: red;\n}\n\n", 2,
    "[data-book-container=\"c1\"] h1:where([data-book-element]){position:absolute;top:10px;left:10px;margin:0;padding:0;font-size:30px;font-family:book-c1-0;}[data-book-container=\"c1\"] p:where([data-book-element]){position:absolute;top:80px;left:40px;margin:0;padding:0;font-size:20px;font-family:book-c1-0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"pattern\"]{background-color:white;background-image:linear-gradient(currentColor 1px, transparent 1px), linear-gradient(to right, currentColor 1px, transparent 1px);background-size:50px 50px;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"solid\"]{background-color:red;}")

fn test_fixture_typst_reflow (): bool =
  _is("/* MathML support rules emitted by the Typst 0.15 HTML exporter. */\n\n/* Alignment */\n.typst-math mtable.right-align mtd,\n.typst-math mtable mtd.right-align,\n.typst-math mtable.left-align mtd.right-align,\n.typst-math mtable.aligned mtd:nth-child(odd) {\n  justify-items: end;\n  text-align: right;\n}\n\n.typst-math mtable.cases mtd,\n.typst-math mtable.left-align mtd,\n.typst-math mtable mtd.left-align,\n.typst-math mtable.aligned mtd:nth-child(even),\n.typst-math math:is(:not([display])) > mtable.multiline-equation mtd {\n  justify-items: start;\n  text-align: left;\n}\n\n.typst-math mtable.cases mtd,\n.typst-math mtable.aligned mtd,\n.typst-math mtable mtd.flushed,\n.typst-math mtable mtd.left-flush {\n  padding-left: 0;\n}\n\n.typst-math mtable.cases mtd,\n.typst-math mtable.aligned mtd,\n.typst-math mtable mtd.flushed,\n.typst-math mtable mtd.right-flush {\n  padding-right: 0;\n}\n\n/* Tables */\n.typst-math mtable {\n  math-style: inherit;\n}\n\n.typst-math mtd {\n  math-depth: auto-add;\n  math-style: compact;\n  math-shift: compact;\n}\n\n/* Equations */\n.typst-math mtable.multiline-equation mtd {\n  math-depth: inherit;\n  math-style: inherit;\n  math-shift: inherit;\n  padding: 0;\n}\n\n.typst-math math > mtable.multiline-equation mtr:not(:last-child) mtd {\n  padding-bottom: 0.5em;\n}\n\n/* Fractions */\n.typst-math mfrac {\n  padding-inline: 0;\n  margin-inline: 0.1em;\n}\n\n/* Accents */\n.typst-math mover[accent=\"true\"] > :first-child {\n  font-feature-settings: \"dtls\";\n}\n\n.typst-math mover.dotted[accent=\"true\"] > :first-child {\n  font-feature-settings: \"dtls\" 0;\n}\n\n/* Script level, display style, and math shift */\n.typst-math munder > :nth-child(2),\n.typst-math munderover > :nth-child(2) {\n  math-shift: compact;\n}\n\n.typst-math munder[accentunder=\"true\"] > :not(:first-child),\n.typst-math mover[accent=\"true\"] > :not(:first-child) {\n  math-depth: inherit;\n  math-style: inherit;\n  math-shift: inherit;\n}\n\n/* Keep large equations usable without imposing colors or backgrounds. */\n.typst-math-display > math[display=\"block\"] {\n  max-width: 100%;\n  overflow-x: auto;\n  overflow-y: hidden;\n}\n\n.typst-math-error {\n  color: inherit;\n  text-decoration: underline wavy currentcolor;\n}\n", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"right-align\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"right-align\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"left-align\"] :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"right-align\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]):nth-child(2n+1 of :where([data-book-element])){justify-items:end;text-align:right;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"cases\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"left-align\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"left-align\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]):nth-child(2n of :where([data-book-element])){justify-items:start;text-align:left;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"cases\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"flushed\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"left-flush\"]{padding-left:0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"cases\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"flushed\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"right-flush\"]{padding-right:0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"multiline-equation\"] :is(div, span):where([data-book-element=\"mtd\"]){padding:0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"math\"]) > :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"multiline-equation\"] :is(div, span):where([data-book-element=\"mtr\"]):not(:nth-last-child(1 of :where([data-book-element]))) :is(div, span):where([data-book-element=\"mtd\"]){padding-bottom:0.5em;}")

fn test_fixture_typst_fixed (): bool =
  _is("/* MathML support rules emitted by the Typst 0.15 HTML exporter. */\n\n/* Alignment */\n.typst-math mtable.right-align mtd,\n.typst-math mtable mtd.right-align,\n.typst-math mtable.left-align mtd.right-align,\n.typst-math mtable.aligned mtd:nth-child(odd) {\n  justify-items: end;\n  text-align: right;\n}\n\n.typst-math mtable.cases mtd,\n.typst-math mtable.left-align mtd,\n.typst-math mtable mtd.left-align,\n.typst-math mtable.aligned mtd:nth-child(even),\n.typst-math math:is(:not([display])) > mtable.multiline-equation mtd {\n  justify-items: start;\n  text-align: left;\n}\n\n.typst-math mtable.cases mtd,\n.typst-math mtable.aligned mtd,\n.typst-math mtable mtd.flushed,\n.typst-math mtable mtd.left-flush {\n  padding-left: 0;\n}\n\n.typst-math mtable.cases mtd,\n.typst-math mtable.aligned mtd,\n.typst-math mtable mtd.flushed,\n.typst-math mtable mtd.right-flush {\n  padding-right: 0;\n}\n\n/* Tables */\n.typst-math mtable {\n  math-style: inherit;\n}\n\n.typst-math mtd {\n  math-depth: auto-add;\n  math-style: compact;\n  math-shift: compact;\n}\n\n/* Equations */\n.typst-math mtable.multiline-equation mtd {\n  math-depth: inherit;\n  math-style: inherit;\n  math-shift: inherit;\n  padding: 0;\n}\n\n.typst-math math > mtable.multiline-equation mtr:not(:last-child) mtd {\n  padding-bottom: 0.5em;\n}\n\n/* Fractions */\n.typst-math mfrac {\n  padding-inline: 0;\n  margin-inline: 0.1em;\n}\n\n/* Accents */\n.typst-math mover[accent=\"true\"] > :first-child {\n  font-feature-settings: \"dtls\";\n}\n\n.typst-math mover.dotted[accent=\"true\"] > :first-child {\n  font-feature-settings: \"dtls\" 0;\n}\n\n/* Script level, display style, and math shift */\n.typst-math munder > :nth-child(2),\n.typst-math munderover > :nth-child(2) {\n  math-shift: compact;\n}\n\n.typst-math munder[accentunder=\"true\"] > :not(:first-child),\n.typst-math mover[accent=\"true\"] > :not(:first-child) {\n  math-depth: inherit;\n  math-style: inherit;\n  math-shift: inherit;\n}\n\n/* Keep large equations usable without imposing colors or backgrounds. */\n.typst-math-display > math[display=\"block\"] {\n  max-width: 100%;\n  overflow-x: auto;\n  overflow-y: hidden;\n}\n\n.typst-math-error {\n  color: inherit;\n  text-decoration: underline wavy currentcolor;\n}\n", 2,
    "[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"right-align\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"right-align\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"left-align\"] :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"right-align\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]):nth-child(2n+1 of :where([data-book-element])){justify-items:end;text-align:right;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"cases\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"left-align\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"left-align\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]):nth-child(2n of :where([data-book-element])){justify-items:start;text-align:left;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"cases\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"flushed\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"left-flush\"]{padding-left:0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"cases\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"aligned\"] :is(div, span):where([data-book-element=\"mtd\"]),[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"flushed\"],[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"]) :is(div, span):where([data-book-element=\"mtd\"])[data-book-class~=\"right-flush\"]{padding-right:0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"multiline-equation\"] :is(div, span):where([data-book-element=\"mtd\"]){padding:0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math\"] :is(div, span):where([data-book-element=\"math\"]) > :is(div, span):where([data-book-element=\"mtable\"])[data-book-class~=\"multiline-equation\"] :is(div, span):where([data-book-element=\"mtr\"]):not(:nth-last-child(1 of :where([data-book-element]))) :is(div, span):where([data-book-element=\"mtd\"]){padding-bottom:0.5em;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-class~=\"typst-math-error\"]{color:inherit;}")

fn test_attr_lang_bar (): bool =
  _is("[lang|=en]{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[lang|=\"en\"]{margin:0;}")

fn test_attr_ops (): bool =
  _is("[title^=b]{margin:0} [title~=\"b\"]{margin:0} [title$=\"b\"]{margin:0} [title*=\"b\"]{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[title^=\"b\"]{margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[title~=\"b\"]{margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[title$=\"b\"]{margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[title*=\"b\"]{margin:0;}")

fn test_attr_names (): bool =
  _is("[dir=rtl]{margin:0} [colspan=\"2\"]{margin:0} [hidden]{margin:0} [rowspan]{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[dir=\"rtl\"]{margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[colspan=\"2\"]{margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[hidden]{margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[rowspan]{margin:0;}")

fn test_attr_epub_type (): bool =
  _is("[epub|type~=x]{margin:0} [epub|type]{margin:0}", 1,
    "[data-book-container=\"c1\"] :where([data-book-element])[data-book-epub-type~=\"x\"]{margin:0;}[data-book-container=\"c1\"] :where([data-book-element])[data-book-epub-type]{margin:0;}")

fn test_attr_other_dropped (): bool =
  _is("[display]{margin:0} [accent=\"true\"]{margin:0} [a=\"b\" i]{margin:0} q{margin:0}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){margin:0;}")

fn test_pseudo_kept (): bool =
  _is("p:first-of-type{margin:0} p:last-child{margin:0} p:empty{margin:0} p:not(:first-child){margin:0}", 1,
    "[data-book-container=\"c1\"] p:where([data-book-element]):nth-child(1 of :where([data-book-element=\"p\"])){margin:0;}[data-book-container=\"c1\"] p:where([data-book-element]):nth-last-child(1 of :where([data-book-element])){margin:0;}[data-book-container=\"c1\"] p:where([data-book-element]):empty{margin:0;}[data-book-container=\"c1\"] p:where([data-book-element]):not(:nth-child(1 of :where([data-book-element]))){margin:0;}")

fn test_pseudo_dropped (): bool =
  _is("a:hover{margin:0} p:is(.a,.b){margin:0} p:where(.a){margin:0} q{margin:0}", 1,
    "[data-book-container=\"c1\"] q:where([data-book-element]){margin:0;}")

fn test_style_reflow (): bool =
  _style_is("margin:0;color:#333;width:url(x);position:fixed", 1, "margin:0;color:#333;")

fn test_style_fixed (): bool =
  _style_is("font-size:12px;color:red;position:absolute", 2, "font-size:12px;color:red;position:absolute;")

fn test_style_important (): bool =
  _style_is("margin:0 !important;padding:0", 1, "margin:0;padding:0;")

fn test_style_empty (): bool =
  _style_is("", 1, "")

fn test_refs_import (): bool = _refs_of("@import \"a.css\"; p{margin:0}") = 1

fn test_refs_font_face (): bool = _refs_of("@font-face{font-family:\"X\";src:url(x.otf)} p{margin:0}") = 1

fn test_refs_faces (): bool = _refs_of("@font-face{font-family:Foo;src:url('a.otf')} @font-face{font-family:Foo;font-weight:bold;src:url(b.otf)} @font-face{font-family:\"Bar\";font-style:italic;src:url(c.otf)}") = 5

fn test_refs_none (): bool = _refs_of("p{margin:0}") = 0

fn test_refused_0 (): bool = _refused("p{x:url(x)}")

fn test_refused_1 (): bool = _refused("p{x:image-set(x)}")

fn test_refused_2 (): bool = _refused("p{x:image(x)}")

fn test_refused_3 (): bool = _refused("p{x:src(x)}")

fn test_refused_4 (): bool = _refused("p{x:cross-fade(x)}")

fn test_refused_5 (): bool = _refused("p{x:element(x)}")

fn test_refused_6 (): bool = _refused("p{x:paint(x)}")

fn test_refused_7 (): bool = _refused("p{x:env(x)}")

fn test_refused_8 (): bool = _refused("p{x:var(--x)}")

fn test_refused_9 (): bool = _refused("p{x:attr(x)}")

fn test_refused_10 (): bool = _refused("p{x:expression(x)}")

fn test_refused_11 (): bool = _refused("p{x:@import \"a\"}")

fn test_refused_12 (): bool = _refused("p{x:@font-face{}}")

fn test_refused_13 (): bool = _refused("p{x:margin:0 !important}")

fn test_refused_14 (): bool = _refused("p{x:a\\b}")

fn test_refused_15 (): bool = _refused("p{x:<b>}")

fn test_refused_16 (): bool = _refused("p{x:/* c */}")

fn test_refused_17 (): bool = _refused("p{x:URL(x)}")

fn test_refused_18 (): bool = _refused("p{x:Url(x)}")

fn test_refused_19 (): bool = _refused("p{x:@IMPORT \"a\"}")

fn test_refused_clean (): bool = not(_refused("p{margin:0;color:#333}"))

fn test_refused_near_miss (): bool = not(_refused("p{margin:0;font-size:1em;background:red}"))

(* The table (keyword: refused table, bats-lang/css#88): book_refused is defined over it *)
fn test_refused_table_count (): bool = book_refused_count() = 17

(* Entry i and every later one is refused alone and inside a text *)
fun _entries_refused {i:nat | i <= 17} .<17 - i>. (i: int i): bool =
  if i >= 17 then true
  else if _refused(book_refused_entry(i)) then
    (if _refused_cat("p{x:", book_refused_entry(i)) then _entries_refused(i + 1) else false)
  else false

fn test_refused_table_every_entry (): bool = _entries_refused(0)

(* An entry with its last byte left off is no entry, in a text of its own *)
fn test_refused_table_clean (): bool =
  not(_refused("p{margin:0;ur(x);image-se(x);@impor;!importan;/ *;fon-face;var;vr(x)}"))

(* ---- the contrast table read at run time ---- *)

fn test_lin_of_black (): bool = let
  val @(pf | lo, hi) = lin_of(0)
in lo = 0 andalso hi = 0 end

fn test_lin_of_white (): bool = let
  val @(pf | lo, hi) = lin_of(255)
in lo = 100000 andalso hi = 100000 end

fn test_lin_of_brackets (): bool = let
  val @(pf | lo, hi) = lin_of(128)
in lo <= hi andalso hi - lo <= 1 andalso lo > 21000 andalso hi < 22000 end

fn test_contrast_black_on_white (): bool = let
  val @(pf | ok) = contrast_45_of(0, 0, 0, 255, 255, 255)
in ok end

fn test_contrast_white_on_black (): bool = let
  val @(pf | ok) = contrast_45_of(255, 255, 255, 0, 0, 0)
in ok end

fn test_contrast_grey_767676 (): bool = let
  val @(pf | ok) = contrast_45_of(118, 118, 118, 255, 255, 255)
in ok end

fn test_contrast_grey_777777 (): bool = let
  val @(pf | ok) = contrast_45_of(119, 119, 119, 255, 255, 255)
in not(ok) end

fn test_contrast_same (): bool = let
  val @(pf | ok) = contrast_45_of(200, 10, 10, 200, 10, 10)
in not(ok) end

end
