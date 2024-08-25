;;;; ops.lisp

(in-package #:specops.z80)

(specops nil (op0 &optional op1) *assembler-prototype-z80*
   ((:tabular :cross-adding :matcher match-ops))
   ((    ) (#x0       ) (#x1        ) (#x2        ) (#x3        ) (#x4        ) (#x5       ) (#x6        ) (#x7       ) (#x8       ) (#x9       ) (#xA        ) (#xB      ) (#xC        ) (#xD     ) (#xE        ) (#xF     ))
   ((#x00) (:nop      ) (:ld bc   xx) (:ld (bc)  a) (:inc     bc) (:inc      b) (:dec     b) (:ld     b x) (:rlca     ) (:ex af -af) (:add hl bc) (:ld  a (bc)) (:dec   bc) (:inc      c) (:dec   c) (:ld  c    x) (:rrca   ))
   ((#x10) (:djnz    x) (:ld de   xx) (:ld (de)  a) (:inc     de) (:inc      d) (:dec     d) (:ld     d x) (:rla      ) (:jr      x) (:add hl de) (:ld  a (de)) (:dec   de) (:inc      e) (:dec   e) (:ld  e    x) (:rra    ))
   ((#x20) (:jr   nz x) (:ld hl   xx) (:ld (xx) hl) (:inc     hl) (:inc      h) (:dec     h) (:ld     h x) (:daa      ) (:jr  z   x) (:add hl hl) (:ld hl (xx)) (:dec   hl) (:inc      l) (:dec   l) (:ld  l    x) (:cpl    ))
   ((#x30) (:jr   nc x) (:ld sp   xx) (:ld (xx)  a) (:inc     sp) (:inc   (hl)) (:dec  (hl)) (:ld  (hl) x) (:scf      ) (:jr  c   x) (:add hl sp) (:ld  a (xx)) (:dec   sp) (:inc      a) (:dec   a) (:ld  a    x) (:ccf    ))
   ((#x40) (:ld    b b) (:ld  b    c) (:ld    b  d) (:ld     b e) (:ld     b h) (:ld    b l) (:ld  b (hl)) (:ld    b a) (:ld  c   b) (:ld   c  c) (:ld  c    d) (:ld  c  e) (:ld    c  h) (:ld  c l) (:ld  c (hl)) (:ld  c a))
   ((#x50) (:ld    d b) (:ld  d    c) (:ld    d  d) (:ld     d e) (:ld     d h) (:ld    d l) (:ld  d (hl)) (:ld    d a) (:ld  e   b) (:ld   e  c) (:ld  e    d) (:ld  e  e) (:ld    e  h) (:ld  e l) (:ld  e (hl)) (:ld  e a))
   ((#x60) (:ld    h b) (:ld  h    c) (:ld    h  d) (:ld     h e) (:ld     h h) (:ld    h l) (:ld  h (hl)) (:ld    h a) (:ld  l   b) (:ld   l  c) (:ld  l    d) (:ld  l  e) (:ld    l  h) (:ld  l l) (:ld  l (hl)) (:ld  l a))
   ((#x70) (:ld (hl) b) (:ld (hl)  c) (:ld (hl)  d) (:ld  (hl) e) (:ld  (hl) h) (:ld (hl) l) (:halt      ) (:ld (hl) a) (:ld  a   b) (:ld   a  c) (:ld  a    d) (:ld  a  e) (:ld    a  h) (:ld  a l) (:ld  a (hl)) (:ld  a a))
   ((#x80) (:add   a b) (:add a    c) (:add   a  d) (:add    a e) (:add    a h) (:add   a l) (:add a (hl)) (:add   a a) (:adc a   b) (:adc  a  c) (:adc a    d) (:adc a  e) (:adc   a  h) (:adc a l) (:adc a (hl)) (:adc a a))
   ((#x90) (:sub     b) (:sub      c) (:sub      d) (:sub      e) (:sub      h) (:sub     l) (:sub   (hl)) (:sub     a) (:sbc a   b) (:sbc  a  c) (:sbc a    d) (:sbc a  e) (:sbc   a  h) (:sbc a l) (:sbc a (hl)) (:sbc a a))
   ((#xA0) (:and     b) (:and      c) (:and      d) (:and      e) (:and      h) (:and     l) (:and   (hl)) (:and     a) (:xor     b) (:xor     c) (:xor      d) (:xor    e) (:xor      h) (:xor   l) (:xor   (hl)) (:xor   a))
   ((#xB0) (:or      b) (:or       c) (:or       d) (:or       e) (:or       h) (:or      l) (:or    (hl)) (:or      a) (:cp      b) (:cp      c) (:cp       d) (:cp     e) (:cp       h) (:cp    l) (:cp    (hl)) (:cp    a))
   ((#xC0) (:ret    nz) (:pop     bc) (:jp   nz xx) (:jp      xx) (:call nz xx) (:push   bc) (:add    a x) (:rst   00h) (:ret     z) (:ret      ) (:jp  z   xx) ()          (:call  z xx) (:call xx) (:adc a    x) (:rst 08h))
   ((#xD0) (:ret    nc) (:pop     de) (:jp   nc xx) (:out (x)  a) (:call nc xx) (:push   de) (:sub      x) (:rst   10h) (:ret     c) (:exx      ) (:jp  c   xx) (:in a (x)) (:call  c xx) ()         (:sbc a    x) (:rst 18h))
   ((#xE0) (:ret    po) (:pop     hl) (:jp   po xx) (:ex (sp) hl) (:call po xx) (:push   hl) (:and      x) (:rst   20h) (:ret    pe) (:jp   (hl)) (:jp pe   xx) (:ex de hl) (:call pe xx) ()         (:xor      x) (:rst 28h))
   ((#xF0) (:ret     p) (:pop     af) (:jp    p xx) (:di        ) (:call  p xx) (:push   af) (:or       x) (:rst   30h) (:ret     m) (:ld  sp hl) (:jp  m   xx) (:ei      ) (:call  m xx) ()         (:cp       x) (:rst 38h)))
