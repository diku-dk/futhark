type triad 'a = (a, a, a)
type v3 = triad f32
type m33 = triad v3
entry toM33 a0 a1 a2 b0 b1 b2 c0 c1 c2 : m33 =
    ( (a0, a1, a2)
    , (b0, b1, b2)
    , (c0, c1, c2) )

entry fromM33 (m:m33) =
    ( m.0.0, m.0.1, m.0.2
    , m.1.0, m.1.1, m.1.2
    , m.2.0, m.2.1, m.2.2 )
