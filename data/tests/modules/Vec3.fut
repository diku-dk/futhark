-- ==
-- tags { disable }
structure Vec3 =
  struct
    structure F32 =
      struct
        type t = ( f32 , f32 , f32 )
        fun t add(t a , t b) =
          let (a1, a2, a3) = a in
          let (b1, b2, b3) = b in
          (a1 + b1, a2 + b2 , a3 + b3)
    
        fun t subtract(t a , t b) =
          let (a1, a2, a3) = a in
          let (b1, b2, b3) = b in
          (a1 - b1, a2 - b2 , a3 - b3)
    
        fun t scale(f32 k , t a) =
          let (a1, a2, a3) = a in
          (a1 * k, a2 * k , a3 * k)
    
        fun f32 dot(t a , t b) =
          let (a1, a2, a3) = a in
          let (b1, b2, b3) = b in
          a1*b1 + a2*b2 + a3*b3
      end
    
    structure Int =
      struct
        type t = ( int , int , int )
        fun t add(t a , t b) =
          let (a1, a2, a3) = a in
          let (b1, b2, b3) = b in
          (a1 + b1, a2 + b2 , a3 + b3)
    
        fun t subtract(t a , t b) =
          let (a1, a2, a3) = a in
          let (b1, b2, b3) = b in
          (a1 - b1, a2 - b2 , a3 - b3)
    
        fun t scale(int k , t a) =
          let (a1, a2, a3) = a in
          (a1 * k, a2 * k , a3 * k)
    
        fun int dot(t a , t b) =
          let (a1, a2, a3) = a in
          let (b1, b2, b3) = b in
          a1*b1 + a2*b2 + a3*b3
      end
  end
