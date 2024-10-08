-- Based on #1992, #1997.
-- ==
-- input { [1u8,2u8,3u8] }
module make (G: {
    val size: i64
    val const: u8 -> [size]u8
  }) = {
  def const_map (str: []u8): [][G.size]u8 = map G.const str
}

module thing = make
  ()
    {
      def size: i64 = 3
      
      def const (_: u8): [size]u8 = sized size [1, 2, 3]
    }: {val size: i64 val const: u8 -> [size]u8}
  )

entry main str = id (thing.const_map str)