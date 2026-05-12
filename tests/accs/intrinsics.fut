-- We don't want to expose these constructs to users just yet, as they
-- are not terribly stable.

type~ acc 't = intrinsics.acc t

def scatter_stream [k] 'a 'b
                   (dest: *[k]a)
                   (f: *acc ([k]a) -> b -> acc ([k]a))
                   (bs: []b) : *[k]a =
  intrinsics.scatter_stream dest f bs :> *[k]a

def reduce_by_index_stream [k] 'a 'b
                           (dest: *[k]a)
                           (op: a -> a -> a)
                           (ne: a)
                           (f: *acc ([k]a) -> b -> acc ([k]a))
                           (bs: []b) : *[k]a =
  intrinsics.hist_stream dest op ne f bs :> *[k]a

def write [n] 't (acc: *acc ([n]t)) (i: i64) (v: t) : *acc ([n]t) =
  intrinsics.acc_write acc i v
