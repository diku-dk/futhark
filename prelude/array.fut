-- | Utility functions for arrays.

import "math"
import "soacs"
import "functional"
open import "zip"

-- | The size of the outer dimension of an array.
--
-- **Complexity:** O(1).
def length [n] 't (_: [n]t) = n

-- | Is the array empty?
--
-- **Complexity:** O(1).
def null [n] 't (_: [n]t) = n == 0

-- | The first element of the array.
--
-- **Complexity:** O(1).
#[inline]
def head [n] 't (x: [n]t) = x[0]

-- | The last element of the array.
--
-- **Complexity:** O(1).
#[inline]
def last [n] 't (x: [n]t) = x[n - 1]

-- | Everything but the first element of the array.
--
-- **Complexity:** O(1).
#[inline]
def tail [n] 't (x: [n]t) : [n - 1]t = x[1:]

-- | Everything but the last element of the array.
--
-- **Complexity:** O(1).
#[inline]
def init [n] 't (x: [n]t) : [n - 1]t = x[0:n - 1]

-- | Take some number of elements from the head of the array.
--
-- **Complexity:** O(1).
#[inline]
def take [n] 't (i: i64) (x: [n]t) : [i]t = x[0:i]

-- | Remove some number of elements from the head of the array.
--
-- **Complexity:** O(1).
#[inline]
def drop [n] 't (i: i64) (x: [n]t) : [n - i]t = x[i:]

-- | Statically change the size of an array.  Fail at runtime if the
-- imposed size does not match the actual size.  Essentially syntactic
-- sugar for a size coercion.
#[inline]
def sized [m] 't (n: i64) (xs: [m]t) : [n]t = xs :> [n]t

-- | Split an array at a given position.
--
-- **Complexity:** O(1).
#[inline]
def split [n] [m] 't (xs: [n + m]t) : ([n]t, [m]t) =
  (xs[0:n], xs[n:n + m] :> [m]t)

-- | Return the elements of the array in reverse order.
--
-- **Complexity:** O(1).
#[inline]
def reverse [n] 't (x: [n]t) : [n]t = x[::-1]

-- | Concatenate two arrays.  Warning: never try to perform a reduction
-- with this operator; it will not work.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
#[inline]
def (++) [n] [m] 't (xs: [n]t) (ys: [m]t) : *[n + m]t = intrinsics.concat xs ys

-- | An old-fashioned way of saying `++`.
#[inline]
def concat [n] [m] 't (xs: [n]t) (ys: [m]t) : *[n + m]t = xs ++ ys

-- | Construct an array of consecutive integers of the given length,
-- starting at 0.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
#[inline]
def iota (n: i64) : *[n]i64 =
  0..1..<n

-- | Construct an array comprising valid indexes into some other
-- array, starting at 0.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
#[inline]
def indices [n] 't (_: [n]t) : *[n]i64 =
  iota n

-- | Rotate an array some number of elements to the left.  A negative
-- rotation amount is also supported.
--
-- For example, if `b==rotate r a`, then `b[x] = a[x+r]`.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
--
-- Note: In most cases, `rotate` will be fused with subsequent
-- operations such as `map`, in which case it is free.
#[inline]
def rotate [n] 't (r: i64) (a: [n]t) =
  map (\i -> #[unsafe] a[(i + r) % n]) (iota n)

-- | Construct an array of the given length containing the given
-- value.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
#[inline]
def replicate 't (n: i64) (x: t) : *[n]t =
  map (const x) (iota n)

-- | Construct an array of an inferred length containing the given
-- value.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
#[inline]
def rep 't [n] (x: t) : *[n]t =
  replicate n x

-- | Copy a value.  The result will not alias anything.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
#[inline]
def copy 't (a: t) : *t =
  ([a])[0]

-- | Copy a value. The result will not alias anything. Additionally,
-- there is a guarantee that the result will be laid out in row-major
-- order in memory. This can be used for locality optimisations in
-- cases where the compiler does not otherwise do the right thing.
--
-- **Work:** O(n).
--
-- **Span:** O(1).
#[inline]
def manifest 't (a: t) : *t =
  intrinsics.manifest a

-- | Combines the outer two dimensions of an array.
--
-- **Complexity:** O(1).
#[inline]
def flatten [n] [m] 't (xs: [n][m]t) : [n * m]t =
  intrinsics.flatten xs

-- | Like `flatten`, but on the outer three dimensions of an array.
#[inline]
def flatten_3d [n] [m] [l] 't (xs: [n][m][l]t) : [n * m * l]t =
  flatten (flatten xs)

-- | Like `flatten`, but on the outer four dimensions of an array.
#[inline]
def flatten_4d [n] [m] [l] [k] 't (xs: [n][m][l][k]t) : [n * m * l * k]t =
  flatten (flatten_3d xs)

-- | Splits the outer dimension of an array in two.
--
-- **Complexity:** O(1).
#[inline]
def unflatten 't [n] [m] (xs: [n * m]t) : [n][m]t =
  intrinsics.unflatten n m xs

-- | Like `unflatten`, but produces three dimensions.
#[inline]
def unflatten_3d 't [n] [m] [l] (xs: [n * m * l]t) : [n][m][l]t =
  unflatten (unflatten xs)

-- | Like `unflatten`, but produces four dimensions.
#[inline]
def unflatten_4d 't [n] [m] [l] [k] (xs: [n * m * l * k]t) : [n][m][l][k]t =
  unflatten (unflatten_3d xs)

-- | Transpose an array.
--
-- **Complexity:** O(1).
#[inline]
def transpose [n] [m] 't (a: [n][m]t) : [m][n]t =
  intrinsics.transpose a

-- | True if all of the input elements are true.  Produces true on an
-- empty array.
--
-- **Work:** O(n).
--
-- **Span:** O(log(n)).
def and [n] (xs: [n]bool) = all id xs

-- | True if any of the input elements are true.  Produces false on an
-- empty array.
--
-- **Work:** O(n).
--
-- **Span:** O(log(n)).
def or [n] (xs: [n]bool) = any id xs

-- | Perform a *sequential* left-fold of an array.
--
-- **Work:** O(n ✕ W(f))).
--
-- **Span:** O(n ✕ S(f)).
def foldl [n] 'a 'b (f: a -> b -> a) (acc: a) (bs: [n]b) : a =
  loop acc for b in bs do f acc b

-- | Perform a *sequential* right-fold of an array.
--
-- **Work:** O(n ✕ W(f))).
--
-- **Span:** O(n ✕ S(f)).
def foldr [n] 'a 'b (f: b -> a -> a) (acc: a) (bs: [n]b) : a =
  foldl (flip f) acc (reverse bs)

-- | Create a value for each point in a one-dimensional index space.
--
-- **Work:** *O(n ✕ W(f))*
--
-- **Span:** *O(S(f))*
def tabulate 'a (n: i64) (f: i64 -> a) : *[n]a =
  map1 f (iota n)

-- | Create a value for each point in a two-dimensional index space.
--
-- **Work:** *O(n ✕ m ✕ W(f))*
--
-- **Span:** *O(S(f))*
def tabulate_2d 'a (n: i64) (m: i64) (f: i64 -> i64 -> a) : *[n][m]a =
  map1 (f >-> tabulate m) (iota n)

-- | Create a value for each point in a three-dimensional index space.
--
-- **Work:** *O(n ✕ m ✕ o ✕ W(f))*
--
-- **Span:** *O(S(f))*
def tabulate_3d 'a (n: i64) (m: i64) (o: i64) (f: i64 -> i64 -> i64 -> a) : *[n][m][o]a =
  map1 (f >-> tabulate_2d m o) (iota n)
