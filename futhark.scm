(define-module (gnu packages futhark)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system haskell)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto))

(define-public ghc-indexed-traversable
  (package
    (name "ghc-indexed-traversable")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "indexed-traversable" version))
       (sha256
        (base32 "13b91rkhs6wcshaz3dwx6x3xjpw5z5bm2riwp78zxccqf7p5hs2i"))))
    (build-system haskell-build-system)
    (arguments
     `(#:cabal-revision
       ("1" "0rbcfl0iklix3ppfkxh88y70qmm64lg1l4679z5krya2fa42hqnn")))
    (home-page "http://hackage.haskell.org/package/indexed-traversable")
    (synopsis "FunctorWithIndex, FoldableWithIndex, TraversableWithIndex")
    (description "ghc-indexed-traversable")
    ;; (description
;;      "This package provides three useful generalizations: . @ class Functor f =>
;; FunctorWithIndex i f | f -> i where \\  imap :: (i -> a -> b) -> f a -> f b @ . @
;; class Foldable f => FoldableWithIndex i f | f -> i where \\  ifoldMap :: Monoid m
;; => (i -> a -> m) -> f a -> m @ . @ class (FunctorWithIndex i t,
;; FoldableWithIndex i t, Traversable t) => TraversableWithIndex i t | t -> i where
;; \\  itraverse :: Applicative f => (i -> a -> f b) -> t a -> f (t b) @ .  This
;; package contains instances for types in GHC boot libraries.  For some additional
;; instances see
;; [indexed-traversable-instances](https://hackage.haskell.org/package/indexed-traversable-instances).
;; .  The [keys](https://hackage.haskell.org/package/keys) package provides similar
;; functionality, but uses (associated) @TypeFamilies@ instead of
;; @FunctionalDependencies@.")
    (license license:bsd-2)))

(define-public ghc-splitmix
  (package
    (name "ghc-splitmix")
    (version "0.1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "splitmix" version))
       (sha256
        (base32 "1apck3nzzl58r0b9al7cwaqwjhhkl8q4bfrx14br2yjf741581kd"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (home-page "http://hackage.haskell.org/package/splitmix")
    (synopsis "Fast Splittable PRNG")
    (description "ghc-splitmix")
;;     (description
;;      "Pure Haskell implementation of SplitMix described in .  Guy L.  Steele, Jr.,
;; Doug Lea, and Christine H.  Flood.  2014.  Fast splittable pseudorandom number
;; generators.  In Proceedings of the 2014 ACM International Conference on Object
;; Oriented Programming Systems Languages & Applications (OOPSLA '14).  ACM, New
;; York, NY, USA, 453-472.  DOI: <https://doi.org/10.1145/2660193.2660195> .  The
;; paper describes a new algorithm /SplitMix/ for /splittable/ pseudorandom number
;; generator that is quite fast: 9 64 bit arithmetic/logical operations per 64 bits
;; generated. . /SplitMix/ is tested with two standard statistical test suites
;; (DieHarder and TestU01, this implementation only using the former) and it
;; appears to be adequate for \"everyday\" use, such as Monte Carlo algorithms and
;; randomized data structures where speed is important. .  In particular, it
;; __should not be used for cryptographic or security applications__, because
;; generated sequences of pseudorandom values are too predictable (the mixing
;; functions are easily inverted, and two successive outputs suffice to reconstruct
;; the internal state).")
    (license license:bsd-3)))

(define-public ghc-random
  (package
    (name "ghc-random")
    (version "1.2.0")
    (outputs '("out" "static" "doc"))
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/random/random-"
             version
             ".tar.gz"))
       (sha256
        (base32 "1pmr7zbbqg58kihhhwj8figf5jdchhi7ik2apsyxbgsqq3vrqlg4"))))
    (build-system haskell-build-system)
    (arguments
     `(#:tests? #f
       #:cabal-revision
       ("6" "1hzfz9b1cxrsya8i53yx145iypaakfsfjix7l8girhx7vbz0cm8r")))
    (inputs `(("ghc-splitmix" ,ghc-splitmix)))
   ;; ghc-random is widely used and causes quite a few loops.
;    (native-inputs
;     `(("ghc-doctest" ,ghc-doctest)
;       ("ghc-mwc-random" ,ghc-mwc-random)
;       ("ghc-primitive" ,ghc-primitive)
;       ("ghc-unliftio" ,ghc-unliftio)
;       ("ghc-vector" ,ghc-vector)
;       ("ghc-smallcheck" ,ghc-smallcheck)
;       ("ghc-tasty" ,ghc-tasty)
;       ("ghc-tasty-smallcheck" ,ghc-tasty-smallcheck)
;       ("ghc-tasty-expected-failure" ,ghc-tasty-expected-failure)
;       ("ghc-tasty-hunit" ,ghc-tasty-hunit)))
    (home-page "https://hackage.haskell.org/package/random")
    (synopsis "Random number library")
    (description "This package provides a basic random number generation
library, including the ability to split random number generators.")
    (license license:bsd-3)))

(define-public ghc-quickcheck
  (package
    (name "ghc-quickcheck")
    (version "2.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://hackage.haskell.org/package/QuickCheck/QuickCheck-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "1wrnrm9sq4s0bly0q58y80g4153q45iglqa34xsi2q3bd62nqyyq"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-random ghc-splitmix))
    (home-page "https://github.com/nick8325/quickcheck")
    (synopsis "Automatic testing of Haskell programs")
    (description
     "QuickCheck is a library for random testing of program properties.  The
programmer provides a specification of the program, in the form of properties
which functions should satisfy, and QuickCheck then tests that the properties
hold in a large number of randomly generated cases.  Specifications are
expressed in Haskell, using combinators defined in the QuickCheck library.")
    (license license:bsd-3)))


(define-public ghc-base-orphans
  (package
    (name "ghc-base-orphans")
    (version "0.8.6")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "base-orphans" version))
        (sha256
          (base32 "1sqmh3x07aw2l2vzj8dhhs1a41kl8r0n46ayn92dh3yvg95ir8i0"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    ;; (native-inputs (list ghc-quickcheck ghc-hspec hspec-discover))
    (home-page "https://github.com/haskell-compat/base-orphans#readme")
    (synopsis "Backwards-compatible orphan instances for base")
    (description "base-orphans")
;;     (description
;;       "@base-orphans@ defines orphan instances that mimic instances available in later
;; versions of @base@ to a wider (older) range of compilers. @base-orphans@ does
;; not export anything except the orphan instances themselves and complements
;; @<http://hackage.haskell.org/package/base-compat base-compat>@. .  See the
;; README for what instances are covered:
;; <https://github.com/haskell-compat/base-orphans#readme>.  See also the
;; <https://github.com/haskell-compat/base-orphans#what-is-not-covered what is not
;; covered> section.")
    (license license:expat)))

(define-public ghc-hashable
  (package
    (name "ghc-hashable")
    (version "1.4.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "hashable" version))
        (sha256
          (base32 "003nlhi8wzlsikfwi0q6ma3b38wizvazci6dbq3lr4bd6pfnwq43"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-base-orphans))
    ;; (native-inputs
    ;;   (list ghc-hunit
    ;;         ghc-quickcheck
    ;;         ghc-random
    ;;         ghc-test-framework
    ;;         ghc-test-framework-hunit
    ;;         ghc-test-framework-quickcheck2))
    (home-page "http://github.com/haskell-unordered-containers/hashable")
    (synopsis "A class for types that can be converted to a hash value")
    (description
      "This package defines a class, 'Hashable', for types that can be converted to a
hash value.  This class exists for the benefit of hashing-based data structures.
 The package provides instances for basic types and a way to combine hash
values.")
    (license license:bsd-3)))

(define-public ghc-onetuple
  (package
    (name "ghc-onetuple")
    (version "0.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "OneTuple" version))
        (sha256
          (base32 "1vry21z449ph9k61l5zm7mfmdwkwszxqdlawlhvwrd1gsn13d1cq"))))
    (build-system haskell-build-system)
    (inputs (list ghc-hashable ghc-base-orphans))
    (arguments
      `(#:cabal-revision
        ("2" "0gk0656igxl0km9kgh8v7b5vq74kla59ka9hvpzq57njr6bc0j58")))
    (home-page "http://hackage.haskell.org/package/OneTuple")
    (synopsis "Singleton Tuple")
    (description "onetuple")
;;     (description
;;       "This package is a compatibility package for a singleton data type . > data Solo
;; a = Solo a .  Note: it's not a @newtype@ . @Solo@ is available in @base-4.16@
;; (GHC-9.2).")
    (license license:bsd-3)))

(define-public ghc-primitive
  (package
    (name "ghc-primitive")
    (version "0.7.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "primitive" version))
       (sha256
        (base32 "1p01fmw8yi578rvwicrlpbfkbfsv7fbnzb88a7vggrhygykgs31w"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    ;; (native-inputs
    ;;  (list ghc-base-orphans
    ;;        ghc-quickcheck-classes-base
    ;;        ghc-quickcheck
    ;;        ghc-tasty
    ;;        ghc-tasty-quickcheck
    ;;        ghc-tagged
    ;;        ghc-transformers-compat))
    (home-page "https://github.com/haskell/primitive")
    (synopsis "Primitive memory-related operations")
    (description
     "This package provides various primitive memory-related operations.")
    (license license:bsd-3)))

(define-public ghc-vector
  (package
    (name "ghc-vector")
    (version "0.12.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector" version))
       (sha256
        (base32 "0dczbcisxhhix859dng5zhxkn3xvlnllsq60apqzvmyl5g056jpv"))))
    (build-system haskell-build-system)
    (arguments `(#:cabal-revision
                 ("1" "02284cr5f5ghbz18shn8g6jvsgfs0dwgf81kxvf59r2wks8i00h4")
                 #:tests? #f))
    (inputs (list ghc-primitive))
    ;; (native-inputs
    ;;  (list ghc-base-orphans
    ;;        ghc-random
    ;;        ghc-quickcheck
    ;;        ghc-hunit
    ;;        ghc-tasty
    ;;        ghc-tasty-hunit
    ;;        ghc-tasty-quickcheck
    ;;        ghc-base-orphans
    ;;        ghc-random
    ;;        ghc-quickcheck
    ;;        ghc-hunit
    ;;        ghc-tasty
    ;;        ghc-tasty-hunit
    ;;        ghc-tasty-quickcheck
    ;;        ghc-doctest))
    (home-page "https://github.com/haskell/vector")
    (synopsis "Efficient Arrays")
    (description "vector")
;;     (description
;;      ".  An efficient implementation of Int-indexed arrays (both mutable and
;; immutable), with a powerful loop optimisation framework . .  It is structured as
;; follows: . [\"Data.Vector\"] Boxed vectors of arbitrary types. .
;; [\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive representation based on
;; data type families. . [\"Data.Vector.Storable\"] Unboxed vectors of 'Storable'
;; types. . [\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as defined
;; by the @primitive@ package. \"Data.Vector.Unboxed\" is more flexible at no
;; performance cost. . [\"Data.Vector.Generic\"] Generic interface to the vector
;; types. .  There is also a (draft) tutorial on common uses of vector. . *
;; <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>")
    (license license:bsd-3)))

(define-public ghc-time-compat
  (package
    (name "ghc-time-compat")
    (version "1.9.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "time-compat" version))
       (sha256
        (base32 "103b3vpn277kkccv6jv54b2wpi5c00mpb01ndl9w4y4nxc0bn1xd"))))
    (build-system haskell-build-system)
    (arguments `(#:cabal-revision
                 ("3" "1lafp8yk2n8g873ivi36gnwd8syhw5lssm3xj4c1fplnivhg5n22")
                 #:tests? #f))
    (inputs (list ghc-base-orphans ghc-hashable))
    ;; (native-inputs
    ;;  (list ghc-hunit
    ;;        ghc-base-compat
    ;;        ghc-quickcheck
    ;;        ghc-tagged
    ;;        ghc-tasty
    ;;        ghc-tasty-hunit
    ;;        ghc-tasty-quickcheck))
    (home-page "https://github.com/haskellari/time-compat")
    (synopsis "Compatibility package for time")
    (description "time-compat")
;;     (description
;;      "This packages tries to compat as much of @time@ features as possible. . /TODO:/
;; . * Difference type @ParseTime@ and @FormatTime@ instances are missing. . *
;; Formatting varies depending on underlying @time@ version . *
;; @dayFractionToTimeOfDay@ on extreme values")
    (license license:bsd-3)))

(define-public ghc-nothunks
  (package
    (name "ghc-nothunks")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "nothunks" version))
       (sha256
        (base32 "0lqfhnyxhmhajvsgmz5h428pb5zrdy9zvbc5inzhd83cv31yk4f1"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-vector))
    ;; (native-inputs (list ghc-hedgehog ghc-random ghc-tasty ghc-tasty-hedgehog))
    (home-page "http://hackage.haskell.org/package/nothunks")
    (synopsis "Examine values for unexpected thunks")
    (description
     "Long lived application data typically should not contain any thunks.  This
library can be used to examine values for unexpected thunks, which can then be
used in assertions.  This can be invaluable in avoiding memory leaks, or
tracking down existing ones.")
    (license license:expat)))

(define-public ghc-unordered-containers
  (package
    (name "ghc-unordered-containers")
    (version "0.2.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "unordered-containers" version))
       (sha256
        (base32 "09cvqdqaqbf0z5i0hbkgn7hkz44plniznj6zimdx0a86i6lhq3b2"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-hashable))
    ;; (native-inputs
    ;;  (list ghc-chasingbottoms
    ;;        ghc-hunit
    ;;        ghc-quickcheck
    ;;        ghc-random
    ;;        ghc-tasty
    ;;        ghc-tasty-hunit
    ;;        ghc-tasty-quickcheck
    ;;        ghc-nothunks))
    (home-page
     "https://github.com/haskell-unordered-containers/unordered-containers")
    (synopsis "Efficient hashing-based container types")
    (description "unordered-containers")
;;     (description
;;      "Efficient hashing-based container types.  The containers have been optimized for
;; performance critical use, both in terms of large data quantities and high speed.
;; .  The declared cost of each operation is either worst-case or amortized, but
;; remains valid even if structures are shared. . /Security/ .  This package
;; currently provides no defenses against hash collision attacks such as HashDoS.
;; Users who need to store input from untrusted sources are advised to use
;; @Data.Map@ or @Data.Set@ from the @containers@ package instead.")
    (license license:bsd-3)))

(define-public ghc-scientific
  (package
    (name "ghc-scientific")
    (version "0.3.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "scientific" version))
       (sha256
        (base32 "1aa3ngb71l2sh1x2829napnr1w285q0sn2f7z2wvi3ynng2238d3"))))
    (build-system haskell-build-system)
    (arguments `(#:cabal-revision
                 ("2" "01vmr4pz1j0xjcln61m7gng6bzhgri56h05x7sl6xbxjps15likn")
                 #:tests? #f))
    (inputs (list ghc-hashable ghc-integer-logarithms ghc-primitive))
    ;; (native-inputs
    ;;  (list ghc-quickcheck
    ;;        ghc-smallcheck
    ;;        ghc-tasty
    ;;        ghc-tasty-hunit
    ;;        ghc-tasty-quickcheck
    ;;        ghc-tasty-smallcheck))
    (home-page "https://github.com/basvandijk/scientific")
    (synopsis "Numbers represented using scientific notation")
    (description "ghc-scientific")
    ;; (description
;;      "\"Data.Scientific\" provides the number type 'Scientific'.  Scientific numbers are
;; arbitrary precision and space efficient.  They are represented using
;; <http://en.wikipedia.org/wiki/Scientific_notation scientific notation>.  The
;; implementation uses a coefficient @c :: 'Integer'@ and a base-10 exponent @e ::
;; 'Int'@.  A scientific number corresponds to the 'Fractional' number:
;; @'fromInteger' c * 10 '^^' e@. .  Note that since we're using an 'Int' to
;; represent the exponent these numbers aren't truly arbitrary precision.  I intend
;; to change the type of the exponent to 'Integer' in a future release. .  The main
;; application of 'Scientific' is to be used as the target of parsing arbitrary
;; precision numbers coming from an untrusted source.  The advantages over using
;; 'Rational' for this are that: . * A 'Scientific' is more efficient to construct.
;;  Rational numbers need to be constructed using '%' which has to compute the
;; 'gcd' of the 'numerator' and 'denominator'. . * 'Scientific' is safe against
;; numbers with huge exponents.  For example: @1e1000000000 :: 'Rational'@ will
;; fill up all space and crash your program.  Scientific works as expected: . >>>
;; read \"1e1000000000\" :: Scientific 1.0e1000000000 . * Also, the space usage of
;; converting scientific numbers with huge exponents to @'Integral's@ (like: 'Int')
;; or @'RealFloat's@ (like: 'Double' or 'Float') will always be bounded by the
;; target type.")
    (license license:bsd-3)))

(define-public ghc-text-short
  (package
    (name "ghc-text-short")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "text-short" version))
       (sha256
        (base32 "1nid00c1rg5c1z7l9mwk3f2izc2sps2mip2hl30q985dwb6wcpm3"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-hashable))
    ;; (native-inputs (list ghc-tasty ghc-tasty-quickcheck ghc-tasty-hunit))
    (home-page "http://hackage.haskell.org/package/text-short")
    (synopsis "Memory-efficient representation of Unicode text strings")
    (description "ghc-text-short")
    ;; (description
;;      "This package provides the 'ShortText' type which is suitable for keeping many
;; short strings in memory.  This is similiar to how 'ShortByteString' relates to
;; 'ByteString'. .  The main difference between 'Text' and 'ShortText' is that
;; 'ShortText' uses UTF-8 instead of UTF-16 internally and also doesn't support
;; zero-copy slicing (thereby saving 2 words).  Consequently, the memory footprint
;; of a (boxed) 'ShortText' value is 4 words (2 words when unboxed) plus the length
;; of the UTF-8 encoded payload.")
    (license license:bsd-3)))

(define-public ghc-assoc
  (package
    (name "ghc-assoc")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "assoc" version))
       (sha256
        (base32 "0kqlizznjy94fm8zr1ng633yxbinjff7cnsiaqs7m33ix338v66q"))))
    (build-system haskell-build-system)
    (inputs (list ghc-bifunctors ghc-tagged))
    (arguments
     `(#:cabal-revision
       ("2" "17jd3668y4j3kwsqgzqjrxymbglhgzgjbkda48djlbhppkzd3ng0")))
    (home-page "http://hackage.haskell.org/package/assoc")
    (synopsis "swap and assoc: Symmetric and Semigroupy Bifunctors")
    (description "ghc-assoc")
;;     (description
;;      "This package provides generalisations of @swap :: (a,b) -> (b,a)@ and @assoc ::
;; ((a,b),c) -> (a,(b,c))@ to @Bifunctor@s supporting similar operations (e.g.
;; @Either@, @These@).")
    (license license:bsd-3)))

(define-public ghc-these
  (package
    (name "ghc-these")
    (version "1.1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "these" version))
       (sha256
        (base32 "027m1gd7i6jf2ppfkld9qrv3xnxg276587pmx10z9phpdvswk66p"))))
    (build-system haskell-build-system)
    (inputs (list ghc-hashable ghc-assoc))
    (arguments
     `(#:cabal-revision
       ("5" "0jk8cyxlvwfxg3j3cxixs36rnlswgwcwq86agx2kvmzyp1kffsgh")
       #:tests? #f))
    (home-page "https://github.com/haskellari/these")
    (synopsis "An either-or-both data type.")
    (description "ghc-these")
;;     (description
;;      "This package provides a data type @These a b@ which can hold a value of either
;; type or values of each type.  This is usually thought of as an \"inclusive or\"
;; type (contrasting @Either a b@ as \"exclusive or\") or as an \"outer join\" type
;; (contrasting @(a, b)@ as \"inner join\"). . @ data These a b = This a | That b |
;; These a b @ .  Since version 1, this package was split into parts: . *
;; <https://hackage.haskell.org/package/semialign semialign> For @Align@ and @Zip@
;; type-classes. . * <https://hackage.haskell.org/package/semialign-indexed
;; semialign-indexed> For @SemialignWithIndex@ class, providing @ialignWith@ and
;; @izipWith@. . * <https://hackage.haskell.org/package/these-lens these-lens> For
;; lens combinators. . * <http://hackage.haskell.org/package/monad-chronicle
;; monad-chronicle> For transformers variant of @These@.")
    (license license:bsd-3)))


(define-public ghc-case-insensitive
  (package
    (name "ghc-case-insensitive")
    (version "1.2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "case-insensitive" version))
       (sha256
        (base32 "01p40hfjyldfds5jg6vlvvn3ihs4ki63xn6fh8yzngaz1izc2v99"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-hashable))
    ;; (native-inputs (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (home-page "https://github.com/basvandijk/case-insensitive")
    (synopsis "Case insensitive string comparison")
    (description "ghc-case-insensitive")
    ;; (description
;;      "The module @Data.CaseInsensitive@ provides the 'CI' type constructor which can
;; be parameterised by a string-like type like: 'String', 'ByteString', 'Text',
;; etc..  Comparisons of values of the resulting type will be insensitive to cases.")
    (license license:bsd-3)))

(define-public ghc-data-fix
  (package
    (name "ghc-data-fix")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "data-fix" version))
       (sha256
        (base32 "1k0rcbb6dzv0ggdxqa2bh4jr829y0bczjrg98mrk5733q0xjs5rs"))))
    (build-system haskell-build-system)
    (inputs (list ghc-hashable))
    (arguments
     `(#:cabal-revision
       ("2" "0ymn341kg2c1wf1vp04v25bpnf857krhv91q4kl7b2k4h5ipf2g9")))
    (home-page "https://github.com/spell-music/data-fix")
    (synopsis "Fixpoint data types")
    (description
     "Fixpoint types and recursion schemes.  If you define your AST as fixpoint type,
you get fold and unfold operations for free. .  Thanks for contribution to:
Matej Kollar, Herbert Valerio Riedel")
    (license license:bsd-3)))

(define-public ghc-strict
  (package
    (name "ghc-strict")
    (version "0.4.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "strict" version))
       (sha256
        (base32 "0hb24a09c3agsq7sdv8r2b2jc2f4g1blg2xvj4cfadynib0apxnz"))))
    (build-system haskell-build-system)
    (inputs (list ghc-hashable ghc-these ghc-assoc))
    (arguments
     `(#:cabal-revision
       ("4" "0pdzqhy7z70m8gxcr54jf04qhncl1jbvwybigb8lrnxqirs5l86n")))
    (home-page "https://github.com/haskell-strict/strict")
    (synopsis "Strict data types and String IO.")
    (description "ghc-strict")
    ;; (description
;;      "This package provides strict versions of some standard Haskell data types
;; (pairs, Maybe and Either).  It also contains strict IO operations. .  It is
;; common knowledge that lazy datastructures can lead to space-leaks.  This problem
;; is particularly prominent, when using lazy datastructures to store the state of
;; a long-running application in memory.  One common solution to this problem is to
;; use @seq@ and its variants in every piece of code that updates your state.
;; However a much easier solution is to use fully strict types to store such state
;; values.  By \\\"fully strict types\\\" we mean types for whose values it holds that,
;; if they are in weak-head normal form, then they are also in normal form.
;; Intuitively, this means that values of fully strict types cannot contain
;; unevaluated thunks. .  To define a fully strict datatype, one typically uses the
;; following recipe. .  1.  Make all fields of every constructor strict; i.e., add
;; a bang to all fields. .  2.  Use only strict types for the fields of the
;; constructors. .  The second requirement is problematic as it rules out the use
;; of the standard Haskell 'Maybe', 'Either', and pair types.  This library solves
;; this problem by providing strict variants of these types and their corresponding
;; standard support functions and type-class instances. .  Note that this library
;; does currently not provide fully strict lists.  They can be added if they are
;; really required.  However, in many cases one probably wants to use unboxed or
;; strict boxed vectors from the 'vector' library
;; (<http://hackage.haskell.org/package/vector>) instead of strict lists.
;; Moreover, instead of @String@s one probably wants to use strict @Text@ values
;; from the @text@ library (<http://hackage.haskell.org/package/text>). .  This
;; library comes with batteries included; i.e., mirror functions and instances of
;; the lazy versions in @base@.  It also includes instances for type-classes from
;; the @deepseq@, @binary@, and @hashable@ packages.")
    (license license:bsd-3)))

(define-public ghc-uuid-types
  (package
    (name "ghc-uuid-types")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "uuid-types" version))
       (sha256
        (base32 "1pd7xd6inkmmwjscf7pmiwqjks9y0gi1p8ahqbapvh34gadvhs5d"))))
    (build-system haskell-build-system)
    (inputs (list ghc-hashable ghc-random))
    ;; (native-inputs
    ;;  (list ghc-byteorder
    ;;        ghc-quickcheck
    ;;        ghc-tasty
    ;;        ghc-tasty-hunit
    ;;        ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision
       ("2" "0x3limqb67l4i0lfdaqgqbjak7mi7ydk5dhkv80791r3hyhbhiw4")
       #:tests? #f))
    (home-page "https://github.com/haskell-hvr/uuid")
    (synopsis "Type definitions for Universally Unique Identifiers")
    (description "ghc-uuid-types")
;;     (description
;;      "This library contains type definitions for <https://en.wikipedia.org/wiki/UUID
;; Universally Unique Identifiers (UUID)> (as specified in
;; <http://tools.ietf.org/html/rfc4122 RFC 4122>) and basic conversion functions. .
;;  See also the <https://hackage.haskell.org/package/uuid 'uuid' package>
;; providing a high-level API for managing the different UUID versions.")
    (license license:bsd-3)))

(define-public ghc-quickcheck-instances
  (package
    (name "ghc-quickcheck-instances")
    (version "0.3.27")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "quickcheck-instances" version))
        (sha256
          (base32 "10vb3rl1ma9x4qdych9vn8gj9kngkqs2b97f4s6s1a908ddxv4m5"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-quickcheck
            ghc-splitmix
            ghc-case-insensitive
            ghc-data-fix
            ghc-hashable
            ghc-integer-logarithms
            ghc-old-time
            ghc-onetuple
            ghc-scientific
            ghc-strict
            ghc-tagged
            ghc-these
            ghc-time-compat
            ghc-transformers-compat
            ghc-unordered-containers
            ghc-uuid-types
            ghc-vector
            ghc-text-short))
    (arguments
      `(#:cabal-revision
        ("1" "1b17ghhhrw9h625q08145pdpw72xmava73d3xb933slza5jms6nz")))
    (home-page "https://github.com/haskellari/qc-instances")
    (synopsis "Common quickcheck instances")
    (description
      "QuickCheck instances. .  The goal is to supply QuickCheck instances for types
provided by the Haskell Platform. .  Since all of these instances are provided
as orphans, I recommend that you do not use this library within another library
module, so that you don't impose these instances on down-stream consumers of
your code. .  For information on writing a test-suite with Cabal see
<https://www.haskell.org/cabal/users-guide/developing-packages.html#test-suites>")
    (license license:bsd-3)))

(define-public ghc-indexed-traversable-instances
  (package
    (name "ghc-indexed-traversable-instances")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "indexed-traversable-instances" version))
        (sha256
          (base32 "0i4s8fbqbgvkd2na48zwhlrcjpwxkx5rdh6f9fq2h4sl7c1d23hh"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs
      (list ghc-indexed-traversable
            ghc-onetuple
            ghc-tagged
            ghc-unordered-containers
            ghc-vector))
    ;; (native-inputs
    ;;   (list ghc-quickcheck
    ;;         ghc-quickcheck-instances
    ;;         ghc-tasty
    ;;         ghc-tasty-quickcheck))
    (home-page
      "http://hackage.haskell.org/package/indexed-traversable-instances")
    (synopsis
     "More instances of FunctorWithIndex, FoldableWithIndex, TraversableWithIndex")
    (description "indexed-traversable-instances")
;;     (description
;;       "This package provides extra instances for type-classes in the
;; [indexed-traversable](https://hackage.haskell.org/package/indexed-traversable)
;; package. .  The intention is to keep this package minimal; it provides instances
;; that formely existed in @lens@ or @optics-extra@.  We recommend putting other
;; instances directly into their defining packages.  The @indexed-traversable@
;; package is light, having only GHC boot libraries as its dependencies.")
    (license license:bsd-2)))

(define-public ghc-witherable
  (package
    (name "ghc-witherable")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "witherable" version))
        (sha256
          (base32 "0121ic4xkv3k568j23zp22a5lrv0k11h94fq7cbijd18fjr2n3br"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-base-orphans
            ghc-hashable
            ghc-unordered-containers
            ghc-vector
            ghc-indexed-traversable
            ghc-indexed-traversable-instances))
    ;; (native-inputs
    ;;   (list ghc-quickcheck
    ;;         ghc-quickcheck-instances
    ;;         ghc-tasty
    ;;         ghc-tasty-quickcheck))
    (arguments
      `(#:cabal-revision
        ("2" "1ljnv5xf6w7x58akj0a0yw16j63jkka0dvfvmjqwbn76aqg3pzc1")
        #:tests? #f))
    (home-page "https://github.com/fumieval/witherable")
    (synopsis "filterable traversable")
    (description
      "This package provides a stronger variant of `traverse` which can remove elements
and generalised mapMaybe, catMaybes, filter")
    (license license:bsd-3)))

(define-public ghc-attoparsec
  (package
    (name "ghc-attoparsec")
    (version "0.14.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "attoparsec" version))
       (sha256
        (base32 "0v4yjz4qi8bwhbyavqxlhsfb1iv07v10gxi64khmsmi4hvjpycrz"))))
    (build-system haskell-build-system)
    (inputs (list ghc-scientific))
    ;; (native-inputs
    ;;  (list ghc-quickcheck
    ;;        ghc-quickcheck-unicode
    ;;        ghc-tasty
    ;;        ghc-tasty-quickcheck
    ;;        ghc-vector))
    (arguments
     `(#:cabal-revision
       ("1" "149ihklmwnl13mmixq6iq5gzggkgqwsqrjlg2fshqwwbvbd4nn3r")
       #:tests? #f))
    (home-page "https://github.com/bgamari/attoparsec")
    (synopsis "Fast combinator parsing for bytestrings and text")
    (description
     "This package provides a fast parser combinator library, aimed particularly at
dealing efficiently with network protocols and complicated text/binary file
formats.")
    (license license:bsd-3)))

(define-public ghc-comonad
  (package
    (name "ghc-comonad")
    (version "5.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "comonad" version))
       (sha256
        (base32 "04rxycp2pbkrvhjgpgx08jmsipjz4cdmhv59dbp47k4jq8ndyv7g"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-tagged
           ghc-transformers-compat
           ghc-distributive
           ghc-indexed-traversable))
    (arguments
     `(#:cabal-revision
       ("1" "0zlgkcd61cwsdbgjz03pfbjxhj6dc25792h7rwh0zy677vbsn6hz")))
    (home-page "http://github.com/ekmett/comonad/")
    (synopsis "Comonads")
    (description "Comonads.")
    (license license:bsd-3)))

(define-public ghc-bifunctors
  (package
    (name "ghc-bifunctors")
    (version "5.5.11")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "bifunctors" version))
       (sha256
        (base32 "070964w7gz578379lyj6xvdbcf367csmz22cryarjr5bz9r9csrb"))))
    (build-system haskell-build-system)
    (inputs (list ghc-base-orphans ghc-comonad ghc-th-abstraction ghc-tagged))
    ;; (native-inputs (list ghc-hspec ghc-quickcheck ghc-transformers-compat hspec-discover))
    (arguments
     `(#:cabal-revision
       ("1" "1xl5xqr76k7ixq2bjszjh83xkg3szarnzbrv2ahxnmmfbbl5whnc")
       #:tests? #f))
    (home-page "http://github.com/ekmett/bifunctors/")
    (synopsis "Bifunctors")
    (description "Bifunctors.")
    (license license:bsd-3)))

(define-public ghc-distributive
  (package
    (name "ghc-distributive")
    (version "0.6.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "distributive" version))
       (sha256
        (base32 "14bb66qyfn43bj688igfvnfjw7iycjf4n2k38sm8rxbqw2916dfp"))))
    (build-system haskell-build-system)
    (inputs (list ghc-base-orphans ghc-tagged))
    ;; (native-inputs (list ghc-generic-deriving ghc-hspec hspec-discover))
    (arguments
     `(#:cabal-revision
       ("1" "033890dfyd23dh7g7px863l0hr1b881jnhv4kgwaq16a3iagb68g")
       #:tests? #f))
    (home-page "http://github.com/ekmett/distributive/")
    (synopsis "Distributive functors -- Dual to Traversable")
    (description "ghc-distributive")
    ;; (description "Distributive functors -- Dual to @Traversable@")
    (license license:bsd-3)))


(define-public ghc-semigroupoids
  (package
    (name "ghc-semigroupoids")
    (version "5.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "semigroupoids" version))
       (sha256
        (base32 "169pjrm7lxjxrqj5q1iyl288bx5nj8n0pf2ri1cclxccqnvcsibd"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-base-orphans
           ghc-bifunctors
           ghc-transformers-compat
           ghc-contravariant
           ghc-distributive
           ghc-comonad
           ghc-tagged
           ghc-hashable
           ghc-unordered-containers))
    (home-page "http://github.com/ekmett/semigroupoids")
    (synopsis "Semigroupoids: Category sans id")
    (description "ghc-semigroupoids")
    ;; (description
;;      "This package provides a wide array of (semi)groupoids and operations for working
;; with them. .  A 'Semigroupoid' is a 'Category' without the requirement of
;; identity arrows for every object in the category. .  A 'Category' is any
;; 'Semigroupoid' for which the Yoneda lemma holds. .  When working with comonads
;; you often have the @\\<*\\>@ portion of an @Applicative@, but not the @pure@.
;; This was captured in Uustalu and Vene's \\\"Essence of Dataflow Programming\\\" in
;; the form of the @ComonadZip@ class in the days before @Applicative@.  Apply
;; provides a weaker invariant, but for the comonads used for data flow programming
;; (found in the streams package), this invariant is preserved.  Applicative
;; function composition forms a semigroupoid. .  Similarly many structures are
;; nearly a comonad, but not quite, for instance lists provide a reasonable
;; 'extend' operation in the form of 'tails', but do not always contain a value. .
;; We describe the relationships between the type classes defined in this package
;; and those from `base` (and some from `contravariant`) in the diagram below.
;; Thick-bordered nodes correspond to type classes defined in this package;
;; thin-bordered ones correspond to type classes from elsewhere.  Solid edges
;; indicate a subclass relationship that actually exists; dashed edges indicate a
;; subclass relationship that /should/ exist, but currently doesn't. .
;; <<https://raw.githubusercontent.com/ekmett/semigroupoids/master/img/classes.svg
;; Relationships among type classes from this package and others>> .  Apply, Bind,
;; and Extend (not shown) give rise the Static, Kleisli and Cokleisli semigroupoids
;; respectively. .  This lets us remove many of the restrictions from various monad
;; transformers as in many cases the binding operation or @\\<*\\>@ operation does
;; not require them. .  Finally, to work with these weaker structures it is
;; beneficial to have containers that can provide stronger guarantees about their
;; contents, so versions of 'Traversable' and 'Foldable' that can be folded with
;; just a 'Semigroup' are added.")
    (license license:bsd-2)))

(define-public ghc-semialign
  (package
    (name "ghc-semialign")
    (version "1.2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "semialign" version))
       (sha256
        (base32 "0ci1jpp37p1lzyjxc1bljd6zgg407qmkl9s36b50qjxf85q6j06r"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-these
           ghc-hashable
           ghc-indexed-traversable
           ghc-indexed-traversable-instances
           ghc-tagged
           ghc-unordered-containers
           ghc-vector
           ghc-semigroupoids))
    (arguments
     `(#:cabal-revision
       ("1" "00cmkdmgqlk8v2jg084nddlj2qkwj68nqk9p3l07kzwf796rn5qf")))
    (home-page "https://github.com/haskellari/these")
    (synopsis "Align and Zip type-classes from the common Semialign ancestor.")
    (description "ghc-semialign")
    ;; (description
;;      "The major use of @These@ of this is provided by the @align@ member of
;; @Semialign@ class, representing a generalized notion of \"zipping with padding\"
;; that combines structures without truncating to the size of the smaller input. .
;; It turns out that @zip@ operation fits well the @Semialign@ class, forming
;; lattice-like structure.")
    (license license:bsd-3)))

(define-public ghc-aeson
  (package
    (name "ghc-aeson")
    (version "2.0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "aeson" version))
        (sha256
          (base32 "09dk0j33n262dm75vff3y3i9fm6lh06dyqswwv7a6kvnhhmhlxhr"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs
      (list ghc-base-compat-batteries
            ghc-time-compat
            ghc-attoparsec
            ghc-data-fix
            ghc-dlist
            ghc-hashable
            ghc-indexed-traversable
            ghc-onetuple
            ghc-primitive
            ghc-quickcheck
            ghc-scientific
            ghc-semialign
            ghc-strict
            ghc-tagged
            ghc-text-short
            ghc-th-abstraction
            ghc-these
            ghc-unordered-containers
            ghc-uuid-types
            ghc-vector
            ghc-witherable))
    ;; (native-inputs
    ;;   (list ghc-base-compat
    ;;         ghc-base-orphans
    ;;         ghc-base16-bytestring
    ;;         ghc-diff
    ;;         ghc-generic-deriving
    ;;         ghc-integer-logarithms
    ;;         ghc-quickcheck-instances
    ;;         ghc-tasty
    ;;         ghc-tasty-golden
    ;;         ghc-tasty-hunit
    ;;         ghc-tasty-quickcheck
    ;;         ghc-indexed-traversable
    ;;         ghc-semialign
    ;;         ghc-primitive
    ;;         ghc-text-short))
    (home-page "https://github.com/haskell/aeson")
    (synopsis "Fast JSON parsing and encoding")
    (description "aeson")
    ;; (description
;;       "This package provides a JSON parsing and encoding library optimized for ease of
;; use and high performance. .  To get started, see the documentation for the
;; @Data.Aeson@ module below. . (A note on naming: in Greek mythology, Aeson was
;; the father of Jason.)")
    (license license:bsd-3)))

(define-public ghc-megaparsec
  (package
    (name "ghc-megaparsec")
    (version "9.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "megaparsec" version))
       (sha256
        (base32 "1whjn3n14h2q3ja1v7zllzmj28ai7lqwfbif22c08rl00wpwmwhd"))))
    (build-system haskell-build-system)
    (inputs (list ghc-case-insensitive ghc-parser-combinators ghc-scientific))
    (arguments
     `(#:cabal-revision
       ("1" "1jv3j59fvykvplj3c653c9rk5wiwg6x0jnljhi2vknzxxmxkjj02")))
    (home-page "https://github.com/mrkkrp/megaparsec")
    (synopsis "Monadic parser combinators")
    (description
     "This is an industrial-strength monadic parser combinator library.  Megaparsec is
a feature-rich package that tries to find a nice balance between speed,
flexibility, and quality of parse errors.")
    (license license:bsd-2)))

(define-public ghc-versions
  (package
    (name "ghc-versions")
    (version "5.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "versions" version))
        (sha256
          (base32 "1ca3m9rvx89mniipbkxz3nm49mz7s4nhqc11hpsa6hjw9ff5kcjv"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-megaparsec ghc-hashable ghc-parser-combinators))
    ;; (native-inputs
    ;;   (list ghc-microlens
    ;;         ghc-quickcheck
    ;;         ghc-tasty
    ;;         ghc-tasty-hunit
    ;;         ghc-tasty-quickcheck))
    (home-page "https://github.com/fosskers/versions")
    (synopsis "Types and parsers for software version numbers.")
    (description "ghc-versions")
;;     (description
;;       "This package provides a library for parsing and comparing software version
;; numbers.  We like to give version numbers to our software in a myriad of ways.
;; Some ways follow strict guidelines for incrementing and comparison.  Some follow
;; conventional wisdom and are generally self-consistent.  Some are just plain
;; asinine.  This library provides a means of parsing and comparing /any/ style of
;; versioning, be it a nice Semantic Version like this: . > 1.2.3-r1+git123 . ...or
;; a monstrosity like this: . > 2:10.2+0.0093r3+1-1 .  Please switch to
;; <http://semver.org Semantic Versioning> if you aren't currently using it.  It
;; provides consistency in version incrementing and has the best constraints on
;; comparisons. .  This library implements version @2.0.0@ of the SemVer spec.")
    (license license:bsd-3)))

(define-public ghc-neat-interpolation
  (package
    (name "ghc-neat-interpolation")
    (version "0.5.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "neat-interpolation" version))
        (sha256
          (base32 "01v5ccv16gw13mjxdxcxnbs6ggzmqh4vwzybzkzqzm40la163aqz"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-megaparsec))
    ;; (native-inputs
    ;;   (list ghc-quickcheck
    ;;         ghc-quickcheck-instances
    ;;         ghc-rerebase
    ;;         ghc-tasty
    ;;         ghc-tasty-hunit
    ;;         ghc-tasty-quickcheck))
    (home-page "https://github.com/nikita-volkov/neat-interpolation")
    (synopsis "A quasiquoter for neat and simple multiline text interpolation")
    (description
      "This package provides a quasiquoter for producing Text values with support for a
simple interpolation of input values.  It removes the excessive indentation from
the input and accurately manages the indentation of all lines of the
interpolated variables.")
    (license license:expat)))

(define-public ghc-srcloc
  (package
    (name "ghc-srcloc")
    (version "0.6.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "srcloc" version))
        (sha256
          (base32 "1ldn2dwxhyjll6nvsxylgvxfl7m6j4kk1qzv5xhklg1avfhz4khm"))))
    (build-system haskell-build-system)
    (home-page "https://github.com/mainland/srcloc")
    (synopsis "Data types for managing source code locations.")
    (description
      "Data types for tracking, combining, and printing source code locations.")
    (license license:bsd-3)))

(define-public ghc-mainland-pretty
  (package
    (name "ghc-mainland-pretty")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "mainland-pretty" version))
        (sha256
          (base32 "19z2769rik6kwvsil2if2bfq2v59jmwv74jy3fy4q3q3zy4239p1"))))
    (build-system haskell-build-system)
    (inputs (list ghc-srcloc))
    (home-page "https://github.com/mainland/mainland-pretty")
    (synopsis "Pretty printing designed for printing source code.")
    (description
      "Pretty printing designed for printing source code based on Wadler's paper /A
Prettier Printer/.  The main advantage of this library is its ability to
automatically track the source locations associated with pretty printed values
and output appropriate #line pragmas and its ability to produce output in the
form of lazy text using a builder.")
    (license license:bsd-3)))

(define-public ghc-exception-transformers
  (package
    (name "ghc-exception-transformers")
    (version "0.4.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "exception-transformers" version))
        (sha256
          (base32 "1zmd2s40m86c9mhv32l5bvvf5r52cgpxvb4v5phyc3pjwlr7m8g5"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-fail ghc-transformers-compat))
    ;; (native-inputs
    ;;   (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (home-page "http://hackage.haskell.org/package/exception-transformers")
    (synopsis "Type classes and monads for unchecked extensible exceptions.")
    (description
      "This package provides type classes, a monad and a monad transformer that support
unchecked extensible exceptions as well as asynchronous exceptions.  It is
compatible with the transformers package.")
    (license license:bsd-3)))

(define-public ghc-exception-mtl
  (package
    (name "ghc-exception-mtl")
    (version "0.4.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "exception-mtl" version))
        (sha256
          (base32 "0d51rsrcjy52d62f51hb6fdg0fj9b0qbv8hqf6523pndwsxbq4zc"))))
    (build-system haskell-build-system)
    (inputs (list ghc-exception-transformers))
    (home-page "http://hackage.haskell.org/package/exception-mtl")
    (synopsis "Exception monad transformer instances for mtl classes.")
    (description
      "This package provides exception monad transformer instances for the classes
defined by mtl.")
    (license license:bsd-3)))

(define-public ghc-th-lift-instances
  (package
    (name "ghc-th-lift-instances")
    (version "0.1.19")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-lift-instances" version))
       (sha256
        (base32 "0rk0q609q8pha4wqxxhrr221nc9lc9wanif3qm1g8lav51500pd8"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-vector ghc-th-lift))
    ;; (native-inputs (list ghc-quickcheck))
    (home-page "http://github.com/bennofs/th-lift-instances/")
    (synopsis "Lift instances for template-haskell for common data types.")
    (description "ghc-th-lift-instances")
;;     (description
;;      "Most data types in the haskell platform do not have Lift instances.  This
;; package provides orphan instances for containers, text, bytestring and vector.
;; It also provides compat instances for older versions of @template-haskell@ .
;; Note that <https://hackage.haskell.org/package/th-lift th-lift> package provides
;; Template Haskell based derivation of @Lift@ instances (when you cannot use
;; @DeriveLift@ extension), and <https://hackage.haskell.org/package/th-orphans
;; th-orphans> package provides instances for TH datatypes.")
    (license license:bsd-3)))

(define-public ghc-th-orphans
  (package
    (name "ghc-th-orphans")
    (version "0.13.12")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "th-orphans" version))
       (sha256
        (base32 "03n6qxnpxhbzyzbyrjq77d1y62dwgx39mmxfwmnc04l8pawgrxxz"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-th-compat
           ghc-th-expand-syns
           ghc-th-lift
           ghc-th-reify-many
           ghc-th-lift-instances))
    ;; (native-inputs (list ghc-hspec))
    (arguments
     `(#:cabal-revision
       ("2" "15660jac7m705jp6nm5lia51ifchbjrr6x3kj5sfij9dbj1f5dkj")
       #:tests? #f))
    (home-page "http://hackage.haskell.org/package/th-orphans")
    (synopsis "Orphan instances for TH datatypes")
    (description
     "Orphan instances for TH datatypes.  In particular, instances for Ord and Lift,
as well as a few missing Show / Eq.  These instances used to live in
haskell-src-meta, and that's where the version number started.")
    (license license:bsd-3)))

(define-public ghc-haskell-src-meta
  (package
    (name "ghc-haskell-src-meta")
    (version "0.8.9")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "haskell-src-meta" version))
       (sha256
        (base32 "0y3hnqg33pqzc66nl58va8lvwvmb0mx5q0p9r55i6cc3xwjahiyx"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-haskell-src-exts ghc-syb ghc-th-orphans))
    ;; (native-inputs (list ghc-hunit ghc-tasty ghc-tasty-hunit))
    (home-page "http://hackage.haskell.org/package/haskell-src-meta")
    (synopsis "Parse source to template-haskell abstract syntax.")
    (description
     "The translation from haskell-src-exts abstract syntax to template-haskell
abstract syntax isn't 100% complete yet.")
    (license license:bsd-3)))

(define-public ghc-language-c-quote
  (package
    (name "ghc-language-c-quote")
    (version "0.13")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "language-c-quote" version))
        (sha256
          (base32 "02axz6498sg2rf24qds39n9gysc4lm3v354h2qyhrhadlfq8sf6d"))))
    (build-system haskell-build-system)
    (inputs
      (list ghc-exception-mtl
            ghc-exception-transformers
            ghc-mainland-pretty
            ghc-srcloc
            ghc-syb
            ghc-haskell-src-meta))
    (native-inputs (list ghc-happy ghc-alex))
    ;; (native-inputs
    ;;   (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (arguments
      `(#:cabal-revision
        ("1" "1vl92h4z294ycg87140qk7v40r7vz43n8anpd2w1jdnwd6w4f4m3")
        #:tests? #f))
    (home-page "https://github.com/mainland/language-c-quote")
    (synopsis "C/CUDA/OpenCL/Objective-C quasiquoting library.")
    (description
      "This package provides a general parser for the C language, including most GCC
extensions and some CUDA and OpenCL extensions as well as the entire Objective-C
language.")
    (license license:bsd-3)))

(define-public ghc-githash
  (package
    (name "ghc-githash")
    (version "0.1.6.2")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "githash" version))
        (sha256
          (base32 "1vkwc7j71vdrxy01vlm6xfp16kam7m9bnj9y3h217fzhq5mjywhz"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-th-compat))
    ;; (native-inputs (list ghc-hspec ghc-temporary ghc-unliftio))
    (home-page "https://github.com/snoyberg/githash#readme")
    (synopsis "Compile git revision info into Haskell projects")
    (description
      "Please see the README and documentation at
<https://www.stackage.org/package/githash>")
    (license license:bsd-3)))

(define-public ghc-futhark-manifest
  (package
    (name "ghc-futhark-manifest")
    (version "1.0.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "futhark-manifest" version))
        (sha256
          (base32 "0zm6c907a9ywl9isslmqrl7k42076azzrrx16z0dj8w1pns69nw5"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-aeson))
    ;; (native-inputs
    ;;   (list ghc-quickcheck
    ;;         ghc-tasty
    ;;         ghc-tasty-hunit
    ;;         ghc-tasty-quickcheck
    ;;         ghc-quickcheck-instances))
    (home-page "http://hackage.haskell.org/package/futhark-manifest")
    (synopsis "Definition and serialisation instances for Futhark manifests.")
    (description
      "The Futhark compiler generates JSON manifest files that describe the C API of a
compiled program.  This package provides definitions for reading and writing
such files.")
    (license license:isc)))

(define-public ghc-vector-binary-instances
  (package
    (name "ghc-vector-binary-instances")
    (version "0.2.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "vector-binary-instances" version))
       (sha256
        (base32 "0kgmlb4rf89b18d348cf2k06xfhdpamhmvq7iz5pab5014hknbmp"))))
    (build-system haskell-build-system)
    (inputs (list ghc-vector))
    ;; (native-inputs (list ghc-tasty ghc-tasty-quickcheck))
    (arguments
     `(#:cabal-revision
       ("1" "0rbjskq11wlfa97h8bidzk145lkqrv00kx1rgwgdcfbgz1l73iha")
       #:tests? #f))
    (home-page "https://github.com/bos/vector-binary-instances")
    (synopsis "Instances of Data.Binary for vector")
    (description
     "Instances for Binary for the types defined in the vector package, making it easy
to serialize vectors to and from disk.  We use the generic interface to vectors,
so all vector types are supported.  Specific instances are provided for unboxed,
boxed and storable vectors. .  To serialize a vector: . > *Data.Vector.Binary>
let v = Data.Vector.fromList [1..10] > *Data.Vector.Binary> v > fromList
[1,2,3,4,5,6,7,8,9,10] :: Data.Vector.Vector > *Data.Vector.Binary> encode v >
Chunk \"\\NUL\\NUL\\NUL\\NUL\\NUL...\\NUL\\NUL\\NUL\\t\\NUL\\NUL\\NUL\\NUL\
\" Empty .  Which
you can in turn compress before writing to disk: . > compress .  encode $ v >
Chunk \"\\US\\139\\b\\NUL\\NUL\\N...\\229\\240,\\254:\\NUL\\NUL\\NUL\" Empty .  Try the
cereal-vector package if you are looking for Data.Serialize instances.")
    (license license:bsd-3)))

(define-public ghc-bytestring-to-vector
  (package
    (name "ghc-bytestring-to-vector")
    (version "0.3.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "bytestring-to-vector" version))
        (sha256
          (base32 "0ji836sl72wlhy6yay11kl86w0nrcdc1lafbi94bx9c8rpf5pyyc"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-vector))
    ;; (native-inputs (list ghc-byteorder ghc-quickcheck))
    (home-page "https://github.com/sheyll/bytestring-to-vector")
    (synopsis "Convert between ByteString and Vector.Storable without copying")
    (description "ghc-bytestring-to-vector")
;;     (description
;;       "This library allows conversion between the types from @Data.ByteString@ (package
;; @bytestring@) and @Data.Vector.Storable@ (package @vector@) without copying the
;; underlying data.  This is useful, for example, when @ByteString@ IO produces or
;; consumes vectors of numbers in native byte order. .  The conversion relies on
;; the fact that @ByteString@ and @Vector@ use their respective @ForeignPtr@s in
;; compatible ways. .  This library is a fork of the @spool@ package written by
;; Keegan McAllister.")
    (license license:bsd-3)))

(define-public ghc-futhark-data
  (package
    (name "ghc-futhark-data")
    (version "1.0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "futhark-data" version))
        (sha256
          (base32 "1mskhngmxfd5mimr78wni6ih6mng9rg3mfyx9bfvi05vz9hdx54a"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs
      (list ghc-bytestring-to-vector
            ghc-half
            ghc-megaparsec
            ghc-scientific
            ghc-vector
            ghc-vector-binary-instances))
    ;; (native-inputs
    ;;   (list ghc-quickcheck ghc-tasty ghc-tasty-hunit ghc-tasty-quickcheck))
    (home-page "http://hackage.haskell.org/package/futhark-data")
    (synopsis "An implementation of the Futhark data format.")
    (description
      "The Futhark compiler and its tools uses a simple external data representation to
encode arrays and scalars.  This package implements both a Haskell-level
representation of these values, as well as utility functions for reading and
writing values in both the textual and binary format.")
    (license license:isc)))

(define-public ghc-futhark-server
  (package
    (name "ghc-futhark-server")
    (version "1.1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "futhark-server" version))
        (sha256
          (base32 "0hbglw570j09r8f7cxcyn7dr99iykw94p6fglncw8l210jvcffbm"))))
    (build-system haskell-build-system)
    (inputs (list ghc-futhark-data ghc-temporary))
    (home-page "http://hackage.haskell.org/package/futhark-server")
    (synopsis "Client implementation of the Futhark server protocol.")
    (description
      "This package provides an easy way to interact with a running Futhark server-mode
program from a Haskell program.  Provides both direct support of the protocol,
as well as convenience functions for loading and extracting data.")
    (license license:isc)))

(define-public ghc-directory-tree
  (package
    (name "ghc-directory-tree")
    (version "0.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "directory-tree" version))
        (sha256
          (base32 "05z5ws58kky3wcwwwv6q16m9vs0lzj35qqs9v5acy9m2nfal8272"))))
    (build-system haskell-build-system)
    (home-page "http://brandon.si/code/directory-tree-module-released/")
    (synopsis
     "A simple directory-like tree datatype, with useful IO functions")
    (description "ghc-directory-tree")
;;     (description
;;       "This package provides a simple directory-like tree datatype, with useful IO
;; functions and Foldable and Traversable instance .  Provides a simple data
;; structure mirroring a directory tree on the filesystem, as well as useful
;; functions for reading and writing file and directory structures in the IO monad.
;; .  Importing the library and optional (useful) Foldable and Traverable
;; libraries: . > import System.Directory.Tree > import qualified Data.Foldable as
;; F > import qualified Data.Traversable as T .  Write a hand-made directory tree
;; of textfiles (strings) to the disk.  Simulates creating a new user Tux's home
;; directory on a unix machine: . > writeDirectory$ \"/home\" :/ Dir \"Tux\" [File
;; \"README\" \"Welcome!\"] . \"read\" a directory by opening all the files at a filepath
;; with readFile, returning an 'AnchoredDirTree String' (d2).  Then check for any
;; IO failures: . > do (base :/ d2) <- readDirectory \"../parent_dir/dir2/\" >    let
;; failed = anyFailed d2 >    if failed then ... .  Use Foldable instance function
;; to concat a directory 'dir' of text files into a single file under the same
;; directory: . > do (b :/ dt) <- readDirectory dir >    let f = F.concat dt >
;; return$ b :/ File \"ALL_TEXT\" f .  Open all the files in the current directory as
;; lazy bytestrings, ignoring the base path in Anchored wrapper: . > import
;; qualified Data.ByteString.Lazy as B > do (_ :/ dTree) <- readDirectoryWith
;; B.readFile \"./\" .  This version also offers an experimental function
;; `readDirectoryWithL` that does lazy directory IO, allowing you to treat the
;; returned `DirTree` as if it were a normal lazily-generated data structure. .
;; For example, the following does only the amount of IO necessary to list the file
;; names of the children of the root directory, similar to \"ls /\": . > do d <-
;; readDirectoryWithL readFile \"/\" >    mapM_ (putStrLn .  name) $ contents $ free
;; d .  Any ideas or suggestions for improvements are most welcome :-) . /CHANGES/:
;; from 0.11 . - export 'System.Directory.Tree.transformDir' as requested . - add
;; test suite to cabal file . - remove redundant @removeNonexistent@ (thanks to
;; dmwit for patch) .")
    (license license:bsd-3)))

(define-public ghc-bmp
  (package
    (name "ghc-bmp")
    (version "1.2.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "bmp" version))
        (sha256
          (base32 "1k6s5z520dadj38y7ja0m4syrg094gyq14c63i6wx2701zj3viiw"))))
    (build-system haskell-build-system)
    (arguments
      `(#:cabal-revision
        ("1" "0ghc3klxfz5v8rb3rwblrnxmxfafplxrd9gc0y975g8k1q71p44k")))
    (home-page "https://github.com/benl23x5/bmp")
    (synopsis "Read and write uncompressed BMP image files.")
    (description
      "Read and write uncompressed BMP image files.  100% robust Haskell
implementation.")
    (license license:expat)))

(define-public ghc-transformers-base
  (package
    (name "ghc-transformers-base")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "transformers-base" version))
       (sha256
        (base32 "146g69yxmlrmvqnzwcw4frxfl3z04lda9zqwcqib34dnkrlghfrj"))))
    (build-system haskell-build-system)
    (inputs (list ghc-transformers-compat ghc-base-orphans))
    (home-page "https://github.com/mvv/transformers-base")
    (synopsis "Lift computations from the bottom of a transformer stack")
    (description "ghc-transformers-base")
;;     (description
;;      "This package provides a straightforward port of @monadLib@'s BaseM typeclass to
;; @transformers@.")
    (license license:bsd-3)))

(define-public ghc-profunctors
  (package
    (name "ghc-profunctors")
    (version "5.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "profunctors" version))
       (sha256
        (base32 "0an9v003ivxmjid0s51qznbjhd5fsa1dkcfsrhxllnjja1xmv5b5"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-base-orphans
           ghc-bifunctors
           ghc-comonad
           ghc-contravariant
           ghc-distributive
           ghc-tagged))
    (arguments
     `(#:cabal-revision
       ("2" "1dhg8bys9qnfbvhy4cm4fivanmnik4rg0spshkwyp9s3j88qadix")))
    (home-page "http://github.com/ekmett/profunctors/")
    (synopsis "Profunctors")
    (description "Profunctors.")
    (license license:bsd-3)))

(define-public ghc-free
  (package
    (name "ghc-free")
    (version "5.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "free" version))
       (sha256
        (base32 "121b81wxjk30nc27ivwzxjxi1dcwc30y0gy8l6wac3dxwvkx2c5j"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-comonad
           ghc-distributive
           ghc-indexed-traversable
           ghc-semigroupoids
           ghc-th-abstraction
           ghc-transformers-base
           ghc-profunctors))
    (arguments
     `(#:cabal-revision
       ("1" "0x3d7jp17m65f25838ic26kvnpjfb99iw3d60ga57n8505shxywb")))
    (home-page "http://github.com/ekmett/free/")
    (synopsis "Monads for free")
    (description "ghc-free")
;;     (description
;;      "Free monads are useful for many tree-like structures and domain specific
;; languages. .  If @f@ is a 'Functor' then the free 'Monad' on @f@ is the type of
;; trees whose nodes are labeled with the constructors of @f@.  The word \\\"free\\\"
;; is used in the sense of \\\"unrestricted\\\" rather than \\\"zero-cost\\\": @Free f@
;; makes no constraining assumptions beyond those given by @f@ and the definition
;; of 'Monad'.  As used here it is a standard term from the mathematical theory of
;; adjoint functors. .  Cofree comonads are dual to free monads.  They provide
;; convenient ways to talk about branching streams and rose-trees, and can be used
;; to annotate syntax trees.  The cofree comonad can be seen as a stream
;; parameterized by a 'Functor' that controls its branching factor. .  More
;; information on free monads, including examples, can be found in the following
;; blog posts: <http://comonad.com/reader/2008/monads-for-free/>
;; <http://comonad.com/reader/2011/free-monads-for-less/>")
    (license license:bsd-3)))

(define-public ghc-listlike
  (package
    (name "ghc-listlike")
    (version "4.7.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://hackage.haskell.org/package/ListLike/ListLike-"
         version ".tar.gz"))
       (sha256
        (base32
         "08jip0q2f9qc95wcqka2lrqpf8r7sswsi5104w73kyrbmfirqnrd"))))
    (build-system haskell-build-system)
    (inputs
     (list ghc-vector
           ghc-dlist
           ghc-fmlist
           ghc-hunit
           ghc-quickcheck
           ghc-random
           ghc-utf8-string))
    (home-page "https://github.com/JohnLato/listlike")
    (synopsis "Generic support for list-like structures")
    (description "The ListLike module provides a common interface to the
various Haskell types that are list-like.  Predefined interfaces include
standard Haskell lists, Arrays, ByteStrings, and lazy ByteStrings.
Custom types can easily be made ListLike instances as well.

ListLike also provides for String-like types, such as String and
ByteString, for types that support input and output, and for types that
can handle infinite lists.")
    (license license:bsd-3)))

(define-public ghc-process-extras
  (package
    (name "ghc-process-extras")
    (version "0.7.4")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "process-extras" version))
       (sha256
        (base32 "0klqgr37f1z2z6i0a9b0giapmq0p35l5k9kz1p7f0k1597w7agi9"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-data-default ghc-listlike ghc-generic-deriving))
    ;; (native-inputs (list ghc-hunit))
    (home-page "https://github.com/seereason/process-extras")
    (synopsis "Process extras")
    (description "ghc-process-extras")
;;     (description
;;      "Extends <http://hackage.haskell.org/package/process>.  Read process input and
;; output as ByteStrings or Text, or write your own ProcessOutput instance.  Lazy
;; process input and output.  ProcessMaker class for more flexibility in the
;; process creation API.")
    (license license:expat)))

(define-public ghc-temporary
  (package
    (name "ghc-temporary")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "temporary" version))
       (sha256
        (base32 "144qhwfwg37l3k313raf4ssiz16jbgwlm1nf4flgqpsbd69jji4c"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs (list ghc-random))
    ;; (native-inputs (list ghc-tasty ghc-tasty-hunit ghc-base-compat))
    (home-page "https://github.com/feuerbach/temporary")
    (synopsis "Portable temporary file and directory support")
    (description
     "This package provides functions for creating temporary files and directories.")
    (license license:bsd-3)))

(define-public ghc-futhark
  (package
    (name "ghc-futhark")
    (version "0.21.8")
    (source
      (origin
        (method url-fetch)
        (uri (hackage-uri "futhark" version))
        (sha256
          (base32 "16qvrmqrfsnj11f59xkb3cgibd1wq80bcssjfzbicwmrlxi86im6"))))
    (build-system haskell-build-system)
    (arguments `(#:tests? #f))
    (inputs
      (list ghc-aeson
            ghc-ansi-terminal
            ghc-base16-bytestring
            ghc-blaze-html
            ghc-bytestring-to-vector
            ghc-bmp
            ghc-cryptohash-md5
            ghc-directory-tree
            ghc-dlist
            ghc-file-embed
            ghc-free
            ghc-futhark-data
            ghc-futhark-server
            ghc-futhark-manifest
            ghc-githash
            ghc-half
            ghc-language-c-quote
            ghc-mainland-pretty
            ghc-cmark-gfm
            ghc-megaparsec
            ghc-neat-interpolation
            ghc-parallel
            ghc-random
            ghc-process-extras
            ghc-regex-tdfa
            ghc-srcloc
            ghc-temporary
            ghc-terminal-size
            ghc-vector
            ghc-versions
            ghc-zip-archive
            ghc-zlib))
    (native-inputs (list ghc-alex ghc-happy))
    ;; (native-inputs
    ;;   (list ghc-quickcheck
    ;;         ghc-parser-combinators
    ;;         ghc-tasty
    ;;         ghc-tasty-hunit
    ;;         ghc-tasty-quickcheck))
    (home-page "https://futhark-lang.org")
    (synopsis
     "An optimising compiler for a functional, array-oriented language.")
    (description "futhark")
;;     (description
;;       "Futhark is a small programming language designed to be compiled to efficient
;; parallel code.  It is a statically typed, data-parallel, and purely functional
;; array language in the ML family, and comes with a heavily optimising
;; ahead-of-time compiler that presently generates GPU code via CUDA and OpenCL,
;; although the language itself is hardware-agnostic. .  For more information, see
;; the website at https://futhark-lang.org .  For introductionary information about
;; hacking on the Futhark compiler, see
;; <https://futhark.readthedocs.io/en/latest/hacking.html the hacking guide>.
;; Regarding the internal design of the compiler, the following modules make good
;; starting points: . * \"Futhark\" contains a basic architectural overview of the
;; compiler. * \"Futhark.IR.Syntax\" explains the basic design of the intermediate
;; representation (IR). * \"Futhark.Construct\" explains how to write code that
;; manipulates and creates AST fragments. . <<docs/assets/ohyes.png You too can go
;; fast once you rewrite your program in Futhark.>>")
    (license license:isc)))

ghc-futhark
