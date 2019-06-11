---
title:  An overview of property testing
author: Tom Sydney Kerckhove
patat:
  margins:
    left: 1
    right: 1
---

# An overview of property testing

- QuickCheck
- Hedgehog
- Validity


# What do you need for property testing

- Properties
- Generators
- Shrinking

# Quickcheck

``` haskell
class Arbitrary a where
```

. . .

``` haskell
  arbitrary :: Gen a -- A generator for A
```

. . .

``` haskell
  shrink :: a -> [a] -- A shrinking function far A
```

. . .

``` haskell
λ> quickCheck $ \(str :: String) -> reverse (reverse str) == str
+++ OK, passed 100 tests.
```

# Property combinators for non-specialised randomised property testing

Test whether `<|*|>` is reflexive:

``` haskell
\x -> x <|*|> x
```

. . .

Property combinator

``` haskell
reflexive :: Arbitrary a => (a -> a -> Bool) -> Property
reflexive rel = \x -> x `rel` x
```

. . .

Only for non-specialised property testing

``` haskell
> 2 :% 2 == (1 :% 1 :: Rational)
False
```

# No laws or semantics for `Arbitrary`

`Gen Rational`

- `0 :% 0` ?

. . .

- `1 :% 0` ?

. . .

- `5 :% -1` ?

. . .

- `-5 :% 1` ?

. . .

- Never generate un-normalised values? !> never test these edge-cases

. . .

- Generate un-normalised values?       !> `(==)` breaks

# Newtypes for guiding generators

``` Haskell
instance Arbitrary Rational
```

. . .


``` Haskell
newtype Normalised a = Normalised a
instance Arbitrary (Normalised Rational)
```

. . .


``` Haskell
reflexive (\(Normalised nn1) (Normalised nn2) -> nn1 == nn2)
```

. . .

!> Cannot compose these

# Expensive generators

```
There is no generic arbitrary implementation included because
we don’t know how to make a high-quality one. If you want one,
consider using the testing-feat or generic-random packages.
```

!> No default implementation of `arbitrary`
!> Default shrinking: `shrink _ = []`

# Orphan instances

Where do you put `Arbitrary` instances?

- Next to data     !> depend on `QuickCheck`

. . .

- In test suite    !> Rewrite for every test suite

. . .

- `quickcheck-instances` !> Extra dependencies

. . .

- Seperate library !> Maintenance overhaid

# Hedgehog

``` haskell
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_reverse :: Property
prop_reverse =
  property $ do
    xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
    reverse (reverse xs) === xs
```

# Hedgehog generators

- Value-level generators, no type class

. . .

- Built-in (monadic!) shrinking via lazy shrink trees

. . .

- More general -> monadic effects

# Sub-optimal shrinking

Hedgehog:

``` haskell
λ > let gen = Gen.enum 'a' 'b'
λ > Gen.printTree (Gen.filter (uncurry (==)) ((,) <$> gen <*> gen))
=== Outcome ===
('b','b')
=== Shrinks ===
('a','a')
('a','a')
('a','a')
```

. . .

Quickcheck:

``` haskell
λ filter (uncurry (==)) $ QC.shrink ('b','b')
[]
```

Validity:

``` haskell
λ filter (uncurry (==)) $ shrinkUnchecked ('b', 'b')
[('a','a')]
```

# Even more expensive generators

!> No type class at all

# Cumbersome property combinators

``` haskell
associative :: Gen a -> (a -> a -> a) -> Property
```

!> Need to string generators around

# Validity-based testing

