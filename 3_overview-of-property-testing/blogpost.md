---
title: QuickCheck, Hedgehog, Validity
tags: property testing, testing, validity, quickcheck, hedgehog
category: Programming
authors:
  - Tom Sydney Kerckhove
  - Alexey Kuleshevich
  - Niklas Hambüchen
  - Michael Snoyman
---


I have been working on property testing for years now.
My current conclusion is that property testing  still has so much uncovered potential that with just some more effort, it can become even greater tool for software development than it already is today.
The concept is relatively young and the design space relatively unexplored.
This post seeks to provide an overview of the different approaches and to outline a comparison between the most commonly used libraries for property-based testing in Haskell.
These include two popular libraries: QuickCheck and HedgeHog, and a new approach called "Validity-based Testing".

## The problems with testing

There are many ways to try to make sure that code works.
They each have their own trade-offs and we should consider these trade-offs carefully.
The first step is to consider how expensive breakage would be.
The next step is to use the right tools.
Use the right programming language, build system, etc, so that code is safe in certain ways automatically.
Is it worth it to formally verify all of your code? Maybe, probably not.
When you have gone through these steps, you turn to testing, and most likely: unit testing.

By "unit test", here, I mean a test that checks whether the output of a function, for a single example, is equal to the expected output.
An example `hspec` snippet in Haskell:

``` haskell
myFunction exampleInput `shouldBe` expectedOutput
```

There are two standard problems with unit tests:

* One has to write many examples to cover all possible code paths.
  Even when we do write tests that cover all code paths, we may not notice that we have forgotten to write code to deal with edge cases.
  We call this problem the coverage problem.

* Writing these tests is both cumbersome, and usually not considered enjoyable.
  Unit testing is expensive from a developer-time perspective.
  We call this problem the developer cost problem.


## Property testing as a solution to the coverage problem

Property testing is a different approach to testing.
The idea is that you can make a test parametric in a certain argument, and the test should still pass for every argument that you pass in.
The next question is then: Where do you get these arguments?
That depends on the kind of property test:

- Exhaustive property test: Run the test for each element of the argument type.
- Randomised property test: Run the test for N random elements drawn from the entire argument type.
- Exhaustive specialised property test: Run the test for each element of a subset of the argument type.
- Randomised specialised property test: Run the test for N random elements drawn from a subset of the argument type.

An exhaustive property test would be the most comprehensive, but for argument types inhabited by a very large or infinite number of values, like `Int64` or `String`, this is infeasible.
To make the runtime of a property test reasonable, we usually opt for a randomised property test.
Randomised property testing probabilistically solves the coverage problem of tests, but it exacerbates the developer cost problem.
It is generally harder to come up with a general property of your code than an input-output example.
You could argue that one would never write as many unit tests as property tests will generate for them, but the point here is about the strict amount of thinking and typing that the programmer has to do to get started.
It is a trade-off.

In this post, we will use the following running example of a property test:

``` haskell
(\str -> reverse (reverse str) == str) :: String -> Bool
```

This property states that if you reverse a string twice, then you get the original string.
(For the purposes of this posts, we assume that data structures are finite.
This is important because testing frameworks require that data is `Show`-able in order to print counter examples.)



### Generators

Randomised property testing requires a way to generate the random values.
These generators are responsible for generating the arguments to the property tests.
An appropriate generator can often be the difference between finding a bug, and not finding a bug.

For example, when only generating small lists, the following property test may seem to hold:
``` haskell
(\ls -> length ls < 100)
```

It is important to note that not all types allow easy sampling of values.
Generating a value of type `Word8` is relatively simple: We can just choose a value between `0x00` and `0xff` uniformly at random.
However, generating a value from an unbounded type like `String` is tricky.
Indeed, there is no way to sample a value uniformly from the type `String` because there are infinitely many Strings, and there isn't one obvious way to sample from an infinite set.
To solve this problem, generators often use some way to guide how large of a value to generate.
This is usually called the size parameter.
This size parameter then allows the generator for `String`s to generate a `String` up to the given size.

Generators can usually also be adapted to the exact test at hand.
For example, combinators can usually be composed to build bigger combinators, or filtered to only return the generated values that satisfy a given predicate.


### Shrinking

When a property test fails, it is likely that the value that made the test fail is large.
This makes it hard to reason about why the test failed.
A standard solution to this problem is to try and shrink the value that makes the test fail.
The test is re-run with smaller values to make sure that the test still fails with the smaller value.
After shrinking, the resulting value should be of a more manageable size.

#### An example of shrinking

Suppose we believe that the following property holds:

After lower-casing every character in a string, all the characters in this string will be lower-case.

``` haskell
\str -> all isLower (map toLower str)
```

When we run the property test without shrinking, we may get a counterexample like the following:

```
"As5Enu3auO4"
```

It is certainly not immediately obvious to me why this is a counterexample for the above property.
However, when we rerun the property test _with_ shrinking, the counterexample is shrunk to the following:

``` haskell
"1"
```

Now it becomes rather obvious why the test failed, if we start debugging as follows:

```
λ toLower '1'
'1'
λ isLower '1'
False
```

Indeed, applying `toLower` to the character `'1'`  returns `'1'`, but it is also not considered lower-case by `isLower`.
In this case, the property that we thought would hold just does not hold. We could try to specialise it so that we get a property that does hold, but that is beyond the scope of this example.


Note that the shrunk counterexample: `"1"` is not necessarily part of the original counter example.
This brings me to my next point:

#### Shrinking with invariants

It is important that the shrunk value does not just fail the test, it must also have been generatable by the generator that was used.
For example, consider the following property test that you may mistakenly write:

For every number larger than six: the number is larger than five and odd. (pseudocode)

```
∀ n. (n > 6) => (n > 5 && odd n)
```

This property is designed to be tested with values larger than six.
We can constrain the set to draw random values from to the set of integers larger than six, using randomised specialised testing.
However, if the shrinking method is not adapted as well, to preserve this constraint on input set, then something unexpected might occur.
At some point, the generator might generate `8` as its random integer larger than `6`, and fail the property test.
Before this value is reported, it is shrunk to zero because this is the smallest integer, and shrinking might as well try this first. (Note that shrinking is for humans, which is why integers are shrunk to zero, (debatably) the simplest integer, instead of `minBound :: Int`, the smallest integer.)
Zero still fails the property test, because zero is not greater than five, so we report that this property test fails because zero fails to make the property evaluate to `True`.
This will make the output look something like the following:

```
failed: the following number larger than six does not pass the property test: 0
```

It is therefore important that the shrinking method preserves the intended invariants of the subset that one wants to test.
A value should only ever be shrunk to a smaller value that is still within this subset.



## Quickcheck approach and problems

The QuickCheck library was the pioneer of randomised property testing in Haskell.
QuickCheck makes use of a type class called `Arbitrary`.

[The `Arbitrary` type class](https://www.stackage.org/haddock/lts-12.11/QuickCheck-2.11.3/Test-QuickCheck-Arbitrary.html#t:Arbitrary) contains two functions:

- [`arbitrary :: Arbitrary a => Gen a`](https://www.stackage.org/haddock/lts-12.11/QuickCheck-2.11.3/Test-QuickCheck-Arbitrary.html#v:arbitrary) to generate random values.
  `arbitrary` is the default generator to use to generate values of a given type.
- [`shrink :: Arbitrary a => a -> [a]`](https://www.stackage.org/haddock/lts-12.11/QuickCheck-2.11.3/Test-QuickCheck-Arbitrary.html#v:shrink) to shrink a given value.

You can run a quickcheck by passing something [`Testable`](https://www.stackage.org/haddock/lts-12.11/QuickCheck-2.11.3/Test-QuickCheck-Property.html#t:Testable) to the [`quickCheck`](https://www.stackage.org/haddock/lts-12.11/QuickCheck-2.11.3/Test-QuickCheck-Test.html#v:quickCheck) function.
Something `Testable` is usually of the form `FooBar -> Bool` where `FooBar` is a member of the `Arbitrary` type class.
Quickcheck then generates random values of type `FooBar` and evaluates the property with it to check that the resulting boolean is `True`.

The example property of the `reverse` function is written as follows, using QuickCheck:

```
λ> quickCheck $ \(str :: String) -> reverse (reverse str) == str
+++ OK, passed 100 tests.
```

The QuickCheck approach has certain trade-offs, some of which we'll discover and analyse below.

### Property combinators for non-specialised randomised property testing

To test whether a given binary relation `<|*|>` is reflexive, we can write a property test as follows:

``` haskell
\x -> x <|*|> x
```

If we want to test reflexivity of many binary relations, we may write a function that takes a binary relation and produces a property.
Such a function is called a property combinator:

``` haskell
reflexive :: Arbitrary a => (a -> a -> Bool) -> Property
reflexive rel = \x -> x `rel` x
```

Now we can use this property combinator to test reflexivity of any binary relation for which the arguments are a member of `Arbitrary`.

Note that there is an important limitation to this approach.
This property combinator can only test reflexivity of a given binary relation if the relation is reflexive for ALL possible values that may be generated by the `arbitrary` generator.

This is already a problem with `Rational` (from `GHC.Real`) and `Eq`, for example.
[The `Eq` instance for `Ratio a`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Real.html#line-82) asssumes that values are normalised:

```
> 2 :% 2 == 1 :% 1
False
```

If `arbitrary :: Gen Rational` ever generates un-normalised values, then we can no longer test properties which require that `(==)` works as if the values are actual ratios, even if the property holds for all values of `Rational` except un-normalised values.
This brings me to the next point


### No laws or semantics for the `Arbitrary` type class

To continue with the previous example of `Rational`.
How should we implement the `Arbitrary` instance for `Rational`:

- Should we ever let it generate `0 :% 0`?
- Should we ever let it generate `1 :% 0`?
- Should we ever let it generate `5 :% -1`? How about `-5 :% 1`?
- Should we take the size parameter into account?

There are some considerations here:

- If `arbitrary :: Gen Rational` never generates un-normalised values, then property tests about functions that deal with `(==)` assuming that values are actual ratios will never test what happens for this edge-case. Maybe they work, maybe they do not.
- If `arbitrary :: Gen Rational` generates un-normalised values, then we cannot test properties which require that `(==)` works as if the values are actual ratios, even if the property holds for all values of `Rational` except
un-normalised values.

There is a solution for problems like this in `QuickCheck`: Using newtypes to guide generation.
For example, the `Positive a` newtype wrapper has a specialised generator to make sure that `arbitrary :: Positive Rational` only generates positive `Rational` values.
One could imagine a newtype wrapper like `Normalised a` that could have a specialised generator to make sure that `Normalised Rational` never generates un-normalised values, if `arbitrary :: Gen Rational` is implemented to sometimes generate un-normalised values.

However, this newtype approach has an important limitation as well.
The property test to test for reflexivity would now look something like this:

```
reflexive (\(Normalised nn1) (Normalised nn2) -> nn1 == nn2)
```

(Note that you cannot just supply a custom generator if you want to use property combinators that assume an `Arbitrary` constraint.)
This is rather cumbersome.
It is a direct result of the fact that values like `5 :% -5` are not considered valid values.
Who decided that these values are invalid even though they are certainly members of their type?
It is the programmer who wrote the `GHC.Real` module and decided that `Ratio` values should satisfy the invariant that they are normalised.
These invariants are not statically checked.
QuickCheck's `Arbitrary` typeclass makes it cumbersome to work with values for which not all values are considered valid values.

### Expensive generators and shrinking

The authors of QuickCheck chose to not have a default implementation for `Arbitrary`.
This is because there are certain problems with the automatic generation of `Arbitrary` instances.

[The QuickCheck documentation mentions](https://www.stackage.org/haddock/lts-12.12/QuickCheck-2.11.3/Test-QuickCheck-Arbitrary.html#v:arbitrary) that:

> There is no generic arbitrary implementation included because we don't know how to make a high-quality one. If you want one, consider using the testing-feat or generic-random packages.

There are multiple seperate libraries to allow programmers to have GHC implement `Arbitrary` instances automatically using `Generic` instances.
However, using one of these is not common practice.

As a result, there is no default implemetation of `arbitrary` and the default implementation for `shrink` is `const []`.
This means that by default, `shrink` is never overridden with a more sensible implementation and counter examples are never shrunk.

Because programmers have to implement generators and shrinking themselves, using QuickCheck (and `Arbitrary` in particular) can be considered expensive (in developer-time) and friction-full.


### Orphan instances for `Arbitrary`

A minor problem, but an annoying one nonetheless, is the question of where to write the `Arbitrary` instances.

* If we write the `Arbitrary` instances next to the data types, then the entire library that we are writing must depend on `QuickCheck`.
* If we write the `Arbitrary` instances in the test suite, then they are orphan instances and we have to re-write them for every test suite.
* If we write the `Arbitrary` instances in a seperate library, then we can depend on this library for the `Arbitrary` instances, but this adds maintenance overhead.

The most commonly chosen solution seems to be to put `Arbitrary` instances in the `quickcheck-instances` library, but this has another problem: now the test suite depends on extra libraries for which the instances are in `quickcheck-instances`, that the test suite may not be using at all.



## Hedgehog as a solution to QuickCheck's problems

[Hedgehog](https://hedgehog.qa) is a newer way to do property testing.

The example property of the `reverse` function is written as follows, using Hedgehog:

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

### Hedgehog generators

Hedgehog uses a different implementation of generators.
In particular, hedgehog generators have built-in shrinking, so that every generated value can automatically be shrunk without any extra developer effort to implement a shrinking function.
This built-in shrinking also automatically takes care to ensure that shrunk values obey the same invariants as the generator.

Whereas QuickCheck declares "shrink" as a function, in Hedgehog
generating and shrinking are performed simultaneously. Instead of
generating a single value, we can generate a rose tree of them, where
the main output is at the root, and the other nodes carry shrunk
versions of it, with the smallest ones at the leaves.

Generators are defined more generally, to allow for monadic effects during generation.
As such, generators are not seperated from properties entirely.
Indeed, as you can see in the `prop_reverse` example above, the generation of data and testing of properties can be interleaved monadically.

[Hedgehog also has adapters such that a programmer can use QuickCheck generators in HedgeHog properties.](https://www.stackage.org/haddock/lts-11.22/hedgehog-quickcheck-0.1/Hedgehog-Gen-QuickCheck.html)


### Sub-optimal shrinking

Hedgehog explicitly makes a tradeoff when it comes to shrinking.
The Hedgehog authors chose to make shrinking sub-optimal in order to build it into generators.

The following is an example of such a tradeoff.
Here Hedgehog is generating two characters that are equal, by generating them independently and then filterig the equal ones.
It is a contrived example, admittedly, but it makes my point.

``` haskell
λ > let gen = Gen.enum 'a' 'b' in Gen.printTree (Gen.filter (uncurry (==)) ((,) <$> gen <*> gen))
=== Outcome ===
('b','b')
=== Shrinks ===
('a','a')
('a','a')
('a','a')
```

As you can see, Hedgehog correctly shrinks the generated value `('b', 'b')` to tuples with equal characters `('a', 'a')`, but it shrinks to the same value many times.
This problem grows as the generated datastructure grows in size, and as the amount of options to try while shrinking grows.
This is the main drawback with integrating shrinking with generating: It requires some duplicated work during shrinking.

Side note: QuickCheck handles this case wrong as well, but in a different way:

``` haskell
λ filter (uncurry (==)) $ QC.shrink ('b','b')
[]
```

Spoiler (see the rest of the post): `genvalidity` handles this case correctly.

``` haskell
λ filter (uncurry (==)) $ shrinkUnchecked ('b', 'b')
[('a','a')]
```

### Even more expensive generators

Hedgehog does not use any type classes for generators like QuickCheck does with `Arbitrary`.
Instead, every generator must be a manually-written value.

This means that Hedgehog does not have any problems with orphan instances or with a lack of laws or semantics.
However, it also means that writing a generator for a value in Hedgehog now requires even more developer effort.


### More cumbersome property combinators.

It is still possible to write property combinators for Hedgehog properties, but now the programmer will have to explicitly pass in a given generator:

```
associative :: Gen a -> (a -> a -> a) -> Property
```

This makes writing property combinators more cumbersome, and tests using property combinators more verbose.



## Validity-based Testing as a solution to the problems of both QuickCheck and Hedgehog

Validity-based testing attempts to use the advantages from each of both QuickCheck and Hedgehog and make a different tradeoff.
It focuses on making property testing cheap (in terms of developer time) and removing as much friction as possible.

The goals are:
- Free generators
- Free shrinking
- Cheap properties

The example property of the reverse function is written as follows, using the `validity`-based libraries:

```
forAllUnchecked $ \str -> reverse (reverse str) == (str :: String)
```

### The `GenUnchecked` type-class in the `genvalidity` library for free generators and free shrinking.

The [`GenUnchecked`](https://www.stackage.org/haddock/lts-12.11/genvalidity-0.5.1.0/Data-GenValidity.html#t:GenUnchecked) type class defines both a generator: [`genUnchecked :: Gen a`](https://www.stackage.org/haddock/lts-12.11/genvalidity-0.5.1.0/Data-GenValidity.html#v:genUnchecked) and a shrinking function [`shrinkUnchecked :: a -> [a]`](https://www.stackage.org/haddock/lts-12.11/genvalidity-0.5.1.0/Data-GenValidity.html#v:shrinkUnchecked), just like QuickCheck does with `Arbitrary`.

Unlike QuickCheck's `Arbitrary`, `GenUnchecked` has very clear semantics:
Any value that can possibly be represented by the type should be generatable (excluding bottom values and infinite values).
Because of these very clear semantics, the implementations of both the `genUnchecked` and `shrinkUnchecked` functions are generated via the `Generic` instance by default.
This means that the programmer never has to write such a generator themselves, and shrinking comes for free as well.


### The `validity` library to define invariants explicitly

The next step for validity-based testing is to assert that types with internal invariants are as natural as types themselves.
Indeed, types exist to constrain the possible values of a given variable using compile-time checks whereas runtime invariants exist to constraint the possible values of a given variable using programming-time cleverness.
This will be important later, to further remove friction, but requires a bit of extra up-front thought.

The `validity` library allows programmers to explicitly define, _in code_ (instead of documentation), what it means for a value of a given type to be  valid.
Types that have an instance of the `Validity` typeclass implement the `validate :: a -> Validation` to explain all the reasons why a value may be invalid.
The semantics of this type class is that an invalid value of any type should never exist at runtime, otherwise the programmer has made a mistake.

For example, the type `Map k v` has an internal invariant: it should always have [`Data.Map.valid :: Ord k => Map k v -> Bool`](https://www.stackage.org/haddock/lts-12.11/containers-0.5.11.0/Data-Map-Lazy.html#v:valid) evaluate to `True`.
[Its `Validity` instance](https://www.stackage.org/haddock/lts-12.18/validity-containers-0.3.1.0/src/Data.Validity.Map.html#line-12) defines this explicitly:

``` haskell
-- | A 'Map' of things is valid if all the keys and values are valid and the 'Map' itself
-- is valid.
instance (Ord k, Validity k, Validity v) => Validity (Map k v) where
    validate m =
        mconcat
            [ declare "The Map structure is valid." $ M.valid m
            , delve "Map elements" $ M.toList m
            ]
``` 

This implementation checks that the internal map structure is valid: `M.valid`, and that the individual elements of the map are valid values by themselves as well.

The `validate` function does not just check whether a value is valid, but also reports all the reasons why a value may not be valid.
This makes for very useful error messages.
For example:

``` haskell
newtype Prime = Prime { unPrime :: Int } -- INVARIANT: isPrime

instance Validity Prime where
  validate (Prime p) = declare "The number is prime" $ isPrime p
```

```
λ let Left e = prettyValidate [Prime 2, Prime 3, Prime 5, Prime 6]
λ putStrLn e
The element at index 3 in the list
  \ Violated: The number is prime -- Aha!
```

The default implementation of `Validity` uses a type's `Generic` instance to derive an implementation that defines a value to be valid if all of its sub-parts are valid.
This makes it frictionless to instantiate `Validity` for types of which the validity of its values depends only on its sub-parts.

### The `GenValid` type-class in the `genvalidity` library for free generators and free shrinking for valid values.

To generate valid values, `genvalidity` has the [`GenValid`](https://www.stackage.org/haddock/lts-12.11/genvalidity-0.5.1.0/Data-GenValidity.html#v:GenValid) type class.
This type class also has very clear semantics.

* The `genValid :: Gen a` generator must only ever generate valid values.
* The `shrinkValid :: a -> [a]` shrinking function should must only ever shrink valid values to valid values..
* The `genValid` generator should be able to generate all possible valid values that can exist at runtime.

These semantics also allow for default implementations:

``` haskell
genValid :: GenValid a => Gen a
genValid = genUnchecked `suchThat` isValid

shrinkValid :: GenValid a => a -> [a]
shrinkValid = filter isValid . shrinkUnchecked
```

This makes it relatively frictionless to generate valid values as well.

Note that if there are not many valid values of a given type, or if checking validity is expensive, the default `genValid` implementation using `suchThat` may be too slow, and a programmer may have to override it with a faster implementation.
This is the tradeoff that `genvalidity` makes to remove friction, but there are helper functions in `genvalidity` to write faster generators too.

As a result, the following is all that's necessary for a programmer to write a custom type with generators, and start writing property tests:

``` haskell
{-# LANGUAGE DeriveGeneric #-}
module Example where

data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
  } deriving (Show, Eq, Generic)

instance Validity MyType

instance GenUnchecked MyType
instance GenValid MyType
```

### Overriding `genValid` to make it faster

While `genValid` is always correct by default, it can sometimes be too slow.
In such cases we want to override `genValid` to make it faster, but then it is up to us to make sure that it is still correct.
When overriding `genValid`, it is advisable to add [a test that makes sure that `genValid` still only generates valid values](https://www.stackage.org/haddock/lts-12.18/genvalidity-hspec-0.6.2.0/Test-Validity-GenValidity.html#v:genValidSpec)
It is also important that `genValid` can generate any value that would be considered valid.
We cannot test for this, so it is up to the programmer to maintain this property of the generator.

As an example, consider a `newtype Prime = Prime Int` with a `Validity` instance that describes that the integer inside is prime.
The `genUnchecked` generator is generated using the `Generic` instance, and it works exactly as it should.
The default implementation of `genValid` is equivalent to the following:

``` haskell
genValid :: Gen Prime
genValid = genUnchecked `suchThat` isValid
```

There are two ways to speed up a generator that uses `suchThat`:

- Generate values that are automatically valid, and don't filter them using `isValid` afterward.
  This is particularly useful if the `isValid` predicate is expensive to compute.
  In the case of `Prime`: primality testing is relatively expensive.

- Generate values that are more likely to be valid than the ones generated by `genUnchecked` and then filter them with `isValid`.
  This is particularly useful if there aren't many valid values.
  In the case of `Prime`: the likelihood for an arbitrary number goes down as the number grows larger.


In the case of the `Prime` example, we could speed up `genValid` by replacing `genUnchecked` by a generator that
either generates `2`, or an odd number.
The result would look like this:

``` haskell
genValid :: Gen Prime
genValid = (oneof [pure 2, (\x -> 2*x + 1) <$> genUnchecked]) `suchThat` isValid
```

### Cheap properties using property combinators from `genvalidity-property`

Once these type classes have been implemented, a programmer can use the many property combinators from `genvalidity-property` and even test suite combinators from `genvalidity-hspec`.

``` haskell
-- # Property combinators
-- Test whether two functions are inverses
inverseFunctions :: (Show a, Eq a, GenUnchecked a) => (a -> b) -> (b -> a) -> Property
-- Test whether a binary relation is symmetric
symmetry :: (Show a, GenUnchecked a) => (a -> a -> Bool) -> Property
-- Test whether a binary operator is commutative:
commutative :: (Show a, Eq a, GenUnchecked a) => (a -> a -> a) -> Property
-- Test whether a function maintains validity
producesValidsOnValids :: (Show a, Show b, GenValid a, Validity b) => (a -> b) -> Property
```

Each property combinator in `genvalidity-property` comes in multiple variants:

- `symmetry` for unchecked values
- `symmetryOnValid` for values generated by `GenValid`
- `symmetryOnArbitrary` for values generated by `Arbitrary` (for compatibility with QuickCheck)
- `symmetryOnGens` for values generated by a given generator

### Cheap properties using test suite combinators from `genvalidity-hspec`

With the `TypeApplications` extension that is new in GHC 8.0, we can now write entire test suites that are parametric in type argument (without the use of proxies).
These so-called test suite combinators are of the form `testSuiteCombinator :: forall a. Spec`.

The `genvalidity-hspec` library and its companion libraries already contain many test suite combinators.

``` haskell
-- Test whether a custom Eq instance of a type makes sense
eqSpec :: forall a. (Show a, Eq a, Typeable a, GenUnchecked a) => Spec
-- Test that a Monad doesn't violate the monad laws
monadSpec :: forall (f :: * -> *). (Monad f, Eq (f Int), Show (f Int), Typeable f, GenUnchecked (f Int)) => Spec
-- Test whether a lens doesn't violate the lens laws.
lensSpec :: forall s b. (Show b, Eq b, GenUnchecked b, Validity b, Show s, Eq s, GenUnchecked s, Validity s) => Lens' s b -> Spec
```

Each property combinator in `genvalidity-hspec` comes in multiple variants:

- `eqSpec` for unchecked values
- `eqSpecOnValid` for values generated by `GenValid`
- `eqSpecOnArbitrary` for values generated by `Arbitrary` (for backward compatibility)
- `eqSpecOnGens` for values generated by a given generator

## Conclusion

Property testing can be a great tool in your quality arsenal.
This point has outlined the major similarities and differences between property testing tools.
You should now be more equipped to make the necessary tradeoffs between these tools, but more important than trying to optimise your usage is that you give property testing a try.

Below are some exercises to get you started with Validity-based testing.
[`validity`](https://www.stackage.org/package/validity) and the related libraries can all be found on Stackage.

### Exercise: Odd

Objective: Generate valid `Odd` values using `genValid` by completing the following piece of code.

``` haskell
newtype Odd = Odd Int -- INVARIANT: The Int must be odd
```

TODO: put this in a spoiler

Solution:
``` haskell
instance Odd Int where
  validate (Odd i) = declare "The Int is odd" $ odd i
instance GenUnchecked Odd
instance GenValid Odd
```

### Exercise: JSON

Objective: Test whether the JSON instance in the following piece of code works, using `genvalidity-hspec-aeson`, by completing the following piece of code:

```
data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
  } deriving (Show, Eq, Generic)

instance FromJSON MyType where
  parseJSON = withObject "MyType" $ \o -> MyType <$> o .: "bool" <*> o .: "Rational"
instance ToJSON MyType where
  toJSON mt = object ["bool" .= (myBool mt), "double" .= (myRational mt)]
```

TODO: put this in a spoiler
Solution:

``` haskell
data MyType = MyType
  { myBool :: Bool
  , myRational :: Rational
  } deriving (Show, Eq, Generic)

instance FromJSON MyType where
  parseJSON = withObject "MyType" $ \o -> MyType <$> o .: "bool" <*> o .: "Rational"
instance ToJSON MyType where
  toJSON mt = object ["bool" .= (myBool mt), "double" .= (myRational mt)]

instance Validity MyType
instance GenUnchecked MyType
instance GenValid MyType

spec :: Spec
spec =
  describe "MyType" $
    jsonSpecOnValid @MyType
```
