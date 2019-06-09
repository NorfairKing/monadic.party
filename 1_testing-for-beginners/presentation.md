---
title:  Testing for beginners in Haskell
author: Tom Sydney Kerckhove
patat:
  margins:
    left: 1
    right: 1
---

# Testing for beginners in Haskell

(Testing for beginners) in Haskell
Testing for (beginners in Haskell)

# (Testing for beginners) in Haskell

Why do we test?

```
+--------------------------------------------+
|          All programs                      |
| +----------------------------------------+ |
| |     Programs that typecheck            | |
| |  +------------------------------+      | |
| |  | Programs that pass our tests |      | |
| |  |        +-------------------------+  | |
| |  +--------|    Correct Programs     |  | |
| |           +-------------------------+  | |
| +----------------------------------------+ |
+--------------------------------------------+
```

<!--

1. Use a precise type system so that the 
   set of programs that typecheck is small.
2. Use tests to make sure that the set of
   programs that pass tests includes (ideally)
   only programs that satisfy the specifications.

The set of correct programs need not only contain one program.

-->

# (Testing for beginners) in Haskell

What is a test?

``` haskell
type Test = IO () -- Some code to execute
```

``` haskell
data TestResult = TestPasses | TestFails
-- If it crashes, then the test fails.
doesItCrash :: Test -> IO TestResult
```

# Testing for (beginners in Haskell): Hspec

``` haskell
-- Easy way to make code crash
shouldBe :: (Show a, Eq a) => a -> a -> IO ()
```

``` haskell
main :: IO ()
main = hspec $
  describe "addition" $       -- Scope
    it "still makes sense" $  -- Sentence description
      1 + 1 `shouldBe` 2      -- Actual test
```


# Fundamental rule of safety

When can you stop testing?


*Any code that could have been written, that passes the checks,
and is still wrong, is to be written with a small enough
likelihood for you to be comfortable.*


# Assessing the likelihood that some code will be written

- Inherent Complexity
- Length
- Diffitulty


# Example: reversing a list

``` haskell
-- | Reverse a list
reverse :: [a] -> [a]
```

Example, write code that typechecks and
passes these tests and is still wrong:

tests:

``` haskell
main :: IO ()
main = hspec $
  describe "reverse" $
    it "does nothing to an empty list" $
      reverse [] `shouldBe` []
    it "does nothing to a list of 5" $
      reverse [5] `shouldBe` [5]
    it "reverses this list correctly" $
      reverse [1,5,4] `shouldBe` [4,5,1]
```

# Example: reversing a list

Wrong, but passes tests:

``` haskell
rev :: [a] -> [a]
rev [] = []
rev [x] = [x]
rev [x,y,z] = [z,y,x]
rev ls = ls
```

Correct:

``` haskell
rev :: [a] -> [a]
rev [] = []
rev (a:as) = rev as ++ [a]
```


# Properties

``` haskell
-- | Suppose you had a function like this.
--
-- Run the given function for every possible argument.
propertyTest :: (a -> IO ()) -> Test
```

# Example: sorting a list


``` haskell
-- | Sort a list
sort :: Ord a => [a] -> [a]
```

tests:

``` haskell
main :: IO ()
main = hspec $
  describe "sort" $
    it "does nothing to an empty list" $
      sort [] `shouldBe` []
    it "does nothing to a list of 5" $
      sort [5] `shouldBe` [5]
    it "sorts this list correctly" $
      sort [1,5,4] `shouldBe` [1,4,5]
```

# Example: reversing a list

Wrong, but passes tests:

``` haskell
s :: [a] -> [a]
s [] = []
s [x] = [x]
s [x,y,z] = [x,z,y]
s ls = ls
```

Correct:

``` haskell
s [] = []
s (x:xs) = s small ++ mid ++ s large
  where
    small = filter (< x) xs
    mid   = filter (== x) xs ++ [x]
    large = filter (> x) xs
```

# Example: sorting a list

``` haskell
main :: IO ()
main = hspec $
  describe "reverse" $
    it "does nothing to an empty list" $
      sort [] `shouldBe` []
    it "does nothing to a list of 5" $
      sort [5] `shouldBe` [5]
    it "sorts this list correctly" $
      sort [1,5,4] `shouldBe` [1,4,5]
    it "is idempotent" $ 
      property $ \ls ->
        sort (sort ls) `shouldBe` sort ls
```

# Example: reversing a list

Wrong, but passes tests:

``` haskell
s :: [a] -> [a]
s [] = []
s [x] = [x]
s [x, y] = if x <= y then [x, y] else [y, x]
s [x, y, z] =
  if x <= y
  then if y <= z then [x, y, z] else [x, z, y]
  else if x <= z then [y, x, z] else [y, z, x]
s ls = ls
```

Correct:

``` haskell
s [] = []
s (x:xs) = s small ++ mid ++ s large
  where
    small = filter (< x) xs
    mid   = filter (== x) xs ++ [x]
    large = filter (> x) xs
```

# Example: sorting a list

``` haskell
main :: IO ()
main = hspec $
  describe "reverse" $
    it "does nothing to an empty list" $
      sort [] `shouldBe` []
    it "does nothing to a list of 5" $
      sort [5] `shouldBe` [5]
    it "sorts this list correctly" $
      sort [1,5,4] `shouldBe` [1,4,5]
    it "is idempotent" $ 
      property $ \ls ->
        sort (sort ls) `shouldBe` sort ls
    it "sorts a list" $ 
      property $ \ls ->
        isSorted (sort ls)

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted (x:xs) = all (>= x) xs && isSorted xs
```


# Overview

- Tests are cool (see diagram)
- Tests are simple:

``` haskell
type Test = IO () -- Some code to execute
type Property = a -> Test -- Sort-of
```

- Tests are simple in Haskell:

``` Haskell
spec :: Spec
spec = do
  describe "myFunction" -- Scope
    it "works" $        -- Sentence description
      works myFunction  -- Assertion
```

- Try breaking things
