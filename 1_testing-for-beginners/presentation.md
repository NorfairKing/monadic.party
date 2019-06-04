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

What is a test?


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

# (Testing for beginners) in Haskell

``` haskell
type Test = IO ()
```

```
doesItCrash :: IO () -> IO Bool
```

# Testing for (beginners in Haskell): Hspec

``` haskell
shouldBe :: (Show a, Eq a) => a -> a -> IO ()
```

``` haskell
main :: IO ()
main = hspec $
  describe "addition" $
    it "still makes sense" $ 
      1 + 1 `shouldBe` 2
```


# Fundamental rule of safety

When can you stop testing?


*Any code that could have been written, that passes the checks,
and is still wrong, is to be written with a small enough
likelihood for you to be comfortable.*


# Assessing the likelihood that some code will be written

- Inherent Complexity of passing the checks
- Length of code


# Example: reversing a list

Example, write code that passes these tests and is still wrong:

spec: Reverse a list

```
reverse :: [a] -> [a]
```

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

