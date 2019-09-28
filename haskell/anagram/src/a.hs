module A where

data Person =
  Person String Int

name :: Person -> String
name (Person s _) = s

age :: Person -> Int
age (Person _ n) = n
