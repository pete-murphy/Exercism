{-# LANGUAGE InstanceSigs #-}

module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = EmptyList | Cons a (LinkedList a) 
  deriving (Eq, Show)

instance Foldable LinkedList where
  foldr :: (a -> b -> b) -> b -> LinkedList a -> b
  foldr _ b EmptyList = b
  foldr f b (Cons x xs) = f x (foldr f b xs)

datum :: LinkedList a -> a
datum (Cons x _) = x
datum EmptyList = error "Didn't handle this case!"

fromList :: [a] -> LinkedList a
fromList xs = case xs of
  [] -> EmptyList
  (y:ys) -> Cons y (fromList ys)

isNil :: LinkedList a -> Bool
isNil EmptyList = True
isNil _ = False

new :: a -> LinkedList a -> LinkedList a
new = Cons

next :: LinkedList a -> LinkedList a
next EmptyList = error "Can't handle an empty list"
next (Cons _ xs) = xs

nil :: LinkedList a
nil = EmptyList

reverseLinkedList :: LinkedList a -> LinkedList a

-- reverseLinkedList EmptyList = EmptyList

-- reverseLinkedList (Cons x restOfList) = append (reverseLinkedList restOfList) (new x nil)
-- reverseLinkedList = go EmptyList
--   where 
--     go acc EmptyList = acc
--     go acc (Cons x restOfList) = go (Cons x acc) restOfList

reverseLinkedList = foldl (flip Cons) EmptyList

append :: LinkedList a -> LinkedList a -> LinkedList a
append EmptyList otherList = otherList
append (Cons x xs) otherList = Cons x (append xs otherList)

toList :: LinkedList a -> [a]
toList EmptyList = []
toList (Cons x xs) = x : toList xs