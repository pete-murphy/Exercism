{-# LANGUAGE InstanceSigs #-}

module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

data BST a
  = Node (BST a) a (BST a)
  | Leaf
  deriving (Eq, Show)

instance Foldable BST where
  foldMap :: Monoid m => (a -> m) -> BST a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

bstLeft :: BST a -> Maybe (BST a)
bstLeft (Node x _ _) = Just x
bstLeft _ = Nothing

bstRight :: BST a -> Maybe (BST a)
bstRight (Node _ _ x) = Just x
bstRight _ = Nothing

bstValue :: BST a -> Maybe a
bstValue (Node _ x _) = Just x
bstValue _ = Nothing

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (Node l val r)
  | x > val = Node l val (insert x r)
  | otherwise = Node (insert x l) val r

singleton :: a -> BST a
singleton x = Node Leaf x Leaf

toList :: BST a -> [a]
toList = foldMap (: [])
