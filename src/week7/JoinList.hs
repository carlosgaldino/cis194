module JoinList where

import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append ((tag x) <> (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag (Single x _)   = x
tag (Append x _ _) = x
