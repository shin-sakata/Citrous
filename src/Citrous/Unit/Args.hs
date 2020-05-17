{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Citrous.Unit.Args where

-- | Type Level List (Heterogeneous List)
data HList :: [*] -> * where
  HNil :: HList '[]
  (:::) :: a -> HList xs -> HList (a ': xs)

infixr 5 :::

-- | Function Type for applying HList to function
--
--    Example :
--    Fn '[Int, Text] Bool == Int -> Text -> Bool
type family Fn (as :: [*]) r

type instance Fn '[] r = r

type instance Fn (x ': xs) r = x -> Fn xs r

-- | TypeFamily to represent the type after appending of HList
type family (ls :: [k]) ++ (rs :: [k]) :: [k]

type instance '[] ++ ys = ys

type instance (x ': xs) ++ ys = x ': (xs ++ ys)

-- | Append HList
hAppend :: HList xs -> HList ys -> HList (xs ++ ys)
hAppend HNil ys          = ys
hAppend (x1 ::: HNil) ys = x1 ::: ys

-- | Apply HList to function
apply :: Fn xs r -> HList xs -> r
apply v HNil       = v
apply f (a ::: as) = apply (f a) as
