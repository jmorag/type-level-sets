{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExampleSet2 where

import GHC.TypeLits ( Nat, CmpNat, type (+), type (-) )
import Data.Type.Set

type instance Cmp (Natural n) (Natural m) = CmpNat n m
type instance Cmp String String      = 'EQ
type instance Cmp String (Natural _) = 'LT
type instance Cmp (Natural _) String = 'GT

data Natural (a :: Nat) where
    Z :: Natural 0
    S :: Natural n -> Natural (n + 1)

deriving instance Show (Natural n)

instance Eq (Natural n) where
    _ == _ = True

class MkNat (n :: Nat) where
  mkNat :: Natural n

instance MkNat 0 where
  mkNat = Z

instance {-# OVERLAPS #-} (MkNat (n - 1), ((n - 1) + 1) ~ n) => MkNat n where
  mkNat = S $ mkNat @(n - 1)

foo :: Set '[String, Natural 1]
foo = asSet $ Ext "str1" $ Ext (S Z) Empty

bar :: Set '[String]
bar = asSet $ Ext "str2" Empty

foobar :: Set '[String, Natural 1]
foobar = foo `union` bar

barfoo :: Set '[String, Natural 1]
barfoo = bar `union` foo

fooStr :: String
fooStr = project Proxy foo

foobarStr :: String
foobarStr = project Proxy foobar

barfooStr :: String
barfooStr = project Proxy barfoo

fooHasNat1 :: Bool
fooHasNat1 = member (Proxy :: Proxy (Natural 1)) foo

barHasNat1 :: Bool
barHasNat1 = member (Proxy :: Proxy (Natural 1)) bar

r0_9 :: Set '[Natural 0, Natural 1, Natural 2, Natural 3, Natural 4, Natural 5, Natural 6, Natural 7, Natural 8, Natural 9]
r0_9 =
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat Empty

r10_19 :: Set '[Natural 10, Natural 11, Natural 12, Natural 13, Natural 14, Natural 15, Natural 16, Natural 17, Natural 18, Natural 19]
r10_19 =
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat Empty

r20_29 :: Set '[Natural 20, Natural 21, Natural 22, Natural 23, Natural 24, Natural 25, Natural 26, Natural 27, Natural 28, Natural 29]
r20_29 =
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat Empty

r30_39 :: Set '[Natural 30, Natural 31, Natural 32, Natural 33, Natural 34, Natural 35, Natural 36, Natural 37, Natural 38, Natural 39]
r30_39 =
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat $
  Ext mkNat Empty
