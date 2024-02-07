{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module MyLib where

import GHC.Generics

data Ab = Ab { f1 :: Maybe Int, f2 :: Maybe String }
    deriving (Generic, Show)

data Gb = Gb { f3 :: String, f4 :: Maybe Ab, f5 :: Maybe [Ab]}
    deriving (Generic, Show)

data JSON = O [(String, JSON)] | S String | L [JSON] | I Int | N deriving Show

unO :: JSON -> [(String, JSON)]
unO x = case x of
    O l -> l
    _ -> undefined

class ToJSON a where
    toJSON :: a -> JSON
    default toJSON :: (Generic a, ToJSON' (Rep a)) => a -> JSON
    toJSON x = toJSON' (from x)

class ToJSON' f where
    toJSON' :: f p -> JSON

instance (ToJSON' f, ToJSON' g) => ToJSON' (f :+: g) where
    toJSON' (L1 x) = toJSON' x
    toJSON' (R1 x) = toJSON' x

instance (ToJSON' f, ToJSON' g) => ToJSON' (f :*: g) where
    toJSON' (x :*: y) = O (unO (toJSON' x) ++ unO (toJSON' y))

instance {-# OVERLAPPING #-} (ToJSON' f, Selector m) => ToJSON' (M1 S m f) where
    toJSON' x = O [(selName x, toJSON' (unM1 x))]

instance (ToJSON c) => ToJSON' (K1 i c) where
    toJSON' (K1 x) = toJSON x

instance ToJSON Char where
    toJSON x = S [x]

instance {-# OVERLAPPING #-} ToJSON [Char] where
    toJSON = S

instance ToJSON Int where
    toJSON = I

instance ToJSON a => ToJSON [a] where
    toJSON = L . map toJSON

instance ToJSON a => ToJSON (Maybe a) where
    toJSON Nothing = N
    toJSON (Just x) = toJSON x

instance ToJSON' f => ToJSON' (M1 i t f) where
    toJSON' (M1 x) = toJSON' x

instance ToJSON Ab
instance ToJSON Gb

{-
data Add = Add (Maybe Int -> RetAdd)
data RetAdd = End Int | Continue Add
add :: Add
add = Add (inner 0)
    where
        inner sum Nothing = End sum
        inner sum (Just x) = Continue (Add (inner (sum + x)))

(<**>) :: RetAdd -> Maybe Int -> RetAdd
(<**>) (Continue (Add f)) x = f x
(<**>) (End x) _ = End x

retAdd :: RetAdd -> Int
retAdd (End x) = x

start :: RetAdd
start = Continue add
-}