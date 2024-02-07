{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Mod1 where

import MyLib
import Init
import GHC.Generics
import Data.Set

instance FromJSON Ab
instance FromJSON Gb

class FromJSON a where
    fromJSON :: JSON -> Maybe a
    default fromJSON :: (Generic a, FromJSON' (Rep a)) => JSON -> Maybe a
    fromJSON j = to <$> (fromJSON' j)

class FromJSON' f where
    fromJSON' :: JSON -> Maybe (f p)

instance (FromJSON' f, FromJSON' g) => FromJSON' (f :+: g) where
    fromJSON' j = case fromJSON' @f j of
        Nothing -> R1 <$> fromJSON' @g j
        x -> L1 <$> x

instance (PutKeyValue' f, PutKeyValue' g, Init' f, Init' g) => FromJSON' (f :*: g) where
    fromJSON' (O kvs') = do
        (finalF, finalG, finalReqF, finalReqG, _, _) <- inner kvs' initF initG requiredF requiredG optionalF optionalG
        if union finalReqF finalReqG == empty then Just (finalF :*: finalG) else Nothing
        where
            (initF, requiredF, optionalF) = init' :: (f p, Set String, Set String)
            (initG, requiredG, optionalG) = init' :: (g p, Set String, Set String)
            inner [] fp gp reqF reqG optF optG = Just (fp, gp, reqF, reqG, optF, optG)
            inner ((k, v) : kvs) fp gp reqF reqG optF optG =
                if member k reqF then
                    let fp' = putKeyValue' k v fp in
                    case fp' of
                        Nothing -> Nothing
                        Just fp'' -> inner kvs fp'' gp (delete k reqF) reqG optF optG
                else if member k reqG then
                    let gp' = putKeyValue' k v gp in
                    case gp' of
                        Nothing -> Nothing
                        Just gp'' -> inner kvs fp gp'' reqF (delete k reqG) optF optG
                else if member k optF then
                    let fp' = putKeyValue' k v fp in
                    case fp' of
                        Nothing -> Nothing
                        Just fp'' -> inner kvs fp'' gp reqF reqG (delete k optF) optG
                else if member k optG then
                    let gp' = putKeyValue' k v gp in
                    case gp' of
                        Nothing -> Nothing
                        Just gp'' -> inner kvs fp gp'' reqF reqG optF (delete k optG)
                else
                    inner kvs fp gp reqF reqG optF optG
    fromJSON' _ = Nothing

class PutKeyValue' f where
    putKeyValue' :: String -> JSON -> f p -> Maybe (f p)

instance (PutKeyValue' f, PutKeyValue' g) => PutKeyValue' (f :*: g) where
    putKeyValue' k v (fp :*: gp) = do
        fp' <- putKeyValue' k v fp
        gp' <- putKeyValue' k v gp
        return (fp' :*: gp')

instance (Selector m, FromJSON a) => PutKeyValue' (M1 S m (Rec0 a)) where
    putKeyValue' k v x = if selName x == k then
        case fromJSON v of
            Nothing -> Nothing
            Just value -> Just (M1 $ K1 value)
        else
            Just x

instance (Init' (M1 S m (Rec0 a)), PutKeyValue' (M1 S m (Rec0 a)), Selector m) => FromJSON' (M1 S m (Rec0 a)) where
    fromJSON' (O kvs') = do
        (final, finalReq, _) <- inner kvs' init'' required optional
        if finalReq == empty then Just final else Nothing
        where
            (init'', required, optional) = (init' @(M1 S m (Rec0 a)))
            inner [] fp req opt = Just (fp, req, opt)
            inner ((k, v) : kvs) fp req opt =
                if member k req then
                    case putKeyValue' k v fp of
                        Nothing -> Nothing
                        Just fp' -> inner kvs fp' req opt
                else if member k opt then
                    case putKeyValue' k v fp of
                        Nothing -> Nothing
                        Just fp' -> inner kvs fp' req opt
                else
                    inner kvs fp req opt
    fromJSON' _ = Nothing

instance FromJSON' f => FromJSON' (M1 i t f) where
    fromJSON' x = do
        val <- fromJSON' x
        return (M1 val)

instance FromJSON Int where
    fromJSON (I x) = Just x
    fromJSON _ = Nothing

instance FromJSON a => FromJSON [a] where
    fromJSON (L l) = sequence (fromJSON @a <$> l)
    fromJSON _ = Nothing

instance FromJSON Char where
    fromJSON (S c) = if length c == 1 then Just $ head c else Nothing
    fromJSON _ = Nothing

instance {-# OVERLAPPING #-} FromJSON [Char] where
    fromJSON (S s) = Just s
    fromJSON _ = Nothing

instance FromJSON a => FromJSON (Maybe a) where
    fromJSON x = Just $ fromJSON x