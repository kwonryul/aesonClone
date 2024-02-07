{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Init where

import MyLib
import GHC.Generics
import Data.Set

class Init' f where
    init' :: (f p, Set String, Set String)

instance (Init' f, Init' g) => Init' (f :*: g) where
    init' = (initF :*: initG, union requiredF requiredG, union optionalF optionalG)
        where
            (initF, requiredF, optionalF) = init' @f
            (initG, requiredG, optionalG) = init' @g

instance {-# OVERLAPPING #-} (Selector m) => Init' (M1 S m (Rec0 (Maybe a))) where
    init' = (val, empty, singleton $ selName val)
        where
            val = M1 $ K1 Nothing :: M1 S m (Rec0 (Maybe a)) p

instance (Selector m) => Init' (M1 S m f) where
    init' = (val, singleton $ selName val, empty)
        where
            val = M1 undefined :: M1 S m f p