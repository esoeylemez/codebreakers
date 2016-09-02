-- Copyright 2016 Ertugrul Söylemez
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module:     Codebreakers.Mod
-- Copyright:  Copyright 2016 Ertugrul Söylemez
-- License:    Apache License 2.0
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Codebreakers.Mod
    ( -- * Modular arithmetic
      Mod(..)
    )
    where

import Data.Proxy
import Data.Reflection


newtype Mod n =
    Mod Integer
    deriving (Eq, Ord, Show)

instance (Modulus n) => Num (Mod n) where
    Mod x + Mod y = Mod (mod (x + y) n)  where n = reflect (Proxy :: Proxy n)
    Mod x - Mod y = Mod (mod (x - y) n)  where n = reflect (Proxy :: Proxy n)
    Mod x * Mod y = Mod (mod (x*y) n)    where n = reflect (Proxy :: Proxy n)

    abs (Mod x) = Mod (mod x n)  where n = reflect (Proxy :: Proxy n)

    fromInteger x = Mod (mod x n)  where n = reflect (Proxy :: Proxy n)

    negate (Mod 0) = Mod 0
    negate (Mod x) = Mod (n - x)  where n = reflect (Proxy :: Proxy n)

    signum (Mod 0) = Mod 0
    signum (Mod _) = Mod 1


type Modulus n = Reifies n Integer
