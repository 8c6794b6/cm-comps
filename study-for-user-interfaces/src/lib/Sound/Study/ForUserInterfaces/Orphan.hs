{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Module to contain orphan instances.

-}
module Sound.Study.ForUserInterfaces.Orphan where

-- For Data.Hashable
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import Sound.SC3
import Sound.SC3.UGen.MCE

#define DERIVE_1(ty) \
deriving instance Generic ty; \
instance Hashable ty;

DERIVE_1(UGenId)
DERIVE_1(Special)
DERIVE_1(Proxy)
DERIVE_1(Primitive)
DERIVE_1(MRG)
DERIVE_1(Label)
DERIVE_1(Rate)
DERIVE_1(Control)
DERIVE_1(Constant)
DERIVE_1(UGen)

deriving instance Generic (MCE a)
instance Hashable a => Hashable (MCE a)
