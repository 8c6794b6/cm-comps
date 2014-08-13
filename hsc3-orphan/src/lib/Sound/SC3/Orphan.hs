{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Copyright   : 8c6794b6, 2014
License     : BSD3

Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : unknown

Temporary module to contain orphan instances for data types defined in hsc3.

-}
module Sound.SC3.Orphan where

import Sound.SC3
import Sound.SC3.UGen.MCE

import GHC.Generics (Generic)
import Data.Data (Data, Typeable)
import Data.Hashable (Hashable)

#define DERIVE_1(ty) \
deriving instance Generic ty; \
deriving instance Typeable ty; \
deriving instance Data ty; \
instance Hashable ty;

DERIVE_1(AddAction)
DERIVE_1(Constant)
DERIVE_1(Control)
DERIVE_1(FromPort)
DERIVE_1(Graph)
DERIVE_1(KType)
DERIVE_1(Label)
DERIVE_1(MRG)
DERIVE_1(Node)
DERIVE_1(Primitive)
DERIVE_1(Proxy)
DERIVE_1(Rate)
DERIVE_1(Special)
DERIVE_1(Synthdef)
DERIVE_1(UGen)
DERIVE_1(UGenId)

deriving instance Generic (MCE a)
deriving instance Typeable MCE
deriving instance Data a => Data (MCE a)
instance Hashable a => Hashable (MCE a)
