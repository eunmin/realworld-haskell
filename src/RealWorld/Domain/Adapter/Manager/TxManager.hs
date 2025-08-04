{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Adapter.Manager.TxManager where

import Effectful (Effect)
import Effectful.TH (makeEffect)

data TxManager :: Effect where
  WithTx :: m a -> TxManager m a

makeEffect ''TxManager
