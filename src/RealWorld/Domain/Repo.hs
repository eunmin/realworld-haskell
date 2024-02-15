module RealWorld.Domain.Repo where

import Relude

class Tx m where
  withTx :: ExceptT e m a -> ExceptT e m a