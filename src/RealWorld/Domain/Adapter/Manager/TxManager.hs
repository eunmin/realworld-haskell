module RealWorld.Domain.Adapter.Manager.TxManager where

import Relude

class TxManager m where
  withTx :: ExceptT e m a -> ExceptT e m a
