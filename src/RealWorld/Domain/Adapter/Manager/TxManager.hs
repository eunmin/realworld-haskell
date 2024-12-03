module RealWorld.Domain.Adapter.Manager.TxManager where

class TxManager m where
  withTx :: ExceptT e m a -> ExceptT e m a