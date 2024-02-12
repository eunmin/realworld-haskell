module Conduit.Domain.Repo where

class Tx m where
  withTx :: m a -> m a