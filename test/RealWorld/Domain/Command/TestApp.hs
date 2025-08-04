{-# LANGUAGE DataKinds #-}

module RealWorld.Domain.Command.TestApp where

import Effectful (Eff, IOE, runEff)
import Effectful.Katip (KatipE, runKatipContextE)
import Katip (initLogEnv)
import RealWorld.Domain.Adapter.Gateway.PasswordGateway (PasswordGateway (..))
import RealWorld.Domain.Adapter.Gateway.TokenGateway (TokenGateway (..))
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager (..))
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository (..))
import RealWorld.Domain.Command.Fixture (Fixture (..))
import qualified RealWorld.Domain.Command.Interpreter.Gateway.PasswordGateway as PasswordGatewayInterpreter
import qualified RealWorld.Domain.Command.Interpreter.Gateway.TokenGateway as TokenGatewayInterpreter
import qualified RealWorld.Domain.Command.Interpreter.Manager.TxManager as TxManagerInterpreter
import qualified RealWorld.Domain.Command.Interpreter.Repository.UserRepository as UserRepositoryInterpreter
import Relude

type TestApp =
  Eff
    '[PasswordGateway, TokenGateway, UserRepository, TxManager, KatipE, IOE]

runApp :: Fixture IO -> TestApp a -> IO a
runApp fixture action = do
  le <- initLogEnv "RealWorld" "test"
  runEff
    . runKatipContextE le () mempty
    . TxManagerInterpreter.run
    . UserRepositoryInterpreter.run fixture
    . TokenGatewayInterpreter.run fixture
    . PasswordGatewayInterpreter.run fixture
    $ action
