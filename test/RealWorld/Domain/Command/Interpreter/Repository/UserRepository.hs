{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Command.Interpreter.Repository.UserRepository where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Database.PgUserRepository as PgUserRepository
import Relude hiding (Reader)
import RealWorld.Domain.Command.Fixture (Fixture)
import qualified RealWorld.Domain.Command.Fixture as Fixture

run :: (IOE :> es) => Fixture IO -> Eff (UserRepository : es) a -> Eff es a
run fixture = interpret $ \_ -> \case
  Save user -> liftIO $ Fixture._saveUser fixture user
  FindById userId -> liftIO $ Fixture._findUserById fixture userId
  FindByUsername username -> liftIO $ Fixture._findUserByUsername fixture username
  FindByEmail email -> liftIO $ Fixture._findUserByEmail fixture email
  Follow followerId followeeId -> liftIO $ Fixture._followUser fixture followerId followeeId
  Unfollow followerId followeeId -> liftIO $ Fixture._unfollowUser fixture followerId followeeId
  HasFollowing followerId followeeId -> liftIO $ Fixture._hasFollowingUser fixture followerId followeeId
