{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Interpreter.Adapter.Repository.UserRepository where

import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Reader.Dynamic (Reader)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository (..))
import qualified RealWorld.Infra.Component.Database as Database
import qualified RealWorld.Infra.Database.PgUserRepository as PgUserRepository
import Relude hiding (Reader)

run :: (Reader Database.State :> es, IOE :> es) => Eff (UserRepository : es) a -> Eff es a
run = interpret $ \_ -> \case
  Save user -> PgUserRepository.save user
  FindById userId -> PgUserRepository.findById userId
  FindByUsername username -> PgUserRepository.findByUsername username
  FindByEmail email -> PgUserRepository.findByEmail email
  Follow followerId followeeId -> PgUserRepository.follow followerId followeeId
  Unfollow followerId followeeId -> PgUserRepository.unfollow followerId followeeId
  HasFollowing followerId followeeId -> PgUserRepository.hasFollowing followerId followeeId