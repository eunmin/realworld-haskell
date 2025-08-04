{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Domain.Adapter.Repository.UserRepository where

import Data.ULID (ULID)
import Effectful (Effect)
import Effectful.TH (makeEffect)
import RealWorld.Domain.Command.User.Entity.User (User)
import RealWorld.Domain.Command.User.Value (Email, Username)
import Relude

data UserRepository :: Effect where
  Save :: User -> UserRepository m Bool
  FindById :: ULID -> UserRepository m (Maybe User)
  FindByUsername :: Username -> UserRepository m (Maybe User)
  FindByEmail :: Email -> UserRepository m (Maybe User)
  Follow :: ULID -> ULID -> UserRepository m Bool
  Unfollow :: ULID -> ULID -> UserRepository m Bool
  HasFollowing :: ULID -> ULID -> UserRepository m Bool

makeEffect ''UserRepository
