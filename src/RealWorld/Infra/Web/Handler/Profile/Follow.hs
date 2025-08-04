{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Profile.Follow where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Effectful (Eff)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.User.UseCase (FollowUserError (..), FollowUserResult (..))
import qualified RealWorld.Domain.Command.User.UseCase as UserUseCase
import RealWorld.Domain.Query.Data (Profile (..))
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, JSON, Post, ServerError, (:>))

type Route =
  "profiles"
    :> Capture "username" Text
    :> "follow"
    :> Post '[JSON] FollowResponse

data FollowResponse = FollowResponse
  { profile :: Profile
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toError :: FollowUserError -> ServerError
toError FollowUserErrorInvalidUserId = badRequest "Invalid User ID"
toError FollowUserErrorUserNotFound = notFound' "User not found"
toError FollowUserErrorCantFollowSelf = badRequest "Cannot follow self"
toError FollowUserErrorAlreadyFollowing = badRequest "Already following"
toError FollowUserErrorInvalidUsername = badRequest "Invalid username"

handler ::
  ( KatipE Eff.:> es
  , UserRepository Eff.:> es
  , TxManager Eff.:> es
  , Error ServerError Eff.:> es
  ) =>
  ApiAuth ->
  Text ->
  Eff es FollowResponse
handler (ApiAuth userId _) username = do
  result <- UserUseCase.followUser toCommand
  case result of
    Right result' -> pure $ FollowResponse $ toProfile result'
    Left err -> do
      katipAddContext (sl "error" err <> sl "username" username) $ do
        $(logTM) ErrorS "follow error"
      throwError $ toError err
 where
  toCommand :: UserUseCase.FollowUserCommand
  toCommand =
    UserUseCase.FollowUserCommand
      { userId = userId
      , username = username
      }
  toProfile :: UserUseCase.FollowUserResult -> Profile
  toProfile result =
    Profile
      { username = result.username
      , bio = result.bio
      , image = result.image
      , following = result.following
      }