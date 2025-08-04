{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module RealWorld.Infra.Web.Handler.Profile.Unfollow where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Swagger (ToSchema)
import Effectful (Eff)
import qualified Effectful as Eff
import Effectful.Error.Dynamic (Error, throwError)
import Effectful.Katip
import RealWorld.Domain.Adapter.Manager.TxManager (TxManager)
import RealWorld.Domain.Adapter.Repository.UserRepository (UserRepository)
import RealWorld.Domain.Command.User.UseCase (UnfollowUserError (..), UnfollowUserResult (..))
import qualified RealWorld.Domain.Command.User.UseCase as UserUseCase
import RealWorld.Domain.Query.Data (Profile (..))
import RealWorld.Infra.Converter.Aeson ()
import RealWorld.Infra.Web.Auth (ApiAuth (..))
import RealWorld.Infra.Web.ErrorResponse (badRequest, notFound')
import RealWorld.Infra.Web.Schema ()
import Relude
import Servant (Capture, Delete, JSON, ServerError, (:>))

type Route =
  "profiles"
    :> Capture "username" Text
    :> "follow"
    :> Delete '[JSON] UnfollowResponse

data UnfollowResponse = UnfollowResponse
  { profile :: Profile
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

toError :: UnfollowUserError -> ServerError
toError UnfollowUserErrorInvalidUserId = badRequest "Invalid User ID"
toError UnfollowUserErrorInvalidUsername = badRequest "Invalid Username"
toError UnfollowUserErrorUserNotFound = notFound' "User not found"

handler ::
  ( KatipE Eff.:> es
  , UserRepository Eff.:> es
  , TxManager Eff.:> es
  , Error ServerError Eff.:> es
  ) =>
  ApiAuth ->
  Text ->
  Eff es UnfollowResponse
handler (ApiAuth userId _) username = do
  result <- UserUseCase.unfollowUser toCommand
  case result of
    Right result' -> pure $ UnfollowResponse $ toProfile result'
    Left err -> do
      katipAddContext (sl "error" err <> sl "username" username) $ do
        $(logTM) ErrorS "follow error"
      throwError $ toError err
 where
  toCommand :: UserUseCase.UnfollowUserCommand
  toCommand =
    UserUseCase.UnfollowUserCommand
      { userId = userId
      , username = username
      }
  toProfile :: UserUseCase.UnfollowUserResult -> Profile
  toProfile result =
    Profile
      { username = result.username
      , bio = result.bio
      , image = result.image
      , following = result.following
      }