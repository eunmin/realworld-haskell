{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Domain.Command.User.UseCaseSpec where

import RealWorld.Domain.Command.Fixture (
  Fixture (
    _findUserByEmail,
    _findUserByUsername,
    _generateToken,
    _hashPassword,
    _saveUser
  ),
  emptyFixture,
  generateUser,
 )
import RealWorld.Domain.Command.TestApp (runApp)
import RealWorld.Domain.Command.User.UseCase (
  RegistrationCommand (RegistrationCommand),
  RegistrationError (..),
  RegistrationResult (RegistrationResult),
  registration,
 )
import RealWorld.Domain.Command.User.Value (
  HashedPassword (HashedPassword),
  Token (Token),
 )
import Relude
import Test.Hspec (Spec, describe, it, shouldReturn)

deriving instance Show RegistrationError

spec :: Spec
spec = describe "registration" $ do
  it "should pass" $ do
    let fixture =
          emptyFixture
            { _hashPassword = \_ -> pure $ Just $ HashedPassword "hashed"
            , _findUserByUsername = \_ -> pure Nothing
            , _findUserByEmail = \_ -> pure Nothing
            , _saveUser = \_ -> pure True
            , _generateToken = \_ _ -> pure $ Token "token"
            }
    let command = RegistrationCommand "username" "username@example.com" "abcd1234!@#$"
    let result = runApp fixture $ registration command
    result `shouldReturn` Right (RegistrationResult "token")

  it "should fail when username is already taken" $ do
    user <- generateUser
    let fixture =
          emptyFixture
            { _hashPassword = \_ -> pure $ Just $ HashedPassword "hashed"
            , _findUserByUsername = \_ -> pure $ Just user
            }
    let command = RegistrationCommand "username" "username@example.com" "abcd1234!@#$"
    let result = runApp fixture $ registration command
    result `shouldReturn` Left RegistrationErrorUsernameAlreadyExists

  it "should fail when email is already taken" $ do
    user <- generateUser
    let fixture =
          emptyFixture
            { _hashPassword = \_ -> pure $ Just $ HashedPassword "hashed"
            , _findUserByUsername = \_ -> pure Nothing
            , _findUserByEmail = \_ -> pure $ Just user
            }
    let command = RegistrationCommand "username" "username@example.com" "abcd1234!@#$"
    let result = runApp fixture $ registration command
    result `shouldReturn` Left RegistrationErrorEmailAlreadyExists
