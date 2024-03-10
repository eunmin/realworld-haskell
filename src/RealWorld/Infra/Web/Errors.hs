{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.Errors where

import RealWorld.Domain.Command.User.UseCase
import Relude
import qualified Text.Show

data Messages
  = Unauthorized
  | UserNotFound
  | ProfileNotFound
  | UsernameAlreadyExists
  | EmailAlreadyExists
  | InvalidPassword
  | InvalidUsername
  | InvalidEmail

instance Show Messages where
  show Unauthorized = "인증되지 않은 사용자입니다."
  show UserNotFound = "사용자를 찾을 수 없습니다."
  show ProfileNotFound = "프로필을 찾을 수 없습니다."
  show UsernameAlreadyExists = "이미 사용 중인 username 입니다."
  show EmailAlreadyExists = "이미 사용 중인 이메일 입니다."
  show InvalidPassword = "비밀번호에는 최소 12자 이상, 최소 하나의 문자, 숫자, 특수문자가 포함되어야 합니다."
  show InvalidUsername = "잘못된 username 입니다. username은 최소 3자 이상, 최대 20자 이하로 입력해주세요."
  show InvalidEmail = "잘못된 이메일 주소입니다. 이메일 주소를 확인해주세요."

instance Show RegistrationError where
  show RegistrationErrorUsernameAlreadyExists = show UsernameAlreadyExists
  show RegistrationErrorEmailAlreadyExists = show EmailAlreadyExists
  show RegistrationErrorInvalidPassword = show InvalidPassword
  show RegistrationErrorInvalidUsername = show InvalidUsername
  show RegistrationErrorInvalidEmail = show InvalidEmail

instance Show AuthenticationError where
  show AuthenticationErrorUserNotFound = show UserNotFound
  show AuthenticationErrorInvalidPassword = show InvalidPassword
  show AuthenticationErrorInvalidEmail = show InvalidEmail

instance Show FollowUserError where
  show FollowUserErrorInvalidToken = show Unauthorized
  show FollowUserErrorUserNotFound = "팔로우할 사용자를 찾을 수 없습니다."
  show FollowUserErrorCantFollowSelf = "자신을 팔로우할 수 없습니다."
  show FollowUserErrorAlreadyFollowing = "이미 팔로우 중인 사용자입니다."
  show FollowUserErrorInvalidUsername = "팔로우할 사용자를 찾을 수 없습니다."