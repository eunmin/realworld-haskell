{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Infra.Web.Errors where

import RealWorld.Domain.Command.User.UseCase
import Relude

data Messages
  = Unauthorized
  | UserNotFound
  | ProfileNotFound
  | UsernameAlreadyExists
  | EmailAlreadyExists
  | InvalidPassword
  | InvalidUsername
  | InvalidEmail

instance ToText Messages where
  toText Unauthorized = "인증되지 않은 사용자입니다."
  toText UserNotFound = "사용자를 찾을 수 없습니다."
  toText ProfileNotFound = "프로필을 찾을 수 없습니다."
  toText UsernameAlreadyExists = "이미 사용 중인 username 입니다."
  toText EmailAlreadyExists = "이미 사용 중인 이메일 입니다."
  toText InvalidPassword = "비밀번호에는 최소 12자 이상, 최소 하나의 문자, 숫자, 특수문자가 포함되어야 합니다."
  toText InvalidUsername = "잘못된 username 입니다. username은 최소 3자 이상, 최대 20자 이하로 입력해주세요."
  toText InvalidEmail = "잘못된 이메일 주소입니다. 이메일 주소를 확인해주세요."

instance ToText RegistrationError where
  toText RegistrationErrorUsernameAlreadyExists = toText UsernameAlreadyExists
  toText RegistrationErrorEmailAlreadyExists = toText EmailAlreadyExists
  toText RegistrationErrorInvalidPassword = toText InvalidPassword
  toText RegistrationErrorInvalidUsername = toText InvalidUsername
  toText RegistrationErrorInvalidEmail = toText InvalidEmail

instance ToText AuthenticationError where
  toText AuthenticationErrorUserNotFound = toText UserNotFound
  toText AuthenticationErrorInvalidPassword = toText InvalidPassword
  toText AuthenticationErrorInvalidEmail = toText InvalidEmail

instance ToText FollowUserError where
  toText FollowUserErrorInvalidUserId = toText Unauthorized
  toText FollowUserErrorUserNotFound = toText UserNotFound
  toText FollowUserErrorCantFollowSelf = "자신을 팔로우할 수 없습니다."
  toText FollowUserErrorAlreadyFollowing = "이미 팔로우 중인 사용자입니다."
  toText FollowUserErrorInvalidUsername = "팔로우할 사용자를 찾을 수 없습니다."
