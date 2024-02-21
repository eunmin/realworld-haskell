module RealWorld.Domain.Command.Article.Value where

import Relude

----------------------------------------------------------------------------------------------------
-- Slug

newtype Slug = Slug {unSlug :: Text}
  deriving (Show, Eq, Generic)

toSlug :: Title -> Slug
toSlug = Slug . unTitle

----------------------------------------------------------------------------------------------------
-- Title

newtype Title = Title {unTitle :: Text}
  deriving (Show, Eq, Generic)

mkTitle :: Text -> Maybe Title
mkTitle = Just . Title

----------------------------------------------------------------------------------------------------
-- Description

newtype Description = Description {unDescription :: Text}
  deriving (Show, Eq, Generic)

mkDescription :: Text -> Maybe Description
mkDescription = Just . Description

----------------------------------------------------------------------------------------------------
-- Body

newtype Body = Body {unBody :: Text}
  deriving (Show, Eq, Generic)

mkBody :: Text -> Maybe Body
mkBody = Just . Body

----------------------------------------------------------------------------------------------------
-- Tag

newtype Tag = Tag {unTag :: Text}
  deriving (Show, Eq, Generic)

mkTag :: Text -> Maybe Tag
mkTag = Just . Tag
