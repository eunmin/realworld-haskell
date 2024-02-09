module Conduit.Util.Maybe (justToNothing) where

import Data.Maybe (Maybe (..))

justToNothing :: Maybe a -> Maybe ()
justToNothing Nothing = Just ()
justToNothing (Just _) = Nothing