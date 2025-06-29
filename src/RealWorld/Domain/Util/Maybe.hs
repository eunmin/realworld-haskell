module RealWorld.Domain.Util.Maybe (justToNothing) where

import Relude

justToNothing :: Maybe a -> Maybe ()
justToNothing Nothing = Just ()
justToNothing (Just _) = Nothing
