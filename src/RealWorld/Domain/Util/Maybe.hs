module RealWorld.Domain.Util.Maybe (justToNothing) where

justToNothing :: Maybe a -> Maybe ()
justToNothing Nothing = Just ()
justToNothing (Just _) = Nothing
