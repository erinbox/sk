{-# LANGUAGE PatternSynonyms #-}
import Data.Functor ((<&>))

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr a_ b_ = case a_ of
  Just _ -> a_
  Nothing -> b_

data T = S | K | T :> T deriving (Eq)
infixl 5 :>
pattern I = S :> K :> K

instance Show T where
  show t = case t of
    S -> "S"
    K -> "K"
    I -> "I"
    t1 :> t2 -> show t1 ++ case t2 of
      I -> show t2
      _ :> _ -> "(" ++ show t2 ++ ")"
      _ -> show t2

oneStep :: T -> Maybe T
oneStep t = case t of
  K :> a :> b      -> Just a
  S :> a :> b :> c -> Just $ a :> c :> (b :> c)
  a :> b           -> maybeOr (oneStep a <&> (:> b)) (oneStep b <&> (a :>))
  _                -> Nothing

-- May not terminate
reduce :: T -> [T]
reduce a = (a :) $ maybe [] reduce (oneStep a)

-- Prints:
--   SKI(KIS)
--   K(KIS)(I(KIS))
--   KIS
--   I
main :: IO ()
main = mapM_ print $ reduce (S :> K :> I :> (K :> I :> S))