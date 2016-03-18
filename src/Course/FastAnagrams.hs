{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S
import Control.Monad

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams s filename = do
  content <- readFile filename
  let dictWords = to $ lines content
  let anagrams = to $ permutations s
  return $ from $ S.intersection anagrams dictWords

to :: List Chars -> S.Set NoCaseString
to xs = S.fromList $ hlist $ NoCaseString <$> xs

from :: S.Set NoCaseString -> List Chars
from s = ncString <$> listh (S.toList s)

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare = compare `on` ncString

instance Show NoCaseString where
  show = show . ncString
