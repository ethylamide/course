{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import Course.Applicative
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> Filename -> IO (List Chars)
fastAnagrams word filename = do
  content <- readFile filename
  let dictionary = S.fromList $ hlist $ lines content
  let annagrams = S.fromList $ hlist $ permutations word
  return $ listh $ S.toList $ S.intersection annagrams dictionary

newtype NoCaseString = NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
