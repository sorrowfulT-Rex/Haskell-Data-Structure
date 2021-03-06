{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MMZKDS.Utilities where

import           Data.Bits (shiftL)
import           Data.Bool (bool)
import           Data.Coerce (Coercible, coerce)
import           Data.Foldable (toList)
import           Data.Maybe (isJust)


--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

idArrayList, idAVLSet, idFDQ, idMArrayList, idMHeapPQ, idMLinkedList :: String
idArrayList   = "ArrayList"
idAVLSet      = "AVLSet"
idFDQ         = "FDQ"
idMArrayList  = "MArrayList"
idMHeapPQ     = "MHeapPQ"
idMLinkedList = "MLinkedList"

idMUArrayList, idMUHeapPQ, idMULinkedList, idUArrayList :: String
idMUArrayList  = "MUArrayList"
idMUHeapPQ     = "MUHeapPQ"
idMULinkedList = "MULinkedList"
idUArrayList   = "UArrayList"
 
--------------------------------------------------------------------------------
-- Array & STArray
--------------------------------------------------------------------------------

-- | Utility Function. 
-- Returns an error indicating the length of the array has exceeds the limit.
-- For arrays indexed on @Int@, however, this is not going to happen in practice
-- since it requires more than 1024 PiB memory.
-- 
arrayLengthOverflowError :: a
arrayLengthOverflowError = error "Length of array has overflowed!"

-- | Utility Function. 
-- Takes the current length of an array and returns a larger length.
--
expandedSize :: Int -> Int
expandedSize = (1 +) . (`div` 2) . (3 *)

-- | Utility Function. 
-- Takes the needed length of an array and returns a larger
-- number as the physical length, so that some extra space is provided.
--
initialSize :: Int -> Int
initialSize = expandedSize . shiftL 1 . ceiling . logBase 2 . fromIntegral

-- | Utility Function. 
-- Returns an array out of bound error.
--
outOfBoundError :: Int -> a
outOfBoundError i
  = error $ "Index " ++ show i ++ " is out of bound!"
