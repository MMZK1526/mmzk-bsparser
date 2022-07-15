{-# LANGUAGE FlexibleInstances #-}

-- Contains a uniform interface converting UTF-8 String and Text to ByteString.

module MMZK.BSParser.Convert where

import           Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString.UTF8 as BSU
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text.Encoding as TSE
import           Data.Word

class ByteStringLike s where
  toByteString :: s -> ByteString

  fromByteString :: ByteString -> s

instance ByteStringLike ByteString where
  toByteString   = id
  fromByteString = id

instance ByteStringLike [Word8] where
  toByteString   = pack
  fromByteString = unpack

instance ByteStringLike String where
  toByteString   = BSU.fromString
  fromByteString = BSU.toString

instance ByteStringLike Text where
  toByteString   = TSE.encodeUtf8
  fromByteString = TSE.decodeUtf8

toChar :: Word8 -> Char
toChar = chr . fromIntegral
{-# INLINE toChar #-}

fromChar :: Char -> Word8
fromChar = fromIntegral . ord
{-# INLINE fromChar #-}
