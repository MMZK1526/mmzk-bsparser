{-# LANGUAGE FlexibleInstances #-}

-- Contains interfaces for conversions between Text, ByteString and UFT-8
-- String.

module MMZK.BSParser.Convert where

import           Data.ByteString (ByteString, pack, unpack)
import qualified Data.ByteString.UTF8 as BSU
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TSE
import           Data.Word

class StringLike t where
  toString   :: t -> String

  fromString :: String -> t

instance StringLike String where
  toString   = id
  fromString = id

instance StringLike Text where
  toString   = T.unpack
  fromString = T.pack

instance StringLike ByteString where
  toString   = BSU.toString
  fromString = BSU.fromString

instance StringLike [Word8] where
  toString   = BSU.toString . pack
  fromString = unpack . BSU.fromString

class TextLike t where
  toText :: t -> Text

  fromText :: Text -> t

instance TextLike Text where
  toText   = id
  fromText = id

instance TextLike String where
  toText   = T.pack
  fromText = T.unpack

instance TextLike ByteString where
  toText   = TSE.decodeUtf8
  fromText = TSE.encodeUtf8

instance TextLike [Word8] where
  toText   = TSE.decodeUtf8 . pack
  fromText = unpack . TSE.encodeUtf8

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
{-# INLINE [2] toChar #-}

fromChar :: Char -> Word8
fromChar = fromIntegral . ord
{-# INLINE [2] fromChar #-}
