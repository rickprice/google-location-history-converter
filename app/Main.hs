-- {-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Control.Applicative (many, (<|>))
import qualified Data.Text as T
import qualified Data.JsonStream.Parser as J
import NeatInterpolation (text)
-- import Data.JsonStream.Parser (parseByteString)
import Data.JsonStream.Parser (parseByteString)
-- import Data.ByteString as BS


-- import Data.ByteString.Lazy as BL
-- import Data.ByteString as BS
-- import Data.Text as TS
-- import Data.Text.Lazy as TL
-- import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
-- import Data.ByteString.UTF8 as BSU      -- from utf8-string
import Data.Text.Encoding as TSE
-- import Data.Text.Lazy.Encoding as TLE

main :: IO ()
main = do
    print ( parseByteString resultParser (TSE.encodeUtf8 jsonStreamTestString))
    -- print "finished"

jsonStreamTestString::T.Text
jsonStreamTestString =
    [text|
    {
    "took":42,
      "errors":true,
      "items": [
        {"index": {"_index":"test","_type":"type1","_id":"1","status":400,"error":"Some random error 1"}},
        {"index": {"_index":"test","_type":"type1","_id":"2","status":400,"error":"Some random error 2"}},
        {"index": {"_index":"test","_type":"type1","_id":"3","status":400,"error":"Some random error 3"}},
        {"index":{"_index":"test","_type":"type1","_id":"4","_version":2,"status":200}}
        {"index": {"_index":"test","_type":"type1","_id":"5","status":400,"error":"Some random error 4"}},
        ]
      }
    |]

-- | Result of bulk operation
resultParser :: J.Parser [(T.Text, T.Text)]
resultParser = ([] <$ J.filterI not ("errors" J..: J.bool))
              <|> many ("items" J..: J.arrayOf bulkItemError)

bulkItemError :: J.Parser (T.Text, T.Text)
bulkItemError = J.objectWithKey "index" $
    (,) <$> "_id"   J..: J.string
        <*> "error" J..: J.string
        <*  J.filterI statusError ("status" J..: J.integer)
  where
    statusError s = s < 200 || s > (299 :: Int)

