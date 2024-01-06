-- {-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Control.Applicative (many, (<|>))
import qualified Data.Text as T
import qualified Data.JsonStream.Parser as J
import NeatInterpolation (text)


main :: IO ()
main = do
    print "finished"

jsonStreamTestString::T.Text
jsonStreamTestString =
    [text|
    {
    "took":42,
      "errors":true,
      "items": [
        {"index": {"_index":"test","_type":"type1","_id":"3","status":400,"error":"Some error "}},
        {"index":{"_index":"test","_type":"type1","_id":"4","_version":2,"status":200}}
        ]
      }
    |]

-- | Result of bulk operation
resultParser :: J.Parser [(T.Text, T.Text)]
resultParser =    const [] <$> J.filterI not ("errors" J..: J.bool)
              <|> many ("items" J..: J.arrayOf bulkItemError)

bulkItemError :: J.Parser (T.Text, T.Text)
bulkItemError = J.objectWithKey "index" $
    (,) <$> "_id"   J..: J.string
        <*> "error" J..: J.string
        <*  J.filterI statusError ("status" J..: J.integer)
  where
    statusError s = s < 200 || s > (299 :: Int)

