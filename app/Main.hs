-- {-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
import Control.Applicative (many, (<|>))
import qualified Data.Text as T
import qualified Data.JsonStream.Parser as J
-- import Data.JsonStream.Parser ((.:), (.:?), (.|))

-- import Data.Text
-- import qualified Data.Aeson
-- import Data.ByteString.Lazy qualified as BS
-- import Data.Time.Clock
-- import Model
-- import System.Exit
-- import Prelude
-- import qualified Data.Text as T
-- import qualified Data.JsonStream.Parser as J
-- import Data.JsonStream.Parser
-- import Data.JsonStream.Parser ((.:), (.:?), (.|))




-- import Data.ByteString.Lazy as BL
-- import Data.ByteString as BS
-- import Data.Text as TS
-- import Data.Text.Lazy as TL
-- import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
-- import Data.ByteString.UTF8 as BSU      -- from utf8-string
-- import qualified Data.ByteString.Char8 as BC
-- import Data.Text.Encoding as TSE
-- import Data.Text.Lazy.Encoding as TLE

-- import Data.Time
-- import Control.Monad (mfilter)
-- import Data.ByteString qualified as BS2
-- import Control.Monad

{- | This is like the standard 'foldr' function on lists, but for 'Entries'.
 Compared to 'foldEntries' it skips failures.
-}


main :: IO ()
main = do
    print "finished"

jsonStreamTestString = "{ \
     \  \"took\":42, \
     \   \"errors\":true, \
     \    \"items\": [ \
     \      {\"index\": {\"_index\":\"test\",\"_type\":\"type1\",\"_id\":\"3\",\"status\":400,\"error\":\"Some error \"}}, \
     \      {\"index\":{\"_index\":\"test\",\"_type\":\"type1\",\"_id\":\"4\",\"_version\":2,\"status\":200}} \
     \      ] \
     \ }"

-- jsonStreamTestStringBS2 = BC.pack jsonStreamTestString


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

