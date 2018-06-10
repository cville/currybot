{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

-- # to test this
-- #   $ stack build && stack ghci
-- #   ghci> :l src/RequestDoc.hs
-- #   ghci> :set -XOverloadedStrings
-- #   ghci> test "id"

-- HTTP client library
import Network.Wreq  
-- Data selection
import Control.Lens   
import Data.Aeson.Lens (_String, key)
-- JSON library
import Data.Aeson
import Data.Aeson.Types
-- Support for DeriveGeneric
import GHC.Generics
-- Support for byte strings
import qualified Data.ByteString.Lazy.Char8 as C
-- Support for text strings
import Data.Text

--  URL to Hoggle API  (Sample call: http://www.haskell.org/hoogle/?mode=json&hoogle=id&count=2)
hoogleApi = "http://www.haskell.org/hoogle"

-- Given a string referring to a function/package/etc., query Hoogle and return the result.
-- The result is either an error string or a HoogleResult with the details.
queryHoogle :: Text -> IO (Either String HoogleResult)
queryHoogle query = do
  let opts = defaults & param "mode" .~ ["json"] & param "hoogle" .~ [query] & param "count" .~ ["2"]
  r <- getWith opts hoogleApi
  let rb = r ^. responseBody 
  let decoded = eitherDecode rb :: Either String HoogleResult
  return decoded
  
-- Test runner (use in GHCi)
test :: Text -> IO ()
test query = do
  res <- queryHoogle query
  handleResult res

-- Helper to print out the result
handleResult :: Either String HoogleResult -> IO ()
handleResult (Left a) = putStrLn ("Error: " ++ a)
handleResult (Right (HoogleResult items)) = mapM_ handleItem items

handleItem :: HoogleItem -> IO ()
handleItem (HoogleItem location self docs) = putStrLn (
    "{\n" ++
    "  location: " ++ location ++ "\n" ++
    "  self:     " ++ self ++ "\n" ++
    "  docs:     " ++ (maybe "" id docs) ++ "\n" ++
    "}"
  )
    

-- A result item from Hoogle
data HoogleItem = HoogleItem {
     location:: String
     ,self :: String
     ,docs :: Maybe String
  } deriving (Generic, Show)
instance FromJSON HoogleItem
instance ToJSON HoogleItem


-- A result from Hoogle with one or more result items
data HoogleResult = HoogleResult {
  results :: [HoogleItem]
 } deriving (Generic, Show)
instance FromJSON HoogleResult
instance ToJSON HoogleResult

-- Some examples of how decoding can be done for Aeson.

-- Decode to a generic Value (using JSON library defaults)
decodedSampleAsValue = eitherDecode (C.pack sample) :: Either String Value

-- Decode to a HoogleResult (using our more specific type)
decodedSample = eitherDecode (C.pack sample) :: Either String HoogleResult

-- An example of the kind of response Hoogle might return
sample = Prelude.unlines [" {",
    " ",
    "     \"results\":[",
    "         {",
    "             \"location\":\"http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.htmlv:id\",",
    "             \"self\":\"id :: a -\x003e a\",",
    "             \"docs\":\"Identity function. \"",
    "         },",
    "         {",
    "             \"location\":\"http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Category.htmlv:id\",",
    "             \"self\":\"id :: Category cat =\x003e cat a a\",",
    "             \"docs\":\"\"",
    "         }",
    "     ],",
    "     \"version\":\"4.2.26\"",
    " ",
    " }"]