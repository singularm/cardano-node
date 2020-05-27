module Cardano.Api.MetaData where

import           Cardano.Prelude
import           Prelude (String)

import           Control.Exception (IOException)
import qualified Control.Exception as Exception
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LC8

-- We want raw JSON -> Value -> MetaData

-- | Read any JSON as its intermediary Value type
readJSON :: FilePath -> IO (Either String Value)
readJSON fp = do
  eBs <- Exception.try $ LC8.readFile fp
  case eBs of
    Left ioEx -> return . Left $ handler ioEx
    Right bs -> return $ eitherDecode' bs
 where
  handler :: IOException -> String
  handler e = "Cardano.Api.MetaData.readJSON: " <> displayException e

-- Will be a little tricky
-- valueToMetaData :: Value -> Either ConvErr MetaData

-- Then we leverage CBOR instances to serialize MetaData & write to disk to be included in a Tx
