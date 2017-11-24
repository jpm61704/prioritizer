module TaskList.IO where

import           Data.Aeson
import           Data.ByteString.Lazy
import           Task.IO
import           TaskList

toJSON :: (ToJSON a, ToJSON b) => TaskList a b -> ByteString
toJSON (TaskList tasks) = encode tasks
