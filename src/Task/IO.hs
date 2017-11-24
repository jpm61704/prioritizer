module Task.IO where

import           Data.Aeson
import           Task

instance (ToJSON a, ToJSON b) => ToJSON (Task a b) where
  toEncoding = genericToEncoding defaultOptions

instance (FromJSON a, FromJSON b) => FromJSON (Task a b)
