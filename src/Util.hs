module Util
    ( getDay
    , utcDiffDays
    ) where

import           Data.Time

getDay :: UTCTime -> Day
getDay (UTCTime day _) = day

utcDiffDays :: UTCTime -> UTCTime -> Integer
utcDiffDays x y = case (getDay x, getDay y) of
  (x', y') -> diffDays x' y'
