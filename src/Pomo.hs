{-# LANGUAGE DeriveGeneric #-}
module Pomo where

import qualified Data.IntMap  as M
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Task
import           TaskList
import           Util

type PomoTask = Task Text Pomo

type Pomodoros = Integer
type Estimated = Pomodoros
type Completed = Pomodoros

data Pomo = Pomo {
              estimated :: Estimated,
              completed :: Completed,
              started   :: UTCTime,
              due       :: UTCTime,
              finished  :: Maybe UTCTime
            } deriving (Generic)



pomo :: Estimated -> Completed -> UTCTime -> UTCTime -> Maybe UTCTime -> Pomo
pomo e c s d f
  | e <= 0    = error "Estimated pomodoros cannot be less than zero"
  | c < 0     = error "Completed pomodoros cannot be less than zero"
  | otherwise = Pomo e c s d f

averagePerDay :: UTCTime -> Pomo -> Double
averagePerDay today p = pomos_left / days_left
  where pomos_left = fromIntegral $ remaining p
        days_left = fromIntegral $ daysLeft today p


daysLeft :: UTCTime -> Pomo -> Integer
daysLeft t p = utcDiffDays (due p) t

remaining :: Pomo -> Integer
remaining p = est - cmp
  where est = fromIntegral $ estimated p
        cmp = fromIntegral $ completed p

progress :: Pomo -> Percentage
progress p = cmp / est
  where est = fromIntegral $ estimated p
        cmp = fromIntegral $ completed p

pomoDailyList :: (Show a) => UTCTime -> TaskList a Pomo -> TaskList a Integer
pomoDailyList date (TaskList taskmap) = TaskList $ filter_zeros avg_taskmap
  where filter_zeros = M.filter (\(Task _ x)-> x < 1)
        avg_taskmap  = (fmap . fmap) (round . averagePerDay date) taskmap
