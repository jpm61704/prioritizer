module TaskList where

import qualified Data.IntMap as M
import           Data.Time
import           Task

newtype TaskList a b = TaskList {
                        taskMap :: M.IntMap (Task a b) }

instance Functor (TaskList a) where
    fmap f (TaskList l) = TaskList $ fmap (fmap f) l


type DailyList a b = TaskList a b




-- I want each tasklist to have functions that operate on its data
