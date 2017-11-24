{-# LANGUAGE DeriveGeneric #-}
module Task where



import           Data.Time
import           GHC.Generics

type Percentage = Double
type TimeLeft = Integer -- units of time left (days by default)
type Now = UTCTime

data Task a b = Task {
                    description :: a,
                    metics      :: b
                  } deriving Generic

instance (Show a) => Show (Task a b) where
  show (Task x _) = show x

instance Functor (Task a) where
  fmap f (Task x y) = Task x (f y)

{-
data Task a b c = Task {
                    progress       :: a,
                    getProgress    :: a -> Percentage,
                    updateProgress :: a -> a,
                    timing         :: b,
                    getTimeLeft    :: b -> Now -> TimeLeft,
                    description    :: c
                  }
-}
