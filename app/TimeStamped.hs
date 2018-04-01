module TimeStamped where

data TimeStamped a = Stamp a Integer

instance Functor TimeStamped where
  fmap f (Stamp value time) = Stamp (f value) time

getValue :: TimeStamped a -> a
getValue (Stamp value time) = value

getTimeStamp :: TimeStamped a -> Integer
getTimeStamp (Stamp value time) = time
