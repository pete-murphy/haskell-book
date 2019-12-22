{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving        #-}

data Any =
  forall a. Show a =>
            Any a

deriving instance Show Any

xs :: [Any]
xs = [Any 1, Any 'a', Any True]

main = undefined
