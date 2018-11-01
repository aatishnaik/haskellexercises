module Reader where
newtype Reader t a = Reader { runReader :: t -> a }

instance Functor (Reader t) where
    fmap f hist = Reader (f . runReader hist)

instance Applicative (Reader t) where
    pure = Reader . const
    ff <*> fx = Reader $ \t -> (runReader ff t) (runReader fx t)

instance Monad (Reader t) where
    return = pure
    ma >>= f = Reader $ \t -> runReader (f (runReader ma t)) t

ask :: Reader a a
ask = Reader id