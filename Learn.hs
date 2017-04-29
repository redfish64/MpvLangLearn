import Data.Text
import Control.Exception.Base
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.RWS
import Control.Monad.Reader

data MyException = MyException String deriving (Show)
instance Exception MyException 

foo  x | x < 0 =  "foo"
foo x = "fee"


doMonadOnList :: Monad m => [a] -> (a -> m b) -> (m [b])
doMonadOnList [] _ = return []
doMonadOnList (a : as) f =
   do
     b <- (f a)
     bs <- doMonadOnList as f
     return (b : bs)
                                                        
-- doMonadOnMaybe :: Monad m => Maybe a -> (a -> m b) -> (m (Maybe b))
-- doMonadOnMaybe Nothing _ = return Nothing
-- doMonadOnMaybe (Just a) f = ????

-- co: Doesn't seem possible, but strangely seems like it would work on most
--     monads... dunno.. actually it doesn't seem to work for Maybe
-- magic :: (Monad m1, Monad m2) => (m1 a) -> (a -> m2 b) -> m2 (m1 b)
-- magic m1a f =
--   (m1a >>= (\a -> return (f a)))


eithertest :: EitherT (IO ()) IO ()
eithertest =
  do
    --left $ putStrLn "Hello Left?"
    left $ putStrLn "Hello Left?"
    two <- liftIO $ ((return $ 1 + 1) :: IO Int)
    liftIO $ putStrLn ("Hello Right? " ++ (show two))
    return ()
    

doeithertest :: IO ()
doeithertest = runEitherT eithertest >>= (\x -> case x of (Left c) -> c;(Right c) -> return c)


eitherstatetest :: StateT Int (EitherT (IO ()) IO) ()
eitherstatetest =
  do
    --lift $ left $ putStrLn "Hello Left?"
    state <- get
    liftIO $ putStrLn ("State is " ++ (show state))
    two <- liftIO $ ((return $ 1 + 1) :: IO Int)
    liftIO $ putStrLn ("Hello Right? " ++ (show two))
    put two
    state <- get
    liftIO $ putStrLn ("State is " ++ (show state))
    return ()
    
doeitherstatetest :: IO ()
doeitherstatetest =
  do
    val <- runEitherT (runStateT eitherstatetest 1)
    case val of (Left c) -> c; (Right c)-> putStrLn $ "result: "++(show c)
    return ()

data X m = X { xfoo :: m ()}

xmonadTest :: X m -> m ()
xmonadTest x = xfoo x


data X2 m = Maybe m
type X2p m = Maybe m


rwsTest :: IO ()
rwsTest =
   do
     (res, state, writer) <- runRWST myMonadBro 5 6.0
     putStrLn $ "state is "++(show state)
     putStrLn $ "writer is "++(show writer)
   where
     myMonadBro :: RWST Int [String] Double IO ()
     myMonadBro =
       do
         v <- ask
         put 7.0
         tell ["my man!"]
         tell ["yo brah!"]
         let x = 1
         tell [show x]
         return ()

--data Xz = Xz { mytype :: * }
