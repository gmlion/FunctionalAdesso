-- Examples adapted from "Monad Transformers are good, actually" by Gabriella Gonzalez
-- https://youtu.be/w9ExsWcoXPs?si=FGf1t7lH5oHWkdXf

import qualified Text.Read
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.Trans
import Control.Monad.Trans.Maybe 
-- Usually MTL (Monad Transformers Library) is imported. It aggregates and exposes several functionalities
-- using the transformers library behind the scenes.
-- MaybeT, though, is not exported there and we have to use the "lower level" transformers library

-- We use ReadMaybe, which is a polymorphic function to parse a String into the output type t as Maybe t
readInt :: IO (Maybe Int)
readInt = do
    input <- getLine
    return (Text.Read.readMaybe input)

-- Imagine we want to parse three ints consecutively, continuing only in the Just case
-- We should pattern match inside the IO do notation, but we can "merge" the two monads and
-- use bind instead

readThreeInts :: IO (Maybe (Int, Int, Int))
readThreeInts = runMaybeT maybeIO -- runMaybeT "unpack" the merged monad back to the nested monads we want
    where 
        maybeIO = do
            int0 <- MaybeT readInt
            int1 <- MaybeT readInt
            int2 <- MaybeT readInt
            return (int0, int1, int2)

-- The next example will use Alternative instead of bind
ensureInt = do
    maybeInt <- readInt
    case maybeInt of
        Nothing -> ensureInt
        Just n -> return (Just n)

ensureInt' :: IO (Maybe Int)
ensureInt' = runMaybeT loop 
    where
        loop = MaybeT readInt <|> loop
                        -- Our top level monad is IO, but the Alternative instance used here
                        -- is the one of Maybe

-- We may want to "interact" with the original monad (IO in this case)
readAndPrintTwoInts = runMaybeT maybeIO
    where
        maybeIO = do
            int0 <- MaybeT readInt
            MaybeT (fmap Just (print int0))     -- print int0 :: IO ()
                                                -- Just (print int0) :: Maybe IO ()
                                                -- fmap Just (print int0) :: IO Maybe ()
                                                -- MaybeT (fmap Just (print int0)) :: MaybeT IO ()
            int1 <- MaybeT readInt
            lift (print int1)                   -- Same as above. We "lift" the IO to MaybeT with no intermediate Maybe to manage

    
