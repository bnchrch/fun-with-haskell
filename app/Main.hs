{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
-- import Data.Monoid
import Data.IORef
import qualified Data.Text as T
import Data.Time.Clock

data MySession = EmptySession
newtype MyAppState = DummyAppState (IORef Int)

main :: IO ()
main = do
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8000 (spock spockCfg app)

-- | This is the app
app :: SpockM () MySession MyAppState ()
app = do
  get root handleRoot
  get ("hello" <//> var) handleHello
  get "time" returnTime

returnTime :: ActionCtxT ctx (WebStateM () MySession MyAppState) a
returnTime =  do
    time <- liftIO getCurrentTime
    -- return time
    text $ T.pack (show time)
    -- (getCurrentTime :: IO UTCTime) >>= (\time :: UTCTime) ->
    --   (text "show time" :: ActionCtxT ctx ...)

-- test :: IO a -> Writer String a
-- fmap test serverIO
-- wrapper >>= continuation :: m a -> (a -> m b) -> m b

-- liftIO :: IO a -> m a
-- generalizeIO

-- MoandIO -- IOable


-- instance MonadIO (RIO cfg a) where
--   liftIO wrapped =
--     -- how to wrap this in Maybe a
--     -- And later we'll have some way of converting m a -> IO a

--   runRIO config
--   m a -> IO a

-- getCurrentTime.then(time => {
--   return(text(time))
-- })

-- do
--   a <- foo
--   b <- bar a

-- const a = await foo
-- const b = await bar(a)

-- Right 42 >>= \_ ->
--   Just 43

  -- Just 43



handleRoot :: ActionCtxT ctx (WebStateM () MySession MyAppState) a
handleRoot = text "Hello World!"

handleHello :: T.Text -> ActionCtxT ctx (WebStateM () MySession MyAppState) a
handleHello name = do
    (DummyAppState ref) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
    text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))