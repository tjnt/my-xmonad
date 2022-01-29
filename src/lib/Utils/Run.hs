module Utils.Run
    ( spawnWithOutput
    , spawnAndWait
    , spawnTerminal
    , spawnTerminalAndDo
    , spawnOrClose
    , spawnTerminalOrClose
    , spawnIfDown
    , readProcess
    , readProcess'
    ) where

import           Control.Exception       (catch)
import           Control.Monad           (void, when)
import           Data.Functor            ((<&>))
import qualified Data.List               as L
import           GHC.IO.Exception        (IOException)
import           System.IO               (hClose, hGetContents)
import           System.Process          (readProcess, runInteractiveCommand)
import           XMonad                  (ManageHook, Query, X, ask, asks,
                                          config, gets, io, killWindow, liftX,
                                          spawn, terminal, windowset, (<&&>))
import           XMonad.Actions.SpawnOn  (spawnAndDo)
import           XMonad.Actions.WindowGo (ifWindow)
import qualified XMonad.StackSet         as W
import           XMonad.Util.Run         (runProcessWithInput)

spawnWithOutput :: String -> X String
spawnWithOutput cmd = io $ do
    (hin, hout, herr, _) <- runInteractiveCommand cmd
    out <- hGetContents hout
    when (out == out) $ return () -- wait for exit
    hClose hin >> hClose hout >> hClose herr
    return out

spawnAndWait :: String -> X ()
spawnAndWait cmd = void $ spawnWithOutput cmd

-- not work this
-- spawnAndWait :: String -> X ()
-- spawnAndWait cmd = io $ callCommand cmd

spawnTerminal :: String -> X ()
spawnTerminal args = do
    t <- asks (terminal . config)
    spawn . concat $ [t, " ", args]

spawnTerminalAndDo :: ManageHook -> String -> X ()
spawnTerminalAndDo mh args = do
    t <- asks (terminal . config)
    spawnAndDo mh . concat $ [t, " ", args]

closeHook :: ManageHook
closeHook = ask >>= liftX . killWindow >> mempty

isInCurrentWs :: Query Bool
isInCurrentWs = ask >>= \w -> liftX $ do
    L.elem w <$> gets (W.index . windowset)

closeMaybe :: X () -> Query Bool -> X ()
closeMaybe f qry = ifWindow (qry <&&> isInCurrentWs) closeHook f

spawnOrClose :: String -> Query Bool -> X ()
spawnOrClose = closeMaybe . spawn

spawnTerminalOrClose :: String -> Query Bool -> X ()
spawnTerminalOrClose = closeMaybe . spawnTerminal

spawnIfDown :: String -> X ()
spawnIfDown cmd = io $ pidof (head $ words cmd) >>= \ps -> when (null ps) $ spawn cmd

readProcess' :: FilePath -> [String] -> String -> IO (Maybe String)
readProcess' path args input = (Just <$> readProcess path args input) `catch` econst Nothing

pidof :: FilePath -> IO [Int]
pidof x = (runProcessWithInput "pidof" [x] [] <&> (map read . words)) `catch` econst []

econst :: Monad m => a -> IOException -> m a
econst = const . return
