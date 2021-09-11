module Utils.Run
    ( spawnWithOutput
    , spawnAndWait
    , spawnTerminal
    , spawnTerminalAndDo
    ) where

import           Control.Monad          (void, when)
import           System.IO              (hClose, hGetContents)
import           System.Process         (runInteractiveCommand)
import           XMonad                 (ManageHook, X, asks, config, io, spawn,
                                         terminal)
import           XMonad.Actions.SpawnOn (spawnAndDo)

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
