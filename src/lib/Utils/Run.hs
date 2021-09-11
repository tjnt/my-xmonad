module Utils.Run
    ( spawnWithOutput
    , spawnAndWait
    ) where

import           Control.Monad  (void, when)
import           System.IO      (hClose, hGetContents)
import           System.Process (runInteractiveCommand)
import           XMonad         (X, io)

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
