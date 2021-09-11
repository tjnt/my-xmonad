module Utils.Run
    ( spawnAndWait
    ) where

import           Control.Monad  (when)
import           System.IO      (hClose, hGetContents)
import           System.Process (runInteractiveCommand)
import           XMonad         (X, io)

spawnAndWait :: String -> X ()
spawnAndWait cmd = io $ do
    (hin, hout, herr, _) <- runInteractiveCommand cmd
    out <- hGetContents hout
    when (out == out) $ return () -- wait for exit
    hClose hin >> hClose hout >> hClose herr

-- not work this
-- spawnAndWait :: String -> X ()
-- spawnAndWait cmd = io $ callCommand cmd
