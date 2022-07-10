module Xmobar.Plugins.SimpleIOReader
    (SimpleIOReader (..)
    )
where

import           Text.Printf (printf)
import           Xmobar      (Exec (..))

data SimpleIOReader = SimpleIOReader (IO String) String Int

instance Show SimpleIOReader where
    show (SimpleIOReader _ a r) = printf "SimpleIOReader (%s %s)" (show a) (show r)

instance Read SimpleIOReader where
    readsPrec _ = undefined

instance Exec SimpleIOReader where
    alias (SimpleIOReader _ a _) = a
    rate  (SimpleIOReader _ _ r) = r
    run   (SimpleIOReader f _ _) = f
