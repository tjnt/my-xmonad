module Plugins.SimpleReader
    (SimpleReader (..)
    )
where

import           Text.Printf (printf)
import           Xmobar      (Exec (..))

data SimpleReader = SimpleReader (IO String) String Int

instance Show SimpleReader where
    show (SimpleReader _ a r) = printf "SimpleReader (%s %s)" (show a) (show r)

instance Read SimpleReader where
    readsPrec _ = undefined

instance Exec SimpleReader where
    alias (SimpleReader _ a _) = a
    rate  (SimpleReader _ _ r) = r
    run   (SimpleReader f _ _) = f
