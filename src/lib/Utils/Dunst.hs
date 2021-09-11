module Utils.Dunst
    ( Urgency (..)
    , DunstOption (..)
    , dunstDefaultOption
    , dunstifyLow
    , dunstifyNormal
    , dunstifyCritical
    , dunstifyIndicator
    , dunstify
    ) where

import           Text.Printf (printf)
import           XMonad      (X, spawn)

data Urgency = UrgencyLow
             | UrgencyNormal
             | UrgencyCritical
  deriving (Show,Eq)

data DunstOption = DunstOption
    { dunstUrgency      :: Urgency
    , dunstTransient    :: Bool
    , dunstOtherOptions :: String
    }
  deriving (Show,Eq)

dunstDefaultOption :: DunstOption
dunstDefaultOption = DunstOption
    { dunstUrgency = UrgencyLow
    , dunstTransient = False
    , dunstOtherOptions = ""
    }

dunstifyLow :: String -> String -> X ()
dunstifyLow = dunstify
    dunstDefaultOption
        { dunstUrgency = UrgencyLow
        , dunstTransient = True
        }

dunstifyNormal :: String -> String -> X ()
dunstifyNormal = dunstify
    dunstDefaultOption
        { dunstUrgency = UrgencyNormal
        , dunstTransient = False
        }

dunstifyCritical :: String -> String -> X ()
dunstifyCritical = dunstify
    dunstDefaultOption
        { dunstUrgency = UrgencyCritical
        , dunstTransient = False
        }

dunstifyIndicator :: String -> String -> String -> X ()
dunstifyIndicator val = dunstify
    dunstDefaultOption
        { dunstUrgency = UrgencyLow
        , dunstTransient = True
        , dunstOtherOptions = printf "-h int:value:%s" val
        }

dunstify :: DunstOption -> String -> String -> X ()
dunstify opt summary body = spawn $
    printf "dunstify -a xmonad %s %s %s '%s' '%s'"
        urgency transient options summary body
  where
    urgency = "-u " <> case dunstUrgency opt of
        UrgencyLow      -> "low"
        UrgencyNormal   -> "normal"
        UrgencyCritical -> "normal"
    transient = if dunstTransient opt then "-h int:transient:1" else ""
    options = dunstOtherOptions opt
