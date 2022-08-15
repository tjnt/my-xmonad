{-# LANGUAGE RecordWildCards #-}

module Xmobar.Plugins.SimpleIOMonitor
    ( SimpleIOMonitor (..)
    , SimpleIOMonitorOpts (..)
    , showIcon
    )
where

import           System.Console.GetOpt              (ArgDescr (ReqArg),
                                                     OptDescr (Option))
import           Text.Printf                        (printf)
import           Xmobar                             (Exec (..))
import           Xmobar.Plugins.Monitors.Common     (MConfig, Monitor, io,
                                                     mkMConfig, parseOptsWith,
                                                     parseTemplate)
import           Xmobar.Plugins.Monitors.Common.Run (runM)

type Args  = [String]
type Alias = String
type Rate  = Int

data SimpleIOMonitor =
    SimpleIOMonitor (SimpleIOMonitorOpts -> Monitor [String]) Args Alias Rate

instance Show SimpleIOMonitor where
    show (SimpleIOMonitor _ _ a _) = printf "SimpleIOMonitor (%s)" a

instance Read SimpleIOMonitor where
    readsPrec _ = undefined

instance Exec SimpleIOMonitor where
    alias (SimpleIOMonitor _ _ a _) = a
    rate  (SimpleIOMonitor _ _ _ r) = r
    start (SimpleIOMonitor f a _ r) = runM a ioMonitorConfig (runIOMonitor f) r

type FontNo = Maybe Int
type Color  = Maybe String

data SimpleIOMonitorOpts = SimpleIOMonitorOpts
    { iconPattern                      :: [Char]
    , iconFontNo                       :: FontNo
    , iconLowC, iconNormalC, iconHighC :: Color
    , iconLowV, iconHighV              :: Float
    , iconMinV, iconMaxV               :: Float
    }

defaultOpts :: SimpleIOMonitorOpts
defaultOpts = SimpleIOMonitorOpts
    { iconPattern = []
    , iconFontNo = Nothing
    , iconLowC = Nothing, iconNormalC = Nothing, iconHighC = Nothing
    , iconLowV = 0,  iconHighV = 0
    , iconMinV = 0,  iconMaxV = 0
    }

options :: [OptDescr (SimpleIOMonitorOpts -> SimpleIOMonitorOpts)]
options =
    [ Option "" ["icon-pattern"] (ReqArg (\x o -> o { iconPattern = x }) "") ""
    , Option "" ["icon-font-no"] (ReqArg (\x o -> o { iconFontNo = Just (read x) }) "") ""
    , Option "" ["icon-low-color"] (ReqArg (\x o -> o { iconLowC = Just x }) "") ""
    , Option "" ["icon-normal-color"] (ReqArg (\x o -> o { iconNormalC = Just x }) "") ""
    , Option "" ["icon-high-color"] (ReqArg (\x o -> o { iconHighC = Just x }) "") ""
    , Option "" ["icon-low-value"] (ReqArg (\x o -> o { iconLowV = read x }) "") ""
    , Option "" ["icon-high-value"] (ReqArg (\x o -> o { iconHighV = read x }) "") ""
    , Option "" ["icon-min-value"] (ReqArg (\x o -> o { iconMinV = read x }) "") ""
    , Option "" ["icon-max-value"] (ReqArg (\x o -> o { iconMaxV = read x }) "") ""
    ]

ioMonitorConfig :: IO MConfig
ioMonitorConfig = mkMConfig "<0>" (map show [0 :: Int ..])

runIOMonitor :: (SimpleIOMonitorOpts -> Monitor [String])
             -> [String] -> Monitor String
runIOMonitor f argv = do
    opts <- io $ parseOptsWith options defaultOpts argv
    f opts >>= parseTemplate

showIcon :: SimpleIOMonitorOpts -> Float -> String
showIcon SimpleIOMonitorOpts { iconPattern = [] } _ = ""
showIcon SimpleIOMonitorOpts { iconPattern = icons, .. } val =
    let icon  = choseIcon icons iconFontNo iconMinV iconMaxV val
        color = withColor iconLowC iconNormalC iconHighC
                          iconLowV iconHighV val
     in color icon

choseIcon :: String -> FontNo -> Float -> Float -> Float -> String
choseIcon []  _ _ _ _ = ""
choseIcon [p] no _ _ _ = wrapFont no [p]
choseIcon icons no mn mx val = wrapFont no [icons !! max 0 pos]
  where
    div' x y = if y /= 0 then x / y else 0
    pc = (val - mn) `div'` (mx - mn)
    pos = let len = length icons
           in pred . min len $ round (fromIntegral len * pc)

withColor :: Color -> Color -> Color
          -> Float -> Float -> Float -> (String -> String)
withColor lowc normalc highc low high val
  | high < val = wrapColor highc
  | low  < val = wrapColor normalc
  | otherwise  = wrapColor lowc

wrapFont :: FontNo -> String -> String
wrapFont (Just n) s = printf "<fn=%d>%s</fn>" n s
wrapFont Nothing s  = s

wrapColor :: Color -> String -> String
wrapColor (Just c) s = printf "<fc=%s>%s</fc>" c s
wrapColor Nothing s  = s
