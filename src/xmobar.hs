{-# LANGUAGE RecordWildCards #-}

import           Control.Exception             (SomeException, bracket, catch)
import           Control.Monad                 (filterM, msum, unless, when)
import           Data.Function                 ((&))
import           Data.List                     (isPrefixOf, isSubsequenceOf,
                                                isSuffixOf)
import qualified Data.Map.Strict               as M
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Network.Bluetooth             (Device (..), closeClient,
                                                devices, newClient)
import           System.Directory              (listDirectory)
import           System.Environment            (getEnv)
import           System.IO.Unsafe              (unsafeDupablePerformIO)
import           Text.Printf                   (printf)
import           Theme.Theme                   (base01, base02, base03, base07,
                                                base0C, basebg, myFont)
import           Utils.Run                     (readProcess)
import           XMonad.Hooks.StatusBar.PP     (trim, wrap, xmobarAction,
                                                xmobarColor, xmobarFont)
import           Xmobar                        (Align (L), Command (Com),
                                                Config (..), Date (Date),
                                                Monitors (Battery, Brightness, Cpu, DynNetwork, Volume, Wireless),
                                                Runnable (Run),
                                                XMonadLog (UnsafeXMonadLog),
                                                XPosition (TopSize),
                                                defaultConfig, xmobar)
import           Xmobar.Plugins.SimpleIOReader (SimpleIOReader (SimpleIOReader))

homeDir :: String
homeDir = unsafeDupablePerformIO (getEnv "HOME")

xmonadDir :: String
xmonadDir = homeDir <> "/.xmonad"

xmobarActionT :: String -> String -> String -> String -> String
xmobarActionT cmd title =
    xmobarAction $ "termite --exec " <> wrap "\"" "\"" cmd
                 <> if null title then "" else " --title " <> title

data IconParam a = IconParam
    { iconPattern         :: [String]
    , iconLowV, iconHighV :: a
    , iconLowC, iconHighC :: String
    , iconMinV, iconMaxV  :: a
    }

choseIcon :: RealFrac a => [String] -> a -> a -> a -> String
choseIcon []  _ _ _ = ""
choseIcon [p] _ _ _ = xmobarFont 1 p
choseIcon ptn mn mx val = xmobarFont 1 $ ptn !! max 0 pos
  where
    pc = (val - mn) / (mn - mx)
    pos = let len = length ptn
           in pred . min len $ round (fromIntegral len * pc)

withColor :: Ord a => (a, String) -> (a, String) -> a -> (String -> String)
withColor (low, lowc) (high, highc) val
  | high <= val = xmobarColor highc ""
  | low  <= val = xmobarColor lowc ""
  | otherwise        = id

formatWithIcon :: RealFloat a => String -> IconParam a -> a -> String
formatWithIcon fmt IconParam {..} val = printf fmt (color icon) (round val :: Int)
  where
    icon  = choseIcon iconPattern iconMinV iconMaxV val
    color = withColor (iconLowV, iconLowC) (iconHighV, iconHighC) val

memUsage :: String -> IconParam Float -> IO String
memUsage fmt ip = do
    m <- M.fromList . map ((\ ln -> (head ln, read (ln !! 1) :: Float)) . words)
      . take 8 . lines <$> fileMEM
    let total = m M.! "MemTotal:"; free = m M.! "MemFree:"
        buffer = m M.! "Buffers:"; cache = m M.! "Cached:"
        available = M.findWithDefault (free + buffer + cache) "MemAvailable:" m
        used = total - available
        usedratio = used / total
    return $ formatWithIcon fmt ip (usedratio * 100)
    `catch` ((const $ return "ERR") :: SomeException -> IO String)
  where
    fileMEM = readFile "/proc/meminfo"

coreTemp :: String -> IconParam Float -> IO String
coreTemp fmt ip = maybe "ERR" (formatWithIcon fmt ip) <$> readTemp
  where
    readTemp :: IO (Maybe Float)
    readTemp = do
        subdir <- head <$> filterSubDir (matcher "hwmon" "") hwmonPath
        labels <- filterM isCoreLabel =<< filterSubDir (matcher "temp" "label") subdir
        values <- mapM (fmap (/ 1000) . read1Line . labelToInput) labels :: IO [Float]
        return $ do
            when (null values) Nothing
            Just $ sum values / fromIntegral (length values)
        `catch` ((const $ return Nothing) :: SomeException -> IO (Maybe Float))
      where
        filterSubDir m p = map ((p <> "/") <>) . filter m <$> listDirectory p
        matcher prefix suffix s = prefix `isPrefixOf` s && suffix `isSuffixOf` s
        isCoreLabel p = readFile p >>= \a -> return $ take 4 a == "Core"
        labelToInput = (++ "input") . reverse . drop 5 . reverse
        read1Line f = read . head . lines <$> readFile f
        hwmonPath = "/sys/bus/platform/devices/coretemp.0/hwmon"

wifiIcon :: IO String
wifiIcon = do
    stdout <- trim <$> readProcess "wifi" [] ""
    let icon | isSubsequenceOf "on" stdout = xmobarColor base02 "" "\xfaa8"
             | otherwise = "\xfaa9"
    return $ xmobarFont 1 icon

bluetoothIcon :: IO String
bluetoothIcon = do
    stdout <- trim <$> readProcess "bluetooth" [] ""
    let icon | isSubsequenceOf "on" stdout = xmobarColor base0C "" "\xf5ae"
             | otherwise = "\xf5b1"
    return $ xmobarFont 1 icon

deviceIcons :: IO String
deviceIcons = do
    bracket newClient closeClient $ \client -> do
        devs <- devices client
        return . concatMap (' ' :) $ mapMaybe convertIcon devs
    `catch` (const $ return "ERR" :: SomeException -> IO String)
  where
    convertIcon dev = do
        conn <- devConnected dev
        unless conn Nothing
        name <- devName dev
        icon <- devIcon dev
        Just . xmobarFont 1 . fromMaybe "\xf128"
            $ msum [ deviceIconMap1 M.!? name
                   , deviceIconMap2 M.!? icon
                   ]
    deviceIconMap1 = M.fromList
        [ ("HHKB-BT", "\xf812")
        , ("AKG K371-BT", "\xf7ca")
        , ("Bose Mini II SoundLink", "\xf9c2")
        ]
    deviceIconMap2 = M.fromList
        [ ("audio-card", "\xf5af")
        , ("input-mouse", "\xf87c")
        , ("input-keyboard", "\xf80b")
        , ("input-gaming", "\xf11b")
        ]

dunstNotifyCount :: IO String
dunstNotifyCount = do
    stdout <- trim <$> readProcess "dunstctl" ["count", "history"] ""
    let icon = case stdout of
                   ""  -> xmobarColor base01 "" "\xf861"
                   "0" -> "\xf860"
                   _   -> xmobarColor base02 "" "\xf868"
    return $ printf "%s %s" (xmobarFont 1 icon) stdout

dateAndTimeFormat :: String
dateAndTimeFormat = printf "%s %%m/%%d %%a %s %%H:%%M" calendar clock
  where
   calendar = xmobarFont 1 "\xf073"
   clock = xmobarFont 1 "\xf64f"

myAdditionalFonts :: [String]
myAdditionalFonts =
    [ "xft:RictyDiminished Nerd Font:style=Regular:size=10"
    , "xft:RictyDiminished Nerd Font:style=Regular:size=12"
    , "xft:RictyDiminished Nerd Font:style=Regular:size=20"
    ]

myCommands :: [Runnable]
myCommands =
    [ Run UnsafeXMonadLog
    , Run $ Cpu
        [ "--template", "<ipat><total>%"
        , "--Low",      "40"
        , "--High",     "85"
        , "--normal",   base03
        , "--high",     base01
        , "--width",    "3"
        , "--"
        , "--load-icon-pattern", xmobarFont 1 "\xfb19"
        ] 20
    , Run $ SimpleIOReader
        (memUsage "%s%3d%%"
          IconParam
              { iconPattern = ["\xf85a"]
              , iconLowV = 50, iconHighV = 90
              , iconLowC = base03, iconHighC = base01
              , iconMinV = 0, iconMaxV = 0
              }
        ) "memory" 20
    , Run $ SimpleIOReader
        (coreTemp "%s%3d℃"
          IconParam
            { iconPattern = ["\xf2cb","\xf2ca","\xf2c9","\xf2c8","\xf2c7"]
            , iconLowV = 40, iconHighV = 60
            , iconLowC = base03, iconHighC = base01
            , iconMinV = 20, iconMaxV = 100
            }
        ) "coretemp" 40
    , Run $ DynNetwork
        [ "--template", "<rxipat><rx>kb  <txipat><tx>kb"
        , "--Low",      "102400"
        , "--High",     "1024000"
        , "--normal",   base03
        , "--high",     base01
        , "--width",    "5"
        , "--"
        , "--rx-icon-pattern", xmobarFont 1 "\xf6d9"
        , "--tx-icon-pattern", xmobarFont 1 "\xfa51"
        ] 20
    , Run $ Brightness
        [ "--template", xmobarFont 1 "<bar>" <> "<percent>%"
        , "--bfore",    "\xf5d9\xf5da\xf5db\xf5dc\xf5dd\xf5dd\xf5de\xf5de\xf5df\xf5df"
        , "--bwidth",   "0"
        , "--width",    "3"
        , "--"
        , "-D",         "intel_backlight"
        ] 40
    , Run $ Volume "default" "Master"
        [ "--template", "<status><volume>%"
        , "--bfore",    "\xfa7e\xfa7e\xfa7f\xfa7f\xfa7f\xfa7d\xfa7d\xfa7d\xf028\xf028"
        , "--bwidth",   "0"
        , "--width",    "3"
        , "--"
        , "--on",       xmobarFont 1 "<volumebar>"
        , "--off",      xmobarFont 1 "\xfa80"
        , "--onc",      base02
        , "--offc",     base01
        ] 40
    , Run $ SimpleIOReader wifiIcon "wifi" 100
    , Run $ Wireless "wlp3s0"
        [ "--template", "<quality>%"
        , "--width",    "3"
        ] 40
    , Run $ SimpleIOReader bluetoothIcon "bluetooth" 100
    , Run $ SimpleIOReader deviceIcons "deviceicons" 100
    , Run $ Battery
        [ "--template", "<acstatus>"
        , "--bfore",    "\xf244\xf243\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf240"
        , "--bwidth",   "0"
        , "--Low",      "20"
        , "--High",     "80"
        -- , "--low",      base01
        -- , "--high",     base02
        , "--"
        , "-a",         "dunstify -a xmonad -u critical -h int:transient:1 'Battery running out !!'"
        , "-A",         "10"
        , "-o",         xmobarFont 1 "<leftbar> " <> "<left>% " <> "(<timeleft>) " <> "<watts>w"
        , "-O",         xmobarFont 1 "<leftbar> " <> "<left>% " <> xmobarFont 1 "\xf0e7 " <> "<watts>w"
        , "-i",         xmobarFont 1 "\xf1e6 " <> "<left>%"
        ] 100
    , Run $ SimpleIOReader dunstNotifyCount "dunst" 40
    , Run $ Date dateAndTimeFormat "date" 100
    , Run $ Com (xmonadDir <> "/scripts/trayer-padding-icon.sh") [] "trayerpad" 100
    ]

myTemplate :: String
myTemplate =
    xmobarFont 3 "\xe777"
    <> " %UnsafeXMonadLog% }{"
    <> concatMap (wrap " " " ")
        [ "%cpu%"             & xmobarActionT "htop -s PERCENT_CPU" "htop" "3"
        , "%memory%"          & xmobarActionT "htop -s PERCENT_MEM" "htop" "3"
        , "%coretemp%"
        , "%dynnetwork%"      & xmobarActionT "nmtui-edit" "" "3"
        , "%bright%"          & xmobarAction  "xmonadctl brightness-up" "4"
                              . xmobarAction  "xmonadctl brightness-down" "5"
        , "%default:Master%"  & xmobarAction  "xmonadctl volume-master-toggle" "1"
                              . xmobarAction  "xmonadctl volume-master-up" "4"
                              . xmobarAction  "xmonadctl volume-master-down" "5"
                              . xmobarActionT "pulsemixer" "" "3"
        , "%wifi%%wlp3s0wi%"  & xmobarAction  "xmonadctl wifi-toggle" "1"
                              . xmobarActionT "nmtui-connect" "" "3"
        , "%bluetooth%"       & (<> "%deviceicons%")
                              . xmobarAction  "xmonadctl bluetooth-toggle" "1"
                              . xmobarActionT "bluetooth-tui" "" "3"
        , "%dunst%"           & xmobarAction  "dunstctl history-pop" "1"
                              . xmobarAction  "killall dunst ; dunst" "3"
        , "%battery%"
        , "%date%"            & xmobarActionT "sh -c 'ncal -C -A1 ; \
                                              \ read -p \\\"press enter, close this.\\\" a'"
                                              "xmobar-cal" "3"
        ]
    <> "%trayerpad%"

main :: IO ()
main = xmobar $ defaultConfig
    { font = myFont
    , additionalFonts = myAdditionalFonts
    , bgColor = basebg
    , fgColor = base07
    , position = TopSize L 100 30
    , lowerOnStart = True
    , overrideRedirect = True
    , persistent = True
    , iconRoot = xmonadDir <> "/icons"
    , sepChar = "%"
    , alignSep = "}{"
    , commands = myCommands
    , template = myTemplate
    }
