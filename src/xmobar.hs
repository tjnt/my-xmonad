import           Control.Exception              (SomeException, bracket, catch)
import           Control.Monad                  (filterM, msum, unless, when)
import qualified Data.ByteString.Char8          as BS
import           Data.Function                  ((&))
import           Data.IORef                     (IORef, newIORef, readIORef,
                                                 writeIORef)
import           Data.List                      (isPrefixOf, isSubsequenceOf,
                                                 isSuffixOf)
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Time.Clock                (UTCTime, diffUTCTime,
                                                 getCurrentTime)
import           Network.Bluetooth              (Device (..), closeClient,
                                                 devices, newClient)
import           System.Directory               (listDirectory)
import           System.Environment             (getEnv)
import           System.IO.Unsafe               (unsafeDupablePerformIO)
import           Text.Printf                    (printf)
import           Theme.Theme                    (base01, base02, base03, base07,
                                                 base0C, basebg, myFont)
import           Utils.Run                      (readProcess)
import           XMonad.Hooks.StatusBar.PP      (trim, wrap, xmobarAction,
                                                 xmobarColor, xmobarFont)
import           Xmobar                         (Align (L), Command (Com),
                                                 Config (..), Date (Date),
                                                 Monitors (Battery, Volume, Wireless),
                                                 Runnable (Run),
                                                 XMonadLog (UnsafeXMonadLog),
                                                 XPosition (TopSize),
                                                 defaultConfig, xmobar)
import           Xmobar.Plugins.Monitors.Common (Monitor, io, showDigits,
                                                 showPercentWithColors,
                                                 showWithColors, takeDigits)
import           Xmobar.Plugins.SimpleIOMonitor (SimpleIOMonitor (SimpleIOMonitor),
                                                 SimpleIOMonitorOpts (..),
                                                 showIcon)
import           Xmobar.Plugins.SimpleIOReader  (SimpleIOReader (SimpleIOReader))

homeDir :: String
homeDir = unsafeDupablePerformIO (getEnv "HOME")

xmonadDir :: String
xmonadDir = homeDir <> "/.xmonad"

xmobarActionT :: String -> String -> String -> String -> String
xmobarActionT cmd title =
    xmobarAction $ "termite --exec " <> wrap "\"" "\"" cmd
                 <> if null title then "" else " --title " <> title

cpuUsage :: SimpleIOMonitorOpts -> Monitor [String]
cpuUsage = go $ unsafeDupablePerformIO (readCpu >>= newIORef)
  where
    go :: IORef [Int] -> SimpleIOMonitorOpts -> Monitor [String]
    go ref opts = do
        diff <- io $ do
            prev <- readIORef ref
            crnt <- readCpu
            writeIORef ref crnt
            return $ zipWith (-) crnt prev
        let sd = fromIntegral $ sum diff
            div' n = if sd /= 0 then fromIntegral n / sd else 0
            totalratio = cpuTotal $ map div' diff
        val <- showPercentWithColors totalratio
        return [showIcon opts (totalratio * 100), val]
      where
        cpuTotal (u:n:s:_) = sum [u, n, s]
        cpuTotal _         = error "invalid cpu data"

    readCpu = map (maybe 0 fst . BS.readInt) . tail . BS.words
            . head . BS.lines <$> BS.readFile "/proc/stat"

memUsage :: SimpleIOMonitorOpts -> Monitor [String]
memUsage opts = io readMem >>= \m -> do
    let total = m M.! "MemTotal:"; free = m M.! "MemFree:"
        buffer = m M.! "Buffers:"; cache = m M.! "Cached:"
        available = M.findWithDefault (free + buffer + cache) "MemAvailable:" m
        used = total - available
        usedratio = used / total
    val <- showPercentWithColors usedratio
    return [showIcon opts (usedratio * 100), val]
  where
    readMem = M.fromList . map (conv . map BS.unpack . BS.words)
            . take 8 . BS.lines <$> BS.readFile "/proc/meminfo"
    conv (a:b:_) = (a, read b :: Float)
    conv _       = error "invalid mem data"

coreTemp :: SimpleIOMonitorOpts -> Monitor [String]
coreTemp opts = io readTemp >>= (\values -> do
    when (null values) $ error "read value is empty"
    let avg = sum values / fromIntegral (length values)
    val <- showWithColors (show . (round :: Float -> Int)) avg
    return [showIcon opts avg, val])
  where
    readTemp = do
        subdir <- head <$> filterSubDir (matcher "hwmon" "") hwmonPath
        labels <- filterM isCoreLabel =<< filterSubDir (matcher "temp" "label") subdir
        mapM (fmap (/ 1000) . read1Line . labelToInput) labels
      where
        filterSubDir m p = map ((p <> "/") <>) . filter m <$> listDirectory p
        matcher prefix suffix s = prefix `isPrefixOf` s && suffix `isSuffixOf` s
        isCoreLabel p = readFile p >>= \a -> return $ take 4 a == "Core"
        labelToInput = (++ "input") . reverse . drop 5 . reverse
        read1Line f = read . head . lines <$> readFile f
        hwmonPath = "/sys/bus/platform/devices/coretemp.0/hwmon"

network :: SimpleIOMonitorOpts -> Monitor [String]
network = go $ unsafeDupablePerformIO (newIORef Nothing)
  where
    go :: IORef (Maybe ((Int,Int), UTCTime))
       -> SimpleIOMonitorOpts -> Monitor [String]
    go ref opts = do
        (dr,dt) <- io $ do
            prev <- readIORef ref
            n1 <- sumValue <$> readNetwork
            t1 <- getCurrentTime
            writeIORef ref $ Just (n1,t1)
            return $ maybe (0,0) (\(n0,t0) ->
                let scx = realToFrac $ diffUTCTime t1 t0
                 in if scx > 0 then diffRate n0 n1 scx else (0,0)) prev
        let icons = iconPattern opts
            rxIcon = showIcon opts { iconPattern = [head icons] } dr
            txIcon = showIcon opts { iconPattern = [icons !! 1] } dt
        rx <- formatValue dr
        tx <- formatValue dt
        return [rxIcon, rx, txIcon, tx]
      where
        sumValue = foldr (\(_,(r,t)) (rs,ts) -> (rs + r, ts + t)) (0,0)
        diffRate (r0,t0) (r1,t1) scx = (rate r0 r1, rate t0 t1)
          where rate v0 v1 = takeDigits 2 $ fromIntegral (v1 - v0) / scx
        convUnit v
          | v < 1024**1 = (v, "B")
          | v < 1024**2 = (v / 1024**1, "K")
          | v < 1024**3 = (v / 1024**2, "M")
          | otherwise   = (v / 1024**3, "G")
        formatValue v =
            let (v',s) = convUnit v
             in (++ s) <$> showWithColors (showDigits 0) v'

    readNetwork = BS.readFile "/proc/net/dev" >>= filterDev
                . map parseLine . drop2Line
      where
        drop2Line = drop 2 . BS.lines
        parseLine = selectCols . map BS.unpack . BS.words
        selectCols cols = (init (head cols), (read (cols !! 1), read (cols !! 9)))

    filterDev = filterM (\(d,_) -> predicate d)
      where
        predicate dev
          | dev `elem` excludes = return False
          | otherwise = (`elem` ["up", "unknown"]) <$> readstate dev
        excludes = ["lo"]
        readstate d = head . lines
                  <$> readFile (printf "/sys/class/net/%s/operstate" d)

brightness :: SimpleIOMonitorOpts -> Monitor [String]
brightness opts = io readBrightness >>= \v -> do
    val <- showPercentWithColors v
    return [showIcon opts (v * 100), val]
  where
    readBrightness =
        (/) <$> read' "brightness" <*> read' "max_brightness"
        `catch` (const $ return 0 :: SomeException -> IO Float)
    read' f = read <$> readFile ("/sys/class/backlight/intel_backlight/" <> f)

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
    , Run $ SimpleIOMonitor
        cpuUsage
        [ "--template",          "<0><1>%"
        , "--width",             "3"
        , "--"
        , "--icon-pattern",      "\xfb19"
        , "--icon-font-no",      "1"
        , "--icon-normal-color", base03
        , "--icon-high-color",   base01
        , "--icon-low-value",    "40"
        , "--icon-high-value",   "80"
        ] "cpu" 20
    , Run $ SimpleIOMonitor
        memUsage
        [ "--template",          "<0><1>%"
        , "--width",             "3"
        , "--"
        , "--icon-pattern",      "\xf85a"
        , "--icon-font-no",      "1"
        , "--icon-normal-color", base03
        , "--icon-high-color",   base01
        , "--icon-low-value",    "50"
        , "--icon-high-value",   "80"
        ] "memory" 20
    , Run $ SimpleIOMonitor
        coreTemp
        [ "--template",         "<0><1>℃"
        , "--width",            "3"
        , "--"
        , "--icon-pattern",     "\xf2cb\xf2ca\xf2c9\xf2c8\xf2c7"
        , "--icon-font-no",     "1"
        , "--icon-normal-color", base03
        , "--icon-high-color",   base01
        , "--icon-low-value",   "40"
        , "--icon-high-value",  "60"
        , "--icon-min-value",   "20"
        , "--icon-max-value",   "90"
        ] "coretemp" 40
    , Run $ SimpleIOMonitor
        network
        [ "--template",          "<0><1>  <2><3>"
        , "--width",             "4"
        , "--"
        , "--icon-pattern",      "\xf6d9\xfa51"
        , "--icon-font-no",      "1"
        , "--icon-normal-color", base03
        , "--icon-high-color",   base01
        , "--icon-low-value",    "102400"
        , "--icon-high-value",   "1024000"
        ] "network" 20
    , Run $ SimpleIOMonitor
        brightness
        [ "--template",          "<0><1>%"
        , "--width",             "3"
        , "--"
        , "--icon-pattern",      "\xf5d9\xf5da\xf5db\xf5dc\xf5dd\xf5de\xf5df"
        , "--icon-font-no",      "1"
        , "--icon-normal-color", base03
        , "--icon-high-color",   base01
        , "--icon-low-value",    "50"
        , "--icon-high-value",   "80"
        , "--icon-min-value",    "0"
        , "--icon-max-value",    "100"
        ] "brightness" 40
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
        , "%network%"         & xmobarActionT "nmtui-edit" "" "3"
        , "%brightness%"      & xmobarAction  "xmonadctl brightness-up" "4"
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
