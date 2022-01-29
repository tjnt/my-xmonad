import           Control.Monad             (msum, unless)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Function             ((&))
import           Data.List                 (isSubsequenceOf)
import qualified Data.Map.Strict           as M
import           Data.Maybe                (catMaybes, fromMaybe)
import           Plugins.SimpleReader      (SimpleReader (SimpleReader))
import           System.Environment        (getEnv)
import           System.IO.Unsafe          (unsafeDupablePerformIO)
import           Text.Printf               (printf)
import           Theme.Theme               (base01, base02, base03, base07,
                                            base0C, basebg, myFont)
import           Utils.Run                 (readProcess, readProcess')
import           XMonad.Hooks.StatusBar.PP (trim, wrap, xmobarAction,
                                            xmobarColor, xmobarFont)
import           Xmobar                    (Align (L), Command (Com),
                                            Config (..), Date (Date),
                                            Monitors (Battery, Brightness, Cpu, DynNetwork, Memory, MultiCoreTemp, Volume, Wireless),
                                            Runnable (Run),
                                            XMonadLog (UnsafeXMonadLog),
                                            XPosition (TopSize), defaultConfig,
                                            xmobar)

config :: Config
config =
    defaultConfig
        { font = myFont
        , additionalFonts =
            [ "xft:RictyDiminished Nerd Font:style=Regular:size=10"
            , "xft:RictyDiminished Nerd Font:style=Regular:size=12"
            , "xft:RictyDiminished Nerd Font:style=Regular:size=20"
            ]
        , bgColor = basebg
        , fgColor = base07
        , position = TopSize L 100 30
        , lowerOnStart = True
        , overrideRedirect = True
        , persistent = True
        , iconRoot = xmonadDir <> "/icons"
        , sepChar = "%"
        , alignSep = "}{"
        , template =
            xmobarFont 3 "\xe777"
            <> " %UnsafeXMonadLog% }{"
            <> concatMap (wrap " " " ")
                [ "%cpu%" & runTerminal "htop -s PERCENT_CPU" "htop" "3"
                , "%memory%" & runTerminal "htop -s PERCENT_MEM" "htop" "3"
                , "%multicoretemp%"
                , "%dynnetwork%" & runTerminal "nmtui-edit" "" "3"
                , "%bright%"
                , "%default:Master%" & xmobarAction "xmonadctl volume-master-toggle" "1"
                                     . runTerminal "pulsemixer" "" "3"
                , "%wifi%%wlp3s0wi%" & xmobarAction "xmonadctl wifi-toggle" "1"
                                     . runTerminal "nmtui-connect" "" "3"
                , "%bluetooth%" & (<> "%deviceicons%")
                                . xmobarAction "xmonadctl bluetooth-toggle" "1"
                                . xmobarAction "blueman-manager" "3"
                , "%dunst%" & xmobarAction "dunstctl history-pop" "1"
                            . xmobarAction "killall dunst ; dunst" "3"
                , "%battery%"
                , "%date%"
                ]
            <> "%trayerpad%"
        , commands =
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
                ] 10
            , Run $ Memory
                [ "--template", "<usedipat><usedratio>%"
                , "--Low",      "40"
                , "--High",     "90"
                , "--normal",   base03
                , "--high",     base01
                , "--width",    "3"
                , "--"
                , "--used-icon-pattern", xmobarFont 1 "\xf85a"
                ] 10
            , Run $ MultiCoreTemp
                [ "--template", xmobarFont 1 "<avgbar>" <> "<avg>℃"
                , "--bfore",    "\xf2cb\xf2cb\xf2ca\xf2ca\xf2c9\xf2c9\xf2c8\xf2c8\xf2c7\xf2c7"
                , "--bwidth",   "0"
                , "--Low",      "40"
                , "--High",     "60"
                , "--width",    "3"
                -- , "--normal",   base03
                -- , "--high",     base01
                ] 50
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
                ] 10
            , Run $ Brightness
                [ "--template", xmobarFont 1 "<bar>" <> "<percent>%"
                , "--bfore",    "\xf5d9\xf5da\xf5db\xf5dc\xf5dd\xf5dd\xf5de\xf5de\xf5df\xf5df"
                , "--bwidth",   "0"
                , "--width",    "3"
                , "--"
                , "-D",         "intel_backlight"
                ] 20
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
                ] 20
            , Run $ SimpleReader wifiIcon "wifi" 100
            , Run $ Wireless "wlp3s0"
                [ "--template", "<quality>%"
                , "--width",    "3"
                ] 10
            , Run $ SimpleReader bluetoothIcon "bluetooth" 100
            , Run $ SimpleReader deviceIcons "deviceicons" 100
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
            , Run $ SimpleReader dunstNotifyCount "dunst" 50
            , Run $ Date "%m/%d %a %H:%M" "date" 100
            , Run $ Com (xmonadDir <> "/scripts/trayer-padding-icon.sh") [] "trayerpad" 100
            ]
      }

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

data DeviceInfo = DeviceInfo
    { devConnected :: Bool
    , devName      :: String
    , devIcon      :: String
    }
  deriving (Eq, Show)

deviceIcons :: IO String
deviceIcons = do
    devices <- map (replace ':' '_' . (!! 1) . words) . lines
        <$> readProcess "bluetoothctl" ["paired-devices"] ""
    res <- catMaybes <$> mapM getDeviceInfo devices
    return . concatMap ((' ' :) . convertIcon) $ res
  where
    replace a b = map (\x -> if x == a then b else x)

    getDeviceInfo :: String -> IO (Maybe DeviceInfo)
    getDeviceInfo addr = runMaybeT $ do
        conn <- ("true" `isSubsequenceOf`) <$>
            MaybeT (readProcess' "dbus-send" (dbusArgs addr "Connected") "")
        unless conn $ (MaybeT . return) Nothing
        name <- dbusTrimValue <$> MaybeT (readProcess' "dbus-send" (dbusArgs addr "Name") "")
        icon <- dbusTrimValue <$> MaybeT (readProcess' "dbus-send" (dbusArgs addr "Icon") "")
        return DeviceInfo { devConnected = conn, devName = name, devIcon = icon }
      where
        dbusArgs s p =
            [ "--print-reply=literal"
            , "--system"
            , "--dest=org.bluez"
            , "/org/bluez/hci0/dev_" <> s
            , "org.freedesktop.DBus.Properties.Get"
            , "string:org.bluez.Device1"
            , "string:" <> p
            ]
        dbusTrimValue = unwords . tail . words

    convertIcon dev = xmobarFont 1 . fromMaybe "\xf128"
                    $ msum [ devName dev `M.lookup` deviceIconMap1
                           , devIcon dev `M.lookup` deviceIconMap2
                           ]
      where
        deviceIconMap1 :: M.Map String String
        deviceIconMap1 = M.fromList
            [ ("HHKB-BT", "\xf812")
            , ("AKG K371-BT", "\xf7ca")
            , ("Bose Mini II SoundLink", "\xf9c2")
            ]
        deviceIconMap2 :: M.Map String String
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

runTerminal :: String -> String -> String -> String -> String
runTerminal cmd title =
    xmobarAction $ "termite --exec " <> wrap "\"" "\"" cmd
                 <> if null title then "" else " --title " <> title

homeDir :: String
homeDir = unsafeDupablePerformIO (getEnv "HOME")

xmonadDir :: String
xmonadDir = homeDir <> "/.xmonad"

main :: IO ()
main = xmobar config
