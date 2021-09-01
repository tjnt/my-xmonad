import           System.Environment      (getEnv)
import           System.IO.Unsafe        (unsafeDupablePerformIO)
import           Text.Printf             (printf)
import           Theme.Theme             (base01, base02, base07, base0B,
                                          basebg, myFont)
import           XMonad.Hooks.DynamicLog (wrap, xmobarAction)
import           Xmobar                  (Align (L), Command (Com), Config (..),
                                          Date (Date),
                                          Monitors (Battery, Brightness, Cpu, DynNetwork, Memory, MultiCoreTemp, Volume, Wireless),
                                          Runnable (Run),
                                          StdinReader (UnsafeStdinReader),
                                          XPosition (TopSize), defaultConfig,
                                          xmobar)

config :: Config
config =
    defaultConfig
        { font = myFont
        , additionalFonts =
                    [ "xft:RictyDiminished Nerd Font:style=Regular:size=10"
                    , "xft:RictyDiminished Nerd Font:style=Regular:size=20"
                    ]
        , bgColor = basebg
        , fgColor = base07
        , position = TopSize L 100 30
        , lowerOnStart = True
        , overrideRedirect = True
        , persistent = True
        , iconRoot = homeDir <> "/.xmonad/icons"
        , sepChar = "%"
        , alignSep = "}{"
        , template = xmobarFont 2 "\xe777"
                   <> " %UnsafeStdinReader% }{"
                   <> concatMap (wrap " " " ")
                    [ runTUI "htop -s PERCENT_CPU" "htop" "3" "%cpu%"
                    , runTUI "htop -s PERCENT_MEM" "htop" "3" "%memory%"
                    , "%multicoretemp%"
                    , runTUI "nmtui-edit" "" "3" "%dynnetwork%"
                    , xmobarAction "wifi toggle" "1" $
                      runTUI "nmtui-connect" "" "3" "%wlp3s0wi%"
                    , "%bright%"
                    , xmobarAction "amixer -q set Master toggle" "1" $
                      runTUI "pulsemixer" "" "3" "%default:Master%"
                    , "%battery%"
                    , "%date%"
                    , "%trayerpad%"
                    ]
        , commands =
            [ Run UnsafeStdinReader
            , Run $ Cpu
                [ "--template", "<ipat><total>%"
                , "--Low",      "40"
                , "--High",     "85"
                , "--normal",   base0B
                , "--high",     base01
                , "--width",    "3"
                , "--"
                , "--load-icon-pattern", xmobarFont 1 "\xfb19"
                ] 10
            , Run $ Memory
                [ "--template", "<usedipat><usedratio>%"
                , "--Low",      "40"
                , "--High",     "90"
                , "--normal",   base0B
                , "--high",     base01
                , "--width",    "3"
                , "--"
                , "--used-icon-pattern", xmobarFont 1 "\xf85a"
                ] 10
            , Run $ MultiCoreTemp
                [ "--template", xmobarFont 1 "<avgbar>" <> "<avg> ℃"
                , "--bfore",    "\xf2cb\xf2cb\xf2ca\xf2ca\xf2c9\xf2c9\xf2c8\xf2c8\xf2c7\xf2c7"
                , "--bwidth",   "0"
                , "--Low",      "40"
                , "--High",     "60"
                , "--width",    "3"
                -- , "--normal",   base0B
                -- , "--high",     base01
                ] 50
            , Run $ DynNetwork
                [ "--template", "<rxipat><rx>kb  <txipat><tx>kb"
                , "--Low",      "102400"
                , "--High",     "1024000"
                , "--normal",   base0B
                , "--high",     base01
                , "--width",    "4"
                , "--"
                , "--rx-icon-pattern", xmobarFont 1 "\xf6d9"
                , "--tx-icon-pattern", xmobarFont 1 "\xfa51"
                ] 10
            , Run $ Wireless "wlp3s0"
                [ "--template", xmobarFont 1 "<qualitybar>" <> "<quality>%"
                , "--bfore",    "\xfaa9\xfaa8\xfaa8\xfaa8\xfaa8\xfaa8\xfaa8\xfaa8\xfaa8\xfaa8"
                , "--bwidth",   "0"
                , "--width",    "3"
                ] 10
            , Run $ Brightness
                [ "--template", "<ipat><percent>%"
                , "--width",    "3"
                , "--"
                , "-D",         "intel_backlight"
                , "--brightness-icon-pattern", xmobarFont 1 "\xf5df"
                ] 10
            , Run $ Volume "default" "Master"
                [ "--template", "<status><volume>%"
                , "--width",    "3"
                , "--"
                , "--on",       xmobarFont 1 "\xfa7d"
                , "--off",      xmobarFont 1 "\xfa80"
                , "--onc",      base0B
                , "--offc",     base01
                ] 10
            , Run $ Battery
                [ "--template", "<acstatus>"
                , "--bfore",    "\xf244\xf243\xf243\xf243\xf242\xf242\xf242\xf241\xf241\xf240"
                , "--bwidth",   "0"
                , "--Low",      "20"
                , "--High",     "80"
                -- , "--low",      base01
                -- , "--high",     base02
                , "--"
                , "-a",         "notify-send -u critical 'Battery running out!!'"
                , "-A",         "10"
                , "-o",         xmobarFont 1 "<leftbar> " <> "<left>% " <> "(<timeleft>) " <> "<watts>w"
                , "-O",         xmobarFont 1 "<leftbar> " <> "<left>% " <> xmobarFont 1 "\xf0e7 " <> "<watts>w"
                , "-i",         xmobarFont 1 "\xf1e6 " <> "<left>%"
                ] 100
            , Run $ Date "%m/%d %a %H:%M" "date" 100
            , Run $ Com "trayer-padding-icon.sh" [] "trayerpad" 100
            ]
      }

runTUI :: String -> String -> String -> String -> String
runTUI cmd title = xmobarAction
                 $ "termite --exec " <> wrap "\"" "\"" cmd
                 <> if null title then "" else " --title " <> title

homeDir :: String
homeDir = unsafeDupablePerformIO (getEnv "HOME")

xmobarFont :: Int -> String -> String
xmobarFont n = wrap (printf "<fn=%d>" n) "</fn>"

main :: IO ()
main = xmobar config
