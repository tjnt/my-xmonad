import           Text.Printf (printf)
import           Theme.Theme
import           Xmobar

config :: Config
config =
    defaultConfig
        { font = myFont
        , bgColor = basebg
        , fgColor = base07
        , position = TopSize L 100 30
        , lowerOnStart = True
        , overrideRedirect = True
        , persistent = True
        , iconRoot = "/home/tjnt/.xmonad/icons"
        , sepChar = "%"
        , alignSep = "}{"
        , template = " %UnsafeStdinReader% }{| %cpu% | %memory% | %multicoretemp% | %dynnetwork% | %wlp3s0wi% | %bright% | %default:Master% | %battery% | %date% | %trayerpad%"
        , commands =
            [ Run UnsafeStdinReader
            , Run $ Cpu
                [ "--template", "<ipat> <total>%"
                , "--Low",      "40"
                , "--High",     "85"
                , "--normal",   base0B
                , "--high",     base01
                , "--width",    "3"
                , "--"
                , "--load-icon-pattern", "<icon=cpu.xbm/>"
                ] 10
            , Run $ Memory
                [ "--template", "<usedipat> <usedratio>%"
                , "--Low",      "40"
                , "--High",     "90"
                , "--normal",   base0B
                , "--high",     base01
                , "--width",    "3"
                , "--"
                , "--used-icon-pattern", "<icon=mem.xbm/>"
                ] 10
            , Run $ MultiCoreTemp
                [ "--template", "<icon=temp.xbm/> <avg> ℃"
                , "--Low",      "40"
                , "--High",     "60"
                , "--normal",   base0B
                , "--high",     base01
                ] 50
            , Run $ DynNetwork
                [ "--template", "<rxipat> <rx>kb : <txipat> <tx>kb"
                , "--Low",      "102400"
                , "--High",     "1024000"
                , "--normal",   base0B
                , "--high",     base01
                , "--width",    "4"
                , "--"
                , "--rx-icon-pattern", "<icon=arrow_down.xbm/>"
                , "--tx-icon-pattern", "<icon=arrow_up.xbm/>"
                ] 10
            , Run $ Wireless "wlp3s0"
                [ "--template", "<icon=wifi.xbm/> <quality>%"
                , "--width",    "3"
                ] 10
            , Run $ Brightness
                [ "--template", "💡 <percent>%"
                , "--width",    "3"
                , "--"
                , "-D",         "intel_backlight"
                ] 10
            , Run $ Volume "default" "Master"
                [ "--template", "<status> <volume>%"
                , "--width",    "3"
                , "--"
                , "--on",       "🔉"
                , "--off",      "🔇"
                , "--onc",      base0B
                , "--offc",     base01
                ] 10
            , Run $ Battery
                [ "--template", "<leftipat>  <left>% <acstatus> <watts>w"
                , "--Low",      "20"
                , "--High",     "80"
                , "--low",      base01
                , "--high",     base02
                , "--"
                , "-a", "notify-send -u critical 'Battery running out!!'"
                , "-A", "10"
                , "-o",                  printf "<fc=%s>(<timeleft>)</fc>" base07
                , "-O",                  printf "<fc=%s>(charge)</fc>" base07
                , "-i",                  printf "<fc=%s>(idle)</fc>" base07
                , "--on-icon-pattern",   "<icon=power-ac.xbm/>"
                , "--off-icon-pattern",  "<icon=power-bat2.xbm/>"
                , "--idle-icon-pattern", "<icon=power-ac.xbm/>"
                ] 50
            , Run $ Date "%m/%d (%a) %H:%M" "date" 10
            , Run $ Com "trayer-padding-icon.sh" [] "trayerpad" 100
            ]
      }

main :: IO ()
main = xmobar config