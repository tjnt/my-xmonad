{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TupleSections #-}

import           Control.Exception                (catch)
import           Control.Monad                    (when)
import           Data.Bifunctor                   (bimap)
import           Data.Bool                        (bool)
import           Data.List                        (intersect, sortOn)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Monoid                      (All)
import           Data.Tree                        (Tree (Node))
import           GHC.IO.Exception                 (IOException)
import           Numeric                          (showFFloat)
import           Text.Printf                      (printf)
import           Theme.Theme                      (base01, base04, base06,
                                                   base0C, basebg, basefg,
                                                   myFont)
import           Utils.Dunst                      (dunstifyIndicator,
                                                   dunstifyLow)
import           Utils.Run                        (spawnAndWait, spawnTerminal,
                                                   spawnTerminalAndDo,
                                                   spawnWithOutput)
import           Utils.XdgDesktopEntry
import           XMonad                           (Button, Event, Full (Full),
                                                   KeyMask, ManageHook, Window,
                                                   X, XConfig (..), asks,
                                                   button1, button3, button4,
                                                   button5, className,
                                                   composeAll, config,
                                                   controlMask, def, doFloat,
                                                   floatLocation, focus, gets,
                                                   io, mod4Mask,
                                                   mouseMoveWindow, noModMask,
                                                   refresh, sendMessage,
                                                   shiftMask, spawn, title,
                                                   windows, windowset,
                                                   withFocused, xK_b,
                                                   xK_bracketleft, xK_q, xmonad,
                                                   (-->), (.|.), (<+>), (=?))
import           XMonad.Actions.CopyWindow        (kill1)
import           XMonad.Actions.CycleWS           (nextWS, prevWS, shiftToNext,
                                                   shiftToPrev, toggleWS)
import           XMonad.Actions.FlexibleResize    (mouseResizeWindow)
import           XMonad.Actions.FloatKeys         (keysMoveWindow,
                                                   keysResizeWindow)
import           XMonad.Actions.FloatSnap         (afterDrag, snapGrow,
                                                   snapMagicMove,
                                                   snapMagicResize, snapMove,
                                                   snapShrink)
import           XMonad.Actions.Minimize          (maximizeWindowAndFocus,
                                                   minimizeWindow,
                                                   withLastMinimized)
import           XMonad.Actions.SinkAll           (sinkAll)
import           XMonad.Actions.SpawnOn           (manageSpawn)
import           XMonad.Actions.TreeSelect        (TSConfig (..), TSNode (..),
                                                   cancel, defaultNavigation,
                                                   treeselectAction,
                                                   tsDefaultConfig)
import           XMonad.Actions.Warp              (warpToWindow)
import           XMonad.Hooks.DynamicLog          (PP (..), statusBar,
                                                   xmobarAction, xmobarColor,
                                                   xmobarPP)
import           XMonad.Hooks.EwmhDesktops        (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks         (docksEventHook, manageDocks)
import           XMonad.Hooks.ManageHelpers       (doCenterFloat, doFullFloat,
                                                   doRectFloat, isDialog,
                                                   isFullscreen)
import           XMonad.Hooks.Minimize            (minimizeEventHook)
import           XMonad.Hooks.ServerMode          (serverModeEventHookCmd')
import           XMonad.Layout.BoringWindows      (boringWindows, focusDown,
                                                   focusMaster, focusUp)
import           XMonad.Layout.Circle             (Circle (..))
import           XMonad.Layout.Gaps               (Direction2D (..))
import           XMonad.Layout.LayoutCombinators  (JumpToLayout (JumpToLayout),
                                                   (|||))
import           XMonad.Layout.Minimize           (minimize)
import           XMonad.Layout.MouseResizableTile (MRTMessage (..),
                                                   mouseResizableTile,
                                                   mouseResizableTileMirrored)
import           XMonad.Layout.NoBorders          (noBorders, smartBorders)
import           XMonad.Layout.Renamed            (Rename (..), renamed)
import           XMonad.Layout.SimplestFloat      (simplestFloat)
import           XMonad.Layout.Spacing            (Border (..), spacingRaw)
import           XMonad.Layout.ToggleLayouts      (ToggleLayout (..),
                                                   toggleLayouts)
import           XMonad.Prompt                    (XPConfig, XPPosition (..),
                                                   alwaysHighlight, bgColor,
                                                   fgColor, font, height,
                                                   position, promptBorderWidth)
import           XMonad.Prompt.Shell              (shellPrompt)
import qualified XMonad.StackSet                  as W
import           XMonad.Util.EZConfig             (additionalKeysP)
import           XMonad.Util.SpawnOnce            (spawnOnce)

-- Functions

centerFloat :: X ()
centerFloat = withFocused $ \win -> do
    (_, W.RationalRect _ _ w h) <- floatLocation win
    windows $ W.float win $ W.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

toggleFloat :: X ()
toggleFloat = withFocused $ \win -> do
    floats <- gets $ W.floating . windowset
    if win `M.member` floats
       then withFocused $ windows . W.sink
       else floatLocation win >>= windows . W.float win . snd

brightnessCtrl :: Int -> X ()
brightnessCtrl param = do
    maxV <- io $ read <$> readFile fileMax :: X Int
    curV <- io $ read <$> readFile fileCur :: X Int
    let step = maxV `div` 100
        minV = step * 10
        value = curV + step * param
        ajust = max minV $ min maxV value
    spawnAndWait $ printf "echo %d | sudo tee %s > /dev/null" ajust fileCur
  where
    dir = "/sys/class/backlight/intel_backlight/"
    fileMax = dir <> "max_brightness"
    fileCur = dir <> "brightness"

cycleMonitor :: (String, String) -> X ()
cycleMonitor (primary, secondary) = do
    x <- io $ (read <$> readFile file) `catch` handler :: X Int
    let x' = succ x `rem` 4
    io $ x' `seq` writeFile file $ show x'
    spawn $ "xrandr " ++
        case x' of
            0 -> single
            1 -> rightof
            2 -> leftof
            3 -> external
            _ -> single
  where
    handler :: IOException -> IO Int
    handler _ = return 0
    file     = "/tmp/monitor-mode"
    single   = printf "--output %s --auto --output %s --off" primary secondary
    rightof  = printf "--output %s --auto --output %s --auto --right-of %s" primary secondary primary
    leftof   = printf "--output %s --auto --output %s --auto --left-of %s" primary secondary primary
    external = printf "--output %s --off --output %s --auto" primary secondary

notifyVolumeChange :: String -> X ()
notifyVolumeChange target = do
    w <- words . last . lines
        <$> spawnWithOutput (printf "amixer get %s" target)
    when (length w == 6) $
        let (v, t) = (trimVol (w!!4), trimMut (w!!5))
            msg = printf "%s volume%s" target (bool " [mute]" "" (t=="on"))
         in dunstifyIndicator v msg ""
  where
    trimVol = takeWhile (/='%') . tail
    trimMut = takeWhile (/=']') . tail

notifyBrightnessChange :: X ()
notifyBrightnessChange = do
    maxV <- io $ read <$> readFile fileMax
    curV <- io $ read <$> readFile fileCur
    let v = showDigits 0 ((curV / maxV) * 100)
     in dunstifyIndicator v "brightness" ""
  where
    dir = "/sys/class/backlight/intel_backlight/"
    fileMax = dir <> "max_brightness"
    fileCur = dir <> "actual_brightness"
    showDigits :: (RealFloat a) => Int -> a -> String
    showDigits d n = showFFloat (Just d) n ""

volumeToggle, volumeUp, volumeDown :: String -> X ()
volumeToggle target =
    spawnAndWait (printf "amixer -q set %s toggle" target) >>
    notifyVolumeChange target
volumeUp target =
    spawnAndWait (printf "amixer -q set %s 10%%+" target) >>
    notifyVolumeChange target
volumeDown target =
    spawnAndWait (printf "amixer -q set %s 10%%-" target) >>
    notifyVolumeChange target

wifiToggle :: X ()
wifiToggle = do
    spawnAndWait "wifi toggle"
    w <- (!!2) . words <$> spawnWithOutput "wifi"
    dunstifyLow ("wifi turn " <> w) ""

boluetoothToggle :: X ()
boluetoothToggle = do
    spawnAndWait "bluetooth toggle"
    w <- (!!2) . words <$> spawnWithOutput "bluetooth"
    dunstifyLow ("bluetooth turn " <> w) ""

-- shell prompt

myXPConfig :: XPConfig
myXPConfig = def
    { font              = myFont
    , bgColor           = basebg
    , fgColor           = basefg
    , promptBorderWidth = 0
    , position          = Top
    , alwaysHighlight   = True
    , height            = 30
    }

-- tree select

applicationMenu :: IO (Tree (TSNode (X ())))
applicationMenu = Node (TSNode "Application Menu" "Open application menu" (return ()))
                . convert . construct template <$> readDescktopEntrys
  where
    categories = [ "WebBrowser", "AudioVideo", "Office", "Utility"
                 , "System", "Settings", "Other"
                 ]
    template = M.fromList $ map (,[]) categories
    construct m []     = m
    construct m (e:es) =
        let ks = maybe [] (`intersect` M.keys template) $ desktopEntryCategories e
            k = if null ks then "Other" else head ks
            m' = M.insertWith (\a b -> head a:b) k [e] m
         in construct m' es
    convert m = map mkCategoryNode categories
      where
        mkCategoryNode cate =
            let es = sortOn desktopEntryName $ m M.! cate
                childs = mapMaybe mkEntryNode es
             in Node (TSNode cate "" (return ())) childs
        mkEntryNode e = do
            name <- desktopEntryName e
            exec <- head . words <$> desktopEntryExec e
            -- _ <- desktopEntryCategories e  -- ignore categories set
            let comment = fromMaybe "" $ desktopEntryComment e
                cmd = maybe exec
                        (bool exec (printf "--exec '%s'" exec))
                        (desktopEntryTerminal e)
            Just $ Node (TSNode name comment (spawnTerminal cmd)) []

myTreeSelectAction :: X ()
myTreeSelectAction = do
    appMenu <- io applicationMenu
    treeselectAction myTsConfig $ myTsMenu <> [appMenu]
  where
    myTsConfig = tsDefaultConfig
        { ts_hidechildren = True
        , ts_font         = myFont
        , ts_background   = readColor basebg "C0"
        , ts_node         = (0xff000000, readColor base0C "FF")
        , ts_nodealt      = (0xff000000, readColor base04 "FF")
        , ts_highlight    = (0xffffffff, readColor base01 "FF")
        , ts_extra        = 0xffffffff
        , ts_node_width   = 280
        , ts_node_height  = 30
        , ts_originX      = 0
        , ts_originY      = 0
        , ts_indent       = 80
        , ts_navigate     = navigation
        }
      where
        readColor color alpha =
            read . (++) "0x" . (++) alpha . tail $ color
        navigation = M.union
            defaultNavigation $
            M.fromList
                [ ((noModMask, xK_q), cancel)
                , ((controlMask, xK_bracketleft), cancel)
                ]
    myTsMenu =
       [ Node (TSNode "System menu" "Open system menu" (return ()))
           [ Node (TSNode "Monitor OFF" "" (spawn "xset dpms force standby")) []
           , Node (TSNode "Standby" "" (spawn "systemctl suspend")) []
           , Node (TSNode "Hibernate" "" (spawn "systemctl hibernate")) []
           , Node (TSNode "Shutdown" "" (spawn "systemctl poweroff")) []
           , Node (TSNode "Reboot"   ""   (spawn "systemctl reboot")) []
           ]
       ]

captureScreen :: X()
captureScreen = spawn $
    "scrot -s $(xdg-user-dir PICTURES)/%Y-%m-%d-%T-shot.png "
    <> "-e 'dunstify -a xmonad -u low -h int:transient:1 \"saved capture\" \"$f\" ;"
    <> "feh --title screen-capture \"$f\" &'"

-- Key bindings

myKeys :: [(String, X ())]
myKeys =
    [ -- focus (BoringWindows)
      ("M-j",           focusDown)
    , ("M-k",           focusUp)
    , ("M-m",           focusMaster)
      -- launch
    , ("M-S-<Return>",  spawnTerminal "--exec tmux")
    , ("M-C-<Return>",  spawnTerminalAndDo doCenterFloat "--exec=tmux")
      -- shell prompt
    , ("M-p",           shellPrompt myXPConfig)
      -- tree select
    , ("M-o",           myTreeSelectAction)
      -- clipboard history
    , ("M-y",           spawnTerminal "--exec 'clip.sh sel' --title clipboard")
    , ("M-S-y",         spawnTerminal "--exec 'clip.sh del' --title clipboard")
      -- resizing window ratio
    , ("M-u",           sendMessage ShrinkSlave)
    , ("M-n",           sendMessage ExpandSlave)
      -- minimize window
    , ("M-z",           withFocused minimizeWindow)
    , ("M-x",           withLastMinimized maximizeWindowAndFocus)
      -- close window
    , ("M-c",           kill1)
      -- refresh window and warp pointer
    , ("M-r",           refresh >> warpToWindow 0.5 0.5)
      -- toggle fullscreen
    , ("M-f",           sendMessage ToggleLayout)
      -- toggle float
    , ("M-t",           toggleFloat)
      -- sink all
    , ("M-S-t",         sinkAll)
      -- move to center
    , ("M-g",           centerFloat)
      -- cycle workspaces
    , ("M-a",           toggleWS)
    , ("M-d",           nextWS)
    , ("M-s",           prevWS)
    , ("M-S-d",         shiftToNext)
    , ("M-S-s",         shiftToPrev)
      -- direct layout switch
    , ("M-6",           sendMessage $ JumpToLayout "Tall")
    , ("M-7",           sendMessage $ JumpToLayout "Mirror")
    , ("M-8",           sendMessage $ JumpToLayout "Circle")
    , ("M-9",           sendMessage $ JumpToLayout "Float")
      -- float keys
    , ("M-<Up>",        withFocused $ keysMoveWindow'   (0,-10))
    , ("M-<Down>",      withFocused $ keysMoveWindow'   (0,10))
    , ("M-<Left>",      withFocused $ keysMoveWindow'   (-10,0))
    , ("M-<Right>",     withFocused $ keysMoveWindow'   (10,0))
    , ("M-S-<Up>",      withFocused $ keysResizeWindow' (0,-10) (0,0))
    , ("M-S-<Down>",    withFocused $ keysResizeWindow' (0, 10) (0,0))
    , ("M-S-<Left>",    withFocused $ keysResizeWindow' (-10,0) (0,0))
    , ("M-S-<Right>",   withFocused $ keysResizeWindow' (10,0) (0,0))
    , ("M-C-<Up>",      withFocused $ snapMove          U Nothing)
    , ("M-C-<Down>",    withFocused $ snapMove          D Nothing)
    , ("M-C-<Left>",    withFocused $ snapMove          L Nothing)
    , ("M-C-<Right>",   withFocused $ snapMove          R Nothing)
    , ("M-C-S-<Up>",    withFocused $ snapShrink        D Nothing)
    , ("M-C-S-<Down>" , withFocused $ snapGrow          D Nothing)
    , ("M-C-S-<Left>" , withFocused $ snapShrink        R Nothing)
    , ("M-C-S-<Right>", withFocused $ snapGrow          R Nothing)
      -- screenshot
    , ("<Print>",                   captureScreen)
      -- volume control
    , ("<XF86AudioMute>",           volumeToggle  "Master")
    , ("<XF86AudioRaiseVolume>",    volumeUp      "Master")
    , ("<XF86AudioLowerVolume>",    volumeDown    "Master")
    , ("<XF86AudioMicMute>",        volumeToggle  "Capture")
    , ("S-<XF86AudioRaiseVolume>",  volumeUp      "Capture")
    , ("S-<XF86AudioLowerVolume>",  volumeDown    "Capture")
      -- brightness control
    , ("<XF86MonBrightnessUp>",     brightnessCtrl 10    >> notifyBrightnessChange)
    , ("<XF86MonBrightnessDown>",   brightnessCtrl (-10) >> notifyBrightnessChange)
      -- toggle monitor
    , ("<XF86Display>",             cycleMonitor ("eDP1", "HDMI2"))
      -- toggle wifi
    , ("<XF86WLAN>",                wifiToggle)
      -- toggle bluetooth
    , ("<XF86Bluetooth>",           boluetoothToggle)
    ]
  where
    convert :: (Integral a, Num b) => (a,a) -> (b,b)
    convert = bimap fromIntegral fromIntegral
    keysMoveWindow' = keysMoveWindow . convert
    keysResizeWindow' d g = keysResizeWindow (convert d) (convert g)

-- Mouse bindings

myMouseBindings :: XConfig l ->M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
    [ ((modm, button1), \w ->
            focus w >> mouseMoveWindow w >>
            afterDrag (snapMagicMove (Just 50) (Just 50) w))
    , ((modm .|. shiftMask, button1), \w ->
            focus w >> mouseMoveWindow w >>
            afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w))
    , ((modm, button3), \w ->
            focus w >> mouseResizeWindow w >>
            afterDrag (snapMagicResize [R,D] (Just 50) (Just 50) w))
    , ((modm, button4), \_ -> windows W.swapUp)
    , ((modm, button5), \_ -> windows W.swapDown)
    ]

-- Layout Hook

myLayoutHook = toggleLayouts expand normal
  where
    spacing = spacingRaw True (Border 2 2 4 4) True (Border 0 0 0 0) False
    tall   = boringWindows . minimize . smartBorders . spacing
           $ mouseResizableTile
    mirror = boringWindows . minimize . smartBorders . spacing
           $ mouseResizableTileMirrored
    circle = boringWindows . minimize . smartBorders
           $ Circle
    float  = boringWindows . minimize . smartBorders
           $ simplestFloat
    full   = boringWindows . minimize . noBorders
           $ Full
    normal =     renamed [ Replace "Tall"   ] tall
             ||| renamed [ Replace "Mirror" ] mirror
             ||| renamed [ Replace "Circle" ] circle
             ||| renamed [ Replace "Float"  ] float
    expand =     renamed [ Replace "Full"   ] full

-- Manage Hook

myManageHook :: ManageHook
myManageHook = manageSpawn <+> manageDocks <+> composeAll
    [ className =? "Xmessage"    --> doFloat
    , className =? "MPlayer"     --> doFloat
    , className =? "mplayer2"    --> doFloat
    , className =? "Pavucontrol" --> doFloat
    , className =? "Peek"        --> doFloat
    , title =? "htop"            --> doFloat
    , title =? "pulsemixer"      --> doRectFloat (W.RationalRect 0 0 0.5 0.4)
    , title =? "nmtui"           --> doFloat
    , title =? "nmtui-edit"      --> doFloat
    , title =? "nmtui-connect"   --> doFloat
    , title =? "screen-capture"  --> doFloat
    , title =? "clipboard"       --> doRectFloat (W.RationalRect 0 0 0.4 1.0)
    , isFullscreen               --> doFullFloat
    , isDialog                   --> doFloat
    ]

-- Event Hook

myServerModeHook :: X [(String, X ())]
myServerModeHook = return
    [ ("open-terminal",         spawnTerminal "tmux")
    , ("volume-master-toggle",  volumeToggle  "Master")
    , ("volume-master-up",      volumeUp      "Master")
    , ("volume-master-down",    volumeDown    "Master")
    , ("volume-capture-toggle", volumeToggle  "Capture")
    , ("volume-capture-up",     volumeUp      "Capture")
    , ("volume-capture-down",   volumeDown    "Capture")
    , ("wifi-toggle",           wifiToggle)
    , ("bluetooth-toggle",      boluetoothToggle)
    ]
    <> workspaceCommands
  where
    workspaceCommands :: X [(String, X ())]
    workspaceCommands = asks (workspaces . config) >>= \wss -> return
                            [ (m <> i, windows $ f i) | i <- wss
                            , (f, m) <- [ (W.greedyView, "view-workspace-")
                                        , (W.shift, "shift-workspace-") ]
                            ]

myEventHook :: Event -> X All
myEventHook = handleEventHook def
              <+> docksEventHook
              <+> fullscreenEventHook
              <+> minimizeEventHook
              <+> serverModeEventHookCmd' myServerModeHook

-- Startup Hook

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "compton -b"
    spawnOnce "dunst"
    spawnOnce $ printf
              "trayer --edge top --align right --widthtype request --height 31 \
               \--expand true --transparent true --alpha 0 --tint 0x%s \
               \--SetDockType true --SetPartialStrut true" $ tail basebg
    spawnOnce "feh --randomize --bg-fill $HOME/.wallpaper/*"
    spawnOnce "xbindkeys"
    spawnOnce "dropbox start"
    spawnOnce "clipd"
    -- spawnOnce "nm-tray"
    -- spawnOnce "pnmixer"
    -- spawnOnce "blueman-applet"
    -- spawnOnce "conky -bd"

-- xmobar

myXMobar = statusBar "xmobar"
    xmobarPP
        { ppOrder           = id
        , ppCurrent         = xmobarColor base01 basebg . clickable "●"
        , ppUrgent          = xmobarColor base06 basebg . clickable "●"
        , ppVisible         = xmobarColor base01 basebg . clickable "⦿"
        , ppHidden          = xmobarColor base06 basebg . clickable "●"
        , ppHiddenNoWindows = xmobarColor base06 basebg . clickable "○"
        , ppTitle           = xmobarColor base04 basebg
        , ppOutput          = putStrLn
        , ppWsSep           = " "
        , ppSep             = "  "
        , ppLayout          = fromMaybe "" .(iconMap M.!?)
        }
    toggleStrutsKey
  where
    clickable s n = xmobarAction ("xmonadctl view-workspace-" <> n) "1" s
    toggleStrutsKey XConfig { XMonad.modMask = m } = (m, xK_b)
    icon = printf "<icon=%s/>"
    iconMap = M.fromList
        [ ("Tall",   icon "layout-tall.xpm")
        , ("Mirror", icon "layout-mirror.xpm")
        , ("Circle", icon "layout-circle.xpm")
        , ("Float",  icon "layout-float.xpm")
        , ("Full",   icon "layout-full.xpm")
        ]

-- main

myConfig = ewmh def
    { modMask = mod4Mask
    , terminal = "termite"
    , workspaces = map show [1..5]
    , borderWidth = 4
    , normalBorderColor = base06
    , focusedBorderColor = base01
    , focusFollowsMouse = True
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , handleEventHook = myEventHook
    , startupHook = myStartupHook
    , mouseBindings = myMouseBindings
    }
    `additionalKeysP` myKeys

main :: IO ()
main = xmonad =<< myXMobar myConfig
