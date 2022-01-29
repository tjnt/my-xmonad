{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TupleSections #-}

import           Control.Exception                   (catch)
import           Control.Monad                       (when)
import           Data.Bifunctor                      (bimap)
import           Data.Bool                           (bool)
import           Data.List                           (intersect, sortOn)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromMaybe, mapMaybe)
import           Data.Monoid                         (All)
import           Data.Tree                           (Tree (Node))
import           GHC.IO.Exception                    (IOException)
import           Hooks.AvoidDocksFloat               (doFloat, doFullFloat,
                                                      doRectFloat)
import           Numeric                             (showFFloat)
import           Text.Printf                         (printf)
import           Theme.Theme                         (base01, base04, base06,
                                                      base0C, basebg, basefg,
                                                      myFont)
import           Utils.Dunst                         (dunstCloseAll,
                                                      dunstHistoryPop,
                                                      dunstRestart,
                                                      dunstifyIndicator,
                                                      dunstifyLow)
import           Utils.Run                           (spawnAndWait, spawnIfDown,
                                                      spawnTerminal,
                                                      spawnTerminalAndDo,
                                                      spawnTerminalOrClose,
                                                      spawnWithOutput)
import           Utils.XdgDesktopEntry
import           XMonad                              (Button, Event,
                                                      Full (Full), KeyMask,
                                                      ManageHook,
                                                      Mirror (Mirror), Query,
                                                      Window, WorkspaceId, X,
                                                      XConfig (..), asks,
                                                      button1, button2, button3,
                                                      button4, button5, cfgDir,
                                                      className, composeAll,
                                                      config, controlMask, def,
                                                      directories,
                                                      floatLocation, focus,
                                                      gets, io, mod4Mask,
                                                      mouseMoveWindow,
                                                      noModMask, refresh,
                                                      resource, sendMessage,
                                                      shiftMask, spawn,
                                                      terminal, title, windows,
                                                      windowset, withFocused,
                                                      xC_left_ptr,
                                                      xK_bracketleft, xK_q,
                                                      xmonad, (-->), (.|.),
                                                      (<&&>), (<+>), (=?))
import           XMonad.Actions.CopyWindow           (kill1)
import           XMonad.Actions.CycleSelectedLayouts (cycleThroughLayouts)
import           XMonad.Actions.CycleWS              (Direction1D (Next, Prev),
                                                      WSType (WSIs), moveTo,
                                                      shiftTo, toggleWS)
import           XMonad.Actions.FlexibleResize       (mouseResizeWindow)
import           XMonad.Actions.FloatKeys            (keysMoveWindow,
                                                      keysResizeWindow)
import           XMonad.Actions.FloatSnap            (afterDrag, snapGrow,
                                                      snapMagicMove,
                                                      snapMagicResize, snapMove,
                                                      snapShrink)
import           XMonad.Actions.Minimize             (maximizeWindowAndFocus,
                                                      minimizeWindow,
                                                      withLastMinimized)
import           XMonad.Actions.Promote              (promote)
import           XMonad.Actions.SinkAll              (sinkAll)
import           XMonad.Actions.SpawnOn              (manageSpawn,
                                                      shellPromptHere,
                                                      spawnHere)
import           XMonad.Actions.TiledWindowDragging  (dragWindow)
import           XMonad.Actions.TreeSelect           (TSConfig (..),
                                                      TSNode (..), cancel,
                                                      defaultNavigation,
                                                      treeselectAction)
import           XMonad.Actions.Warp                 (warpToWindow)
import           XMonad.Hooks.DynamicIcons           (IconConfig (..), appIcon,
                                                      dynamicIconsPP,
                                                      iconsGetFocus)
import           XMonad.Hooks.EwmhDesktops           (ewmh, ewmhFullscreen)
import           XMonad.Hooks.InsertPosition         (Focus (Newer),
                                                      Position (Below),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers          (composeOne, doCenterFloat,
                                                      isDialog, isFullscreen,
                                                      (-?>))
import           XMonad.Hooks.Minimize               (minimizeEventHook)
import           XMonad.Hooks.RefocusLast            (isFloat,
                                                      refocusLastLayoutHook,
                                                      refocusLastWhen)
import           XMonad.Hooks.ServerMode             (serverModeEventHookCmd')
import           XMonad.Hooks.StatusBar              (StatusBarConfig,
                                                      statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP           (PP (..), filterOutWsPP,
                                                      xmobarAction,
                                                      xmobarBorder, xmobarColor,
                                                      xmobarFont, xmobarPP)
import           XMonad.Hooks.ToggleHook             (runLogHook, toggleHook,
                                                      toggleHookAllNew,
                                                      willHookAllNewPP)
import           XMonad.Layout.BoringWindows         (boringWindows, focusDown,
                                                      focusMaster, focusUp)
import           XMonad.Layout.Circle                (Circle (..))
import           XMonad.Layout.Gaps                  (Direction2D (..))
import           XMonad.Layout.Grid                  (Grid (Grid))
import           XMonad.Layout.LayoutCombinators     (JumpToLayout (JumpToLayout),
                                                      (|||))
import           XMonad.Layout.Minimize              (minimize)
import           XMonad.Layout.NoBorders             (noBorders, smartBorders)
import           XMonad.Layout.Renamed               (Rename (..), renamed)
import           XMonad.Layout.ResizableTile         (MirrorResize (MirrorExpand, MirrorShrink),
                                                      ResizableTall (ResizableTall))
import           XMonad.Layout.SimplestFloat         (simplestFloat)
import           XMonad.Layout.Spacing               (Border (..), spacingRaw)
import           XMonad.Layout.ThreeColumns          (ThreeCol (ThreeColMid))
import           XMonad.Layout.ToggleLayouts         (ToggleLayout (..),
                                                      toggleLayouts)
import           XMonad.Layout.TrackFloating         (trackFloating)
import           XMonad.Prompt                       (XPConfig, XPPosition (..),
                                                      alwaysHighlight, bgColor,
                                                      fgColor, font, height,
                                                      position,
                                                      promptBorderWidth)
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Cursor                  (setDefaultCursor)
import           XMonad.Util.EZConfig                (additionalKeysP)
import           XMonad.Util.NamedScratchpad         (NamedScratchpad (NS),
                                                      NamedScratchpads,
                                                      defaultFloating,
                                                      namedScratchpadAction,
                                                      namedScratchpadManageHook,
                                                      scratchpadWorkspaceTag)
import           XMonad.Util.SpawnOnce               (spawnOnce)

-- Functions

showDigits :: (RealFloat a) => Int -> a -> String
showDigits d n = showFFloat (Just d) n ""

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
    maxV <- io $ read <$> readFile fileMax :: X Float
    curV <- io $ read <$> readFile fileCur :: X Float
    let step = maxV / 100
        minV = step * 10
        value = curV + step * fromIntegral param
        ajust = showDigits 0 (max minV $ min maxV value)
    spawnAndWait $ printf "echo %s | sudo tee %s > /dev/null" ajust fileCur
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
            msg = printf "%s volume%s" target $ bool " [mute]" "" (t=="on")
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
    w <- (!!2) . words <$> spawnWithOutput "wifi toggle"
    dunstifyLow ("wifi turn " <> w) ""

boluetoothToggle :: X ()
boluetoothToggle = do
    w <- (!!2) . words <$> spawnWithOutput "bluetooth toggle"
    dunstifyLow ("bluetooth turn " <> w) ""

clipboardHistory :: String -> X()
clipboardHistory opt = do
    clipsh <- (<> "/scripts/clip.sh") <$> asks (cfgDir . directories)
    spawnTerminal $ printf "--exec '%s %s' --title clipboard" clipsh opt

captureScreen :: X()
captureScreen = spawn $
    "scrot -s $(xdg-user-dir PICTURES)/%Y-%m-%d-%T-shot.png "
    <> "-e 'dunstify -a xmonad -u low -h int:transient:1 \"saved capture\" \"$f\" ;"
    <> "feh --title screen-capture \"$f\" &'"

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
                . convert . construct <$> readDescktopEntrys
  where
    categories = [ "WebBrowser", "AudioVideo", "Office", "Utility"
                 , "System", "Settings", "Other"
                 ]
    construct []     = M.empty
    construct (e:es) =
        let ks = maybe [] (`intersect` categories) $ desktopEntryCategories e
            k = bool (head ks) "Other" (null ks)
         in M.insertWith (\a b -> head a:b) k [e] $ construct es
    convert m = map mkCategoryNode categories
      where
        mkCategoryNode cate =
            let es = sortOn desktopEntryName $ m M.! cate
                childs = mapMaybe mkEntryNode es
             in Node (TSNode cate "" (return ())) childs
        mkEntryNode e = do
            name <- desktopEntryName e
            exec <- unwords . filter (('%' /=) . head) . words
                <$> desktopEntryExec e
            -- _ <- desktopEntryCategories e  -- ignore categories not set
            let comment = fromMaybe "" $ desktopEntryComment e
                cmd = if fromMaybe False (desktopEntryTerminal e)
                         then spawnTerminal $ printf "--exec '%s'" exec
                         else spawn exec
            Just $ Node (TSNode name comment cmd) []

myTreeSelectAction :: X ()
myTreeSelectAction = do
    appMenu <- io applicationMenu
    treeselectAction myTsConfig $ myTsMenu <> layoutMenu <> [appMenu]
  where
    myTsConfig = def
        { ts_hidechildren = True
        , ts_font         = myFont
        , ts_background   = readColor basebg "C0"
        , ts_node         = (0xff000000, readColor base0C "FF")
        , ts_nodealt      = (0xff000000, readColor base04 "FF")
        , ts_highlight    = (0xffffffff, readColor base01 "FF")
        , ts_extra        = 0xffffffff
        , ts_node_width   = 240
        , ts_node_height  = 26
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
    layoutMenu =
        [ Node (TSNode "Layout menu" "Open layout menu" (return ()))
          (map (\s -> Node (TSNode s "" (sendMessage $ JumpToLayout s)) [])
            [ "Tall" , "Mirror", "Float"
            , "Three", "Grid",   "Circle"
            ]
          )
        ]

-- scratch pad

myScratchpads :: NamedScratchpads
myScratchpads =
    [ NS "pulsemixer" (termcmd "pulsemixer" "pulsemixer") (title =? "pulsemixer") defaultFloating
    , NS "terminal" "termite --title scratch-terminal" (title =? "scratch-terminal") defaultFloating
    , NS "qalculate" "qalculate-gtk" (className =? "Qalculate-gtk") defaultFloating
    -- , NS "ytop" (termcmd "ytop" "ytop") (title =? "ytop") defaultFloating
    ]
  where
    termcmd c t = printf "termite --exec %s --title %s" c t

-- Key bindings

myKeys :: [(String, X ())]
myKeys =
    [ -- focus (BoringWindows)
      ("M-j",             focusDown)
    , ("M-k",             focusUp)
    , ("M-m",             focusMaster)
      -- swap master
    , ("M-S-m",           promote)
      -- launch
    , ("M-<Return>",      spawnTerminal "--exec tmux")
    , ("M-S-<Return>",    spawnTerminal "")  -- without tmux
    , ("M-C-<Return>",    spawnTerminalAndDo doCenterFloat "--exec=tmux")
    , ("M-C-S-<Return>",  spawnTerminalAndDo doCenterFloat "")
      -- shell prompt
    , ("M-p",             shellPromptHere myXPConfig)
      -- tree select
    , ("M-o",             myTreeSelectAction)
      -- clipboard history
    , ("M-y",             clipboardHistory "sel")
    , ("M-S-y",           clipboardHistory "del")
      -- resizing window ratio
    , ("M-u",             sendMessage MirrorExpand)
    , ("M-n",             sendMessage MirrorShrink)
      -- minimize window
    , ("M-z",             withFocused minimizeWindow)
    , ("M-x",             withLastMinimized maximizeWindowAndFocus)
      -- close window
    , ("M-c",             kill1)
      -- refresh window and warp pointer
    , ("M-r",             refresh >> warpToWindow 0.5 0.5)
      -- toggle fullscreen
    , ("M-f",             sendMessage ToggleLayout)
      -- toggle float
    , ("M-t",             toggleFloat)
      -- sink all
    , ("M-S-t",           sinkAll)
      -- move to center
    , ("M-g",             centerFloat)
      -- toggle manage hook
    , ("M-b",             sendMessage ToggleStruts)
      -- toggle manage hook
    , ("M-v",             toggleHookAllNew "insertBelow" >> runLogHook)
      -- cycle workspaces
    , ("M-a",             toggleWS)
    , ("M-d",             moveTo  Next nonNSP)
    , ("M-s",             moveTo  Prev nonNSP)
    , ("M-S-d",           shiftTo Next nonNSP)
    , ("M-S-s",           shiftTo Prev nonNSP)
      -- cycle specific layout
    , ("M-<Space>",       cycleThroughLayouts ["Tall", "Mirror", "Float"])
      -- direct layout switch
    , ("M-6",             sendMessage $ JumpToLayout "Tall")
    , ("M-7",             sendMessage $ JumpToLayout "Mirror")
    , ("M-8",             sendMessage $ JumpToLayout "Float")
    , ("M-9",             sendMessage $ JumpToLayout "Three")
    , ("M-0",             sendMessage $ JumpToLayout "Grid")
      -- float keys
    , ("M-<Up>",          withFocused $ keysMoveWindow'   (0,-10))
    , ("M-<Down>",        withFocused $ keysMoveWindow'   (0,10))
    , ("M-<Left>",        withFocused $ keysMoveWindow'   (-10,0))
    , ("M-<Right>",       withFocused $ keysMoveWindow'   (10,0))
    , ("M-S-<Up>",        withFocused $ keysResizeWindow' (0,-10) (0,0))
    , ("M-S-<Down>",      withFocused $ keysResizeWindow' (0, 10) (0,0))
    , ("M-S-<Left>",      withFocused $ keysResizeWindow' (-10,0) (0,0))
    , ("M-S-<Right>",     withFocused $ keysResizeWindow' (10,0) (0,0))
    , ("M-C-<Up>",        withFocused $ snapMove          U Nothing)
    , ("M-C-<Down>",      withFocused $ snapMove          D Nothing)
    , ("M-C-<Left>",      withFocused $ snapMove          L Nothing)
    , ("M-C-<Right>",     withFocused $ snapMove          R Nothing)
    , ("M-C-S-<Up>",      withFocused $ snapShrink        D Nothing)
    , ("M-C-S-<Down>" ,   withFocused $ snapGrow          D Nothing)
    , ("M-C-S-<Left>" ,   withFocused $ snapShrink        R Nothing)
    , ("M-C-S-<Right>",   withFocused $ snapGrow          R Nothing)
      -- dunst operation
    , ("M-[",             dunstCloseAll)
    , ("M-S-[",           dunstRestart)
    , ("M-]",             dunstHistoryPop)
      -- application launcher
    , ("M-<F2>",          spawnHere "chromium")
    , ("M-<F3>",          spawnHere "firefox")
    , ("M-<F4>",          spawnHere "firefox -private-window")
    , ("M-<F5>",          spawnHere "thunderbird")
    , ("M-<F8>",          spawnTerminal "--exec ranger")
    , ("M-<F9>",          spawnTerminalOrClose "--exec ytop --title ytop" (title =? "ytop"))
    , ("M-S-<F9>",        spawnTerminalOrClose "--exec htop --title htop" (title =? "htop"))
    , ("M-<F10>",         namedScratchpadAction myScratchpads "pulsemixer")
    , ("M-<F11>",         spawnTerminalOrClose "--exec hcalc" (title =? "hcalc"))
    , ("M-S-<F11>",       namedScratchpadAction myScratchpads "qalculate")
    , ("M-<F12>",         namedScratchpadAction myScratchpads "terminal")
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
    nonNSP = WSIs . return $ (/= scratchpadWorkspaceTag) . W.tag

-- Mouse bindings

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
    [ ((modm, button1), \w ->
            focus w >> mouseMoveWindow w >>
            afterDrag (snapMagicMove (Just 50) (Just 50) w) >>
            windows W.shiftMaster)
    , ((modm .|. shiftMask, button1), dragWindow)
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((modm, button3), \w ->
            focus w >> mouseResizeWindow w >>
            afterDrag (snapMagicResize [R,D] (Just 50) (Just 50) w) >>
            windows W.shiftMaster)
    , ((modm, button4), \_ -> windows W.swapUp)
    , ((modm, button5), \_ -> windows W.swapDown)
    ]

-- Layout Hook

myLayoutHook = refocusLastLayoutHook . trackFloating
             . avoidStruts . boringWindows
             $ toggleLayouts expand normal
  where
    tall   = minimize . smartBorders . spacing
           $ ResizableTall 1 (3/100) (1/2) []
    mirror = minimize . smartBorders . spacing
           $ Mirror (ResizableTall 1 (3/100) (1/2) [])
    float  = minimize . smartBorders
           $ simplestFloat
    three  = minimize . smartBorders . spacing
           $ ThreeColMid 1 (3/100) (1/2)
    grid   = minimize . smartBorders . spacing
           $ Grid
    circle = minimize . smartBorders
           $ Circle
    full   = minimize . noBorders
           $ Full

    normal = rename "Tall"   tall
         ||| rename "Mirror" mirror
         ||| rename "Float"  float
         ||| rename "Three"  three
         ||| rename "Grid"   grid
         ||| rename "Circle" circle
    expand = rename "Full"   full

    spacing = spacingRaw True (Border 4 4 8 8) True (Border 4 4 4 4) True
    rename s = renamed [ Replace s ]

-- Manage Hook

myManageHook :: ManageHook
myManageHook =
    toggleHook "insertBelow" (insertPosition Below Newer)
    <+> manageSpawn
    <+> composeAll
        [ className =? "Xmessage"    --> doFloat
        , className =? "mpv"         --> doFloat
        , className =? "MPlayer"     --> doFloat
        , className =? "mplayer2"    --> doFloat
        , className =? "Pavucontrol" --> doFloat
        , className =? "Peek"        --> doFloat
        , className =? "Qalculate-gtk" --> doFloat
        , className =? "Firefox" <&&> resource =? "Toolkit" --> doFloat
        , title =? "htop"            --> doFullFloat
        , title =? "ytop"            --> doRectFloat (W.RationalRect 0 0 0.5 0.6)
        , title =? "pulsemixer"      --> doRectFloat (W.RationalRect 0 0 0.5 0.4)
        , title =? "scratch-terminal"--> doRectFloat (W.RationalRect 0 0 1.0 0.4)
        , title =? "clipboard"       --> doRectFloat (W.RationalRect 0 0 0.4 1.0)
        , title =? "hcalc"           --> doRectFloat (W.RationalRect 0 0 0.4 0.4)
        , title =? "nmtui"           --> doFloat
        , title =? "nmtui-edit"      --> doFloat
        , title =? "nmtui-connect"   --> doFloat
        , title =? "screen-capture"  --> doFloat
        , isFullscreen               --> doFullFloat
        , isDialog                   --> doFloat
        ]
    <+> namedScratchpadManageHook myScratchpads

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
              <+> refocusLastWhen isFloat
              <+> minimizeEventHook
              <+> serverModeEventHookCmd' myServerModeHook

-- Startup Hook

myStartupHook :: X ()
myStartupHook = do
    setDefaultCursor xC_left_ptr
    spawnIfDown "compton -b"
    spawnIfDown "dunst"
    spawnIfDown $ printf
              "trayer --edge top --align right --widthtype request --height 31 \
               \--expand true --transparent true --alpha 0 --tint 0x%s \
               \--SetDockType true --SetPartialStrut true" $ tail basebg
    spawnOnce "feh --randomize --bg-fill $HOME/.wallpaper/*"
    spawnIfDown "xbindkeys"
    spawnIfDown "dropbox start"
    spawnIfDown "clipd"
    -- spawnOnce "nm-tray"
    -- spawnOnce "pnmixer"
    -- spawnOnce "blueman-applet"
    -- spawnOnce "conky -bd"

-- xmobar

myPP :: PP
myPP = xmobarPP
    { ppOrder           = order
    , ppCurrent         = xmobarColor base01 basebg . xmobarBorder "Bottom" base01 2
    , ppUrgent          = xmobarColor base06 basebg
    , ppVisible         = xmobarColor base04 basebg
    , ppHidden          = xmobarColor base06 basebg
    , ppHiddenNoWindows = xmobarColor base06 basebg
    , ppTitle           = xmobarColor base04 basebg
    , ppLayout          = fromMaybe "" . (layoutIcons M.!?)
    , ppOutput          = putStrLn
    , ppWsSep           = " "
    , ppSep             = "  "
    , ppExtras          = [extToggleHookPP]
    }
  where
    order (w:l:t:e:xs) = w:l:e:t:xs
    order xs           = xs

    layoutIcons = M.fromList
        [ ("Tall",   icon "layout-tall.xpm")
        , ("Mirror", icon "layout-mirror.xpm")
        , ("Float",  icon "layout-float.xpm")
        , ("Three",  icon "layout-three.xpm")
        , ("Grid",   icon "layout-grid.xpm")
        , ("Circle", icon "layout-circle.xpm")
        , ("Full",   icon "layout-full.xpm")
        ]
      where
        icon = printf "<icon=%s/>"

    extToggleHookPP = maybe (Just iconAbove) return
                  <$> willHookAllNewPP "insertBelow" (const iconBelow)
      where
        iconAbove = xmobarFont 1 "\xfa53"
        iconBelow = xmobarFont 1 "\xfa54"

myXMobar :: StatusBarConfig
myXMobar = statusBarProp "xmobar"
         . dynamicIconsPP iconConfig
         . filterOutWsPP [scratchpadWorkspaceTag] $ myPP
  where
    iconConfig :: IconConfig
    iconConfig = def
        { iconConfigIcons  = appIcons
        , iconConfigFmt    = iconsFmtReplace' (xmobarFont 1 "\xf988") concat
        , iconConfigFilter = iconsGetFocus
        }

    iconsFmtReplace' :: String -> ([String] -> String)
                     -> WorkspaceId -> [String] -> String
    iconsFmtReplace' s cat ws is =
        clickable ws $ bool (cat is) s (null is)
      where
        clickable n = xmobarAction ("xmonadctl view-workspace-" <> n) "1"

    appIcons :: Query [String]
    appIcons = composeOne
        [ className =? "Termite"     -?> appIconFont 2 "\xe795"
        , className =? "Firefox"     -?> appIconFont 2 "\xe745"
        , className =? "Chromium"    -?> appIconFont 2 "\xe743"
        , className =? "thunderbird" -?> appIconFont 1 "\xf6ed"
        , className =? "Vieb"        -?> appIconFont 1 "\xe7c5"
        , className =? "Gvim"        -?> appIconFont 1 "\xe7c5"
        , pure True                  -?> appIconFont 1 "\xfc63"
        ]
      where
        appIconFont n = appIcon . xmobarFont n

-- main

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . withSB myXMobar . docks $ def
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
