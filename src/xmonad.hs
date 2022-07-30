{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import           Control.Exception                   (catch)
import           Control.Monad                       (unless, when)
import           Data.Bifunctor                      (bimap)
import           Data.Bool                           (bool)
import           Data.List                           (intersect, isPrefixOf,
                                                      sortOn)
import qualified Data.Map                            as M
import           Data.Maybe                          (fromMaybe, mapMaybe)
import           Data.Monoid                         (All)
import           Data.Tree                           (Tree (Node))
import           GHC.IO.Exception                    (IOException)
import           Hooks.AvoidDocksFloat               (doFloat, doFullFloat,
                                                      doRectFloat)
import           Numeric                             (showFFloat)
import           System.Exit                         (exitSuccess)
import           System.IO                           (hClose, hPutStr,
                                                      openTempFile)
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
                                                      Full (Full),
                                                      IncMasterN (..), KeyMask,
                                                      KeySym, Layout,
                                                      ManageHook,
                                                      Mirror (Mirror), Query,
                                                      Resize (..), Window,
                                                      WorkspaceId, X,
                                                      XConfig (..), asks,
                                                      button1, button2, button3,
                                                      button4, button5, cfgDir,
                                                      className, composeAll,
                                                      config, controlMask, def,
                                                      directories,
                                                      floatLocation, focus,
                                                      gets, io, kill, mod4Mask,
                                                      mouseMoveWindow,
                                                      noModMask, refresh,
                                                      resource, screenWorkspace,
                                                      sendMessage, setLayout,
                                                      shiftMask, spawn, title,
                                                      whenJust, windows,
                                                      windowset, withFocused,
                                                      withUnfocused,
                                                      xC_left_ptr,
                                                      xK_bracketleft, xK_q,
                                                      xK_slash, xmonad, (-->),
                                                      (.|.), (<&&>), (<+>),
                                                      (=?))
import           XMonad.Actions.CycleSelectedLayouts (cycleThroughLayouts)
import           XMonad.Actions.CycleWS              (WSType (WSIs),
                                                      findWorkspace, moveTo,
                                                      shiftTo, toggleWS)
import           XMonad.Actions.FlexibleResize       (mouseResizeWindow)
import           XMonad.Actions.FloatKeys            (keysMoveWindow,
                                                      keysResizeWindow)
import           XMonad.Actions.FloatSnap            (afterDrag, snapGrow,
                                                      snapMagicMove,
                                                      snapMagicResize, snapMove,
                                                      snapShrink)
import           XMonad.Actions.Minimize             (maximizeWindow,
                                                      maximizeWindowAndFocus,
                                                      minimizeWindow,
                                                      withLastMinimized,
                                                      withMinimized)
import           XMonad.Actions.Promote              (promote)
import           XMonad.Actions.SpawnOn              (manageSpawn,
                                                      shellPromptHere,
                                                      spawnHere)
import           XMonad.Actions.SwapWorkspaces       (swapWithCurrent)
import           XMonad.Actions.TiledWindowDragging  (dragWindow)
import           XMonad.Actions.TreeSelect           (TSConfig (..),
                                                      TSNode (..), cancel,
                                                      defaultNavigation,
                                                      treeselectAction)
import           XMonad.Actions.Warp                 (warpToWindow)
import           XMonad.Actions.WithAll              (killAll, sinkAll, withAll)
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
import           XMonad.Prompt.XMonad                (xmonadPromptCT)
import qualified XMonad.StackSet                     as W
import           XMonad.Util.Cursor                  (setDefaultCursor)
import           XMonad.Util.EZConfig                (mkNamedKeymap)
import           XMonad.Util.NamedActions            (NamedAction,
                                                      addDescrKeys', addName,
                                                      oneName, showKm, subtitle)
import           XMonad.Util.NamedScratchpad         (NamedScratchpad (NS),
                                                      NamedScratchpads,
                                                      defaultFloating,
                                                      namedScratchpadAction,
                                                      namedScratchpadManageHook,
                                                      scratchpadWorkspaceTag)
import           XMonad.Util.NamedWindows            (getName)
import           XMonad.Util.SpawnOnce               (spawnOnce)
import           XMonad.Util.Types                   (Direction1D (Next, Prev),
                                                      Direction2D (D, L, R, U))
import           XMonad.Util.WorkspaceCompare        (getSortByIndex)

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

-- minimize window operation

restoreAllMinimized :: X ()
restoreAllMinimized = withMinimized $ mapM_ maximizeWindow

restoreSelectedMinimized :: X ()
restoreSelectedMinimized = withMinimized $ \ws -> do
    unless (null ws) $ do
        ns <- mapM (fmap show . getName) ws
        let ns' = zipWith (printf "%d:%s") [(0::Int)..] ns
            cmds = zip ns' $ map maximizeWindowAndFocus ws
         in xmonadPromptCT "select minimized" cmds myXPConfig

-- XMonad.Actions.SwapWorkspaces.swapTo
-- extension add WSType parameter
swapTo :: Direction1D -> WSType -> X ()
swapTo dir t = findWorkspace getSortByIndex dir t 1 >>= windows . swapWithCurrent

toggleInsertMode :: X()
toggleInsertMode = toggleHookAllNew "insertBelow" >> runLogHook

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
    n <- (`rem` length mode) . succ <$> io ((read <$> readFile file) `catch` handler)
    io $ n `seq` writeFile file $ show n
    spawn $ "xrandr " ++ mode !! n
  where
    handler :: IOException -> IO Int
    handler _ = return 0
    file     = "/tmp/monitor-mode"
    single   = printf "--output %s --auto --output %s --off" primary secondary
    rightof  = printf "--output %s --auto --output %s --auto --right-of %s" primary secondary primary
    leftof   = printf "--output %s --auto --output %s --auto --left-of %s" primary secondary primary
    external = printf "--output %s --off --output %s --auto" primary secondary
    mode     = [single, rightof, leftof, external]

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

bluetoothToggle :: X ()
bluetoothToggle = do
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

-- | modify XMonad.Hooks.ManageHelpers (^?)
-- q ^? x. if the result of x 'isPrefixOf' q, return True
(^?) :: (Eq a, Functor m) => m [a] -> [a] -> m Bool
q ^? x = (x `isPrefixOf`) <$> q

-- layout cycle

myLayoutsCycle :: [String]
myLayoutsCycle = ["Tall", "Mirror", "Float"]

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
            [ "Tall", "Mirror", "Float", "Three", "Grid", "Circle" ]
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

keyBindings :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
keyBindings conf =
    category "launching terminal"
    [ ("M-<Return>",     spawnTerminal "--exec tmux",                    "launch terminal")
    , ("M-S-<Return>",   spawnTerminal "",                               "launch terminal without tmux")
    , ("M-C-<Return>",   spawnTerminalAndDo doCenterFloat "--exec=tmux", "launch terminal (float)")
    , ("M-C-S-<Return>", spawnTerminalAndDo doCenterFloat "",            "launch terminal without tmux (float)")
    ] ++
    category "launching extensions"
    [ ("M-p",    shellPromptHere myXPConfig,  "launch shell prompt")
    , ("M-o",    myTreeSelectAction,          "launch tree select menu")
    , ("M-y",    clipboardHistory "sel",      "clipboard history")
    , ("M-S-y",  clipboardHistory "del",      "clipboard history (del)")
    ] ++
    category "close / full / minimize"
    [ ("M-c",    kill,                                     "close the focused window")
    , ("M-S-c",  killAll,                                  "close all windows")
    , ("M-f",    sendMessage ToggleLayout,                 "toggle fullscreen")
    , ("M-z",    withFocused minimizeWindow,               "minimize focused window")
    , ("M-S-z",  withAll minimizeWindow,                   "minimize all windows")
    , ("M-C-z",  withUnfocused minimizeWindow,             "minimize windows expect focused")
    , ("M-x",    withLastMinimized maximizeWindowAndFocus, "restore last minimized window")
    , ("M-S-x",  restoreAllMinimized,                      "restore all minimized window")
    , ("M-C-x",  restoreSelectedMinimized,                 "restore selected minimized window")
    ] ++
    category "changing layouts"
    [ ("M-<Space>",    cycleThroughLayouts myLayoutsCycle,  "next layout")
    , ("M-S-<Space>",  setLayout $ layoutHook conf,         "reset the layout")
    , ("M-r",          refresh >> warpToWindow 0.5 0.5,     "refresh windows and warp pointer")
    ] ++
    category "direct jump to layout"
    [ ("M-6",        sendMessage $ JumpToLayout "Tall",    "layout Tall")
    , ("M-7",        sendMessage $ JumpToLayout "Mirror",  "layout Mirror")
    , ("M-8",        sendMessage $ JumpToLayout "Float",   "layout Float")
    , ("M-9",        sendMessage $ JumpToLayout "Three",   "layout Three")
    , ("M-0",        sendMessage $ JumpToLayout "Grid",    "layout Grid")
    ] ++
    category "move focus up or down the window stack"
    [ ("M-j",        focusDown,    "focus down")
    , ("M-k",        focusUp,      "focus up")
    , ("M-m",        focusMaster,  "focus the master")
    , ("M-<Tab>",    focusDown,    "focus down")
    , ("M-S-<Tab>",  focusUp,      "focus up")
    ] ++
    category "modifying the window order"
    [ ("M-S-m",  promote,             "swap with the master")
    , ("M-S-j",  windows W.swapDown,  "swap with the master")
    , ("M-S-k",  windows W.swapUp,    "swap with the master")
    ] ++
    category "resizing the master/slave ratio"
    [ ("M-h",    sendMessage Shrink,        "shrink the master area")
    , ("M-l",    sendMessage Expand,        "expand the master area")
    , ("M-n",    sendMessage MirrorShrink,  "shrink the slave area")
    , ("M-u",    sendMessage MirrorExpand,  "expand the slave area")
    ] ++
    category "floating layer support"
    [ ("M-t",    toggleFloat,  "toggle float / sink the focused window")
    , ("M-S-t",  sinkAll,      "sink all floating windows")
    , ("M-g",    centerFloat,  "float the focused window and move center")
    ] ++
    category "moving floating window by key"
    [ ("M-<Up>",         withFocused $ keysMoveWindow'   (0,-10),        "move up")
    , ("M-<Down>",       withFocused $ keysMoveWindow'   (0,10),         "move down")
    , ("M-<Left>",       withFocused $ keysMoveWindow'   (-10,0),        "move left")
    , ("M-<Right>",      withFocused $ keysMoveWindow'   (10,0),         "move right")
    , ("M-S-<Up>",       withFocused $ keysResizeWindow' (0,-10) (0,0),  "resize up")
    , ("M-S-<Down>",     withFocused $ keysResizeWindow' (0, 10) (0,0),  "resize down")
    , ("M-S-<Left>",     withFocused $ keysResizeWindow' (-10,0) (0,0),  "resize left")
    , ("M-S-<Right>",    withFocused $ keysResizeWindow' (10,0) (0,0),   "resize right")
    , ("M-C-<Up>",       withFocused $ snapMove          U Nothing,      "snap move up")
    , ("M-C-<Down>",     withFocused $ snapMove          D Nothing,      "snap move down")
    , ("M-C-<Left>",     withFocused $ snapMove          L Nothing,      "snap move left")
    , ("M-C-<Right>",    withFocused $ snapMove          R Nothing,      "snap move right")
    , ("M-C-S-<Up>",     withFocused $ snapShrink        D Nothing,      "snap shrink up")
    , ("M-C-S-<Down>" ,  withFocused $ snapGrow          D Nothing,      "snap grow down")
    , ("M-C-S-<Left>" ,  withFocused $ snapShrink        R Nothing,      "snap shrink left")
    , ("M-C-S-<Right>",  withFocused $ snapGrow          R Nothing,      "snap grow right")
    ] ++
    category "change the number of windows in the master area"
    [ ("M-,",    sendMessage (IncMasterN 1),     "Increment the number of windows in the master area")
    , ("M-.",    sendMessage (IncMasterN (-1)),  "Deincrement the number of windows in the master area")
    ] ++
    category "quit, or restart"
    [ ("M-q",    restartXmonad,   "restart xmonad")
    , ("M-S-q",  io exitSuccess,  "quit xmonad")
    ] ++
    category "switching workspaces"
    [ (m ++ i,  windows $ f i,  d ++ i)
    | i <- workspaces conf
    , (f, m, d) <- [ (W.greedyView,     "M-",    "switch to workspace ")
                   , (W.shift,          "M-S-",  "move client to workspace ")
                   , (swapWithCurrent,  "M-C-",  "swap current workspace to ")
                   ]
    ] ++
    category "switching screens"
    [ (m ++ k,  screenWorkspace i >>= flip whenJust (windows . f),  d ++ show i)
    | (k, i) <- zip ["w", "e"] [0..]
    , (f, m, d) <- [ (W.view,   "M-",    "switch to screen number ")
                   , (W.shift,  "M-S-",  "move client to screen number ")
                   ]
    ] ++
    category "additional workspace operation"
    [ ("M-a",    toggleWS,             "toggle to previously workspace")
    , ("M-d",    moveTo  Next nonNSP,  "move to next workspace")
    , ("M-s",    moveTo  Prev nonNSP,  "move to previous workspace")
    , ("M-S-d",  shiftTo Next nonNSP,  "shift to next workspace")
    , ("M-S-s",  shiftTo Prev nonNSP,  "shift to previous workspace")
    , ("M-C-d",  swapTo  Next nonNSP,  "swap to next workspace")
    , ("M-C-s",  swapTo  Prev nonNSP,  "swap to previous workspace")
    ] ++
    category "toggle manage hook"
    [ ("M-b",    sendMessage ToggleStruts,  "toggle show / hide status bar")
    , ("M-v",    toggleInsertMode,          "toggle window insert mode")
    ] ++
    category "dunst operation"
    [ ("M-[",    dunstCloseAll,    "close all dunst notification")
    , ("M-S-[",  dunstRestart,     "clear all dunst notification (restart dunst)")
    , ("M-]",    dunstHistoryPop,  "show dunst notification from history")
    ] ++
    category "launch applications"
    [ ("M-<F2>",     spawnHere "chromium",                 "chromium")
    , ("M-<F3>",     spawnHere "firefox",                  "firefox")
    , ("M-<F4>",     spawnHere "firefox -private-window",  "firefox (private)")
    , ("M-<F5>",     spawnHere "thunderbird",              "thunderbird")
    , ("M-<F7>",     spawnTerminalOrClose "--exec ytop --title ytop" (title =? "ytop"), "ytop")
    , ("M-S-<F7>",   spawnTerminalOrClose "--exec htop --title htop" (title =? "htop"), "htop")
    , ("M-<F8>",     spawnTerminalOrClose "--exec nmtui" (title ^? "nmtui"), "nmtui")
    , ("M-<F9>",     namedScratchpadAction myScratchpads "pulsemixer", "pulsemixer")
    , ("M-<F10>",    spawnTerminalOrClose "--exec bluetooth-tui" (title =? "bluetooth-tui"), "bluetooth-tui")
    , ("M-<F11>",    spawnTerminalOrClose "--exec hcalc" (title =? "hcalc"), "hcalc")
    , ("M-S-<F11>",  namedScratchpadAction myScratchpads "qalculate", "qalculate")
    , ("M-<F12>",    namedScratchpadAction myScratchpads "terminal",  "terminal (scratchpad)")
    ] ++
    category "special & multimedia keys"
    [ ("<Print>",                   captureScreen,            "capture screenshot")
    , ("<XF86AudioMute>",           volumeToggle  "Master",   "toggle master volume on / off")
    , ("<XF86AudioRaiseVolume>",    volumeUp      "Master",   "master volume up")
    , ("<XF86AudioLowerVolume>",    volumeDown    "Master",   "master volume down")
    , ("<XF86AudioMicMute>",        volumeToggle  "Capture",  "toggle capture volume on / off")
    , ("S-<XF86AudioRaiseVolume>",  volumeUp      "Capture",  "capture volume up")
    , ("S-<XF86AudioLowerVolume>",  volumeDown    "Capture",  "capture volume down")
    , ("<XF86MonBrightnessUp>",     brightnessCtrl 10    >> notifyBrightnessChange,  "monitor brightness up")
    , ("<XF86MonBrightnessDown>",   brightnessCtrl (-10) >> notifyBrightnessChange,  "monitor brightness down")
    , ("<XF86Display>",             cycleMonitor ("eDP1", "HDMI2"),  "cycle monitor mode")
    , ("<XF86WLAN>",                wifiToggle,       "toggle wifi on / off")
    , ("<XF86Bluetooth>",           bluetoothToggle,  "toggle bluetooth on / off")
    ]
  where
    category nm ks =
        subtitle nm : mkNamedKeymap conf ((\(key, action, desc) -> (key, oneName (action, desc))) <$> ks)
    convert :: (Integral a, Num b) => (a,a) -> (b,b)
    convert = bimap fromIntegral fromIntegral
    keysMoveWindow' = keysMoveWindow . convert
    keysResizeWindow' d g = keysResizeWindow (convert d) (convert g)
    nonNSP = WSIs . return $ (/= scratchpadWorkspaceTag) . W.tag
    restartXmonad = spawn
        "if type xmonad; \
        \ then xmonad --recompile &&  xmonad --restart; \
        \ else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = keys conf
            $ addDescrKeys' ((modMask conf .|. shiftMask, xK_slash), showHelp) keyBindings conf
  where
    showHelp xs = addName "Show Keybindings" $ do
        path <- io $ do
            (p, h) <- openTempFile "/tmp" "xmonad-keyguide.txt"
            hPutStr h (unlines (showKm xs)) >> hClose h >> return p
        spawnTerminal $ printf "--exec 'sh -c \"less %s ; rm -f %s\"'" path path

-- Mouse bindings

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { modMask = modm } = M.fromList
    [ ((modm, button1), \w ->
            focus w >> mouseMoveWindow w >>
            afterDrag (snapMagicMove (Just 50) (Just 50) w))
    , ((modm .|. shiftMask, button1), dragWindow)
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((modm, button3), \w ->
            focus w >> mouseResizeWindow w >>
            afterDrag (snapMagicResize [R,D] (Just 50) (Just 50) w))
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
        , title ^? "nmtui"           --> doFloat
        , title =? "bluetooth-tui"   --> doFloat
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
    , ("bluetooth-toggle",      bluetoothToggle)
    , ("next-layout",           cycleThroughLayouts myLayoutsCycle)
    , ("prev-layout",           cycleThroughLayouts (reverse myLayoutsCycle))
    , ("toggle-insert-mode",    toggleInsertMode)
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
    , ppLayout          = ppLayoutIcons
    , ppOutput          = putStrLn
    , ppWsSep           = " "
    , ppSep             = "  "
    , ppExtras          = [extToggleHookPP]
    }
  where
    order (w:l:t:e:xs) = w:l:e:t:xs
    order xs           = xs

    ppLayoutIcons = clickable . fromMaybe "？" . (layoutIcons M.!?)
      where
        clickable = xmobarAction "xmonadctl next-layout" "1"
                  . xmobarAction "xmonadctl prev-layout" "3"
        layoutIcons = M.fromList
            [ ("Tall",   icon "layout-tall.xpm")
            , ("Mirror", icon "layout-mirror.xpm")
            , ("Float",  icon "layout-float.xpm")
            , ("Three",  icon "layout-three.xpm")
            , ("Grid",   icon "layout-grid.xpm")
            , ("Circle", icon "layout-circle.xpm")
            , ("Full",   icon "layout-full.xpm")
            ]
        icon = printf "<icon=%s/>"

    extToggleHookPP = fmap clickable . maybe (Just iconAbove) return
                  <$> willHookAllNewPP "insertBelow" (const iconBelow)
      where
        clickable = xmobarAction "xmonadctl toggle-insert-mode" "1"
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
        , className ^? "thunderbird" -?> appIconFont 1 "\xf6ed"
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
    , keys = myKeys
    , mouseBindings = myMouseBindings
    }
