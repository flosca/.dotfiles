import XMonad
import Data.Monoid
import System.Exit

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders

import XMonad.Actions.WindowGo

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Util.Scratchpad

myTerminal      = "urxvt"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 0

myModMask       = mod4Mask
altMask         = mod1Mask

myWorkspaces    = ["1","2","3","4", "5", "6", "7", "8", "9"]

myNormalBorderColor  = "#333333"
myFocusedBorderColor = "#AFAF87"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm , xK_Return), spawn $ XMonad.terminal conf)

    -- launch Firefox
    , ((modm,               xK_f ), runOrRaise "firefox" (className =? "Firefox" <||> className =? "Firefox-bin"))

    -- launch rofi
    , ((modm,               xK_p     ), spawn "rofi -show run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_m ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_m ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    --, ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Fn keys bindings
    , ((0, xF86XK_AudioRaiseVolume ), spawn "amixer --quiet set Master 3%+")
    , ((0, xF86XK_AudioLowerVolume ), spawn "amixer --quiet set Master 3%-")
    , ((0, xF86XK_AudioMute        ), spawn "pactl list sinks | grep -q Mute:.no && pactl set-sink-mute 0 1 || pactl set-sink-mute 0 0")
    , ((0, xF86XK_AudioNext        ), spawn "xbacklight +10")
    , ((0, xF86XK_AudioPrev        ), spawn "xbacklight -10")
    , ((0, xF86XK_AudioPlay        ), spawn "xbacklight = 100") 
    , ((0, xK_Print                ), spawn "scrot")

    -- Scratchpad hotkey
    , ((modm              , xK_s     ), scratchpadSpawnAction conf)

    ]
    ++

    --
    -- mod-[1.9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by draggingmyWorkspaces
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    ]

myLayout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

scratchpadHook = scratchpadManageHook (W.RationalRect paddingLeft paddingTop width height')
  where
    height'     = 0.9
    width       = 0.9
    paddingTop  = 0.05
    paddingLeft = (1 - width) / 2

myManageHook = scratchpadHook <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "VLC"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.5     -- terminal height, 10%
    w = 0.5       -- terminal width, 100%
    t = 0.25   -- distance from top edge, 90%
    l = 0.25   -- distance from left edge, 0%

myEventHook = fullscreenEventHook

myLogHook = dynamicLogWithPP myPP

myBar = "xmobar"

myPP = defaultPP { ppCurrent = xmobarColor "#66A9BA" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#B3B3B3" "" . wrap "(" ")"
                     , ppHiddenNoWindows = xmobarColor "#B3B3B3" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     , ppLayout = xmobarColor "#B3B3B3" ""
                     , ppTitle =  xmobarColor "#B3B3B3" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     , ppSort    = fmap (. scratchpadFilterOutWorkspace) (ppSort xmobarPP)
                     }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

startup :: X ()
startup = do
          ewmhDesktopsStartup >> setWMName "LG3D"
          spawn "setxkbmap -layout 'us,ru' -option 'grp:win_space_toggle'"
          spawn "feh --bg-scale ~/.wall.jpg"

main = xmonad =<< statusBar myBar myPP toggleStrutsKey defaults

defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings    
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = noBorders $ avoidStruts myLayout,
        manageHook         = myManageHook <+> manageDocks,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = startup
    }
