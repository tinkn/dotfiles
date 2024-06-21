import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Run

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import XMonad.Layout.Magnifier
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns

import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.ManageHook -- scratchpad
import XMonad.Util.NamedScratchpad -- scratchpad
import qualified XMonad.StackSet as W -- does something important i'm sure.

import XMonad.Actions.CycleWS (nextScreen, shiftNextScreen, prevScreen, shiftPrevScreen)

main :: IO ()
main = do
        xmproc <- spawnPipe "~/.config/.fehbg"
        xmproc <- spawnPipe "~/.config/.screenlayout.sh"
        -- xmproc <- spawnPipe "xmodmap ~/.config/.Xmodmap"
	xmproc <- spawnPipe "pipewire"
        xmonad $ ewmhFullscreen $ ewmh $ withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey $ docks $ myConfig

myConfig =
        def
                { modMask = mod4Mask
                , layoutHook = spacingWithEdge 5 $ myLayout
                , manageHook = myManageHook -- insertPosition End Newer
                , startupHook = startup
                , terminal = "kitty"
                , workspaces = myWorkspaces
                , borderWidth = 0
                }
                `additionalKeysP` [ ("M-a", spawn "flameshot gui") -- Take a screenshot of an area
		                  , ("M-S-a", spawn "flameshot full") -- Take a screenshot of all screens
                                  , ("M-x", kill) -- Close windows
                                  , ("M-t", spawn "kitty") 
                                  -- , ("M-r", spawn "rofi -show drun -show-icons") 
                                  , ("M-r", spawn ".config/rofi/launchers/type-7/launcher.sh") -- Run launcher
                                  , ("M-p", spawn ".config/rofi/powermenu/type-5/powermenu.sh") -- Run power menu
                                  , ("M-a", namedScratchpadAction myScratchPads "pavucontrol") -- scratchpad 
                                  , ("M-s", namedScratchpadAction myScratchPads "terminal") -- scratchpad 
				
				  -- Modified Keybindings
                                  , ("M-n", windows W.focusDown)
                                  , ("M-e", windows W.focusUp)
                                  , ("M-h", windows W.focusMaster)
                                  , ("M-S-n", windows W.swapDown)
                                  , ("M-S-e", windows W.swapUp)
                                  , ("M-i", sendMessage Shrink)
                                  , ("M-m", sendMessage Expand)
                                  , ("M-f", nextScreen)
                                  , ("M-S-f", shiftNextScreen)
                                  , ("M-w", prevScreen)
                                  , ("M-S-w", shiftPrevScreen)
                                  , ("<F9>", spawn "pamixer -i 5")
                                  , ("<F10>", spawn "pamixer -d 5")
                                  , ("<F11>", spawn "pamixer -t")
                                  ]

myManageHook :: ManageHook
myManageHook =
        composeAll
                [ className =? "confirm" --> doFloat
                , className =? "file_progress" --> doFloat
                , className =? "dialog" --> doFloat
                , className =? "download" --> doFloat
                , className =? "error" --> doFloat
                , className =? "notification" --> doFloat
                , className =? "splash" --> doFloat
                , className =? "Emacs" --> doShift "1:emacs"
                , className =? "kitty" --> doShift "2:code"
                , className =? "TelegramDesktop" --> doShift "3:msg"
                , className =? "nheko" --> doShift "3:msg"
                , className =? "Firefox" --> doShift "4:web"
                , className =? "Chromium" --> doShift "4:web"
                -- , className =? "Alacritty" --> doFloat
                , className =? "Thunar" --> doShift "6:file"
                , className =? "Opera" --> doShift "7:web"
                ]
		<+> namedScratchpadManageHook myScratchPads

myScratchPads = [
-- run htop in term, top half, perfect fit.
    NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol") 
        (customFloating $ W.RationalRect (1 / 6) (1 / 8) (2 / 3) (1/2)), 
    NS "terminal" "alacritty --class scratchpad" (className =? "scratchpad") 
        (customFloating $ W.RationalRect (1 / 6) (1 / 8) (2 / 3) (3/4)) 
    ] where role = stringProperty "WM_WINDOW_ROLE"

startup = do
        spawnOnce  "picom"
        spawnOnce  "/usr/libexec/xfce-polkit"


myWorkspaces = ["1:emacs", "2:code", "3:msg", "4:web", "5:term", "6:file", "7:web", "8:tools", "9:any"]

-- myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
myLayout = avoidStruts (tiled ||| Full)
    where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 1 / 2
        delta = 3 / 100

myXmobarPP :: PP
myXmobarPP =
        def
                { ppSep = magenta " • "
                , ppTitleSanitize = xmobarStrip
                , ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
                , ppHidden = white . wrap " " ""
                , ppHiddenNoWindows = lowWhite . wrap " " ""
                , ppUrgent = red . wrap (yellow "!") (yellow "!")
                , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
                , ppExtras = [logTitles formatFocused formatUnfocused]
                }
    where
        formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
        formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

        ppWindow :: String -> String
        ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

        blue, lowWhite, magenta, red, white, yellow :: String -> String
        magenta = xmobarColor "#ff79c6" ""
        blue = xmobarColor "#bd93f9" ""
        white = xmobarColor "#f8f8f2" ""
        yellow = xmobarColor "#f1fa8c" ""
        red = xmobarColor "#ff5555" ""
        lowWhite = xmobarColor "#888888" ""
