-- vim :foldmethod=marker
{--
  Xmonad Layout 'optimized' for CRKBD keyboard
  author: Neoterux
  github: https://github.com/Neoterux
  repo: https://github.com/Neoterux/XmonadConfig
  crkbd: https://github.com/foostan/crkbd
--}
import XMonad
-- **** LAYOUTS IMPORTS
-- Layout to use
-- provide a spacing between Talls
import XMonad.Layout.Spacing 
--            ( spacing )
-- import XMonad.Layout.Tabbed
import XMonad.Layout.Accordion
-- ** No Border configuration
import XMonad.Layout.NoBorders
-- ** Resizable Tile Layout
import XMonad.Layout.ResizableTile 
-- ** Grid Layout
import XMonad.Layout.Grid 
-- *** HOOKS
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
-- *** CONFIGS

-- *** PROMPT
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
import XMonad.Prompt.Shell

-- *** ADDIIONALS
import XMonad.Actions.FloatSnap
import XMonad.Util.Run(spawnPipe)
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Maybe (fromJust)

{-- :Config-documentation {{{
          **********          BEGIN OF THE CONFIGURATION
          ** var definitions:
          notes:
              The keybinding showed here were designed for my crkbd keyboard keymap configuration,
              some of them could be 'imposible' to do in a common keyboard, so if you want to use this
              configuration, you should modify, maybe, all the keymaps.

          appLauncher: The launcher used for call *.desktop/commands files (default is rofi).
                       callable through [mod] + [P]
          
          screenshotCmd: The command/app to make screenshot. 
          
          wspaces:      Contains the workspaces, limited to 10 for crkbd comodity
                        you can travel between them with the keymap:
                        [mod] + [shift] + [<wspace_number>] pd: workspace 10 is maped to 0

          focusOnCursor:  Set to True to change dinamically the focus from a tile, to the tile 
                          where the cursors is. (default is False)

          mainTerm:     The default terminal emulator used in XMonad.

          borderThickness: The thickness of the tile window border, this border show which tile
                           window is focused in this time.

          myStatusBar:  The status bar used by XMonad, default is: Xmobar. If change you should
                        change 'main', 'myPP' and 'toggleStutsKey'.
          
          keybind:    Contains the keymaps that xmonad would recognize, the list is:
                      {-- GLOBAL --}
                      [mod] => Win/super -- can change directly in 'defaults' variable
                      [alt] + [ctrl] + [t] : Open default terminal
                      [mod] + [return]     : Open default terminal
                      [mod] + [shft] + [s] : Open/launch screenshot application -- change in 'screenshotCmd'
                      [mod] + [shft] + [c] : Kill the focused window/app
                      [mod] + [p]          : Open the application Launcher
                      [mod] + [q]          : Recompile xmonad and apply changes
                      [mod] + [shft] + [q] : Close XMonad
                      [mod] + [space]      : Go to next Layout
                      [mod] + [t]          : Convet to tile window a floating window
                      
                      {-- RESIZABLE TALL LAYOUT --}
                      [mod] + [w]          : Increase Tall size
                      [mod] + [s]          : Decrease Tall size

                      {--       WORKSPACE       --}
                      -- n is the workspace number, no the 'n' key
                      [mod] + [n]          : Change to the n-workspace
                      [mod] + [shft] + [n] : Shift the current focus tile to the n-workspace

                      {--          BAR          --}
                      [mod] + [b]          : Toggle the gap of the xmobar
          
          mouseBinding:    Contains maps that use the mouse buttons to do an action:
                      {--        WINDOW        --}
                      [mod] + [button1]    : Set the clicked tile to floating mode and let dragg it.
                      [mod] + [button2]    : Change the current Tall to the clicked tile.

          HOOKS:
            Hooks let to XMonad to do certain action to specified window/app by their WM_CLASS, or other
            property.

            the defaults hooks in this configurations are:
            'Zoom Meetings':
                  1. Move the app to the '6:mtg' workspace
                  2. Fix the notification to make them a floating window instead to be a tile
            'Microsoft edge', 'Firefox', 'Google Chrome':
                  1. Move to the '2:web' workspace
            'IntelliJ IDEA', 'vscode':
                  1. Move to the '3:dev' workspace
}}} --}

-- Main method
main :: IO()
main = xmonad . ewmhFullscreen .docks =<< statusBar myStatusBar myPP toggleStrutsKey defaults


{--  BASIC CONFIGS  --}
appLauncher :: String
appLauncher = "rofi -show"

screenshotCmd :: String
screenshotCmd = "flameshot gui"

mainTerm :: String
mainTerm = "kitty"

borderThickness :: Dimension
borderThickness = 1

focusOnCursor::Bool
focusOnCursor = True

myStatusBar :: String
myStatusBar = "xmobar"

-- workspaces
wspaces :: [String]
wspaces = ["1:main", "2:web", "3:dev", "4:games", "5:studies", "6:mtg", "7", "8", "9", "10"] 
wspacesIndex = M.fromList $ zip wspaces [1 ..]
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
	where
	  i = fromJust $ M.lookup ws wspacesIndex

{-- 
          KEYMAP CONFIGURATION
          if you want to add a keymap, edit the keybind function.
--}
myKeymap :: (XConfig l) -> M.Map (KeyMask, KeySym) (X())
myKeymap = M.fromList . keybind  -- Don't touch to add a keymap

-- : Keymaps {{{
keybind :: (XConfig l) -> [((KeyMask, KeySym), X())]

keybind conf@(XConfig {XMonad.modMask = modm}) =
    -- Launch terminal with alt + control + T
  [ ((mod1Mask .|. controlMask, xK_t), spawn $ XMonad.terminal conf)


  , ((modm, xK_Return), spawn $XMonad.terminal conf) -- 'Legacy' terminal invocation

    -- Launch Flameshot gui for screen shot [super + shft + S]
  , ((modm .|. shiftMask, xK_s), spawn screenshotCmd)

    -- kill current window
  , ((modm .|. shiftMask, xK_c), kill)

    -- Invoke default appLauncher
  , ((modm,               xK_p), spawn appLauncher)

    -- Invoke gmrun [may be removed]
  , ((modm .|. shiftMask, xK_p), spawn "gmrun")

    -- Recompile and restart
  , ((modm,               xK_q), spawn "xmonad --recompile; xmonad --restart")

  -- Restore from floating to tile window
  , ((modm,               xK_t), withFocused $ windows . W.sink)

  -- Exit Xmonad
  , ((modm .|. shiftMask, xK_q), io $ exitWith ExitSuccess)
  ]
  ++
  {-- 
          Tiling window keys 
  --}
        -- Go to the next layout 
  [ ((modm,            xK_space), sendMessage NextLayout)
  
  -- Adjust tile size
    -- Expand Tall
  , ((modm,               xK_w), sendMessage Expand) 

    -- decrease Tall
  , ((modm,               xK_s), sendMessage Shrink) 
  ]
  ++
        {- WorkSpace key bindings-}
  [ ((m .|. modm, k), windows $ f i)
                -- Define the workspace with the respective key 
         | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1..xK_9] ++ [xK_0]
                -- [0]: behaviour to change the workspace
                -- [1]: behaviour to send a window to another workspace
        , (f, m) <- [(W.greedyView, shiftMask), (W.shift, 0)]]

-- Keybind for toggle the gap of the bar
toggleStrutsKey :: (XConfig l) -> (KeyMask, KeySym)
toggleStrutsKey XConfig { XMonad.modMask = modm } = (modm, xK_b)
-- }}}
-- Mouse Configuration

mBindings :: (XConfig l) -> M.Map (KeyMask, Button) (Window -> X())
mBindings  = M.fromList . mouseBinding

mouseBinding :: (XConfig l) -> [( (KeyMask, Button), Window -> X() )]
mouseBinding (XConfig {XMonad.modMask = modm}) = 
    -- mod-button1[left-click], set the window in floating mode and move by dragging
  [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                    >> windows W.shiftMaster
                                    >> afterDrag (snapMagicMove (Just 50) (Just 50) w))
  , ((modm .|. shiftMask, button1), \w -> focus w 
                                    >> mouseMoveWindow w 
                                    >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w ))
  , ((modm, button3), \w -> focus w
                            >> mouseResizeWindow w
                            >> afterDrag (snapMagicResize [R,D] (Just 50) (Just 50) w))
    -- mod-mouse-wheel-button shift between tiling window to the master
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  ]


{--
               WINDOWS HOOKS
--}
-- Zoom manage hook by: https://www.peterstuart.org/posts/2021-09-06-xmonad-zoom/
-- : Windows Hooks {{{
-- manageZoomHook :: [Query (Endo WindowSet)]
manageZoomHook = 
    [ (className =? zoomClassName) <&&> shouldFloat <$> title --> doFloat
  ,   (className =? zoomClassName) <&&> title =? "Settings" --> doCenterFloat
  , (className =? zoomClassName) <&&> shouldSink  <$> title --> wdoSink
    ]
    where
      zoomClassName = "zoom"
      tileTitles = 
        [ "Zoom - Free Account"     -- welcome/Main window
        , "Zoom - Licensed Account" --welcome/Main window
        , "Zoom"  --Meeting window on creation
        , "Zoom Meeting" -- meeting window shortly after creation
        ]
      shouldFloat wtitle = wtitle `notElem` tileTitles
      shouldSink  wtitle = wtitle `elem`    tileTitles
      wdoSink = (ask >>= doF . W.sink) <+> doF W.swapDown

myManageHook = composeAll $ manageZoomHook ++
    -- General manageHooks
    -- ** Browsers
  [ className =? "Microsoft-edge"             --> doShift "2:web"
  , className =? "Google-Chrome"              --> doShift "2:web"
  , className =? "firefox"                    --> doShift "2:web"
  , (className =? "firefox") <&&> isPiP <$> title --> doFloat
  -- ******* MISC HOOKS *******

  -- ** Development apps
--  , className =? "jetbrains-idea"             --> doShift "3:dev"
  , className =? "jetbrains-dataspell"  --> doFloat
  , className =? "Code"                       --> doShift "3:dev" -- VScode & other distro?
  , (className =? "Code") <&&> isPopup <$> title --> doIgnore
  , (className =? "Code") <&&> isPopup <$> title --> doFloat
  , className =? "Matplotlib"  --> doFloat

  -- ** Games
  , className =? "Minecraft-Launcher"         --> doShift "4:games"
  , className =? "MultiMC"                    --> doShift "4:games"
  , className =? "Minecraft"                  --> doShift "4:games"
  , className =? "Console window for 1.18 — MultiMC 5" --> doFloat

  -- ** Studies Things
  , className =? teamsClass                   --> doShift "5:studies"
  , (className =? teamsClass) <&&> isRegistered <$> title --> doFloat
  , className =? "zoom"                       --> doShift "6:mtg"

  -- ** Other 
  , className =? "desktop_window"             --> doIgnore
  , className =? "stalonetray"                --> doIgnore
  , className =? "Org.gnome.Nautilus"         --> doFloat
  , className =? "gmrun"                      --> doFullFloat
  , className =? "Gmrun"                      --> doFloat
  , className =? "bashrun"                    --> doFloat
  , className =? "feh"                        --> doFloat
  , isFullscreen                              --> doFullFloat
  , className =? "Pavucontrol"                --> doCenterFloat
  ]
    where
      teamsClass = "Microsoft Teams - Preview"
      pipNames = ["Picture-in-Picture"]
      titles = [ "Picture-in-Picture"
               , "Microsoft Teams Notification"
               ]
      popupTitleNames = [ "Open Folder" ]
      
      isRegistered wtitle = wtitle `elem` titles
      isPiP wtitle = wtitle `elem` pipNames
      isPopup wtitle = wtitle `elem` popupTitleNames
-- }}}
{--
      BAR CONFIGURATION
--}
-- : Xmobar Configuration {{{
myPP :: PP
myPP = xmobarPP {
  ppCurrent = xmobarColor "#ffb601" "" . wrap "[" "]",
  ppVisible = xmobarColor "#ffb601" "" . wrap "" "" . clickable,
  ppSep = "|"
}

-- Layout definition
-- myHandleEventHook :: Event -> X `base-4.15.0.0`:Data.Semigroup.Internal.All
myHandleEventHook = mconcat 
  [ dynamicTitle $ composeAll manageZoomHook
--  , fullscreenEventHook -- ewmhFullscareen --fullscreenEventHook
  ]

--mouseStateHook = \e -> case e of
--                         CrossingEvent {ev_window=w, ev_event_type=t}
--                           | t == enterNotify, ev_mode e== notifyNormal -> do
--                             mouseFocusState <- rfocusFollow; let mfocusActive = mouseFocusState
--                             whenX (io $ readIORef mfocusActive) (focus w)
--                             return $ All True
--                          _ -> return $ All True
-- }}}
{-
   ** Implement this if available
mfocusStateHook :: Event -> X All
mfocusStateHook ev@(CrossingEvent {ev_window=w, ev_event_type=t}) 
                  |  t== enterNotify || ev_mode ev == notifyNormal  = do
                        whenX (io readState) (focus w)
                  |  otherwise = def All True
-}
-- : Layout Hook {{{
myLayoutHook = resizableTall
           ||| Mirror resizableTall
           ||| Grid
           ||| noBorders Full
           ||| Accordion
    where
      resizableTall = ResizableTall 1 (3/100) (1/2) []
-- }}}
-- : Startup hook {{{

myStartupHook = do 
	setWMName "LG3D"
	spawn "~/.config/xmonad/scripts/autostart.sh &"
	spawn "~/.config/xmonad/scripts/systray.sh &"
	spawn "~/.config/xmonad/scripts/backlight.sh &"
	spawn "~/.config/xmonad/scripts/startup-apps.sh &"
-- }}}

-- : Configuration Monad {{{
defaults = ewmh def {
      terminal = mainTerm
    , modMask  = mod4Mask -- win/super key as mod key
    , borderWidth = borderThickness
    , workspaces = wspaces
    , focusFollowsMouse = focusOnCursor

    -- Key bindings
    --keys = keybind,
    , keys = myKeymap
    , mouseBindings = mBindings

    -- Layout hooks
    , layoutHook = avoidStruts . smartBorders . spacing 2 $ myLayoutHook
    , handleEventHook = myHandleEventHook -- <+> mfocusStateHook,
    , manageHook = myManageHook
    -- Startup hook
    , startupHook = myStartupHook
    }
-- }}}
