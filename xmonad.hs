import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.WindowArranger
import XMonad.Layout.Spacing
import XMonad.Config.Bluetile
import XMonad.Actions.MessageFeedback
import XMonad.Hooks.EwmhDesktops
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt
import XMonad.Util.EZConfig (additionalKeys, checkKeymap)
import System.Exit
import Data.Monoid

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


main :: IO()
main = xmonad =<< statusBar myStatusBar myPP toggleStrutsKey defaults


-- workspaces
wspaces = ["main", "web", "development", "terminals", "studies", "6", "7", "8", "9", "10"] 


-- Keymaps configuration
modKey = mod4Mask

-- keybind :: [(String, X())]
keybind conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
	-- Launch terminal with Ctrl + Shift + T
	[ ((controlMask .|. shiftMask, xK_t), spawn "kitty")

	, ((modm, xK_Return), spawn $XMonad.terminal conf) -- 'Legacy' terminal invocation

	-- Launch Flameshot gui for screen shot [super + shft + S]
	, ((modm .|. shiftMask, xK_s), spawn "/usr/bin/flameshot gui")

	-- kill current window
	, ((modm .|. shiftMask, xK_c), kill)

	-- Invoke dmenu
	, ((modm, xK_p), spawn "dmenu_run -fn -*-profont-*-*-*-*-12-*-*-*-*-*-*-* -nb black -nf rgb:a0/a0/a0 -sb rgb:00/80/80 -sf black")
	, ((modm .|. shiftMask, xK_p), spawn "gmrun")

	-- Recompile and restart
 	, ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
	-- Restore from floating to tile window
	, ((modm, xK_t), withFocused $ windows . W.sink)
	-- Exit Xmonad
	, ((modm .|. shiftMask, xK_q), io (exitWith ExitSuccess))
	-- Adjust tile size
	, ((modm, xK_a), sendMessage (IncreaseLeft  1))
	, ((modm, xK_w), sendMessage (IncreaseUp    1))
	, ((modm, xK_d), sendMessage (IncreaseRight 1))
	, ((modm, xK_s), sendMessage (IncreaseDown  1))
	, ((modm .|. shiftMask, xK_a), sendMessage (DecreaseLeft  1))
  	, ((modm .|. shiftMask, xK_w), sendMessage (DecreaseUp    1))
	, ((modm .|. shiftMask, xK_d), sendMessage (DecreaseRight 1))
	, ((modm .|. shiftMask, xK_s), sendMessage (DecreaseDown  1))

	]
	++
		{- WorkSpace key bindings-}
	[((m .|. modm, k), windows $ f i)
				| (i, k) <- zip (XMonad.workspaces conf) [xK_1..xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- Keybind for toggle the gap of the bar
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

-- Mouse Configuration

-- mouseBindings :: [(String, X())]
mBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
	-- mod-button1, set the window in floating mode and move by dragging
	[ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
									>> windows W.shiftMaster))
	, ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

 	]

-- Terminals
mainTerm = kittyTerm
kittyTerm = "kitty"
alacrittyTerm = "alacritty"

-- Window things
borderThickness = 1
shrinkSize = 50
focusOnCursor::Bool
focusOnCursor = False
myStatusBar = "xmobar"
myPP = xmobarPP {
	ppCurrent = xmobarColor "#429942" "" . wrap "<" ">"

}



defaults = def {
	terminal = mainTerm,
	modMask  = mod4Mask,
	borderWidth = borderThickness,
	workspaces = wspaces,
	focusFollowsMouse = focusOnCursor,

	-- Key bindings
	keys = keybind,
	mouseBindings = mBindings,

	-- Layout hooks
	layoutHook = avoidStruts $ spacing 3 $ Tall 1 (1/300) (1/2) ||| Full,
	handleEventHook = fullscreenEventHook
	--startupHook = do
		--return() >> checkKeymap defaults keybind
	}
