import           Control.Monad              ()
import           Data.List                  (sort)
import qualified Data.Map                   as M
import           Data.Monoid
import           System.Directory           (getHomeDirectory)
import           System.Exit
import           System.FilePath            (combine)
import           System.IO
import           XMonad
import           XMonad.Actions.Promote     (promote)
import qualified XMonad.Actions.Search      as S
import qualified XMonad.Actions.Submap      as SM
import           XMonad.Actions.WindowGo    (runOrRaiseNext)
import           XMonad.Config.Desktop
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks  -- avoid windows covering menu
import           XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen)
import           XMonad.Hooks.UrgencyHook  -- notification
import           XMonad.Layout.Monitor
import           XMonad.Layout.NoBorders    (smartBorders)
import           XMonad.Prompt
import           XMonad.Prompt.AppLauncher  (launchApp)
import           XMonad.Prompt.Pass         (passPrompt)
import           XMonad.Prompt.RunOrRaise   (runOrRaisePrompt)
import           XMonad.Prompt.Window
import           XMonad.Prompt.XMonad       (xmonadPromptC)
import qualified XMonad.StackSet            as W
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedWindows
import           XMonad.Util.Run            (spawnPipe, safeSpawn)

dotXmonadVersion :: String
dotXmonadVersion = "v0.0.1"

-- | The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "urxvt"

-- | Simple query utility to reduce duplication
--
query :: String -> String -> Query Bool
query appname classname = appName =? appname <&&> className =? classname

myTerminalQuery :: Query Bool
myTerminalQuery = query "urxvt" "URxvt"

myPdfReader :: String
myPdfReader = "zathura"

myPdfReaderQuery :: String -> Query Bool
myPdfReaderQuery "apvlv" = query "apvlv" "Apvlv"
myPdfReaderQuery "zathura" = query "zathura" "Zathura"
myPdfReaderQuery _ = error "Undefined"

-- | My preferential browser
--
myBrowser :: String
myBrowser = "qutebrowser"

myBrowserQuery :: Query Bool
myBrowserQuery = qutebrowserQuery

-- | VM query
--
vmQuery :: Query Bool
vmQuery = (query "qemu-system-x86_64" "qemu-system-x86_64") <||>
          (query "Qt-subapplication" "VirtualBox") <||>
          className =? ".emulator64-arm-wrapped"  <||>
          className =? "Android SDK Manager"

-- | My preferential emacs
--
myEmacsQuery :: Query Bool
myEmacsQuery = query "_emacs-wrapped" "Emacs" <||>
               query "emacs" "Emacs"

-- | Whether focus follows the mouse pointer.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- | Whether a mouse click select the focus or is just passed to the window
--
myClickJustFocuses :: Bool
myClickJustFocuses = True

-- | Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth  = 1

-- | modMask lets you specify which modkey you want to use. mod4mask
-- is window key. I'm used to prefix key because of emacs, stumpwm,
-- xmonad-customed (here), conkeror, firefox with keysnail (rip),
-- qutebrowser-customed
--
myModMask :: KeyMask
myModMask = mod4Mask

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

-- | Prefix key
--
prefixKey :: String
prefixKey = "C-;"

-- | A utility function to compute the final binding with the prefix key
--
prefix :: String -> String
prefix = ((prefixKey ++ " ") ++)

-- | Execute a command and show its result in a zenity window dialog
--
spawnZenityCmd :: String -> String -> X ()
spawnZenityCmd home =
  let zenityCmd :: String -> String
      zenityCmd cmd = zenity ++ " --info --text \"$(" ++ cmd ++ ")\""
        where zenity = nixProfilePath home ("zenity" :: String)
  in spawn . zenityCmd

-- | Complete the binary path for a nix profile
--
nixProfilePath :: String -> String -> String
nixProfilePath home cmd = combine home program
  where nixProfilePath = ".nix-profile/bin"::String
        program = combine nixProfilePath cmd

-- | Run or raise command found in nix profile
--
nixRunOrRaise :: String -> String -> Query Bool -> X ()
nixRunOrRaise home cmd = runOrRaiseNext $ nixProfilePath home cmd

-- | Run or raise home script found in ~/bin/
--
homeRunOrRaise :: String -> String -> Query Bool -> X ()
homeRunOrRaise home cmd = runOrRaiseNext $ combine home program
  where program = combine ("bin"::String) cmd

-- | Spawn command adding the extra nix profile path needed
--
mySpawn :: String -> String -> X ()
mySpawn home cmd = spawn $ nixProfilePath home cmd

libreOfficeQuery :: Query Bool
libreOfficeQuery = (appName =? "libreofficedev" <||> appName =? "libreoffice") <&&>
                   (className =? "libreofficedev-writer" <||>
                    className =? "libreoffice-writer" <||>
                    className =? "libreofficedev-calc" <||>
                    className =? "libreoffice-calc" <||>
                    className =? "libreofficedev-draw" <||>
                    className =? "libreoffice-draw" <||>
                    className =? "libreoffice-startcenter")

conkerorQuery :: Query Bool
conkerorQuery = query "Navigator" "Conkeror"

qutebrowserQuery :: Query Bool
qutebrowserQuery = className =? "qutebrowser"

vlcQuery :: Query Bool
vlcQuery = query "vlc" "vlc"

xephyrQuery :: Query Bool
xephyrQuery = query ".Xephyr-wrapped" "Xephyr"

zenityQuery :: Query Bool
zenityQuery = query "zenity" "Zenity"

-- | My keymap as (prefix keybindings, command description, command)
--
myKeymapWithDescription :: String -> XConfig Layout -> [(String, String, X ())]
myKeymapWithDescription home conf@(XConfig { layoutHook = myLayoutHook
                                           , workspaces = myWss}) =
  [ (prefix "C-g"       , "abort"                      , mySpawn home "xdotool key Escape")
  , (prefix "v"         , "version"                    , spawnZenityCmd home $ "echo version:" ++ dotXmonadVersion)
  , (prefix "M1-c"      , "mouse-click-at-point"       , mySpawn home "xdotool click 1")
  , (prefix "M1-d"      , "xdotool-prompt"             , launchApp myXPConfig "xdotool")
  , (prefix "e"         , "emacs"                      , homeRunOrRaise home "emacs"               myEmacsQuery)
  , (prefix "S-c"       , "lighttable"                 , nixRunOrRaise home "light"                (query "lighttable" "LightTable"))
  , (prefix "C-r"       , "simplescreenrecorder"       , nixRunOrRaise home "simplescreenrecorder" (query "simplescreenrecorder" "SimpleScreenRecorder"))
  , (prefix "M1-S-x"    , "mcomix"                     , nixRunOrRaise home "mcomix"               (query "mcomix" "MComix"))
  , (prefix prefixKey   , "promote"                    , promote)  -- window manipulation
  , (prefix "x"         , "terminal"                   , homeRunOrRaise home myTerminal            myTerminalQuery)
  , (prefix "C-x"       , "xterm"                      , nixRunOrRaise home "xterm"                (query "xterm" "XTerm"))
  , (prefix "S-s"       , "sweethome-3d"               , nixRunOrRaise home "sweethome3d"          (query "sun-awt-X11-XFramePeer" "com-eteks-sweethome3d-SweetHome3D"))
  , (prefix "s"         , "signal-desktop"             , nixRunOrRaise home "signal-desktop"       (query "signal" "Signal"))
  , (prefix "S-t"       , "vlc"                        , nixRunOrRaise home "vlc"                  vlcQuery)
  , (prefix "C-e"       , "pdf-reader"                 , nixRunOrRaise home myPdfReader            (myPdfReaderQuery myPdfReader))
  , (prefix "C-i"       , "image-viewer"               , nixRunOrRaise home "feh"                  (query "feh" "feh"))
  , (prefix "d"         , "pinta"                      , nixRunOrRaise home "pinta"                (query "Pinta" "Pinta"))
  , (prefix "C-a"       , "music-reader"               , nixRunOrRaise home "audacious"            (query "audacious" "Audacious"))
  , (prefix "S-g"       , "gparted"                    , runOrRaiseNext "sudo gparted"             (query "gpartedbin" "GParted"))   -- expect this installed as main system
  , (prefix "C-S-x"     , "xosview"                    , nixRunOrRaise home "xosview2"             (query "xosview" "XOsview2"))
  , (prefix "C-S-g"     , "dia"                        , nixRunOrRaise home "dia"                  (query "dia-normal" "Dia-Normal"))
  , (prefix "f"         , "browser"                    , homeRunOrRaise home myBrowser             myBrowserQuery)
  , (prefix "b"         , "qutebrowser"                , homeRunOrRaise home myBrowser             myBrowserQuery)  -- qutebrowser working on nixos, not on plain nix
  , (prefix "M1-t"      , "tuxguitar"                  , nixRunOrRaise home "tuxguitar"            (query "TuxGuitar" "TuxGuitar"))
  , (prefix "l"         , "libre-office"               , nixRunOrRaise home "libreoffice"          libreOfficeQuery)
  , (prefix "C-S-e"     , "env"                        , spawnZenityCmd home "env")
  , (prefix "a"         , "date"                       , spawnZenityCmd home "date")
  , (prefix "S-k"       , "ssh-add-l"                  , spawnZenityCmd home "ssh-add -l")
  , (prefix "S-e"       , "cat-etc-environment"        , spawnZenityCmd home "cat /etc/environment")
  , (prefix "S-h"       , "cat-etc-hosts"              , spawnZenityCmd home "cat /etc/hosts")
  , (prefix "C-S-i"     , "sbin-ifconfig"              , spawnZenityCmd home "ifconfig")
  , (prefix "S-b"       , "acpi"                       , spawnZenityCmd home "acpi -b")
  , (prefix "^"         , "top"                        , spawnZenityCmd home "top -b -n 1 -c -d 1")
  , (prefix "C-t"       , "toggle-touchpad"            , spawn "~/bin/toggle-touchpad")
  , (prefix "C-s"       , "print-screen"               , spawn "~/bin/print-screen")
  , (prefix "M1-s"      , "mouse-print-screen"         , spawn "~/bin/print-screen mouse")
  , (prefix "C-S-s"     , "suspend"                    , spawn "systemctl suspend")
  , (prefix "C-S-h"     , "hibernate"                  , spawn "systemctl hibernate")
  , (prefix "C-b"       , "brightness-decrease"        , mySpawn home "xbacklight -dec 10")
  , (prefix "C-f"       , "brightness-increase"        , mySpawn home "xbacklight -inc 10")
  , (prefix "C-S-m"     , "brightness-half"            , mySpawn home "xbacklight -set 50")
  , (prefix "S-m"       , "brightness-max"             , mySpawn home "xbacklight -set 100")
  , (prefix "M1-f"      , "sound-increase"             , spawn "amixer set Master 5%+")
  , (prefix "M1-b"      , "sound-decrease"             , spawn "amixer set Master 5%-")
  , (prefix "M1-m"      , "sound-toggle"               , spawn "amixer set Master toggle")
  , (prefix "M1-e"      , "pdf-reader-prompt"          , launchApp myXPConfig myPdfReader)
  , (prefix "r"         , "exec"                       , runOrRaisePrompt myXPConfig)
  , (prefix "g"         , "goto"                       , windowPrompt myXPConfig Goto allWindows)
  , (prefix "M1-x"      , "meta-x"                     , xmonadPromptC keymapDescription myXPConfig)
  , (prefix "p"         , "pass-read"                  , passPrompt myXPConfig)
  , (prefix "k"         , "kill-current-window"        , kill >> mySpawn home "notify-send -t 1000 'window closed!'")
  , (prefix "<Space>"   , "rotate-layout"              , sendMessage NextLayout)
  , (prefix "C-<Space>" , "reset-layout"               , setLayout myLayoutHook)
  , (prefix "M1-r"      , "refresh"                    , refresh)
  , (prefix "C-M1-b"    , "banish-mouse"               , spawn "~/bin/banish-mouse")
  , (prefix "C-M1-l"    , "lock-screen"                , mySpawn home "xscreensaver-command -lock")
  , (prefix "o"         , "window-move-focus-next"     , windows W.focusDown)
  , (prefix "m"         , "window-move-focus-master"   , windows W.focusMaster)
  , (prefix "<Return>"  , "window-swap-focus-master"   , windows W.swapMaster)
  , (prefix "C-j"       , "window-swap-focus-next"     , windows W.swapDown)
  , (prefix "C-k"       , "window-swap-focus-previous" , windows W.swapUp)
  , (prefix "M1-l"      , "master-shrink-area"         , sendMessage Shrink)
  , (prefix "M1-h"      , "master-expand-area"         , sendMessage Expand)
  , (prefix "t"         , "window-push-back-tiling"    , withFocused $ windows . W.sink)
  , (prefix "C-S-q"     , "xmonad-recompile"           , spawn "~/bin/xmonad-action recompile")
  , (prefix "S-q"       , "xmonad-restart"             , spawn "~/bin/xmonad-action restart")
  , (prefix "M1-q"      , "xmonad-quit"                , io exitSuccess)] ++
  -- M1-n   - Switch to workspace with id n
  -- S-n    - Move the client to workspace with id n
  [(prefix $ pk ++ k, desc ++ k , windows $ f i) | (i, k) <- zip myWss $ map show [1 .. length myWss]
                                                 , (f, pk, desc) <- [ (W.view  , "M1-", "workspace-keep-screen-switch-to-id-")
                                                                    , (W.shift, "S-" , "workspace-move-client-to-id-")]] ++
    -- S-M1-n - Switch to physical/Xinerama screen n
    -- C-M1-n - Move client to screen 1, 2, or 3
    --
  [(prefix $ pk ++ show screenNumber, metaXAction ++ show screenNumber, screenWorkspace screenNumber >>= flip whenJust (windows . fnAction))
    | screenNumber <- [1,0],
      (fnAction, pk, metaXAction) <- [ (W.view,  "S-M1-", "switch-to-physical-screen-")
                                    , (W.shift, "C-M1-", "move-client-to-screen-")]]
  where keymapDescription = map (\ (keybinding, xmonadActionDesc, xmonadAction) -> (xmonadActionDesc ++ " - " ++ keybinding, xmonadAction)) fullKeymap
        fullKeymap = myKeymapWithDescription home conf

-- | key bindings
--
myKeys :: String -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys home conf = mkKeymap conf keymap
                   where keymap = map (\ (keybinding, _, xmonadAction) -> (keybinding, xmonadAction)) fullKeymap
                         fullKeymap = myKeymapWithDescription home conf

-- | Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)]
    -- you may also bind events to the mouse scroll wheel (button4 and button5)

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

-- | My own layout
--
myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where -- default tiling algorithm partitions the screen into two panes
        tiled   = Tall nmaster delta ratio
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by master pane
        ratio   = 1/2
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100

------------------------------------------------------------------------
-- Workspaces:

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

workspaceEmacs, workspaceTerminal, workspaceWeb, workspaceCode, workspaceIrc, workspaceIde, workspaceFloat, workspaceBooks, workspaceDb , workspaceVM, workspaceDevVM, workspaceRemote :: String
workspaceEmacs    = "01:emacs"
workspaceTerminal = "02:terminal"
workspaceWeb      = "03:web"
workspaceCode     = "04:code"
workspaceIrc      = "05:irc"
workspaceDb       = "06:db"
workspaceFloat    = "07:ide"
workspaceVM       = "08:vm"
workspaceDevVM    = "09:dev-vm"
workspaceBooks    = "10:books"
workspaceIde      = "11:ide"
workspaceRemote   = "12:remote"

myWorkspaces :: [String]
myWorkspaces = sort [ workspaceEmacs
                    , workspaceTerminal
                    , workspaceWeb
                    , workspaceCode
                    , workspaceIrc
                    , workspaceVM
                    , workspaceDevVM
                    , workspaceIde
                    , workspaceFloat
                    , workspaceBooks
                    , workspaceDb
                    , workspaceRemote
                    ]

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'appName' (`resource`) are used below.
--
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ isFullscreen                                --> doFullFloat
    , vlcQuery                                    --> doFullFloat
    , xephyrQuery                                 --> doShift workspaceFloat >> doFloat
    , zenityQuery                                 --> doFloat
    , myEmacsQuery                                --> doShift workspaceEmacs
    , myTerminalQuery                             --> doShift workspaceTerminal
    , myBrowserQuery                              --> doShift workspaceWeb
    , conkerorQuery                               --> doShift workspaceWeb
    , myPdfReaderQuery myPdfReader                --> doShift workspaceBooks
    , vmQuery                                     --> doShift workspaceVM
    , className =? ".remmina-wrapped"             --> doShift workspaceRemote
    , manageMonitor screenKeyMonitor
    ]
  ------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook = mempty

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook :: X ()
myStartupHook = return ()

-- | Font
--
myDefaultFont :: String
myDefaultFont = "xft:DejaVu Sans Mono for Powerline:size=10"

-- | Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor :: String
myNormalBorderColor = "#1e2320"
myFocusedBorderColor :: String
myFocusedBorderColor = "#5193f7"

-- | Default configuration for prompt
--
myXPConfig :: XPConfig
myXPConfig = def
              { font              = "xft:DejaVu Sans Mono"
              , bgColor           = "#1e2320"
              , fgColor           = "#dddddd"
              , bgHLight          = "#5f5f5f"
              , fgHLight          = "#ffffff"
              , borderColor       = "#ffffff"
              , height            = 20
              , position          = Top
              , historySize       = 256
              , alwaysHighlight   = True
              , promptBorderWidth = 0}

-- | Make the screenkey window appear in front of other windows
--
screenKeyMonitor :: Monitor a
screenKeyMonitor = monitor
     { prop = ClassName "Screenkey"
     , rect = Rectangle 0 0 15 20 -- rectangle 20x20 in upper left corner
     , persistent = True
     , opacity = 0.8
     }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

-- | Urgency hook for notifications
--
instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    safeSpawn "$HOME/.nix-profile/bin/notify-send"
              [show name, "workspace " ++ idx]

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- | Now run xmonad with all the defaults we set up.
main :: IO ()
main = do
  home <- getHomeDirectory
  xmproc <- spawnPipe $ combine home "bin/xmobar"
  xmonad $ withUrgencyHook LibNotifyUrgencyHook
         $ desktopConfig {
                  focusFollowsMouse  = myFocusFollowsMouse
                , clickJustFocuses   = myClickJustFocuses
                , borderWidth        = myBorderWidth
                , modMask            = myModMask
                , workspaces         = myWorkspaces
                , normalBorderColor  = myNormalBorderColor
                , focusedBorderColor = myFocusedBorderColor
                , keys               = myKeys home
                , mouseBindings      = myMouseBindings
                , layoutHook         = smartBorders .avoidStruts $ myLayout
                , manageHook         = manageDocks <+> myManageHook
                -- Status bars and logging
                -- Perform an arbitrary action on each internal state change
                -- or X event.
                -- See the 'XMonad.Hooks.DynamicLog' extension for examples.
                --
                , logHook            = dynamicLogWithPP $ xmobarPP
                                                        { ppOutput = hPutStrLn xmproc
                                                        , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
                                                        , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
                                                        , ppSep = "   "
                                                        }
                , startupHook        = myStartupHook
                , handleEventHook    = docksEventHook
            }
