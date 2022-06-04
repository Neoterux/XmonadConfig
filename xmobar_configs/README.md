Xmobar Configuration Files
--------------------------

This directory contains multiple configuration files
for AMD/Intel processors and if is using on laptop/desktop.

## Features

- Show CPU Temperature
- Show Memory statistics
- Show Memory usage
- Show network traffic
- show Date
- Show Application that is using more RAM
- Show the title of the current/focused window
- Show the current Workspace
- Show the current layout for the Tall
- Thermal Monitoring (*WIP*)

### Laptop
- Show Bluetooth (*TODO*)
- Show/Control Brightness (*TODO*)
- Volume Mixer (*TODO*)
- Wireless Monitor (*TODO*)


#### Future/Possible TODO list
- Keyboard monitor [layout/caps]
- Mail monitor
- Music Monitor [spotify/etc.]
- Interactable App launcher / icons

## Installation 

To install any of the files in this directory, you can use
*Symlinks* or *Copy* the file:

#### Symlinks
```
$ ln -s $XMONAD_LOCATION/xmobar_configs/<config_file> $HOME/.xmobarrc
```

#### Copy
```
$ cp $XMONAD_LOCATION/xmobar_configs/<config_file> $HOME/.xmobarrc
```

## Additional Resources

- [https://codeberg.org/jao/xmobar-config](xmobar configuration examples)
- [https://github.com/jaor/xmobar](xmobar sources/GitHub Repo)
- [https://github.com/jaor/xmobar/blob/master/doc/quick-start.org](xmobar quick start
  guide)
- [https://github.com/jaor/xmobar/blob/master/doc/plugins.org](Plugin index for xmobar)
- [https://github.com/jaor/xmobar/blob/master/doc/write-your-own-plugin.org](Write plugin
  for xmobar)
