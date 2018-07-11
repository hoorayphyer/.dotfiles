This document explains what I find out along the way of configuring xkb.

1. the following entries has from highest to lowest precedence over keyboard layout
   1) input method such as fcitx, can modify keyboard layout. To disable,
      run fcitx-config-gtk3, go to AddOns, find X Keyboard Integration, then click configure at the bottom. Uncheck "allow to override" and
      "use 1st keyboard as the default layout".
   2) gnome3, run "gsettings set org.gnome.desktop.input-sources sources "[('xkb','us')]"". Also can provide variants and options this way. Or, run "dconf-editor" in the terminal and then navigate to find input-sources and make changes there.
   3) the setxkpmap actions, which uses /usr/share/X11/xkb/(rules, symbols, ...). What can also affect at this level is maybe the file 00-keyboard.conf, which may as well be absent, loacated at /etc/X11/xorg.conf.d/. This is the one users get to modify to specify xorg configs, among which is the xkb keyboard. To query stuff, use "setxkbmap -print -verbose 10" or "setxkbmap -query"
      
2. .local/share/xorg/Xorg.0.log for Xserver logs

3. use xev to test the key action

4. it is preferred now to use rules of xkb rather than xkbcomp to configure. If xkbcomp is used, one may need to worry about removing /var/lib/xkb/*.xkm to make new keymap effective. Also, one will use .xkb files as config files. A better way to provide config files to xorg is through /usr/share/X11/xorg.conf.d/ and /etc/X11/xorg.conf.d/, the latter of which has higher precedence

5. xorg keymap and console keymap are two different things. setxkbmap only affects Xorg keymap. Configuring console keymap requires different tools. 

6. localectl can set keyboard layout for both Xorg and the console. I don't know how it affects console. But for Xorg, it writes to /etc/X11/xorg.conf.d/00-keyboar.conf. Note that this is at the level of 1.3), so gnome and fcitx can override it.

7. evdev is the rules used by linux system, which seems to the default. evdev is also a name for drivers. But here, the driver libinput is actually used by default. See /usr/share/X11/xorg.conf.d/40-libinput.conf

8. how to start gnome graphically? One way is to enable gdm.service, then it will startx. Or, one can add "exec startx" in .zprofile( not .zlogin? ). Either way, one needs to add "exec gnome-session" in .xinitrc. There is this /etc/gdm/Xsession file whose purpose is not known.

9. startx uses /etc/X11/xinit/xserverrc

10. Four inputs in rules : model, layout, variant, option          outputs: keycode, symbols, types, geometry, compat

11. put the following in .xprofile for fcitx to work
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

12. lxde. use "exec startlxde" in .xinitrc in place of gnome-session
