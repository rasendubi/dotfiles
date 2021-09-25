{ pkgs, config, ... }:
{
  xdg.mimeApps = {
    enable = true;
    # TODO this is simultaneously handled globally.. -.-
    defaultApplications = {
      "application/pdf" = [ "emacsclient.desktop" ];
      "x-scheme-handler/org-protocol" = [ "org-protocol.desktop" ];
      "x-scheme-handler/msteams" = [ "teams.desktop" ];
      "image/png" = [ "org.inkscape.Inkscape.desktop" ];  # annoyiiiing
      "image/svg+xml" = [ "org.inkscape.Inkscape.desktop" ];
      "x-scheme-handler/http" = [ "qutebrowser.desktop" ];
      "x-scheme-handler/https" = [ "qutebrowser.desktop" ];
    };
  };
  # targets.genericLinux.enable = true;
  xdg.configFile."tridactyl/tridactylrc".text = "bind ;r js javascript:location.href = 'org-protocol:/roam-ref?template=r&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title)
";

  # this (maybe) fixes the issue that zoom cannot change to bluetooth sink: https://unix.stackexchange.com/questions/452907/pavucontrol-wont-change-output-on-some-apps
  home.file.".alsoftrc".text = ''
[pulse]
allow-moves=yes
  '';
  xdg.configFile."dunst/dunstrc".source = ../dunst/dunstrc;

  # autostart
  xdg.configFile."autostart/nm-applet.desktop".source = "/run/current-system/sw/share/applications/nm-applet.desktop";
  # xdg.configFile."autostart/firefox.desktop".source = "/run/current-system/sw/share/applications/firefox.desktop";
  # xdg.configFile."autostart/rxvt-unicode.desktop".source = "/run/current-system/sw/share/applications/rxvt-unicode.desktop";
#   xdg.configFile."autostart/clipit.desktop".text = ''[Desktop Entry]  <- I use clipmon now..
# Icon=clipit-trayicon
# Exec=clipit
# Terminal=false
# Type=Application
# Categories=GTK;GNOME;Application;Utility;
# Name=ClipIt'';

  xdg.configFile."autostart/qutebrowser.desktop".text = ''[Desktop Entry]
Name=Qutebrowser
Type=Application
Categories=Network;WebBrowser;
Exec=qutebrowser
  '';

  xdg.configFile."autostart/blueman-applet.desktop".text = ''[Desktop Entry]
Name=Blueman Applet
Comment=Manage your Bluetooth connections
Icon=bluetooth
Exec=blueman-applet
Terminal=false
Type=Application
NoDisplay=true
NotShowIn=KDE;GNOME;'';
  xdg.configFile."autostart/pasystray.desktop".text = ''[Desktop Entry]
Version=1.0
Name=PulseAudio System Tray
GenericName=
Comment=An Applet for PulseAudio
Exec=pasystray
Icon=pasystray
StartupNotify=true
Type=Application
Categories=AudioVideo;Audio;
Keywords=pulseaudio;tray;system tray;applet;volume;'';
#   xdg.configFile."autostart/flameshot.desktop".text = ''[Desktop Entry]
# Name=Flameshot
# Comment=Screenshot tool
# Exec=flameshot
# Icon=flameshot
# StartupNotify=true
# Type=Application'';

  # python
#   home.packages = with pkgs; let moritzsphd = python37Packages.buildPythonPackage rec {
#   name = "moritzsphd";
#   src = /home/moritz/Projects/moritzsphd;
#   propagatedBuildInputs = with python37Packages; [ numpy ];  # incomplete..
# }; in
home.packages =   with pkgs; [
    xpdf # this is an insecure package. an exception is in config.nix
    dolphin
    tmux
    xdotool
    xss-lock
    (pass.withExtensions (exts: [ exts.pass-otp ]))
    acpilight

    minicom
    firefox
    google-chrome
    arandr
    escrotum
    ripgrep
    pavucontrol

    google-play-music-desktop-player

    inconsolata
    dejavu_fonts
    source-code-pro
    ubuntu_font_family
    powerline-fonts
    terminus_font

    # Emacs fonts
    # input-mono
    libertine

    direnv
    clipit
    # vlc
    betaflight-configurator

    ];

  home.sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

  # programs.emacs =
  #   let e = import ./emacs.nix { inherit pkgs; };
  #   in {
  #     enable = true;
  #     package = e.emacs;
  #     extraPackages = e.emacsPackages;
  #   };
  # services.emacs.enable = true;

  services.lorri.enable = true;

  programs.browserpass = {
    enable = true;
    browsers = ["firefox" "chrome"];
  };

  # programs.home-manager = {
  #   enable = true;
  #   path = "/home/rasen/dotfiles/channels/home-manager";
  # };

  # services.syncthing.enable = true; # already enabled in the main config

  programs.fish = {
    enable = true;
    shellAliases = {
      g = "git";
    };
    shellInit = ''
      set -gx PATH $HOME/bin $PATH
      set EDITOR vim

      if [ "$TERM" = rxvt-unicode-256color ]
        set -x TERM xterm-256color
      end

      ssh-add -l > /dev/null
      if [ $status -ne 0 ]
          ssh-add
      end

      eval (direnv hook fish)
      fish_vi_key_bindings

      alias l 'ls -lhtra'
      alias clip 'xclip -selection clipboard'
    '';
  };
  # programs.bash = {
  #   enable = true;
  #   shellAliases = {
  #     g = "git";
  #   };
  # };

  # controlled by nixos-config/README.org
  # home.keyboard = {
  #   layout = "de,us,ua";
  #   variant = "bone,workman,";
  # };

  # xsession.enable = true;

  #xsession.windowManager.exwm = {
  #  enable = true;
  #};
  # xsession.pointerCursor = {
  #   name = "Vanilla-DMZ";
  #   package = pkgs.vanilla-dmz;
  #   size = 128;
  # };
  xsession.initExtra = ''
    # xkbcomp /home/moritz/nixos-config/.Xkeymap $DISPLAY # TODO enable?

    autorandr -c
    xrdb -merge ~/.Xresources
  '';
  # ~/.screenlayout/default.sh

  fonts.fontconfig.enable = true;

  programs.htop.enable = true;
#   programs.urxvt = {
#     enable = true;
#     iso14755 = false;

#     fonts = [
#       "-*-terminus-medium-r-normal-*-32-*-*-*-*-*-iso10646-1"
#     ];

#     scroll = {
#       # urxvt.scrollTtyOutput:   false
#       # urxvt.scrollTtyKeypress:  true
#       # urxvt.secondaryScroll:    true
#       #URxvt.intensityStyles: false
#       bar.enable = false;
#       lines = 65535;
#       scrollOnOutput = false;
#       scrollOnKeystroke = true;
#     };
#     extraConfig = let
#       colors = {
#         S_yellow = "#b58900";
#         S_orange = "#cb4b16";
#         S_red = "#dc322f";
#         S_magenta = "#d33682";
#         S_violet = "#6c71c4";
#         S_blue = "#268bd2";
#         S_cyan = "#2aa198";
#         S_green = "#859900";

#         S_base03 = "#002b36";
#         S_base02 = "#073642";
#         S_base01 = "#586e75";
#         S_base00 = "#657b83";
#         S_base0 = "#839496";
#         S_base1 = "#93a1a1";
#         S_base2 = "#eee8d5";
#         S_base3 = "#fdf6e3";
#       };
#     in with colors; {
#       "loginShell" = "true";
#       "urgentOnBell" = "true";
#       "secondaryScroll" = "true";

#       "background" = S_base03;
#       "foreground" = S_base0;
#       "fading" = 40;
#       "fadeColor" = S_base03;
#       "cursorColor" = S_base1;
#       "pointerColorBackground" = S_base01;
#       "pointerColorForeground" = S_base1
# ;
#       "color0" = S_base02;
#       "color1" = S_red;
#       "color2" = S_green;
#       "color3" = S_yellow;
#       "color4" = S_blue;
#       "color5" = S_magenta;
#       "color6" = S_cyan;
#       "color7" = S_base2;
#       "color8" = S_base03;
#       "color9" = S_orange;
#       "color10" = S_base01;
#       "color11" = S_base00;
#       "color12" = S_base0;
#       "color13" = S_violet;
#       "color14" = S_base1;
#       "color15" = S_base3;
#       "Xft.dpi" = 240;
#       "Xcursor.size" = 64;
#       "Xft.autohint" = 0;
#       "Xft.lcdfilter" = "lcddefault";
#       "Xft.hintstyle" = "hintfull";
#       "Xft.hinting" = 1;
#       "Xft.antialias" = 1;
#       "Xft.rgba" = "rgb";
#     };
#   };
  # this is colliding with the configs I already have in ~/.config/autorandr, so I disabled it
  programs.autorandr = {
    enable = false;
    profiles = {
      "home" = {
        fingerprint = {
          DP-3 = "00ffffffffffff0010acc0a042524530031c010380351e78eae245a8554da3260b5054a54b00714f8180a9c0a940d1c0e10001010101a36600a0f0701f80302035000f282100001a000000ff004438565846383148304552420a000000fc0044454c4c205032343135510a20000000fd001d4c1e8c1e000a202020202020018802032ef15390050402071601141f1213272021220306111523091f07830100006d030c001000003c200060030201023a801871382d40582c25000f282100001e011d8018711c1620582c25000f282100009e04740030f2705a80b0588a000f282100001e565e00a0a0a02950302035000f282100001a0000000000000000008a";
          eDP-1 = "00ffffffffffff004d108d1400000000051c0104a52213780ea0f9a95335bd240c5157000000010101010101010101010101010101014dd000a0f0703e803020350058c210000018000000000000000000000000000000000000000000fe00464e564452804c513135364431000000000002410328011200000b010a202000ee";
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x2160";
            mode = "3840x2160";
            rate = "60.00";
            dpi = 220;
          };
          DP-3 = {
            enable = true;
            position = "0x0";
            mode = "3840x2160";
            rate = "29.98";
            dpi = 183;
          };
        };
      };
      "2screen" = {
        fingerprint = {
          eDP-1 = "00ffffffffffff004d108d1400000000051c0104a52213780ea0f9a95335bd240c5157000000010101010101010101010101010101014dd000a0f0703e803020350058c210000018000000000000000000000000000000000000000000fe00464e564452804c513135364431000000000002410328011200000b010a202000ee";
          DP-3 = "00ffffffffffff0010acc2d0545741312c1b010380351e78eaad75a9544d9d260f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a02950302035000e282100001a000000ff004d59334e44374234314157540a000000fc0044454c4c205032343138440a20000000fd0031561d711c000a202020202020010302031bb15090050403020716010611121513141f2065030c001000023a801871382d40582c45000e282100001e011d8018711c1620582c25000e282100009ebf1600a08038134030203a000e282100001a7e3900a080381f4030203a000e282100001a00000000000000000000000000000000000000000000000000000000d8";
        };
        config = {
          eDP-1 = {
            enable = true;
            primary = true;
            position = "0x1440";
            mode = "3840x2160";
            rate = "60.00";
            dpi = 220;
          };
          DP-3 = {
            enable = true;
            position = "640x0";
            mode = "2560x1440";
            rate = "59.95";
            dpi = 124;
          };
        };
      };
    };
  };

  # programs.zathura = {
  #   enable = true;
  #   options = {
  #     incremental-search = true;
  #   };
  #   extraConfig = ''
  #     map j scroll up
  #     map k scroll down
  #   '';
  # };

  services.unclutter = {
    enable = true;
  };

  services.network-manager-applet.enable = true;
}
