{ pkgs, config, ... }:
{
  targets.genericLinux.enable = true;

  home.packages = [
    pkgs.dolphin
    pkgs.tmux
    pkgs.xdotool
    pkgs.xss-lock
    (pkgs.pass.withExtensions (exts: [ exts.pass-otp ]))
    pkgs.acpilight

    pkgs.minicom
    pkgs.firefox
    pkgs.google-chrome
    pkgs.arandr
    pkgs.escrotum
    pkgs.ripgrep
    pkgs.pavucontrol

    pkgs.google-play-music-desktop-player

    pkgs.inconsolata
    pkgs.dejavu_fonts
    pkgs.source-code-pro
    pkgs.ubuntu_font_family
    pkgs.powerline-fonts
    pkgs.terminus_font

    # Emacs fonts
    pkgs.input-mono
    pkgs.libertine

    pkgs.direnv
  ];

  home.sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

  programs.emacs =
    let e = import ./emacs.nix { inherit pkgs; };
    in {
      enable = true;
      package = e.emacs;
      extraPackages = e.emacsPackages;
    };
  services.emacs.enable = true;

  services.lorri.enable = true;

  programs.browserpass = {
    enable = true;
    browsers = ["firefox" "chrome"];
  };

  # programs.home-manager = {
  #   enable = true;
  #   path = "/home/rasen/dotfiles/channels/home-manager";
  # };

  services.syncthing.enable = true;

  programs.fish = {
    enable = true;
    shellAliases = {
      g = "git";
    };
    shellInit = ''
      set -gx PATH $HOME/bin $PATH

      if [ "$TERM" = rxvt-unicode-256color ]
        set -x TERM xterm-256color
      end

      eval (direnv hook fish)
    '';
  };
  # programs.bash = {
  #   enable = true;
  #   shellAliases = {
  #     g = "git";
  #   };
  # };


  home.keyboard = {
    layout = "us,ua";
    variant = "workman,";
  };

  xsession.enable = true;
  xsession.windowManager.awesome = {
    enable = true;
    luaModules = [
      pkgs.luaPackages.luafilesystem
      pkgs.luaPackages.cjson
    ];
    # try this?
    # noArgb = true;
  };
  xsession.initExtra = ''
    xkbcomp /home/rasen/dotfiles/.Xkeymap $DISPLAY

    autorandr -c
    xrdb -merge ~/.Xresources
  '';
  # ~/.screenlayout/default.sh

  fonts.fontconfig.enable = true;

  programs.htop.enable = true;
  programs.urxvt = {
    enable = true;
    iso14755 = false;

    fonts = [
      "-*-terminus-medium-r-normal-*-32-*-*-*-*-*-iso10646-1"
    ];

    scroll = {
      bar.enable = false;
      lines = 65535;
      scrollOnOutput = false;
      scrollOnKeystroke = true;
    };
    extraConfig = {
      "loginShell" = "true";
      "urgentOnBell" = "true";
      "secondaryScroll" = "true";

      "background" = "#101010";
      "foreground" = "#d0d0d0";
      "color0" = "#101010";
      "color1" = "#960050";
      "color2" = "#66aa11";
      "color3" = "#c47f2c";
      "color4" = "#30309b";
      "color5" = "#7e40a5";
      "color6" = "#3579a8";
      "color7" = "#9999aa";
      "color8" = "#303030";
      "color9" = "#ff0090";
      "color10" = "#80ff00";
      "color11" = "#ffba68";
      "color12" = "#5f5fee";
      "color13" = "#bb88dd";
      "color14" = "#4eb4fa";
      "color15" = "#d0d0d0";
    };
  };

  programs.autorandr = {
    enable = true;
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
            dpi = 284;
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
            dpi = 284;
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

  programs.zathura = {
    enable = true;
    options = {
      incremental-search = true;
    };
    extraConfig = ''
      map j scroll up
      map k scroll down
    '';
  };

  services.unclutter = {
    enable = true;
  };
}
