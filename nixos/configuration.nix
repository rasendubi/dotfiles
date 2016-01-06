# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/b27c07d0-aaf7-44a1-87e1-5a2cb30954ec";
    fsType = "ext4";
  };
  swapDevices = [
    { device = "/dev/disk/by-uuid/f0bd0438-3324-4295-9981-07015fa0af5e"; }
    { device = "/dev/disk/by-uuid/75822d9d-c5f0-495f-b089-f57d0de5246d"; }
  ];

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.firefox.jre = true;

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    extraEntries = ''
      menuentry 'Gentoo' {
        configfile (hd1,1)/grub2/grub.cfg
      }
    '';
  };

  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
  boot.initrd.kernelModules = [ "wl" ];

  networking = {
    hostName = "Larry";

    wicd.enable = true;
    interfaceMonitor.enable = false;
    wireless.enable = false;
    # useDHCP = false;
  };

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # for steam
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  time.timeZone = "Europe/Kiev";

  environment.systemPackages = with pkgs; [
    kde4.kde_baseapps
    kde4.oxygen_icons
    kde4.konsole
    kde4.kde_runtime
    kde4.kdeartwork
    shared_mime_info

    vpnc

    wget
    (vim_configurable.override { python3 = true; })
    emacs
    rxvt_unicode
    zsh
    htop
    psmisc # for killall
    vlc
    google-chrome
    # firefox
    # (wrapFirefox { browser = firefox; })
    firefoxWrapper
    skype
    steam
    # mnemosyne # The one at upstream is broken. Fix is already in master
    libreoffice
    nix-repl
    irssi
    qbittorrent
    calibre

    python
    python3

    # awesome wm setup
    wmname
    kbdd
    xclip
    # xxkb # It's in nixpkgs' master already but not in channel.

    # do I need this for regular setup?
    gnumake
    binutils
    gcc
    gcc-arm-embedded
    ghc
    stack
    minicom
    openocd
  ];

  services.openssh.enable = true;

  services.printing.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us,ru,ua";
  services.xserver.xkbOptions = "grp_led:caps,grp:caps_toggle,grp:menu_toggle";

  services.xserver.displayManager.slim.enable = true;
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.windowManager.awesome = {
    enable = true;
    luaModules = [ pkgs.luaPackages.luafilesystem ];
  };

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    vertEdgeScroll = true;
  };

  programs.zsh.enable = true;

  users.extraUsers.rasen = {
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "users" "wheel" "networkmanager" "dialout" "plugdev" ];
    shell = "/var/run/current-system/sw/bin/zsh";
    initialPassword = "HelloWorld";
  };

  users.extraGroups = {
    plugdev = { };
  };

  services.udev.packages = with pkgs; [ openocd ];

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = false;

    fonts = with pkgs; [
      corefonts
      terminus_font
      dejavu_fonts
      source-code-pro
      hasklig
       #pkgs.cantarell_fonts
       #pkgs.dejavu_fonts
       #pkgs.dosemu_fonts
       #pkgs.freefont_ttf
       #pkgs.liberation_ttf
       pkgs.terminus_font
       #pkgs.ubuntu_font_family
       #pkgs.ucsFonts
       #pkgs.unifont
       #pkgs.vistafonts
       #pkgs.xlibs.fontadobe100dpi
       #pkgs.xlibs.fontadobe75dpi
       #pkgs.xlibs.fontadobeutopia100dpi
       #pkgs.xlibs.fontadobeutopia75dpi
       #pkgs.xlibs.fontadobeutopiatype1
       #pkgs.xlibs.fontarabicmisc
       pkgs.xlibs.fontbh100dpi
       pkgs.xlibs.fontbh75dpi
       pkgs.xlibs.fontbhlucidatypewriter100dpi
       pkgs.xlibs.fontbhlucidatypewriter75dpi
       pkgs.xlibs.fontbhttf
       pkgs.xlibs.fontbhtype1
       pkgs.xlibs.fontbitstream100dpi
       pkgs.xlibs.fontbitstream75dpi
       pkgs.xlibs.fontbitstreamtype1
       #pkgs.xlibs.fontcronyxcyrillic
       pkgs.xlibs.fontcursormisc
       pkgs.xlibs.fontdaewoomisc
       pkgs.xlibs.fontdecmisc
       pkgs.xlibs.fontibmtype1
       pkgs.xlibs.fontisasmisc
       pkgs.xlibs.fontjismisc
       pkgs.xlibs.fontmicromisc
       pkgs.xlibs.fontmisccyrillic
       pkgs.xlibs.fontmiscethiopic
       pkgs.xlibs.fontmiscmeltho
       pkgs.xlibs.fontmiscmisc
       pkgs.xlibs.fontmuttmisc
       pkgs.xlibs.fontschumachermisc
       pkgs.xlibs.fontscreencyrillic
       pkgs.xlibs.fontsonymisc
       pkgs.xlibs.fontsunmisc
       pkgs.xlibs.fontwinitzkicyrillic
       pkgs.xlibs.fontxfree86type1
    ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
