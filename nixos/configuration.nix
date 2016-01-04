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
    wget
    (vim_configurable.override { python3 = true; })
    emacs
    rxvt_unicode
    zsh
    htop
    kde5.dolphin
    kde5.konsole
    psmisc # for killall
    vlc
    google-chrome
    firefox
    skype
    steam
    # mnemosyne # The one at upstream is broken. Fix is already in master
    libreoffice

    python
    python3

    # awesome wm setup
    wmname
    kbdd
    xclip
    # xxkb # It's in nixpkgs' master already but not in channel.

    # do I need this for regular setup?
    gnumake
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
    extraGroups = [ "wheel" "networkmanager" ];
    shell = "/var/run/current-system/sw/bin/zsh";
    initialPassword = "HelloWorld";
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
