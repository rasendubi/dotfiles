{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  nix.buildCores = 8;

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/b27c07d0-aaf7-44a1-87e1-5a2cb30954ec";
    fsType = "ext4";
  };
  swapDevices = [
    # TODO: set priority
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
  };

  # for steam
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  time.timeZone = "Europe/Kiev";

  # Fixes okular
  environment.pathsToLink = [ "/share" ];

  environment.systemPackages = with pkgs; [
    kde4.kde_baseapps
    kde4.oxygen_icons
    kde4.kde_runtime
    kde4.kwin_styles
    kde4.kfilemetadata
    kde4.okular
    kde4.gwenview
    shared_mime_info
    oxygen-gtk2
    oxygen-gtk3

    wget
    (vim_configurable.override { python3 = true; })
    emacs
    rxvt_unicode
    zsh
    htop
    psmisc # for killall
    mosh
    tmux
    zip
    unzip
    unrar
    git
    vlc
    google-chrome
    # firefox
    # (wrapFirefox { browser = firefox; })
    firefoxWrapper
    skype
    steam
    mnemosyne
    libreoffice
    nix-repl
    irssi
    qbittorrent
    calibre
    deadbeef
    wine

    python
    python3

    # awesome wm setup
    wmname
    kbdd
    xclip
    scrot
    xxkb

    # do I need this for regular setup?
    gnumake
    binutils
    gcc
    gcc-arm-embedded
    minicom
    openocd

    ghc
    stack
    cabal2nix
    cabal-install
  ];

  # Install oxygen-gtk
  environment.shellInit = ''
    export GTK_PATH=$GTK_PATH:${pkgs.oxygen_gtk}/lib/gtk-2.0
    export GTK2_RC_FILES=$GTK2_RC_FILES:${pkgs.oxygen_gtk}/share/themes/oxygen-gtk/gtk-2.0/gtkrc
  '';

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    startWhenNeeded = true;
  };

  services.openvpn.servers = {
    kaa.config = ''
      client
      dev tap
      port 22
      proto tcp
      tls-client
      persist-key
      persist-tun
      ns-cert-type server
      remote vpn.kaa.org.ua
      ca /root/.vpn/ca.crt
      key /root/.vpn/alexey.shmalko.key
      cert /root/.vpn/alexey.shmalko.crt
    '';
  };

  services.avahi = {
    enable = true;
    nssmdns = true;
  };

  services.gitolite = {
    enable = false;
    adminPubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhMhxIwZJgIY6CNSNEH+BetF/WCUtDFY2KTIl8LcvXNHZTh4ZMc5shTOS/ROT4aH8Awbm0NjMdW33J5tFMN8T7q89YZS8hbBjLEh8J04Y+kndjnllDXU6NnIr/AenMPIZxJZtSvWYx+f3oO6thvkZYcyzxvA5Vi6V1cGx6ni0Kizq/WV/mE/P1nNbwuN3C4lCtiBC9duvoNhp65PctQNohnKQs0vpQcqVlfqBsjQ7hhj2Fjg+Ofmt5NkL+NhKQNqfkYN5QyIAulucjmFAieKR4qQBABopl2F6f8D9IjY8yH46OCrgss4WTf+wxW4EBw/QEfNoKWkgVoZtxXP5pqAz rasen@Larry";
  };

  services.redshift = {
    # I'm on vacation, so I don't need this now
    # enable = true;
    latitude = "50.4500";
    longitude = "30.5233";
  };

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

  environment.shellAliases = {
    g = "git";
  };

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

  i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" ];

  services.udev.packages = with pkgs; [ openocd ];

  fonts = {
    enableCoreFonts = true;
    enableFontDir = true;
    enableGhostscriptFonts = false;

    fonts = with pkgs; [
      powerline-fonts
      inconsolata
      corefonts
      terminus_font
      dejavu_fonts
      source-code-pro
      ubuntu_font_family
      unifont
    ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
