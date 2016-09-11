{ config, pkgs, ... }:
{
  imports = [
    {
      nixpkgs.config.allowUnfree = true;

      # The NixOS release to be compatible with for stateful data such as databases.
      system.stateVersion = "15.09";
    }

    {
      nix.nixPath =
        let dotfiles = "/home/rasen/dotfiles";
        in [
          "nixos-config=${dotfiles}/nixos/configuration.nix"
          "dotfiles=${dotfiles}"
          "${dotfiles}/channels"
        ];
    }
    {
      system.copySystemConfiguration = true;
    }
    {
      users.extraUsers.rasen = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "users" "wheel" ];
        initialPassword = "HelloWorld";
      };
    }
    {
      imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];
    
      boot.initrd.availableKernelModules = [ "ahci" "xhci_hcd" ];
      boot.initrd.kernelModules = [ "wl" ];
    
      boot.kernelModules = [ "kvm-intel" "wl" ];
      boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
    }
    {
      fileSystems = {
        "/" = {
          device = "/dev/disk/by-uuid/ba82dd25-a9e5-436f-ae76-4ee44d53b2c6";
          fsType = "ext4";
        };
        "/home" = {
          device = "/dev/disk/by-uuid/b27c07d0-aaf7-44a1-87e1-5a2cb30954ec";
          fsType = "ext4";
        };
      };
    }
    {
      swapDevices = [
        # TODO: set priority
        # { device = "/dev/disk/by-uuid/f0bd0438-3324-4295-9981-07015fa0af5e"; }
        { device = "/dev/disk/by-uuid/75822d9d-c5f0-495f-b089-f57d0de5246d"; }
      ];
    }
    {
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
    }
    {
      nix.maxJobs = 8;
      nix.buildCores = 8;
    
      networking = {
        hostName = "Larry";
    
        useDHCP = false;
        wicd.enable = true;
        wireless.enable = false;
      };
    
      services.xserver.synaptics = {
        enable = true;
        twoFingerScroll = true;
        vertEdgeScroll = true;
      };
    }
    {
      hardware.nvidiaOptimus.disable = true;
    }
    {
      nix.useSandbox = true;
    }
    {
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
    }
    {
      services.avahi = {
        enable = true;
        browseDomains = [ ];
        interfaces = [ "tap0" ];
        nssmdns = true;
        publish = {
          enable = true;
          addresses = true;
        };
      };
    }
    {
      systemd.services.avahi-daemon.wantedBy = [ "multi-user.target" ];
      systemd.services.avahi-daemon.after = [ "openvpn-kaa.target" ];
    }
    {
      services.openssh = {
        enable = true;
        passwordAuthentication = false;
    
        # Disable default firewall rules
        ports = [];
        listenAddresses = [
          { addr = "0.0.0.0"; port = 22; }
        ];
      };
    
      # allow ssh from VPN network only
      networking.firewall = {
        extraCommands = ''
          ip46tables -D INPUT -i tap0 -p tcp -m tcp --dport 22 -j ACCEPT 2> /dev/null || true
          ip46tables -A INPUT -i tap0 -p tcp -m tcp --dport 22 -j ACCEPT
        '';
      };
    }
    {
      programs.mosh.enable = true;
    }
    {
      services.gitolite = {
        enable = true;
        adminPubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDJhMhxIwZJgIY6CNSNEH+BetF/WCUtDFY2KTIl8LcvXNHZTh4ZMc5shTOS/ROT4aH8Awbm0NjMdW33J5tFMN8T7q89YZS8hbBjLEh8J04Y+kndjnllDXU6NnIr/AenMPIZxJZtSvWYx+f3oO6thvkZYcyzxvA5Vi6V1cGx6ni0Kizq/WV/mE/P1nNbwuN3C4lCtiBC9duvoNhp65PctQNohnKQs0vpQcqVlfqBsjQ7hhj2Fjg+Ofmt5NkL+NhKQNqfkYN5QyIAulucjmFAieKR4qQBABopl2F6f8D9IjY8yH46OCrgss4WTf+wxW4EBw/QEfNoKWkgVoZtxXP5pqAz rasen@Larry";
      };
    }
    {
      services.dnsmasq = {
        enable = true;
    
        # These are used in addition to resolv.conf
        servers = [ "8.8.8.8" "8.8.4.4" ];
    
        extraConfig = ''
          listen-address=127.0.0.1
          cache-size=1000
    
          no-negcache
        '';
      };
    
      # Put the text in /etc/resolv.conf.head
      #
      # That will prepend dnsmasq server to /etc/resolv.conf (dhcpcd-specific)
      environment.etc."resolv.conf.head".text = ''
        nameserver 127.0.0.1
      '';
    }
    {
      networking.firewall = {
        enable = true;
        allowPing = false;
    
        connectionTrackingModules = [];
        autoLoadConntrackHelpers = false;
      };
    }
    {
      services.xserver.enable = true;
    }
    {
      i18n.supportedLocales = [ "en_US.UTF-8/UTF-8" ];
    }
    {
      time.timeZone = "Europe/Kiev";
    }
    {
      services.xserver.displayManager.slim.enable = true;
    }
    {
      services.xserver.windowManager.awesome = {
        enable = true;
        luaModules = [ pkgs.luaPackages.luafilesystem ];
      };
    }
    {
      services.xserver.desktopManager.xterm.enable = false;
    }
    {
      environment.systemPackages = [
        pkgs.wmname
        pkgs.kbdd
        pkgs.xclip
        pkgs.scrot
      ];
    }
    {
      services.xserver.layout = "us,ru,ua";
    }
    {
      services.xserver.xkbOptions = "grp:caps_toggle,grp:menu_toggle,grp_led:caps";
    }
    {
      environment.systemPackages = [
        pkgs.xxkb
      ];
    }
    {
      services.redshift = {
        enable = true;
        latitude = "50.4500";
        longitude = "30.5233";
      };
    }
    {
      environment.systemPackages = [
        pkgs.kde4.oxygen_icons
        pkgs.kde4.kwin_styles
      ];
    }
    {
      environment.systemPackages = [ pkgs.oxygen-gtk2 pkgs.oxygen-gtk3 ];
    
      environment.shellInit = ''
        export GTK_PATH=$GTK_PATH:${pkgs.oxygen_gtk}/lib/gtk-2.0
        export GTK2_RC_FILES=$GTK2_RC_FILES:${pkgs.oxygen_gtk}/share/themes/oxygen-gtk/gtk-2.0/gtkrc
      '';
    }
    {
      environment.systemPackages = [
        pkgs.gnome3.adwaita-icon-theme
      ];
    }
    {
      fonts = {
        enableCoreFonts = true;
        enableFontDir = true;
        enableGhostscriptFonts = false;
    
        fonts = with pkgs; [
          inconsolata
          corefonts
          dejavu_fonts
          source-code-pro
          ubuntu_font_family
          unifont
        ];
      };
    }
    {
      environment.systemPackages = [
        pkgs.kde4.gwenview
        pkgs.kde4.kde_baseapps # <-- dolphin
        pkgs.kde4.kde_runtime
        pkgs.kde4.kfilemetadata
        pkgs.kde4.filelight
        pkgs.shared_mime_info
      ];
    }
    {
      environment.pathsToLink = [ "/share" ];
    }
    {
      environment.systemPackages = [
        pkgs.firefoxWrapper
      ];
    }
    {
      nixpkgs.config.firefox.jre = true;
    }
    {
      nixpkgs.config.packageOverrides = pkgs: rec {
        jrePlugin = pkgs.icedtea_web;
      };
    }
    {
      environment.systemPackages = [
        pkgs.zathura
      ];
    }
    {
      environment.systemPackages = [
        pkgs.google-chrome
        pkgs.skype
        pkgs.libreoffice
        pkgs.qbittorrent
        pkgs.calibre
        pkgs.mnemosyne
        pkgs.deadbeef
        pkgs.wine
        pkgs.vlc
        pkgs.mplayer
        pkgs.smplayer
        pkgs.gparted
        pkgs.unetbootin
        pkgs.kvm
        pkgs.thunderbird
        pkgs.xscreensaver
        pkgs.xss-lock
        pkgs.alarm-clock-applet
      ];
    }
    {
      environment.systemPackages = [
        (pkgs.vim_configurable.override { python3 = true; })
        pkgs.emacs
      ];
    }
    {
      environment.systemPackages = [
        pkgs.ycmd
        pkgs.rustracer
        pkgs.ditaa
        pkgs.jre
      ];
    }
    {
      environment.systemPackages = [
        pkgs.rxvt_unicode
      ];
    }
    {
      fonts = {
        fonts = [
          pkgs.powerline-fonts
          pkgs.terminus_font
        ];
      };
    }
    {
      programs.fish.enable = true;
    }
    {
      programs.zsh.enable = true;
    
      users.defaultUserShell = "/run/current-system/sw/bin/zsh";
    }
    {
      environment.systemPackages = [
        pkgs.gitFull
        pkgs.gitg
      ];
    }
    {
      environment.systemPackages = [
        pkgs.tmux
      ];
    }
    {
      environment.systemPackages = [
        pkgs.ghc
        pkgs.haskellPackages.ghc-mod
        pkgs.stack
        pkgs.cabal-install
        pkgs.cabal2nix
      ];
    }
    {
      environment.systemPackages = [
        pkgs.gnumake
        pkgs.cmake
        pkgs.binutils
        pkgs.gcc
        pkgs.gcc-arm-embedded
        (pkgs.gdb.override { multitarget = true; })
        pkgs.minicom
        pkgs.openocd
        pkgs.expect
        pkgs.telnet
      ];
    }
    {
      users.extraGroups.plugdev = { };
      users.extraUsers.rasen.extraGroups = [ "plugdev" "dialout" ];
    
      services.udev.packages = [ pkgs.openocd ];
    }
    {
      environment.systemPackages = [
        pkgs.arduino
      ];
    }
    {
      environment.systemPackages = [
        pkgs.wget
        pkgs.htop
        pkgs.psmisc
        pkgs.zip
        pkgs.unzip
        pkgs.unrar
        pkgs.p7zip
        pkgs.irssi
        pkgs.bind
        pkgs.file
        pkgs.which
        pkgs.whois
        pkgs.gnupg
        pkgs.utillinuxCurses
    
        pkgs.patchelf
    
        pkgs.man-pages
        pkgs.stdman
        pkgs.posix_man_pages
        pkgs.stdmanpages
    
        pkgs.nix-repl
        pkgs.nox
        pkgs.python
        pkgs.python3
      ];
    }
    {
      environment.systemPackages = [
        pkgs.steam
      ];
    }
    {
      hardware.opengl.driSupport32Bit = true;
      hardware.pulseaudio.support32Bit = true;
    }
    {
      environment.systemPackages = [
        pkgs.nethack
      ];
    }
  ];
}
