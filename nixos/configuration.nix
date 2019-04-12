{ config, pkgs, lib, ... }:
let
  meta = import ./meta.nix;
  machine-config = lib.getAttr meta.name {
    omicron = [
      {
        imports = [
          <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
        ];
      
        boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
        boot.kernelModules = [ "kvm-intel" ];
        boot.extraModulePackages = [ ];
      
        nix.maxJobs = lib.mkDefault 4;
      
        powerManagement.cpuFreqGovernor = "powersave";
      
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
      }
      {
        boot.initrd.luks.devices = [
          {
            name = "root";
            device = "/dev/disk/by-uuid/8b591c68-48cb-49f0-b4b5-2cdf14d583dc";
            preLVM = true;
          }
        ];
        fileSystems."/boot" = {
          device = "/dev/disk/by-uuid/BA72-5382";
          fsType = "vfat";
        };
        fileSystems."/" = {
          device = "/dev/disk/by-uuid/434a4977-ea2c-44c0-b363-e7cf6e947f00";
          fsType = "ext4";
          options = [ "noatime" "nodiratime" "discard" ];
        };
        fileSystems."/home" = {
          device = "/dev/disk/by-uuid/8bfa73e5-c2f1-424e-9f5c-efb97090caf9";
          fsType = "ext4";
          options = [ "noatime" "nodiratime" "discard" ];
        };
        swapDevices = [
          { device = "/dev/disk/by-uuid/26a19f99-4f3a-4bd5-b2ed-359bed344b1e"; }
        ];
      }
      {
        services.xserver.libinput = {
          enable = true;
          accelSpeed = "0.7";
        };
      }
      {
        i18n = {
          consolePackages = [
            pkgs.terminus_font
          ];
          consoleFont = "ter-132n";
        };
      }
      {
        boot.loader.grub.gfxmodeEfi = "1024x768";
      }
      {
        services.xserver.dpi = 276;
      }
    ];
  };

in
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
        extraGroups = [ "users" "wheel" "input" ];
        initialPassword = "HelloWorld";
      };
    }
    {
      nix.nixPath = [ "nixpkgs-overlays=/home/rasen/dotfiles/nixpkgs-overlays" ];
    }
    {
      nix.useSandbox = "relaxed";
    }
    {
      hardware.bluetooth.enable = true;
      hardware.pulseaudio = {
        enable = true;
    
        # NixOS allows either a lightweight build (default) or full build
        # of PulseAudio to be installed.  Only the full build has
        # Bluetooth support, so it must be selected here.
        package = pkgs.pulseaudioFull;
      };
    }
    {
      environment.systemPackages = [
        pkgs.ntfs3g
      ];
    }
    {
      networking = {
        hostName = meta.name;
    
        networkmanager.enable = true;
    
        # disable wpa_supplicant
        wireless.enable = false;
      };
    
      users.extraUsers.rasen.extraGroups = [ "networkmanager" ];
    
      environment.systemPackages = [
        pkgs.networkmanagerapplet
      ];
    }
    {
      hardware.pulseaudio = {
        enable = true;
        support32Bit = true;
      };
    
      environment.systemPackages = [ pkgs.pavucontrol ];
    }
    {
      services.locate = {
        enable = true;
        localuser = "rasen";
      };
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
      services.openssh = {
        enable = true;
        passwordAuthentication = false;
      };
    }
    {
      services.openssh = {
        # Doing this won't open firewall for everybody.
        ports = [];
        listenAddresses = [
          { addr = "0.0.0.0"; port = 22; }
        ];
      };
    
      # Open firewall for tap0 only
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
      services.dnsmasq = {
        enable = true;
    
        # These are used in addition to resolv.conf
        servers = [
          "8.8.8.8"
          "8.8.4.4"
        ];
    
        extraConfig = ''
          listen-address=127.0.0.1
          cache-size=1000
    
          no-negcache
        '';
      };
    }
    {
      services.syncthing = {
        enable = true;
        user = "rasen";
        dataDir = "/home/rasen/.config/syncthing";
        openDefaultPorts = true;
      };
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
      services.postgresql.enable = true;
    }
    {
      virtualisation.docker.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.isync
      ];
    }
    {
      services.dovecot2 = {
        enable = true;
        enablePop3 = false;
        enableImap = true;
        mailLocation = "maildir:~/Mail:LAYOUT=fs";
      };
    
      # dovecot has some helpers in libexec (namely, imap).
      environment.pathsToLink = [ "/libexec/dovecot" ];
    }
    {
      environment.systemPackages = [
        pkgs.msmtp
      ];
    }
    {
      environment.systemPackages = [
        pkgs.notmuch
      ];
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
      services.xserver.displayManager.slim.enable = true;
      services.xserver.windowManager = {
        default = "awesome";
        awesome = {
          enable = true;
          luaModules = [ pkgs.luaPackages.luafilesystem pkgs.luaPackages.cjson ];
        };
      };
    }
    {
      services.xserver.desktopManager.xterm.enable = false;
    }
    {
      environment.systemPackages = [
        pkgs.wmname
        pkgs.xclip
        pkgs.escrotum
      ];
    }
    {
      services.xserver.layout = "us,ua";
      services.xserver.xkbVariant = "workman,";
    
      # Use same config for linux console
      i18n.consoleUseXkbConfig = true;
    }
    {
      services.xserver.xkbOptions = "grp:caps_toggle,grp:menu_toggle,grp_led:caps";
    }
    {
      services.redshift = {
        enable = true;
        latitude = "50.4500";
        longitude = "30.5233";
      };
    }
    {
      hardware.acpilight.enable = true;
      environment.systemPackages = [
        pkgs.acpilight
      ];
      users.extraUsers.rasen.extraGroups = [ "video" ];
    }
    {
      imports = [
        <nixpkgs/nixos/modules/hardware/acpilight.nix>
      ];
    }
    {
      environment.systemPackages = [
        pkgs.oxygen-icons5
      ];
    }
    (let
      oldpkgs = import (pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "1aa77d0519ae23a0dbef6cab6f15393cfadcc454";
        sha256 = "1gcd8938n3z0a095b0203fhxp6lddaw1ic1rl33q441m1w0i19jv";
      }) { config = config.nixpkgs.config; };
    in {
      environment.systemPackages = [ oldpkgs.oxygen-gtk2 oldpkgs.oxygen-gtk3 ];
    
      environment.shellInit = ''
        export GTK_PATH=$GTK_PATH:${oldpkgs.oxygen_gtk}/lib/gtk-2.0
        export GTK2_RC_FILES=$GTK2_RC_FILES:${oldpkgs.oxygen_gtk}/share/themes/oxygen-gtk/gtk-2.0/gtkrc
      '';
    })
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
        pkgs.gnupg
        pkgs.pinentry
      ];
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
      };
    
      systemd.user.sockets.gpg-agent-ssh = {
        wantedBy = [ "sockets.target" ];
        listenStreams = [ "%t/gnupg/S.gpg-agent.ssh" ];
        socketConfig = {
          FileDescriptorName = "ssh";
          Service = "gpg-agent.service";
          SocketMode = "0600";
          DirectoryMode = "0700";
        };
      };
    
      services.pcscd.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.yubikey-manager
        pkgs.yubikey-personalization
        pkgs.yubikey-personalization-gui
      ];
    
      services.udev.packages = [ pkgs.yubikey-personalization ];
    }
    {
      environment.systemPackages = [
        (pkgs.pass.withExtensions (exts: [ exts.pass-otp ]))
      ];
    }
    {
      programs.browserpass.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.gwenview
        pkgs.dolphin
        pkgs.kdeFrameworks.kfilemetadata
        pkgs.filelight
        pkgs.shared_mime_info
      ];
    }
    {
      environment.pathsToLink = [ "/share" ];
    }
    {
      environment.systemPackages = [
        pkgs.google-chrome
      ];
    }
    {
      environment.systemPackages = [
        pkgs.firefox
        pkgs.icedtea_web
      ];
    }
    (let
      oldpkgs = import (pkgs.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs-channels";
        rev = "14cbeaa892da1d2f058d186b2d64d8b49e53a6fb";
        sha256 = "0lfhkf9vxx2l478mvbmwm70zj3vfn9365yax7kvm7yp07b5gclbr";
      }) { config = { firefox.icedtea = true; }; };
    in {
      nixpkgs.config.firefox = {
        icedtea = true;
      };
    
      environment.systemPackages = [
        (pkgs.runCommand "firefox-esr" { preferLocalBuild = true; } ''
          mkdir -p $out/bin
          ln -s ${oldpkgs.firefox-esr}/bin/firefox $out/bin/firefox-esr
        '')
      ];
    })
    {
      environment.systemPackages = [
        pkgs.zathura
      ];
    }
    {
      programs.slock.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.xss-lock
      ];
    }
    {
      environment.systemPackages = [
        pkgs.libreoffice
        pkgs.qbittorrent
        pkgs.google-play-music-desktop-player
        pkgs.deadbeef
        pkgs.tdesktop # Telegram
    
        pkgs.mplayer
        pkgs.smplayer
    
        # Used by naga setup
        pkgs.xdotool
    
        pkgs.hledger
        pkgs.drive
      ];
    }
    {
      environment.systemPackages = [
        (pkgs.vim_configurable.override { python3 = true; })
        pkgs.neovim
      ];
    }
    {
      services.emacs = {
        enable = true;
        defaultEditor = true;
        package = (pkgs.emacsPackagesNgGen pkgs.emacs).emacsWithPackages (epkgs:
          (with epkgs.melpaStablePackages; [
            use-package
            diminish
            el-patch
    
            evil
            evil-numbers
            evil-swap-keys
            evil-collection
            evil-surround
    
            smex
            counsel
            whitespace-cleanup-mode
            which-key
            projectile
    
            diff-hl
            yasnippet
            company
            flycheck
            color-identifiers-mode
            f
    
            avy
            wgrep
            org-pomodoro
            nix-mode
            haskell-mode
            rust-mode
            racer
            pip-requirements
            js2-mode
            rjsx-mode
            typescript-mode
            tide
            php-mode
            web-mode
            groovy-mode
            go-mode
            lua-mode
            ledger-mode
            markdown-mode
            edit-indirect
            json-mode
            yaml-mode
            jinja2-mode
            gitconfig-mode
            terraform-mode
            graphviz-dot-mode
            fish-mode
            notmuch
            airline-themes
            visual-fill-column
            beacon
            google-translate
            writegood-mode
            edit-server
          ]) ++
          (with epkgs.melpaPackages; [
            # Not present in melpa-stable
            general
            flycheck-jest
            purescript-mode
            psc-ide
            restclient
            mbsync
            nix-sandbox
            prettier-js
            flycheck-rust
            flycheck-inline
    
            # Don't work in melpa-stable
            counsel-projectile
            ivy
    
            # Don't work / too old in melpa-stable
            magit
            evil-magit
    
            # too old
            evil-org
          ]) ++
          [
            epkgs.orgPackages.org-plus-contrib
    
            pkgs.ycmd
          ]
        );
      };
      environment.systemPackages = [
        pkgs.ripgrep
        (pkgs.aspellWithDicts (dicts: with dicts; [en en-computers en-science ru uk]))
    
        pkgs.rustc
        pkgs.cargo
        pkgs.rustracer
      ];
      environment.variables.RUST_SRC_PATH = "${pkgs.rustPlatform.rustcSrc}";
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
      users.defaultUserShell = pkgs.fish;
    }
    {
      environment.systemPackages = [
        pkgs.qrencode
        pkgs.feh
      ];
    }
    {
      programs.zsh.enable = true;
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
        pkgs.minicom
        pkgs.openocd
        pkgs.telnet
        pkgs.saleae-logic
      ];
    }
    {
      users.extraGroups.plugdev = { };
      users.extraUsers.rasen.extraGroups = [ "plugdev" "dialout" ];
    
      services.udev.packages = [ pkgs.openocd pkgs.android-udev-rules ];
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
        pkgs.bind
        pkgs.file
        pkgs.which
        pkgs.utillinuxCurses
    
        pkgs.patchelf
    
        pkgs.nox
    
        pkgs.python
        pkgs.python3
    
        pkgs.awscli
        pkgs.nodejs-10_x
        pkgs.shellcheck
    
        pkgs.irssi
      ];
    }
    {
      environment.systemPackages = [
        pkgs.nethack
      ];
    }
  ] ++ machine-config;
}
