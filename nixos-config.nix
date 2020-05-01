{ name, config, pkgs, lib, inputs, ... }:
let
  machine-config = lib.getAttr name {
    moxps = [
      {
        imports = [
          (import "${inputs.nixos-hardware}/dell/xps/15-9560")
          inputs.nixpkgs.nixosModules.notDetected
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
        boot.initrd.luks.devices = {
          root = {
            device = "/dev/disk/by-uuid/8b591c68-48cb-49f0-b4b5-2cdf14d583dc";
            preLVM = true;
          };
        };
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
        console.packages = [
          pkgs.terminus_font
        ];
        console.font = "ter-132n";
      }
      {
        services.xserver.dpi = 286;
      }
    ];
  };

in
{
  imports = [
    {
      nixpkgs.config.allowUnfree = true;

      # The NixOS release to be compatible with for stateful data such as databases.
      system.stateVersion = "19.09";
    }

    {
      nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };
    }
    {
      nix.nixPath = [
        "nixpkgs=${inputs.nixpkgs}"
      ];
    }
    {
      users.extraUsers.moritz = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "users" "wheel" "input" ];
        initialPassword = "HelloWorld";
      };
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
      services.openvpn.servers.nano-vpn = {
        config = ''
          config /root/openvpn/nano-vpn.ovpn
        '';
      };
    }
    {
      networking = {
        hostName = name;
    
        networkmanager.enable = true;
    
        # disable wpa_supplicant
        wireless.enable = false;
      };
    
      users.extraUsers.moritz.extraGroups = [ "networkmanager" ];
    
      environment.systemPackages = [
        pkgs.networkmanagerapplet
      ];
    }
    {
      services.avahi = {
        enable = true;
        interfaces = [];
        openFirewall = false;
      };
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
        localuser = "moritz";
      };
    }
    {
      services.openssh = {
        enable = true;
        passwordAuthentication = false;
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
        user = "moritz";
        dataDir = "/home/moritz/.config/syncthing";
        configDir = "/home/moritz/.config/syncthing";
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
      virtualisation.docker.enable = true;
    }
    {
      environment.systemPackages = [ pkgs.borgbackup ];
    }
    {
      services.udev.packages = [ pkgs.android-udev-rules ];
      programs.adb.enable = true;
      users.users.moritz.extraGroups = ["adbusers"];
    }
    {
      services.fwupd.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.direnv
      ];
      programs.fish.shellInit = ''
        eval (direnv hook fish)
      '';
    
      services.lorri.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.isync
      ];
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
      time.timeZone = "Europe/Berlin";
    }
    {
      services.xserver.displayManager.lightdm.enable = true;
    }
    {
      services.xserver.windowManager = {
        exwm = {
          enable = true;
        };
      };
      services.xserver.displayManager.defaultSession = "none+exwm";
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
      services.xserver.layout = "de,de,us";
      services.xserver.xkbVariant = "bone,,";
    
      # Use same config for linux console
      console.useXkbConfig = true;
    }
    {
      # services.xserver.xkbOptions = "grp:lctrl_toggle,grp_led:caps,ctrl:nocaps";
    }
    {
      services.redshift = {
        enable = true;
      };
      location.provider = "geoclue2";
    }
    {
      hardware.acpilight.enable = true;
      environment.systemPackages = [
        pkgs.acpilight
      ];
      users.extraUsers.moritz.extraGroups = [ "video" ];
    }
    {
      fonts = {
        enableFontDir = true;
        enableGhostscriptFonts = false;
    
        fonts = with pkgs; [
          inconsolata
          dejavu_fonts
          source-code-pro
          ubuntu_font_family
          unifont
    
          # Used by Emacs
          input-mono
          libertine
        ];
      };
    }
    {
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryFlavor = "qt";
      };
    
      ## is it no longer needed?
      #
      # systemd.user.sockets.gpg-agent-ssh = {
      #   wantedBy = [ "sockets.target" ];
      #   listenStreams = [ "%t/gnupg/S.gpg-agent.ssh" ];
      #   socketConfig = {
      #     FileDescriptorName = "ssh";
      #     Service = "gpg-agent.service";
      #     SocketMode = "0600";
      #     DirectoryMode = "0700";
      #   };
      # };
    
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
      ];
    }
    {
      environment.systemPackages = [
        pkgs.qutebrowser
      ];
    }
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
        pkgs.google-play-music-desktop-player
        pkgs.tdesktop # Telegram
    
        pkgs.mplayer
        pkgs.smplayer
    
        # Used by naga setup
        pkgs.xdotool
      ];
    }
    {
      environment.systemPackages = [
        (pkgs.vim_configurable.override { python3 = true; })
        pkgs.neovim
      ];
    }
    {
      services.emacs =
        let emacsConfig = import .config/nixpkgs/emacs.nix { inherit pkgs; };
        in {
          enable = true;
          defaultEditor = true;
          package = emacsConfig.finalEmacs;
        };
      environment.systemPackages = [
        pkgs.ripgrep
        (pkgs.aspellWithDicts (dicts: with dicts; [en en-computers en-science ru uk]))
    
        # pkgs.rustup
        # pkgs.rustracer
    
        # pkgs.clojure
        # pkgs.leiningen
      ];
      # environment.variables.RUST_SRC_PATH = "${pkgs.rustPlatform.rustcSrc}";
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
    
        pkgs.python3
      ];
      # environment.variables.NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      # environment.variables.PATH = "$HOME/.npm-global/bin:$PATH";
    }
  ] ++ machine-config;
}
