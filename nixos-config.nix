#
# This file is auto-generated from "README.org"
#
{ name, config, pkgs, lib, inputs, ... }:
let
  machine-config = lib.getAttr name {
    omicron = [
      {
        imports = [
          (import "${inputs.nixos-hardware}/dell/xps/13-9360")
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
      (let
        commonOptions = {
          repo = "borg@10.13.0.3:.";
          encryption.mode = "keyfile-blake2";
          encryption.passCommand = "cat /root/secrets/borg";
          compression = "auto,lzma,9";
          doInit = false;
          environment = { BORG_RSH = "ssh -i /root/.ssh/borg"; };
          # UTC timestamp
          dateFormat = "-u +%Y-%m-%dT%H:%M:%S";
        };
      in {
        services.borgbackup.jobs."all" = commonOptions // {
          archiveBaseName = "${config.networking.hostName}";
          paths = [
            "/var/lib/gitolite/"
            "/home/rasen/backup/"
            "/home/rasen/.ssh/"
            "/home/rasen/.gnupg/"
            "/home/rasen/.password-store/"
            "/home/rasen/dotfiles/"
            "/home/rasen/org/"
      
            # Mail
            "/home/rasen/Mail/"
            "/home/rasen/.mbsync/"
          ];
          exclude = [
            # Scanning notmuch takes too much time and doesn't make much
            # sense as it is easily replicable
            "/home/rasen/Mail/.notmuch"
          ];
        };
      
        # Start backup on boot if missed one while laptop was off
        systemd.timers.borgbackup-job-all.timerConfig = {
          Persistent = true;
        };
      
        # Require VPN connection for repo to be reachable
        systemd.services.borgbackup-job-all = {
          requires = ["openvpn-nano-vpn.service"];
        };
      })
      {
        console.packages = [
          pkgs.terminus_font
        ];
        console.font = "ter-132n";
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
      users.extraUsers.rasen = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "users" "wheel" "input" ];
        initialPassword = "HelloWorld";
      };
      nix.trustedUsers = ["rasen"];
    }
    {
      nix.useSandbox = true;
    }
    {
      environment.systemPackages = [ pkgs.naga ];
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
    
      users.extraUsers.rasen.extraGroups = [ "networkmanager" ];
    
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
        localuser = "rasen";
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
      services.gitolite = {
        enable = true;
        user = "git";
        adminPubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHH15uiQw3jBbrdlcRb8wOr8KVltuwbHP/JOFAzXFO1l/4QxnKs6Nno939ugULM7Lu0Vx5g6FreuCOa2NMWk5rcjIwOzjrZnHZ7aoAVnE7H9scuz8NGnrWdc1Oq0hmcDxdZrdKdB6CPG/diGWNZy77nLvz5JcX1kPLZENPeApCERwR5SvLecA4Es5JORHz9ssEcf8I7VFpAebfQYDu+VZZvEu03P2+5SXv8+5zjiuxM7qxzqRmv0U8eftii9xgVNC7FaoRBhhM7yKkpbnqX7IeSU3WeVcw4+d1d8b9wD/sFOyGc1xAcvafLaGdgeCQGU729DupRRJokpw6bBRQGH29 rasen@omicron";
      };
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
        configDir = "/home/rasen/.config/syncthing";
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
      services.udev.packages = [ pkgs.android-udev-rules ];
      programs.adb.enable = true;
      users.users.rasen.extraGroups = ["adbusers"];
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
      time.timeZone = "Europe/Kiev";
    }
    {
      services.xserver.displayManager.lightdm.enable = true;
    }
    {
      services.xserver.windowManager = {
        awesome = {
          enable = true;
          luaModules = [ pkgs.luaPackages.luafilesystem pkgs.luaPackages.cjson ];
        };
      };
      services.xserver.displayManager.defaultSession = "none+awesome";
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
      console.useXkbConfig = true;
    }
    {
      services.xserver.xkbOptions = "grp:lctrl_toggle,grp_led:caps,ctrl:nocaps";
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
      users.extraUsers.rasen.extraGroups = [ "video" ];
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
      services.emacs = {
        enable = true;
        defaultEditor = true;
        package = pkgs.my-emacs;
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
