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
        boot.kernelModules = [ "kvm-intel" "wl" ];
        boot.extraModulePackages = [ config.boot.kernelPackages.rtl88x2bu config.boot.kernelPackages.broadcom_sta ];
      
        hardware.opengl = {
          enable = true;
          extraPackages = [
            pkgs.vaapiIntel
            pkgs.vaapiVdpau
            pkgs.libvdpau-va-gl
          ];
        };
      
        nix.settings.max-jobs = lib.mkDefault 4;
      
        # powerManagement.cpuFreqGovernor = "powersave";
      
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
          touchpad.accelSpeed = "0.7";
        };
      }
      {
        services.xserver.config = ''
          Section "Device"
            Identifier "Intel Graphics"
            Driver "intel"
      
            Option "TearFree" "true"
            Option "TripleBuffer" "true"
          EndSection
        '';
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
          Persistent = lib.mkForce true;
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
      system.stateVersion = "21.05";
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
      imports = [inputs.home-manager.nixosModules.home-manager];
      home-manager = {
        useUserPackages = true;
        useGlobalPkgs = true;
        users.rasen = inputs.self.lib.home-manager-common;
      };
    }
    {
      # for compatibility with nix-shell, nix-build, etc.
      environment.etc.nixpkgs.source = inputs.nixpkgs;
      nix.nixPath = ["nixpkgs=/etc/nixpkgs"];
    
      # register self and nixpkgs as flakes for quick access
      nix.registry = {
        self.flake = inputs.self;
    
        nixpkgs.flake = inputs.nixpkgs;
      };
    }
    {
      nix.settings.sandbox = true;
    }
    {
      users.extraUsers.rasen = {
        isNormalUser = true;
        uid = 1000;
        extraGroups = [ "users" "wheel" ];
        initialPassword = "HelloWorld";
      };
      nix.settings.trusted-users = ["rasen"];
    }
    {
      environment.systemPackages = [ pkgs.xorg.xhost ];
      services.xserver.windowManager.session = lib.singleton {
        name = "exwm";
        start = ''
          xhost +SI:localuser:$USER
          exec emacs
        '';
          # exec ${pkgs.my-emacs}/bin/emacsclient -a "" -c
      };
      services.xserver.displayManager.lightdm.enable = true;
      # services.xserver.displayManager.startx.enable = true;
      services.xserver.displayManager.defaultSession = "none+exwm";
    }
    {
      programs.slock.enable = true;
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
      services.xserver.layout = "us,ua";
      services.xserver.xkbVariant = "workman,";
    
      # Use same config for linux console
      console.useXkbConfig = true;
    }
    {
      services.xserver.xkbOptions = "grp:lctrl_toggle,grp_led:caps,ctrl:nocaps";
      # services.xserver.xkbOptions = "grp:caps_toggle,grp_led:caps";
    }
    {
      users.users.rasen.extraGroups = [ "input" ];
    }
    {
      networking = {
        hostName = name;
    
        networkmanager = {
          enable = true;
          wifi.powersave = false;
        };
    
        # disable wpa_supplicant
        wireless.enable = false;
      };
    
      users.extraUsers.rasen.extraGroups = [ "networkmanager" ];
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
          interface=lo
          bind-interfaces
          listen-address=127.0.0.1
          cache-size=1000
    
          no-negcache
        '';
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
      services.locate = {
        enable = true;
        localuser = "rasen";
      };
    }
    {
      services.gitolite = {
        enable = true;
        user = "git";
        adminPubkey = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHH15uiQw3jBbrdlcRb8wOr8KVltuwbHP/JOFAzXFO1l/4QxnKs6Nno939ugULM7Lu0Vx5g6FreuCOa2NMWk5rcjIwOzjrZnHZ7aoAVnE7H9scuz8NGnrWdc1Oq0hmcDxdZrdKdB6CPG/diGWNZy77nLvz5JcX1kPLZENPeApCERwR5SvLecA4Es5JORHz9ssEcf8I7VFpAebfQYDu+VZZvEu03P2+5SXv8+5zjiuxM7qxzqRmv0U8eftii9xgVNC7FaoRBhhM7yKkpbnqX7IeSU3WeVcw4+d1d8b9wD/sFOyGc1xAcvafLaGdgeCQGU729DupRRJokpw6bBRQGH29 rasen@omicron";
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
      virtualisation.virtualbox.host.enable = true;
      users.extraGroups.vboxusers.members = ["rasen"];
    }
    {
      services.logind = {
        lidSwitchDocked = "ignore";
        lidSwitchExternalPower = "ignore";
      };
    }
    {
      hardware.acpilight.enable = true;
      environment.systemPackages = [ pkgs.acpilight ];
      users.extraUsers.rasen.extraGroups = [ "video" ];
    }
    {
      services.redshift = {
        enable = true;
      };
      location.provider = "geoclue2";
    }
    {
      # PipeWire requires pulseaudio to be disabled.
      hardware.pulseaudio.enable = false;
    
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
    
    
        media-session.config.bluez-monitor.rules = [
          {
            # Matches all cards
            matches = [ { "device.name" = "~bluez_card.*"; } ];
            actions = {
              "update-props" = {
                "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
                # mSBC is not expected to work on all headset + adapter combinations.
                "bluez5.msbc-support" = true;
                # SBC-XQ is not expected to work on all headset + adapter combinations.
                "bluez5.sbc-xq-support" = true;
              };
            };
          }
          {
            matches = [
              # Matches all sources
              { "node.name" = "~bluez_input.*"; }
              # Matches all outputs
              { "node.name" = "~bluez_output.*"; }
            ];
            actions = {
              "node.pause-on-idle" = false;
            };
          }
        ];
      };
    }
    {
      hardware.bluetooth.enable = true;
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
    
      services.udev.packages = [
        pkgs.yubikey-personalization
        pkgs.libu2f-host
      ];
    }
    {
      environment.pathsToLink = [ "/share" ];
    }
    {
      environment.systemPackages = [
        pkgs.vim_configurable
      ];
    }
    {
      programs.fish.enable = true;
      users.defaultUserShell = pkgs.fish;
    }
    {
      environment.systemPackages = [ pkgs.tmux ];
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
        # pkgs.utillinuxCurses
        pkgs.ripgrep
        pkgs.xclip
    
        pkgs.patchelf
    
        pkgs.python3
      ];
      # environment.variables.NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      # environment.variables.PATH = "$HOME/.npm-global/bin:$PATH";
    }
    {
      environment.systemPackages = [ pkgs.git ];
    }
    {
      documentation = {
        man.enable = true;
        dev.enable = true;
      };
    
      environment.systemPackages = [
        pkgs.man-pages
        pkgs.stdman
        pkgs.posix_man_pages
        pkgs.stdmanpages
      ];
    }
    {
      virtualisation.docker.enable = true;
    }
    {
      fonts = {
        fontconfig.enable = true;
        fontDir.enable = true;
        enableGhostscriptFonts = false;
    
        fonts = with pkgs; [
          pkgs.inconsolata
          pkgs.dejavu_fonts
          pkgs.source-code-pro
          pkgs.ubuntu_font_family
          pkgs.unifont
          pkgs.powerline-fonts
          pkgs.terminus_font
        ];
      };
    }
    {
      console.earlySetup = true;
    }
  ] ++ machine-config;
}
