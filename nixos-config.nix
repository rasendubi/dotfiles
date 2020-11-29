{ name, config, pkgs, lib, inputs, ... }:
let
  machine-config = lib.getAttr name {
    mobook = [
      {
        imports = [
          (import "${inputs.nixos-hardware}/apple/macbook-pro")
          (import "${inputs.nixos-hardware}/common/pc/laptop/ssd")
          # inputs.nixpkgs.modules.hardware.network.broadcom-43xx # <- using import vs not using import?
         #  <nixpkgs/nixos/modules/hardware/network/broadcom-43xx.nix> <- this is when using channels instead of flakes?
          inputs.nixpkgs.nixosModules.notDetected
        ];
      
        services.udev.extraRules =
          # Disable XHC1 wakeup signal to avoid resume getting triggered some time
          # after suspend. Reboot required for this to take effect.
          # lib.optionalString
            # (lib.versionAtLeast pkgs.kernelPackages.kernel.version "3.13")
            ''SUBSYSTEM=="pci", KERNEL=="0000:00:14.0", ATTR{power/wakeup}="disabled"'';
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
            
        # accelerateion
        # nixpkgs.config.packageOverrides = pkgs: {
        #   vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
        # };
        # hardware.opengl = {
        #   enable = true;
        #   extraPackages = with pkgs; [
        #     intel-media-driver # LIBVA_DRIVER_NAME=iHD
        #     vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
        #     vaapiVdpau
        #     libvdpau-va-gl
        #   ];
        # };
      
        boot.kernelModules = [ "kvm-intel" ];
      
        # powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
      }
      {
     boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/E64F-3226";
      fsType = "vfat";
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/912c5850-5f71-4d15-8b69-1e0dad5718b0"; }
    ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/73edc386-3f1a-46ff-9ae1-76a4fd6c0ea4";
      fsType = "btrfs";
    };

  boot.initrd.luks.devices = {
    cryptkey = {
      device = "/dev/disk/by-uuid/179ecdea-edd4-4dc5-b8c3-5ed760bc2a0d";
    };
    cryptroot = {
      device = "/dev/disk/by-uuid/623db0a5-d0e0-405a-88ae-b83a3d321656";
      keyFile = "/dev/mapper/cryptkey";
    };
    cryptswap = {
      device = "/dev/disk/by-uuid/da63991e-8edd-48db-bc4b-66fbc96917eb";
      keyFile = "/dev/mapper/cryptkey";
    };
  };


  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  # high-resolution display
  hardware.video.hidpi.enable = lib.mkDefault true;
}


      
      {
        services.xserver.libinput = {
          enable = true;
          accelSpeed = "0.7";
        };
        # displayManager.lightdm.greeters.gtk.cursorTheme = {  # TODO if home manager cursor doesnt work
        #   name = "Vanilla-DMZ";
        #   package = pkgs.vanilla-dmz;
        #   size = 64;
        # };
      }
    ];
  };
  # nur-no-pkgs = import (builtins.fetchTarball {
  #   url = "https://github.com/nix-community/NUR/archive/master.tar.gz";
  #   sha256 = "10dq8abmw30lrpwfg7yb1zn6fb5d2q94yhsvg6dwcknn46nilbxs";
  # }) {
  #     nurpkgs = pkgs;
  #     inherit pkgs;
  #     repoOverrides = {
  #       moritzschaefer = import /home/moritz/Projects/nur-packages;
  #     };
  #   };
in
{
  disabledModules = [ "services/x11/window-managers/exwm.nix"  ]; 
  imports = [
    (import "${inputs.nixpkgs-moritz}/nixos/modules/services/x11/window-managers/exwm.nix")
    (import "${inputs.musnix}")
    {
    
      nixpkgs.config.allowUnfree = true;

      # The NixOS release to be compatible with for stateful data such as databases.
      system.stateVersion = "20.09";
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
      environment.systemPackages = [
        pkgs.home-manager
      ];
    }
    {
      hardware.bluetooth.enable = true;
      hardware.bluetooth.powerOnBoot = false;
      services.blueman.enable = true;
      hardware.bluetooth.config.General.Enable = "Source,Sink,Media,Socket";
      hardware.pulseaudio = {
        enable = true;
    
        # NixOS allows either a lightweight build (default) or full build
        # of PulseAudio to be installed.  Only the full build has
        # Bluetooth support, so it must be selected here.
    
        extraModules = [ pkgs.pulseaudio-modules-bt ];
        # package = pkgs.pulseaudioFull;
      };
    }
    {
      environment.systemPackages = [
        pkgs.ntfs3g
      ];
    }
    {
      environment.systemPackages = [
        pkgs.sshfs
      ];
      fileSystems."/mnt/cclab_nas" = {
        device = "//nas22.ethz.ch/biol_imhs_ciaudo";
        fsType = "cifs";
        options = [ "credentials=/home/moritz/.secret/cclab_nas.credentials" "workgroup=d.ethz.ch" "uid=moritz" "gid=users" "noauto"];
      };
    
    # https://releases.nixos.org/nix-dev/2016-September/021763.html  TODO not working :/
      fileSystems."/mnt/cclab_server" = let
        sshAsUser = user: 
          pkgs.writeScript "ssh_as_${user}" ''
            exec ${pkgs.sudo}/bin/sudo -i -u ${user} \
              ${pkgs.openssh}/bin/ssh $@
          '';
      in {
        # device = "sshfs#schamori@mhs-cclab-srv001.ethz.ch:/";
        fsType = "fuse";
        device = "${pkgs.sshfsFuse}/bin/sshfs#schamori@mhs-cclab-srv001.ethz.ch:/";
        options = [
                "noauto" "_netdev" "allow_other" "x-gvfs-hide" #"reconnect"  # "x-systemd.automount" 
                "ServerAliveInterval=5" "ServerAliveCountMax=1"
                "uid=30925" "gid=100" "umask=0"   # TODO comment if fails
                "ssh_command=${sshAsUser "moritz"}"
              ];
      };
      
      # https://soultrace.net/mount-network-share-after-boot/ <- more beautiful
      networking.networkmanager.dispatcherScripts = [
        {
          source = pkgs.writeText "mountHook" ''
            if [ "$2" != "vpn-up" ]; then
                logger "exit: event $2 != vpn-up"
                exit
            fi
            mount /mnt/cclab_nas
            # mount /mnt/cclab_server
            logger "Mounted cclab_nas"
          '';
          type = "basic";
        }
        {
          source = pkgs.writeText "umountHook" ''
            if [ "$2" != "vpn-pre-down" ]; then
                logger "exit: event $2 != vpn-pre-down"
                exit
            fi
            umount -a -l -t cifs
            umount /mnt/cclab_server
            logger "Unmounted cclab_nas"
          '';
          type = "pre-down";
        }
      ];
      
      systemd.services.suspend-disconnect = {
        description = "Disconnect VPN before suspend";
        wantedBy = [ "systemd-suspend.service" ];
        before = [ "systemd-suspend.service" ];
        script = ''
          /run/current-system/sw/bin/nmcli con down id VPN\ ETHZ 2> /tmp/suspend
        '';
        serviceConfig.Type = "oneshot";
      };
      # systemd.services.tun-connect = {
      #   wants = [ "sys-devices-virtual-net-tun0.device" ];
      #   after = [ "sys-devices-virtual-net-tun0.device" ];
      #   requires = [];
      #   services.systemd-logind.environment.SYSTEMD_LOG_LEVEL
      #   requires
      #   script = ''
      #   echo "cte" > /tmp/vpn
      #   mount /mnt/cclab_nas
      #   '';
      # };
      # powerManagement.powerDownCommands = "\"fusermount -u /home/moritz/sshfs \"\n\"echo ieie > /tmp/testt\"";  # doesn't work (at least not without reboot..)
    }
    {
      system.autoUpgrade.enable = true;
    }
    {
      systemd.timers.hibernate-on-low-battery = {
        wantedBy = [ "multi-user.target" ];
        timerConfig = {
          OnUnitActiveSec = "120";
          OnBootSec= "120";
        };
      };
      systemd.services.hibernate-on-low-battery =
        let
          battery-level-sufficient = pkgs.writeShellScriptBin
            "battery-level-sufficient" ''
            test "$(cat /sys/class/power_supply/BAT0/status)" != Discharging \
              || test "$(cat /sys/class/power_supply/BAT0/capacity)" -ge 5
          '';
        in
          {
            serviceConfig = { Type = "oneshot"; };
            onFailure = [ "hibernate.target" ];
            script = "${battery-level-sufficient}/bin/battery-level-sufficient";
          };
    }
    {
      nix.gc.automatic = true;
      nix.gc.options = "--delete-older-than 12d";
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
        zeroconf.discovery.enable = true;
        systemWide = false;
        package = pkgs.pulseaudioFull; # .override { jackaudioSupport = true; };  # need "full" for bluetooth
      };
    
      environment.systemPackages = with pkgs; [ pavucontrol libjack2 jack2 qjackctl jack2Full jack_capture ];
    
      # services.jack = {
      #   jackd.enable = true;
      #   # support ALSA only programs via ALSA JACK PCM plugin
      #   alsa.enable = false;
      #   # support ALSA only programs via loopback device (supports programs like Steam)
      #   loopback = {
      #     enable = true;
      #     # buffering parameters for dmix device to work with ALSA only semi-professional sound programs
      #     #dmixConfig = ''
      #     #  period_size 2048
      #     #'';
      #   };
      # };
      # boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];
    
      users.extraUsers.moritz.extraGroups = [ "audio" ];  # "jackaudio" 
    
      # from https://github.com/JeffreyBenjaminBrown/nixos-experiments/blob/6c4be545e2ec18c6d9b32ec9b66d37c59d9ebc1f/audio.nix
      security.sudo.extraConfig = ''
        moritz  ALL=(ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl
        '';
      musnix = {
        enable = true;
        alsaSeq.enable = false;
    
        # Find this value with `lspci | grep -i audio` (per the musnix readme).
        # PITFALL: This is the id of the built-in soundcard.
        #   When I start using the external one, change it.
        soundcardPciId = "00:1f.3";
    
        # If I build with either of these, I get a PREEMPT error, much like
        #   https://github.com/musnix/musnix/issues/100
        # kernel.realtime = true;
        # kernel.optimize = true;
    
        # das_watchdog.enable = true;
          # I don't think this does anything without the realtime kernel.
    
        # magic to me
        rtirq = {
          # highList = "snd_hrtimer";
          resetAll = 1;
          prioLow = 0;
          enable = true;
          nameList = "rtc0 snd";
        };
      };
        
    
    }
    {
      services.printing.enable = true;
      services.printing.drivers = with pkgs; [
        gutenprint
        gutenprintBin
        samsungUnifiedLinuxDriver
        splix
      ];
      services.system-config-printer.enable = true;
    }
    {
      services.locate = {
        enable = true;
        localuser = "moritz";
      };
    }
    {
      services.openssh = {
        enable = false;
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
      environment.systemPackages = [
        pkgs.docker-compose
        pkgs.kvm
        pkgs.qemu
      ];
    
      users.users.moritz.extraGroups = ["libvirtd"];  # required for qemu I think 
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
      # services.udisks2.enable = true;
      services.devmon.enable = true;
    }
    {
      services.logind.extraConfig = ''
        HandlePowerKey=suspend
      '';
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
      services.xserver.displayManager = {
        autoLogin = {
          user = "moritz";
          enable = true;
        };
        lightdm = {
          enable = true;
        };
      };
    }
    {
      services.xserver.windowManager = {
        exwm = {
          enable = true;
          extraPackages = epkgs: with epkgs; [ emacsql-sqlite pkgs.imagemagick ];  # unfortunately, adding zmq and jupyter here, didn't work so I had to install them manually (i.e. compiling emacs-zmq)
          enableDefaultConfig = false;  # todo disable and enable loadScript
          # careful, 'loadScript option' was merged from Vizaxo into my personal nixpkgs repo.
          loadScript = ''
            (require 'exwm)
            ;; most of it is now in .spacemacs.d/lisp/exwm.el
            ;; (require 'exwm-systemtray)
            (require 'exwm-randr)
            ;; (setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "HDMI1" 2 "DP2" 3 "eDP1" 4 "HDMI1" 5 "DP2"))
            ;; (setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "eDP1" 2 "HDMI1" 3 "eDP1" 4 "eDP1" 5 "eDP1"))
            ;; (exwm-randr-enable)
            ;; (exwm-systemtray-enable)
            (exwm-enable)
          '';
        };
        stumpwm.enable = true;
      };
      services.xserver.displayManager.defaultSession = "none+exwm";  # Firefox works more fluently with plasma5+exwm instead of "none+exwm". or does it??
      services.xserver.desktopManager = {
        xterm.enable = false;
        plasma5.enable = true;
        xfce = {
          enable = true;
          noDesktop= true;
          enableXfwm = true;
        };
      };
      services.picom.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.wmname
        pkgs.xclip
        pkgs.escrotum
      ];
    }
    {
      environment.systemPackages = with pkgs; [
        dunst
      ];
      systemd.user.services."dunst" = {
        enable = true;
        description = "";
        wantedBy = [ "default.target" ];
        serviceConfig.Restart = "always";
        serviceConfig.RestartSec = 2;
        serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
      };
    }
    {
      services.xserver.layout = "de,de,us";
      services.xserver.xkbVariant = "bone,,";
      services.xserver.xkbOptions= "terminate:ctrl_alt_bksp";
    
      # Use same config for linux console
      console.useXkbConfig = true;
    }
    {
      services.xserver.autoRepeatDelay = 180;
      services.xserver.autoRepeatInterval = 50;
    
      # Use same config for linux console
      console.useXkbConfig = true;
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
        pkgs.brightnessctl
      ];
      users.extraUsers.moritz.extraGroups = [ "video" ];
    }
    {
      fonts = {
        # fontDir.enable = true; # 21.03 rename
        enableFontDir = true;
        enableGhostscriptFonts = false;
    
        fonts = with pkgs; [
          corefonts
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
      programs.ssh = {
        startAgent = true;
      };
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = false;
        pinentryFlavor = "qt";
      };
    
      # is it no longer needed?
      
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
      environment.systemPackages = with pkgs; [
        (pass.withExtensions (exts: [ exts.pass-otp ]))
        pinentry-curses
        pinentry-qt
        pinentry-emacs
      ];
      # services.keepassx.enable = true;
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
        (pkgs.firefox.override { extraNativeMessagingHosts = [ pkgs.passff-host ]; })
      ];
    }
    {
      environment.systemPackages = [
        pkgs.qutebrowser
      ];
      environment.variables.QUTE_BIB_FILEPATH = "/home/moritz/wiki/papers/references.bib";
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
      environment.systemPackages = with pkgs; [
        igv
      ];
    }
    {
      environment.systemPackages =
        let wrapper = pkgs.writeScriptBin "spotify-highres" ''
          #!${pkgs.stdenv.shell}
          exec ${pkgs.spotify}/bin/spotify --force-device-scale-factor=2
          '';
      in
         [ pkgs.spotify wrapper ];
    }
    {
      services.tor.enable = false;
      services.tor.client.enable = false;
    }
    {
      environment.systemPackages = [ pkgs.steam ];
      hardware.opengl.driSupport32Bit = true;
      hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva vaapiIntel];
      hardware.pulseaudio.support32Bit = true;
    }
    {
      environment.systemPackages = with pkgs; [
        dmenu
        soulseekqt
        gnome3.cheese
        gnome3.gnome-screenshot
        pandoc   # TODO make a latex section
        # haskellPackages.pandoc-crossref  # broken...
        haskellPackages.pandoc-citeproc
        # texlive.combined.scheme-full
        sparkleshare
        gnome3.gpaste
        autorandr
        
        # kdenlive  # fails in current unstable
        audacity
        google-play-music-desktop-player
        tdesktop # Telegram
        signal-cli # Signal
        signal-desktop # Signal
        zoom-us
        libreoffice
        wineWowPackages.stable
        # winetricks  # requires p7zip (which is unsafe...)
        gimp-with-plugins
    
        mplayer
        smplayer
        lm_sensors
        tcl
    
        # Used by naga setup
        xdotool
      ];
    }
    {
      environment.variables.XDG_CONFIG_DIRS = [ "/etc/xdg" ]; # we should probably have this in NixOS by default
      environment.etc."xdg/mimeapps.list" = {
        text = ''
          [Default Applications]
          image/png=inkscape.desktop;
          image/svg+xml=inkscape.desktop;
        '';
      };
    }
    {
      environment.variables.EDITOR = "vim";
      environment.systemPackages = [
        (pkgs.vim_configurable.override { python3 = true; })
        pkgs.neovim
      ];
    }
    {
      environment.systemPackages = [
        pkgs.conda
      ];
    }
    {
      environment.systemPackages = [
        pkgs.rxvt_unicode
      ];
    }
    {
      fonts = {
        fonts = with pkgs; [
          powerline-fonts
          terminus_font
    
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
        pkgs.python37Packages.powerline
      ];
    }
    {
    
    
      environment.systemPackages = let R-with-my-packages = pkgs.rWrapper.override{ packages = with pkgs.rPackages; [ ggplot2 eulerr gridExtra INSPEcT XVector S4Vectors]; };
      in [ R-with-my-packages ];
    }
    {
      environment.systemPackages = let python = (with pkgs; python3.withPackages (python-packages: with python-packages; [
        python3
        pandas
        openpyxl
        biopython
        scikitlearn
        matplotlib
        pyproj
        seaborn
        requests
        ipdb
        isort
        tox
        tqdm
        xlrd
        pyyaml
        matplotlib-venn
        networkx
        statsmodels
        up-set-plot
        # jedi
        # json-rpc
        # service-factory
    
        fritzconnection
        # jupyter
        # jupyter_core
        powerline
        adjust-text
        # up-set-plot
        # moritzsphd
        tabulate
        # swifter
        gffutils
        pyensembl
        pybedtools
        pybigwig
        xdg
        epc
        importmagic
        jupyterlab
        jupyter_console
        ipykernel
        pyperclip
        scikit-plot
        scikit-bio
        powerline
        python-language-server
        pyls-isort
        pyls-mypy
        # ptvsd
      ])); in with pkgs.python3Packages; [
        python  # let is stronger than with, which is why this installs the correct python (the one defined above)
        pkgs.pipenv
        pip
        pkgs.nodePackages.pyright
        python-language-server
        selenium
        # pkgs.zlib
        #pkgs.zlib.dev
        # nur-no-pkgs.repos.moritzschaefer.python3Packages.cytoflow
      ];
      # environment.variables.LD_LIBRARY_PATH = with pkgs; "$LD_LIBRARY_PATH:${stdenv.cc.cc.lib}/lib/libstdc++.so.6";  # TODO doesnt work anymore because of libgl 
    }
    {
      environment.systemPackages = with pkgs; [ clojure leiningen ];
    }
    {
      environment.systemPackages = with pkgs; [
        libGL
      ];
    }
    {
      environment.systemPackages = with pkgs; [
        bedtools
      ];
    }
    {
      environment.systemPackages = with pkgs; [
        tldr
        nmap
        sqlite
        gitAndTools.hub
        youtube-dl
        sshfs
        bash
        wget
        htop
        psmisc
        zip
        unzip
        unrar
        # p7zip marked as insecure
        bind
        file
        which
        utillinuxCurses
        powerstat
        pciutils
        ag
        ispell
        usbutils
        libv4l
        v4l-utils
        gparted
        etcher
        powerline-fonts
        xsel
        tree
        gitAndTools.diff-so-fancy
        gitAndTools.git-hub
        pypi2nix
        lsyncd
        gnupg
        imagemagick
        gdb
        ncdu
        mesa-demos
    
    
        patchelf
    
        cmake
        gnumake
    
      ];
      # environment.variables.NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      # environment.variables.PATH = "$HOME/.npm-global/bin:$PATH";
    }
  ] ++ machine-config;
}
