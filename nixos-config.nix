{ name, config, pkgs, lib, inputs, ... }:
let
  machine-config = lib.getAttr name {
    moxps = [
      {
        environment.systemPackages = with pkgs; let
          nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
            export __NV_PRIME_RENDER_OFFLOAD=1
            export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
            export __GLX_VENDOR_LIBRARY_NAME=nvidia
            export __VK_LAYER_NV_optimus=NVIDIA_only
            exec -a "$0" "$@"
          '';
        in [nvidia-offload];
        imports = [
          (import "${inputs.nixos-hardware}/dell/xps/15-9560/intel")
          inputs.nixpkgs.nixosModules.notDetected
        ];
        # accelerateion
        nixpkgs.config.packageOverrides = pkgs: {
          vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
        };
        hardware.opengl = {
          enable = true;
          extraPackages = with pkgs; [
            intel-media-driver # LIBVA_DRIVER_NAME=iHD
            vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
            vaapiVdpau
            libvdpau-va-gl
          ];
        };
      
        boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
        boot.kernelModules = [ "kvm-intel" ];
        boot.kernelParams = [ "acpi_rev_override=1" "pcie_aspm=off" "nouveau.modeset=0" ];
        boot.extraModulePackages = [ ];
      
        nix.maxJobs = lib.mkDefault 8;
      
        services.undervolt = {
          enable = true;
          coreOffset = -125;
          gpuOffset = -75;
        };
        services.tlp.enable = true;
        powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
        hardware.nvidia.prime = {
          # Bus ID of the Intel GPU. You can find it using lspci, either under 3D or VGA
          intelBusId = "PCI:0:2:0";
          # Bus ID of the NVIDIA GPU. You can find it using lspci, either under 3D or VGA
          nvidiaBusId = "PCI:1:0:0";
        };
        hardware.nvidia.prime.offload.enable = false;  # TODO
        hardware.bumblebee.enable = lib.mkForce false;
      
      }
      {
        fileSystems."/" =
          { device = "/dev/disk/by-uuid/8f0a4152-e9f1-4315-8c34-0402ff7efff4";
            fsType = "btrfs";
          };
      
        fileSystems."/boot" =
          { device = "/dev/disk/by-uuid/A227-1A0D";
            fsType = "vfat";
          };
      
        swapDevices =
          [ { device = "/dev/disk/by-uuid/9eca5b06-730e-439f-997b-512a614ccce0"; }
          ];
      
      
        boot.initrd.luks.devices = {
          cryptkey.device = "/dev/disk/by-uuid/ccd19ab7-0e4d-4df4-8912-b87139de56af";
          cryptroot = {
            device="/dev/disk/by-uuid/88242cfe-48a1-44d2-a29b-b55e6f05d3d3";
            keyFile="/dev/mapper/cryptkey";
            };
          cryptswap = {
            device="/dev/disk/by-uuid/f6fa3573-44a9-41cc-bab7-da60d21e27b3";
            keyFile="/dev/mapper/cryptkey";
          };
        };
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
      {
        console.packages = [
          pkgs.terminus_font
        ];
        console.font = "ter-132n";
      }
      {
        services.xserver.dpi = 240;
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
  imports = [
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
      services.blueman.enable = true;
      hardware.bluetooth.config.General.Enable = "Source,Sink,Media,Socket";
      hardware.pulseaudio = {
        enable = true;
    
        # NixOS allows either a lightweight build (default) or full build
        # of PulseAudio to be installed.  Only the full build has
        # Bluetooth support, so it must be selected here.
    
        extraModules = [ pkgs.pulseaudio-modules-bt ];
        package = pkgs.pulseaudioFull;
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
      };
    
      environment.systemPackages = [ pkgs.pavucontrol ];
    }
    {
      services.printing.enable = true;
      services.printing.drivers = with pkgs; [
        gutenprint
        gutenprintBin
        samsungUnifiedLinuxDriver
        splix
      ];
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
            ;; (add-hook 'exwm-randr-screen-change-hook (lambda () (start-process-shell-command "xrandr" nil "xrandr --fb 7680x2160 --output HDMI1 --auto --scale 2x2 --pos 0x0  --output eDP1 --auto --scale 1x1 --pos 3840x0"))) ;; TODO this leads to an endless cascade of screen updates.. -.-
            ;; (exwm-randr-enable)
            ;; (exwm-systemtray-enable)
            (exwm-enable)
          '';
        };
      };
      services.xserver.displayManager.defaultSession = "plasma5+exwm";  # Firefox works more fluently with plasma5+exwm instead of "none+exwm"
      services.xserver.desktopManager = {
        plasma5.enable = true;
        xfce.enable = true;
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
    services.tor.enable = true;
    services.tor.client.enable = true;
    {
      environment.systemPackages = [ pkgs.steam ];
      hardware.opengl.driSupport32Bit = true;
      hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva vaapiIntel];
      hardware.pulseaudio.support32Bit = true;
    }
    {
      environment.systemPackages = with pkgs; [
        gnome3.cheese
        pandoc   # TODO make a latex section
        # haskellPackages.pandoc-crossref  # broken...
        haskellPackages.pandoc-citeproc
        texlive.combined.scheme-full
        sparkleshare
        gnome3.gpaste
        autorandr
        
        kdenlive
        audacity
        google-play-music-desktop-player
        tdesktop # Telegram
        signal-cli # Signal
        signal-desktop # Signal
        zoom-us
        flameshot
        libreoffice
        wineWowPackages.stable
        # winetricks  # requires p7zip (which is unsafe...)
        gimp-with-plugins
    
        mplayer
        smplayer
    
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
      environment.systemPackages = let R-with-my-packages = pkgs.rWrapper.override{ packages = with pkgs.rPackages; [ ggplot2 eulerr gridExtra ]; };
      in [ R-with-my-packages ];
    }
    {
      environment.systemPackages = let python = (with pkgs; python3.withPackages (python-packages: with python-packages; [
        python3
        pandas
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
    
        fritzconnection
        #jupyter
        #jupyter_core
        powerline
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
    
    
        patchelf
    
        cmake
        gnumake
    
      ];
      # environment.variables.NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      # environment.variables.PATH = "$HOME/.npm-global/bin:$PATH";
    }
  ] ++ machine-config;
}
