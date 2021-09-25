{ name, config, pkgs, lib, inputs, ... }:
let
  machine-config = lib.getAttr name {
    moxps = [
      {
        imports = [
          (import "${inputs.nixos-hardware}/common/cpu/intel")
          (import "${inputs.nixos-hardware}/common/cpu/intel/kaby-lake")
          (import "${inputs.nixos-hardware}/common/pc/laptop")  # tlp.enable = true
          # (import "${inputs.nixos-hardware}/common/pc/laptop/acpi_call.nix")  # tlp.enable = true
          (import "${inputs.nixos-hardware}/common/pc/laptop/ssd")
          inputs.nixpkgs.nixosModules.notDetected
        ];
      
        # from nixos-hardware
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = false;  # disabled after a boot or two to prevent usage on that kind of ram
        services.thermald.enable = true; 
      
        # from initial config and other webresources
        boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
        boot.kernelModules = [ "kvm-intel" ];
        boot.kernelParams = [ "acpi_rev_override=5" "i915.enable_guc=2" "pcie_aspm=off" ];  # "nouveau.modeset=0" ];  # 5,6,1 doesn't seem to make a difference. pcie_aspm=off might be required to avoid freezes
        
        # OpenGL accelerateion
        # nixpkgs.config.packageOverrides = pkgs: {
        #   vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
        # };
        # hardware.opengl = {
        #   enable = true;
        #   driSupport = true;
        #   extraPackages = with pkgs; [
        #     intel-media-driver # LIBVA_DRIVER_NAME=iHD <- works for VLC
        #     vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
        #     vaapiVdpau
        #     libvdpau-va-gl
        #   ];
        # };
      
      
        nix.maxJobs = lib.mkDefault 8;
      
        # TODO enable and check
        # services.undervolt = {
        #   enable = true;
        #   coreOffset = 0;
        #   gpuOffset = 0;
        #   # coreOffset = -125;
        #   # gpuOffset = -75;
        # };
        powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
        powerManagement.enable = true;
      
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
          touchpad.accelSpeed = "0.7";
        };
        services.xserver.displayManager.lightdm.greeters.gtk.cursorTheme = {
          name = "Vanilla-DMZ";
          package = pkgs.vanilla-dmz;
          size = 64;
        };
        environment.variables.XCURSOR_SIZE = "64";
      }
      {
        musnix = {
          # Find this value with `lspci | grep -i audio` (per the musnix readme).
          # PITFALL: This is the id of the built-in soundcard.
          #   When I start using the external one, change it.
          soundcardPciId = "00:1f.3";
        };
      }
      {
        console.packages = [
          pkgs.terminus_font
        ];
        console.font = "ter-132n";
        services.xserver.dpi = 220;
      }
    ];
    mobook = [
      {
        imports = [
          # (import "${inputs.nixos-hardware}/apple/macbook-pro") # messes up the keyboard...
          (import "${inputs.nixos-hardware}/common/pc/laptop/ssd")
          (import "${inputs.nixos-hardware}/common/pc/laptop")  # tlp.enable = true
          (import "${inputs.nixos-hardware}/common/cpu/intel")
          #inputs.nixpkgs.modules.hardware.network.broadcom-43xx # <- using import vs not using import?
         #  <nixpkgs/nixos/modules/hardware/network/broadcom-43xx.nix> <- this is when using channels instead of flakes?
          inputs.nixpkgs.nixosModules.notDetected
        ];
        
        hardware.facetimehd.enable = true;
      
        # from https://wiki.archlinux.org/index.php/MacBookPro11,x#Powersave
        services.udev.extraRules = let
          # remove_script = pkgs.requireFile {
          #   name = "remove_ignore_usb_devices.sh";
          #   url = "https://gist.githubusercontent.com/anonymous/9c9d45c4818e3086ceca/raw/2aa42b5b7d564868ff089dc72445f24586b6c55e/gistfile1.sh";
          #   sha256 = "b2e1d250b1722ec7d3a381790175b1fdd3344e638882ac00f83913e2f9d27603";
          # };
          remove_script = ''
          # from https://gist.github.com/anonymous/9c9d45c4818e3086ceca
          logger -p info "$0 executed."
          if [ "$#" -eq 2 ];then
              removevendorid=$1
              removeproductid=$2
              usbpath="/sys/bus/usb/devices/"
              devicerootdirs=`ls -1 $usbpath`
              for devicedir in $devicerootdirs; do
                  if [ -f "$usbpath$devicedir/product" ]; then
                      product=`cat "$usbpath$devicedir/product"`
                      productid=`cat "$usbpath$devicedir/idProduct"`
                      vendorid=`cat "$usbpath$devicedir/idVendor"`
                      if [ "$removevendorid" == "$vendorid" ] && [ "$removeproductid" == "$productid" ];    then
                          if [ -f "$usbpath$devicedir/remove" ]; then
                              logger -p info "$0 removing $product ($vendorid:$productid)"
                          echo 1 > "$usbpath$devicedir/remove"
                              exit 0
                else
                              logger -p info "$0 already removed $product ($vendorid:$productid)"
                              exit 0
                fi
                      fi
                  fi
              done
          else
              logger -p err "$0 needs 2 args vendorid and productid"
              exit 1
          fi'';
          remove_script_local = pkgs.writeShellScript "remove_ignore_usb-devices_local.sh" remove_script; #(import ./remove_ignore_usb_devices.sh.nix); # (builtins.readFile remove_script)
        in
          ''
          # /etc/udev/rules.d/99-apple_cardreader.rules
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="05ac", ATTRS{idProduct}=="8406", RUN+="${remove_script_local} 05ac 8406"
          # /etc/udev/rules.d/99-apple_broadcom_bcm2046_bluetooth.rules
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="05ac", ATTRS{idProduct}=="8289", RUN+="${remove_script_local} 05ac 8289"
          SUBSYSTEMS=="usb", ATTRS{idVendor}=="0a5c", ATTRS{idProduct}=="4500", RUN+="${remove_script_local} 0a5c 4500"
      
          # Disable XHC1 wakeup signal to avoid resume getting triggered some time
          # after suspend. Reboot required for this to take effect.
          SUBSYSTEM=="pci", KERNEL=="0000:00:14.0", ATTR{power/wakeup}="disabled"
          '';
      
        systemd.services.disable-gpe06 = {
          description = "Disable GPE06 interrupt leading to high kworker";
          wantedBy = [ "multi-user.target" ];
          script = ''
            /run/current-system/sw/bin/bash -c 'echo "disable" > /sys/firmware/acpi/interrupts/gpe06'
          '';
          serviceConfig.Type = "oneshot";
        };
      
      
        boot.loader.systemd-boot.enable = true;
        # boot.loader.efi.canTouchEfiVariables = true;
            
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
      
      
        boot.kernelModules = [ "kvm-intel" "wl" ];
        boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "usbhid" ];
        boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];
      
        hardware.video.hidpi.enable = lib.mkDefault true;
        powerManagement.enable = true;
        powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
        
        services.mbpfan = {
          enable = true;
          lowTemp = 60;
          highTemp = 67;
          maxTemp = 84;
        };
      }
      {
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
      }
      {
        services.xserver.libinput = {
          enable = true;
          touchpad.accelSpeed = "0.7";
        };
        # displayManager.lightdm.greeters.gtk.cursorTheme = {  # TODO if home manager cursor doesnt work
        #   name = "Vanilla-DMZ";
        #   package = pkgs.vanilla-dmz;
        #   size = 64;
        # };
      }
      {
        musnix = {
          # Find this value with `lspci | grep -i audio` (per the musnix readme).
          # PITFALL: This is the id of the built-in soundcard.
          #   When I start using the external one, change it.
          soundcardPciId = "00:1b.0";  # 00:1b.0 or 00:03.0
        };
      }
      {
        console.packages = [
          pkgs.terminus_font
        ];
        console.font = "ter-132n";
        services.xserver.dpi = 200;
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
  # disabledModules = [ "services/x11/window-managers/exwm.nix"  ]; 
  imports = [
    # (import "${inputs.nixpkgs-moritz}/nixos/modules/services/x11/window-managers/exwm.nix")
    (import "${inputs.musnix}")
    {
    
      nixpkgs.config.allowUnfree = true;
      nixpkgs.config.allowBroken = true;

      # The NixOS release to be compatible with for stateful data such as databases.
      system.stateVersion = "20.03";
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
      system.nixos.tags = [ "with-intel" ];
      services.xserver.videoDrivers = [ "intel" ];  # modesetting didn't help
      hardware.nvidiaOptimus.disable = true;
      boot.blacklistedKernelModules = [ "nouveau" "nvidia" ];  # bbswitch
      
      # https://github.com/NixOS/nixpkgs/issues/94315 <- from here. bugfix for this: https://discourse.nixos.org/t/update-to-21-05-breaks-opengl-because-of-dependency-on-glibc-2-31/14218 note, that there are multiple occurences of this
      # hardware.opengl.package = pkgs.nixpkgs-2009.mesa_drivers;
      services.xserver = {
        displayManager = {
          lightdm.enable = false;
          gdm.enable = true;
        };
      };
    }
    {
      hardware.bluetooth.enable = true;
      hardware.bluetooth.powerOnBoot = false;
      services.blueman.enable = true;
      hardware.bluetooth.settings.General.Enable = "Source,Sink,Media,Socket";
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
        options = [ "credentials=/home/moritz/.secret/cclab_nas.credentials" "workgroup=d.ethz.ch" "uid=moritz" "gid=users" "noauto" "echo_interval=30" ];
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
      nix.gc.options = "--delete-generations +12";
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
        enable = true;
        passwordAuthentication = false;
      };
      users.users.moritz.openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMc+scl71X7g21XFygTNB3onyGuION89iHSUw0eYcN2H mail+macbook@moritzs.de" ];
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
        package = pkgs.unstable.syncthing;
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
      virtualisation.virtualbox.host.enable = true;
      virtualisation.docker.enable = true;
      virtualisation.docker.enableNvidia = true;
      
      systemd.enableUnifiedCgroupHierarchy = false;  # workaround https://github.com/NixOS/nixpkgs/issues/127146
      hardware.opengl.driSupport32Bit = true;
      environment.systemPackages = [
        pkgs.docker-compose
        pkgs.kvm
        pkgs.qemu
        pkgs.nvtop # for nvidia
        pkgs.usbtop
      ];
    
      users.users.moritz.extraGroups = ["libvirtd" "docker"];  # the former is required for qemu I think 
    }
    {
      environment.systemPackages =
        let mount_external = pkgs.writeScriptBin "mount-external" ''
          #!${pkgs.stdenv.shell}
          sudo ${pkgs.cryptsetup}/bin/cryptsetup luksOpen /dev/disk/by-uuid/aeebfb90-65b5-4515-bf6e-001d0cfc8a40 encrypted-2tb
          sudo mount /dev/mapper/encrypted-2tb /mnt/encrypted
          '';
        umount_external = pkgs.writeScriptBin "umount-external" ''
          #!${pkgs.stdenv.shell}
          sudo umount /mnt/encrypted
          sudo ${pkgs.cryptsetup}/bin/cryptsetup luksClose encrypted-2tb
          '';
      in
         [ mount_external umount_external pkgs.borgbackup ];
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
      services.xserver = {
        # desktopManager.gnome3.enable = true;
        displayManager = {
          startx.enable = false;
          autoLogin = {  # if errors, then disable again
            user = "moritz";
            enable = false;
          }; 
        };
        enable = true;
      };
    }
    {
      services.xserver.windowManager = {
        exwm = {
          enable = true;
          extraPackages = epkgs: with epkgs; [ emacsql-sqlite pkgs.imagemagick pkgs.escrotum epkgs.vterm ];  # unfortunately, adding zmq and jupyter here, didn't work so I had to install them manually (i.e. compiling emacs-zmq)
          # I only managed to compile emacs-zmq once (~/emacs.d/elpa/27.1/develop/zmq-.../emacs-zmq.so). I just copied it from there to mobook
          enableDefaultConfig = false;  # todo disable and enable loadScript
          # careful, 'loadScript option' was merged from Vizaxo into my personal nixpkgs repo.
          loadScript = ''
            (require 'exwm)
            ;; most of it is now in .spacemacs.d/lisp/exwm.el
            (require 'exwm-systemtray)
            (require 'exwm-randr)
            ;; (setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "HDMI1" 2 "DP2" 3 "eDP1" 4 "HDMI1" 5 "DP2"))
            ;; (setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "eDP1" 2 "HDMI1" 3 "eDP1" 4 "eDP1" 5 "eDP1"))
            ;; (exwm-randr-enable)
            (exwm-systemtray-enable)
            (exwm-enable)
          '';
        };
        stumpwm.enable = false;
      };
      # services.xserver.displayManager.defaultSession = "none+exwm";  # Firefox works more fluently with plasma5+exwm instead of "none+exwm". or does it??
      services.xserver.desktopManager = {
        xterm.enable = false;
        plasma5.enable = true;
        xfce = {
          enable = true;
          noDesktop= true;
          enableXfwm = true;
        };
      };
      # services.picom.enable = true;
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
      services.xserver.xkbOptions= "lv5:rwin_switch_lock,terminate:ctrl_alt_bksp";
    
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
      # services.xserver.synaptics.enable = true;
      # services.xserver.synaptics.dev = "/dev/input/event7";
      # services.xserver.synaptics.tapButtons = false;
      # services.xserver.synaptics.buttonsMap = [ 1 3 2 ];
      # services.xserver.synaptics.twoFingerScroll = true;
      # services.xserver.synaptics.palmDetect = false;
      # services.xserver.synaptics.accelFactor = "0.001";
      # services.xserver.synaptics.additionalOptions = ''
      #   Option "SHMConfig" "on"
      #   Option "VertScrollDelta" "-100"
      #   Option "HorizScrollDelta" "-100"
      #   Option "Resolution" "370"
      # '';
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
        fontDir.enable = true;
        enableGhostscriptFonts = false;
    
        fonts = with pkgs; [
          corefonts
          inconsolata
          dejavu_fonts
          source-code-pro
          ubuntu_font_family
          unifont
    
          # Used by Emacs
          # input-mono
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
        mirage-im
        element-desktop
      ];
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
         [ pkgs.spotify wrapper pkgs.playerctl ];
    }
    {
      services.tor.enable = false;
      services.tor.client.enable = false;
    }
    {
      environment.systemPackages = with pkgs; [
        #haskellPackages.pandoc
        jabref
        nixpkgs-2009.pandoc
        nixpkgs-2009.haskellPackages.pandoc-crossref  # broken...
        nixpkgs-2009.haskellPackages.pandoc-citeproc  # broken...
        texlive.combined.scheme-full
      ];
    }
    {
      environment.systemPackages = [ pkgs.supercollider ];
    }
    {
       virtualisation.virtualbox.host.enable = true;
       users.extraGroups.vboxusers.members = [ "moritz" ];
       nixpkgs.config.allowUnfree = true;
       virtualisation.virtualbox.host.enableExtensionPack = true;
    }
    {
      environment.systemPackages =
        with pkgs;
        let sparkleshare_fixed = sparkleshare.overrideAttrs ( oldAttrs: {
          postInstall = ''
            wrapProgram $out/bin/sparkleshare \
                --set PATH ${symlinkJoin {
                  name = "mono-path";
                  paths = [
                    coreutils
                    bash
                    git
                    git-lfs
                    glib
                    mono
                    openssh
                    openssl
                    xdg_utils
                  ];
                }}/bin \
                --set MONO_GAC_PREFIX ${lib.concatStringsSep ":" [
                  appindicator-sharp
                  gtk-sharp-3_0
                  webkit2-sharp
                ]} \
                --set LD_LIBRARY_PATH ${lib.makeLibraryPath [
                  appindicator-sharp
                  gtk-sharp-3_0.gtk3
                  webkit2-sharp
                  webkit2-sharp.webkitgtk
                ]}
          '';
          } ); in
        [
        xcolor
        vlc
        aria
        jetbrains.pycharm-community
        obs-studio
        jmtpfs
        qbittorrent
        blender
        teams
        inkscape
        arandr
        dmenu
        # soulseekqt
        gnome3.cheese
        gnome3.gnome-screenshot
        sparkleshare_fixed 
        gnome3.gpaste
        autorandr
        teams
        libnotify
        
        # kdenlive  # fails in current unstable
        audacity
        google-play-music-desktop-player
        tdesktop # Telegram
        signal-cli # Signal
        signal-desktop # Signal
        unstable.zoom-us
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
      environment.systemPackages = [ pkgs.niv ];
    }
    {
      environment.variables.EDITOR = "vim";
      environment.systemPackages = [
        pkgs.vim_configurable # .override { python3 = true; })
        pkgs.neovim
      ];
    }
    {
      # leads to trouble only..
      systemd.services.modem-manager.enable = false;
      systemd.services."dbus-org.freedesktop.ModemManager1".enable = false;
      
      services.udev.extraRules = ''
        # Atmel DFU
        ### ATmega16U2
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2fef", TAG+="uaccess"
        ### ATmega32U2
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff0", TAG+="uaccess"
        ### ATmega16U4
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff3", TAG+="uaccess"
        ### ATmega32U4
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff4", TAG+="uaccess"
        ### AT90USB64
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ff9", TAG+="uaccess"
        ### AT90USB128
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="2ffb", TAG+="uaccess"
        ### Pro Micro 5V/16MHz
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="1b4f", ATTRS{idProduct}=="9205", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
        ## dog hunter AG
        ### Leonardo
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2a03", ATTRS{idProduct}=="0036", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
        ### Micro
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="2a03", ATTRS{idProduct}=="0037", TAG+="uaccess", ENV{ID_MM_DEVICE_IGNORE}="1"
      '';
      environment.systemPackages = [ pkgs.unstable.qmk ];
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
        pkgs.git-lfs
      ];
    }
    {
      environment.systemPackages = [
        pkgs.tmux
        pkgs.python37Packages.powerline
      ];
    }
    {
      environment.systemPackages = let R-with-my-packages = pkgs.rWrapper.override{ packages = with pkgs.rPackages; [ ggplot2 eulerr gridExtra INSPEcT XVector S4Vectors MAGeCKFlute]; };
      in [ R-with-my-packages ];
    }
    {
      environment.systemPackages =
        let python = (with pkgs; python3.withPackages (python-packages: with python-packages;
          let opencvGtk = opencv4.override (old : { enableGtk2 = true; enableGStreamer = true; });
          in [
          # gseapy
          orger
          hpi
          icecream
          plotly
          pytorch
          # ignite
          pytorch-lightning
          python3
          pandas
          XlsxWriter
          opencvGtk
          openpyxl
          biopython
          # scikitlearn
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
          smogn
          docker
          absl-py
          hjson
          pygments
          # ptvsd
          ])); in with pkgs.python3Packages; [
        python  # let is stronger than with, which is why this installs the correct python (the one defined above)
        pkgs.rPackages.orca  # required for plotly
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
        zlib
        zstd
        gcc
        pkg-config
        autoconf
      ];
    }
    {
      environment.systemPackages = with pkgs; [
        bedtools
      ];
    }
    {
      # environment.systemPackages = [ pkgs.unstable.esphome ];  # 1.15.0 fixes bug
     
      # from https://raw.githubusercontent.com/platformio/platformio-core/master/scripts/99-platformio-udev.rules
      # QinHeng Electronics HL-340 USB-Serial adapter
      services.udev.extraRules = ''
        #  CP210X USB UART
        ATTRS{idVendor}=="10c4", ATTRS{idProduct}=="ea60", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # FT231XS USB UART
        ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # Prolific Technology, Inc. PL2303 Serial Port
        ATTRS{idVendor}=="067b", ATTRS{idProduct}=="2303", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # QinHeng Electronics HL-340 USB-Serial adapter
        ATTRS{idVendor}=="1a86", ATTRS{idProduct}=="7523", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # Arduino boards
        ATTRS{idVendor}=="2341", ATTRS{idProduct}=="[08][02]*", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
        ATTRS{idVendor}=="2a03", ATTRS{idProduct}=="[08][02]*", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # Arduino SAM-BA
        ATTRS{idVendor}=="03eb", ATTRS{idProduct}=="6124", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{MTP_NO_PROBE}="1"
    
        # Digistump boards
        ATTRS{idVendor}=="16d0", ATTRS{idProduct}=="0753", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # Maple with DFU
        ATTRS{idVendor}=="1eaf", ATTRS{idProduct}=="000[34]", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # USBtiny
        ATTRS{idProduct}=="0c9f", ATTRS{idVendor}=="1781", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # USBasp V2.0
        ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="05dc", MODE:="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        # Teensy boards
        ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
        ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
        KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"
    
        #TI Stellaris Launchpad
        ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00fd", MODE="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        #TI MSP430 Launchpad
        ATTRS{idVendor}=="0451", ATTRS{idProduct}=="f432", MODE="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
    
        #GD32V DFU Bootloader
        ATTRS{idVendor}=="28e9", ATTRS{idProduct}=="0189", MODE="0666", ENV{ID_MM_DEVICE_IGNORE}="1", ENV{ID_MM_PORT_IGNORE}="1"
        '';
    }
    {
      environment.systemPackages = [ pkgs.arduino ];
      users.extraUsers.moritz.extraGroups = [ "dialout" ];
    }
    {
      environment.systemPackages = with pkgs; [
        tmux
        unstable.gpu-burn
        gdrive
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
        # utillinuxCurses
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
        # pypi2nix
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
