#
# This file is auto-generated from "pie.org"
#
{ config, pkgs, inputs, ... }:
{
  imports = [
    "${inputs.nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
    inputs.agenix.nixosModules.default
    {
      networking.hostName = "pie";
    
      system.stateVersion = "19.09";
    
      # Disable any documentation to free up some space
      documentation.enable = false;
    
      boot.initrd.systemd.emergencyAccess = true;
    }
    {
      nix = {
        settings.trusted-users = ["@wheel"];
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };
    }
    {
      services.openssh = {
        enable = true;
        settings = {
          PasswordAuthentication = false;
          PermitRootLogin = "no";
        };
      };
    
      users.users.rasen = {
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDHH15uiQw3jBbrdlcRb8wOr8KVltuwbHP/JOFAzXFO1l/4QxnKs6Nno939ugULM7Lu0Vx5g6FreuCOa2NMWk5rcjIwOzjrZnHZ7aoAVnE7H9scuz8NGnrWdc1Oq0hmcDxdZrdKdB6CPG/diGWNZy77nLvz5JcX1kPLZENPeApCERwR5SvLecA4Es5JORHz9ssEcf8I7VFpAebfQYDu+VZZvEu03P2+5SXv8+5zjiuxM7qxzqRmv0U8eftii9xgVNC7FaoRBhhM7yKkpbnqX7IeSU3WeVcw4+d1d8b9wD/sFOyGc1xAcvafLaGdgeCQGU729DupRRJokpw6bBRQGH29 rasen@omicron"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIulco7Hi0uJ90GLwuDvgG/Xlv/i2R4ySa5+/dOYygsr rasen@bayraktar"
        ];
        extraGroups = [ "wheel" ];
      };
    
      security.sudo.wheelNeedsPassword = false;
    }
    {
      services.avahi = {
        enable = true;
        nssmdns4 = true;
        publish = {
          enable = true;
          addresses = true;
          userServices = true;
        };
      };
    }
    {
      programs.mosh.enable = true;
      environment.systemPackages = [
        pkgs.vim
        pkgs.psmisc
      ];
    }
    # {
    #   services.openvpn.servers.nano-vpn = {
    #     # vendored ovpn config with secrets, etc.
    #     config = ''
    #       config /root/openvpn/nano-vpn.ovpn
    #     '';
    #   };
    # }
    {
      services.gitolite = {
        enable = true;
        enableGitAnnex = true;
        user = "git";
        adminPubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIulco7Hi0uJ90GLwuDvgG/Xlv/i2R4ySa5+/dOYygsr rasen@bayraktar";
        dataDir = "/run/media/ext-data/gitolite";
      };
    }
    {
      services.borgbackup.repos = {
        borg = {
          path = "/run/media/ext-data/borg";
          authorizedKeys = [
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDSqvRXKfEb52vB6QfLWUnuAD9KqJB8AtQ4STRA8cpFIRElU/3jJ2oUxZv2NF/cBefsK0BZ7ayLXpcOHyAMgUnoJqzmzzBkmmPPHC5lcz6jlLIhr2BjAFbtCKz25xjherqc7a/A47YnV6nLlS+yKuDxzal1HYAEKLGz6SHkqBIjW1u8QFDnZt+MtFqFNN1BqNFqmBKkuHYewynF/gON/d9M+759mCjNwzEqbKig3v6dWP1iD5gqupsn1AInrDehoENDpCeJzwL+2ZL30bor7lBvEoAqNmfezLLsZKzRBECobwCsI6FzZcF/qlF667ZqSpkW0wEYQGCjHXDANstkO1qR root@omicron"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIulco7Hi0uJ90GLwuDvgG/Xlv/i2R4ySa5+/dOYygsr rasen@bayraktar"
          ];
        };
      };
    }
    {
      # Do not auto-create borg repo
      systemd.services.borgbackup-repo-borg.enable = false;
    
      # Prepare mount point
      system.activationScripts = {
        ensure-ext-data = {
          text = ''
            mkdir -p /run/media/ext-data
          '';
          deps = [];
        };
      };
    
      environment.etc.crypttab = {
        mode = "0600";
        text = ''
          wdisk UUID=391eb77a-5504-4b34-8ff0-f1612f419427 none luks,noauto,nofail,x-systemd.device-timeout=1ms
        '';
      };
    
      # Configure auto-mountable drive
      fileSystems."/run/media/ext-data" = {
        device = "/dev/disk/by-uuid/63972645-dbc8-4543-b854-91038b2da6cb";
        fsType = "ext4";
        options = [
          "noauto"                       # do not mount on boot
          "nofail"
          "x-systemd.automount"          # mount when needed
          "x-systemd.device-timeout=1ms" # device should be plugged already—do not wait for it
          "x-systemd.idle-timout=5m"     # unmount after 5 min of inactivity
        ];
        # depends = [ "/dev/mapper/wdisk" ];
      };
    }
    {
      boot.loader.grub.enable = false;
      boot.loader.generic-extlinux-compatible.enable = true;
    
      # boot.kernelPackages = pkgs.linuxKernel.packages.linux_rpi3;
    
      # fix the following error:
      # modprobe: FATAL: Module ahci not found in directory
      # https://github.com/NixOS/nixpkgs/issues/154163#issuecomment-1350599022
      # nixpkgs.overlays = [
      #   (_final: super: {
      #     makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; });
      #   })
      # ];
    
      # Enable TTY
      boot.kernelParams = [
        "cma=32M"
        "console=ttyS1,115200n8"
        "console=ttyS0,115200n8"
      ];
    }
    {
      networking.wireless.enable = true;
    
      age.secrets.wireless_secrets.file = ./secrets/wireless_secrets.age;
      networking.wireless.secretsFile = config.age.secrets.wireless_secrets.path;
    
      networking.wireless.networks."Rotem Indiana".pskRaw = "ext:rotem_indiana";
      networking.wireless.networks."Rotem Indiana".priority = 10;
      networking.wireless.networks."Rotem Indiana_Backup".pskRaw = "ext:rotem_indiana_backup";
    
      hardware.enableRedistributableFirmware = true;
    
      # doesn’t seem to be needed anymore
      # hardware.firmware = [
      #   (pkgs.stdenv.mkDerivation {
      #     name = "broadcom-rpi3bplus-extra";
      #     src = pkgs.fetchurl {
      #       url = "https://raw.githubusercontent.com/RPi-Distro/firmware-nonfree/b518de4/brcm/brcmfmac43455-sdio.txt";
      #       sha256 = "0r4bvwkm3fx60bbpwd83zbjganjnffiq1jkaj0h20bwdj9ysawg9";
      #     };
      #     phases = [ "installPhase" ];
      #     installPhase = ''
      #       mkdir -p $out/lib/firmware/brcm
      #       cp $src $out/lib/firmware/brcm/brcmfmac43455-sdio.txt
      #     '';
      #   })
      # ];
    }
    {
      # swapDevices = [ { device = "/swapfile"; size = 1024; } ];
    }
  ];
}
