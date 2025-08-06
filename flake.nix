#
# This file is auto-generated from "README.org"
#
{
  description = "rasendubi's packages and NixOS/home-manager configurations";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-25.05";
    };

    nixpkgs-unstable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    home-manager = {
      type = "github";
      owner = "rycee";
      repo = "home-manager";
      ref = "release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:lnl7/nix-darwin/nix-darwin-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware = {
      type = "github";
      owner = "NixOS";
      repo = "nixos-hardware";
      flake = false;
    };
    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
    };
  };

  outputs = { self, ... }@inputs:
    let
      # Flakes are evaluated hermetically, thus are unable to access
      # host environment (including looking up current system).
      #
      # That's why flakes must explicitly export sets for each system
      # supported.
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin"];

      # genAttrs applies f to all elements of a list of strings, and
      # returns an attrset { name -> result }
      #
      # Useful for generating sets for all systems or hosts.
      genAttrs = list: f: inputs.nixpkgs.lib.genAttrs list f;

      # Generate pkgs set for each system. This takes into account my
      # nixpkgs config (allowUnfree) and my overlays.
      pkgsBySystem =
        let mkPkgs = system: import inputs.nixpkgs {
              inherit system;
              overlays = self.overlays.${system};
              config = {
                allowUnfree = true;
                input-fonts.acceptLicense = true;
              };
            };
        in genAttrs systems mkPkgs;

      # genHosts takes an attrset { name -> options } and calls mkHost
      # with options+name. The result is accumulated into an attrset
      # { name -> result }.
      #
      # Used in NixOS and Home Manager configurations.
      genHosts = hosts: mkHost:
        genAttrs (builtins.attrNames hosts) (name: mkHost ({ inherit name; } // hosts.${name}));

      # merges a list of attrsets into a single attrset
      mergeSections = inputs.nixpkgs.lib.foldr inputs.nixpkgs.lib.mergeAttrs {};

    in mergeSections [
      (let
        nixosHosts = {
          omicron = { system = "x86_64-linux";  config = ./nixos-config.nix; };
      
          # pie uses a separate config as it is very different
          # from other hosts.
          pie =     { system = "aarch64-linux"; config = ./pie.nix; };
        };
      
        mkNixosConfiguration = { name, system, config }:
          let pkgs = pkgsBySystem.${system};
          in inputs.nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              { nixpkgs = { inherit pkgs; }; }
              (import config)
            ];
            specialArgs = { inherit name inputs; };
          };
      
      in {
        nixosConfigurations = genHosts nixosHosts mkNixosConfiguration;
      })
      (let
        homeManagerHosts = {
        };
      
        mkHomeManagerConfiguration = { system, name, config, username, homeDirectory }:
          let pkgs = pkgsBySystem.${system};
          in inputs.home-manager.lib.homeManagerConfiguration {
            inherit system pkgs username homeDirectory;
            configuration = { lib, ... }: {
              nixpkgs.config.allowUnfree = true;
              nixpkgs.config.firefox.enableTridactylNative = true;
              nixpkgs.overlays = self.overlays.${system};
              imports = [
                self.lib.home-manager-common
      
                (import config)
              ];
            };
          };
      
      in {
        # Re-export common home-manager configuration to be reused between
        # NixOS module and standalone home-manager config.
        lib.home-manager-common = { name, lib, pkgs, config, ... }: {
          imports = [
            {
              options.hostname = lib.mkOption {
                default = null;
                type = lib.types.nullOr lib.types.str;
                description = "hostname so that other home-manager options can depend on it.";
              };
            }
            ({ config, lib, pkgs, ... }:
            
            {
              config = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
                # Install MacOS applications to the user environment.
                home.file."Applications/Home Manager Apps".source = let
                  apps = pkgs.buildEnv {
                    name = "home-manager-applications";
                    paths = config.home.packages;
                    pathsToLink = "/Applications";
                  };
                in "${apps}/Applications";
              };
            })
            {
              home.file."nixpkgs".source = inputs.nixpkgs;
              systemd.user.sessionVariables.NIX_PATH = pkgs.lib.linux-only (lib.mkForce "nixpkgs=$HOME/nixpkgs\${NIX_PATH:+:}$NIX_PATH");
            
              xdg.configFile."nix/registry.json".text = builtins.toJSON {
                version = 2;
                flakes = [
                  {
                    from = { id = "self"; type = "indirect"; };
                    to = ({
                      type = "path";
                      path = inputs.self.outPath;
                    } // lib.filterAttrs
                      (n: v: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
                      inputs.self);
                  }
                  {
                    from = { id = "nixpkgs"; type = "indirect"; };
                    to = ({
                      type = "path";
                      path = inputs.nixpkgs.outPath;
                    } // lib.filterAttrs
                      (n: v: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
                      inputs.nixpkgs);
                  }
                ];
              };
            }
            {
              accounts.email.accounts = lib.mkIf (config.hostname == "bayraktar") (lib.mkForce {
                fluxon = {
                  realName = "Oleksii Shmalko";
                  address = "oleksii@fluxon.com";
                  flavor = "gmail.com";
                  primary = true;
            
                  passwordCommand = "pass fluxon/google.com/as@fluxon.com/email";
                  maildir.path = "fluxon";
            
                  msmtp.enable = true;
                  notmuch.enable = true;
                  mbsync.enable = true;
                  mbsync.create = "maildir";
                };
              });
              programs.mbsync.extraConfig = lib.mkForce "";
              programs.notmuch.extraConfig = {
                index."header.Sender" = "Sender";
              };
            }
            {
              programs.emacs = {
                enable = true;
                package = pkgs.my-emacs.base;
                extraPackages = pkgs.my-emacs.packages;
                overrides = pkgs.my-emacs.overrides;
              };
              # services.emacs.enable = true;
            
              # fonts used by emacs
              home.packages = [
                pkgs.ibm-plex
                pkgs.monaspace
                pkgs.dejavu_fonts
                pkgs.libertine
                pkgs.roboto-mono
                pkgs.mononoki
                pkgs.iosevka
              ];
            }
            {
              home.packages = [ pkgs.ripgrep ];
            }
            {
              home.packages = [
                (pkgs.hunspellWithDicts (with pkgs.hunspellDicts; [en_US en_US-large uk_UA]))
              ];
            }
            {
              home.packages = pkgs.lib.linux-only [
                pkgs.xss-lock
              ];
            }
            {
              home.packages = pkgs.lib.linux-only [ pkgs.escrotum ];
            }
            {
              home.keyboard = {
                layout = "us,ua";
                variant = "workman,";
              };
            }
            {
              xsession.initExtra = ''
                xkbcomp ${./Xkeymap} $DISPLAY
              '';
            }
            {
              home.packages = [ pkgs.xorg.xkbcomp ];
            }
            {
              home.file.".XCompose".text = ''
                include "%L"
            
                <Multi_key> <less> <equal>           : "⇐" U21D0 # Leftwards Double Arrow
                <Multi_key> <equal> <greater>        : "⇒" U21D2 # RIGHTWARDS DOUBLE ARROW
                <Multi_key> <less> <greater> <equal> : "⇔" U21D4 # LEFT RIGHT DOUBLE ARROW
                <Multi_key> <equal> <less> <greater> : "⇔" U21D4 # LEFT RIGHT DOUBLE ARROW
                <Multi_key> <minus> <less> <greater> : "↔" U2194 # LEFT RIGHT ARROW
            
                <Multi_key> <s> <u> <m> : "∑"
                <Multi_key> <f> <a> : "∀"                 # for all
                <Multi_key> <t> <e> : "∃"                 # there exists
                <Multi_key> <slash> <t> <e> : "∄"
                <Multi_key> <asciitilde> <equal> : "≅"    # approximately equal
                <Multi_key> <asciitilde> <asciitilde> : "≈"   U2248   # ~ ~ ALMOST EQUAL TO
                <Multi_key> <i> <n> : "∈" U2208
                <Multi_key> <n> <i> <n> : "∉" U2209
            
                # White Right Pointing Index
                <Multi_key> <rght> : "☞" U261E
            
                <Multi_key> <o> <c> : "℃"
                <Multi_key> <o> <f> : "℉"
            
                <Multi_key> <x> <x> : "❌"  # Cross Mark
            
                <Multi_key> <apostrophe> <apostrophe> : "́" # stress
            
                <Multi_key> <O> <slash> : "⌀" U2300 # DIAMETER SIGN
                <Multi_key> <slash> <O> : "⌀" U2300 # DIAMETER SIGN
                <Multi_key> <r> <r> : "√" U221A # SQUARE ROOT
                <Multi_key> <r> <3> : "∛" U221B # CUBE ROOT
                <Multi_key> <m> <A> : "∀" U2200 # FOR ALL
                <Multi_key> <m> <E> : "∃" U2203 # THERE EXISTS
                <Multi_key> <m> <i> : "∊" U220A # SMALL ELEMENT OF
                <Multi_key> <m> <d> : "∂" U2202 # PARTIAL DIFFERENTIAL
                <Multi_key> <m> <D> : "∆" U2206 # INCREMENT, Laplace operator
                <Multi_key> <m> <S> : "∑" U2211 # N-ARY SUMMATION, Sigma
                <Multi_key> <m> <I> : "∫" U222B # INTEGRAL
                <Multi_key> <m> <minus> : "−" U2212 # MINUS SIGN
                <Multi_key> <equal> <asciitilde> : "≈" U2248 # ALMOST EQUAL TO
                <Multi_key> <asciitilde> <equal> : "≈" U2248 # ALMOST EQUAL TO
                <Multi_key> <underscore> <underscore> : "‾" U023E # OVERLINE
                <Multi_key> <equal> <slash>  	: "≠"   U2260 # NOT EQUAL TO
                <Multi_key> <slash> <equal>  	: "≠"   U2260 # NOT EQUAL TO
                <Multi_key> <minus> <equal> 	: "≡"   U2261 # IDENTICAL TO
                <Multi_key> <equal> <minus> 	: "≡"   U2261 # IDENTICAL TO
                <Multi_key> <m> <less> <equal> : "≤" U2264 # LESS-THAN OR EQUAL TO
                <Multi_key> <m> <greater> <equal> : "≥" U2265 # GREATER-THAN OR EQUAL TO
                <Multi_key> <m> <o> <o> : "∞" # infty
                <Multi_key> <m> <_> <i> : "ᵢ" # subscript i
                <Multi_key> <m> <^> <i> : "ⁱ" # superscript i
                <Multi_key> <m> <_> <minus> : "₋" # subscript minus
                <Multi_key> <m> <^> <minus> : "⁻" # superscript minus
                <Multi_key> <m> <_> <plus> : "₊" # subscript plus
                <Multi_key> <m> <^> <plus> : "⁺" # superscript plus
                <Multi_key> <m> <asterisk> : "∘" # ring (function compose) operator
                <Multi_key> <m> <period> : "∙" # dot operator
                <Multi_key> <m> <asciitilde> : "∝" # proportional to
                <Multi_key> <q> <e> <d> : "∎" # q.e.d.
              '';
            }
            {
              services.xcape = {
                enable = pkgs.stdenv.isLinux;
                mapExpression = {
                  Control_L = "Escape";
                };
              };
            }
            {
              home.packages = pkgs.lib.linux-only [ pkgs.networkmanagerapplet ];
            }
            {
              programs.direnv.enable = true;
              programs.direnv.nix-direnv.enable = true;
              services.lorri.enable = pkgs.stdenv.isLinux;
            }
            {
              programs.autorandr = {
                enable = true;
                profiles =
                  let
                    omicron = "00ffffffffffff004d104a14000000001e190104a51d11780ede50a3544c99260f505400000001010101010101010101010101010101cd9180a0c00834703020350026a510000018a47480a0c00834703020350026a510000018000000fe0052584e3439814c513133335a31000000000002410328001200000b010a202000cc";
                    work = "00ffffffffffff004d108d1400000000051c0104a52213780ea0f9a95335bd240c5157000000010101010101010101010101010101014dd000a0f0703e803020350058c210000018000000000000000000000000000000000000000000fe00464e564452804c513135364431000000000002410328011200000b010a202000ee";
                    home-monitor = "00ffffffffffff0010acc0a042524530031c010380351e78eae245a8554da3260b5054a54b00714f8180a9c0a940d1c0e10001010101a36600a0f0701f80302035000f282100001a000000ff004438565846383148304552420a000000fc0044454c4c205032343135510a20000000fd001d4c1e8c1e000a202020202020018802032ef15390050402071601141f1213272021220306111523091f07830100006d030c001000003c200060030201023a801871382d40582c25000f282100001e011d8018711c1620582c25000f282100009e04740030f2705a80b0588a000f282100001e565e00a0a0a02950302035000f282100001a0000000000000000008a";
                    home-monitor-2 = "00ffffffffffff004c2d767135305943341f0103804024782a6115ad5045a4260e5054bfef80714f810081c081809500a9c0b300010108e80030f2705a80b0588a0078682100001e000000fd0030901eff8f000a202020202020000000fc004c53323841473730304e0a2020000000ff0048345a524330303236380a2020017f02034bf14761103f04035f762309070783010000e305c0006b030c001000b83c200020016dd85dc401788053003090c354056d1a0000020f3090000461045a04e6060501615a00e30f4100565e00a0a0a029503020350078682100001a6fc200a0a0a055503020350078682100001a0000000000000000000000000000000037";
                    work-monitor = "00ffffffffffff0010acc2d0545741312c1b010380351e78eaad75a9544d9d260f5054a54b008100b300d100714fa9408180d1c00101565e00a0a0a02950302035000e282100001a000000ff004d59334e44374234314157540a000000fc0044454c4c205032343138440a20000000fd0031561d711c000a202020202020010302031bb15090050403020716010611121513141f2065030c001000023a801871382d40582c45000e282100001e011d8018711c1620582c25000e282100009ebf1600a08038134030203a000e282100001a7e3900a080381f4030203a000e282100001a00000000000000000000000000000000000000000000000000000000d8";
                  in {
                    "omicron" = {
                      fingerprint = {
                        eDP-1 = omicron;
                      };
                      config = {
                        eDP-1 = {
                          enable = true;
                          primary = true;
                          position = "0x0";
                          mode = "3200x1800";
                          rate = "60.00";
                        };
                      };
                    };
                    "omicron-home" = {
                      fingerprint = {
                        eDP-1 = omicron;
                        DP-1 = home-monitor;
                      };
                      config = {
                        eDP-1.enable = false;
                        DP-1 = {
                          enable = true;
                          primary = true;
                          position = "0x0";
                          mode = "3840x2160";
                          rate = "60.00";
                        };
                      };
                    };
                    "omicron-home-2" = {
                      fingerprint = {
                        eDP-1 = omicron;
                        DP-1 = home-monitor-2;
                      };
                      config = {
                        eDP-1.enable = false;
                        DP-1 = {
                          enable = true;
                          primary = true;
                          position = "0x0";
                          mode = "3840x2160";
                          rate = "60.00";
                        };
                      };
                    };
                    "omicron-home-monitor" = {
                      fingerprint = {
                        DP-1 = home-monitor;
                      };
                      config = {
                        DP-1 = {
                          enable = true;
                          primary = true;
                          position = "0x0";
                          mode = "3840x2160";
                          rate = "60.00";
                        };
                      };
                    };
                    omicron-home-monitor-2 = {
                      fingerprint = {
                        DP-1 = home-monitor-2;
                      };
                      config = {
                        DP-1 = {
                          enable = true;
                          primary = true;
                          position = "0x0";
                          mode = "3840x2160";
                          rate = "60.00";
                        };
                      };
                    };
                  };
              };
            }
            {
              home.packages = pkgs.lib.linux-only [ pkgs.acpilight ];
            }
            {
              home.packages = pkgs.lib.linux-only [ pkgs.pavucontrol ];
            }
            {
              home.packages = pkgs.lib.linux-only [
                pkgs.firefox
                pkgs.google-chrome
              ];
            }
            {
              xdg.configFile."tridactyl/tridactylrc".text = ''
                " drop all existing configuration
                sanitize tridactyllocal tridactylsync
                
                bind J scrollline -10
                bind K scrollline 10
                bind j scrollline -2
                bind k scrollline 2
              '';
            }
            {
              home.packages = [
                pkgs.imagemagick
                # latex for displaying fragments in org-mode
                (pkgs.texlive.combine {
                  inherit (pkgs.texlive)
                    scheme-small
                    dvipng
                    dvisvgm
                    mhchem # chemistry
                    tikz-cd # category theory diagrams
                    # required for org export
                    wrapfig
                    capt-of
                  ;
                })
                pkgs.ghostscript
              ];
            }
            {
              home.packages = [ pkgs.graphviz ];
            }
            {
              home.packages = [
                pkgs.findutils
                pkgs.gawk
              ];
            }
            {
              # Store mails in ~/Mail
              accounts.email.maildirBasePath = "Mail";
            
              accounts.email.accounts.as = {
                realName = "Oleksii Shmalko";
                address = "me@alexeyshmalko.com";
                flavor = "plain";
            
                userName = "me@alexeyshmalko.com";
                imap.host = "imap.secureserver.net";
                imap.port = 993;
                imap.tls.enable = true;
                smtp.host = "smtpout.secureserver.net";
                smtp.port = 465;
                smtp.tls.enable = true;
            
                passwordCommand = "pass me@alexeyshmalko.com";
                maildir.path = "alexeyshmalko";
            
                msmtp.enable = true;
                notmuch.enable = true;
                mbsync.enable = true;
                mbsync.create = "maildir";
              };
            
              # Use mbsync to fetch email. Configuration is constructed manually
              # to keep my current email layout.
              programs.mbsync = {
                enable = true;
                extraConfig = lib.mkBefore ''
                  MaildirStore local
                  Path ~/Mail/
                  Inbox ~/Mail/INBOX
                  SubFolders Verbatim
                '';
              };
            
              # Notmuch for email browsing, tagging, and searching.
              programs.notmuch = {
                enable = true;
                new.ignore = [
                  ".mbsyncstate"
                  ".mbsyncstate.lock"
                  ".mbsyncstate.new"
                  ".mbsyncstate.journal"
                  ".uidvalidity"
                  "dovecot-uidlist"
                  "dovecot-keywords"
                  "dovecot.index"
                  "dovecot.index.log"
                  "dovecot.index.log.2"
                  "dovecot.index.cache"
                  "/^archive/"
                ];
              };
            
              # msmtp for sending mail
              programs.msmtp.enable = true;
            
              # My Maildir layout predates home-manager configuration, so I do not
              # use mbsync config generation from home-manager, to keep layout
              # compatible.
              imports =
                let
                  emails = [
                    { name = "gmail";   email = "rasen.dubi@gmail.com";    path = "Personal"; primary = true; }
                    { name = "ps";      email = "ashmalko@doctoright.org"; path = "protocolstandard"; }
                    { name = "egoless"; email = "me@egoless.tech";         path = "egoless"; }
                  ];
                  mkGmailBox = { name, email, path, ... }@all: {
                    accounts.email.accounts.${name} = {
                      realName = "Oleksii Shmalko";
                      address = email;
                      flavor = "gmail.com";
            
                      passwordCommand = "pass imap.gmail.com/${email}";
                      maildir.path = path;
            
                      msmtp.enable = true;
                      notmuch.enable = true;
                    } // (removeAttrs all ["name" "email" "path"]);
            
                    programs.mbsync.extraConfig = ''
                      IMAPAccount ${name}
                      Host imap.gmail.com
                      User ${email}
                      PassCmd "pass imap.gmail.com/${email}"
                      SSLType IMAPS
                      CertificateFile /etc/ssl/certs/ca-certificates.crt
            
                      IMAPStore ${name}-remote
                      Account ${name}
            
                      Channel sync-${name}-all
                      Far :${name}-remote:"[Gmail]/All Mail"
                      Near :local:${path}/all
                      Create Both
                      SyncState *
            
                      Channel sync-${name}-spam
                      Far :${name}-remote:"[Gmail]/Spam"
                      Near :local:${path}/spam
                      Create Both
                      SyncState *
            
                      Channel sync-${name}-sent
                      Far :${name}-remote:"[Gmail]/Sent Mail"
                      Near :local:${path}/sent
                      Create Both
                      SyncState *
            
                      Group sync-${name}
                      Channel sync-${name}-all
                      Channel sync-${name}-spam
                      Channel sync-${name}-sent
            
                    '';
                  };
                in map mkGmailBox emails;
            }
            {
              home.file.".mailcap".text = ''
                text/html; firefox %s
                application/pdf; open %s
              '';
            }
            {
              home.packages = [ pkgs.rss2email ];
            }
            {
              home.packages = [
                pkgs.yubikey-manager
                pkgs.yubikey-personalization
              ];
            }
            {
              home.packages = [
                (pkgs.pass.withExtensions (exts: [ exts.pass-otp exts.pass-audit exts.pass-genphrase ]))
                pkgs.qrencode
              ];
            }
            {
              programs.browserpass = {
                enable = true;
                browsers = ["firefox" "chrome"];
              };
            }
            {
              home.packages = [
                pkgs.age
                pkgs.age-plugin-yubikey
              ];
            }
            {
              home.sessionVariables.PASSAGE_DIR = "${config.home.homeDirectory}/.password-store";
              home.sessionVariables.PASSAGE_IDENTITIES_FILE = "${config.home.homeDirectory}/.config/passage/identities";
              home.packages = [
                (pkgs.passage.overrideAttrs (old: {
                  patches =
                    (old.patches or [])
                    ++ [
                      # Allow using multiple identities.
                      (pkgs.fetchpatch {
                        url = "https://github.com/FiloSottile/passage/commit/fba940f9e9ffbad7b746f26b8d6323ef6f746187.patch";
                        sha256 = "sha256-2w/k6JmcxFq9ThBasM0sL+58fwutF1ioZzwRFXfJgME=";
                      })
                    ];
                }))
              ];
            }
            {
              home.packages = [
                pkgs.unstable.gopass
                pkgs.unstable.gopass-jsonapi
              ];
            }
            {
              home.packages = pkgs.lib.linux-only [
                pkgs.kdePackages.gwenview
                pkgs.kdePackages.dolphin
                # pkgs.kdeFrameworks.kfilemetadata
                pkgs.kdePackages.filelight
                pkgs.shared-mime-info
              ];
            }
            {
              programs.zathura = {
                enable = true;
                options = {
                  incremental-search = true;
                };
            
                # Swap j/k (for Workman layout)
                extraConfig = ''
                  map j scroll up
                  map k scroll down
                '';
              };
            }
            {
              home.packages = [
                (pkgs.lib.linux-only pkgs.tdesktop) # Telegram
                pkgs.feh
            
                # mesa is broken on macOS
                (pkgs.lib.linux-only pkgs.mplayer)
                (pkgs.lib.linux-only pkgs.smplayer)
              ];
            }
            {
              home.packages = [
                pkgs.vim_configurable
              ];
            }
            {
              home.file.".vim".source = ./.vim;
              home.file.".vimrc".source = ./.vim/init.vim;
            }
            {
              programs.urxvt = {
                enable = true;
                iso14755 = false;
            
                fonts = [
                  "-*-terminus-medium-r-normal-*-32-*-*-*-*-*-iso10646-1"
                ];
            
                scroll = {
                  bar.enable = false;
                  lines = 65535;
                  scrollOnOutput = false;
                  scrollOnKeystroke = true;
                };
                extraConfig = {
                  "loginShell" = "true";
                  "urgentOnBell" = "true";
                  "secondaryScroll" = "true";
            
                  # Molokai color theme
                  "background" = "#101010";
                  "foreground" = "#d0d0d0";
                  "color0" = "#101010";
                  "color1" = "#960050";
                  "color2" = "#66aa11";
                  "color3" = "#c47f2c";
                  "color4" = "#30309b";
                  "color5" = "#7e40a5";
                  "color6" = "#3579a8";
                  "color7" = "#9999aa";
                  "color8" = "#303030";
                  "color9" = "#ff0090";
                  "color10" = "#80ff00";
                  "color11" = "#ffba68";
                  "color12" = "#5f5fee";
                  "color13" = "#bb88dd";
                  "color14" = "#4eb4fa";
                  "color15" = "#d0d0d0";
                };
              };
            }
            {
              programs.fish = {
            
                interactiveShellInit = ''
                  function vterm_prompt_end;
                    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
                  end
                  functions --copy fish_prompt vterm_old_fish_prompt
                  function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
                    # Remove the trailing newline from the original prompt. This is done
                    # using the string builtin from fish, but to make sure any escape codes
                    # are correctly interpreted, use %b for printf.
                    printf "%b" (string join "\n" (vterm_old_fish_prompt))
                    vterm_prompt_end
                  end
                '';
            
                functions.vterm_printf = ''
                  function vterm_printf;
                    if [ -n "$TMUX" ]
                      # tell tmux to pass the escape sequences through
                      # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
                      printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
                    else if string match -q -- "screen*" "$TERM"
                      # GNU screen (screen, screen-256color, screen-256color-bce)
                      printf "\eP\e]%s\007\e\\" "$argv"
                    else
                      printf "\e]%s\e\\" "$argv"
                    end
                  end
                '';
            
                functions.vterm_cmd = ''
                  function vterm_cmd --description 'Run an emacs command among the ones been defined in vterm-eval-cmds.'
                      set -l vterm_elisp ()
                      for arg in $argv
                        set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
                      end
                      vterm_printf '51;E'(string join "" $vterm_elisp)
                  end
                '';
            
                # Use current directory as title. The title is picked up by
                # `vterm-buffer-name-string` in emacs.
                #
                # prompt_pwd is like pwd, but shortens directory name:
                # /home/rasen/dotfiles -> ~/dotfiles
                # /home/rasen/prg/project -> ~/p/project
                functions.fish_title = ''
                  function fish_title
                    prompt_pwd
                  end
                '';
              };
            }
            {
              programs.fish = {
                enable = true;
                shellAliases = {
                  g = "git";
                  rm = "rm -r";
                  ec = "emacsclient";
                };
                functions = {
                  # old stuff
                  screencast = ''
                    function screencast
                        # key-mon --meta --nodecorated --theme=big-letters --key-timeout=0.05 &
                        ffmpeg -probesize 3000000000 -f x11grab -framerate 25 -s 3840x3960 -i :0.0 -vcodec libx264 -threads 2 -preset ultrafast -crf 0 ~/tmp/record/record-(date +"%FT%T%:z").mkv
                        # killall -r key-mon
                    end
                  '';
                  reencode = ''
                    function reencode
                        ffmpeg -i file:$argv[1] -c:v libx264 -crf 0 -preset veryslow file:(basename $argv[1] .mkv).crf-0.min.mkv
                    end
                  '';
                };
              };
            
              # manage other shells as well
              # programs.bash.enable = true;
              programs.zsh.enable = true;
              programs.zsh.initContent = ''
                if [[ -f "~/.zshrc_local" ]]; then
                  source "~/.zshrc_local"
                fi
              '';
            }
            {
              programs.fish.functions.fish_user_key_bindings = ''
                function fish_user_key_bindings
                    fish_vi_key_bindings
            
                    bind -s j up-or-search
                    bind -s k down-or-search
                    bind -s -M visual j up-line
                    bind -s -M visual k down-line
            
                    bind -s '.' repeat-jump
                end
              '';
            }
            {
              programs.tmux = {
                enable = true;
                keyMode = "vi";
                # Use C-a as prefix
                shortcut = "a";
                # To make vim work properly
                terminal = "screen-256color";
            
                # start numbering from 1
                baseIndex = 1;
                # Allows for faster key repetition
                escapeTime = 0;
                historyLimit = 10000;
            
                reverseSplit = true;
            
                clock24 = true;
            
                extraConfig = ''
                  bind-key S-left swap-window -t -1
                  bind-key S-right swap-window -t +1
            
                  bind h select-pane -L
                  bind k select-pane -D
                  bind j select-pane -U
                  bind l select-pane -R
            
                  bind r source-file ~/.tmux.conf \; display-message "Config reloaded..."
            
                  set-window-option -g automatic-rename
                '';
              };
            }
            {
              home.packages = [ pkgs.just ];
            }
            {
              home.packages = [ pkgs.dtach ];
            }
            {
              programs.git = {
                enable = true;
                package = pkgs.gitAndTools.gitFull;
            
                userName = "Oleksii Shmalko";
                userEmail = if name == "bayraktar" then "oleksii@fluxon.com" else "rasen.dubi@gmail.com";
            
                # signing = {
                #   key = "EB3066C3";
                #   signByDefault = true;
                # };
            
                extraConfig = {
                  gpg.format = "ssh";
                  gpg.ssh.allowedSignersFile = "~/.ssh/allowed_signers";
                  user.signingKey = "~/.ssh/code_signing.pub";
                  commit.gpgsign = true;
            
                  sendemail = {
                    smtpencryption = "ssl";
                    smtpserver = "smtp.gmail.com";
                    smtpuser = "rasen.dubi@gmail.com";
                    smtpserverport = 465;
                  };
            
                  color.ui = true;
                  core.editor = "vim";
                  push.default = "simple";
                  pull.rebase = true;
                  rebase.autostash = true;
                  rerere.enabled = true;
                  advice.detachedHead = false;
                  merge.conflictStyle = "zdiff3";
            
                  init.defaultBranch = "main";
                };
              };
            }
            {
              programs.git.aliases = {
                cl    = "clone";
                gh-cl = "gh-clone";
                cr    = "cr-fix";
                p     = "push";
                pl    = "pull";
                f     = "fetch";
                fa    = "fetch --all";
                a     = "add";
                ap    = "add -p";
                d     = "diff";
                dl    = "diff HEAD~ HEAD";
                ds    = "diff --staged";
                l     = "log --show-signature";
                l1    = "log -1";
                lp    = "log -p";
                c     = "commit";
                ca    = "commit --amend";
                co    = "checkout";
                cb    = "checkout -b";
                cm    = "checkout origin/master";
                de    = "checkout --detach";
                fco   = "fetch-checkout";
                br    = "branch";
                s     = "status";
                re    = "reset --hard";
                r     = "rebase";
                rc    = "rebase --continue";
                ri    = "rebase -i";
                m     = "merge";
                t     = "tag";
                su    = "submodule update --init --recursive";
                bi    = "bisect";
              };
            }
            {
              programs.git.extraConfig = {
                url."git@github.com:".pushInsteadOf = "https://github.com/";
              };
            }
            {
              programs.git.extraConfig.github.name = "rasendubi";
            }
            {
              home.sessionVariables.npm_config_prefix = "$HOME/.npm-global";
              home.sessionPath = ["$HOME/.npm-global/bin"];
            }
            {
              home.packages = [ pkgs.plantuml ];
            }
            {
              home.packages = [
                pkgs.sbcl
                # pkgs.clpm
              ];
            }
            {
              fonts.fontconfig.enable = true;
              home.packages = [
                pkgs.iosevka
                pkgs.inconsolata
                pkgs.dejavu_fonts
                pkgs.source-code-pro
                pkgs.ubuntu_font_family
                pkgs.unifont
                pkgs.powerline-fonts
                pkgs.terminus_font
              ];
            }
            {
              xresources.properties = {
                "Xft.dpi" = 276;
                "Xcursor.size" = 64;
              };
            }
            {
              home.file = {
                ".nethackrc".source = ./.nethackrc;
              };
            
              programs.fish.shellInit = ''
                set -x PATH ${./bin} $PATH
              '';
            }
          ];
          home.stateVersion = "21.05";
        };
        homeManagerConfigurations = genHosts homeManagerHosts mkHomeManagerConfiguration;
      })
      (let
        mkPackages = system:
          let
            pkgs = pkgsBySystem.${system};
          in
            mergeSections [
              (let
                emacs-base =
                  if pkgs.stdenv.isDarwin
                  then (pkgs.emacs.overrideAttrs (old: {
                    patches =
                      (old.patches or [])
                      ++ [
                        # Fix OS window role so that yabai can pick up emacs
                        (pkgs.fetchpatch {
                          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
                          sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
                        })
                        # Enable rounded window with no decoration
                        (pkgs.fetchpatch {
                          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
                          sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
                        })
                        # Make emacs aware of OS-level light/dark mode
                        (pkgs.fetchpatch {
                          url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
                          sha256 = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
                        })
                      ];
                    # configureFlags =
                    #   (old.configureFlags or [])
                    #   ++ [
                    #     "LDFLAGS=-headerpad_max_install_names"
                    #   ];
                  })).override {
                    # Workaround https://github.com/NixOS/nixpkgs/issues/395169
                    withNativeCompilation = false;
                  }
                  else pkgs.emacs.override {
                    withX = true;
                    # select lucid toolkit
                    toolkit = "lucid";
                    withGTK3 = false;
                  };
                emacs-packages = (epkgs:
                  (with epkgs.melpaPackages; [
              
                    activity-watch-mode
                    aggressive-indent
                    atomic-chrome
                    avy
                    bash-completion
                    beacon
                    blacken
                    cider
                    clojure-mode
                    cmake-mode
                    color-identifiers-mode
                    company
                    company-box
                    counsel
                    counsel-projectile
                    dart-mode
                    diff-hl
                    diminish
                    direnv
                    dockerfile-mode
                    doom-modeline
                    dtrt-indent
                    edit-indirect
                    el-patch
                    elpy
                    emojify
                    envrc
                    epresent
                    evil
                    evil-collection
                    evil-numbers
                    evil-org
                    evil-surround
                    evil-swap-keys
                    exec-path-from-shell
                    expand-region
                    fish-completion
                    fish-mode
                    flycheck
                    flycheck-inline
                    flycheck-jest
                    flycheck-rust
                    forge
                    forth-mode
                    just-mode
                    justl
                    general
                    go-mode
                    google-translate
                    gptel
                    graphviz-dot-mode
                    groovy-mode
                    haskell-mode
                    imenu-list
                    ivy
                    ivy-bibtex
                    ivy-pass
                    jinja2-mode
                    js2-mode
                    json-mode
                    ledger-mode
                    lispyville
                    lsp-haskell
                    lsp-mode
                    lsp-ui
                    lua-mode
                    magit
                    markdown-mode
                    modus-themes
                    nix-mode
                    nix-sandbox
                    notmuch
                    ol-notmuch
                    org-cliplink
                    org-download
                    org-drill
                    org-ref
                    org-roam
                    org-roam-bibtex
                    org-super-agenda
                    paren-face
                    pass
                    php-mode
                    pip-requirements
                    plantuml-mode
                    prettier-js
                    projectile
                    protobuf-mode
                    psc-ide
                    purescript-mode
                    py-autopep8
                    racer
                    racket-mode
                    restclient
                    rjsx-mode
                    ryo-modal
                    god-mode
                    multiple-cursors
                    rust-mode
                    slime
                    smex
                    spaceline
                    svelte-mode
                    swift-mode
                    terraform-mode
                    tide
                    typescript-mode
                    visual-fill-column
                    vterm
                    vue-mode
                    w3m
                    web-mode
                    wgrep
                    which-key
                    whitespace-cleanup-mode
                    writegood-mode
                    yaml-mode
                    yasnippet
                    zig-mode
              
                    corfu
                    cape
                    vertico
                    orderless
                    consult
                    embark
                    marginalia
                    smartparens
                    git-link
              
                  ]) ++
                  [
                    epkgs.elpaPackages.org
                    epkgs.nongnuPackages.org-contrib
                    epkgs.elpaPackages.adaptive-wrap
                    # not available in melpa
                    epkgs.elpaPackages.exwm
                    epkgs.elpaPackages.valign
              
                    epkgs.elpaPackages.eglot
              
                    (epkgs.trivialBuild rec {
                      pname = "org-fc";
                      version = "20201121";
                      src = pkgs.fetchFromGitHub {
                        # owner = "rasendubi";
                        # repo = "org-fc";
                        # rev = "35ec13fd0412cd17cbf0adba7533ddf0998d1a90";
                        # sha256 = "sha256-2h1dIR7WHYFsLZ/0D4HgkoNDxKQy+v3OaiiCwToynvU=";
                        owner = "l3kn";
                        repo = "org-fc";
                        rev = "cc191458a991138bdba53328690a569b8b563502";
                        sha256 = "sha256-wzMSgS4iZfpKOICqQQuQYNPb2h7i4tTWsMs7mVmgBt8=";
                      };
                      packageRequires = [
                        epkgs.elpaPackages.org
                        epkgs.melpaPackages.hydra
                      ];
                      propagatedUserEnvPkgs = [ pkgs.findutils pkgs.gawk ];
              
                      postInstall = ''
                        cp -r ./awk/ $LISPDIR/
                      '';
              
                      meta = {
                        description = "Spaced Repetition System for Emacs org-mode";
                        license = pkgs.lib.licenses.gpl3;
                      };
                    })
              
                    # required for org-roam/emacsql-sqlite3
                    pkgs.sqlite
              
                    pkgs.notmuch
                    pkgs.w3m
                    pkgs.imagemagick
                    pkgs.shellcheck
              
                    pkgs.direnv
              
                    (pkgs.python3.withPackages (pypkgs: [
                      pypkgs.autopep8
                      pypkgs.black
                      pypkgs.flake8
                      pypkgs.mypy
                      pypkgs.pylint
                      pypkgs.virtualenv
                    ]))
              
                    (pkgs.aspellWithDicts (dicts: with dicts; [en en-computers en-science uk]))
              
                    # latex for displaying fragments in org-mode
                    (pkgs.texlive.combine {
                      inherit (pkgs.texlive)
                        scheme-small
                        dvipng
                        dvisvgm
                        mhchem # chemistry
                        tikz-cd # category theory diagrams
                        # required for org export
                        wrapfig
                        capt-of
                      ;
                    })
                    pkgs.ghostscript
                  ]
                );
              
                overrides = self: super: {
                  # select org from elpa
                  org = super.elpaPackages.org;
                };
              
                emacs-final = ((pkgs.emacsPackagesFor emacs-base).overrideScope overrides).emacsWithPackages emacs-packages;
              
              in {
                my-emacs = emacs-final // {
                  base = emacs-base;
                  overrides = overrides;
                  packages = emacs-packages;
                };
              })
            ];
      
      in {
        packages = genAttrs systems mkPackages;
      })
      (let
        mkOverlays = system: [
          # mix-in all local packages, so they are available as pkgs.${packages-name}
          (final: prev: self.packages.${system})
      
          (final: prev: {
            unstable = import inputs.nixpkgs-unstable {
              inherit system;
              overlays = self.overlays.${system};
              config = { allowUnfree = true; };
            };
          })
          (final: prev: {
            lib = prev.lib // {
              linux-only = prev.lib.mkIf final.stdenv.isLinux;
            };
          })
          inputs.emacs-overlay.overlay
          (final: prev: {
            hunspellDicts = prev.hunspellDicts // {
              # uk_UA affix file strips all latin characters from input, so
              # combining en_US,uk_UA stops spell-checking english words
              # entirely.
              #
              # Remove these iconv lines, so that we can use both dictionaries
              # together.
              uk_UA = prev.hunspellDicts.uk_UA.overrideAttrs (old: {
                postInstall = ''
                  sed -i 's/^ICONV 64$/ICONV 2/' $out/share/hunspell/uk_UA.aff
                  sed -i '/^ICONV [^ ]* 0$/d' $out/share/hunspell/uk_UA.aff
                '';
              });
            };
          })
        ];
      in {
        overlays = genAttrs systems mkOverlays;
      })
      (let
        darwinHosts = {
          bayraktar = {
            system = "aarch64-darwin";
            systemStateVersion = 4;
            homeManagerStateVersion = "21.05";
          
          
            modules = [
              ({ lib, ... }: {
                home-manager.users."rasen" = {
                  programs.git.userEmail = lib.mkForce "oleksii@fluxon.com";
                };
              })
            ];
          
          };
          oleksiishmalko = {
            system = "aarch64-darwin";
            primaryUser = "oleksii.shmalko";
            systemStateVersion = 6;
            homeManagerStateVersion = "25.05";
          
            modules = [
              ({ lib, ... }: {
                home-manager.users."oleksii.shmalko" = {
                  programs.git.userEmail = lib.mkForce "oleksii.shmalko@datadoghq.com";
                };
              })
            ];
          };
        };
      
        mkDarwinConfiguration = { name, system, systemStateVersion, homeManagerStateVersion, primaryUser ? "rasen", modules ? [] }:
          inputs.darwin.lib.darwinSystem {
            inherit system;
            modules = modules ++ [
              {
                networking.hostName = name;
                system.primaryUser = primaryUser;
                system.stateVersion = systemStateVersion;
                home-manager.sharedModules = [
                  ({lib, ...}: {
                    home.stateVersion = lib.mkForce homeManagerStateVersion;
                  })
                ];
              }
              self.darwin-common
            ];
          };
      
      in {
        darwin-common = { lib, pkgs, config, ... }: {
          imports = [
            {
              nix = {
                extraOptions = ''
                  experimental-features = nix-command flakes
                '';
              };
            }
            {
              imports = [inputs.home-manager.darwinModules.home-manager];
              home-manager = {
                useUserPackages = false;
                useGlobalPkgs = true;
                users."${config.system.primaryUser}" = { ... }: {
                  imports = [
                    inputs.self.lib.home-manager-common
                    { hostname = config.networking.hostName; }
                  ];
                };
                # users.rasen = inputs.self.lib.home-manager-common;
                backupFileExtension = "bak";
              };
            }
            {
              nixpkgs.config = {
                allowUnfree = true;
              };
              nixpkgs.overlays = self.overlays.aarch64-darwin;
            }
            {
              users.users.${config.system.primaryUser} = {
                description = "Oleksii Shmalko";
                home = "/Users/${config.system.primaryUser}/";
              };
            }
            {
              system.defaults = {
                NSGlobalDomain = {
                  # auto switch light/dark mode
                  AppleInterfaceStyleSwitchesAutomatically = true;
            
                  # disable automatic capitalization
                  NSAutomaticCapitalizationEnabled = false;
            
                  # Use F1, F2, etc keys by default (without needing to press fn)
                  "com.apple.keyboard.fnState" = true;
            
                  # disable beeps
                  "com.apple.sound.beep.volume" = 0.0;
            
                  # Disable natural scrolling
                  "com.apple.swipescrolldirection" = false;
                };
            
                dock = {
                  # Auto-hide dock
                  autohide = true;
                };
            
                finder = {
                  # Show extensions in finder
                  AppleShowAllExtensions = true;
                  # Prefer list view
                  FXPreferredViewStyle = "Nlsv";
                  # Open home directory by default
                  NewWindowTarget = "Home";
                  # Show pathbar
                  ShowPathbar = true;
                };
            
                # Disable default fn keyboard layout switch. I have my own with Karabiner
                hitoolbox.AppleFnUsageType = "Do Nothing";
            
                # Show seconds in clock
                menuExtraClock.ShowSeconds = true;
              };
            
              system.keyboard = {
                enableKeyMapping = true;
                # Move tilde back on non-US keyboards
                nonUS.remapTilde = true;
                # Remap caps lock to control
                remapCapsLockToControl = true;
              };
            }
            {
              services.yabai = {
                enable = true;
                package = pkgs.yabai;
                config = {
                  layout = "bsp";
                  window_shadow = "float";
                  window_gap = 10;
                  focus_follows_mouse = "autoraise";
                  mouse_follows_focus = "on";
                  mouse_modifier = "fn";
                  mouse_action1 = "move";
                  mouse_action2 = "resize";
                };
                extraConfig = ''
                  yabai -m rule --add app=Emacs manage=on
                '';
              };
            }
            {
              environment.systemPackages = [ pkgs.skhd ];
              services.skhd = {
                enable = true;
                skhdConfig = ''
                  :: default : emacsclient -e '(message "default mode")'
                  :: escaping : emacsclient -e '(message "escaping mode")'
                  # :: e : emacsclient -e '(message "escape mode")'
                  # :: es ; e
                  # #default < lcmd - 0x2a ; e
                  # e < lcmd - 0x2a ; default
            
                  default < lcmd - 0x2A ; escaping
                  escaping < lcmd - 0x2A ; default
            
                  # escaping < lcmd - e ~
            
                  default < cmd + ctrl - r : yabai -m space --layout $(yabai -m query --spaces --space | jq -r 'if .type == "bsp" then "float" else "bsp" end')
                  # default < lcmd + ctrl - n : yabai -m window --ratio rel:-0.05
                  # default < lcmd + ctrl - o : yabai -m window --ratio rel:+0.05
                  default < lcmd + shift - n : yabai -m window --swap west
                  default < lcmd + shift - u : yabai -m window --swap north
                  default < lcmd + shift - e : yabai -m window --swap south
                  default < lcmd + shift - o : yabai -m window --swap east
                  default < lcmd - n [
                    * : yabai -m window --focus west
                    "Emacs" ~
                  ]
                  default < lcmd - u [
                    * : yabai -m window --focus north
                    "Emacs" ~
                  ]
                  default < lcmd - e [
                    * : yabai -m window --focus south
                    "Emacs" ~
                  ]
                  default < lcmd - o [
                    * : yabai -m window --focus east
                    "Emacs" ~
                  ]
                '';
              };
            }
            {
              system.defaults.CustomUserPreferences = {
                "com.apple.symbolichotkeys" = {
                  AppleSymbolicHotKeys = let
            
                    disabled = { enabled = false; };
            
                    modifier = enabled: code: if enabled then code else 0;
                    enabled =
                      { shift ? false
                      , control ? false
                      , option ? false
                      , command ? false
                      , function ? false
                        # Keycode from
                        # /System/Library/Frameworks/Carbon.framework/Versions/A/Frameworks/HIToolbox.framework/Versions/A/Headers/Events.h
                      , key
                        # ASCII code of the key
                      , ascii ? 65535
                      }:
                      let
                        modifiers = lib.foldl' builtins.bitOr 0 [
                          (modifier shift    (lib.fromHexString "0x020000"))
                          (modifier control  (lib.fromHexString "0x040000"))
                          (modifier option   (lib.fromHexString "0x080000"))
                          (modifier command  (lib.fromHexString "0x100000"))
                          (modifier function (lib.fromHexString "0x800000"))
                        ];
                      in {
                        enabled = true;
                        value = {
                          parameters = [ascii key modifiers];
                          type = "standard";
                        };
                      };
            
                  in {
                    # Move left a space
                    "79" = enabled {
                      command = true;
                      control = true;
                      key = 123; # left arrow
                    };
                    # Move right a space
                    "81" = enabled {
                      command = true;
                      control = true;
                      key = 124; # right arrow
                    };
                    # quick note
                    "190" = disabled;
            
                    # Cmd-P -> Show Launchpad
                    "160" = enabled {
                      command = true;
                      key = 31; # o
                      ascii = 112; # p
                    };
                    # Show spotlight search
                    "64" = disabled;
                  };
                };
              };
            
              system.activationScripts.activateHotkeys = {
                enable = true;
                text = ''
                  /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
                '';
              };
            }
            {
              environment.systemPackages = [ pkgs.syncthing ];
            }
            {
              environment.systemPackages = [ pkgs.gnupg ];
              programs.gnupg.agent = {
                enable = true;
                enableSSHSupport = true;
              };
            }
            {
              environment.systemPackages = [ pkgs.pinentry_mac ];
            }
            {
              programs.fish.enable = true;
              programs.zsh.enable = true;
              # add fish to default shells
              environment.shells = [ pkgs.fish ];
              # set it as default for me
              users.users."${config.system.primaryUser}".shell = pkgs.fish;
            }
          ];
        };
      
        darwinConfigurations = genHosts darwinHosts mkDarwinConfiguration;
      })
    ];
}
