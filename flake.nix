{
  description = "rasendubi's NixOS/home-manager configuration";

  edition = 201909;

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "rasendubi";
      repo = "nixpkgs";
      ref = "melpa-2020-04-27";
      # owner = "NixOS";
      # repo = "nixpkgs-channels";
      # ref = "nixpkgs-unstable";
    };
    nixos-hardware = {
      type = "github";
      owner = "NixOS";
      repo = "nixos-hardware";
      flake = false;
    };
    home-manager = {
      type = "github";
      owner = "rycee";
      repo = "home-manager";
      ref = "bqv-flakes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = self.overlays;
        config = { allowUnfree = true; };
      };
    in {
      nixosConfigurations =
        let
          hosts = ["omicron"];
          mkHost = name:
            nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = [
                { nixpkgs = { inherit pkgs; }; }
                (import ./nixos-config.nix)
              ];
              specialArgs = { inherit name inputs; };
            };
        in nixpkgs.lib.genAttrs hosts mkHost;

      packages.x86_64-linux =
        let
          mergePackages = nixpkgs.lib.foldr nixpkgs.lib.mergeAttrs {};
        in
          mergePackages [
            {
              # note it's a new attribute and does not override old one
              input-mono = (pkgs.input-fonts.overrideAttrs (old: {
                src = pkgs.requireFile {
                  name = "Input-Font.zip";
                  url = "https://input.fontbureau.com/download/index.html?customize&fontSelection=fourStyleFamily&regular=InputMonoNarrow-Regular&italic=InputMonoNarrow-Italic&bold=InputMonoNarrow-Bold&boldItalic=InputMonoNarrow-BoldItalic&a=0&g=0&i=topserif&l=serifs_round&zero=0&asterisk=height&braces=straight&preset=default&line-height=1.2&email=";
                  sha256 = "0nn41w2b6jvsbr3r4lfy4p8w2ssjmgdjzd1pbj7p0vmawjpvx2w8";
                };
                outputHash = "1w2i660dg04nyc6fc6r6sd3pw53h8dh8yx4iy6ccpii9gwjl9val";
              }));
            }
            (let
              websigner =
                { stdenv
                , fetchurl
                , autoPatchelfHook
                , gtk2
                , glib
                , pcsclite
                }:
                stdenv.mkDerivation {
                  pname = "procreditbank-websigner";
                  version = "2020-01-20";
            
                  src = fetchurl {
                    url = "https://ibank.procreditbank.com.ua/websigner-linux.bin";
                    sha256 = "1bm88jg7nhgrmc0q5hv35hgv4nc0d15ihl0acrhf6x5f7wv4pszv";
                  };
            
                  nativeBuildInputs = [ autoPatchelfHook ];
            
                  buildInputs = [ gtk2 glib pcsclite ];
            
                  unpackCmd = ''
                    sh $src --extract
                  '';
            
                  dontConfigure = true;
            
                  dontBuild = true;
            
                  installPhase = ''
                    mkdir -p $out/bin
                    mkdir -p $out/lib/websigner/hosts/firefox
                    mkdir -p $out/lib/websigner/hosts/chromium
            
                    install -m 555 x86_64-linux/npwebsigner.so $out/lib/websigner
                    install -m 777 x86_64-linux/nmwebsigner $out/lib/websigner
            
                    sed "s|PLUGIN_PATH|$out/lib/websigner/nmwebsigner|" com.bifit.websigner-mozilla.json > $out/lib/websigner/hosts/firefox/com.bifit.websigner.json
                    sed "s|PLUGIN_PATH|$out/lib/websigner/nmwebsigner|" com.bifit.websigner-chrome.json > $out/lib/websigner/hosts/chromium/com.bifit.websigner.json
            
                    mkdir -p $out/lib/mozilla/native-messaging-hosts
                    ln -s $out/lib/websigner/hosts/firefox/*.json $out/lib/mozilla/native-messaging-hosts
                  '';
                };
            in {
              procreditbank-websigner = pkgs.callPackage websigner { };
            })
          ];

      overlays = [
        (_self: _super: self.packages.x86_64-linux)
        (self: super: {
          firefox = super.firefox.override {
            extraNativeMessagingHosts = [ self.procreditbank-websigner ];
          };
        })
      ];

      homeManagerConfigurations.x86_64-linux =
        let
          hosts = ["AlexeyShmalko"];
          mkHost = hostname:
            home-manager.lib.homeManagerConfiguration {
              configuration = { ... }: {
                nixpkgs.config.allowUnfree = true;
                nixpkgs.overlays = self.overlays;
                imports = [(import ./.config/nixpkgs/home.nix)];
              };
              username = "rasen";
              homeDirectory = "/home/rasen";
              inherit system pkgs;
            };
        in nixpkgs.lib.genAttrs hosts mkHost;
    };
}
