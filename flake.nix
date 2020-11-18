{
  description = "Moritz's NixOS/home-manager configuration";

  # edition = 201909;

  inputs = {
    nixpkgs = {
      type = "github";
      # owner = "rasendubi";
      # repo = "nixpkgs";
      # ref = "melpa-2020-04-27";
      owner = "moritzschaefer";
      # repo = "nixpkgs-channels";
      repo = "nixpkgs";
      rev = "246294708d4b4d0f7a9b63fb3b6866860ed78704";
      # ref = "nixpkgs-unstable";
      ref = "master";
    };
    nixos-hardware = {
      type = "github";
      owner = "NixOS";
      repo = "nixos-hardware";
      flake = false;
    };
    nur = {
      url = github:nix-community/NUR;
    };
    home-manager = {
      type = "github";
      owner = "rycee";
      repo = "home-manager";
      ref = "bqv-flakes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixos-hardware, home-manager, nur }@inputs:
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
          hosts = ["moxps"];
          mkHost = name:
            nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = [
                { nixpkgs = { inherit pkgs;  }; }
                (import ./nixos-config.nix)
                { nixpkgs.overlays = [ nur.overlay ]; }
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
                  url = "https://input.fontbureau.com/build/?fontSelection=fourStyleFamily&regular=InputMonoNarrow-Regular&italic=InputMonoNarrow-Italic&bold=InputMonoNarrow-Bold&boldItalic=InputMonoNarrow-BoldItalic&a=0&g=0&i=topserif&l=serifs_round&zero=0&asterisk=height&braces=straight&preset=default&line-height=1.2&accept=I+do&email=";
                  sha256 = "888bbeafe4aa6e708f5c37b42fdbab526bc1d125de5192475e7a4bb3040fc45a";
                };
                outputHash = "1w2i660dg04nyc6fc6r6sd3pw53h8dh8yx4iy6ccpii9gwjl9val";
              }));
            }
          ];

      overlays = [
        (_self: _super: self.packages.x86_64-linux)
        (_self: _super: { conda = _super.conda.override { extraPkgs = [ _super.which ]; }; })  # this is an overlay
        ( let
            myOverride = {
              packageOverrides = _self: _super: {
                service-factory =_super.buildPythonPackage rec {
                  pname = "service_factory";
                  version = "0.1.6";
                  propagatedBuildInputs = [ _super.pytest ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "abd8e715e2d32ee83ea4bbe365d34e0f94e3068ec03683f09f4512f657e1cd64";
                  };
                };
              
                json-rpc =_super.buildPythonPackage rec {
                  pname = "json-rpc";
                  version = "1.13.0";
                  buildInputs = [ _super.pytest ];
                  propagatedBuildInputs = [ _super.pytest ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "def0dbcf5b7084fc31d677f2f5990d988d06497f2f47f13024274cfb2d5d7589";
                  };
                };
                up-set-plot = _super.buildPythonPackage rec {
                  pname = "UpSetPlot";
                  version = "0.4.1";
                  buildInputs = [ _super.pytestrunner ];
                  propagatedBuildInputs = [ _super.matplotlib _super.pandas ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "c1e23af4d90ca88d024cdea45dc3a84591cd97a80a6a3dfc18b5e7ad2b93944f";
                  };
                };
                adjust-text = _super.buildPythonPackage rec {
                  pname = "adjustText";
                  version = "0.7.3";
                  propagatedBuildInputs = [ _super.matplotlib _super.numpy ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "b90e275a95b4d980cbbac7967914b8d66477c09bc346a0b3c9e2125bba664b06";
                  };
                };
              };
            };
          in _self: _super: rec {
            # Add an override for each required python version. 
            # There’s currently no way to add a package that’s automatically picked up by 
            # all python versions, besides editing python-packages.nix
            python2 = _super.python2.override myOverride;
            python3 = _super.python3.override myOverride;
            python38 = _super.python38.override myOverride;
            python2Packages = python2.pkgs;
            python3Packages = python3.pkgs;
            # python37Packages = python37.pkgs;
            python38Packages = python38.pkgs;
          } )
      ];

      homeManagerConfigurations.x86_64-linux =
        let
          hosts = ["MoritzSchaefer"];
          mkHost = hostname:
            home-manager.lib.homeManagerConfiguration {
              configuration = { ... }: {
                nixpkgs.config.allowUnfree = true;
                nixpkgs.overlays = self.overlays;
                imports = [(import ./.config/nixpkgs/home.nix)];
              };
              username = "moritz";
              homeDirectory = "/home/moritz";
              inherit system pkgs;
            };
        in nixpkgs.lib.genAttrs hosts mkHost;
    };
}
