{
  description = "rasendubi's NixOS/home-manager configuration";

  edition = 202005;

  inputs = {
    nixpkgs = {
      type = "github";
      # owner = "rasendubi";
      # repo = "nixpkgs";
      # ref = "melpa-2020-04-27";
      owner = "NixOS";
      repo = "nixpkgs-channels";
      ref = "nixpkgs-unstable";
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
          hosts = ["moxps"];
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
            
          ];

      overlays = [
        (_self: _super: self.packages.x86_64-linux)
        
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
              username = "moritz";
              homeDirectory = "/home/moritz";
              inherit system pkgs;
            };
        in nixpkgs.lib.genAttrs hosts mkHost;
    };
}
