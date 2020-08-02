{
  allowUnfree = true;
  pulseaudio = true;
  permittedInsecurePackages = [
    "xpdf-4.02"
  ];
  packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
      repoOverrides = {
        moritzschaefer = import /home/moritz/Projects/nur-packages;
      };
    };


    # this is just a test. I may delete it some time?
    myPackages = pkgs.buildEnv {
      name = "my-packages";
      paths = with pkgs; [
        aspell
        bc
        coreutils
        gdb
        ffmpeg
        nixUnstable
        emscripten
        jq
        nox
        silver-searcher
      ];
    };
  };
}
