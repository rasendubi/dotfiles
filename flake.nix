{
  description = "Moritz's NixOS/home-manager configuration";

  # edition = 201909;

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-20.09";
    };
    nixpkgs-unstable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };
    nixpkgs-moritz = {
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
    musnix = {
      type = "github";
      owner = "musnix";
      repo = "musnix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-moritz, nixpkgs-unstable, nixos-hardware, home-manager, nur, musnix }@inputs:
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
          hosts = ["moxps" "mobook"];
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
        (final: prev: {
          unstable = import inputs.nixpkgs-unstable {
            inherit system;
            overlays = self.overlays; # .${system};
            config = { allowUnfree = true; };
          };
        })
        (_self: _super: { conda = _super.conda.override { extraPkgs = [ _super.which ]; }; })  # this is an overlay
        # TODO override R package  (openssl)
        ( let
            myOverride = rec {
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
                matplotlib-venn = _super.buildPythonPackage rec {
                  version = "0.11.5";
                  pname = "matplotlib-venn";
        
                  src = builtins.fetchGit {
                    url = "git://github.com/konstantint/matplotlib-venn";
                    rev = "c26796c9925bdac512edf48387452fbd1848c791";
                  };
        
                  checkInputs = [ _super.pytest ];
                  propagatedBuildInputs = [ _super.matplotlib _super.numpy _super.scipy ];
        
                  checkPhase = ''
                    pytest
                  '';
        
                  # Tests require extra dependencies
                  doCheck = false;
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/konstantint/matplotlib-venn";
                  #   description = "Area-weighted venn-diagrams for Python/matplotlib";
                  #   license = licenses.mit;
                  # };
                };
                swifter = _super.buildPythonPackage rec {
                  version = "0.304";
                  pname = "swifter";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "5fe99d18e8716e82bce5a76322437d180c25ef1e29f1e4c5d5dd007928a316e9";
                  };
        
                  checkInputs = [ _super.nose ];
                  propagatedBuildInputs = [ _super.pandas _super.psutil _super.dask _super.tqdm
                                            _super.ipywidgets _super.numba _super.bleach
                                            _super.parso _super.distributed ];
        
                  disabled = _super.pythonOlder "3.7";
        
                  pythonImportsCheck = [ "swifter" ];
                  checkPhase = ''
                    nosetests
                  '';
        
                  # Tests require extra dependencies
                  doCheck = true;
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/jmcarpenter2/swifter";
                  #   description = "A package which efficiently applies any function to a pandas dataframe or series in the fastest available manner";
                  #   license = licenses.mit;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                pyensembl = _super. buildPythonPackage rec {
                  version = "1.8.5";
                  pname = "pyensembl";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "13dd05aba296e4acadb14de5a974e6f73834452851a36b9237917ae85b3e060f";
                  };
        
                  propagatedBuildInputs = [ _super.numpy _super.pandas _self.datacache _super.six _self.memoized-property
                                            _self.gtfparse _self.tinytimer _self.serializable ];
        
                  # pythonImportsCheck = [ "pyensembl" ];
                  doCheck = false;  # import fails (only) in build environment because pyensembl creates a file in root directory
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/openvax/pyensembl";
                  #   description = " Python interface to access reference genome features (such as genes, transcripts, and exons) from Ensembl ";
                  #   license = licenses.asl20;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                gffutils = _super.buildPythonPackage rec {
                  version = "0.10.1";
                  pname = "gffutils";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "a8fc39006d7aa353147238160640e2210b168f7849cb99896be3fc9441e351cb";
                  };
        
        
                  checkInputs = [ _super.nose _super.wget ];
                  propagatedBuildInputs = [ _super.pyfaidx _super.six _super.argh _super.argcomplete _super.simplejson ];
                  doCheck = false;
        
                  # checkPhase = ''  # unfortunately fails
                  #   # sh gffutils/test/data/download-large-annotation-files.sh
                  #   # nosetests
                  # '';
                  pythonImportsCheck = [ "gffutils" ];
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/daler/gffutils";
                  #   description = "GFF and GTF file manipulation and interconversion http://daler.github.io/gffutils";
                  #   license = licenses.mit;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                gtfparse = _super.buildPythonPackage rec {
                  version = "1.2.0";
                  pname = "gtfparse";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "2f27aa2b87eb43d613edabf27f9c11147dc595c8683b440ac1d88e9acdb85873";
                  };
        
                  checkInputs = [ _super.nose _super.six ];
                  propagatedBuildInputs = [ _super.numpy _super.pandas ];
                  doCheck = false;
        
                  pythonImportsCheck = [ "gtfparse" ];
                  # checkPhase = ''
                  #   # PYTHONPATH='test' nosetests # fails because six is not found
                  # '';
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/openvax/gtfparse";
                  #   description = " Parsing tools for GTF (gene transfer format) files ";
                  #   license = licenses.asl20;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                memoized-property = _super.buildPythonPackage rec {
                  version = "1.0.3";
                  pname = "memoized-property";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "4be4d0209944b9b9b678dae9d7e312249fe2e6fb8bdc9bdaa1da4de324f0fcf5";
                  };
        
        
                  pythonImportsCheck = [ "memoized_property" ];
                  doCheck = false;
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/estebistec/python-memoized-property";
                  #   description = "A simple python decorator for defining properties that only run their fget function once ";
                  #   license = licenses.bsd3;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                pybedtools = _super.buildPythonPackage rec {
                  version = "0.8.1";
                  pname = "pybedtools";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "c035e078617f94720eb627e20c91f2377a7bd9158a137872a6ac88f800898593";
                  };
        
                  checkInputs = [ _super.pytest _super.numpydoc _super.psutil _super.pyyaml _super.sphinx ];
                  propagatedBuildInputs = [ _super.numpy _super.pandas _super.pysam _super.six pkgs.zlib pkgs.bash pkgs.bedtools ];  # Is it OK to use pkgs here?
        
                  checkPhase = ''
                    # pytest -v --doctest-modules
                    # ${_super.python.interpreter} -c 'import pybedtools'  # test and import do not work in checkPhase, because the built pyx file cannot be included
                  '';
        
                  # Tests require extra dependencies
                  doCheck = false;
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/daler/pybedtools";
                  #   description = "Python wrapper -- and more -- for Aaron Quinlan's BEDTools (bioinformatics tools) http://daler.github.io/pybedtools";
                  #   license = licenses.gpl2;
                  # };
                };
                scikit-plot = _super.buildPythonPackage rec {
                  version = "0.3.7";
                  pname = "scikit-plot";
        
                  src = builtins.fetchGit {
                    url = "https://github.com/moritzschaefer/scikit-plot";
                    ref = "feature/label-dots";
                    rev = "da4029703ab8bf45f9e417854d75727471ff8596";
                  };
                  
                  # src = _super.fetchPypi {
                  #   inherit pname version;
                  #   sha256 = "2c7948817fd2dc06879cfe3c1fdde56a8e71fa5ac626ffbe79f043650baa6242";
                  # };
        
                  checkInputs = [ _super.nose ];
                  propagatedBuildInputs = [ _super.matplotlib _self.scikitlearn _super.scipy _super.joblib ];
        
                  checkPhase = ''
                    nosetests
                  '';
                };
                datacache = _super.buildPythonPackage rec {
                  version = "1.1.5";
                  pname = "datacache";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "b2ca31b2b9d3803a49645ab4f5b30fdd0820e833a81a6952b4ec3a68c8ee24a7";
                  };
        
                  propagatedBuildInputs = [ _super.pandas _super.appdirs _super.progressbar33 _super.requests _self.typechecks _super.mock ];
        
                  pythonImportsCheck = [ "datacache" ];
                };
                serializable = _super.buildPythonPackage rec {
                  version = "0.2.1";
                  pname = "serializable";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "ec604e5df0c1236c06d190043a407495c4412dd6b6fd3b45a8514518173ed961";
                  };
        
                  checkInputs = [ _super.nose ];
                  propagatedBuildInputs = [ _self.typechecks _super.six _super.simplejson ];
        
                  checkPhase = ''
                    nosetests
                  '';
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/iskandr/serializable";
                  #   description = "Base class with serialization methods for user-defined Python objects";
                  #   license = licenses.asl20;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                tinytimer = _super.buildPythonPackage rec {
                  version = "0.0.0";
                  pname = "tinytimer";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "6ad13c8f01ab6094e58081a5367ffc4c5831f2d6b29034d2434d8ae106308fa5";
                  };
        
                  pythonImportsCheck = [ "tinytimer" ];
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/iskandr/tinytimer";
                  #   description = "Tiny Python benchmarking library";
                  #   license = licenses.asl20;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                typechecks = _super.buildPythonPackage rec {
                  version = "0.1.0";
                  pname = "typechecks";
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "7d801a6018f60d2a10aa3debc3af65f590c96c455de67159f39b9b183107c83b";
                  };
        
                  pythonImportsCheck = [ "typechecks" ];
        
                  # meta = with stdenv.lib; {
                  #   homepage = "https://github.com/openvax/typechecks";
                  #   description = "Helper functions for runtime type checking";
                  #   license = licenses.asl20;
                  #   maintainers = [ maintainers.moritzs ];
                  # };
                };
                # gseapy = _super.buildPythonPackage rec {
                #   version = "0.10.4";
                #   pname = "gseapy";
        
                #   propagatedBuildInputs = [
                #       _super.scipy
                #       _super.matplotlib
                #       _super.requests
                #       _super.joblib
                #       _super.bioservices  # fuucccckkk
                #       _self.numpy
                #       _super.pandas ];
                #   src = _super.fetchPypi {
                #     inherit pname version;
                #     sha256 = "6404b79a3b5dc07ed39f6a4f67b3c662df5bd8b0d50829c2819d8921a768dffb";
                #   };
                #   checkPhase = ''
                    
                #   '';
                #   # pythonImportsCheck = [ "smogn" ];
                # };
                smogn = _super.buildPythonPackage rec {
                  version = "0.1.2";
                  pname = "smogn";
        
                  propagatedBuildInputs = [ _self.numpy _super.pandas _super.tqdm ];
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "6555b907f2c9df223eae8813abd09054ad6491fc8509a23fccc9d578b3e76d89";
                  };
                  checkPhase = ''
                    
                  '';
                  # pythonImportsCheck = [ "smogn" ];
                };
                # I don't know how to overwrite seaborn from unstable. That's why I overwrite it manually..
                seaborn = _super.buildPythonPackage rec {
                  pname = "seaborn";
                  version = "0.11.1";
                  disabled = _super.pythonOlder "3.6";
                  doCheck = false;
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "44e78eaed937c5a87fc7a892c329a7cc091060b67ebd1d0d306b446a74ba01ad";
                  };
        
                  checkInputs = [ _super.nose ];
                  propagatedBuildInputs = [ _super.pandas _super.matplotlib ];
                };
                scikitlearn = _super.buildPythonPackage rec {
                  pname = "scikit-learn";
                  version = "0.24.1";
                  doCheck = false;
        
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "oDNKGALmTWVgIsO/q1anP71r9LEpg0PzaIryFRgQu98=";
                  };
        
                  buildInputs = [
                    _super.pillow
                    pkgs.gfortran
                    pkgs.glibcLocales
                  ] ++ pkgs.lib.optionals pkgs.stdenv.cc.isClang [
                    pkgs.llvmPackages.openmp
                  ];
        
                  nativeBuildInputs = [
                    _super.cython
                  ];
        
                  propagatedBuildInputs = [
                    _super.numpy
                    _super.scipy
                    _super.numpy.blas
                    _super.joblib
                    _super.threadpoolctl
                  ];
                  LC_ALL="en_US.UTF-8";
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
