{
  description = "Moritz's NixOS/home-manager configuration";

  # edition = 201909;

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-23.11";
    };
    nixpkgs-2009 = {
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
      # rev = "246294708d4b4d0f7a9b63fb3b6866860ed78704";
      # ref = "nixpkgs-unstable";
      ref = "fix-libnvidia-container";
    };
    # nixpkgs-local = {
    #   url = "/home/moritz/Projects/nixpkgs/";
    # };
    
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
      owner = "nix-community";
      repo = "home-manager";
      ref = "release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix.url = "github:ryantm/agenix";
    musnix = {
      type = "github";
      owner = "musnix";
      repo = "musnix";
      flake = false;
    };
  };
  
# nixpkgs-local
  outputs = { self, nixpkgs, nixpkgs-moritz, nixpkgs-2009, nixpkgs-unstable, nixos-hardware, home-manager, nur, agenix, musnix }@inputs:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = self.overlays;
        config = { allowUnfree = true;
                    allowBroken = true;
                    nvidia.acceptLicense = true;
                    permittedInsecurePackages = [
                      "adobe-reader-9.5.5"
                      "qtwebkit-5.212.0-alpha4"
                      "openjdk-18+36"
                      "python-2.7.18.6"
                    ];
                    };
      };
    in {
      nixosConfigurations =
        let
          hosts = ["moxps" "mobook" "mopad"];
          mkHost = name:
            nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              modules = [
                { nixpkgs = { inherit pkgs;  }; }
                (import ./nixos-config.nix)
                { nixpkgs.overlays = [ nur.overlay ]; }
                agenix.nixosModules.default
                {
                  environment.systemPackages = [ agenix.packages.${system}.default ];
                  age.identityPaths = [ "/home/moritz/.ssh/id_ed25519_agenix" ];
                }
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
        
            config = { allowUnfree = true;  allowBroken = true; nvidia.acceptLicense = true; };
          };
          nixpkgs-2009 = import inputs.nixpkgs-2009 {
            inherit system;
            overlays = self.overlays; # .${system};
            config = { allowUnfree = true; };
          };
        
          # mkNvidiaContainerPkg = { name, containerRuntimePath, configTemplate, additionalPaths ? [] }:
          #   let
          #     nvidia-container-runtime = pkgs.callPackage "${inputs.nixpkgs}/pkgs/applications/virtualization/nvidia-container-runtime" {
          #       inherit containerRuntimePath configTemplate;
          #     };
          #   in pkgs.symlinkJoin {
          #     inherit name;
          #     paths = [
          #       # (callPackage ../applications/virtualization/libnvidia-container { })
          #       (pkgs.callPackage "${inputs.nixpkgs-moritz}/pkgs/applications/virtualization/libnvidia-container" { inherit (pkgs.linuxPackages) nvidia_x11; })
          #       nvidia-container-runtime
          #       (pkgs.callPackage "${inputs.nixpkgs}/pkgs/applications/virtualization/nvidia-container-toolkit" {
          #         inherit nvidia-container-runtime;
          #       })
          #     ] ++ additionalPaths;
          #   };
        
          # nvidia-docker = pkgs.mkNvidiaContainerPkg {
          #   name = "nvidia-docker";
          #   containerRuntimePath = "${pkgs.docker}/libexec/docker/runc";
          #   # configTemplate = "${inputs.nixpkgs}/pkgs/applications/virtualization/nvidia-docker/config.toml";
          #   configTemplate = builtins.toFile "config.toml" ''
          #   disable-require = false
          #   #swarm-resource = "DOCKER_RESOURCE_GPU"
        
          #   [nvidia-container-cli]
          #   #root = "/run/nvidia/driver"
          #   #path = "/usr/bin/nvidia-container-cli"
          #   environment = []
          #   debug = "/var/log/nvidia-container-runtime-hook.log"
          #   ldcache = "/tmp/ld.so.cache"
          #   load-kmods = true
          #   #no-cgroups = false
          #   #user = "root:video"
          #   ldconfig = "@@glibcbin@/bin/ldconfig"
          #   '';
          #   additionalPaths = [ (pkgs.callPackage "${inputs.nixpkgs}/pkgs/applications/virtualization/nvidia-docker" { }) ];
          # };
          # mesa-pin = import inputs.mesa-pin {
          #   inherit system;
          #   overlays = self.overlays; # .${system};
          #   config = { allowUnfree = true; };
          # };
        })
        (_self: _super: { emacs = _super.emacs29; exwm-emacs = ((_super.emacsPackagesFor _super.emacs29).emacsWithPackages (epkgs: with epkgs; [ emacsql-sqlite _super.imagemagick _super.escrotum vterm exwm ])); })  # emasc.withPackages is not available :((((
        
        (_self: _super: { conda = _super.conda.override { extraPkgs = [ _super.libffi_3_3 _super.libffi _super.which _super.libxcrypt ]; }; })  # this is an overlay
        # TODO override R package  (openssl)
        ( let
            myOverride = rec {
              packageOverrides = _self: _super: {
              
                # python-socks = _super.buildPythonPackage rec { # overwrite because too old
                #   pname = "python-socks";
                #   version = "2.0.3";
        
                #   src = _super.fetchPypi {
                #     inherit pname version;
                #     # sha256 = "e3a9ca8e554733862ce4d8ce1d10efb480fd3a3acdafd03393943ec00c98ba8a"; 2.0.3
                #   };
        
                #   propagatedBuildInputs = with _super; [ trio curio async-timeout anyio ];
                # };
        
                # aiohttp-socks-new = _super.buildPythonPackage rec {  # if >=0.7 is needed
                #   pname = "aiohttp-socks";
                #   version = "0.7.1";
                #   propagatedBuildInputs = [ _super.aiohttp _super.attrs _self.python-socks];
                #   doCheck = false;
                #   src = _super.fetchPypi {
                #     inherit version;
                #     pname = "aiohttp_socks";
                #     sha256 = "2215cac4891ef3fa14b7d600ed343ed0f0a670c23b10e4142aa862b3db20341a";
                #   };
                # };
                googletransx = _super.buildPythonPackage rec {
                  pname = "googletransx";
                  version = "2.4.2";
                  propagatedBuildInputs = [ _super.requests ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "c46567e3365c2abbe8af1004121b6303f530bf72025d1c3045ed14861902d6da";
                  };
                };
                twint = _super.buildPythonPackage rec {
                  pname = "twint";
                  version = "2.1.22";
                  propagatedBuildInputs = with _super; [ aiohttp aiodns beautifulsoup4 cchardet elasticsearch pysocks pandas aiohttp-socks schedule geopy fake-useragent _self.googletransx ];
                  
                  postPatch = ''
                    substituteInPlace setup.py --replace "dataclasses" ""
                  '';
                  doCheck = false;
                  src = builtins.fetchGit {
                    url = "https://github.com/twintproject/twint/";
                    rev = "e7c8a0c764f6879188e5c21e25fb6f1f856a7221";
                  };
                };
                pdfannots = _super.buildPythonPackage rec {
                  pname = "pdfannots";
                  version = "0.3";
                  propagatedBuildInputs = [ _super.pdfminer ];
                  nativeBuildInputes = [ _super.setuptools-scm ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "5931fdab0f06283536b58782bec16109a6c193816d6df0ab737924513ea7ed0a";
                  };
                };
        
                cachew = _super.buildPythonPackage rec {
                  pname = "cachew";
                  version = "0.9.0";
                  propagatedBuildInputs = [ _super.setuptools-scm _super.appdirs _super.sqlalchemy ];
                  nativeBuildInputes = [ _super.setuptools-scm ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "8d2b82260e35c48e9c27efc8054c46ff3fe2c0a767e7534f1be2719541b5d8a7";
                  };
                };
                hpi =_super.buildPythonPackage rec {
                  pname = "HPI";
                  version = "0.3.20211031";
                  propagatedBuildInputs = [ _super.pytz _super.appdirs _super.more-itertools _super.decorator _super.click _super.setuptools-scm _super.logzero _self.cachew _super.mypy ];  # orjson
                  nativeBuildInputes = [ _super.setuptools-scm ];
                  SETUPTOOLS_SCM_PRETEND_VERSION = version;
                  doCheck = false;
                  src = builtins.fetchGit {
                    url = "git://github.com/karlicoss/HPI";
                    rev = "a1f03f9c028df9d1898de2cc14f1df4fa6d8c471";
                  };
                };
                orger =_super.buildPythonPackage rec {
                  pname = "orger";
                  version = "0.3.20210220";
                  propagatedBuildInputs = [ _super.appdirs _super.atomicwrites _super.setuptools-scm ];
                  nativeBuildInputes = [ _super.setuptools-scm ];
                  doCheck = false;
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "cb6191e685c91f3bb760b2997c386e0f5e94562d13ab0dc69230c60ddbf52cf0";
                  };
                };
        
        
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
                # pyensembl = _super. buildPythonPackage rec {
                #   version = "1.8.5";
                #   pname = "pyensembl";
        
                #   src = _super.fetchPypi {
                #     inherit pname version;
                #     sha256 = "13dd05aba296e4acadb14de5a974e6f73834452851a36b9237917ae85b3e060f";
                #   };
        
                #   propagatedBuildInputs = [ _super.numpy _super.pandas _self.datacache _super.six _self.memoized-property
                #                             _self.gtfparse _self.tinytimer _self.serializable ];
        
                #   # pythonImportsCheck = [ "pyensembl" ];
                #   doCheck = false;  # import fails (only) in build environment because pyensembl creates a file in root directory
        
                #   # meta = with stdenv.lib; {
                #   #   homepage = "https://github.com/openvax/pyensembl";
                #   #   description = " Python interface to access reference genome features (such as genes, transcripts, and exons) from Ensembl ";
                #   #   license = licenses.asl20;
                #   #   maintainers = [ maintainers.moritzs ];
                #   # };
                # };
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
                    rev = "70ea50616366c87ef730f53efb192217b725a9f0";
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
                # serializable = _super.buildPythonPackage rec {
                #   version = "0.2.1";
                #   pname = "serializable";
        
                #   src = _super.fetchPypi {
                #     inherit pname version;
                #     sha256 = "ec604e5df0c1236c06d190043a407495c4412dd6b6fd3b45a8514518173ed961";
                #   };
        
                #   checkInputs = [ _super.nose ];
                #   propagatedBuildInputs = [ _self.typechecks _super.six _super.simplejson ];
        
                #   checkPhase = ''
                #     nosetests
                #   '';
        
                #   # meta = with stdenv.lib; {
                #   #   homepage = "https://github.com/iskandr/serializable";
                #   #   description = "Base class with serialization methods for user-defined Python objects";
                #   #   license = licenses.asl20;
                #   #   maintainers = [ maintainers.moritzs ];
                #   # };
                # };
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
                easydev = _super.buildPythonPackage rec {
                  version = "0.12.0";
                  pname = "easydev";
        
                  propagatedBuildInputs = [
                      _super.colorama
                      _super.pexpect
                      _super.colorlog
                      ];
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "f4a340c5ffe193654c387d271bcd466d1fe56bf9850f2704122d3b52b1e6090d";
                  };
                  checkPhase = ''
                    
                  '';
                };
                # attrs = _super.buildPythonPackage rec {
                #   pname = "attrs";
                #   version = "21.2.0";
        
                #   src = _super.fetchPypi {
                #     inherit pname version;
                #     sha256 = "ef6aaac3ca6cd92904cdd0d83f629a15f18053ec84e6432106f7a4d04ae4f5fb";
                #   };
        
                #   # macOS needs clang for testing
                #   checkInputs = [
                #     _super.pytest _super.hypothesis _super.zope_interface _super.pympler _super.coverage _super.six
                #   ];
        
                #   checkPhase = ''
                #     py.test
                #   '';
        
                #   # To prevent infinite recursion with pytest
                #   doCheck = false;
        
                # };
                requests-cache = _super.buildPythonPackage rec {
                  version = "0.8.0";
                  pname = "requests-cache";
        
                  propagatedBuildInputs = [
                      _super.requests
                      _super.appdirs
                      _super.attrs
                      _super.cattrs
                      _super.url-normalize
                      ];
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "2f80b2a43d6bb886558181133d9b74db12f1eed42c190b53d8e98ab62a0d2231";
                  };
                  checkPhase = ''
                    
                  '';
                };
        
                bioservices = _super.buildPythonPackage rec {
                  version = "1.8.0";
                  pname = "bioservices";
        
                  propagatedBuildInputs = [
                      _super.grequests
                      _super.requests
                      _self.requests-cache
                      _self.easydev
                      _super.beautifulsoup4
                      _super.xmltodict
                      _super.lxml
                      _super.suds-jurko
                      _super.appdirs
                      _super.wrapt
                      _super.pandas
                      _super.colorlog
                      ];
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "e581f7096b0083afa1e9d5b075c46b5a8e042767ca0fedb617daa50d1e1a739f";
                  };
                  checkPhase = ''
                    
                  '';
                  # pythonImportsCheck = [ "smogn" ];
                };
        
                gseapy = _super.buildPythonPackage rec {
                  version = "0.10.4";
                  pname = "gseapy";
        
                  propagatedBuildInputs = [
                      _super.scipy
                      _super.matplotlib
                      _super.requests
                      _super.joblib
                      _self.bioservices
                      _self.numpy
                      _super.pandas ];
                  src = _super.fetchPypi {
                    inherit pname version;
                    sha256 = "6404b79a3b5dc07ed39f6a4f67b3c662df5bd8b0d50829c2819d8921a768dffb";
                  };
                  checkPhase = ''
                    
                  '';
                };
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
                # seaborn = _super.buildPythonPackage rec {
                #   pname = "seaborn";
                #   version = "0.11.1";
                #   disabled = _super.pythonOlder "3.6";
                #   doCheck = false;
        
                #   src = _super.fetchPypi {
                #     inherit pname version;
                #     sha256 = "44e78eaed937c5a87fc7a892c329a7cc091060b67ebd1d0d306b446a74ba01ad";
                #   };
        
                #   checkInputs = [ _super.nose ];
                #   propagatedBuildInputs = [ _super.pandas _super.matplotlib ];
                # };
                # scikitlearn_0241 = _super.buildPythonPackage rec {
                #   pname = "scikit-learn";
                #   version = "0.24.1";
                #   doCheck = false;
        
                #   src = _super.fetchPypi {
                #     inherit pname version;
                #     sha256 = "oDNKGALmTWVgIsO/q1anP71r9LEpg0PzaIryFRgQu98=";
                #   };
        
                #   buildInputs = [
                #     _super.pillow
                #     pkgs.gfortran
                #     pkgs.glibcLocales
                #   ] ++ pkgs.lib.optionals pkgs.stdenv.cc.isClang [
                #     pkgs.llvmPackages.openmp
                #   ];
        
                #   nativeBuildInputs = [
                #     _super.cython
                #   ];
        
                #   propagatedBuildInputs = [
                #     _super.numpy
                #     _super.scipy
                #     _super.numpy.blas
                #     _super.joblib
                #     _super.threadpoolctl
                #   ];
                #   LC_ALL="en_US.UTF-8";
                # };
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

      homeConfigurations.moritz =
        #let
          # hosts = ["MoritzSchaefer"];
          # mkHost = hostname:
            home-manager.lib.homeManagerConfiguration {
              pkgs = nixpkgs.legacyPackages.${system};
              # nixpkgs.config.allowUnfree = true;
              # nixpkgs.overlays = self.overlays;
              modules = [ ./.config/nixpkgs/home.nix {
                home = {
                  username = "moritz";
                  homeDirectory = "/home/moritz";
                  stateVersion = "18.09";
                };
                }
              ];
            };
        # in nixpkgs.lib.genAttrs hosts mkHost;
    };
}
