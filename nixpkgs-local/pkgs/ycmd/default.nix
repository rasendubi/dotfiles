{ stdenv, fetchgit, cmake, python, llvmPackages, boost, pythonPackages, buildPythonPackage, makeWrapper
}:

stdenv.mkDerivation rec {
  name = "ycmd-2016-01-02";

  src = fetchgit {
    url = "git://github.com/Valloric/ycmd.git";
    rev = "79c8bc256e9036c397a3ff2c0589578f816b46ec";
    sha256 = "0y261xbd73v6fbhv5jprdjydgp9yh7skzrh9rglk6zm3jlwvbj9z";
  };

  buildInputs = [ python cmake llvmPackages.clang boost makeWrapper ];

  propagatedBuildInputs = with pythonPackages; [ waitress frozendict bottle ];

  configurePhase = ":";

  buildPhase = ''
    ./build.py --clang-completer --system-libclang --system-boost
  '';

  installPhase = with pythonPackages; ''
    mkdir -p $out/lib/ycmd/third_party $out/bin
    cp -r ycmd/ CORE_VERSION libclang.so.* ycm_client_support.so ycm_core.so $out/lib/ycmd/
    wrapProgram $out/lib/ycmd/ycmd/__main__.py \
      --prefix PYTHONPATH : "$(toPythonPath ${waitress}):$(toPythonPath ${frozendict}):$(toPythonPath ${bottle})"
    ln -s $out/lib/ycmd/ycmd/__main__.py $out/bin/ycmd
  '';

  meta = {
    description = "A code-completion and comprehension server";
    homepage = "https://github.com/Valloric/ycmd";
    license = stdenv.lib.licenses.gpl3;
    platforms = stdenv.lib.platforms.all;
  };
}
