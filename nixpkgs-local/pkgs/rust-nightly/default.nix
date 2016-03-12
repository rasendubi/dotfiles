{ date, hash
, stdenv, fetchurl, zlib }:

let
  target =
    if stdenv.system == "i686-linux" then "i686-unknown-linux-gnu" else
    if stdenv.system == "x86_64-linux" then "x86_64-unknown-linux-gnu" else
    if stdenv.system == "i686-darwin" then "i868-apple-darwin" else
    if stdenv.system == "x86_64-darwin" then "x86_64-apple-darwin" else
    abort "no snapshot to bootstrap for this platfrom (missing target triple)";

in stdenv.mkDerivation rec {
  name = "rust-nightly-${date}";

  src = fetchurl {
    url = "https://static.rust-lang.org/dist/${date}/rust-nightly-${target}.tar.gz";
    sha256 = hash;
  };

  installPhase = ''
    ./install.sh --prefix=$out --disable-ldconfig
  '';

  dontStrip = true;

  preFixup = if stdenv.isLinux then let
    rpath = stdenv.lib.concatStringsSep ":" [
      "$out/lib"
      (stdenv.lib.makeLibraryPath [ zlib ])
      ''${stdenv.cc.cc}/lib${stdenv.lib.optionalString stdenv.is64bit "64"}''
    ];
  in
  ''
    for executable in ${stdenv.lib.concatMapStringsSep " " (s: "$out/bin/" + s) [ "cargo" "rustc" "rustdoc" ]}; do
      patchelf --interpreter "${stdenv.glibc}/lib/${stdenv.cc.dynamicLinker}" \
        --set-rpath "${rpath}" \
        "$executable"
    done
    for library in $out/lib/*.so; do
      patchelf --set-rpath "${rpath}" "$library"
    done
  '' else "";
}
