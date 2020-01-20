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
}
