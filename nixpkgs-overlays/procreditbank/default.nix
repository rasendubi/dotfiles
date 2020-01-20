self: super:
{
  procreditbank-websigner = self.callPackage ./websigner.nix { };

  firefox = super.firefox.override {
    extraNativeMessagingHosts = [ self.procreditbank-websigner ];
  };
}
