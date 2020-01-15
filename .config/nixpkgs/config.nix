{
  allowUnfree = true;
  packageOverrides = pkgs: {
    nur = import /home/rasen/dotfiles/channels/nur {
      inherit pkgs;
    };
  };
}
