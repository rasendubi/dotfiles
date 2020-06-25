{ lib, pkgs, config, ... }:
{
  targets.genericLinux.enable = true;

  gtk.enable = true;
  qt.enable = true;

  home.packages = [
    pkgs.minicom
    pkgs.arandr
    pkgs.ripgrep
  ];

  programs.git.userEmail = lib.mkForce "alexey.shmalko@ringteam.com";

  home.sessionVariables.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";

  # This does not work after migration to flakes
  # programs.home-manager = {
  #   enable = true;
  #   path = "/home/rasen/dotfiles/channels/home-manager";
  # };

  services.syncthing.enable = true;

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "qt";
  };

  xsession.enable = true;
  xsession.initExtra = ''
    autorandr -c
    xrdb -merge ~/.Xresources
    xkbcomp ${./Xkeymap} $DISPLAY
  '';
  xsession.windowManager.command = ''
    xhost +SI:localuser:$USER
    exec emacs -f server-start
  '';

  # xsession.windowManager.awesome = {
  #   enable = true;
  #   luaModules = [
  #     pkgs.luaPackages.luafilesystem
  #     pkgs.luaPackages.cjson
  #   ];
  #   # try this?
  #   # noArgb = true;
  # };
  # ~/.screenlayout/default.sh

  programs.htop.enable = true;

  accounts.email = lib.mkForce {
    maildirBasePath = "Mail";
    accounts = {
      "alexey.shmalko@ringteam.com" = {
        primary = true;
        flavor = "gmail.com";
        address = "alexey.shmalko@ringteam.com";
        realName = "Alexey Shmalko";

        passwordCommand = "pass imap.gmail.com/alexey.shmalko@ringteam.com";

        mbsync = {
          enable = true;
          create = "both";
          patterns = [
            "[Gmail]/All Mail"
            "[Gmail]/Sent Mail"
            "[Gmail]/Spam"
          ];
        };
        notmuch.enable = true;
        msmtp.enable = true;
      };
    };
  };
  programs.mbsync.extraConfig = lib.mkForce "";
  programs.notmuch.hooks = {
    preNew = "mbsync -a";
    postNew = "notmuch tag --input ~/dotfiles/notmuch-tags";
  };
}
