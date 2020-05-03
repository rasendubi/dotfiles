{ pkgs ? import <nixpkgs> {} }:
rec {
  emacs = pkgs.emacs.override {
    # Build emacs with proper imagemagick support.
    # See https://github.com/NixOS/nixpkgs/issues/70631#issuecomment-570085306
    imagemagick = pkgs.imagemagickBig;
  };
  emacsPackages = (epkgs:
    (with epkgs.melpaPackages; [
      aggressive-indent
      avy
      beacon
      blacken
      cider
      clojure-mode
      cmake-mode
      color-identifiers-mode
      company
      company-box
      company-lsp
      counsel
      counsel-projectile
      diff-hl
      diminish
      direnv
      dockerfile-mode
      dtrt-indent
      edit-indirect
      edit-server
      el-patch
      elpy
      epresent
      evil
      evil-collection
      evil-magit
      evil-numbers
      evil-org
      evil-surround
      evil-swap-keys
      fish-mode
      flycheck
      flycheck-inline
      flycheck-jest
      flycheck-rust
      forth-mode
      general
      gitconfig-mode
      go-mode
      google-translate
      graphviz-dot-mode
      groovy-mode
      haskell-mode
      imenu-list
      ivy
      jinja2-mode
      js2-mode
      json-mode
      ledger-mode
      lispyville
      lsp-mode
      lsp-ui
      lua-mode
      magit
      markdown-mode
      mbsync
      modus-operandi-theme
      monokai-theme
      nix-mode
      nix-sandbox
      org-cliplink
      org-download
      org-drill
      org-super-agenda
      paren-face
      php-mode
      pip-requirements
      plantuml-mode
      prettier-js
      projectile
      protobuf-mode
      psc-ide
      purescript-mode
      py-autopep8
      racer
      restclient
      rjsx-mode
      rust-mode
      smex
      spaceline
      terraform-mode
      tide
      typescript-mode
      use-package
      visual-fill-column
      vue-mode
      w3m
      web-mode
      wgrep
      which-key
      whitespace-cleanup-mode
      writegood-mode
      yaml-mode
      yasnippet

      # from my melpa update
      org-roam
      company-org-roam
      # org-roam-bibtex
      ivy-bibtex
      org-ref

      # provided by pkgs.notmuch:
      # notmuch
    ]) ++
    [
      epkgs.orgPackages.org-plus-contrib
      epkgs.elpaPackages.adaptive-wrap

      pkgs.ycmd
      pkgs.notmuch
      pkgs.w3m
      pkgs.imagemagick
      pkgs.shellcheck

      (pkgs.python3.withPackages (pypkgs: [
        pypkgs.autopep8
        pypkgs.black
        pypkgs.flake8
        pypkgs.mypy
        pypkgs.pylint
        pypkgs.virtualenv
      ]))

      # latex for displaying fragments in org-mode
      (pkgs.texlive.combine {
        inherit (pkgs.texlive) scheme-small dvipng dvisvgm mhchem ;
      })
    ]
  );

  finalEmacs = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages emacsPackages;
}
