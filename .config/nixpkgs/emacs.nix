{ pkgs ? import <nixpkgs> {} }:
rec {
  emacs = pkgs.emacs;
  emacsPackages = (epkgs:
    (with epkgs.melpaPackages; [
      use-package
      diminish
      el-patch

      evil
      evil-numbers
      evil-swap-keys
      evil-collection
      evil-surround
      evil-magit
      evil-org

      lispyville
      aggressive-indent
      paren-face

      smex
      ivy
      counsel
      counsel-projectile
      whitespace-cleanup-mode
      which-key
      projectile
      diff-hl
      yasnippet
      company
      flycheck
      color-identifiers-mode
      magit
      f

      imenu-list
      avy
      wgrep
      org-drill
      org-pomodoro
      org-cliplink
      org-download
      epresent
      nix-mode
      haskell-mode
      rust-mode
      racer

      elpy
      blacken
      py-autopep8
      pip-requirements

      js2-mode
      rjsx-mode
      typescript-mode
      tide
      vue-mode
      php-mode
      web-mode
      groovy-mode
      go-mode
      # lua-mode
      ledger-mode
      markdown-mode
      edit-indirect
      json-mode
      yaml-mode
      jinja2-mode
      gitconfig-mode
      terraform-mode
      clojure-mode
      cider
      graphviz-dot-mode
      fish-mode
      visual-fill-column
      beacon
      google-translate
      writegood-mode
      edit-server
      cmake-mode
      dtrt-indent
      dockerfile-mode
      plantuml-mode

      general
      flycheck-jest
      purescript-mode
      psc-ide
      restclient
      mbsync
      nix-sandbox
      prettier-js
      flycheck-rust
      flycheck-inline
      monokai-theme
      spaceline

      lsp-mode
      lsp-ui
      company-lsp

      # provided by pkgs.notmuch:
      # notmuch
    ]) ++
    [
      epkgs.orgPackages.org-plus-contrib

      pkgs.ycmd
      pkgs.notmuch

      # pkgs.python3Packages.elpy
      # pkgs.python2Packages.pip
      # pkgs.python2Packages.setuptools
      # pkgs.python3Packages.pip
      # pkgs.python3Packages.setuptools
      # pkgs.python3Packages.jedi
      # pkgs.python3Packages.rope
      (pkgs.python3.withPackages (pypkgs: [
        pypkgs.flake8
        pypkgs.pylint
        pypkgs.mypy
        pypkgs.autopep8
        pypkgs.black
      ]))
      # pkgs.python3Packages.flake8
      # pkgs.python3Packages.autopep8
      # pkgs.python3Packages.yapf
      # pkgs.python3Packages.black
    ]
  );

  finalEmacs = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages emacsPackages;
}
