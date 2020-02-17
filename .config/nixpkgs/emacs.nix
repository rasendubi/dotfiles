{ pkgs ? import <nixpkgs> {} }:
rec {
  emacs = pkgs.emacs;
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
      company-lsp
      counsel
      counsel-projectile
      diff-hl
      diminish
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
      monokai-theme
      nix-mode
      nix-sandbox
      org-cliplink
      org-download
      org-drill
      org-pomodoro
      paren-face
      php-mode
      pip-requirements
      plantuml-mode
      prettier-js
      projectile
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
      web-mode
      wgrep
      which-key
      whitespace-cleanup-mode
      writegood-mode
      yaml-mode
      yasnippet

      # provided by pkgs.notmuch:
      # notmuch
    ]) ++
    [
      epkgs.orgPackages.org-plus-contrib
      epkgs.org-roam

      pkgs.ycmd
      pkgs.notmuch

      (pkgs.python3.withPackages (pypkgs: [
        pypkgs.autopep8
        pypkgs.black
        pypkgs.flake8
        pypkgs.mypy
        pypkgs.pylint
        pypkgs.virtualenv
      ]))
    ]
  );

  finalEmacs = (pkgs.emacsPackagesNgGen emacs).emacsWithPackages emacsPackages;
}
