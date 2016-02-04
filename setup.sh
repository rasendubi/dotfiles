#!/bin/sh
FILES=".vimrc .vim .nvimrc .nvim .gitconfig .zshrc .zsh .tmux.conf .xxkbrc .Xresources .config/awesome .config/nvim .config/xxkb .nethackrc .emacs.d"

DEST=$1

if [ -z "$DEST" ]; then
    DEST="$HOME"
fi

BASE=$(dirname $(readlink -f $0))

ask_install() {
    FILENAME=$1

    LINK="$DEST/$FILENAME"
    TARGET="$BASE/$FILENAME"

    if [ -e $LINK ]; then
        echo "$LINK exists. Skipping..."
    else
        read -r -p "Link $LINK to $TARGET? [y/N] " response
        case $response in
            [yY][eE][sS]|[yY])
                ln -v -s "$TARGET" "$LINK"
                ;;
        esac
    fi
}

for FILE in $FILES; do
    ask_install $FILE
done

read -r -p "Copy NixOS config? [y/N] " response
case "$response" in
    [yY][eE][sS]|[yY])
        sudo mkdir -v -p /etc/nixos
        sudo cp -v "$BASE/nixos"/* "/etc/nixos/"
        ;;
esac
