#!/bin/sh
FILES=".vimrc .vim .nvimrc .nvim .gitconfig .zshrc .zsh .tmux.conf .xxkbrc .Xresources .config/awesome .config/nvim .config/xxkb .nethackrc .emacs.d .ssh bin"

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

if [ ! -f "$BASE/.ssh/id_rsa" ]; then
    read -r -p "$BASE/.ssh/id_rsa doesn't exist. Decrypt file? [y/N] " response
    case "$response" in
        [yY][eE][sS]|[yY])
            install -m 600 /dev/null "$BASE/.ssh/id_rsa"
            gpg2 --output "$BASE/.ssh/id_rsa" --yes --decrypt "$BASE/.ssh/id_rsa.gpg"
            ;;
    esac
else
    echo "$BASE/.ssh/id_rsa exists. Skipping..."
fi

read -r -p "Copy NixOS config? [y/N] " response
case "$response" in
    [yY][eE][sS]|[yY])
        sudo mkdir -v -p /etc/nixos
        sudo cp -v "$BASE/nixos"/* "/etc/nixos/"
        ;;
esac
