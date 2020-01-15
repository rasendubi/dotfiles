#!/bin/sh
FILES=".vimrc .vim .nvimrc .nvim .gitconfig .zshrc .zsh .tmux.conf .Xresources .config/awesome .config/nvim .nethackrc .emacs.d .ssh bin .config/zathura .irssi .config/xkb .config/fish .msmtprc .notmuch-config .mbsyncrc .config/nixpkgs"

DEST=$1

if [ -z "$DEST" ]; then
    DEST="$HOME"
fi

BASE=$(cd "$(dirname "$0")" && pwd)

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

if [ ! -e "$DEST/.config/fish/functions/fisher.fish" ]; then
    read -r -p "Install fisherman and all plugins? [y/N] " response
    case $response in
        [yY][eE][sS]|[yY])
            curl -Lo "$DEST/.config/fish/functions/fisher.fish" --create-dirs \
                https://raw.githubusercontent.com/fisherman/fisherman/master/fisher.fish
            fish -c fisher
            ;;
    esac
fi
