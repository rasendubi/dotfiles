function fish_user_key_bindings
    fish_vi_key_bindings

    bind -s j up-or-search
    bind -s k down-or-search
    bind -s -M visual j up-line
    bind -s -M visual k down-line

    bind -s '.' repeat-jump
end
