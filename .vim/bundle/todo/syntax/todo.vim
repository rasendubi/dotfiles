" Vim syntax file
" Language: todo list
" Maintainer: Alexey Shmalko

if exists("b:current_syntax")
    finish
endif

let b:current_syntax = "todo"

syntax match todoComment "\v#.*$"
syntax match todoHeader "\v^.*:\s*$"
syntax match todoElementName "\v^\s*-.*$"
syntax match todoDone "\v^\s*\+.*$"
syntax match todoQuoted "\v`[^`]*`"
syntax match todoConstraints "\v^\s*\[.*\]$"

highlight todoComment ctermfg=darkgrey
highlight todoHeader ctermfg=green cterm=bold
highlight todoElementName ctermfg=cyan
highlight todoDone ctermfg=yellow
highlight todoQuoted ctermfg=white
highlight todoConstraints ctermfg=darkred
