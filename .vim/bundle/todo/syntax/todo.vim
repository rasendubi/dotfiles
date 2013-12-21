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
syntax match todoPartial "\v^\s*\~.*$"
syntax match todoQuoted "\v`[^`]*`"
syntax match todoConstraints "\v^\s*\[.*\]$"

highlight todoComment ctermfg=darkgrey
highlight todoHeader ctermfg=cyan cterm=bold
highlight todoElementName ctermfg=lightred
highlight todoDone ctermfg=green
highlight todoPartial ctermfg=yellow
highlight todoQuoted ctermfg=white
highlight todoConstraints ctermfg=darkred
