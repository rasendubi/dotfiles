set nocompatible

let mapleader = ","
let localleader = "\\"

set number
set nowrap
set autoindent
set smartindent
set cindent

set backspace=indent,eol,start

set tabstop=4
set softtabstop=4
set shiftwidth=4
set expandtab

call pathogen#infect()

filetype plugin on

set showmatch "brackets

set nobackup
set nowritebackup
set noswapfile

set scrolloff=2

let g:clang_library_path="/usr/lib64/llvm"

set title

" Move lines up and down
nnoremap <C-J> :m +1<CR>
nnoremap <C-K> :m -2<CR>
inoremap <C-J> <Esc>:m +1<CR>A
inoremap <C-K> <Esc>:m -2<CR>A

nnoremap - dd

" Edit .vimrc
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Surround with " or '
nnoremap <leader>" viw<Esc>a"<Esc>hbi"<Esc>lel
nnoremap <leader>' viw<Esc>a'<Esc>hbi'<Esc>lel

vnoremap <leader>" <Esc><Esc>`<i"<Esc>`>la"<Esc>
vnoremap <leader>' <Esc><Esc>`<i'<Esc>`>la'<Esc>

iabbr @n Alexey Shmalko
iabbr @@ rasen.dubi@gmail.com

" Insert <leader> and <Esc>
inoremap <C-l> <lt>leader>
inoremap <C-E> <lt>Esc>
cnoremap <C-l> <lt>leader>
cnoremap <C-E> <lt>Esc>

nnoremap H 0
nnoremap L $

inoremap jk <Esc>

" Hard way
" Force to learn jk 
inoremap <Esc> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
inoremap <Up> <nop>
inoremap <Down> <nop>
