" Basic settings {{{
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

filetype plugin on

set showmatch "brackets

" Enable mouse
set mouse=a

" Do not create backup files
set nobackup
set nowritebackup
set noswapfile

set showcmd

set scrolloff=2

let g:clang_library_path="/usr/lib64/llvm"

set title

set langmap=йq,цw,уe,кr,еt,нy,гu,шi,щo,зp,х[,ъ],фa,ыs,вd,аf,пg,рh,оj,лk,дl,ж\\;,э',яz,чx,сc,мv,иb,тn,ьm,ю.,ё',ЙQ,ЦW,УE,КR,ЕT,НY,ГU,ШI,ЩO,ЗP,Х\{,Ъ\},ФA,ЫS,ВD,АF,ПG,РH,ОJ,ЛK,ДL,Ж\:,Э\",ЯZ,ЧX,СC,МV,ИB,ТN,ЬM,Б\<,Ю\>
" }}}
" Status line settings {{{
" Always show status line
set laststatus=2
set statusline=%-20.20f\ %y " Display filename and filetype
set statusline+=[b:%n]      " Buffer number
set statusline+=%m          " Modified flag
set statusline+=%=          " Switch to right side
set statusline+=%l:%-3c     " Current position in file
set statusline+=of\ %L      " Total number of lines
set statusline+=\ %3p%%     " Percent in file
" }}}

call pathogen#infect()

iabbr @n Alexey Shmalko
iabbr @@ rasen.dubi@gmail.com

" Mappings {{{
" Move lines up and down {{{
nnoremap <C-J> :m +1<CR>
nnoremap <C-K> :m -2<CR>
inoremap <C-J> <Esc>:m +1<CR>A
inoremap <C-K> <Esc>:m -2<CR>A
" }}}

nnoremap - dd

" Edit .vimrc and commands {{{
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Insert <leader> and <Esc>
cnoremap <C-l> <lt>leader>
cnoremap <C-E> <lt>Esc>
" }}}

" Navigation {{{
" Start and end of line
nnoremap H 0
nnoremap L $
" }}}

" Resize window {{{
nnoremap + <C-W>+
nnoremap _ <C-W>-
" }}}


" Remove search highlight
nnoremap <silent> <leader>/ :nohlsearch<CR>

" Surround with ", ' or <> {{{
nnoremap <leader>" viw<Esc>a"<Esc>hbi"<Esc>lel
nnoremap <leader>' viw<Esc>a'<Esc>hbi'<Esc>lel

vnoremap <leader>" <Esc><Esc>`<i"<Esc>`>la"<Esc>
vnoremap <leader>' <Esc><Esc>`<i'<Esc>`>la'<Esc>
vnoremap <leader>< <Esc><Esc>`<i<<Esc>`>la><Esc>
vnoremap <leader>> <Esc><Esc>`<i<<Esc>`>la><Esc>
" }}}
" Selection {{{
" Select inside paranthesis
onoremap p i(
" select text inside next paranthesis
onoremap in( :<C-U>normal! f(vi(
" Select inside "", '', <> {{{
onoremap " :<C-U>normal! vi"<CR>
onoremap ' :<C-U>normal! vi'<CR>
onoremap < :<C-U>normal! vi<<CR>
onoremap > :<C-U>normal! vi><CR>
" }}}
" }}}

" Use jk to exit insert-mode
" This increases productivity
inoremap jk <Esc>

" Convert word to uppercase
inoremap <C-U> <Esc>viwUea

" Hard way (restrict use of some features) {{{
" Force to learn jk 
inoremap <Esc>   <C-o>:echoerr "Don't use it!"<CR>
inoremap <Left>  <C-o>:echoerr "Don't use it!"<CR>
inoremap <Right> <C-o>:echoerr "Don't use it!"<CR>
inoremap <Up>    <C-o>:echoerr "Don't use it!"<CR>
inoremap <Down>  <C-o>:echoerr "Don't use it!"<CR>
" }}}
" }}} (mappings)

" Perl file settings {{{
augroup filetype_perl
    autocmd!
    autocmd FileType perl nnoremap <buffer> <F5> :!./%<CR>
    autocmd FileType perl nnoremap <buffer> <F4> :!./% 
augroup end
" }}}

" Vimscript file settings {{{
augroup filetype_vim
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker

    " Insert <leader> and <Esc>
    autocmd FileType vim inoremap <buffer> <C-l> <lt>leader>
    autocmd FileType vim inoremap <buffer> <C-E> <lt>Esc>
augroup end
" }}}

