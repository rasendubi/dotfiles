" Basic settings {{{
set nocompatible

let mapleader = ","
let localleader = "\\"

set number
set relativenumber
set nowrap
set autoindent
set smartindent
set cindent

set backspace=indent,eol,start

set tabstop=4
set softtabstop=4
set shiftwidth=4
set noexpandtab

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

set hlsearch
set incsearch

" List completions
set wildmode=longest:list,full

set langmap=йq,цw,уe,кr,еt,нy,гu,шi,щo,зp,х[,ъ],фa,ыs,вd,аf,пg,рh,оj,лk,дl,ж\\;,э',яz,чx,сc,мv,иb,тn,ьm,ю.,ё',ЙQ,ЦW,УE,КR,ЕT,НY,ГU,ШI,ЩO,ЗP,Х\{,Ъ\},ФA,ЫS,ВD,АF,ПG,РH,ОJ,ЛK,ДL,Ж\:,Э\",ЯZ,ЧX,СC,МV,ИB,ТN,ЬM,Б\<,Ю\>
" }}}
"" Status line settings {{{
" Always show status line
set laststatus=2
"set statusline=%-50.50f\ %y " Display filename and filetype
"set statusline+=[b:%n]      " Buffer number
"set statusline+=%m          " Modified flag
"set statusline+=%=          " Switch to right side
"set statusline+=%l:%-3c     " Current position in file
"set statusline+=\ of\ %L    " Total number of lines
"set statusline+=\ %3p%%     " Percent in file
"" }}}

"NeoBundle Scripts-----------------------------
if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  set runtimepath+=/home/rasen/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('/home/rasen/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Add or remove your Bundles here:
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }

NeoBundle 'Valloric/YouCompleteMe', {
\    'build' : {
\        'linux' : 'git submodule update --init --recursive && ./install.sh --clang-completer --system-libclang',
\    },
\ }
NeoBundle 'altercation/vim-colors-solarized'
if &term != 'linux'
	NeoBundle 'bling/vim-airline'
endif
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'honza/vim-snippets'
NeoBundle 'tpope/vim-fugitive'
" NeoBundle 'Shougo/neosnippet.vim'
" NeoBundle 'Shougo/neosnippet-snippets'
" NeoBundle 'flazz/vim-colorschemes'
" 
" " You can specify revision/branch/tag.
" NeoBundle 'Shougo/vimshell', { 'rev' : '3787e5' }

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------

iabbr @n Alexey Shmalko
iabbr @@ rasen.dubi@gmail.com

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
vnoremap L g_
" }}}

" Window manipulation {{{
" Resize window {{{
nnoremap + <C-W>+
nnoremap _ <C-W>-
nnoremap = <C-W>>
nnoremap - <C-W><
" }}}

" Manipulate windows {{{
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap <C-w><C-v> <C-w><C-v><C-w>l
nnoremap <C-w>v <C-w><C-v><C-w>l
" }}}

" Show trailing spaces
nnoremap <leader>t :execute "highlight trailing ctermbg=red \| match trailing " . '/\v[ \t]+$/'<CR>
" Hide trailing spaces
nnoremap <leader>T :highlight clear trailing<CR>

" Remove search highlight
nnoremap <silent> <leader>/ :nohlsearch<CR>
" It's strange, but following mapping maps to <C-/> as well as <C-->
nnoremap <silent> <c-_> :nohlsearch<CR>

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
" Select one parameter
onoremap op :<C-U>normal! T,vt,<CR>
" select text inside next paranthesis
onoremap in( :<C-U>normal! f(vi(
" Select inside "
onoremap " :<C-U>normal! vi"<CR>
" Select inside '
onoremap ' :<C-U>normal! vi'<CR>
" }}}

" Use jk to exit insert-mode
" This increases productivity
inoremap jk <Esc>

" Convert word to uppercase
inoremap <C-U> <Esc>viwUea

nnoremap <silent> <leader>, :cprevious<CR>
nnoremap <silent> <leader>. :cnext<CR>

" Hard way (restrict use of some features) {{{
nnoremap <C-w>h :echoerr "Don't use it!"<CR>
nnoremap <C-w>j :echoerr "Don't use it!"<CR>
nnoremap <C-w>k :echoerr "Don't use it!"<CR>
nnoremap <C-w>l :echoerr "Don't use it!"<CR>
" }}}
" }}} (mappings)

" C file settings {{{
augroup filetype_c
	autocmd!
	autocmd FileType c,c.* nnoremap <buffer> <F9> :!%:p:r<CR>
	autocmd FileType c,c.* nnoremap <buffer> <F7> :!gcc -std=c99 % -o %:p:r<CR>
" }}}

" Pascal file settings {{{
augroup filetype_pascal
	autocmd!
	autocmd FileType pascal nnoremap <buffer> <F5> :!%:p:r<CR>
	autocmd FileType pascal nnoremap <buffer> <F7> :!fpc %<CR>
" }}}

" Perl file settings {{{
augroup filetype_perl
	autocmd!
	autocmd FileType perl nnoremap <buffer> <F5> :!./%<CR>
	autocmd FileType perl nnoremap <buffer> <F4> :!./% 
augroup end
" }}}

" Python file settings {{{
augroup filetype_python
	autocmd!
	autocmd FileType python nnoremap <buffer> <F5> :!./%<CR>
	autocmd FileType python nnoremap <buffer> <F4> :!./% 
augroup end
" }}}

" Vimscript file settings {{{
augroup filetype_vim
	autocmd!
	autocmd FileType vim setlocal foldmethod=marker

	autocmd FileType vim setlocal nolinebreak

	" Insert <leader> and <Esc>
	autocmd FileType vim inoremap <buffer> <C-l>      <lt>leader>
	autocmd FileType vim inoremap <buffer> <C-V><Esc> <lt>Esc>
	autocmd FileType vim inoremap <buffer> <C-V><CR>  <lt>CR>
augroup end
" }}}

let g:airline_powerline_fonts = 1

let g:clang_library_path = "/usr/lib64/"
let g:clang_complete_copen = 0
let g:clang_hl_errors = 1
let g:clang_snippets = 1
let g:clang_snippets_engine = "ultisnips"
let g:clang_close_preview = 1
let g:clang_complete_macros = 1

let g:netrw_altv = 1
command! G Git

set exrc
set secure

if &t_Co >= 256
	set background=dark
	colorscheme solarized
endif

" let g:UltiSnipsExpandTrigger="<c-j>"

let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_use_ultisnips_completer = 1
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]

let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"

let g:load_doxygen_syntax = 1

let g:hardtime_default_on = 1
let g:hardtime_timeout = 500
let g:hardtime_showmsg = 0

let g:ctrlp_map = '<c-u>'

nnoremap <C-W>F :vertical wincmd f<cr>

let g:hardtime_default_on = 1
let g:hardtime_timeout = 500
let g:hardtime_allow_different_key = 1
let g:hardtime_maxcount = 2

set grepprg=grep\ -nH\ $*
let g:tex_flavor='latex'
