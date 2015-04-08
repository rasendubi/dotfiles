" Basic settings {{{
set nocompatible

let mapleader = " "
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
set noexpandtab

filetype plugin on
syntax on

set showmatch "brackets

" Enable mouse
set mouse=a

" Do not create backup files
set nobackup
set nowritebackup
set noswapfile

set showcmd

set scrolloff=2

set title

set hlsearch
nohlsearch
set incsearch

" List completions
set wildmode=longest:list,full

set langmap=йq,цw,уe,кr,еt,нy,гu,шi,щo,зp,х[,ъ],фa,ыs,вd,аf,пg,рh,оj,лk,дl,ж\\;,э',яz,чx,сc,мv,иb,тn,ьm,ю.,ё',ЙQ,ЦW,УE,КR,ЕT,НY,ГU,ШI,ЩO,ЗP,Х\{,Ъ\},ФA,ЫS,ВD,АF,ПG,РH,ОJ,ЛK,ДL,Ж\:,Э\",ЯZ,ЧX,СC,МV,ИB,ТN,ЬM,Б\<,Ю\>

set exrc
set secure
" }}}

"" Status line settings {{{
" Always show status line
set laststatus=2
"" }}}

"NeoBundle Scripts----------------------------- {{{
if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  if has('nvim')
	  set runtimepath+=~/.nvim/bundle/neobundle.vim/
  else
	  set runtimepath+=~/.vim/bundle/neobundle.vim/
  endif
endif

" Required:
if has('nvim')
	call neobundle#begin(expand('~/.nvim/bundle'))
else
	call neobundle#begin(expand('~/.vim/bundle'))
endif

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
" NeoBundle 'altercation/vim-colors-solarized'
if &term != 'linux'
	NeoBundle 'bling/vim-airline'
endif
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'honza/vim-snippets'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-surround'
NeoBundle 'airblade/vim-gitgutter'
NeoBundle 'pbrisbin/vim-mkdir'
NeoBundle 'tpope/vim-surround'
" NeoBundle 'scrooloose/syntastic'
NeoBundle 'tomasr/molokai'
" NeoBundle 'flazz/vim-colorschemes'

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts------------------------- }}}

" Edit .vimrc and commands {{{
" local .vimrc
if has('nvim')
	nnoremap <leader>ev :vsplit .nvimrc<CR>
else
	nnoremap <leader>ev :vsplit .vimrc<CR>
endif
nnoremap <leader>gv :vsplit $MYVIMRC<CR>

" Insert <leader> and <Esc>
cnoremap <C-l> <lt>leader>
cnoremap <C-E> <lt>Esc>
" }}}

" Navigation {{{
" Start and end of line
nnoremap H ^
nnoremap L $
vnoremap L g_
" }}}

" Window manipulation {{{
" Resize window {{{
nnoremap + <C-W>+
nnoremap _ <C-W>-
" the next one conflicts with ==
" nnoremap = <C-W>>
nnoremap - <C-W><
" }}}

" Manipulate windows {{{
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

inoremap <C-h> <C-o><C-w>h
inoremap <C-j> <C-o><C-w>j
inoremap <C-k> <C-o><C-w>k
inoremap <C-l> <C-o><C-w>l
" }}}

" Remove search highlight
nnoremap <silent> <leader>/ :nohlsearch<CR>

" Use jk to exit insert-mode
" This increases productivity
inoremap jk <Esc>

" Convert word to uppercase
inoremap <C-U> <Esc>viwUea

nnoremap <silent> <leader>, :cprevious<CR>
nnoremap <silent> <leader>. :cnext<CR>

if has('nvim')
	nnoremap <silent> <M-,> :cprevious<CR>
	nnoremap <silent> <M-.> :cnext<CR>
endif


" Hard way (restrict use of some features) {{{
nnoremap o<Esc> :echoerr "Use <lt>leader>o instead"<cr>
nnoremap O<Esc> :echoerr "Use <lt>leader>O instead"<cr>
" }}}
" }}} (mappings)

" Python file settings {{{
augroup filetype_python
	autocmd!
	if has('nvim')
		autocmd FileType python nnoremap <buffer> <F5> :vsplit | ./%<CR>
		autocmd FileType python nnoremap <buffer> <F4> :vsplit | ./% 
	else
		autocmd FileType python nnoremap <buffer> <F5> :!./%<CR>
		autocmd FileType python nnoremap <buffer> <F4> :!./% 
	endif
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

	autocmd FileType vim nnoremap <buffer> <F5> :source %<CR>

	autocmd FileType vim setlocal keywordprg=:help
augroup end
" }}}

" Haskell file settings {{{
augroup filetype_haskell
	autocmd!
	autocmd FileType haskell setlocal expandtab
	if has('nvim')
		autocmd FileType haskell nnoremap <buffer> <F5> :vsplit | term ghci %<cr>
	else
		autocmd FileType haskell nnoremap <buffer> <F5> :!ghci %<cr>
	endif
augroup end
" }}}

" Markdown file settings {{{
augroup filetype_markdown
	autocmd!
	autocmd FileType markdown setlocal wrap
	autocmd FileType markdown setlocal linebreak
	autocmd FileType markdown nnoremap <buffer> j gj
	autocmd FileType markdown nnoremap <buffer> k gk
augroup end
" }}}

let g:airline_powerline_fonts = 1

" YCM settings {{{
let g:clang_library_path = "/usr/lib64/"
let g:clang_complete_copen = 0
let g:clang_hl_errors = 1
let g:clang_snippets = 1
let g:clang_snippets_engine = "ultisnips"
let g:clang_close_preview = 1
let g:clang_complete_macros = 1

let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_use_ultisnips_completer = 1
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]

let g:ycm_global_ycm_extra_conf = "~/.vim/.ycm_extra_conf.py"
" }}}

let g:netrw_altv = 1

if &t_Co >= 256
	" set background=dark
	" colorscheme solarized

	" highlight clear SignColumn

	let g:molokai_original = 1
	let g:rehash256 = 1

	set cursorline

	colorscheme molokai
endif

let g:load_doxygen_syntax = 1

let g:ctrlp_map = '<c-u>'

" Fast saving and closing
nnoremap <leader><leader> :w<cr>
nnoremap <leader>q :q!<cr>
nnoremap <leader>w :wq<cr>

" Opening splits
nnoremap <leader>v <C-w><C-v><C-w>l
nnoremap <leader>s <C-w>s

" Insert newline and stay in normal mode
nnoremap <silent> <leader>o o<Esc>
nnoremap <silent> <leader>O O<Esc>

if has('nvim')
	nnoremap <M-h> :tabp<cr>
	nnoremap <M-l> :tabp<cr>
else
	" alt + h
	nnoremap h :tabp<cr>
	" alt + l
	nnoremap <Esc>l :tabn<cr>
endif


if executable('ag')
	set grepprg=ag\ --nogroup\ --nocolor
endif

" repeat last command but prefix with !
nnoremap !: q:kI!<esc><cr>

" inoremap <Tab> <C-R>=strpart(getline('.'),0,col('.')-1)=~"^[ \t]*$"?"\t":repeat(' ',4-virtcol('.')%4)<cr>
