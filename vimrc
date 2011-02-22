" Pathogen - needed before other files
" {	
	call pathogen#runtime_append_all_bundles()
	call pathogen#helptags()
" }

" Tab/Indentation
" {
	set tabstop=4      "An indentation level every four columns"
	set softtabstop=4  "Vim treats tab as 4 spaces, but respects hard tabs"
	set expandtab      "Convert all tabs typed into spaces"
	set smarttab
	set shiftwidth=4   "Indent/outdent by four columns"
	set shiftround     "Always indent/outdent to the nearest tabstop"
	set ai             "auto-indenting for programming languages
	set formatoptions=rq "Automatically insert comment leader on return, and let gp format comments
	set ignorecase
	set infercase
	set smartcase
" }

" General
" {
	set autochdir
	set backspace=indent,eol,start
	set clipboard+=unnamed "share windows clipboard	
	set fileformats=unix,dos,mac "support all 3, in this order
	set noerrorbells
	set wildmenu " turn on command line completion wild stlye
	set wildignore=*.dll,*.o,*.obj,*.bak,*.exe,*.pyc,*.jpg,*.gif,*.png,*.hi
	set wildmode=list:longest
" }

filetype plugin indent on "load filetype plugins/indent settings
syntax on          "turn syntax highlighting on by default

" turn off compatibility with the old vi
set nocompatible

" automatically show matching brackets. works like it does in bbedit.
" {
	set showmatch
	set showcmd
" }

" do NOT put a carriage return at the end of the last line! if you are programming
" for the web the default will cause http headers to be sent. that's bad.
set binary noeol

" make that backspace key work the way it should
let mapleader = ","
colorscheme desert "TODO: wrap this shit in some os/bg/fg specific settings
set tags=tags;/


" UI Options
" {
	if has('cmdline_info')
		set ruler "turn the ruler on
		set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%)
		set showcmd "show partial commands in the status line
	endif

	if has('statusline')
		set laststatus=1 
		set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
	endif
" }

" Custom commands
" {
	set pastetoggle=<F12> "Turn of indentation when pasting multiple lines
	cmap wro w !sudo tee %<CR><CR> "Write to read only file - WARNING use with caution

	" Make Arrow Keys Useful Again 
	" {
		map <down> <ESC>:bn<RETURN>
		map <left> <ESC>:NERDTreeToggle<RETURN>
		map <right> <ESC>:Tlist<RETURN>
		map <up> <ESC>:bp<RETURN>
	" }
" }

" Backups
" {
	set backup
	silent !mkdir $HOME/.vim/backups > /dev/null 2>&1
	set backupdir=$HOME/.vim/backups " where to put backup file
	silent !mkdir $HOME/.vim/temp > /dev/null 2>&1
	set directory=$HOME/.vim/temp " directory is the directory for temp file
	set makeef=error.err " When using make, where should it dump the file
" }

" """""""""""""""""""""""""
" Language specific options
" """""""""""""""""""""""""

" C/C++
" {
	autocmd FileType c,cpp call <SID>cstuff()
	function <SID>cstuff()
		set cindent
		set formatoptions+=croql
		set formatoptions-=t
	endfunction
" }

" Python
" {
	au FileType python so ~/vim_local/syntax/python.vim
	autocmd FileType python map <buffer> <leader><space> :w!<cr>:!python %<cr>

	"Python compilation
	autocmd FileType python set makeprg=python\ -c\ \"import\ py_compile,sys;\ sys.stderr=sys.stdout;\ py_compile.compile(r'%')\"
	function! Python_Eval_VSplit() range
		let src = tempname()
		let dst = tempname()
		execute ": " . a:firstline . "," . a:lastline . "w " . src
		execute ":!python " . src . " > " . dst
		execute ":pedit! " . dst
	endfunction
	"
	"Map compilation to <F7>
	au FileType python vmap <F7> :call Python_Eval_VSplit()<cr>  
" }

" Haskell { --Dependent on haskmode (vim)
	au BufEnter *.hs,*.lhs compiler ghc
	autocmd FileType haskell map <buffer> <leader><space> :w!<cr>:!ghc --make %<cr>:./%<cr>
	let g:haddock_browser="/usr/bin/links"
" }

" Ruby {
	au BufEnter *.rb,*.rhtml set shiftwidth=2
	au BufEnter *.rb,*.rhtml set softtabstop=2
" }
