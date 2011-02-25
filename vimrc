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
	"set autochdir "Having this set hampers the utility of CommandT
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
" This beauty remembers where you were the last time you edited the file, and returns to the same position.
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif



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
	" Get into edit mode more easily
	nnoremap ; :

	" Train myself to use ; instead of :
	noremap : <NOP>
	
	" Perforce Settings
	" {
		",pe to open for edit
		map <leader>pe :!p4 edit "%:p"<CR><CR>
	" }

	map <leader>ve :e! ~/.vimrc<cr>
	
	" ,y to open Yankring
	map <leader>y :YRShow<CR>

	" ,t to open taglist
	nmap <leader>t :TlistToggle<CR>

	" ,d to open nerd tree
	map <leader>d <ESC>:NERDTreeToggle<CR>

	" ,f to open CommandT
	map <leader>f <ESC>:CommandT<CR>

	set pastetoggle=<F12> "Turn of indentation when pasting multiple lines

	cmap <leader>sudow w !sudo tee %<CR><CR> 
	
	" Make space / shift-space scroll in normal mode
	" {
		noremap <S-space> <C-b>
		noremap <space> <C-f>
	" }
	
	" Make Arrow Keys Useful Again 
	" {
		map <down> <ESC>:bn<RETURN>
		"map <left> <ESC>:NERDTreeToggle<RETURN>
		"map <right> <ESC>:Tlist<RETURN>
		map <up> <ESC>:bp<RETURN>
	" }
	
	" Emacs like bindings for the command line
	" {
		cnoremap <C-A>		<Home>
		cnoremap <C-E>		<End>
		cnoremap <C-K>		<C-U>
		cnoremap <C-P>		<Up>
		cnoremap <C-N>		<Down>
	" }
	
	" Smart movement between windows
	" {
		map <C-j> <C-W>j
		map <C-k> <C-W>k
		map <C-h> <C-W>h
		map <C-l> <C-W>l	
	" }
	
" }


" Backups
" {
	set backup
	silent !mkdir $HOME/.vim/backups > /dev/null 2>&1
	set backupdir=$HOME/.vim/backups " where to put backup file
	silent !mkdir $HOME/.vim/temp > /dev/null 2>&1
	set directory=$HOME/.vim/temp " directory is the directory for temp file
	silent !mkdir $HOME/.vim/undodir > /dev/null 2>&1
	"set undodir=$HOME/.vim/undodir
	"set undofile
	set makeef=error.err " When using make, where should it dump the file
" }

" """""""""""""""""""""""
" Plugin specific options
" """""""""""""""""""""""

" TagList Settings 
" {
	let Tlist_Auto_Open=0 "Let the taglist open automatically
	let Tlist_Compact_Format = 1 "Show small menu
	let Tlist_Ctags_Cmd = 'ctags' "Location of ctags
	let Tlist_Exist_OnlyWindow = 1 "If you are the last, kill yourself
	let Tlist_Sort_Type = 'name' "Order by
" }

"""""""""""""""""""""""""""
" Language specific options
"""""""""""""""""""""""""""

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
" {
	au BufEnter *.hs,*.lhs compiler ghc
	autocmd FileType haskell map <buffer> <leader><space> :w!<cr>:!ghc --make %<cr>:./%<cr>
	let g:haddock_browser="/usr/bin/links"
" }

" Ruby {
	au BufEnter *.rb,*.rhtml,*.erb set shiftwidth=2
	au BufEnter *.rb,*.rhtml,*.erb set softtabstop=2
" }

""""""""""""""""""""""""""""
" Omnicomplete functions
""""""""""""""""""""""""""""
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
