""""""""""""""""""
"Vim configuration
""""""""""""""""""
"Author: Glynn Forrest
"me@glynnforrest.com

"""""""""""""""""
"General settings
"""""""""""""""""

"Enable highlighting
syntax on

"Enable filetypes
filetype plugin on
filetype indent on

"We're not using vi
set nocompatible

"Don't leave backups everywhere
set nobackup
set nowritebackup
set noswapfile

"Searching wraps around end of file and travels
set wrapscan
set incsearch
set nohlsearch

"Search smartly
set smartcase
set ignorecase

"Line numbers and show where we are
set number
set ruler

"Show our half typed commands
set showcmd

"Indenting properly
set autoindent
set smartindent
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab
set smarttab
set lbr
set tw=80

"Expand most folds
set foldlevel=5

"Change buffers without saving
set hidden

"Better filename completion
set wildmode=longest,list,full
set wildmenu
set completeopt=longest,menuone

"See the cursor when moving vertically
set scrolloff=4

"Don't redraw while executing macros
set nolazyredraw

"Refresh automatically
set autoread

"Show tabline only when there are more than 1
set stal=1

"Disable alt menu keys so they can be used for mappings
"Use <F10> to open the menu
set winaltkeys=no

colorscheme elflord

"""""""""""""""""""""
" Mappings
"""""""""""""""""""""

"Map the leader to an easy key
let g:mapleader = ","

"Hotkeys to edit and reload .vimrc
map <leader>v :e! ~/.vimrc<CR>
map <leader>V :source ~/.vimrc<CR>

"Quick file write
map <leader>w :w<CR>
vmap <leader>w <ESC>:w<CR>

"Quick file open
map <leader>e :e

"Quick split
map <leader>s :vspl<CR><C-w>w
map <leader>S :spl<CR><C-w>w

"Copy and paste from the system clipboard
map <leader>P "+P
map <leader>p "+p
map <leader>Y "+Y
map <leader>y "+y

"Quick format of file
map <leader>= gg=G<C-o><C-o>

"Navigate up and down a long line
nnoremap j gj
nnoremap k gk

"Scroll a bit faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

"Buffers and tabs
map <left> :bp<CR>
map <right> :bn<CR>
map <leader>x :bd<CR>
map <leader>q :q<CR>

map <leader><space> :tabf %<CR>
map <leader>z :tabclose<CR>

"Window management
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l
map <C-,> <C-W><
map <C-.> <C-W>>

"Quick change directory
map <leader>cd :cd

"Change directory to current buffer
map <leader>d :cd %:p:h<CR>

"Move up a directory
map <leader>D :cd ..<CR>

"Ultimate in lazy
map <space> :

"Clearer searching
map n nzz
map N Nzz
map * *zz
map # #zz

"Increment everything
set nrformats=alpha,octal,hex

"Toggle line relative line numbers
function! g:ToggleRelativeNumbers()
    if(&rnu == 1)
        set nu
    else
        set rnu
    endif
endfunc

map <F2> :call g:ToggleRelativeNumbers()<CR>

"""""""""""
"Statusline
"""""""""""
set laststatus=2
set statusline=\ %F%m%r\ %y\ [%{&ff}]\ %l/%L\ %{getcwd()}

"Remembers cursor position in a file
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif
