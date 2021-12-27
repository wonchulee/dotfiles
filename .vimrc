" Specify a directory for plugins
" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

" Make sure you use single quotes

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

" Any valid git URL is allowed
Plug 'https://github.com/junegunn/vim-github-dashboard.git'

" Multiple Plug commands can be written in a single line using | separators
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" nerdtree
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'jistr/vim-nerdtree-tabs'

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'roxma/vim-hug-neovim-rpc'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'jiangmiao/auto-pairs'
Plug 'bronson/vim-trailing-whitespace'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'terryma/vim-multiple-cursors'
Plug 'asciidoc/asciidoc'
Plug 'junegunn/gv.vim'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-journal'

" colorscheme
Plug 'dracula/vim'

" Lang
Plug 'rhysd/vim-clang-format'
Plug 'sheerun/vim-polyglot'

" LSP symbols/tags viewer
" Plug 'liuchengxu/vista.vim'
" tag plugin
" Plug 'ludovicchabant/vim-gutentags'
" let g:gutentags_ctags_exclude = ['oe-workdir', 'oe-logs', 'build']

" python
" Plug 'direnv/direnv.vim'
" Plug 'nvie/vim-flake8'
" Plug 'vim-scripts/indentpython.vim'

" Plug 'mattn/webapi-vim'
" Plug 'mattn/gist-vim'

" Initialize plugin system
call plug#end()

" set termguicolors
colorscheme dracula
let g:nord_cursor_line_number_background = 1
let g:nord_uniform_diff_background = 1
let g:nord_italic = 1

set wildmenu
set runtimepath^=~/.vim/bundle/ctrlp.vim
set nu
set hlsearch
set encoding=utf-8
set nobackup
set nowritebackup
set history=50
set ruler
set showcmd
set incsearch
set clipboard=unnamedplus

" Better display for messages
set cmdheight=2

" Softtabs, 2 spaces
set tabstop=2
set shiftwidth=2
set shiftround
set expandtab

" Make it obvious where 82 characters is
set textwidth=82
set colorcolumn=+1

"git-gutter update time
set updatetime=100

let g:indent_guides_enable_on_vim_startup = 1

" NerdTree configuration
map <C-\> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

"during insert, kj escapes, `^ is so that the cursor doesn't move
inoremap kj <Esc>`^
"during insert, lkj escapes and saves
inoremap lkj <Esc>`^:w<CR>
"during insert, lkj escapes and saves and QUITS
inoremap ;lkj <Esc>:wq<CR>

"replace tab to other character
set list
set listchars=tab:!·,trail:·

"ultisnips
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
let g:UltiSnipsEditSplit="vertical"
" let g:UltiSnipsSnippetDirectories = ['~/.vim/UltiSnips']
let g:UltiSnipsSnippetDirectories = ['UltiSnips']

" run python
nnoremap <buffer> <F9> :w<CR>:!clear;python3 %:p<cr>

" fzf key settings
nnoremap <C-p> :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>t :BTags<CR>
nnoremap <Leader>T :Tags<CR>

set backupdir=/tmp//
set directory=/tmp//
set undodir=/tmp//

" airline theme
let g:airline_theme='silver'
