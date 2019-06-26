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

" On-demand loading
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }

Plug 'roxma/vim-hug-neovim-rpc'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'jiangmiao/auto-pairs'
Plug 'bronson/vim-trailing-whitespace'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'airblade/vim-gitgutter'
Plug 'terryma/vim-multiple-cursors'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'asciidoc/asciidoc'
Plug 'junegunn/gv.vim'
Plug 'morhetz/gruvbox'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-journal'

" Lang
Plug 'rust-lang/rust.vim'
Plug 'honza/dockerfile.vim'
Plug 'pangloss/vim-javascript'
Plug 'octol/vim-cpp-enhanced-highlight'
Plug 'derekwyatt/vim-scala'
Plug 'honza/dockerfile.vim'
Plug 'rhysd/vim-clang-format'
Plug 'elzr/vim-json'

" Auto completion
Plug 'valloric/youcompleteme'
Plug 'SirVer/ultisnips'
Plug 'xavierd/clang_complete'

" tag plugin
Plug 'jsfaint/gen_tags.vim'
let g:gen_tags#gtags_default_map = 1

Plug 'direnv/direnv.vim'
Plug 'nvie/vim-flake8'
Plug 'vim-scripts/indentpython.vim'
Plug 'metakirby5/codi.vim'

" Initialize plugin system
call plug#end()

" colorscheme rupza
colorscheme gruvbox
set background=dark

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
set clipboard=unnamed

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
map <S-t> :NERDTreeToggle<CR>
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

nmap <A-t> :TagbarToggle<CR>

"during insert, kj escapes, `^ is so that the cursor doesn't move
inoremap kj <Esc>`^
"during insert, lkj escapes and saves
inoremap lkj <Esc>`^:w<CR>
"during insert, lkj escapes and saves and QUITS
inoremap ;lkj <Esc>:wq<CR>

"replace tab to other character
set list
set listchars=tab:!·,trail:·

"Youcompleteme
let g:ycm_global_ycm_extra_conf = '~/.vim/.ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
let g:ycm_key_list_select_completion = ['', '']
let g:ycm_key_list_previous_completion = ['', '']
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_warning_symbol = '>*'
let g:ycm_server_python_interpreter = '/usr/bin/python3'

nnoremap <C-g> :YcmCompleter GoTo
" nnoremap gg :YcmCompleter GoToImprecise
nnoremap <C-d> :YcmCompleter GoToDeclaration
" nnoremap t :YcmCompleter GetType
" nnoremap p :YcmCompleter GetParent

"ultisnips
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"
let g:UltiSnipsEditSplit="vertical"
" let g:UltiSnipsSnippetDirectories = ['~/.vim/UltiSnips']
let g:UltiSnipsSnippetDirectories = ['UltiSnips']

"clang_complete
let g:clang_library_path='/usr/lib64/llvm6.0/lib/libclang.so.6.0'

" run python
nnoremap <buffer> <F9> :w<CR>:!clear;python %:p<cr>
