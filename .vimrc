syntax on
call plug#begin()
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'alexghergh/securemodelines'
Plug 'rootkiter/vim-hexedit'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
call plug#end()
au BufNewFile,BufRead *.s,*.S set filetype=arm64asm " set assembly files to aarch64
set number
source ~/CocConfig.vim
""" Customize colors
func! s:my_colors_setup() abort
    " this is an example
    hi Pmenu guibg=#d7e5dc gui=NONE
    hi PmenuSel guibg=#b7c7b7 gui=NONE
    hi PmenuSbar guibg=#bcbcbc
    hi PmenuThumb guibg=#585858
endfunc

augroup colorscheme_coc_setup | au!
    au ColorScheme * call s:my_colors_setup()
augroup END
set modeline
set modelines=5

if &filetype ==# 'lisp'
	inoremap (<Enter> (<CR>)<C-c>O
endif
" Auto expanding

set mouse=a
