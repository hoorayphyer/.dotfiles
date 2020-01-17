syntax on
set number

" below is some old set up for LaTeX
" "Required. This makes vim invoke Latex-Suite when you open a tex file.
" filetype plugin on
" 
" " IMPORTANT: win32 users will need to have 'shellslash' set so that latex
" " can be called correctly.
" set shellslash
" 
" " IMPORTANT: grep will sometimes skip displaying the file name if you
" " search in a singe file. This will confuse Latex-Suite. Set your grep
" " program to always generate a file-name.
" set grepprg=grep\ -nH\ $*
" 
" " OPTIONAL: This enables automatic indentation as you type.
" filetype indent on
" 
" " OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" " 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" " The following changes the default filetype back to 'tex':
" let g:tex_flavor='latex'
" "let g:Tex_ViewRule_pdf = 'evince_dbus.py'
" "let g:Tex_CompileRule_pdf = 'pdflatex --synctex=1 -interaction=nonstopmode $*'
" 
" " set up Left and Right in insert mode
" inoremap <C-h> <Left>
" inoremap <C-l> <Right>
" 
" " map Leader key
" let mapleader="<Space>"
" " some key customization
" nnoremap <Leader><Leader> <ESC>
" inoremap <Leader><Leader> <ESC>
" xnoremap <Leader><Leader> <ESC>
" 
" nnoremap <Leader>ww :w<CR>
