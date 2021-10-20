timesheets
===============

This program takes the file containing information about your timesheets and creates a file in `tex` format which can be later converted to `pdf` format.

How to use this program
=======================

1. Clone this repository:

       git clone 'https://github.com/ruzovska/timesheets.git'
1. Navigate to the cloned repository:

       cd timesheets
1. Edit the `input` file to your liking but keep the structure intact.
1. Run the program:

       cabal run timesheets -- input output.tex
1. A file named `output.tex` appeared in the directory. Convert it to `pdf` format using some conversion tool. I use `xelatex`:

       xelatex output.tex output.pdf