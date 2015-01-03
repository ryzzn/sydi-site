sydi-site
=========

A tool generates static website base on Emacs and org mode, like octopress, jekyll.

Homepage: http://sydi.org

Requires: Emacs 24+

Initialize
==========

First, you should clone out this repository.

  git clone https://github.com/ryzzn/sydi-site.git

Then, custom variables to fit your need, which now is located at
header lines of sydi-site.el file.

Last step, initialize website

  ./run.sh init

From now on, you can enjoy your org file editing, blablabla...

Common Usage
============

Once you think it's time to see what the site is like after previous
blablabla, i.e. editing, then you can type these two command in your
shell terminal.

  ./run.sh
  ./run.sh test

The first command without any arguments means generate org file to
HTML file, also some other miscellaneous will publish to the HTML
directory. And the second command with argument of "test" will start a
simple web server, by python module, then start your web browser that
locate to your HTML directory where you can take a real look at your
modification of the site. It's excellent simple, isn't it?
