# haskell-programming-book

This are my solutions for [Haskell Programming from first principles][1]. I 
read the book without doing any of the exercises; then, starting to code some 
things on my own, I realize that I was out of my depth, that's when I decided
to take it one step at a time and went back to the book. 

I started from Chapter 14 - Testing, because I think that's when most of the
non-basic content starts.

## Dev environment

I use [Neovim][3] as my one and only text editor. If you want to see what I'm
using for Haskell you can do it [here][4]. I tried using 
[the hdevtool vim plugin][5] but I had too many problems with it, I asked in
the [Haskell for beginners channel][6] of the functional programming slack for
alternatives and someone point me to [ghcid][7], that's what I'm using.

## Running the tests

First you'll need [The Haskell Tool Stack][2] installed. To run the tests, just
execute `stack test`.

## Other solutions

I'm comparing most of my solutions against the [@andrewMacmurray's repo][8] 
they are really good and very complete.

[1]: http://haskellbook.com/index.html
[2]: https://docs.haskellstack.org/en/stable/README/
[3]: https://neovim.io/
[4]: https://github.com/alejandrodnm/dotfiles/blob/master/vimrc#L69
[5]: https://github.com/bitc/vim-hdevtools
[6]: https://functionalprogramming.slack.com/messages/C04641JCU
[7]: https://github.com/ndmitchell/ghcid
[8]: https://github.com/andrewMacmurray/haskell-book-solutions/
