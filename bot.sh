#!/bin/sh
/usr/bin/env sbcl --noinform --disable-ldb --lose-on-corruption --non-interactive --load src/setup.lisp --eval "(discord-play-register:main)"
