# clojure-life

## What is it

This project consists of a series of implementations of Conway's Life 
in Clojure.  These implementations were done both as kata for 
learning and practice and as fodder for a set of talks that I will be 
doing around these implementations.

## Versions

Each version is in a separate file to facilitate easy comparison.  It's 
hard to say which one is the best... they explore different styles of 
data representation, concurrency, etc.

This ordering is how I wrote them and can be used to illustrate some 
evolution:

* life1 - my first attempt
* life_seq - a rewrite making better use of seqs and functional data
* life_prot - introducing a protocol around the core data
* life_sparse - swap data impl under protocol to only hold live cells 

## Build and run

The project can be compiled with leiningen.  I usually just use the code
interactively via a repl.  Each file so far has a main entry point with a 
function called "life" that takes an initial world (that can be created 
by "init-world") and the number of iterations to compute.

There is also a test-glider function that runs "life" on a 10x10 world 
with an initial glider.

## License

* Copyright (C) 2011 Alex Miller
* Distributed under the Apache 2 License

