# pfc

## Introduction

This is my undergraduate thesis, or "Proyecto Fin de Carrera" in Spanish, that I wrote in 2008. The full title was "Generation, analysis and optimization of migration settings for IPv4/IPv6 networks with functional programming" (original title "Generación, análisis y optimización de escenarios de migración de redes IPv4/IPv6 mediante programación funcional"). The general goal was, given a network with routers of different capabilities (some of them IPv4-only, some dual stack, some implementing different tunneling protocols).

At the time I had just discovered Haskell, a language that deeply impressed me, and was offered the opportunity of using it for my thesis. It was an experience that I truly enjoyed (in no small part thanks to a really good thesis advisor), and to this day it remains one of the programs I have written that I feel more proud of.

Anyway, after 10 years bit rot had set in, and it didn't compile nor run anymore. It also showed the mark of a student project: I hadn't been taught about version control, and Cabal was in its infancy at the time (I didn't even know it existed), so I wrote my own build scripts in my own way. I decided to renovate the project so that it follows modern practices and can be built using standard tools.

Things I have done until now:

* Moved the code to a Git repo.
* Updated the build system to use Stack.
* Updated the user interface from GTK+2 to GTK 3.
* Fixed some errors regarding some libraries that have changed over the years.

## Building and running

The program was written using a tool called HaRP (http://www.cs.chalmers.se/~d00nibro/harp), that allowed using a syntax similar to regular expressions to pattern-match on list. The tool has been long deprecated, and the available packages in Hackage don't even build, so the first step is building it yourself. I have combined the original code and written a Stack file, so it's as simple as:

    cd trhsx-20081106
    stack build
    stack install
    
Then you can build the project itself using Stack. However, the user interface is written in GTK, and you will probably need the development package, that in most Linux distros is called _gtk3-devel_. You will need to install it yourself, as Stack can't install system packages. After doing so, building and running the project is:

    stack build
    stack test      # (if you want to run the provided test bench)
    stack run
    
You have several example networks in the _examples_ directory, most of them synthetic networks, but also some real ones at the time the thesis was written.

## Future plans

* Further clean HaRP and contribute it back to Hackage.
* Rewrite all the comments and function names in English.
* Rewrite the comments to use Haddock syntax.
* Try to make the code simpler and clearer using modern Haskell features.

Things I will probably never do:

* Rewrite the full documentation in English, is too much work and I don't think anybody is interested.
