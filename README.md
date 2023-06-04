# John the artist 🎨

Please make sure to clone or download the zip file of the current repository.

Once downloaded, you need `ghc` compiler (Glorious Haskell Compiler)
and `Cabal` (Common Architecture for Building Applications and Libraries)
as `Maven` for `Java`. The quickest way to
get `ghc` and `Cabal` is to use [ghcup](https://www.haskell.org/ghcup/)
which is the main installer for general Haskell porpuse.
[Here](https://www.haskell.org/ghcup/install/) are the instructions to download it in any operating system.

## How to interact with you program

This project has been tested with `ghc` version `8.10.7`.

So please, please, check your `ghc` version.

```sh
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.10.7
(base)
```

If you don't have the correct version, use `ghcup` to change the default
default `ghc` compiler to `8.10.7`.

The following command opens a user interface to install and
change the default Haskell compiler.

```sh
$ ghcup tui
```

The first time you open this project you
need to compile the targets within the project.

```
$ cabal build
```

This command will create a folder `dist-newstyle` with the
resulting compilation, ignore it, but this is where the required
dependencies are installed. It also compiles the project's source code
and generates the executable or library output.

To run the `Main.hs` file run:

```sh
$ cabal run
```

To lunch `ghci` (Haskell interpreter) in the context of your Cabal project,
you should use:

```sh
$ cabal repl
```

Inside the interpreter you can load any module to interact with as:

```haskell
GHCi> :load <modul-name>
```
or

```haskell
GHCi> :m + <modul-name-1> <modul-name-2>
```

or

```haskell
GHCi> import <modul-name>
```

## Change `ghc` version

This explanation assumes that you will use `ghcup` as your main Haskell installer.

If you have access to
`ghcup tui` (which means you're not using Windows - lucky you!),
it will provide a user interface to help you install and set
the version of `ghc` and other programs related to Haskell.

If you don't have the `tui` program,
you can still install and set as default
any available `ghc` version by following these steps:

1. Install the desired `ghc` version

```bash
$ ghcup install ghc 8.10.7
```

2. Now you need to set the main `ghc` compiler.

```bash
$ ghcup set ghc 8.10.7
```

## Recomendations

- There should be non whitespace in any folder of the project path
- You may have problem building the project because of the `MAX_PATH` configuration
on your system. In windows the `MAX_PATH` is defined with 260 characters. So as the
build project generates long path for the build artifacts, put your project in a folder
with a short path.

## C libraries

The graphics of this projects run on OpenGL (Open Graphics Library) and
in GLUT (OpenGL Utility Toolkit).

OpenGL (Open Graphics Library) is a cross-platform API (Application Programming Interface) for rendering 2D and 3D computer graphics.

GLUT (OpenGL Utility Toolkit) is a library that provides a set of functions for creating graphical user interfaces (GUIs) and handling input events in OpenGL applications.

This modules may need some C libraries you may not have on your system.
Depending on your operating system,
you can usually install this C libraries using your package manager.
For example, on Ubuntu or Debian, you can install the library
using the following command:

### OpenGL

```sh
$ sudo apt-get install libgl1-mesa-dev
```

### GLUT

```sh
$ sudo apt-get install -y \
    libglu1-mesa-dev \
    freeglut3-dev
```

### GNU Multiple Precision Arithmetic Library (gmp)

```sh
$ sudo apt-get install libgmp-dev
```
