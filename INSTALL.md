NOTE: We have only tested our project on Macs, so this is what we recommend
running it on. The instructions for setting up on windows are a guess.

=== Set Up ===
(MacOS)
- Install homebrew (if not already installed)
  - $/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
- Install SDL2
  - $brew install SDL2
  - $brew install SDL2_image
  - $brew install SDL2_ttf
- Install the graphics package
  - $opam install ocamlsdl2
- Everything is now ready
  - To play:
    - $make play

(Windows)
- We did not have any windows machines to test this on, so the instructions
are less exact
- Install SDL2 from https://www.libsdl.org/download-2.0.php
- Install and X11 server such as Xming from https://sourceforge.net/projects/xming/
  - Set the display variable as needed
    - export DISPLAY=:0
- Install the graphics package
  - $opam install ocamlsdl2
- Everything is now ready
  - To play:
    - Run Xming
    - $make play

(Linux)
- Good luck