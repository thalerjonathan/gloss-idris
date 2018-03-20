# gloss-idris
A port of the gloss library from Haskell to Idris, based on version 1.11.1.1. With kind permission from Ben Lippmeier.

## Differences to Haskell version
- PNG only - at the moment only loading of PNG instead of BMP is supported (C bindings for loading PNGs files existed already in https://github.com/eckart/gl-idris so I simply adopted it to my purposes)
- GLEW only - scratched GLUT support because it is completely outdated and should be replaced by GLEW. Unfortunately we lost the ability for text-rendering.
- No text-rendering supported - scratching of GLUT meant also loss of its font-rendering functionality. Need to find a modern, light-weight font-rendering OpenGL library.
- The original Haskell gloss library itself is split into gloss and gloss-rendering. I integrated both into this single library because I saw no point in splitting it for Idris. Also because of Idris' lack of a proper package management system like cabal, having too many dependencies on 3rd party libraries in a project can become quite annoying. If in the future there is a real need for separating out the gloss-rendering functionality, this can be done quite easily. Also the original Haskell gloss library has a large number of examples which are distributed in the gloss-examples library. I integrated this library into this as well for the same reasons. There exists two additional packages in Haskells gloss universe: gloss-raster which supports parallel rendering of raster images (e.g. Raytracing) and gloss-algorithms which provide data structures and algorithms for working with 2D graphics. I plan on porting them (and their respective examples which are located in the examples folder) in the future.

### Idris version: 1.2

### Idris Package Dependencies
- glfw-idris-st:  https://github.com/thalerjonathan/glfw-idris-st
- gl-idris-b:     https://github.com/thalerjonathan/gl-idris-b

### Library Dependencies
- libpng: for loading PNG files
- glfw3: for interfacing with the window manager of the respective system (due to glfw-idris-st)
- glew: for loading the OpenGL functions on the respective system (due to gl-idris-b)

### Building the library
1. Assuming you have installed Idris and it is on your path.
2. Make sure libpng, glfw3 and glew are installed on your system and can be found by pkg-config. On Linux use the package manager of your system, on Mac OS X use brew install.
3. Download / clone glfw-idris-st from https://github.com/thalerjonathan/glfw-idris-st and install it.
4. Download / clone gl-idris-b from https://github.com/thalerjonathan/gl-idris-b and install it.
5. Download / clone gloss-idris
6. Install the library by navigating to the gloss-idris folder and simply invoke 'make' in a terminal.

### Running the examples
1. Assuming you have installed Idris and it is on your path and you have successfully built and installed gloss-idris on your system.
2. Download / clone gloss-idris.
3. Navigate to src/examples/ where the examples are located.
4. Compile your example of choice by navigating to its folder and simply invoking 'make' in a terminal.
5. Run the example.