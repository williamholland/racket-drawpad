# Racket Drawpad

A simple application for drawing with the mouse.

Implemented in Racket as a learning exercise.

![screenshot](https://github.com/williamholland/racket-drawpad/blob/master/screenshot.png)

## Run

Download the build to `drawpad-linux-x86_64` and make sure you can execute the
binaries

    chmod +x drawpad-linux-x86_64/bin/drawpad drawpad-linux-x86_64/lib/plt/racket3m-6.11

Then run the program with:

    ./drawpad-linux-x86_64/bin/drawpad


## Compile

To compile the code locally you will need Racket installed then do

    raco exe --vv drawpad.rkt
