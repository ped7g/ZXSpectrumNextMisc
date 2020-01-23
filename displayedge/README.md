# "runtime" library to read+parse the "sys/displayedge.cfg"

You can include the file "displayedge_rt.i.asm" in your own project and call the provided
API calls to read the user's configuration.

The details about API/usage will be documented later, at this moment this is work-in-progress,
still prototyping the whole functionality (although the read+parse functionality is already
implemented and if you are not afraid to check the source of the dot command tool, it should
be quite straightforward how to use the "runtime" code in your own app).

The cfg file is ordinary ASCII text file, see included test.cfg for details, so you can
also edit the values in any text editor.

# ZXNext dot command DISPLAYEDGE to edit "sys/displayedge.cfg" visually

This is "dot command" for NextZXOS running at TBBlue (aka ZXNext) board (does use tiles
video-mode for graphics).

It lets you edit visually display margins - the visible area of your display in particular
video mode - and store that in the config file. Any SW aware of this config file can then
read it upon start, and shrink the playfield to display everything important within the
area of screen which is visible on your display.

Use the controls to switch to the video mode you want to add margin to (some video modes
have to be pre-selected by starting the machine in particular mode, it is possible to edit
margins of actual video mode), and then edit the values until the "green frame" is visible
well on your display, then save the config.

The controls are:
OP      : to select the edge of display for editing
HJKL    : subtract/add to the margin of selected edge
Q       : exit back to NextZXOS
R       : reload the cfg file currently stored on disk (discards any changes)
S       : save the currently modified values to cfg file
F       : change between 50Hz/60Hz mode (also regular "F3" should work)
T       : in VGA modes you can switch between different video-mode timings
