# scroll_8_ways
Scrolling in 8 directions on msx1 in screen 2.

The package includes two demos using a special video arrangement able to offer two pages of 256x128 (256 tiles each) without affecting the sprites.
In the 48Kb rom all tiles are pre loaded in the two pages in VRAM, only the PNT varies. 
In the 128Kb rom, the two pages are used to implement double buffering, so at each step the tile set is updated in the hidden page, while the other page is shown.

Graphics for the demo levels is courtesy of Santi from his XRacing 

Some technical details:
Color table is reduced to 2KB
Pattern table is 6KB but only last 4K are used
The upper 3rd of the screen can be used with split screen 
