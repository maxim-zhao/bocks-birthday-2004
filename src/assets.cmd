for %%f in (backgrounds\*.png) do bmp2tile "%%f" -tileoffset 0 -savetiles "%%f.tiles.zx7" -savetilemap "%%f.tilemap.zx7" -savepalette "%%f.palette.bin" -exit
for %%f in (sprites\*.png) do bmp2tile "%%f" -nomirror -savetiles "%%f.tiles.zx7" -savetilemap "%%f.tilemap.inc" -exit
for %%f in (music\*.vg*) do PSGTool "%%f"
