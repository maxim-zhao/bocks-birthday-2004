for %%f in (backgrounds\*.png) do "..\..\..\Delphi\bmp2tile\bmp2tile.exe" "%%f" -tileoffset 0 -savetiles "%%f.tiles.zx7" -savetilemap "%%f.tilemap.zx7" -savepalette "%%f.palette.bin" -exit
for %%f in (sprites\*.png) do "..\..\..\Delphi\bmp2tile\bmp2tile.exe" "%%f" -nomirror -savetiles "%%f.tiles.zx7" -savetilemap "%%f.tilemap.inc" -exit
for %%f in (music\*.vg*) do java -jar "..\..\PSGLib\PSGlib-master\tools\PSGTool.jar" "%%f"
