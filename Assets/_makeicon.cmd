@echo off
set CONVERT="C:\Program Files\ImageMagick-7.0.11-Q16-HDRI\convert.exe"

%CONVERT% %1 -resize 16x16   -depth 32 16-32.png
%CONVERT% %1 -resize 32x32   -depth 32 32-32.png
%CONVERT% %1 -resize 48x48   -depth 32 48-32.png
%CONVERT% %1 -resize 256x256 -depth 32 256-32.png

%CONVERT% 16-32.png 32-32.png 48-32.png 256-32.png %~n1.ico
