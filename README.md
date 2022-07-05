# IronAssaultVideoPlayer
  
## What it it?  
It is a player for in-game videos of the 1996 MS-DOS game Iron Assault. It was about piloting giant battle robots in a future war against an evil corporation.
 It had FMVs (full-motion videos), they were monochrome and low-budget, shot mostly with miniatures and stop-motion, but it's obvious today. And back then it meant one thing - THESE ROBOTS WERE REAL! It seemed quite impressive. Or maybe it was because of my young age 😄

## Why make it?  
From the moment I first saw the game (around 1997-98 I think), I remembered its distinctive videos and kept thinking, 'How are they stored?'. Well, now, 27 years after the game release, I went and looked how :-)  
I saw many similar projects for more popular retro games; in fact, there is either a remake or a tool collection for almost any other old game with FMVs. But I never found one for the Iron Assault.  

## What is it made with?  
Delphi 10.x, Firemonkey framework (it's possible to modify it to be compiled with VCL, but what's the point). It can be compiled for non-Windows platforms and run there (checked with Debian Linux 11 x64).  
  
### NOTE 1  
In order to be able to compile for non-Windows target, check that the USE_WIN32_APIS define is not set, so the app uses a TMediaPlayer component for audio playback. When only Windows support is needed, it's better to enable this conditional define, because it uses more efficient Win32 API and plays audio directly from memory, without using temp files.  
  
### NOTE 2  
In order to be able to hear audio on Linux without crashing, you must have VLC installed (it is a requirement of TMediaPlayer).
  
## So, how are the videos stored?  
Nothing tricky, compared to other titles with their own video codecs, tricky compression schemes and so on. But still it took some time figuring it out.  
  
### Video info  
Raw planar video (for displaying in "mode X" 320x200), 16 colors, indexed palette, ~ 14 fps.  
Frame size: 256 x 83 (cutscenes), 96 x 96 (videos in briefings)  
Frame display size: ~ 256 x 166 (cutscenes), 96 x 96 (videos in briefings)  
  
Example for cutscene videos:  
Byte order (total 10624 bytes in 1 frame):  
```  
   [A1A2] [A3A4] ...  } - 5312 bytes, Plane A  
   [B1B2] [B3B4] ...  } - 5312 bytes, Plane B  
```   
([xxxx] = 1 byte, Ax / Bx = 1 nibble (4 bits), each nibble is a 0..15 color index in one of 16-color palettes.)  
Pixel order in usual (linear) mode:  
```  
[A2] [B2] [A1] [B1] [A4] [B4] [A3] [B3] ...  
```  
3 of the palettes ("standard"/blue, "emergency"/red and "evening"/golden) are stored in FILMS\FILM.LZ, and another one ("report"/teal), used in briefing videos, is found in multiple files, including E_MAIN\BACKTXT.LZ. Some videos can be seen in the game multiple times with different palettes.  
  
### Audio info  
Headerless unsigned 8-bit, 11025 Hz, mono, usually stored separately for each of 3 languages.  

## TODO  
- [x] upgrade code quality level from "quick-and-dirty" to "somewhat nice and structured"  
- [ ] save and load settings (game path)  
- [x] implement fmx/mediaplayer audio backend  
- [x] conditionals for Win32 API  
- [x] support more video types from the game  
- [ ] support ALL video types from the game  
- [ ] graphical audio indicators in file list  
- [ ] video playback settings  
- [ ] better A/V sync  
- [ ] support converting videos to avi/mpg/mp4 etc with ffmpeg  
- [ ] ... or via DirectShow on Win32  
- [ ] write tests  
