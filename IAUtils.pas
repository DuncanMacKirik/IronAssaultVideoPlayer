unit IAUtils;

interface

uses
     System.Classes, System.IOUtils, System.SysUtils,
     FMX.Graphics;

const
     MAX_FILE_SIZE = 128 * 1024 * 1024; // 128 MB

     ERR_FILE_TOO_LARGE = 'File is too large';
     ERR_PALETTE_NOT_LOADED = 'Palette was not loaded before playing videos';
     ERR_UNKNOWN_VIDEO_RES = 'Unknown video resolution';
     ERR_CANNOT_MAP_BITMAP_DATA = 'Cannot map bitmap data';

type
     TIAAudioType = (Disabled, English, French, German);

     TOnDrawFrame = procedure (bmp: TBitmap) of object;
     TOnPlaybackDone = procedure (const frames, msec: Int64) of object;

     EFileTooLarge = class(EStreamError)
          constructor Create;
     end;

     EUnknownVideoResolution = class(EStreamError)
          constructor Create;
     end;

     EPaletteNotLoaded = class(EInvalidOpException)
          constructor Create;
     end;

function CombinePath(const Path1, Path2: string): string; inline;


implementation

// TPath.Combine with cross-platform path delimiters support
function CombinePath(const Path1, Path2: string): string; inline;
begin
     Result := TPath.Combine(Path1, Path2);
{$IFNDEF USE_WIN32_API}
{$IFNDEF WINDOWS}
     Result := Result.Replace('\', '/');
{$ENDIF}
{$ENDIF}
end;

{ EFileTooLarge }

constructor EFileTooLarge.Create;
begin
     inherited Create(ERR_FILE_TOO_LARGE);
end;

{ EPaletteNotLoaded }

constructor EPaletteNotLoaded.Create;
begin
     inherited Create(ERR_PALETTE_NOT_LOADED);
end;

{ EUnknownVideoResolution }

constructor EUnknownVideoResolution.Create;
begin
     inherited Create(ERR_UNKNOWN_VIDEO_RES);
end;

end.