unit IAMovieDecoder;

interface

{.$DEFINE USE_WIN32_API}

uses
     System.Classes, System.Types, System.UITypes,
     FMX.Graphics,
{$IFNDEF USE_WIN32_API}
     FMX.Media, FMX.Objects,
{$ENDIF}
     IAPalettes, IAUtils;

const
     MAX_FILE_SIZE = 128 * 1024 * 1024; // 128 MB

     IMG_OFS = 0;

     FPS_DELAY = 70;
     AUDIO_DELAY = 750;
     AUDIO_SAMPLE_RATE = 11025; // Hz
     AUDIO_SKIP_COEF = 0.00128;

     FMT_MOVIE_INFO = '%d frames, %f seconds'#13'%f frames per second.';

     OPT_DYNAMIC_DELAYS = True;
     OPT_USE_AUDIO_SKIP = True;
     OPT_DYNAMIC_SKIPS = True;

type
     IIAMovieDecoder = interface
     ['{12B8CC6F-7706-4111-B128-17919FC93AC6}']
          procedure SetGamePath(const GPath: string);
          procedure LoadMovie(const VideoFN, AudioFN: string;
               const Audio: TIAAudioType;
               const Palette: TIAPaletteType
          );
          procedure Play;
          procedure Stop;
          procedure ForceStop;
     end;

     TIAMovieDecoder = class (TInterfacedObject, IIAMovieDecoder)
     protected
          GamePath: string;
          AudioType: TIAAudioType;
          imgWidth: Integer;
          imgHeight: Integer;
          DoubleHeight: Boolean;
          Data: array of Byte;
          DataOfs: Integer;
          PaletteHandler: IIAPaletteHandler;
          PaletteLoaded: Boolean;
          src, tgt: TRectF;
          Snd: TMemoryStream;
          SkipBuf: array of Byte;
          SkipCount: Integer;
          DynDelay: Integer;
          videoThread: TThread;
          ColorValueFunc: TColorValueFunc;
{$IFNDEF USE_WIN32_API}
          AudioFN: string;
          MPlayer: TMediaPlayer;
{$ENDIF}
          FOnDrawFrame: TOnDrawFrame;
          FOnPlaybackDone: TOnPlaybackDone;

{$IFNDEF USE_WIN32_API}
          procedure InitMediaPlayer;
{$ENDIF}
          function HasAudio: Boolean;

          procedure InitColorFunc(const Palette: TIAPaletteType);
          procedure InitSkipBuffer(Duration: Integer = 0);

          procedure InitFrame;
          function GetNextFrame: TBitmap;
          procedure DrawFrame(bmp: TBitmap);

          procedure LoadPalettes;
          procedure LoadVideo(const VideoFN: string);

          procedure PrepareVideo;
          procedure PrepareAudio(const AudioFName: string);

          procedure StartVideo;
          procedure StartAudio;
          procedure StopAudio;

          procedure DoPlay;
     public
          class var Stopping: Boolean;
          class var Playing: Boolean;

          property OnDrawFrame: TOnDrawFrame read FOnDrawFrame write FOnDrawFrame;
          property OnPlaybackDone: TOnPlaybackDone read FOnPlaybackDone write FOnPlaybackDone;

          constructor Create(OnDrawFrame: TOnDrawFrame; OnPlaybackDone: TOnPlaybackDone);
          destructor Destroy; override;

          procedure SetEventHandlers(OnDrawFrame: TOnDrawFrame; OnPlaybackDone: TOnPlaybackDone);
          procedure SetGamePath(const GPath: string);

          procedure LoadMovie(const VideoFN, AudioFN: string;
               const Audio: TIAAudioType;
               const Palette: TIAPaletteType
          );
          procedure Play;
          procedure Stop;
          procedure ForceStop;
     end;

{$IFDEF USE_WIN32_API}
     TBitmap = FMX.Graphics.TBitmap; // fix for Win32 TBitmap conflict
{$ENDIF}


implementation

uses
     System.Threading, System.Diagnostics, System.IOUtils, System.SysUtils,
     System.TimeSpan
{$IFDEF USE_WIN32_API}
     , Winapi.Windows, Winapi.MMSystem
{$ENDIF};

{ TIAMovieDecoder }

constructor TIAMovieDecoder.Create(OnDrawFrame: TOnDrawFrame; OnPlaybackDone: TOnPlaybackDone);
begin
     Playing := False;
     Stopping := False;
     PaletteHandler := TIAPaletteHandler.Create;
     PaletteLoaded := False;
     SetEventHandlers(OnDrawFrame, OnPlaybackDone);
     if OPT_USE_AUDIO_SKIP and not OPT_DYNAMIC_SKIPS then
          InitSkipBuffer;
{$IFNDEF USE_WIN32_API}
     InitMediaPlayer;
{$ENDIF}
end;

procedure TIAMovieDecoder.DrawFrame(bmp: TBitmap);
begin
     if (bmp <> nil) and Assigned(FOnDrawFrame) then
          TThread.Queue(nil,
               procedure
               begin
                    try
                         FOnDrawFrame(bmp);
                    finally
                         FreeAndNil(bmp);
                    end;
               end
          );
end;

procedure TIAMovieDecoder.ForceStop;
begin
     if Playing and HasAudio then
          StopAudio;
     if not Stopping then
     begin
          Stopping := True;
          Sleep(200);
     end;
end;

destructor TIAMovieDecoder.Destroy;
begin
     if Assigned(Snd) then
          FreeAndNil(Snd);
{$IFNDEF USE_WIN32_API}
     FreeAndNil(MPlayer);
{$ENDIF}
end;

procedure TIAMovieDecoder.DoPlay;
var
     bmp: TBitmap;
     frm: Integer;
     Stopwatch: TStopwatch;
     FrameStart, Delay: Int64;
begin
     Stopwatch := TStopwatch.StartNew;
     frm := 0;
     repeat
          FrameStart := Stopwatch.ElapsedMilliseconds;
          if Stopping then
          begin
               Stopping := False;
               Break;
          end;
          bmp := GetNextFrame;
          DrawFrame(bmp);
          Inc(frm);
          Delay := Stopwatch.ElapsedMilliseconds - FrameStart;
          if not (HasAudio and OPT_DYNAMIC_DELAYS) then
               DynDelay := FPS_DELAY;
          if Delay < DynDelay then
               Sleep(DynDelay - Delay);
     until bmp = nil;
     Stopwatch.Stop;
     TThread.Queue(nil,
          procedure
          begin
               Playing := False;
{$IFNDEF USE_WIN32_API}
               if HasAudio then
                    StopAudio;
{$ENDIF}
               if Assigned(OnPlaybackDone) then
                    OnPlaybackDone(frm, Stopwatch.ElapsedMilliseconds);
          end
     );
end;

function TIAMovieDecoder.GetNextFrame: TBitmap;
var
     lWidth, x, y, xr, i: Integer;
     bmp, bmp2: TBitmap;
     bd: TBitmapData;
     CN, c1, c2, PN: Byte;
begin
     lWidth := imgWidth shr 2;
     i := DataOfs;
     if i >= Length(Data) then
          Exit(nil);
     bmp := TBitmap.Create;
     try
          bmp.Width := imgWidth;
          bmp.Height := imgHeight;
          if not bmp.Map(TMapAccess.Write, bd) then
               raise EInvalidOpException.Create(ERR_CANNOT_MAP_BITMAP_DATA);
          try
               for PN := 0 to 1 do
               begin
                    for y := 0 to imgHeight - 1 do
                    begin
                         for x := 0 to lWidth - 1 do
                         begin
                              if i >= Length(Data) then
                                   Break;
                              CN := Data[i];
                              c1 := CN shr 4;
                              c2 := CN and 15;
                              xr := (x shl 2) + PN;
                              bd.SetPixel(xr, y, ColorValueFunc(c2));
                              Inc(xr, 2);
                              bd.SetPixel(xr, y, ColorValueFunc(c1));
                              Inc(i);
                         end;
                    end;
               end;
          finally
               bmp.Unmap(bd);
               DataOfs := i;
          end;
     bmp.SaveToFile('11111.bmp');
     finally
          bmp2 := TBitmap.Create;
          bmp2.Width := imgWidth;
          if DoubleHeight then
               bmp2.Height := imgHeight * 2
          else
               bmp2.Height := imgHeight;
          with bmp2.Canvas do
          begin
               BeginScene;
               DrawBitmap(bmp, src, tgt, 1);
               EndScene;
          end;
          FreeAndNil(bmp);
     end;
     Result := bmp2;
end;

function TIAMovieDecoder.HasAudio: Boolean;
begin
     Result := AudioType <> Disabled;
end;

procedure TIAMovieDecoder.InitColorFunc(const Palette: TIAPaletteType);
begin
     ColorValueFunc := PaletteHandler.GetColorValueFunc(Palette);
end;

procedure TIAMovieDecoder.InitFrame;
begin
     src := RectF(0, 0, imgWidth, imgHeight);
     if DoubleHeight then
          tgt := RectF(0, 0, imgWidth, imgHeight * 2)
     else
          tgt := RectF(0, 0, imgWidth, imgHeight);
end;

{$IFNDEF USE_WIN32_API}
procedure TIAMovieDecoder.InitMediaPlayer;
begin
     MPlayer := TMediaPlayer.Create(nil);
end;
{$ENDIF}

procedure TIAMovieDecoder.InitSkipBuffer(Duration: Integer);
begin
     if Duration = 0 then
          Duration := AUDIO_DELAY;
     SkipCount := (Duration * AUDIO_SAMPLE_RATE) div 1000;
     SetLength(SkipBuf, SkipCount);
     FillChar(SkipBuf[0], SkipCount, 127);
end;

procedure TIAMovieDecoder.LoadMovie(const VideoFN, AudioFN: string;
     const Audio: TIAAudioType;
     const Palette: TIAPaletteType
);
begin
     if not PaletteLoaded then
          raise EPaletteNotLoaded.Create;
     AudioType := Audio;
     LoadVideo(VideoFN);
     InitColorFunc(Palette);
     InitFrame;
     PrepareVideo;
     if HasAudio then
          PrepareAudio(AudioFN);
end;

procedure TIAMovieDecoder.LoadPalettes;
begin
     PaletteHandler.LoadPalettes(GamePath);
     PaletteLoaded := True;
end;

procedure TIAMovieDecoder.LoadVideo(const VideoFN: string);
var
     VFS: TFileStream;
     VFSz: Int64;
begin
     VFS := TFileStream.Create(VideoFN, fmOpenRead);
     try
          VFSz := VFS.Size;
          if (VFSz mod 10624) = 0 then // cinematics, 256x83
          begin
               imgWidth := 256;
               imgHeight := 83;
               DoubleHeight := True;
          end
          else
               if (VFSz mod 4608) = 0 then // briefings, 96x96
               begin
                    imgWidth := 96;
                    imgHeight := 96;
                    DoubleHeight := False;
               end
               else
                    raise(EUnknownVideoResolution.Create);
          if VFSz >= MAX_FILE_SIZE then
               raise EFileTooLarge.Create;
          SetLength(Data, VFSz);
          VFS.Read(Data[0], VFSz);
     finally
          DataOfs := 0;
          FreeAndNil(VFS);
     end;
end;

procedure TIAMovieDecoder.Play;
begin
     Stopping := False;
     StartVideo;
     if HasAudio then
          StartAudio;
end;

procedure TIAMovieDecoder.PrepareAudio(const AudioFName: string);
const
     Mono: Word = $0001;
     SampleRate: Integer = AUDIO_SAMPLE_RATE; // 8000, 11025, 22050, or 44100
     RiffId: AnsiString = 'RIFF';
     WaveId: AnsiString = 'WAVE';
     FmtId: AnsiString = 'fmt ';
     DataId: AnsiString = 'data';
{$IFNDEF USE_WIN32_API}
     TEMP_FILE_NAME = 'IronAssaultVideoPlayer.tmp';

{from Winapi.MMSystem.pas}
     WAVE_FORMAT_PCM = 1;

type
     PWaveFormatEx = ^TWaveFormatEx;
     tWAVEFORMATEX = record
          wFormatTag: Word;         { format type }
          nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
          nSamplesPerSec: DWORD;  { sample rate }
          nAvgBytesPerSec: DWORD; { for buffer estimation }
          nBlockAlign: Word;      { block size of data }
          wBitsPerSample: Word;   { number of bits per sample of mono data }
          cbSize: Word;           { the count in bytes of the size of }
     end;
{$ENDIF}
var
     WaveFormatEx: TWaveFormatEx;
     MS: TMemoryStream;
     FS: TFileStream;
     TempInt, DataCount, RiffCount: integer;
begin
     with WaveFormatEx do
     begin
          wFormatTag := WAVE_FORMAT_PCM;
          nChannels := Mono;
          nSamplesPerSec := SampleRate;
          wBitsPerSample := $0008;
          nBlockAlign := (nChannels * wBitsPerSample) div 8;
          nAvgBytesPerSec := nSamplesPerSec * nBlockAlign;
          cbSize := 0;
     end;

     FS := TFileStream.Create(AudioFName, fmOpenRead);
     try
          DataCount := FS.Size; // sound data

          if OPT_USE_AUDIO_SKIP then
          begin
               if OPT_DYNAMIC_SKIPS then
                    InitSkipBuffer(Trunc(Single(DataCount) * AUDIO_SKIP_COEF));
               Inc(DataCount, SkipCount);
          end;

          if OPT_DYNAMIC_DELAYS then
               DynDelay := Trunc((Single(DataCount)*1000 / AUDIO_SAMPLE_RATE) / (Length(Data) / 10624));

          if DataCount >= MAX_FILE_SIZE then
               raise EFileTooLarge.Create;

          RiffCount := Length(WaveId) + Length(FmtId) + SizeOf(DWORD) +
          SizeOf(TWaveFormatEx) + Length(DataId) + SizeOf(DWORD) + DataCount; // file data

          MS := TMemoryStream.Create;
          with MS do
          begin
               Write(RiffId[1], 4); // 'RIFF'
               Write(RiffCount, SizeOf(DWORD)); // file data size
               Write(WaveId[1], Length(WaveId)); // 'WAVE'
               Write(FmtId[1], Length(FmtId)); // 'fmt '
               TempInt := SizeOf(TWaveFormatEx);
               Write(TempInt, SizeOf(DWORD)); // TWaveFormat data size
               Write(WaveFormatEx, SizeOf(TWaveFormatEx)); // WaveFormatEx record
               Write(DataId[1], Length(DataId)); // 'data'
               Write(DataCount, SizeOf(DWORD)); // sound data size
               if OPT_USE_AUDIO_SKIP then
                    Write(SkipBuf[0], SkipCount);
               CopyFrom(FS, FS.Size);
          end;
     finally
          FreeAndNil(FS);
     end;
     Snd := MS;
{$IFNDEF USE_WIN32_API}
     AudioFN := CombinePath(TPath.GetTempPath, TEMP_FILE_NAME);
     Snd.SaveToFile(AudioFN);
     FreeAndNil(Snd);
     //InitMediaPlayer;
     MPlayer.FileName := AudioFN;
{$ENDIF}
end;

procedure TIAMovieDecoder.PrepareVideo;
begin
     videoThread := TThread.CreateAnonymousThread(
          procedure
          begin
               DoPlay;
          end
     );
     with videoThread do
     begin
{$IFDEF USE_WIN32_API}
          Priority := TThreadPriority.tpHighest;
{$ENDIF}
          FreeOnTerminate := True;
     end;
end;

procedure TIAMovieDecoder.SetEventHandlers(OnDrawFrame: TOnDrawFrame; OnPlaybackDone: TOnPlaybackDone);
begin
     FOnDrawFrame := OnDrawFrame;
     FOnPlaybackDone := OnPlaybackDone;
end;

procedure TIAMovieDecoder.SetGamePath(const GPath: string);
begin
     GamePath := GPath;
     LoadPalettes;
end;

procedure TIAMovieDecoder.StartAudio;
begin
     //Sleep(750);
{$IFDEF USE_WIN32_API}
     PlaySound(Snd.Memory, 0, SND_MEMORY or SND_ASYNC or SND_NODEFAULT);
     FreeAndNil(Snd);
{$ELSE}
     MPlayer.Play;
{$ENDIF}
end;

procedure TIAMovieDecoder.StartVideo;
begin
     Playing := True;
     videoThread.Start;
end;

procedure TIAMovieDecoder.Stop;
begin
     if Playing and not Stopping then
     begin
          Stopping := True;
          if HasAudio then
               StopAudio;
     end;
end;

procedure TIAMovieDecoder.StopAudio;
begin
{$IFDEF USE_WIN32_API}
     PlaySound(nil, 0, 0);
{$ELSE}
     MPlayer.Clear;
{$ENDIF}
end;

end.
