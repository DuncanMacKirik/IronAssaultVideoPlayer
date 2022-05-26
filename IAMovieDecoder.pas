unit IAMovieDecoder;

interface

{$INCLUDE defines.inc}

uses
     System.Classes, System.Types, System.UITypes,
     FMX.Graphics,
     IAAudioHandler, IAPalettes, IAUtils;

const
     IMG_OFS = 0;

     FPS_DELAY = 70;

     FMT_MOVIE_INFO = '%d frames, %f seconds'#13'%f frames per second.';

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
          AudioHandler: IIAAudioHandler;
          PaletteHandler: IIAPaletteHandler;
          PaletteLoaded: Boolean;
          src, tgt: TRectF;
          DynDelay: Integer;
          videoThread: TThread;
          ColorValueFunc: TColorValueFunc;
          FOnDrawFrame: TOnDrawFrame;
          FOnPlaybackDone: TOnPlaybackDone;

          function HasAudio: Boolean;

          procedure InitColorFunc(const Palette: TIAPaletteType);

          procedure InitFrame;
          function GetNextFrame: TBitmap;
          procedure DrawFrame(bmp: TBitmap);

          procedure LoadPalettes;
          procedure LoadVideo(const VideoFN: string);

          procedure PrepareAudio(const AudioFN: string);
          procedure PrepareVideo;

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


implementation

uses
     System.Threading, System.Diagnostics, System.IOUtils,
     System.SysUtils, System.TimeSpan;

{ TIAMovieDecoder }

constructor TIAMovieDecoder.Create(OnDrawFrame: TOnDrawFrame; OnPlaybackDone: TOnPlaybackDone);
begin
     Playing := False;
     Stopping := False;
     AudioHandler := TIAAudioHandler.Create;
     PaletteHandler := TIAPaletteHandler.Create;
     PaletteLoaded := False;
     SetEventHandlers(OnDrawFrame, OnPlaybackDone);
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
     PrepareAudio(AudioFN);
     PrepareVideo;
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

procedure TIAMovieDecoder.PrepareAudio(const AudioFN: string);
begin
     if not HasAudio then
          Exit;
     AudioHandler.PrepareAudio(AudioFN);
     if OPT_DYNAMIC_DELAYS then
          DynDelay := Trunc((Single(AudioHandler.GetDataCount)*1000 / AUDIO_SAMPLE_RATE) / (Length(Data) / 10624));
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
     AudioHandler.StartAudio;
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
     AudioHandler.StopAudio;
end;

end.
