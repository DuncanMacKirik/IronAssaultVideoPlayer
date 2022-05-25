unit uMain;

interface

uses
     System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
     FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
     FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls, FMX.Edit,
     FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Media,
     FMX.Objects;

{$DEFINE USE_WIN32_SOUND}

const
     MAX_FILE_SIZE = 128 * 1024 * 1024; // 128 MB

     IMG_OFS = $00000000;

     GAME_PATH = 'C:\Games\Iron Assault\IRON';

     PAL_FN = 'IRON_CD\FILMS\FILM.LZ';
     PAL_FN2 = 'IRON_CD\E_MAIN\BACKTXT.LZ';
     PAL_SIZE = 48; // 16 * 3
     PAL_SIZE_3X = 3 * PAL_SIZE;
     PAL_OFS = 4;
     PAL_OFS2 = 196;

     FPS_DELAY = 70;
     AUDIO_DELAY = 750;
     AUDIO_SAMPLE_RATE = 11025; // Hz
     AUDIO_SKIP_COEF = 0.00128;

     FMT_MOVIE_INFO = '%d frames, %f seconds'#13'%f frames per second.';

     ERR_FILE_TOO_LARGE = 'File is too large';
     ERR_UNKNOWN_VIDEO_RES = 'Unknown video resolution';
     ERR_CANNOT_MAP_BITMAP_DATA = 'Cannot map bitmap data';

     OPT_DYNAMIC_DELAYS = True;
     OPT_USE_AUDIO_SKIP = True;
     OPT_DYNAMIC_SKIPS = True;

type
     TColorValueFunc = function (CN: Integer): TAlphaColor of object;

     TForm1 = class(TForm)
          ImageViewer1: TImageViewer;
          btnPlay: TButton;
          mInfo: TMemo;
          Label1: TLabel;
          edGamePath: TEdit;
          btnSelDir: TButton;
          GameFilesList: TListBox;
          rbEnglish: TRadioButton;
          rbFrench: TRadioButton;
          rbGerman: TRadioButton;
          lblStatus: TLabel;
          Label3: TLabel;
          btnStop: TButton;
          imgSelDir: TImage;
          rbPaletteBlue: TRadioButton;
          rbPaletteRed: TRadioButton;
          rbPaletteGolden: TRadioButton;
          rbPaletteTeal: TRadioButton;
          gbAudioLang: TGroupBox;
          gbPalette: TGroupBox;
    rbDisableAudio: TRadioButton;
    Label2: TLabel;
          procedure btnPlayClick(Sender: TObject);
          procedure FormClose(Sender: TObject; var Action: TCloseAction);
          procedure btnSelDirClick(Sender: TObject);
          procedure FormCreate(Sender: TObject);
          procedure GameFilesListItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
          procedure btnStopClick(Sender: TObject);
          procedure FormDestroy(Sender: TObject);
     protected
          GamePath, VideoFN, SoundFN: string;
          HasAudio: Boolean;
          imgWidth: Integer;
          imgHeight: Integer;
          DoubleHeight: Boolean;
          Data: array of Byte;
          DataOfs: Integer;
          src, tgt: TRectF;
          Palette: array [0..767] of Byte;
          Snd: TMemoryStream;
          SkipBuf: array of Byte;
          SkipCount: Integer;
          DynDelay: Integer;
          videoThread: TThread;
          ColorValueFunc: TColorValueFunc;
{$IFNDEF USE_WIN32_SOUND}
          AudioFN: string;
          MPlayer: TMediaPlayer;
{$ENDIF}

          class var Stopping: Boolean;
          class var Playing: Boolean;
     public
          procedure ShowError(const Msg: string);
          procedure ShowStatus(const Msg: string);

          procedure SetGamePath(const Dir: string);
          procedure ScanForGameFiles;

          procedure LoadPalettes;
          procedure LoadMovie;

          procedure InitColorFunc;
          procedure InitSkipBuffer(Duration: Integer = 0);
{$IFNDEF USE_WIN32_SOUND}
          procedure InitMediaPlayer;
{$ENDIF}

          function GetLangSoundFN: string;

          procedure InitFrame;
          function GetNextFrame: TBitmap;
          procedure DrawFrame(bmp: TBitmap);

          procedure PrepareVideo;
          procedure PrepareAudio;

          procedure StartVideo;
          procedure StartAudio;

          procedure StopAudio;
          procedure DoPlay;

          procedure ShowInfo(const frames, msec: Int64);

          procedure DisableLangs;
          procedure EnableLangs;

          function DoGetColorValue(CN: Integer; ClrOfs: Integer): TAlphaColor;
          function GetColorValue(CN: Integer): TAlphaColor;
          function GetColorValueRed(CN: Integer): TAlphaColor;
          function GetColorValueGolden(CN: Integer): TAlphaColor;
          function GetColorValueTeal(CN: Integer): TAlphaColor;
     end;

     EFileTooLarge = class(EStreamError)
          constructor Create;
     end;

     EUnknownVideoResolution = class(EStreamError)
          constructor Create;
     end;

var
     Form1: TForm1;


implementation

uses
     System.Threading, System.Diagnostics, System.IOUtils, System.TimeSpan,
     FMX.Dialogs.Win, Winapi.Windows, Winapi.MMSystem;

type
     TBitmap = FMX.Graphics.TBitmap;

{$R *.fmx}

procedure TForm1.PrepareAudio;
const
     Mono: Word = $0001;
     SampleRate: Integer = AUDIO_SAMPLE_RATE; // 8000, 11025, 22050, or 44100
     RiffId: AnsiString = 'RIFF';
     WaveId: AnsiString = 'WAVE';
     FmtId: AnsiString = 'fmt ';
     DataId: AnsiString = 'data';
{$IFNDEF USE_WIN32_SOUND}
     TEMP_FILE_NAME = 'IronAssaultVideoPlayer.tmp';
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

     FS := TFileStream.Create(GetLangSoundFN, fmOpenRead);
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
{$IFNDEF USE_WIN32_SOUND}
     AudioFN := TPath.Combine(TPath.GetTempPath, TEMP_FILE_NAME);
     Snd.SaveToFile(AudioFN);
     FreeAndNil(Snd);
     //InitMediaPlayer;
     MPlayer.FileName := AudioFN;
{$ENDIF}
end;

procedure TForm1.PrepareVideo;
begin
     videoThread := TThread.CreateAnonymousThread(
          procedure
          begin
               DoPlay;
          end
     );
     with videoThread do
     begin
          Priority := TThreadPriority.tpHighest;
          FreeOnTerminate := True;
     end;
end;

procedure TForm1.btnPlayClick(Sender: TObject);
begin
     if rbDisableAudio.IsChecked then
          HasAudio := False;

     LoadPalettes;
     LoadMovie;
     InitColorFunc;
     InitFrame;
     PrepareVideo;
     if HasAudio then
          PrepareAudio;

     try
          btnPlay.Enabled := False;
          btnStop.Enabled := True;
          Stopping := False;
          StartVideo;
          if HasAudio then
               StartAudio;
     except
          on E: Exception do
          begin
               btnPlay.Enabled := False;
               btnStop.Enabled := True;
               raise E;
          end;
     end;
end;

procedure TForm1.btnSelDirClick(Sender: TObject);
var
     FDir: string;
begin
     FDir := GAME_PATH;
     if SelectDirectory('Select Directory', FDir, FDir) then
     begin
          SetGamePath(FDir);
     end;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
     if Playing and not Stopping then
     begin
          Stopping := True;
          if HasAudio then
               StopAudio;
     end;
end;

procedure TForm1.DisableLangs;
begin
     rbEnglish.Enabled := False;
     rbFrench.Enabled := False;
     rbGerman.Enabled := False;
end;

procedure TForm1.DoPlay;
var
     bmp: TBitmap;
     frm: Integer;
     Stopwatch: TStopwatch;
     Elapsed: TTimeSpan;
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
               ShowInfo(frm, Stopwatch.ElapsedMilliseconds);
{$IFNDEF USE_WIN32_SOUND}
               if HasAudio then
                    StopAudio;
{$ENDIF}
               btnPlay.Enabled := GameFilesList.Selected <> nil;
               btnStop.Enabled := False;
          end
     );
end;

procedure TForm1.DrawFrame(bmp: TBitmap);
begin
     if bmp <> nil then
          TThread.Queue(nil,
               procedure
               begin
                    try
                         ImageViewer1.Bitmap.Assign(bmp);
                    finally
                         FreeAndNil(bmp);
                    end;
               end
          );
end;

procedure TForm1.EnableLangs;
begin
     rbEnglish.Enabled := True;
     rbFrench.Enabled := True;
     rbGerman.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     if Playing and HasAudio then
          StopAudio;
     if not Stopping then
     begin
          Stopping := True;
          Sleep(200);
     end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     Playing := False;
     Stopping := False;
     SetGamePath(GAME_PATH);
     if OPT_USE_AUDIO_SKIP and not OPT_DYNAMIC_SKIPS then
          InitSkipBuffer;
{$IFNDEF USE_WIN32_SOUND}
     InitMediaPlayer;
{$ENDIF}
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
     if Assigned(Snd) then
          FreeAndNil(Snd);
{$IFNDEF USE_WIN32_SOUND}
     FreeAndNil(MPlayer);
{$ENDIF}
end;

(*
 ===============================================
 Video info:
 raw planar ("mode X") video, 16 colors,
 indexed palette, ~ 14 fps.
 Frame size: 256 x 83
 Frame display size: ~ 256 x 166
 Byte order (total 10624 bytes in 1 frame):
   [A1A2] [A3A4] ...  } - 5312 bytes, Plane A
   [B1B2] [B3B4] ...  } - 5312 bytes, Plane B
 ([xxxx] = 1 byte, Ax / Bx = 1 nibble (4 bits),
 each nibble is a 0..15 color index in the
 256-color palette stored in FILM.LZ).

 Conversion to linear mode:
 [0A2] [0B2] [0A1] [0B1] [0A4] [0B4] [0A3] [0B3]
 ...

 Audio info:
 headerless unsigned 8-bit, 11025 Hz, mono
 ===============================================
*)

procedure TForm1.GameFilesListItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
     if Item = nil then
          Exit;
     VideoFN := TPath.Combine(GamePath, 'IRON_CD\FILMS\' + Item.TagString + '.ANI');
     if Item.Tag = 1 then
     begin
          HasAudio := True;
          if Item.TagString.StartsWith('KIA_') then
               SoundFN := 'KIA.RAW'
          else
               SoundFN := Item.TagString + '.RAW';
          EnableLangs;
     end
     else
     begin
          HasAudio := False;
          SoundFN := '';
          DisableLangs;
     end;
     btnPlay.Enabled := not (Playing or Stopping);
end;

function TForm1.GetLangSoundFN: string;
var
     L: Char;
begin
     L := 'E';
     if rbFrench.IsChecked then
          L := 'F';
     if rbGerman.IsChecked then
          L := 'G';
     Result := TPath.Combine(GamePath, 'IRON_CD\' + L + '_FDIGI\' + SoundFN);
end;

function TForm1.DoGetColorValue(CN: Integer; ClrOfs: Integer): TAlphaColor;
var
     Clr: TAlphaColorRec;
begin
     Inc(CN, ClrOfs);
     Clr.A := 255;
     Clr.R := Palette[CN * 3];
     Clr.G := Palette[CN * 3 + 1];
     Clr.B := Palette[CN * 3 + 2];
     Result := Clr.Color;
end;

function TForm1.GetColorValue(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 0);
end;

function TForm1.GetColorValueRed(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 16);
end;

function TForm1.GetColorValueGolden(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 32);
end;

function TForm1.GetColorValueTeal(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 48);
end;

function TForm1.GetNextFrame: TBitmap;
var
     lWidth, x, y, xr, yr, i, p, pb, s: Integer;
     bmp, bmp2: TBitmap;
     ISL: PByteArray;
     bd: TBitmapData;
     SL: Pointer;
     CD: DWord;
     CW: Word;
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
                         SL := bd.GetScanline(y);
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

procedure TForm1.InitColorFunc;
begin
     if rbPaletteRed.IsChecked then
          ColorValueFunc := GetColorValueRed
     else
     if rbPaletteGolden.IsChecked then
          ColorValueFunc := GetColorValueGolden
     else
     if rbPaletteTeal.IsChecked then
          ColorValueFunc := GetColorValueTeal
     else
          ColorValueFunc := GetColorValue;
end;

procedure TForm1.InitFrame;
var
     bmp: TBitmap;
begin
     src := RectF(0, 0, imgWidth, imgHeight);
     if DoubleHeight then
          tgt := RectF(0, 0, imgWidth, imgHeight * 2)
     else
          tgt := RectF(0, 0, imgWidth, imgHeight);
     bmp := TBitmap.Create;
     bmp.Width := imgWidth;
     if DoubleHeight then
          bmp.Height := imgHeight * 2
     else
          bmp.Height := imgHeight;
     ImageViewer1.BitmapScale := 2;
     DrawFrame(bmp);
end;

{$IFNDEF USE_WIN32_SOUND}
procedure TForm1.InitMediaPlayer;
begin
     MPlayer := TMediaPlayer.Create(nil);
end;
{$ENDIF}

procedure TForm1.InitSkipBuffer(Duration: Integer = 0);
begin
     if Duration = 0 then
          Duration := AUDIO_DELAY;
     SkipCount := (Duration * AUDIO_SAMPLE_RATE) div 1000;
     SetLength(SkipBuf, SkipCount);
     FillChar(SkipBuf[0], SkipCount, 127);
end;

procedure TForm1.LoadMovie;
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

procedure TForm1.LoadPalettes;

     procedure DoLoadPalette(const PFN: string; const PalOfs, PalSize, PalIndex: Integer);
     var
          PFS: TFileStream;
     begin
          PFS := TFileStream.Create(
               TPath.Combine(GamePath, PFN),
               fmOpenRead
          );
          try
               PFS.Position := PalOfs;
               PFS.Read(Palette[PalIndex], PalSize);
          finally
               FreeAndNil(PFS);
          end;
     end;

var
     PS, i: Integer;
begin
     DoLoadPalette(PAL_FN, PAL_OFS, PAL_SIZE_3X, 0);
     DoLoadPalette(PAL_FN2, PAL_OFS2, PAL_SIZE, PAL_SIZE_3X);
     for i := 0 to SizeOf(Palette)-1 do
          Palette[i] := (Palette[i] shl 2) or 3;
end;

procedure TForm1.StartAudio;
begin
     //Sleep(750);
{$IFDEF USE_WIN32_SOUND}
     PlaySound(Snd.Memory, 0, SND_MEMORY or SND_ASYNC or SND_NODEFAULT);
     FreeAndNil(Snd);
{$ELSE}
     MPlayer.Play;
{$ENDIF}
end;

procedure TForm1.StartVideo;
begin
     Playing := True;
     videoThread.Start;
end;

procedure TForm1.StopAudio;
begin
{$IFDEF USE_WIN32_SOUND}
     PlaySound(nil, 0, 0);
{$ELSE}
     MPlayer.Clear;
{$ENDIF}
end;

function FileSize(const aFilename: String): Int64;
var
     info: TWin32FileAttributeData;
begin
     result := -1;

     if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
          Exit;

     result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

procedure TForm1.ScanForGameFiles;
var
     inx: Integer;
     CDDir, FilmsDir, FN, MN, LN, SN: string;
     LHasSound: Boolean;
begin
     lblStatus.Visible := False;
     GameFilesList.Clear;
     CDDir := TPath.Combine(GamePath, 'IRON_CD');
     FilmsDir := TPath.Combine(CDDir, 'FILMS');
     if not (TDirectory.Exists(CDDir) and TDirectory.Exists(FilmsDir) and
          TFile.Exists(TPath.Combine(GamePath, PAL_FN)) and
          TFile.Exists(TPath.Combine(GamePath, PAL_FN2))) then
     begin
          ShowError('Game files not found!');
          Exit;
     end;
     ShowStatus('OK, game files found.');
     for FN in TDirectory.GetFiles(FilmsDir, '*.ANI') do
     begin
          MN := TPath.GetFileNameWithoutExtension(FN);
          SN := TPath.Combine(CDDir, 'E_FDIGI');
          if MN.StartsWith('KIA_') then
               SN := TPath.Combine(SN, 'KIA.RAW')
          else
               SN := TPath.Combine(SN, MN + '.RAW');
          if TFile.Exists(SN) then
          begin
               LN := MN + ' *';
               LHasSound := True;
          end
          else
          begin
               LN := MN;
               LHasSound := False;
          end;
          inx := GameFilesList.Items.Add(LN);
          with GameFilesList.ListItems[inx] do
          begin
               TagString := MN;
               if LHasSound then
                    Tag := 1
               else
                    Tag := 0;
          end;
     end;
end;

procedure TForm1.SetGamePath(const Dir: string);
begin
     GamePath := Dir;
     edGamePath.Text := GamePath;
     ScanForGameFiles;
end;

procedure TForm1.ShowError(const Msg: string);
begin
     with lblStatus do
     begin
          TextSettings.FontColor := TAlphaColors.Coral;
          Text := Msg;
          Visible := True;
     end;
end;

procedure TForm1.ShowInfo(const frames, msec: Int64);
var
     sec: Double;
begin
     sec := Double(msec) / 1000;
     mInfo.Lines.Add(Format(FMT_MOVIE_INFO, [frames, sec, Double(frames) / sec]));
end;

procedure TForm1.ShowStatus(const Msg: string);
begin
     with lblStatus do
     begin
          TextSettings.FontColor := TAlphaColors.Darkgreen;
          Text := Msg;
          Visible := True;
     end;
end;

{ EFileTooLarge }

constructor EFileTooLarge.Create;
begin
     inherited Create(ERR_FILE_TOO_LARGE);
end;

{ EUnknownVideoResolution }

constructor EUnknownVideoResolution.Create;
begin
     inherited Create(ERR_UNKNOWN_VIDEO_RES);
end;

end.
