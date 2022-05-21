unit uMain;

interface

uses
     System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
     FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
     FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls, FMX.Edit,
     FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Media;

const
     MAX_FILE_SIZE = 128 * 1024 * 1024;

     IMG_OFS = $00000000;

     GAME_PATH = 'C:\Games\Iron Assault\IRON';

//     VID_FN = 'IRON_CD\FILMS\RESCUE.ANI';
     PAL_FN = 'IRON_CD\FILMS\FILM.LZ';
//     SND_FN = 'IRON_CD\E_FDIGI\RESCUE.RAW';

     imgWidth = 256;
     imgHeight = 83;
     FPS_DELAY = 70; //34;

     FMT_MOVIE_INFO = '%d frames, %f seconds'#13'%f frames per second.';

     ERR_FILE_TOO_LARGE = 'File is too large!';
     ERR_CANNOT_MAP_BITMAP_DATA = 'Cannot map bitmap data!';

type
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
          Label2: TLabel;
          lblStatus: TLabel;
          Label3: TLabel;
          btnStop: TButton;
          MediaPlayer1: TMediaPlayer;
          procedure btnPlayClick(Sender: TObject);
          procedure FormClose(Sender: TObject; var Action: TCloseAction);
          procedure btnSelDirClick(Sender: TObject);
          procedure FormCreate(Sender: TObject);
          procedure GameFilesListItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
          procedure btnStopClick(Sender: TObject);
     protected
          GamePath, VideoFN, SoundFN: string;
          HasSound: Boolean;
          Data: array of Byte;
          DataOfs: Integer;
          src, tgt: TRectF;
          Palette: array [0..767] of Byte;
          Snd: TMemoryStream;

          class var Stopping: Boolean;
          class var Playing: Boolean;
     public
          procedure ShowError(const Msg: string);
          procedure ShowStatus(const Msg: string);

          procedure SetGamePath(const Dir: string);
          procedure ScanForGameFiles;

          procedure LoadPalette;
          procedure LoadMovie;
          procedure LoadSoundFromRawFile;

          function GetLangSoundFN: string;

          procedure InitFrame;
          function GetNextFrame: TBitmap;
          procedure DrawFrame(bmp: TBitmap);

          procedure RunPlayThread;
          procedure PlayMovieSound;

          procedure DoPlay;

          procedure ShowInfo(const frames, msec: Int64);

          procedure DisableLangs;
          procedure EnableLangs;
     end;

     EFileTooLarge = class(EStreamError)
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

procedure TForm1.LoadSoundFromRawFile;
const
     Mono: Word = $0001;
     SampleRate: Integer = 11025; // 8000, 11025, 22050, or 44100
     RiffId: AnsiString = 'RIFF';
     WaveId: AnsiString = 'WAVE';
     FmtId: AnsiString = 'fmt ';
     DataId: AnsiString = 'data';
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
               CopyFrom(FS);
          end;
     finally
          FreeAndNil(FS);
     end;
     Snd := MS;
end;

procedure TForm1.btnPlayClick(Sender: TObject);
begin
     btnPlay.Enabled := False;
     btnStop.Enabled := True;
     LoadPalette;
     LoadMovie;
     if HasSound then
          LoadSoundFromRawFile;
     InitFrame;

     Stopping := False;
     RunPlayThread;
     if HasSound then
          PlayMovieSound;
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
          Stopping := True;
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
               Exit;
          end;
          bmp := GetNextFrame;
          DrawFrame(bmp);
          Inc(frm);
          Delay := Stopwatch.ElapsedMilliseconds - FrameStart;
          if Delay < FPS_DELAY then
               Sleep(FPS_DELAY - Delay);
     until bmp = nil;
     Stopwatch.Stop;
     TThread.Queue(nil,
          procedure
          begin
               Playing := False;
               ShowInfo(frm, Stopwatch.ElapsedMilliseconds);
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
          HasSound := True;
          SoundFN := Item.TagString + '.RAW';
          EnableLangs;
     end
     else
     begin
          HasSound := False;
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

function TForm1.GetNextFrame: TBitmap;
const
     lWidth = imgWidth div 4;
var
     x, y, xr, yr, i, p, pb, s: Integer;
     bmp, bmp2: TBitmap;
     ISL: PByteArray;
     bd: TBitmapData;
     SL: Pointer;
     SLA: PAlphaColor;
     Clr: TAlphaColorRec;
     CD: DWord;
     CW: Word;
     CN, c1, c2: Byte;
begin
     i := DataOfs;
     if i >= Length(Data) then
          Exit(nil);
     Clr.A := 255;
     MediaPlayer1.Media.
     bmp := TBitmap.Create;
     try
          bmp.Width := imgWidth;
          bmp.Height := imgHeight;
          if not bmp.Map(TMapAccess.Write, bd) then
               raise EInvalidOpException.Create(ERR_CANNOT_MAP_BITMAP_DATA);
          try
               for y := 0 to imgHeight - 1 do
               begin
                    SL := bd.GetScanline(y);
                    for x := 0 to lWidth - 1 do
                    begin
                         CN := Data[i];
                         c1 := CN shr 4;
                         c2 := CN and 15;
                         xr := x * 4;
                         Clr.R := Palette[c2 * 3];
                         Clr.G := Palette[c2 * 3 + 1];
                         Clr.B := Palette[c2 * 3 + 2];
                         bd.SetPixel(xr, y, Clr.Color);
                         Inc(xr, 2);
                         Clr.R := Palette[c1 * 3];
                         Clr.G := Palette[c1 * 3 + 1];
                         Clr.B := Palette[c1 * 3 + 2];
                         bd.SetPixel(xr, y, Clr.Color);
                         Inc(i);
                    end;
               end;
               for y := 0 to imgHeight - 1 do
               begin
                    SL := bd.GetScanline(y);
                    for x := 0 to lWidth - 1 do
                    begin
                         CN := Data[i];
                         c1 := CN shr 4;
                         c2 := CN and 15;
                         xr := x * 4 + 1;
                         Clr.R := Palette[c2 * 3];
                         Clr.G := Palette[c2 * 3 + 1];
                         Clr.B := Palette[c2 * 3 + 2];
                         bd.SetPixel(xr, y, Clr.Color);
                         Inc(xr, 2);
                         Clr.R := Palette[c1 * 3];
                         Clr.G := Palette[c1 * 3 + 1];
                         Clr.B := Palette[c1 * 3 + 2];
                         bd.SetPixel(xr, y, Clr.Color);
                         Inc(i);
                    end;
               end;
          finally
               bmp.Unmap(bd);
               DataOfs := i;
          end;
     finally
          bmp2 := TBitmap.Create;
          bmp2.Width := imgWidth;
          bmp2.Height := imgHeight * 2;
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

procedure TForm1.InitFrame;
var
     bmp: TBitmap;
begin
     src := RectF(0, 0, imgWidth, imgHeight);
     tgt := RectF(0, 0, imgWidth, imgHeight * 2);
     bmp := TBitmap.Create;
     bmp.Width := imgWidth;
     bmp.Height := imgHeight;
     ImageViewer1.BitmapScale := 2;
     DrawFrame(bmp);
end;

procedure TForm1.LoadMovie;
var
     VFS: TFileStream;
     VFSz: Int64;
begin
     VFS := TFileStream.Create(VideoFN, fmOpenRead);
     try
          VFSz := VFS.Size;
          if VFSz >= MAX_FILE_SIZE then
               raise EFileTooLarge.Create;
          SetLength(Data, VFSz);
          VFS.Read(Data[0], VFSz);
     finally
          DataOfs := 0;
          FreeAndNil(VFS);
     end;
end;

procedure TForm1.LoadPalette;
var
     PFS: TFileStream;
     i: Integer;
begin
     PFS := TFileStream.Create(
          TPath.Combine(GamePath, PAL_FN),
          fmOpenRead
     );
     try
          PFS.Position := 4;
          PFS.Read(Palette[0], SizeOf(Palette));
          for i := 0 to SizeOf(Palette)-1 do
               Palette[i] := (Palette[i] shl 2) or 3;
     finally
          FreeAndNil(PFS);
     end;
end;

procedure TForm1.PlayMovieSound;
begin
     Sleep(Snd.Size div 1000);
     PlaySound(Snd.Memory, 0, SND_MEMORY or SND_ASYNC or SND_NODEFAULT);
end;

procedure TForm1.RunPlayThread;
var
     playThread: TThread;
begin
     playThread := TThread.CreateAnonymousThread(
          procedure
          begin
               DoPlay;
          end
     );
     playThread.Priority := TThreadPriority.tpHighest;
     Playing := True;
     playThread.Start;
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
     if not (TDirectory.Exists(CDDir) and TDirectory.Exists(FilmsDir)) then
     begin
          ShowError('Game files not found!');
          Exit;
     end;
     ShowStatus('OK, game files found.');
     for FN in TDirectory.GetFiles(FilmsDir, '*.ANI') do
     if (FileSize(FN) mod 10624) = 0 then     
     begin
          MN := TPath.GetFileNameWithoutExtension(FN);
          SN := TPath.Combine(CDDir, 'E_FDIGI');
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

end.
