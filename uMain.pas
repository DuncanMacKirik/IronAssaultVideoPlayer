unit uMain;

interface

uses
     System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
     FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
     FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls,
     FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.ListBox, FMX.Objects,
     IAMovieDecoder, IAPaletteHandler, IAUtils;

const
     GAME_PATH = 'C:\Games\Iron Assault\IRON';

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
          cbAspRatioCorr: TCheckBox;

          procedure FormCreate(Sender: TObject);
          procedure FormClose(Sender: TObject; var Action: TCloseAction);

          procedure btnSelDirClick(Sender: TObject);
          procedure btnPlayClick(Sender: TObject);
          procedure btnStopClick(Sender: TObject);

          procedure GameFilesListItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
          procedure cbAspRatioCorrChange(Sender: TObject);
          procedure rbPaletteBlueChange(Sender: TObject);
          procedure rbPaletteRedChange(Sender: TObject);
          procedure rbPaletteGoldenChange(Sender: TObject);
          procedure rbPaletteTealChange(Sender: TObject);
     protected
          GamePath: string;
          PlayAudio: Boolean;
          MovieDecoder: IIAMovieDecoder;
          VideoFN, AudioFN: string;

          function GetLangAudioFN: string;
          function GetCheckedRadioButton(const rbs: array of TRadioButton): TRadioButton;
     public
          function GetSelectedAudio: TIAAudioType;
          function GetSelectedPalette: TIAPaletteType;

          procedure ShowError(const Msg: string);
          procedure ShowStatus(const Msg: string);

          procedure SetGamePath(const Dir: string);
          procedure ScanForGameFiles;

          procedure DrawFrame(bmp: TBitmap);
          procedure ShowInfo(const frames, msec: Int64);

          procedure DisableLangs;
          procedure EnableLangs;
     end;

var
     Form1: TForm1;


implementation

uses
     System.IOUtils;

{$R *.fmx}

procedure TForm1.btnPlayClick(Sender: TObject);
begin
     MovieDecoder.LoadMovie(VideoFN, GetLangAudioFN, GetSelectedAudio, GetSelectedPalette);
     try
          btnPlay.Enabled := False;
          btnStop.Enabled := True;
          MovieDecoder.Play;
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
     MovieDecoder.Stop;
end;

procedure TForm1.cbAspRatioCorrChange(Sender: TObject);
begin
     MovieDecoder.SetCorrectAspectRatio(cbAspRatioCorr.IsChecked);
end;

procedure TForm1.DisableLangs;
begin
     rbEnglish.Enabled := False;
     rbFrench.Enabled := False;
     rbGerman.Enabled := False;
end;

procedure TForm1.DrawFrame(bmp: TBitmap);
begin
     ImageViewer1.Bitmap.Assign(bmp);
end;

procedure TForm1.EnableLangs;
begin
     rbEnglish.Enabled := True;
     rbFrench.Enabled := True;
     rbGerman.Enabled := True;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
     MovieDecoder.ForceStop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IF CompilerVersion >= 35.0}
     Constraints.MinWidth := 894;
     Constraints.MinHeight := 643;
{$IFEND}
     MovieDecoder := TIAMovieDecoder.Create(True, DrawFrame, ShowInfo);
     SetGamePath(GAME_PATH);
     ImageViewer1.BitmapScale := 2;
end;

procedure TForm1.GameFilesListItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
begin
     if Item = nil then
          Exit;
     VideoFN := CombinePath(GamePath, 'IRON_CD\FILMS\' + Item.TagString + '.ANI');
     if Item.Tag = 1 then
     begin
          PlayAudio := True;
          if Item.TagString.StartsWith('KIA_') then
               AudioFN := 'KIA.RAW'
          else
               AudioFN := Item.TagString + '.RAW';
          EnableLangs;
     end
     else
     begin
          PlayAudio := False;
          AudioFN := '';
          DisableLangs;
     end;
     with TIAMovieDecoder do
          btnPlay.Enabled := not (Playing or Stopping);
end;

function TForm1.GetCheckedRadioButton(const rbs: array of TRadioButton): TRadioButton;
var
     rb: TRadioButton;
begin
     for rb in rbs do
          if rb.IsChecked then
               Exit(rb);
     Exit(nil);
end;

function TForm1.GetLangAudioFN: string;
var
     L: Char;
begin
     if rbDisableAudio.IsChecked then
          Exit('');
     L := Chr(Ord('E') - 1 + GetCheckedRadioButton([rbEnglish, rbFrench, rbGerman]).Tag);
     Result := CombinePath(GamePath, 'IRON_CD\' + L + '_FDIGI\' + AudioFN);
end;

function TForm1.GetSelectedAudio: TIAAudioType;
begin
     if not PlayAudio then
          Result := Disabled
     else
          Result := TIAAudioType(GetCheckedRadioButton([rbDisableAudio, rbEnglish, rbFrench, rbGerman]).Tag);
end;

function TForm1.GetSelectedPalette: TIAPaletteType;
begin
     Result := TIAPaletteType(GetCheckedRadioButton([rbPaletteBlue, rbPaletteRed, rbPaletteGolden, rbPaletteTeal]).Tag);
end;

procedure TForm1.rbPaletteBlueChange(Sender: TObject);
begin
     if Assigned(MovieDecoder) then
          MovieDecoder.PaletteType := Blue;
end;

procedure TForm1.rbPaletteGoldenChange(Sender: TObject);
begin
     if Assigned(MovieDecoder) then
          MovieDecoder.PaletteType := Golden;
end;

procedure TForm1.rbPaletteRedChange(Sender: TObject);
begin
     if Assigned(MovieDecoder) then
          MovieDecoder.PaletteType := Red;
end;

procedure TForm1.rbPaletteTealChange(Sender: TObject);
begin
     if Assigned(MovieDecoder) then
          MovieDecoder.PaletteType := Teal;
end;

procedure TForm1.ScanForGameFiles;
var
     inx: Integer;
     CDDir, FilmsDir, FN, MN, LN, SN: string;
     LHasSound: Boolean;
begin
     lblStatus.Visible := False;
     GameFilesList.Clear;
     CDDir := CombinePath(GamePath, 'IRON_CD');
     FilmsDir := CombinePath(CDDir, 'FILMS');
     if not (TDirectory.Exists(CDDir) and TDirectory.Exists(FilmsDir) and
          TFile.Exists(CombinePath(GamePath, PAL_FN)) and
          TFile.Exists(CombinePath(GamePath, PAL_FN2))) then
     begin
          ShowError('Game files not found!');
          Exit;
     end;
     ShowStatus('OK, game files found.');
     MovieDecoder.SetGamePath(GamePath);
     for FN in TDirectory.GetFiles(FilmsDir, '*.ANI') do
     begin
          MN := TPath.GetFileNameWithoutExtension(FN);
          SN := CombinePath(CDDir, 'E_FDIGI');
          if MN.StartsWith('KIA_') then
               SN := CombinePath(SN, 'KIA.RAW')
          else
               SN := CombinePath(SN, MN + '.RAW');
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
     btnPlay.Enabled := GameFilesList.Selected <> nil;
     btnStop.Enabled := False;
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

end.
