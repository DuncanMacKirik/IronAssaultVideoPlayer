unit IAAudioHandler;

{$INCLUDE defines.inc}

interface

uses
     System.Classes, FMX.Media;

const
     AUDIO_DELAY = 750;
     AUDIO_SAMPLE_RATE = 11025; // Hz
     AUDIO_SKIP_COEF = 0.00128;
     OPT_USE_AUDIO_SKIP = True;
     OPT_DYNAMIC_SKIPS = True;
     OPT_DYNAMIC_DELAYS = True;

type
     IIAAudioHandler = interface
     ['{E830D869-88CF-4C3F-A3C4-CA7F4773CEAC}']
          procedure PrepareAudio(const AudioFName: string);

          procedure StartAudio;
          procedure StopAudio;

          function GetDataCount: Integer;
     end;

     TIAAudioHandler = class(TInterfacedObject, IIAAudioHandler)
     protected
          Snd: TMemoryStream;
          SkipBuf: array of Byte;
          SkipCount: Integer;
          DataCount: Integer;
{$IFNDEF USE_WIN32_API}
          AudioFN: string;
          MPlayer: TMediaPlayer;

          procedure InitMediaPlayer;
{$ENDIF}
          procedure InitSkipBuffer(Duration: Integer = 0);
     public
          constructor Create;
          destructor Destroy; override;

          procedure PrepareAudio(const AudioFName: string);

          procedure StartAudio;
          procedure StopAudio;

          function GetDataCount: Integer;
     end;


implementation

uses
     System.IOUtils, System.SysUtils, System.Types,
{$IFDEF USE_WIN32_API}
     Winapi.Windows, Winapi.MMSystem,
{$ENDIF}
     IAUtils;

{ TIAAudioHandler }

constructor TIAAudioHandler.Create;
begin
{$IFNDEF USE_WIN32_API}
     InitMediaPlayer;
{$ENDIF}
     if OPT_USE_AUDIO_SKIP and not OPT_DYNAMIC_SKIPS then
          InitSkipBuffer;
end;

destructor TIAAudioHandler.Destroy;
begin
     inherited;
     if Assigned(Snd) then
          FreeAndNil(Snd);
{$IFNDEF USE_WIN32_API}
     FreeAndNil(MPlayer);
{$ENDIF}
end;

function TIAAudioHandler.GetDataCount: Integer;
begin
     Result := DataCount;
end;

{$IFNDEF USE_WIN32_API}
procedure TIAAudioHandler.InitMediaPlayer;
begin
     MPlayer := TMediaPlayer.Create(nil);
end;
{$ENDIF}

procedure TIAAudioHandler.InitSkipBuffer(Duration: Integer);
begin
     if Duration = 0 then
          Duration := AUDIO_DELAY;
     SkipCount := (Duration * AUDIO_SAMPLE_RATE) div 1000;
     SetLength(SkipBuf, SkipCount);
     FillChar(SkipBuf[0], SkipCount, 127);
end;

procedure TIAAudioHandler.PrepareAudio(const AudioFName: string);
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
     TempInt, RiffCount: Integer;
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

procedure TIAAudioHandler.StartAudio;
begin
{$IFDEF USE_WIN32_API}
     PlaySound(Snd.Memory, 0, SND_MEMORY or SND_ASYNC or SND_NODEFAULT);
     FreeAndNil(Snd);
{$ELSE}
     MPlayer.Play;
{$ENDIF}
end;

procedure TIAAudioHandler.StopAudio;
begin
{$IFDEF USE_WIN32_API}
     PlaySound(nil, 0, 0);
{$ELSE}
     MPlayer.Clear;
{$ENDIF}
end;

end.