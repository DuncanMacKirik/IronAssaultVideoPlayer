unit IAPaletteHandler;

interface

uses
     System.Classes, System.SysUtils, System.UITypes;

const
     PAL_FN = 'IRON_CD\FILMS\FILM.LZ';
     PAL_FN2 = 'IRON_CD\E_MAIN\BACKTXT.LZ';
     PAL_SIZE = 48; // 16 * 3
     PAL_SIZE_3X = 3 * PAL_SIZE;
     PAL_SIZE_4X = 4 * PAL_SIZE;
     PAL_OFS = 4;
     PAL_OFS2 = 196;

type
     TIAPalette = array [0..767] of Byte;
     TIAPaletteType = (Blue, Red, Golden, Teal);
     TColorValueFunc = function (CN: Integer): TAlphaColor of object;

     IIAPaletteHandler = interface
     ['{67DCBE6A-CF5A-45AA-9482-21DBF91EFC16}']
          function GetColorValueFunc(const Palette: TIAPaletteType): TColorValueFunc;
          procedure LoadPalettes(const GamePath: string);
     end;

     TIAPaletteHandler = class(TInterfacedObject, IIAPaletteHandler)
     protected
          Palette: TIAPalette;

          function DoGetColorValue(CN: Integer; ClrOfs: Integer): TAlphaColor;
          function GetColorValueBlue(CN: Integer): TAlphaColor;
          function GetColorValueRed(CN: Integer): TAlphaColor;
          function GetColorValueGolden(CN: Integer): TAlphaColor;
          function GetColorValueTeal(CN: Integer): TAlphaColor;
          procedure CorrectColors;
     public
          function GetColorValueFunc(const Palette: TIAPaletteType): TColorValueFunc;
          procedure LoadPalettes(const GamePath: string);
     end;


implementation

uses
     System.IOUtils, IAUtils;

function TIAPaletteHandler.GetColorValueBlue(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 0);
end;

function TIAPaletteHandler.GetColorValueRed(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 16);
end;

function TIAPaletteHandler.GetColorValueGolden(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 32);
end;

function TIAPaletteHandler.GetColorValueTeal(CN: Integer): TAlphaColor;
begin
     Result := DoGetColorValue(CN, 48);
end;

{ TIAPaletteHandler }

procedure TIAPaletteHandler.CorrectColors;
var
     i: Integer;
begin
     for i := 0 to SizeOf(Palette)-1 do
          Palette[i] := (Palette[i] shl 2) or 3;
end;

function TIAPaletteHandler.DoGetColorValue(CN, ClrOfs: Integer): TAlphaColor;
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

function TIAPaletteHandler.GetColorValueFunc(const Palette: TIAPaletteType): TColorValueFunc;
var
     ColorValueFuncs: array [TIAPaletteType] of TColorValueFunc;
begin
     ColorValueFuncs[Blue] := GetColorValueBlue;
     ColorValueFuncs[Red] := GetColorValueRed;
     ColorValueFuncs[Golden] := GetColorValueGolden;
     ColorValueFuncs[Teal] := GetColorValueTeal;
     Result := ColorValueFuncs[Palette];
end;

procedure TIAPaletteHandler.LoadPalettes(const GamePath: string);

     procedure DoLoadPalette(const PFN: string; const PalOfs, PalSize, PalIndex: Integer);
     var
          PFS: TFileStream;
     begin
          PFS := TFileStream.Create(
               CombinePath(GamePath, PFN),
               fmOpenRead
          );
          try
               PFS.Position := PalOfs;
               PFS.Read(Palette[PalIndex], PalSize);
          finally
               FreeAndNil(PFS);
          end;
     end;

begin
     DoLoadPalette(CombinePath(GamePath, PAL_FN), PAL_OFS, PAL_SIZE_3X, 0);
     DoLoadPalette(CombinePath(GamePath, PAL_FN2), PAL_OFS2, PAL_SIZE, PAL_SIZE_3X);
     CorrectColors;
end;

end.