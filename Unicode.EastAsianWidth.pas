unit Unicode.EastAsianWidth;

interface

uses
  System.SysUtils, System.RTLConsts;

type
{$SCOPEDENUMS ON}
  { TEastAsianWidth }
  TEastAsianWidth = (Neutral,     // N
                     Fullwidth,   // F
                     Halfwidth,   // H
                     Wide,        // W
                     Narrow,      // Na
                     Ambiguous);  // A

  { TEastAsianWidthHelper }
  TEastAsianWidthHelper = record helper for TEastAsianWidth
  private
    const
      Names: array [TEastAsianWidth] of String =
        ('Neutral',               // Neutral
         'Fullwidth',             // Fullwidth
         'Halfwidth',             // Halfwidth
         'Wide',                  // Wide
         'Narrow',                // Narrow
         'Ambiguous');            // Ambiguous
      Symbols: array [TEastAsianWidth] of String =
        ('N',                     // Neutral
         'F',                     // Fullwidth
         'H',                     // Halfwidth
         'W',                     // Wide
         'Na',                    // Narrow
         'A');                    // Ambiguous
      Width: array [Boolean,TEastAsianWidth] of Integer =
        ((1,                      // Neutral
          2,                      // Fullwidth
          1,                      // Halfwidth
          2,                      // Wide
          1,                      // Narrow
          1),                     // Ambiguous (same as Neutral)
         (1,                      // Neutral
          2,                      // Fullwidth
          1,                      // Halfwidth
          2,                      // Wide
          1,                      // Narrow
          2));                    // Ambiguous (same as Fullwidth)
  private
    class var
      FEastAsian: Boolean;
  public
    function AsName: String; overload; inline;
    class function FromSymbol(const S: String): TEastAsianWidth; static;
    function ToSymbol: String; overload; inline;
    function GetWidth(EastAsian: Boolean): Integer; overload; inline;
    function GetWidth: Integer; overload; inline;
    class function GetEastAsianWidth(C: UCS4Char): TEastAsianWidth; static;
    class property EastAsian: Boolean read FEastAsian write FEastAsian;
  end;

implementation

type
  { TPlaneData }
  TPlaneData = packed record
  public
    Data: array [0..65535] of Byte;
  end;
  PPlaneData = ^TPlaneData;

  { TPlane }
  TPlane = record
    PlaneDefault: Byte;
    PlaneData: PPlaneData;
  end;

const
{$I 'EastAsianWidth.inc'}

function TEastAsianWidthHelper.AsName: String;
begin
  Result := Names[Self];
end;

class function TEastAsianWidthHelper.FromSymbol(const S: String): TEastAsianWidth;
begin
  for Result := Low(TEastAsianWidth) to High(TEastAsianWidth) do
  begin
    if Symbols[Result] = S then
    begin
      Exit;
    end;
  end;
  raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
end;

function TEastAsianWidthHelper.ToSymbol: String;
begin
  Result := Symbols[Self];
end;

function TEastAsianWidthHelper.GetWidth(EastAsian: Boolean): Integer;
begin
  Result := Width[EastAsian,Self];
end;

function TEastAsianWidthHelper.GetWidth: Integer;
begin
  Result := Width[FEastAsian,Self];
end;

class function TEastAsianWidthHelper.GetEastAsianWidth(C: UCS4Char): TEastAsianWidth;
var
  PlaneNum: Integer;
  Plane: TPlane;
  B: Byte;
begin
  PlaneNum := (C shr 16) and $FFFF;
  if (PlaneNum < Low(Planes)) and (PlaneNum > High(Planes)) then
  begin
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  end;

  Plane := Planes[PlaneNum];
  if Plane.PlaneData <> nil then
  begin
    B := Plane.PlaneData^.Data[C and $FFFF];
  end
  else
  begin
    B := Plane.PlaneDefault;
  end;

  Result := TEastAsianWidth(B);
end;

end.
