unit UConvertEastAsianWidth;

interface

uses
  System.Classes, System.SysUtils, System.RegularExpressions;

procedure ConvertEastAsianWidth(Input, Output: TStrings);

implementation

uses
  Unicode.EastAsianWidth;

type
  { TPlaneData }
  TPlaneData = record
    Table: array [$0000..$FFFF] of Byte;
    procedure Init;
    procedure Fill(StartIndex: UInt16; EndIndex: UInt16; Data: Byte);
    function  IsAllSame: Boolean;
  end;

  { TEastAsianWidthProperties }
  TEastAsianWidthProperties = record
    Planes: array [0..16] of TPlaneData;
    procedure Init;
  end;
  PEastAsianWidthProperties = ^TEastAsianWidthProperties;

procedure TPlaneData.Init;
var
  I: Integer;
begin
  for I := Low(Table) to High(Table) do
  begin
    Table[I] := 0;  // 'Neutral'
  end;
end;

procedure TPlaneData.Fill(StartIndex: UInt16; EndIndex: UInt16; Data: Byte);
var
  I: Integer;
begin
  for I := StartIndex to EndIndex do
  begin
    Table[I] := Data;
  end;
end;

function TPlaneData.IsAllSame: Boolean;
var
  I: Integer;
  B: Byte;
begin
  B := Table[Low(Table)];
  for I := Low(Table) + 1 to High(Table) do
  begin
    if Table[I] <> B then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TEastAsianWidthProperties.Init;
var
  I: Integer;
begin
  for I := Low(Planes) to High(Planes) do
  begin
    Planes[I].Init;
  end;
end;

procedure ConvertEastAsianWidth(Input, Output: TStrings);
var
  PEAWP: PEastAsianWidthProperties;
  RegEx1: TRegEx;
  RegEx2: TRegEx;
  Match: TMatch;
  I: Integer;
  S: String;
  Position: Integer;
  StartCodePoint: String;
  EndCodePoint: String;
  Symbol: String;
  Plane: UInt16;
  CPLow: UInt16;
  CPLow2: UInt16;
  PlaneDefault: Byte;
begin
  New(PEAWP);
  try
    PEAWP^.Init;

    RegEx1 := TRegEx.Create('([0-9A-Fa-f]{4,6})[.][.]([0-9A-Fa-f]{4,6})\s*[;]\s*(.{1,2})\s*');
    RegEx2 := TRegEx.Create('([0-9A-Fa-f]{4,6})\s*[;]\s*(.{1,2})\s*');

    S := Input.Strings[0];
    Position := Pos('#',S);
    if Position > 0 then
    begin
      Delete(S,1,Position);
    end;
    S := Trim(S);

    Output.Add('  { Unicode east asian width property database }');
    Output.Add(Format('  { Generated from %s }',[S]));
    Output.Add('');

    for I := 0 to Input.Count - 1 do
    begin
      S := Input.Strings[I];

      Position := Pos('#',S);
      if Position > 0 then
      begin
        Delete(S,Position,Length(S));
      end;
      S := Trim(S);

      if S = '' then
      begin
        Continue;
      end;

      Match := RegEx1.Match(S);
      if Match.Success = True then
      begin
        StartCodePoint := Match.Groups[1].Value;
        EndCodePoint := Match.Groups[2].Value;
        Symbol := Match.Groups[3].Value;
      end
      else
      begin
        Match := RegEx2.Match(S);
        if Match.Success = True then
        begin
          StartCodePoint := Match.Groups[1].Value;
          EndCodePoint := StartCodePoint;
          Symbol := Match.Groups[2].Value;
        end;
      end;

      if Match.Success = True then
      begin
        Plane := (StrToInt64('$' + StartCodePoint)  shr 16) and $FFFF;
        if Plane <= 16 then
        begin
          CPLow  := (StrToInt64('$' + StartCodePoint)) and $FFFF;
          CPLow2 := (StrToInt64('$' + EndCodePoint))   and $FFFF;
          PEAWP^.Planes[Plane].Fill(CPLow,CPLow2,Ord(TEastAsianWidth.FromSymbol(Symbol)));
        end;
      end;
    end;

    for Plane := Low(PEAWP^.Planes) to High(PEAWP^.Planes) do
    begin
      Output.Add(Format('  { Plane%d }',[Plane]));
      if PEAWP^.Planes[Plane].IsAllSame = False then
      begin
        Output.Add(Format('  Plane%d: TPlaneData = (Data: (',[Plane]));
        S := '';
        for I := Low(PEAWP^.Planes[Plane].Table) to High(PEAWP^.Planes[Plane].Table) do
        begin
          if (I mod 16) = 0 then
          begin
            S := '    ';
          end;

          S := S + Format('$%.02X,',[PEAWP^.Planes[Plane].Table[I]]);

          if (I mod 16) = 15 then
          begin
            if I = High(PEAWP^.Planes[Plane].Table) then
            begin
              Delete(S,Length(S),1);
              S := S + '));';
            end
            else
            begin
              S := S + '  ';
            end;
            S := S + Format('  // U+%.4X',[(Plane shl 16) or (I and $FFF0)]);
            Output.Add(S);
            S := '';
          end;
        end;
        PlaneDefault := 0;
      end
      else
      begin
        PlaneDefault := PEAWP^.Planes[Plane].Table[0];
      end;
      Output.Add(Format('  Plane%dDefault = $%.2X;',[Plane,PlaneDefault]));
      Output.Add('');
    end;

    Output.Add('  { Plane data table }');
    Output.Add('  Planes: array [0..16] of TPlane =');
    for Plane := Low(PEAWP^.Planes) to High(PEAWP^.Planes) do
    begin
      if PEAWP^.Planes[Plane].IsAllSame = True then
      begin
        S := Format('     (PlaneDefault: Plane%dDefault; PlaneData: nil),',[Plane]);
      end
      else
      begin
        S := Format('     (PlaneDefault: Plane%dDefault; PlaneData: @Plane%d),',[Plane,Plane]);
      end;

      if Plane = Low(PEAWP^.Planes) then
      begin
        S[5] := '(';
      end
      else if Plane = High(PEAWP^.Planes) then
      begin
        Delete(S,Length(S),1);
        S := S + ');';
      end;
      Output.Add(S);
    end;
    Output.Add('');

  finally
    Dispose(PEAWP);
  end;
end;

end.
