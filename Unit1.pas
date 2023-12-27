unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.RegularExpressions, System.Skia,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Skia,
  System.Character;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    SkLabel1: TSkLabel;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    ScrollBox1: TScrollBox;
    SkLabel2: TSkLabel;
    CheckBox1: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Form1: TForm1;

implementation

uses
  Unicode.EastAsianWidth;

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
begin
  Edit1.Text := #$20BB7 + '野屋のコピペ' +
                #$00E5 +
                #$00E1 + #$0302 + #$0303 + #$0304 +
                #$1F62D +
                #$1F937 + #$1F3FD + #$200D + #$2640 + #$FE0F +
                #$1F468 + #$200D + #$1F469 + #$200D + #$1F467 + #$200D + #$1F466 +
                #$1F469 + #$1F3FD + #$200D + #$1F4BB +
                #$1F1EF + #$1F1F5;
end;

function Dump(const S: String): String;
begin
  Result := '';
  for var C in S do
  begin
    Result := Result + Format('U+%.4X ',[Ord(C)]);
  end;
  if Length(Result) > 0 then
  begin
    Delete(Result,Length(Result),1);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  S: String;
  L: Integer;
  TotalL: Integer;
  W: Integer;
  TotalW: Integer;
begin
  TEastAsianWidth.EastAsian := CheckBox1.Checked;
  SkLabel1.Words.Items[0].Caption := Edit1.Text;

  Memo1.Lines.Clear;
  SkLabel2.Words.Clear;

  TotalL := 0;
  TotalW := 0;
  for var Match in TRegEx.Matches(Edit1.Text,'\X') do
  begin
    S := Match.Value;
    L := Length(S);
    W := TEastAsianWidth.GetEastAsianWidth(Char.ConvertToUtf32(S,0)).GetWidth;
    Memo1.Lines.Add(S + Format(' (%s,L=%d,W=%d)',[Dump(S),L,W]));
    SkLabel2.Words.Add(S + sLineBreak);
    TotalL := TotalL + L;
    TotalW := TotalW + W;
  end;
  Memo1.Lines.Add(Format('Total: L=%d, W=%d',[TotalL,TotalW]));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  SkUnicode: ISkUnicode;
  L: Integer;
  TotalL: Integer;
  W: Integer;
  TotalW: Integer;
begin
  TEastAsianWidth.EastAsian := CheckBox1.Checked;
  SkLabel1.Words.Items[0].Caption := Edit1.Text;

  Memo1.Lines.Clear;
  SkLabel2.Words.Clear;

  TotalL := 0;
  TotalW := 0;
  SkUnicode := TSkUnicode.Create;
  for var S in SkUnicode.GetBreaks(Edit1.Text,TSkBreakType.Graphemes) do
  begin
    L := Length(S);
    W := TEastAsianWidth.GetEastAsianWidth(Char.ConvertToUtf32(S,0)).GetWidth;
    Memo1.Lines.Add(S + Format(' (%s,L=%d,W=%d)',[Dump(S),L,W]));
    SkLabel2.Words.Add(S + sLineBreak);
    TotalL := TotalL + L;
    TotalW := TotalW + W;
  end;
  Memo1.Lines.Add(Format('Total: L=%d, W=%d',[TotalL,TotalW]));
end;

end.
