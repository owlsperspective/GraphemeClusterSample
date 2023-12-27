unit Unit3;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Memo1: TMemo;
    Button1: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private 宣言 }
  public
    { Public 宣言 }
  end;

var
  Form3: TForm3;

implementation

uses
  UConvertEastAsianWidth;

{$R *.dfm}

procedure TForm3.FormShow(Sender: TObject);
begin
  Edit1.Text := ExpandFileName('.\..\..\EastAsianWidth.txt');
  Edit2.Text := ExpandFileName('.\..\..\EastAsianWidth.inc');
end;

procedure TForm3.Button1Click(Sender: TObject);
var
  Input: TStringList;
  Output: TStringList;
  Cursor: TCursor;
begin
  Input := nil;
  Output := nil;
  Cursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    Input := TStringList.Create;
    Output := TStringList.Create;

    Input.LoadFromFile(Edit1.Text,TEncoding.UTF8);

    ConvertEastAsianWidth(Input,Output);

    Memo1.Lines.AddStrings(Output);
    Output.SaveToFile(Edit2.Text,TEncoding.ASCII);

  finally
    Screen.Cursor := Cursor;
    Input.Free;
    Output.Free;
  end;
end;

end.
