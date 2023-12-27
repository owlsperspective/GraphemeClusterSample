program EastAsianWidthConverter;

uses
  Vcl.Forms,
  Unit3 in 'Unit3.pas' {Form3},
  UConvertEastAsianWidth in 'UConvertEastAsianWidth.pas',
  Unicode.EastAsianWidth in 'Unicode.EastAsianWidth.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
