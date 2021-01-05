program dbImage;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1} ,
{$IFDEF WIN32}
  db.Image.jpegdec in '..\src\db.Image.jpegdec.pas',
{$IFEND}
  db.Image.Gray in '..\src\db.Image.Gray.pas',
  db.Image.Load in '..\src\db.Image.Load.pas',
  db.Image.Common in '..\src\db.Image.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
