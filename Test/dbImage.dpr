program dbImage;
{$IF CompilerVersion >= 21.0}
{$WEAKLINKRTTI ON}
{$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  {$IFDEF WIN32}
  db.Image.jpegdec in '..\src\db.Image.jpegdec.pas',
  {$IFEND }
  db.Image.Gray in '..\src\db.Image.Gray.pas',
  db.Image.Load in '..\src\db.Image.Load.pas',
  db.Image.Common in '..\src\db.Image.Common.pas',
  db.Image.Invert in '..\src\db.Image.Invert.pas',
  db.Image.Light in '..\src\db.Image.Light.pas',
  db.Image.Contrast in '..\src\db.Image.Contrast.pas',
  db.Image.Saturation in '..\src\db.Image.Saturation.pas';

{$R *.res}

begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown   := True;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
