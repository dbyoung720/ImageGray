unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, db.Image.Common;

type
  TForm2 = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    trckbrLight: TTrackBar;
    procedure trckbrLightChange(Sender: TObject);
  private
    Fimg   : TImage;
    FsrcBmp: TBitmap;
    FbakBmp: TBitmap;
  public
    { Public declarations }
  end;

procedure ShowLightChange(frm: TForm; img: TImage);

implementation

{$R *.dfm}

procedure ShowLightChange(frm: TForm; img: TImage);
begin
  with TForm2.Create(nil) do
  begin
    Fimg    := img;
    FsrcBmp := img.Picture.Bitmap;

    FbakBmp             := TBitmap.Create;
    FbakBmp.PixelFormat := pf32bit;
    FbakBmp.Width       := img.Picture.Bitmap.Width;
    FbakBmp.Height      := img.Picture.Bitmap.Height;
    FbakBmp.Assign(img.Picture.Bitmap);

    Position := poDesigned;
    Left     := frm.Left + frm.Width - Width - 10;
    Top      := frm.Top + 55;
    ShowModal;
    FbakBmp.Free;
    Free;
  end;
end;

function CheckValue(color: Byte; value: Integer): Byte;
begin
  if color + value > 255 then
    Result := 255
  else if color + value < 0 then
    Result := 0
  else
    Result := color + value;
end;

procedure TForm2.trckbrLightChange(Sender: TObject);
var
  intLightValue: Integer;
  I, J         : Integer;
  pColor       : PRGBQuad;
begin
  FsrcBmp.Assign(FbakBmp);

  intLightValue := trckbrLight.Position;
  lbl4.Caption  := Format('%.3d', [intLightValue]);

  for I := 0 to FsrcBmp.Height - 1 do
  begin
    pColor := FsrcBmp.ScanLine[I];
    for J  := 0 to FsrcBmp.Width - 1 do
    begin
      pColor^.rgbRed   := CheckValue(pColor^.rgbRed, intLightValue);
      pColor^.rgbGreen := CheckValue(pColor^.rgbGreen, intLightValue);
      pColor^.rgbBlue  := CheckValue(pColor^.rgbBlue, intLightValue);
      Inc(pColor);
    end;
  end;

  Fimg.Invalidate;
end;

end.
