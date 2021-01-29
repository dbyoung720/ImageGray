unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Math, System.Diagnostics, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, db.Image.Common;

type
  TForm2 = class(TForm)
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    trckbrLight: TTrackBar;
    lbl5: TLabel;
    trckbrContrast: TTrackBar;
    lbl6: TLabel;
    trckbrSaturation: TTrackBar;
    procedure trckbrLightChange(Sender: TObject);
    procedure trckbrContrastChange(Sender: TObject);
    procedure trckbrSaturationChange(Sender: TObject);
  private
    Fimg   : TImage;
    Fstp   : TStatusPanel;
    FsrcBmp: TBitmap;
    FbakBmp: TBitmap;
  public
    { Public declarations }
  end;

procedure ShowLightChange(frm: TForm; stp: TStatusPanel; img: TImage; const mniItemTag: Integer = 0);

implementation

{$R *.dfm}

uses db.Image.Light, db.Image.Contrast, db.Image.Saturation;

procedure ShowLightChange(frm: TForm; stp: TStatusPanel; img: TImage; const mniItemTag: Integer = 0);
begin
  with TForm2.Create(nil) do
  begin
    Fimg    := img;
    Fstp    := stp;
    FsrcBmp := img.Picture.Bitmap;

    if mniItemTag = 0 then
      Winapi.Windows.SetFocus(trckbrLight.Handle)
    else if mniItemTag = 1 then
      Winapi.Windows.SetFocus(trckbrContrast.Handle)
    else if mniItemTag = 2 then
      Winapi.Windows.SetFocus(trckbrSaturation.Handle);

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

procedure TForm2.trckbrLightChange(Sender: TObject);
begin
  lbl4.Caption := Format('%d', [trckbrLight.Position]);

  with TStopwatch.StartNew do
  begin
    FsrcBmp.Canvas.Draw(0, 0, FbakBmp);
    Light(FsrcBmp, trckbrLight.Position, ltASM);
    Fstp.Text := Format('调节亮度用时：%d 毫秒', [ElapsedMilliseconds]);
  end;

  Fimg.Invalidate;
end;

procedure TForm2.trckbrContrastChange(Sender: TObject);
begin
  lbl5.Caption := Format('%d', [trckbrContrast.Position]);

  with TStopwatch.StartNew do
  begin
    FsrcBmp.Canvas.Draw(0, 0, FbakBmp);
    Contrast(FsrcBmp, trckbrContrast.Position + 128, ctSSE4);
    Fstp.Text := Format('调节对比度用时：%d 毫秒', [ElapsedMilliseconds]);
  end;

  Fimg.Invalidate;
end;

procedure TForm2.trckbrSaturationChange(Sender: TObject);
begin
  lbl6.Caption := Format('%d', [trckbrSaturation.Position]);

  with TStopwatch.StartNew do
  begin
    FsrcBmp.Canvas.Draw(0, 0, FbakBmp);
    Saturation(FsrcBmp, trckbrSaturation.Position + 255, stAVX2);
    Fstp.Text := Format('调节饱和度用时：%d 毫秒', [ElapsedMilliseconds]);
  end;

  Fimg.Invalidate;
end;

end.
