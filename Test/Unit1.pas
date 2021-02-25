unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Diagnostics, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ExtDlgs, Vcl.StdCtrls,
  db.Image.Common;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mniFileOepn: TMenuItem;
    scrlbx1: TScrollBox;
    statTip: TStatusBar;
    imgShow: TImage;
    dlgOpenPic: TOpenPictureDialog;
    mniColor: TMenuItem;
    mniColorGray: TMenuItem;
    mniSize: TMenuItem;
    mniSizeActual: TMenuItem;
    mniSizeStrecth: TMenuItem;
    mniFileLine01: TMenuItem;
    mniFileRestore: TMenuItem;
    mniColorInvert: TMenuItem;
    mniColorLight: TMenuItem;
    mniColorContrast: TMenuItem;
    mniColorLine01: TMenuItem;
    mniColorSaturation: TMenuItem;
    mniColorMode: TMenuItem;
    mniEffect: TMenuItem;
    mniEffectExposure: TMenuItem;
    mniEffectEmboss: TMenuItem;
    mniEffectEngrave: TMenuItem;
    mniEffectBlur: TMenuItem;
    mniEffectSharpen: TMenuItem;
    mniEffectSponge: TMenuItem;
    mniEffectSand: TMenuItem;
    mniEffectDitherBmp: TMenuItem;
    mniGeometry: TMenuItem;
    mniGeometryHMirror: TMenuItem;
    mniGeometryVMirror: TMenuItem;
    mniGeometryRotate: TMenuItem;
    mniGeometryHVMirror: TMenuItem;
    procedure mniFileOepnClick(Sender: TObject);
    procedure mniColorGrayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mniSizeActualClick(Sender: TObject);
    procedure mniSizeStrecthClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mniFileRestoreClick(Sender: TObject);
    procedure mniColorInvertClick(Sender: TObject);
    procedure mniColorLightClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure mniEffectExposureClick(Sender: TObject);
  private
    FstrBackFileName : string;
    FbmpBackup       : TBitmap;
    FTrackColorChange: TTrackBar;
    FlblLightValue   : TLabel;
    procedure BackupBmp;
    procedure LoadImageProc(const strFileName: string; img: TImage);
    procedure OnColorChange(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
    procedure Effect(Proc: TProc<TBitmap>; const strTip: String);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  db.Image.Load, db.Image.Gray, db.Image.Invert, db.Image.Light, db.Image.Contrast, db.Image.Saturation, db.Image.ColorMap, db.Image.Effect;

procedure TForm1.LoadImageProc(const strFileName: string; img: TImage);
begin
  with TStopwatch.StartNew do
  begin
    LoadImage(strFileName, imgShow);
    statTip.Panels[0].Text := Format('JPEG解码用时：%d 毫秒', [ElapsedMilliseconds]);
    BackupBmp;
  end;
end;

procedure TForm1.BackupBmp;
begin
  FbmpBackup.Width  := imgShow.Picture.Bitmap.Width;
  FbmpBackup.Height := imgShow.Picture.Bitmap.Height;
  FbmpBackup.Canvas.Draw(0, 0, imgShow.Picture.Bitmap);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FstrBackFileName := ExtractFilePath(ParamStr(0)) + 'test.jpg';
  if not FileExists(FstrBackFileName) then
  begin
    FstrBackFileName := '';
    Exit;
  end;

  FbmpBackup             := TBitmap.Create;
  FbmpBackup.PixelFormat := pf32bit;

  mniSizeActual.Checked  := False;
  mniSizeStrecth.Checked := True;
  imgShow.AutoSize       := False;
  imgShow.Stretch        := True;
  imgShow.Width          := Width - 20;
  imgShow.Height         := Height - 82;
  LoadImageProc(FstrBackFileName, imgShow);

  PostMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FbmpBackup.free;
end;

procedure TForm1.mniFileOepnClick(Sender: TObject);
begin
  if not dlgOpenPic.Execute then
    Exit;

  FstrBackFileName := dlgOpenPic.FileName;
  LoadImageProc(FstrBackFileName, imgShow);
end;

procedure TForm1.mniFileRestoreClick(Sender: TObject);
begin
  if not FileExists(FstrBackFileName) then
    Exit;

  LoadImageProc(FstrBackFileName, imgShow);
end;

procedure TForm1.mniSizeActualClick(Sender: TObject);
begin
  mniSizeActual.Checked  := True;
  mniSizeStrecth.Checked := False;
  imgShow.AutoSize       := True;
  imgShow.Stretch        := False;
end;

procedure TForm1.mniSizeStrecthClick(Sender: TObject);
begin
  mniSizeActual.Checked  := False;
  mniSizeStrecth.Checked := True;
  imgShow.AutoSize       := False;
  imgShow.Stretch        := True;
  imgShow.Width          := Width - 20;
  imgShow.Height         := Height - 82;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  if imgShow.Stretch then
  begin
    imgShow.Width  := Width - 20;
    imgShow.Height := Height - 82;
  end;
end;

procedure TForm1.mniColorGrayClick(Sender: TObject);
begin
  with TStopwatch.StartNew do
  begin
    Gray(imgShow.Picture.Bitmap, gtSSEParallel);
    statTip.Panels[0].Text := Format('灰值化用时：%d 毫秒', [ElapsedMilliseconds]);
  end;

  imgShow.Invalidate;
end;

procedure TForm1.mniColorInvertClick(Sender: TObject);
begin
  with TStopwatch.StartNew do
  begin
    Invert(imgShow.Picture.Bitmap, itAVX1);
    statTip.Panels[0].Text := Format('反色用时：%d 毫秒', [ElapsedMilliseconds]);
  end;
  imgShow.Invalidate;
end;

procedure TForm1.OnColorChange(Sender: TObject);
var
  bmp     : TBitmap;
  ccChange: TColorChange;
begin
  FTrackColorChange      := TTrackBar(Sender);
  ccChange               := TColorChange(FTrackColorChange.Tag);
  FlblLightValue.Caption := InttoStr(FTrackColorChange.Position);

  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.Width       := FbmpBackup.Width;
    bmp.Height      := FbmpBackup.Height;
    bmp.Canvas.Draw(0, 0, FbmpBackup);

    with TStopwatch.StartNew do
    begin
      if ccChange = ccLight then
        Light(bmp, FTrackColorChange.Position, ltSSEParallel)            // 调节亮度
      else if ccChange = ccContrast then                                 //
        Contrast(bmp, FTrackColorChange.Position + 128, ctSSEParallel)   // 调节对比度
      else if ccChange = ccSaturation then                               //
        Saturation(bmp, FTrackColorChange.Position + 255, stSSEParallel) // 调节饱和度
      else if ccChange = ccColorMode then                                //
        ColorMap(bmp, FTrackColorChange.Position, cmtParallel);          // 调节色彩

      statTip.Panels[0].Text := Format(c_strShowTime[Integer(ccChange)], [ElapsedMilliseconds]);
    end;

    imgShow.Picture.Bitmap.Assign(bmp);
  finally
    bmp.free;
  end;
end;

procedure TForm1.OnResetClick(Sender: TObject);
begin
  if FTrackColorChange <> nil then
    FTrackColorChange.Position := 0;
end;

procedure TForm1.OnCancelClick(Sender: TObject);
begin
  // if FTrackColorChange <> nil then
  // FTrackColorChange.Position := 0;
  imgShow.Picture.Bitmap.Canvas.Draw(0, 0, FbmpBackup);
  TForm(TButton(Sender).Parent).Close;
end;

procedure TForm1.OnOKClick(Sender: TObject);
begin
  BackupBmp;
  TForm(TButton(Sender).Parent).Close;
end;

procedure TForm1.mniColorLightClick(Sender: TObject);
var
  intTag: Integer;
begin
  intTag := TMenuItem(Sender).Tag;
  ShowColorChange(Form1, TColorChange(intTag), OnColorChange, OnResetClick, OnCancelClick, OnOKClick, FlblLightValue, c_intMinMaxValue[intTag, 0], c_intMinMaxValue[intTag, 1], c_strShowTips[intTag, 0], c_strShowTips[intTag, 1]);
end;

procedure TForm1.Effect(Proc: TProc<TBitmap>; const strTip: String);
begin
  with TStopwatch.StartNew do
  begin
    Proc(imgShow.Picture.Bitmap);
    statTip.Panels[0].Text := Format(strTip, [ElapsedMilliseconds]);
  end;
  imgShow.Invalidate;
end;

procedure TForm1.mniEffectExposureClick(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    0:
      Effect(Exposure, '曝光效果用时：%d 毫秒');
    1:
      Effect(Emboss, '浮雕效果用时：%d 毫秒');
    2:
      Effect(Engrave, '雕刻效果用时：%d 毫秒');
    3:
      Effect(Blur, '模糊效果用时：%d 毫秒');
  end;
end;

end.
