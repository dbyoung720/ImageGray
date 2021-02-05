unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Diagnostics, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ExtDlgs;

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
    procedure mniFileOepnClick(Sender: TObject);
    procedure mniColorGrayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mniSizeActualClick(Sender: TObject);
    procedure mniSizeStrecthClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure mniFileRestoreClick(Sender: TObject);
    procedure mniColorInvertClick(Sender: TObject);
    procedure mniColorLightClick(Sender: TObject);
  private
    FstrBackFileName: String;
    procedure LoadImageProc(const strFileName: String; img: TImage);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses db.Image.Load, db.Image.Gray, db.Image.Invert, Unit2;

procedure TForm1.LoadImageProc(const strFileName: String; img: TImage);
begin
  with TStopwatch.StartNew do
  begin
    LoadImage(strFileName, imgShow);
    statTip.Panels[0].Text := Format('JPEG解码用时：%d 毫秒', [ElapsedMilliseconds]);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FstrBackFileName := ExtractFilePath(ParamStr(0)) + 'test.jpg';
  if not FileExists(FstrBackFileName) then
  begin
    FstrBackFileName := '';
    Exit;
  end;

  mniSizeActual.Checked  := False;
  mniSizeStrecth.Checked := True;
  imgShow.AutoSize       := False;
  imgShow.Stretch        := True;
  imgShow.Width          := Width - 20;
  imgShow.Height         := Height - 82;

  LoadImageProc(FstrBackFileName, imgShow);
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
    Gray(imgShow.Picture.Bitmap, gtSSE);
    statTip.Panels[0].Text := Format('灰值化用时：%d 毫秒', [ElapsedMilliseconds]);
  end;

  imgShow.Invalidate;
end;

procedure TForm1.mniColorInvertClick(Sender: TObject);
begin
  with TStopwatch.StartNew do
  begin
    Invert(imgShow.Picture.Bitmap, itAVX);
    statTip.Panels[0].Text := Format('反色用时：%d 毫秒', [ElapsedMilliseconds]);
  end;
  imgShow.Invalidate;
end;

procedure TForm1.mniColorLightClick(Sender: TObject);
begin
  ShowLightChange(Self, statTip.Panels[0], imgShow, TMenuItem(Sender).tag);
end;

end.
