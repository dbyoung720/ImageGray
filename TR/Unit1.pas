unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Diagnostics;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    FAngle    : Integer;
    FTotalTime: Integer;
    FbmpSrc   : TBitmap;
    procedure MyOnMessage(var Msg: TMsg; var Handled: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
{$R test.res}

uses db.Image.Load, db.Image.Rotate;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  PostMessage(Handle, WM_QUIT, 0, 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Position          := poScreenCenter;
  WindowState       := wsMaximized;
  Application.Title := Caption;
  DoubleBuffered    := True;
  FAngle            := 0;
  FTotalTime        := 0;
  FbmpSrc           := TBitmap.Create;
  FbmpSrc.LoadFromResourceName(HInstance, 'TEST');
  FbmpSrc.PixelFormat   := pf32bit;
  Application.OnMessage := MyOnMessage;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FbmpSrc.Free;
end;

procedure TForm1.MyOnMessage(var Msg: TMsg; var Handled: Boolean);
var
  DTime : Cardinal;
  bmpDst: TBitmap;
begin
  while (Msg.message <> WM_QUIT) do
  begin
    if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end
    else
    begin
      bmpDst := TBitmap.Create;
      try
        with TStopWatch.StartNew do
        begin
          Inc(FAngle, 1);                                                                                                                   // 角度加1
          Rotate(FbmpSrc, bmpDst, FAngle);                                                                                                  // 旋转图像
          DTime := ElapsedMilliseconds;                                                                                                     // 旋转图片用时
          Inc(FTotalTime, DTime);                                                                                                           // 旋转用时总时间
          Caption := Format('图像：800X600；旋转角度：%0.3d  耗时：%0.3d 毫秒   帧率：%0.3f 帧/秒', [FAngle mod 360, DTime, PI * 1000 * FAngle / FTotalTime]); // 显示信息
          Canvas.StretchDraw(Canvas.ClipRect, bmpDst);                                                                                      // 绘制图像
        end;

        { 退出 }
        if ((Msg.message = WM_KEYDOWN) or (Msg.message = WM_KEYUP)) and (Msg.wParam = VK_ESCAPE) then
          Close;
      finally
        bmpDst.Free;
      end;
    end;
  end;
end;

end.
