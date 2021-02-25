unit db.Image.Effect;
{
  Func: 32位位图效果图
  Name: dbyoung@sina.com
  Date: 2021-2-25
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；
  Note: 并行程序，不能在 IDE 下运行查看效果。必须脱离 IDE 执行查看效果。
}

interface

uses Winapi.Windows, System.Threading, System.Math, Vcl.Graphics, db.Image.Common;

{ 曝光 }
procedure Exposure(bmp: TBitmap);

{ 浮雕 }
procedure Emboss(bmp: TBitmap);

{ 雕刻 }
procedure Engrave(bmp: TBitmap);

{ 模糊 }
procedure Blur(bmp: TBitmap);

implementation

{ 曝光 }
procedure Exposure(bmp: TBitmap);
var
  X, Y  : Integer;
  pColor: PRGBQuad;
begin
  for Y := 0 to bmp.Height - 1 do
  begin
    pColor := bmp.ScanLine[Y];
    for X  := 0 to bmp.Width - 1 do
    begin
      pColor^.rgbBlue  := Ifthen(pColor^.rgbBlue < 128, not pColor^.rgbBlue, pColor^.rgbBlue);
      pColor^.rgbGreen := Ifthen(pColor^.rgbGreen < 128, not pColor^.rgbGreen, pColor^.rgbGreen);
      pColor^.rgbRed   := Ifthen(pColor^.rgbRed < 128, not pColor^.rgbRed, pColor^.rgbRed);
      Inc(pColor);
    end;
  end;
end;

{ 浮雕 }
procedure Emboss(bmp: TBitmap);
var
  X, Y      : Integer;
  SrcRow    : PRGBQuad;
  SrcNextRow: PRGBQuad;
begin
  for Y := 0 to bmp.Height - 2 do
  begin
    SrcRow     := bmp.ScanLine[Y + 0];
    SrcNextRow := bmp.ScanLine[Y + 1];
    for X      := 0 to bmp.Width - 1 do
    begin
      Inc(SrcNextRow);
      SrcRow^.rgbRed   := EnsureRange(SrcRow^.rgbRed - SrcNextRow^.rgbRed + 128, 0, 255);
      SrcRow^.rgbGreen := EnsureRange(SrcRow^.rgbGreen - SrcNextRow^.rgbGreen + 128, 0, 255);
      SrcRow^.rgbBlue  := EnsureRange(SrcRow^.rgbBlue - SrcNextRow^.rgbBlue + 128, 0, 255);
      Inc(SrcRow);
    end;
  end;
end;

{ 雕刻 }
procedure Engrave(bmp: TBitmap);
var
  X, Y      : Integer;
  SrcRow    : PRGBQuad;
  SrcNextRow: PRGBQuad;
begin
  for Y := 0 to bmp.Height - 2 do
  begin
    SrcRow     := bmp.ScanLine[Y + 0];
    SrcNextRow := bmp.ScanLine[Y + 1];
    for X      := 0 to bmp.Width - 1 do
    begin
      Inc(SrcNextRow);
      SrcRow^.rgbRed   := EnsureRange(SrcNextRow^.rgbRed - SrcRow^.rgbRed + 128, 0, 255);
      SrcRow^.rgbGreen := EnsureRange(SrcNextRow^.rgbGreen - SrcRow^.rgbGreen + 128, 0, 255);
      SrcRow^.rgbBlue  := EnsureRange(SrcNextRow^.rgbBlue - SrcRow^.rgbBlue + 128, 0, 255);
      Inc(SrcRow);
    end;
  end;
end;

{ 模糊 }
procedure Blur(bmp: TBitmap);
var
  X, Y                  : Integer;
  pColorPre             : PRGBQuad;
  pColorOne             : PRGBQuad;
  pColorTwo             : PRGBQuad;
  ValueR, ValueG, ValueB: Integer;
begin
  for Y := 1 to bmp.Height - 2 do
  begin
    pColorOne := bmp.ScanLine[Y + 0];
    pColorTwo := bmp.ScanLine[Y + 1];
    pColorPre := bmp.ScanLine[Y - 1];
    for X     := 1 to bmp.Width - 2 do
    begin
      ValueR := pColorPre^.rgbRed + pColorOne^.rgbRed + pColorTwo^.rgbRed;
      ValueG := pColorPre^.rgbGreen + pColorOne^.rgbGreen + pColorTwo^.rgbGreen;
      ValueB := pColorPre^.rgbBlue + pColorOne^.rgbBlue + pColorTwo^.rgbBlue;

      Inc(pColorPre);
      Inc(pColorOne);
      Inc(pColorTwo);
      ValueR := ValueR + pColorPre^.rgbRed + pColorOne^.rgbRed + pColorTwo^.rgbRed;
      ValueG := ValueG + pColorPre^.rgbGreen + pColorOne^.rgbGreen + pColorTwo^.rgbGreen;
      ValueB := ValueB + pColorPre^.rgbBlue + pColorOne^.rgbBlue + pColorTwo^.rgbBlue;

      Inc(pColorPre);
      Inc(pColorOne);
      Inc(pColorTwo);
      ValueR := ValueR + pColorPre^.rgbRed + pColorOne^.rgbRed + pColorTwo^.rgbRed;
      ValueG := ValueG + pColorPre^.rgbGreen + pColorOne^.rgbGreen + pColorTwo^.rgbGreen;
      ValueB := ValueB + pColorPre^.rgbBlue + pColorOne^.rgbBlue + pColorTwo^.rgbBlue;

      pColorOne^.rgbRed   := ValueR div 9;
      pColorOne^.rgbGreen := ValueG div 9;
      pColorOne^.rgbBlue  := ValueB div 9;

      Dec(pColorPre);
      Dec(pColorOne);
      Dec(pColorTwo);
    end;
  end;
end;

end.
