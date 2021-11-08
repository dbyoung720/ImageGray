unit db.Image.Effect;
{
  Func: 32位位图效果图
  Name: dbyoung@sina.com
  Date: 2021-2-25
  Vers: Delphi 11
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；
  Note: 并行程序，不能在 IDE 下运行查看效果。必须脱离 IDE 执行查看效果。

  Delphi 参数寄存器顺序：
  X86: EAX, EDX, ECX
  X64: ECX, EDX, EAX

  通用寄存器：
  CPU  :
  EAX/EBX/ECX/EDX/EDI/ESI           32位 (x86)
  RAX/RBX/RCX/RDX/RDI/RSI           64位 (x64, EAX 寄存器是 RAX 寄存器的低 32 位)

  SIMD寄存器：
  MMX    :   MM0 --- MM7    064位                                         ( 主要针对浮点运算 )
  SSE2   :  XMM0--- XMM7    128位                                         ( 浮点 + 整数 )
  SSE4   :  XMM0---XMM15    128位                                         ( 浮点 + 整数 )
  AVX    :  YMM0---YMM15    256位 (XMM 寄存器是 YMM 寄存器的低 128 位)    ( 浮点 )
  AVX2   :  YMM0---YMM15    256位 (XMM 寄存器是 YMM 寄存器的低 128 位)    ( 浮点 + 整数 )
  AVX512 :  ZMM0---ZMM31    512位 (YMM 寄存器是 ZMM 寄存器的低 256 位)    ( 浮点 + 整数 )
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

{ 锐化 }
procedure Sharpen(bmp: TBitmap);

{ 油画 }
procedure Sponge(ABmp: TBitmap);

implementation

uses Forms;

{ 曝光 }
procedure Exposure(bmp: TBitmap);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      X: Integer;
      pColor: PRGBQuad;
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      for X := 0 to bmp.Width - 1 do
      begin
        pColor^.rgbBlue := Ifthen(pColor^.rgbBlue < 128, not pColor^.rgbBlue, pColor^.rgbBlue);
        pColor^.rgbGreen := Ifthen(pColor^.rgbGreen < 128, not pColor^.rgbGreen, pColor^.rgbGreen);
        pColor^.rgbRed := Ifthen(pColor^.rgbRed < 128, not pColor^.rgbRed, pColor^.rgbRed);
        Inc(pColor);
      end;
    end);
end;

{ 浮雕 }
procedure Emboss(bmp: TBitmap);
// var
// X, Y    : Integer;
// pColor01: PRGBQuad;
// pColor02: PRGBQuad;
// begin
// for Y := 0 to bmp.Height - 2 do
// begin
// pColor01 := bmp.ScanLine[Y + 0];
// pColor02 := bmp.ScanLine[Y + 1];
// for X    := 0 to bmp.Width - 1 do
// begin
// Inc(pColor02);
// pColor01^.rgbRed   := EnsureRange(pColor01^.rgbRed - pColor02^.rgbRed + 128, 0, 255);
// pColor01^.rgbGreen := EnsureRange(pColor01^.rgbGreen - pColor02^.rgbGreen + 128, 0, 255);
// pColor01^.rgbBlue  := EnsureRange(pColor01^.rgbBlue - pColor02^.rgbBlue + 128, 0, 255);
// Inc(pColor01);
// end;
// end;
// end;
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 2,
    procedure(Y: Integer)
    var
      X: Integer;
      pColor01: PRGBQuad;
      pColor02: PRGBQuad;
    begin
      pColor01 := PRGBQuad(StartScanLine + (Y + 0) * bmpWidthBytes);
      pColor02 := PRGBQuad(StartScanLine + (Y + 1) * bmpWidthBytes);
      for X := 0 to bmp.Width - 1 do
      begin
        Inc(pColor02);
        pColor01^.rgbRed := EnsureRange(pColor01^.rgbRed - pColor02^.rgbRed + 128, 0, 255);
        pColor01^.rgbGreen := EnsureRange(pColor01^.rgbGreen - pColor02^.rgbGreen + 128, 0, 255);
        pColor01^.rgbBlue := EnsureRange(pColor01^.rgbBlue - pColor02^.rgbBlue + 128, 0, 255);
        Inc(pColor01);
      end;
    end);
end;

{ 雕刻 }
procedure Engrave(bmp: TBitmap);
var
  X, Y      : Integer;
  pColor01  : PRGBQuad;
  SrcNextRow: PRGBQuad;
begin
  for Y := 0 to bmp.Height - 2 do
  begin
    pColor01   := bmp.ScanLine[Y + 0];
    SrcNextRow := bmp.ScanLine[Y + 1];
    for X      := 0 to bmp.Width - 1 do
    begin
      Inc(SrcNextRow);
      pColor01^.rgbRed   := EnsureRange(SrcNextRow^.rgbRed - pColor01^.rgbRed + 128, 0, 255);
      pColor01^.rgbGreen := EnsureRange(SrcNextRow^.rgbGreen - pColor01^.rgbGreen + 128, 0, 255);
      pColor01^.rgbBlue  := EnsureRange(SrcNextRow^.rgbBlue - pColor01^.rgbBlue + 128, 0, 255);
      Inc(pColor01);
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

{ 锐化 }
procedure Sharpen(bmp: TBitmap);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(1, bmp.Height - 2,
    procedure(Y: Integer)
    var
      X: Integer;
      pColor01: PRGBQuad;
      pColor02: PRGBQuad;
    begin
      pColor01 := PRGBQuad(StartScanLine + (Y - 0) * bmpWidthBytes);
      pColor02 := PRGBQuad(StartScanLine + (Y - 1) * bmpWidthBytes);
      for X := 0 to bmp.Width - 1 do
      begin
        Dec(pColor02);
        pColor01^.rgbRed := EnsureRange(pColor01^.rgbRed + (pColor01^.rgbRed - pColor02^.rgbRed) div 2, 0, 255);
        pColor01^.rgbGreen := EnsureRange(pColor01^.rgbGreen + (pColor01^.rgbGreen - pColor02^.rgbGreen) div 2, 0, 255);
        pColor01^.rgbBlue := EnsureRange(pColor01^.rgbBlue + (pColor01^.rgbBlue - pColor02^.rgbBlue) div 2, 0, 255);
        Inc(pColor01);
        Inc(pColor02, 2);
      end;
    end);
end;

type
  pRGBArray  = ^TRGBArray;
  PbyteArray = ^TByteArray;
  TRGBArray  = array [0 .. 32768 - 1] of TRGBQuad;
  TByteArray = array [0 .. 16777215] of Byte;

  { 油画 }
procedure Sponge(ABmp: TBitmap);
var
  I, J, X, Y, R: Integer;
begin
  for I   := 0 to ABmp.Height - 1 do
    for J := 0 to ABmp.Width - 1 do
    begin
      Application.ProcessMessages;
      Randomize;
      R                                       := Random(128);
      X                                       := EnsureRange(J + (R - Random(R * 2)), 0, ABmp.Width - 1);
      Y                                       := EnsureRange(I + (R - Random(R * 2)), 0, ABmp.Height - 1);
      PbyteArray(ABmp.ScanLine[I])[J * 4 + 0] := PbyteArray(ABmp.ScanLine[Y])[X * 4 + 0];
      PbyteArray(ABmp.ScanLine[I])[J * 4 + 1] := PbyteArray(ABmp.ScanLine[Y])[X * 4 + 1];
      PbyteArray(ABmp.ScanLine[I])[J * 4 + 2] := PbyteArray(ABmp.ScanLine[Y])[X * 4 + 2];
    end;
end;

end.
