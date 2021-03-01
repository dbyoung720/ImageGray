unit db.Image.GeometricTrans;
{
  Func: 32位位图几何变换
  Name: dbyoung@sina.com
  Date: 2021-2-22
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；
  Note: 并行程序，不能在 IDE 下运行查看效果。必须脱离 IDE 执行查看效果。
}

interface

uses Winapi.Windows, System.Threading, System.Classes, System.Math, Vcl.Graphics, db.Image.Common;

{ 水平翻转 并行模式，需要脱离 IDE 执行 }
procedure HorizMirror(bmp: TBitmap);

{ 垂直翻转 }
procedure VertiMirror(bmp: TBitmap);

{ 转置翻转 并行模式，需要脱离 IDE 执行 }
procedure HAndVMirror(bmp: TBitmap);

{ 旋转 }
procedure Rotate_Scanline(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
procedure Rotate_Parallel(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);

implementation

{ 水平翻转 并行模式，需要脱离 IDE 执行 }
procedure HorizMirror(bmp: TBitmap);
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
      K: DWORD;
      pColor01: PDWORD;
      pColor02: PDWORD;
    begin
      pColor01 := PDWORD(StartScanLine + Y * bmpWidthBytes);
      pColor02 := PDWORD(StartScanLine + Y * bmpWidthBytes);
      for X := 0 to bmp.Width div 2 - 1 do
      begin
        Inc(pColor02, bmp.Width - X - 1);
        K := pColor02^;
        pColor02^ := pColor01^;
        pColor01^ := K;
        Inc(pColor01);
        Dec(pColor02, bmp.Width - X - 1);
      end;
    end);
end;

{ 垂直翻转 }
procedure VertiMirror(bmp: TBitmap);
var
  Count, Y: Integer;
  pColor01: PByte;
  pColor02: PByte;
  tmpColor: PByte;
begin
  Count    := Integer(bmp.ScanLine[0]) - Integer(bmp.ScanLine[1]);
  tmpColor := AllocMem(Count);
  try
    for Y := 0 to bmp.Height div 2 - 1 do
    begin
      pColor01 := bmp.ScanLine[Y];
      pColor02 := bmp.ScanLine[bmp.Height - Y - 1];
      Move(pColor01^, tmpColor^, Count);
      Move(pColor02^, pColor01^, Count);
      Move(tmpColor^, pColor02^, Count);
    end;
  finally
    FreeMem(tmpColor);
  end;
end;

{ 转置翻转 并行模式，需要脱离 IDE 执行 }
procedure HAndVMirror(bmp: TBitmap);
begin
  HorizMirror(bmp);
  VertiMirror(bmp);
end;

procedure Rotate_Point(const bmpSrc: TBitmap; var bmpDst: TBitmap; iAngle: Integer);
var
  CenterX, CenterY: double;
  X, Y            : Integer;
  SrcX, SrcY      : Integer;
  RotaryAngle     : double;
  MoveX, MoveY    : double;
begin
  RotaryAngle        := -(iAngle mod 360) * PI / 180;
  bmpDst.PixelFormat := pf32bit;
  bmpDst.Width       := Round(ABS(bmpSrc.Width * Cos(RotaryAngle)) + ABS(bmpSrc.Height * Sin(RotaryAngle)));
  bmpDst.Height      := Round(ABS(bmpSrc.Width * Sin(RotaryAngle)) + ABS(bmpSrc.Height * Cos(RotaryAngle)));
  MoveX              := (bmpDst.Width - bmpSrc.Width) / 2;
  MoveY              := (bmpDst.Height - bmpSrc.Height) / 2;
  CenterX            := bmpSrc.Width / 2;
  CenterY            := bmpSrc.Height / 2;

  for Y := 0 to bmpDst.Height - 1 do
  begin
    for X := 0 to bmpDst.Width - 1 do
    begin
      SrcX                       := Round((X - CenterX - MoveX) * Cos(RotaryAngle) - (Y - CenterY - MoveY) * Sin(RotaryAngle) + CenterX);
      SrcY                       := Round((X - CenterX - MoveX) * Sin(RotaryAngle) + (Y - CenterY - MoveY) * Cos(RotaryAngle) + CenterY);
      bmpDst.Canvas.Pixels[X, Y] := bmpSrc.Canvas.Pixels[SrcX, SrcY];
    end;
  end;
end;

procedure Rotate_Scanline(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
type
  IntegerArray  = array [0 .. $EFFFFFF] of Integer;
  PIntegerArray = ^IntegerArray;
var
  RotaryAngle                   : Extended;
  CosTheta, SinTheta            : Extended;
  iCosTheta, iSinTheta          : Integer;
  xSrc, ySrc                    : Integer;
  X, Y                          : Integer;
  xODst, yODst                  : Integer;
  CenterX, CenterY              : Integer;
  xPrime, yPrime                : Integer;
  srcWidth, srcHeight           : Integer;
  DstWidth, DstHeight           : Integer;
  yPrimeSinTheta, yPrimeCosTheta: Integer;
  srcBits                       : PInteger;
  dstBits                       : PInteger;
begin
  RotaryAngle := (iAngle mod 360) * PI / 180;
  SinCos(RotaryAngle, SinTheta, CosTheta);
  iSinTheta          := Trunc(SinTheta * (1 shl 16));
  iCosTheta          := Trunc(CosTheta * (1 shl 16));
  bmpSrc.PixelFormat := pf32bit;
  srcWidth           := bmpSrc.Width;
  srcHeight          := bmpSrc.Height;
  CenterX            := srcWidth div 2;
  CenterY            := srcHeight div 2;
  DstWidth           := SmallInt((srcWidth * ABS(iCosTheta) + srcHeight * ABS(iSinTheta)) shr 16);
  DstHeight          := SmallInt((srcWidth * ABS(iSinTheta) + srcHeight * ABS(iCosTheta)) shr 16);
  xODst              := DstWidth div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = -90.0)) and not Odd(DstWidth) then
    Dec(xODst);
  yODst := DstHeight div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = +90.0)) and not Odd(DstHeight) then
    Dec(yODst);
  bmpDst.PixelFormat        := pf32bit;
  bmpDst.Canvas.Brush.Color := clBlack;
  bmpDst.Width              := DstWidth;
  bmpDst.Height             := DstHeight;
  srcBits                   := bmpSrc.ScanLine[srcHeight - 1];
  dstBits                   := @(PIntegerArray(bmpDst.ScanLine[0])[DstWidth - 1]);
  yPrime                    := yODst;
  for Y                     := 0 to DstHeight - 1 do
  begin
    yPrimeSinTheta := yPrime * iSinTheta;
    yPrimeCosTheta := yPrime * iCosTheta;
    xPrime         := xODst;
    for X          := DstWidth - 1 downto 0 do
    begin
      xSrc := SmallInt((xPrime * iCosTheta - yPrimeSinTheta) shr 16) + CenterX;
      ySrc := SmallInt((xPrime * iSinTheta + yPrimeCosTheta) shr 16) + CenterY;
      if (ySrc >= 0) and (ySrc < srcHeight) and (xSrc >= 0) and (xSrc < srcWidth) then
      begin
        Inc(srcBits, ySrc * srcWidth + xSrc);
        dstBits^ := srcBits^;
        Dec(srcBits, ySrc * srcWidth + xSrc);
      end;
      Dec(dstBits);
      Dec(xPrime);
    end;
    Dec(yPrime);
  end;
end;

procedure Rotate_Parallel_Proc(srcBits, dstBits: PInteger; var yPrime: Integer; const srcHeight, srcWidth, DstHeight, DstWidth, iCosTheta, iSinTheta, xODst, CenterX, CenterY: Integer);
var
  X, Y, xPrime                  : Integer;
  xSrc, ySrc                    : Integer;
  yPrimeSinTheta, yPrimeCosTheta: Integer;
begin
  for Y := 0 to DstHeight - 1 do
  begin
    Dec(yPrime);
    xPrime         := xODst;
    yPrimeSinTheta := yPrime * iSinTheta;
    yPrimeCosTheta := yPrime * iCosTheta;
    for X          := 0 to DstWidth - 1 do
    begin
      xSrc := SmallInt((xPrime * iCosTheta - yPrimeSinTheta) shr 16) + CenterX;
      ySrc := SmallInt((xPrime * iSinTheta + yPrimeCosTheta) shr 16) + CenterY;
      if (ySrc >= 0) and (ySrc < srcHeight) and (xSrc >= 0) and (xSrc < srcWidth) then
      begin
        Inc(srcBits, ySrc * srcWidth + xSrc);
        dstBits^ := srcBits^;
        Dec(srcBits, ySrc * srcWidth + xSrc);
      end;
      Dec(dstBits);
      Dec(xPrime);
    end;
  end;
end;

procedure Rotate_Parallel(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
var
  RotaryAngle         : Extended;
  CosTheta, SinTheta  : Extended;
  iCosTheta, iSinTheta: Integer;
  xODst, yODst        : Integer;
  CenterX, CenterY    : Integer;
  yPrime              : Integer;
  srcWidth, srcHeight : Integer;
  DstWidth, DstHeight : Integer;
  srcBits             : PInteger;
  dstBits             : PInteger;
begin
  RotaryAngle := (iAngle mod 360) * PI / 180;
  SinCos(RotaryAngle, SinTheta, CosTheta);
  iSinTheta          := Trunc(SinTheta * (1 shl 16));
  iCosTheta          := Trunc(CosTheta * (1 shl 16));
  bmpSrc.PixelFormat := pf32bit;
  srcWidth           := bmpSrc.Width;
  srcHeight          := bmpSrc.Height;
  CenterX            := srcWidth div 2;
  CenterY            := srcHeight div 2;
  DstWidth           := SmallInt((srcWidth * ABS(iCosTheta) + srcHeight * ABS(iSinTheta)) shr 16);
  DstHeight          := SmallInt((srcWidth * ABS(iSinTheta) + srcHeight * ABS(iCosTheta)) shr 16);
  xODst              := DstWidth div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = -90.0)) and not Odd(DstWidth) then
    Dec(xODst);
  yODst := DstHeight div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = +90.0)) and not Odd(DstHeight) then
    Dec(yODst);
  bmpDst.PixelFormat        := pf32bit;
  bmpDst.Canvas.Brush.Color := clBlack;
  bmpDst.Width              := DstWidth;
  bmpDst.Height             := DstHeight;
  srcBits                   := bmpSrc.ScanLine[srcHeight - 1];
  dstBits                   := @(PIntegerArray(bmpDst.ScanLine[0])[DstWidth - 1]);
  yPrime                    := yODst + 1;
  Rotate_Parallel_Proc(srcBits, dstBits, yPrime, srcHeight, srcWidth, DstHeight, DstWidth, iCosTheta, iSinTheta, xODst, CenterX, CenterY);
end;

end.
