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

uses Winapi.Windows, System.Threading, System.Diagnostics, System.SyncObjs, System.Classes, System.Math, Vcl.Graphics, db.Image.Common;

{ 水平翻转 并行模式，需要脱离 IDE 执行 }
procedure HorizMirror(bmp: TBitmap);

{ 垂直翻转 }
procedure VertiMirror(bmp: TBitmap);

{ 转置翻转 并行模式，需要脱离 IDE 执行 }
procedure HAndVMirror(bmp: TBitmap);

{ 旋转 }
procedure Rotate(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
procedure Rotate_Optimize(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);

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
      swapColor: DWORD;
      pColor01: PDWORD;
      pColor02: PDWORD;
    begin
      pColor01 := PDWORD(StartScanLine + Y * bmpWidthBytes);
      pColor02 := PDWORD(StartScanLine + Y * bmpWidthBytes + 4 * (bmp.Width - 1));
      for X := 0 to bmp.Width div 2 - 1 do
      begin
        swapColor := pColor02^;
        pColor02^ := pColor01^;
        pColor01^ := swapColor;
        Inc(pColor01);
        Dec(pColor02);
      end;
    end);
end;

{ 垂直翻转 }
procedure VertiMirror(bmp: TBitmap);
var
  Y       : Integer;
  Count   : Integer;
  pColor01: Pointer;
  pColor02: Pointer;
  tmpColor: Pointer;
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
      // apex_memcpy(tmpColor, pColor01, Count);
      // apex_memcpy(pColor01, pColor02, Count);
      // apex_memcpy(pColor02, tmpColor, Count);
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

{
  假设对图片上任意点(x,y)，绕一个坐标点(rx0,ry0)逆时针旋转RotaryAngle角度后的新的坐标设为(x', y')，有公式：
  x'= (x - rx0)*cos(RotaryAngle) + (y - ry0)*sin(RotaryAngle) + rx0 ;
  y'=-(x - rx0)*sin(RotaryAngle) + (y - ry0)*cos(RotaryAngle) + ry0 ;

  那么，根据新的坐标点求源坐标点的公式为：
  x=(x'- rx0)*cos(RotaryAngle) - (y'- ry0)*sin(RotaryAngle) + rx0 ;
  y=(x'- rx0)*sin(RotaryAngle) + (y'- ry0)*cos(RotaryAngle) + ry0 ;

  看看这个循环，影响效率的问题有如下三个：
  1、有浮点运算；          可优化为整数运算
  2、只是单个像素点运算；  可优化为整行运算
  3、- CenterX - MoveX) * cos(RotaryAngle)、- CenterY - MoveY) * sin(RotaryAngle)、- CenterX - MoveX) * sin(RotaryAngle)、- CenterY - MoveY) * cos(RotaryAngle) 可以拿到循环上一层；
  4、[SrcX, SrcY] 在原图上不见得存在；
}

{ 存取类的保护成员变量 }
type
  TBMPAccess         = class(TBitmap);
  TBitmapImageAccess = class(TBitmapImage);

procedure Optimize01(bmpSrc, bmpDst: TBitmap; const CenterX, CenterY, RotaryAngle, MoveX, MoveY: double);
var
  X, Y      : Integer;
  SrcX, SrcY: Integer;
begin
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

procedure Optimize02(bmpSrc, bmpDst: TBitmap; const CenterX, CenterY, RotaryAngle, MoveX, MoveY: double);
var
  X, Y      : Integer;
  SrcX, SrcY: Integer;
  srcBits   : PDWORD;
  dstBits   : PDWORD;
  AAA       : Integer;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  for Y := 0 to bmpDst.Height - 1 do
  begin
    for X := 0 to bmpDst.Width - 1 do
    begin
      SrcX := Round((X - CenterX - MoveX) * Cos(RotaryAngle) - (Y - CenterY - MoveY) * Sin(RotaryAngle) + CenterX);
      SrcY := Round((X - CenterX - MoveX) * Sin(RotaryAngle) + (Y - CenterY - MoveY) * Cos(RotaryAngle) + CenterY);
      if (SrcX >= 0) and (SrcX < bmpSrc.Width) and (SrcY >= 0) and (SrcY < bmpSrc.Height) then
      begin
        AAA := SrcY * bmpSrc.Width + SrcX;
        Inc(srcBits, AAA);
        dstBits^ := srcBits^;
        Dec(srcBits, AAA);
      end;
      Inc(dstBits);
    end;
  end;
end;

procedure Optimize03(bmpSrc, bmpDst: TBitmap; const CenterX, CenterY, RotaryAngle, MoveX, MoveY: double);
var
  X, Y                  : Integer;
  SrcX, SrcY            : Integer;
  srcBits               : PDWORD;
  dstBits               : PDWORD;
  xCos, xSin, ySin, yCos: double;
  xValue, yValue        : double;
  CosTheta, SinTheta    : Extended;
  ySinT, YCosT          : double;
  AAA                   : Integer;
begin
  xCos   := (CenterX + MoveX) * Cos(RotaryAngle);
  xSin   := (CenterX + MoveX) * Sin(RotaryAngle);
  yCos   := (CenterY + MoveY) * Cos(RotaryAngle);
  ySin   := (CenterY + MoveY) * Sin(RotaryAngle);
  xValue := -xCos + ySin + CenterX;
  yValue := -xSin - yCos + CenterY;
  SinCos(RotaryAngle, SinTheta, CosTheta);

  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  for Y := 0 to bmpDst.Height - 1 do
  begin
    ySinT := Y * SinTheta - xValue;
    YCosT := Y * CosTheta + yValue;
    for X := 0 to bmpDst.Width - 1 do
    begin
      SrcX := Round(X * CosTheta - ySinT);
      SrcY := Round(X * SinTheta + YCosT);
      if (SrcX >= 0) and (SrcX < bmpSrc.Width) and (SrcY >= 0) and (SrcY < bmpSrc.Height) then
      begin
        AAA := SrcY * bmpSrc.Width + SrcX;
        Inc(srcBits, AAA);
        dstBits^ := srcBits^;
        Dec(srcBits, AAA);
      end;
      Inc(dstBits);
    end;
  end;
end;

procedure Optimize04(bmpSrc, bmpDst: TBitmap; const CenterX, CenterY, RotaryAngle, MoveX, MoveY: double);
var
  dstX, dstY            : Integer;
  SrcX, SrcY            : Integer;
  srcBits               : PDWORD;
  dstBits               : PDWORD;
  xCos, xSin, ySin, yCos: double;
  xValue, yValue        : Integer;
  CosTheta, SinTheta    : Extended;
  iCosTheta, iSinTheta  : Integer;
  ySinT, YCosT          : Integer;
  AAA                   : Integer;
begin
  xCos   := (CenterX + MoveX) * Cos(RotaryAngle);
  xSin   := (CenterX + MoveX) * Sin(RotaryAngle);
  yCos   := (CenterY + MoveY) * Cos(RotaryAngle);
  ySin   := (CenterY + MoveY) * Sin(RotaryAngle);
  xValue := Round(-xCos + ySin + CenterX);
  yValue := Round(-xSin - yCos + CenterY);
  SinCos(RotaryAngle, SinTheta, CosTheta);
  iSinTheta := Trunc(SinTheta * (1 shl 16));
  iCosTheta := Trunc(CosTheta * (1 shl 16));

  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  for dstY := 0 to bmpDst.Height - 1 do
  begin
    ySinT    := dstY * iSinTheta shr 16 - xValue;
    YCosT    := dstY * iCosTheta shr 16 + yValue;
    for dstX := 0 to bmpDst.Width - 1 do
    begin
      SrcX := dstX * iCosTheta shr 16 - ySinT;
      SrcY := dstX * iSinTheta shr 16 + YCosT;
      if (SrcX >= 0) and (SrcX < bmpSrc.Width) and (SrcY >= 0) and (SrcY < bmpSrc.Height) then
      begin
        AAA := SrcY * bmpSrc.Width + SrcX;
        Inc(srcBits, AAA);
        dstBits^ := srcBits^;
        Dec(srcBits, AAA);
      end;
      Inc(dstBits);
    end;
  end;
end;

procedure Optimize05(bmpSrc, bmpDst: TBitmap; const CenterX, CenterY: Integer; const RotaryAngle, MoveX, MoveY: double);
var
  dstX, dstY            : Integer;
  SrcX, SrcY            : Integer;
  srcBits               : array of TRGBQuad;
  dstBits               : PRGBQuad;
  xCos, xSin, ySin, yCos: double;
  xValue, yValue        : Integer;
  CosTheta, SinTheta    : Extended;
  iCosTheta, iSinTheta  : Integer;
  ySinT, YCosT          : Integer;
begin
  xCos   := (CenterX + MoveX) * Cos(RotaryAngle);
  xSin   := (CenterX + MoveX) * Sin(RotaryAngle);
  yCos   := (CenterY + MoveY) * Cos(RotaryAngle);
  ySin   := (CenterY + MoveY) * Sin(RotaryAngle);
  xValue := Round(-xCos + ySin + CenterX);
  yValue := Round(-xSin - yCos + CenterY);
  SinCos(RotaryAngle, SinTheta, CosTheta);
  iSinTheta := Trunc(SinTheta * (1 shl 16));
  iCosTheta := Trunc(CosTheta * (1 shl 16));

  SetLength(srcBits, bmpSrc.Width * bmpSrc.Height);
  CopyMemory(srcBits, TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits, bmpSrc.Width * bmpSrc.Height * 4);
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  for dstY := 0 to bmpDst.Height - 1 do
  begin
    ySinT    := dstY * iSinTheta shr 16 - xValue;
    YCosT    := dstY * iCosTheta shr 16 + yValue;
    for dstX := 0 to bmpDst.Width - 1 do
    begin
      SrcX := dstX * iCosTheta shr 16 - ySinT;
      SrcY := dstX * iSinTheta shr 16 + YCosT;
      if (SrcX >= 0) and (SrcX < bmpSrc.Width) and (SrcY >= 0) and (SrcY < bmpSrc.Height) then
      begin
        dstBits^ := srcBits[SrcY * bmpSrc.Width + SrcX];
      end;
      Inc(dstBits);
    end;
  end;
end;

procedure Optimize06(bmpSrc, bmpDst: TBitmap; const CenterX, CenterY: Integer; const RotaryAngle, MoveX, MoveY: double); inline;
type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [0 .. 0] of TRGBQuad;
var
  xODst, yODst                  : Integer;
  srcWidth, srcHeight           : Integer;
  dstWidth, dstHeight           : Integer;
  srcBits                       : PRGBQuadArray;
  dstBits                       : PRGBQuadArray;
  dstBit                        : PRGBQuad;
  xPrime, yPrime                : Integer;
  X, Y                          : Integer;
  yPrimeSinTheta, yPrimeCosTheta: Integer;
  CosTheta, SinTheta            : Extended;
  iCosTheta, iSinTheta          : Integer;
  xSrc, ySrc                    : Integer;
begin
  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  xODst := dstWidth div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = -90.0)) and not Odd(dstWidth) then
    Dec(xODst);
  yODst := dstHeight div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = +90.0)) and not Odd(dstHeight) then
    Dec(yODst);

  SinCos(RotaryAngle, SinTheta, CosTheta);
  iSinTheta := Trunc(SinTheta * (1 shl 16));
  iCosTheta := Trunc(CosTheta * (1 shl 16));

  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;
  dstBit  := @dstBits[(bmpDst.Width * bmpDst.Height) - 1];

  yPrime := yODst;
  for Y  := 0 to dstHeight - 1 do
  begin
    yPrimeSinTheta := yPrime * iSinTheta;
    yPrimeCosTheta := yPrime * iCosTheta;
    xPrime         := xODst;
    for X          := 0 to dstWidth - 1 do
    begin
      xSrc := SmallInt((xPrime * iCosTheta - yPrimeSinTheta) shr 16) + CenterX;
      ySrc := SmallInt((xPrime * iSinTheta + yPrimeCosTheta) shr 16) + CenterY;
      if (DWORD(ySrc) < DWORD(srcHeight)) and (DWORD(xSrc) < DWORD(srcWidth)) then
      begin
        dstBit^ := srcBits[ySrc * srcWidth + xSrc];
      end;
      Dec(dstBit);
      Dec(xPrime);
    end;
    Dec(yPrime);
  end;
end;

procedure Rotate(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
var
  CenterX, CenterY: Integer;
  RotaryAngle     : double;
  MoveX, MoveY    : double;
begin
  RotaryAngle               := (iAngle mod 360) * PI / 180;
  bmpDst.PixelFormat        := pf32bit;
  bmpDst.Width              := Round(ABS(bmpSrc.Width * Cos(RotaryAngle)) + ABS(bmpSrc.Height * Sin(RotaryAngle)));
  bmpDst.Height             := Round(ABS(bmpSrc.Width * Sin(RotaryAngle)) + ABS(bmpSrc.Height * Cos(RotaryAngle)));
  bmpDst.Canvas.Brush.Color := clBlack;
  bmpDst.Canvas.FillRect(bmpDst.Canvas.ClipRect);

  MoveX   := (bmpDst.Width - bmpSrc.Width) / 2;
  MoveY   := (bmpDst.Height - bmpSrc.Height) / 2;
  CenterX := bmpSrc.Width div 2;
  CenterY := bmpSrc.Height div 2;

  Optimize06(bmpSrc, bmpDst, CenterX, CenterY, RotaryAngle, MoveX, MoveY);
end;

procedure Rotate_Optimize(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
type
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [0 .. 0] of TRGBQuad;
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
  dstWidth, dstHeight           : Integer;
  yPrimeSinTheta, yPrimeCosTheta: Integer;
  srcBits                       : PRGBQuadArray;
  dstBits                       : PRGBQuadArray;
  dstBit                        : PRGBQuad;
begin
  RotaryAngle := (iAngle mod 360) * PI / 180;
  SinCos(RotaryAngle, SinTheta, CosTheta);
  iSinTheta                 := Trunc(SinTheta * (1 shl 16));
  iCosTheta                 := Trunc(CosTheta * (1 shl 16));
  bmpSrc.PixelFormat        := pf32bit;
  srcWidth                  := bmpSrc.Width;
  srcHeight                 := bmpSrc.Height;
  CenterX                   := srcWidth div 2;
  CenterY                   := srcHeight div 2;
  dstWidth                  := SmallInt((srcWidth * ABS(iCosTheta) + srcHeight * ABS(iSinTheta)) shr 16);
  dstHeight                 := SmallInt((srcWidth * ABS(iSinTheta) + srcHeight * ABS(iCosTheta)) shr 16);
  bmpDst.PixelFormat        := pf32bit;
  bmpDst.Width              := dstWidth;
  bmpDst.Height             := dstHeight;
  bmpDst.Canvas.Brush.Color := clBlack;
  bmpDst.Canvas.FillRect(bmpDst.Canvas.ClipRect);

  xODst := dstWidth div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = -90.0)) and not Odd(dstWidth) then
    Dec(xODst);
  yODst := dstHeight div 2;
  if ((RotaryAngle = 0.0) or (RotaryAngle = +90.0)) and not Odd(dstHeight) then
    Dec(yODst);

  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;
  dstBit  := @dstBits[(bmpDst.Width * bmpDst.Height) - 1];

  yPrime := yODst;
  for Y  := 0 to dstHeight - 1 do
  begin
    yPrimeSinTheta := yPrime * iSinTheta;
    yPrimeCosTheta := yPrime * iCosTheta;
    xPrime         := xODst;
    for X          := 0 to dstWidth - 1 do
    begin
      xSrc := SmallInt((xPrime * iCosTheta - yPrimeSinTheta) shr 16) + CenterX;
      ySrc := SmallInt((xPrime * iSinTheta + yPrimeCosTheta) shr 16) + CenterY;
      if (DWORD(ySrc) < DWORD(srcHeight)) and (DWORD(xSrc) < DWORD(srcWidth)) then
      begin
        dstBit^ := srcBits[ySrc * srcWidth + xSrc];
      end;
      Dec(dstBit);
      Dec(xPrime);
    end;
    Dec(yPrime);
  end;
end;

end.
