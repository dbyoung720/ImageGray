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

implementation

{ 存取类的保护成员变量 }
type
  TBMPAccess         = class(TBitmap);
  TBitmapImageAccess = class(TBitmapImage);
  PRGBQuadArray      = ^TRGBQuadArray;
  TRGBQuadArray      = array [0 .. 0] of TRGBQuad;

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

{ 标准旋转函数 }
procedure Optimize01(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
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

{ 优化 Pixels }
procedure Optimize02(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
var
  X, Y      : Integer;
  SrcX, SrcY: Integer;
  srcBits   : PRGBQuadArray;
  dstBits   : PRGBQuadArray;
  dstWidth  : Integer;
  dstHeight : Integer;
  srcWidth  : Integer;
  srcHeight : Integer;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  for Y := 0 to dstHeight - 1 do
  begin
    for X := 0 to dstWidth - 1 do
    begin
      SrcX := Round((X - CenterX - MoveX) * Cos(RotaryAngle) - (Y - CenterY - MoveY) * Sin(RotaryAngle) + CenterX);
      SrcY := Round((X - CenterX - MoveX) * Sin(RotaryAngle) + (Y - CenterY - MoveY) * Cos(RotaryAngle) + CenterY);
      if (DWORD(SrcY) < DWORD(srcHeight)) and (DWORD(SrcX) < DWORD(srcWidth)) then
      begin
        dstBits[Y * dstWidth + X] := srcBits[SrcY * srcWidth + SrcX];
      end;
    end;
  end;
end;

{ 优化循环 }
procedure Optimize03(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
var
  X, Y      : Integer;
  SrcX, SrcY: Integer;
  srcBits   : PRGBQuadArray;
  dstBits   : PRGBQuadArray;
  cxc, cxs  : Single;
  cyc, cys  : Single;
  rac, ras  : Single;
  dstWidth  : Integer;
  dstHeight : Integer;
  srcWidth  : Integer;
  srcHeight : Integer;
  krx, kry  : Single;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  rac := Cos(RotaryAngle);
  ras := Sin(RotaryAngle);
  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;

  for Y := 0 to dstHeight - 1 do
  begin
    krx   := cxc - cys - CenterX + Y * ras;
    kry   := cxs + cyc - CenterY - Y * rac;
    for X := 0 to dstWidth - 1 do
    begin
      SrcX := Round(X * rac - krx);
      SrcY := Round(X * ras - kry);
      if (DWORD(SrcY) < DWORD(srcHeight)) and (DWORD(SrcX) < DWORD(srcWidth)) then
      begin
        dstBits[Y * dstWidth + X] := srcBits[SrcY * srcWidth + SrcX];
      end;
    end;
  end;
end;

{ 优化浮点运算为整数运算 }
procedure Optimize04(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
var
  X, Y      : Integer;
  SrcX, SrcY: Integer;
  srcBits   : PRGBQuadArray;
  dstBits   : PRGBQuadArray;
  cxc, cxs  : Integer;
  cyc, cys  : Integer;
  rac, ras  : Integer;
  kcx, kcy  : Integer;
  dstWidth  : Integer;
  dstHeight : Integer;
  srcWidth  : Integer;
  srcHeight : Integer;
  krx, kry  : Integer;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  rac := Trunc(Cos(RotaryAngle) * (1 shl 16));
  ras := Trunc(Sin(RotaryAngle) * (1 shl 16));
  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;
  kcx := cxc - cys - CenterX * (1 shl 16);
  kcy := cxs + cyc - CenterY * (1 shl 16);

  for Y := 0 to dstHeight - 1 do
  begin
    krx   := kcx + Y * ras;
    kry   := kcy - Y * rac;
    for X := 0 to dstWidth - 1 do
    begin
      SrcX := SmallInt((X * rac - krx) shr 16);
      SrcY := SmallInt((X * ras - kry) shr 16);
      if (DWORD(SrcY) < DWORD(srcHeight)) and (DWORD(SrcX) < DWORD(srcWidth)) then
      begin
        dstBits[Y * dstWidth + X] := srcBits[SrcY * srcWidth + SrcX];
      end;
    end;
  end;
end;

{ 并行优化 }
procedure Optimize05(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
var
  srcBits  : PRGBQuadArray;
  dstBits  : PRGBQuadArray;
  cxc, cxs : Integer;
  cyc, cys : Integer;
  rac, ras : Integer;
  kcx, kcy : Integer;
  dstWidth : Integer;
  dstHeight: Integer;
  srcWidth : Integer;
  srcHeight: Integer;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  rac := Trunc(Cos(RotaryAngle) * (1 shl 16));
  ras := Trunc(Sin(RotaryAngle) * (1 shl 16));
  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;
  kcx := cxc - cys - CenterX * (1 shl 16);
  kcy := cxs + cyc - CenterY * (1 shl 16);

  TParallel.For(0, dstHeight - 1,
    procedure(Y: Integer)
    var
      X: Integer;
      krx, kry: Integer;
      SrcX, SrcY: Integer;
    begin
      krx := kcx + Y * ras;
      kry := kcy - Y * rac;
      for X := 0 to dstWidth - 1 do
      begin
        SrcX := SmallInt((X * rac - krx) shr 16);
        SrcY := SmallInt((X * ras - kry) shr 16);
        if (DWORD(SrcY) < DWORD(srcHeight)) and (DWORD(SrcX) < DWORD(srcWidth)) then
        begin
          dstBits[Y * dstWidth + X] := srcBits[SrcY * srcWidth + SrcX];
        end;
      end;
    end);
end;

procedure Rotate(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
var
  RotaryAngle     : double;
  CenterX, CenterY: Integer;
  MoveX, MoveY    : Integer;
begin
  RotaryAngle               := (iAngle mod 360) * PI / 180;
  bmpDst.PixelFormat        := pf32bit;
  bmpDst.Width              := Round(ABS(bmpSrc.Width * Cos(RotaryAngle)) + ABS(bmpSrc.Height * Sin(RotaryAngle)));
  bmpDst.Height             := Round(ABS(bmpSrc.Width * Sin(RotaryAngle)) + ABS(bmpSrc.Height * Cos(RotaryAngle)));
  bmpDst.Canvas.Brush.Color := clBlack;
  bmpDst.Canvas.FillRect(bmpDst.Canvas.ClipRect);

  MoveX   := (bmpDst.Width - bmpSrc.Width) div 2;
  MoveY   := (bmpDst.Height - bmpSrc.Height) div 2;
  CenterX := bmpSrc.Width div 2;
  CenterY := bmpSrc.Height div 2;

  Optimize05(bmpSrc, bmpDst, RotaryAngle, CenterX, CenterY, MoveX, MoveY);
end;

end.
