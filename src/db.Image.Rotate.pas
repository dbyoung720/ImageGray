unit db.Image.Rotate;
{
  Func: 32位位图任意角度旋转
  Name: dbyoung@sina.com
  Date: 2021-2-22
  Vers: Delphi 11

  原理：
  假设对图片上任意点(x,y)，绕一个坐标点(rx0,ry0)逆时针旋转RotaryAngle角度后的新的坐标设为(x', y')，有公式：
  x'= (x - rx0)*cos(RotaryAngle) + (y - ry0)*sin(RotaryAngle) + rx0 ;
  y'=-(x - rx0)*sin(RotaryAngle) + (y - ry0)*cos(RotaryAngle) + ry0 ;

  那么，根据新的坐标点求源坐标点的公式为：
  x=(x'- rx0)*cos(RotaryAngle) - (y'- ry0)*sin(RotaryAngle) + rx0 ;
  y=(x'- rx0)*sin(RotaryAngle) + (y'- ry0)*cos(RotaryAngle) + ry0 ;
}

interface

uses Winapi.Windows, System.Threading, System.Diagnostics, System.SyncObjs, System.Classes, System.Math, Vcl.Graphics, db.Image.Common;

{ 旋转 }
procedure Rotate(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);

implementation

{ 存取类的保护成员变量 }
type
  TBMPAccess         = class(TBitmap);
  TBitmapImageAccess = class(TBitmapImage);

  { 标准旋转函数 }
procedure Optimize01(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
var
  dstX, dstY: Integer;
  srcX, srcY: Integer;
begin
  for dstY := 0 to bmpDst.Height - 1 do
  begin
    for dstX := 0 to bmpDst.Width - 1 do
    begin
      srcX                             := Round((dstX - CenterX - MoveX) * Cos(RotaryAngle) - (dstY - CenterY - MoveY) * Sin(RotaryAngle) + CenterX);
      srcY                             := Round((dstX - CenterX - MoveX) * Sin(RotaryAngle) + (dstY - CenterY - MoveY) * Cos(RotaryAngle) + CenterY);
      bmpDst.Canvas.Pixels[dstX, dstY] := bmpSrc.Canvas.Pixels[srcX, srcY];
    end;
  end;
end;

{ 优化 Pixels }
procedure Optimize02(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
var
  dstX, dstY: Integer;
  srcX, srcY: Integer;
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

  for dstY := 0 to dstHeight - 1 do
  begin
    for dstX := 0 to dstWidth - 1 do
    begin
      srcX := Round((dstX - CenterX - MoveX) * Cos(RotaryAngle) - (dstY - CenterY - MoveY) * Sin(RotaryAngle) + CenterX);
      srcY := Round((dstX - CenterX - MoveX) * Sin(RotaryAngle) + (dstY - CenterY - MoveY) * Cos(RotaryAngle) + CenterY);
      if (DWORD(srcY) < DWORD(srcHeight)) and (DWORD(srcX) < DWORD(srcWidth)) then
      begin
        dstBits[dstY * dstWidth + dstX] := srcBits[srcY * srcWidth + srcX];
      end;
    end;
  end;
end;

{ 优化循环 }
procedure Optimize03(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
var
  dstX, dstY: Integer;
  srcX, srcY: Integer;
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

  SinCos(RotaryAngle, ras, rac);
  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;

  for dstY := 0 to dstHeight - 1 do
  begin
    krx      := cxc - cys - CenterX + dstY * ras;
    kry      := cxs + cyc - CenterY - dstY * rac;
    for dstX := 0 to dstWidth - 1 do
    begin
      srcX := Round(dstX * rac - krx);
      srcY := Round(dstX * ras - kry);
      if (DWORD(srcY) < DWORD(srcHeight)) and (DWORD(srcX) < DWORD(srcWidth)) then
      begin
        dstBits[dstY * dstWidth + dstX] := srcBits[srcY * srcWidth + srcX];
      end;
    end;
  end;
end;

{ 优化浮点运算为整数运算 }
procedure Optimize04(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer; const ras: Integer = 0; rac: Integer = 0);
var
  dstX, dstY: Integer;
  srcX, srcY: DWORD;
  srcBits   : PRGBQuadArray;
  dstBits   : PRGBQuadArray;
  cxc, cxs  : Integer;
  cyc, cys  : Integer;
  kcx, kcy  : Integer;
  dstWidth  : Integer;
  dstHeight : Integer;
  srcWidth  : DWORD;
  srcHeight : DWORD;
  krx, kry  : Integer;
  intOffset : Integer;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;
  kcx := cxc - cys - CenterX * (1 shl 8);
  kcy := cxs + cyc - CenterY * (1 shl 8);

  for dstY := 0 to dstHeight - 1 do
  begin
    krx       := kcx + dstY * ras;
    kry       := kcy - dstY * rac;
    intOffset := dstY * dstWidth;
    for dstX  := 0 to dstWidth - 1 do
    begin
      srcX := (dstX * rac - krx) shr 8;
      srcY := (dstX * ras - kry) shr 8;
      if (srcY < srcHeight) and (srcX < srcWidth) then
      begin
        dstBits[intOffset + dstX] := srcBits[srcY * srcWidth + srcX];
      end;
    end;
  end;
end;

{ 乘法优化为查表 }
procedure Optimize05(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer; const ras: Integer = 0; rac: Integer = 0);
var
  dstX, dstY: Integer;
  srcX, srcY: DWORD;
  srcBits   : PRGBQuadArray;
  dstBits   : PRGBQuadArray;
  cxc, cxs  : Integer;
  cyc, cys  : Integer;
  kcx, kcy  : Integer;
  dstWidth  : Integer;
  dstHeight : Integer;
  srcWidth  : DWORD;
  srcHeight : DWORD;
  krx, kry  : Integer;
  intOffset : Integer;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;
  kcx := cxc - cys - CenterX * (1 shl 8);
  kcy := cxs + cyc - CenterY * (1 shl 8);

  for dstY := 0 to dstHeight - 1 do
  begin
    krx       := kcx + g_RotateTable[ras, dstY];
    kry       := kcy - g_RotateTable[rac, dstY];
    intOffset := dstY * dstWidth;
    for dstX  := 0 to dstWidth - 1 do
    begin
      srcX := (g_RotateTable[rac, dstX] - krx) shr 8;
      srcY := (g_RotateTable[ras, dstX] - kry) shr 8;
      if (srcY < srcHeight) and (srcX < srcWidth) then
      begin
        dstBits[intOffset + dstX] := srcBits[srcWidth * srcY + srcX];
      end;
    end;
  end;
end;

{ 并行优化 }
procedure Optimize06(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer; const ras: Integer = 0; rac: Integer = 0);
var
  srcBits  : PRGBQuadArray;
  dstBits  : PRGBQuadArray;
  cxc, cxs : Integer;
  cyc, cys : Integer;
  kcx, kcy : Integer;
  dstWidth : Integer;
  dstHeight: Integer;
  srcWidth : DWORD;
  srcHeight: DWORD;
begin
  srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;
  dstBits := TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits;

  dstWidth  := bmpDst.Width;
  dstHeight := bmpDst.Height;
  srcWidth  := bmpSrc.Width;
  srcHeight := bmpSrc.Height;

  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;
  kcx := cxc - cys - CenterX * (1 shl 8);
  kcy := cxs + cyc - CenterY * (1 shl 8);

  TParallel.For(0, dstHeight - 1,
    procedure(dstY: Integer)
    var
      dstX: Integer;
      krx, kry: Integer;
      srcX, srcY: DWORD;
      intOffset: Integer;
    begin
      krx := kcx + g_RotateTable[ras, dstY];
      kry := kcy - g_RotateTable[rac, dstY];
      intOffset := dstY * dstWidth;
      for dstX := 0 to dstWidth - 1 do
      begin
        srcX := (g_RotateTable[rac, dstX] - krx) shr 8;
        srcY := (g_RotateTable[ras, dstX] - kry) shr 8;
        if (srcY < srcHeight) and (srcX < srcWidth) then
        begin
          dstBits[intOffset + dstX] := srcBits[srcWidth * srcY + srcX];
        end;
      end;
    end);
end;

procedure Rotate_Proc(const krx, kry, intOffset: Integer; const srcBits: PRGBQuadArray; dstBits: PRGBQuadArray; const rac, ras: Integer; const dstWidth, srcWidth, srcHeight: DWORD); assembler; inline;
var
  dstX      : Integer;
  srcX, srcY: DWORD;
begin
  for dstX := dstWidth - 1 downto 0 do
  begin
    srcX := (dstX * rac - krx) shr 8;
    srcY := (dstX * ras - kry) shr 8;
    if (srcY < srcHeight) and (srcX < srcWidth) then
    begin
      dstBits[intOffset + dstX] := srcBits[srcY * srcWidth + srcX];
    end;
  end;
end;

procedure Rotate_ASM_Proc(const krx, kry, intOffset: Integer; const srcBits: PRGBQuadArray; dstBits: PRGBQuadArray; const rac, ras: Integer; const dstWidth, srcWidth, srcHeight: DWORD); assembler;
asm
  {$IFDEF  WIN32}
  MOV  [EBP-$04], EAX          // [EBP-$04] = krx
  MOV  [EBP-$08], EDX          // [EBP-$08] = kry
  MOV  EBX, ECX                // EBX = intOffset
  MOV  ECX, dstWidth           // ECX = dstWidth 循环计数

@LOOP:
  MOV   EAX,  ECX               // EAX = X
  IMUL  EAX,  rac               // EAX = X * rac
  SUB   EAX,  [EBP-$04]         // EAX = X * rac - krx
  SHR   EAX,  8                 // EAX = (X * rac - krx) shr 8  = SrcX

  MOV   EDX,  ECX               // EDX = X
  IMUL  EDX,  ras               // EDX = X * ras
  SUB   EDX,  [EBP-$08]         // EDX = X * ras - kry
  SHR   EDX,  8                 // EDX = (X * ras - kry) shr 8  = SrcY

  CMP   EAX,  srcWidth          // IF SrcX < srcWidth
  JNB   @NEXT                   //
  CMP   EDX,  srcHeight         // IF SrcY < srcHeight
  JNB   @NEXT                   //

  MOV  EDI,  EDX                // EDI = (X * ras - kry) shr 8  = SrcY
  IMUL EDI,  srcWidth           // EDI = SrcY * srcWidth
  ADD  EDI,  EAX                // EDI = SrcY * srcWidth + SrcX
  MOV  EDX,  [srcBits]          // EDI = [srcBits]
  MOV  EDI,  [EDX + EDI * 4]    // EDI = srcBits[SrcY * srcWidth + SrcX]

  MOV  EDX,  EBX                // EDX = intOffset
  ADD  EDX,  ECX                // EDX = intOffset + X
  MOV  ESI,  [dstBits]          // ESI = [dstBits]
  MOV  [ESI + EDX * 4], EDI     // dstBits[IndexRow * Integer(dstWidth) + X] = EDI

@NEXT:
  DEC  ECX
  JNZ  @LOOP
  {$IFEND}
END;

procedure Rotate_SSE_Proc(const krx, kry, intOffset: Integer; const srcBits: PRGBQuadArray; dstBits: PRGBQuadArray; const rac, ras: Integer; const dstWidth, srcWidth, srcHeight: DWORD); assembler;
asm
  {$IFDEF  WIN32}
  MOV     EBX,   ECX            // EBX = intOffset
  MOV     ECX,   dstWidth       // ECX = dstWidth 循环计数
  MOVD    XMM1,  rac            // XMM1 = 0, 0, 0, rac
  MOVD    XMM0,  ras            // XMM0 = 0, 0, 0, ras
  SHUFPS  XMM0,  XMM1, 0        // XMM0 = rac, rac, ras, ras
  MOVD    XMM2,  krx            // XMM2 = 0, 0, 0, krx
  MOVD    XMM1,  kry            // XMM1 = 0, 0, 0, kry
  SHUFPS  XMM1,  XMM2, 0        // XMM1 = krx, krx, kry, kry

@LOOP:
  MOVD    XMM4,  ECX            // XMM4 = 0, 0, 0, X
  SHUFPS  XMM4,  XMM4, 0        // XMM4 = X, X, X, X

  PMULLD  XMM4,  XMM0           // X * rac | X * ras
  PSUBD   XMM4,  XMM1           // X * rac - krx | X * ras - kry
  PSRLD   XMM4,  8              // (X * rac - krx) shr 8 |  (X * ras - kry) shr 8

  PEXTRD  EAX,   XMM4, 3        // EAX = (X * rac - krx) shr 8  = SrcX
  PEXTRD  EDX,   XMM4, 1        // EDX = (X * ras - kry) shr 8  = SrcY

  CMP   EAX,  srcWidth          // IF SrcX < srcWidth
  JNB   @NEXT                   //
  CMP   EDX,  srcHeight         // IF SrcY < srcHeight
  JNB   @NEXT                   //

  MOV  EDI,  EDX                // EDI = (X * ras - kry) shr 8  = SrcY
  IMUL EDI,  srcWidth           // EDI = SrcY * srcWidth
  ADD  EDI,  EAX                // EDI = SrcY * srcWidth + SrcX
  MOV  EDX,  [srcBits]          // EDI = [srcBits]
  MOV  EDI,  [EDX + EDI * 4]    // EDI = srcBits[SrcY * srcWidth + SrcX]

  MOV  EDX,  EBX                // EDX = intOffset
  ADD  EDX,  ECX                // EDX = intOffset + X
  MOV  ESI,  [dstBits]          // ESI = [dstBits]
  MOV  [ESI + EDX * 4], EDI     // dstBits[IndexRow * Integer(dstWidth) + X] = EDI

@NEXT:
  DEC  ECX
  JNZ  @LOOP
  {$IFEND}
end;

procedure Rotate_AVX_Proc(const krx, kry, IndexRow: Integer; const srcBits: PRGBQuadArray; dstBits: PRGBQuadArray; const rac, ras: Integer; const dstWidth, srcWidth, srcHeight: DWORD); assembler;
asm

end;

{ 并行 + SIMD 优化 }
procedure Optimize07(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer; const ras: Integer = 0; rac: Integer = 0);
var
  srcBits  : PRGBQuadArray;
  dstBits  : PRGBQuadArray;
  cxc, cxs : Integer;
  cyc, cys : Integer;
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

  cxc := (CenterX + MoveX) * rac;
  cxs := (CenterX + MoveX) * ras;
  cys := (CenterY + MoveY) * ras;
  cyc := (CenterY + MoveY) * rac;
  kcx := cxc - cys - CenterX * (1 shl 8);
  kcy := cxs + cyc - CenterY * (1 shl 8);

  TParallel.For(0, dstHeight - 1,
    procedure(IndexRow: Integer)
    var
      krx, kry: Integer;
      intOffset: Integer;
    begin
      krx := kcx + g_RotateTable[ras, IndexRow];
      kry := kcy - g_RotateTable[rac, IndexRow];
      intOffset := IndexRow * dstWidth;
      Rotate_SSE_Proc(krx, kry, intOffset, srcBits, dstBits, rac, ras, dstWidth, srcWidth, srcHeight);
    end);
end;

procedure Rotate(const bmpSrc: TBitmap; var bmpDst: TBitmap; const iAngle: Integer);
var
  RotaryAngle     : double;
  CenterX, CenterY: Integer;
  MoveX, MoveY    : Integer;
  srac, sras      : Single;
  rac, ras        : Integer;
begin
  RotaryAngle := (iAngle mod 360) * PI / 180;
  SinCos(RotaryAngle, sras, srac);
  rac := Trunc(srac * (1 shl 8));
  ras := Trunc(sras * (1 shl 8));

  bmpDst.PixelFormat := pf32bit;
  bmpDst.Width       := Round(ABS(bmpSrc.Width * srac) + ABS(bmpSrc.Height * sras));
  bmpDst.Height      := Round(ABS(bmpSrc.Width * sras) + ABS(bmpSrc.Height * srac));

  MoveX   := (bmpDst.Width - bmpSrc.Width) div 2;
  MoveY   := (bmpDst.Height - bmpSrc.Height) div 2;
  CenterX := bmpSrc.Width div 2;
  CenterY := bmpSrc.Height div 2;

{$IFDEF WIN32}
  if iAngle mod 90 = 0 then
    Optimize06(bmpSrc, bmpDst, RotaryAngle, CenterX, CenterY, MoveX, MoveY, ras, rac)
  else
    Optimize07(bmpSrc, bmpDst, RotaryAngle, CenterX, CenterY, MoveX, MoveY, ras, rac);
{$ELSE}
  Optimize06(bmpSrc, bmpDst, RotaryAngle, CenterX, CenterY, MoveX, MoveY, ras, rac);
{$ENDIF}
end;

end.
