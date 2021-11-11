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

procedure Rotate_Proc01(const krx, kry, IndexRow: Integer; const srcBits: PRGBQuadArray; dstBits: PRGBQuadArray; const rac, ras: Integer; const dstWidth, srcWidth, srcHeight: DWORD); assembler; inline;
var
  X         : Integer;
  SrcX, SrcY: DWORD;
begin
  for X := dstWidth - 1 downto 0 do
  begin
    SrcX := (X * rac - krx) shr 16;
    SrcY := (X * ras - kry) shr 16;
    if (SrcY < srcHeight) and (SrcX < srcWidth) then
    begin
      dstBits[IndexRow * Integer(dstWidth) + X] := srcBits[SrcY * srcWidth + SrcX];
    end;
  end;
end;

{ ASM }
procedure Rotate_Proc02(const krx, kry, IndexRow: Integer; const srcBits: PRGBQuadArray; dstBits: PRGBQuadArray; const rac, ras: Integer; const dstWidth, srcWidth, srcHeight: DWORD); assembler;
asm
  {$IFDEF  WIN32}
  MOV  [EBP-$04], EAX          // [EBP-$04] = krx
  MOV  [EBP-$08], EDX          // [EBP-$08] = kry
  MOV  EBX, ECX                // EBX = IndexRow
  MOV  ECX, dstWidth           // ECX = dstWidth 循环计数

@LOOP:
  MOV   EAX,  ECX               // EAX = X
  IMUL  EAX,  rac               // EAX = X * rac
  SUB   EAX,  [EBP-$04]         // EAX = X * rac - krx
  SHR   EAX,  16                // EAX = (X * rac - krx) shr 16  = SrcX

  MOV   EDX,  ECX               // EDX = X
  IMUL  EDX,  ras               // EDX = X * ras
  SUB   EDX,  [EBP-$08]         // EDX = X * ras - kry
  SHR   EDX,  16                // EDX = (X * ras - kry) shr 16  = SrcY

  CMP   EAX,  srcWidth          // IF SrcX < srcWidth
  JNB   @NEXT                   //
  CMP   EDX,  srcHeight         // IF SrcY < srcHeight
  JNB   @NEXT                   //

  MOV  EDI,  EDX                // EDI = (X * ras - kry) shr 16  = SrcY
  IMUL EDI,  srcWidth           // EDI = SrcY * srcWidth
  ADD  EDI,  EAX                // EDI = SrcY * srcWidth + SrcX
  MOV  EDX,  [srcBits]          // EDI = [srcBits]
  MOV  EDI,  [EDX + EDI * 4]    // EDI = srcBits[SrcY * srcWidth + SrcX]

  MOV  EDX,  EBX                // EDX = IndexRow
  IMUL EDX,  dstWidth           // EDX = IndexRow * dstWidth
  ADD  EDX,  ECX                // EDX = IndexRow * dstWidth + X
  MOV  ESI,  [dstBits]          // ESI = [dstBits]
  MOV  [ESI + EDX * 4], EDI     // dstBits[IndexRow * Integer(dstWidth) + X] = EDI

@NEXT:
  DEC  ECX
  JNZ  @LOOP
  MOV  ESP,  EBP
  {$IFEND}
END;

{ SSE }
procedure Rotate_Proc03(const krx, kry, IndexRow: Integer; const srcBits: PRGBQuadArray; dstBits: PRGBQuadArray; const rac, ras: Integer; const dstWidth, srcWidth, srcHeight: DWORD); assembler;
asm
  {$IFDEF WIN32}
  EMMS
  MOV   EBX, ECX                                    // EBX = IndexRow
  MOV   ECX, dstWidth                               // ECX = dstWidth 循环计数
  PXOR  XMM7,   XMM7                                // MM7 = $0000000000000000
  MOVQ  XMM6,   c_GrayMMXARGB                       // MM6 = rac, -1, ras, -1

@LOOP:                                              //
  MOVD       XMM0,   ECX                            // MM0 = x, krx, x, kry
  PMADDWD    XMM0,   XMM6                           // MM0 = x * rac + krx * -1 | x * ras + kry * -1


@NEXT:
  DEC ECX
  JNZ @LOOP

  EMMS
  {$IFEND}
end;

{ 并行 + SIMD 优化 }
procedure Optimize06(bmpSrc, bmpDst: TBitmap; const RotaryAngle: double; const CenterX, CenterY, MoveX, MoveY: Integer);
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
    procedure(IndexRow: Integer)
    var
      krx, kry: Integer;
    begin
      krx := kcx + IndexRow * ras;
      kry := kcy - IndexRow * rac;
      Rotate_Proc02(krx, kry, IndexRow, srcBits, dstBits, rac, ras, dstWidth, srcWidth, srcHeight);
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

  Optimize06(bmpSrc, bmpDst, RotaryAngle, CenterX, CenterY, MoveX, MoveY);
end;

end.
