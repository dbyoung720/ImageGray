unit db.Image.Saturation;

interface

uses Winapi.Windows, System.Math, System.Threading, Vcl.Graphics, db.Image.Common;

type
  TSaturationType = (stScanline, stDelphi, stParallel, stASM, stMMX, stSSEParallel, stSSE2, stSSE4, stAVX1, stAVX2, stAVX512knl, stAVX512skx);

procedure Saturation(bmp: TBitmap; const intSaturationValue: Integer; const st: TSaturationType = stAVX1);

implementation

procedure GetGrayAlpha(const intSaturationValue: Integer; var alpha: TAlpha; var grays: TGrays);
var
  X   : Integer;
  I   : Integer;
  Gray: Integer;
begin
  for I := 0 to 255 do
  begin
    alpha[I] := (I * intSaturationValue) shr 8;
  end;

  X     := 0;
  for I := 0 to 255 do
  begin
    Gray     := I - alpha[I];
    grays[X] := Gray;
    Inc(X);
    grays[X] := Gray;
    Inc(X);
    grays[X] := Gray;
    Inc(X);
  end;
end;

procedure Saturation_Scanline(bmp: TBitmap; const alpha: TAlpha; const grays: TGrays);
var
  X, Y  : Integer;
  pColor: PRGBQuad;
  Gray  : Integer;
begin
  for Y := 0 to bmp.height - 1 do
  begin
    pColor := bmp.ScanLine[Y];
    for X  := 0 to bmp.width - 1 do
    begin
      Gray             := grays[pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue];
      pColor^.rgbRed   := Max(0, Min(255, Gray + alpha[pColor^.rgbRed]));
      pColor^.rgbGreen := Max(0, Min(255, Gray + alpha[pColor^.rgbGreen]));
      pColor^.rgbBlue  := Max(0, Min(255, Gray + alpha[pColor^.rgbBlue]));
      Inc(pColor);
    end;
  end;
end;

procedure Saturation_Delphi(bmp: TBitmap; const alpha: TAlpha; const grays: TGrays);
var
  X     : Integer;
  pColor: PRGBQuad;
  Gray  : Integer;
begin
  pColor := GetBitsPointer(bmp);
  for X  := 0 to bmp.width * bmp.height - 1 do
  begin
    Gray             := grays[pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue];
    pColor^.rgbRed   := EnsureRange(Gray + alpha[pColor^.rgbRed], 0, 255);
    pColor^.rgbGreen := EnsureRange(Gray + alpha[pColor^.rgbGreen], 0, 255);
    pColor^.rgbBlue  := EnsureRange(Gray + alpha[pColor^.rgbBlue], 0, 255);
    Inc(pColor);
  end;
end;

procedure Saturation_Parallel(bmp: TBitmap; const alpha: TAlpha; const grays: TGrays);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.height - 1,
    procedure(Y: Integer)
    var
      X: Integer;
      Gray: Integer;
      pColor: PRGBQuad;
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      for X := 0 to bmp.width - 1 do
      begin
        Gray := grays[pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue];
        pColor^.rgbRed := EnsureRange(Gray + alpha[pColor^.rgbRed], 0, 255);
        pColor^.rgbGreen := EnsureRange(Gray + alpha[pColor^.rgbGreen], 0, 255);
        pColor^.rgbBlue := EnsureRange(Gray + alpha[pColor^.rgbBlue], 0, 255);
        Inc(pColor);
      end;
    end);
end;

procedure Saturation_SSEParallel_Proc(pColor: PRGBQuad; const bmpWidth: Integer; const Value, intSaturationValue: PInteger);
// var
// X   : Integer;
// Gray: Integer;
// begin
// for X := 0 to bmpWidth - 1 do
// begin
// Gray             := grays[pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue];
// pColor^.rgbRed   := EnsureRange(Gray + alpha[pColor^.rgbRed], 0, 255);
// pColor^.rgbGreen := EnsureRange(Gray + alpha[pColor^.rgbGreen], 0, 255);
// pColor^.rgbBlue  := EnsureRange(Gray + alpha[pColor^.rgbBlue], 0, 255);
// Inc(pColor);
// end;
asm
  MOVSS   XMM0, [Value]                     // XMM0 = 000000000000000000000000000Value
  MOVSS   XMM1, [intSaturationValue]        // XMM1 = 00000000000000intSaturationValue
  MOV     ECX,  EDX                         // ECX  = 循环计数
  MOVSS   XMM2, [c_PixBGRAMask]             // XMM2 = |00000000|00000000|00000000|000000FF
  SHUFPS  XMM0, XMM0, 0                     // XMM0 = |000Value|000Value|000Value|000Value
  SHUFPS  XMM1, XMM1, 0                     // XMM1 = |intSaturationValue|intSaturationValue|intSaturationValue|intSaturationValue
  SHUFPS  XMM2, XMM2, 0                     // XMM2 = |000000FF|000000FF|000000FF|000000FF

@LOOP:
  MOVUPS  XMM5, [EAX]                       // XMM5 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM6, XMM5                        // XMM6 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM7, XMM5                        // XMM7 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|

  // 获取 4 个像素的 B3, B2, B1, B0
  ANDPS   XMM5, XMM2                        // XMM5 = |000000B3|000000B2|000000B1|000000B0|

  // 获取 4 个像素的 G3, G2, G1, G0
  PSRLD   XMM6, 8                           // XMM6 = |00A3R3G3|00A2R2G2|00A1R1G1|00A0R0G0|
  ANDPS   XMM6, XMM2                        // XMM6 = |000000G3|000000G2|000000G1|000000G0|

  // 获取 4 个像素的 R3, R2, R1, R0
  PSRLD   XMM7, 16                          // XMM7 = |0000A3R3|0000A2R2|0000A1R1|0000A0R0|
  ANDPS   XMM7, XMM2                        // XMM7 = |000000R3|000000R2|000000R1|000000R0|

  // 计算饱和值
  PADDD   XMM5, XMM6                      // XMM5  = G+B
  PADDD   XMM5, XMM7                      // XMM5  = G+B+R

  // I - (I * intSaturationValue) shr 8

  // 返回结果
  PSLLD   XMM6,  8                           // XMM6  = |0000Y300|0000Y200|0000Y100|0000Y000|
  PSLLD   XMM7,  16                          // XMM7  = |00Y30000|00Y20000|00Y10000|00Y00000|
  ORPS    XMM5,  XMM6                        // XMM5  = |0000Y3Y3|0000Y2Y2|0000Y1Y1|0000Y0Y0|
  ORPS    XMM5,  XMM7                        // XMM5  = |00Y3Y3Y3|00Y2Y2Y2|00Y1Y1Y1|00Y0Y0Y0|
  MOVUPS  [EAX], XMM5                        // [EAX] = XMM5

  ADD     EAX, 16                           // pColor 地址加 16，EAX 指向下4个像素的地址
  SUB     ECX, 4                            // Width 减 4, 每 4 个像素一循环
  JNZ     @LOOP                             // 循环


end;

procedure Saturation_SSEParallel(bmp: TBitmap; const intSaturationValue: Integer);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.height - 1,
    procedure(Y: Integer)
    var
      pColor: PRGBQuad;
      Value: Integer;
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      Value := Ifthen(intSaturationValue mod 3 = 0, intSaturationValue div 3, intSaturationValue div 3 + 1);
      Saturation_SSEParallel_Proc(pColor, bmp.width, @Value, @intSaturationValue);
    end);
end;

procedure Saturation(bmp: TBitmap; const intSaturationValue: Integer; const st: TSaturationType = stAVX1);
var
  alpha : TAlpha;
  grays : TGrays;
  pColor: PByte;
  pSatur: PDWORD;
begin
  pColor := GetBitsPointer(bmp);
  pSatur := GetBitsPointer(bmp);
  GetGrayAlpha(intSaturationValue, alpha, grays);

  case st of
    stScanline:
      Saturation_Scanline(bmp, alpha, grays);
    stDelphi:
      Saturation_Delphi(bmp, alpha, grays);
    stParallel:
      Saturation_Parallel(bmp, alpha, grays);
    stASM:
      ;
    stMMX:
      ;
    stSSEParallel:
      Saturation_SSEParallel(bmp, intSaturationValue);
    stSSE2:
      bgraSaturation_sse2(pColor, pSatur, bmp.width, bmp.height, intSaturationValue, alpha, grays);
    stSSE4:
      bgraSaturation_sse4(pColor, pSatur, bmp.width, bmp.height, intSaturationValue, alpha, grays);
    stAVX1:
      bgraSaturation_avx1(pColor, pSatur, bmp.width, bmp.height, intSaturationValue, alpha, grays);
    stAVX2:
      bgraSaturation_avx2(pColor, pSatur, bmp.width, bmp.height, intSaturationValue, alpha, grays);
    stAVX512knl:
      bgraSaturation_avx512knl(pColor, pSatur, bmp.width, bmp.height, intSaturationValue, alpha, grays);
    stAVX512skx:
      bgraSaturation_avx512skx(pColor, pSatur, bmp.width, bmp.height, intSaturationValue, alpha, grays);
  end;
end;

end.
