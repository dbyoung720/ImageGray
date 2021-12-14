unit db.Image.Saturation;
{
  Func: 32位位图饱和度
  Name: dbyoung@sina.com
  Date: 2021-2-23
  Vers: Delphi 11
}

interface

uses Winapi.Windows, System.Threading, System.Math, Vcl.Graphics, db.Image.Common;

type
  TSaturationType = (stScanline, stDelphi, stTable, stParallel, stSSEParallel, stSSE2, stSSE4, stAVX1, stAVX2, stAVX512knl, stAVX512skx);

procedure Saturation(bmp: TBitmap; const intSaturationValue: Integer; const st: TSaturationType = stAVX1);

implementation

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

procedure Saturation_Table(bmp: TBitmap; const intSaturationValue: Integer);
var
  X     : Integer;
  pColor: PRGBQuad;
begin
  pColor := GetBitsPointer(bmp);
  for X  := 0 to bmp.width * bmp.height - 1 do
  begin
    pColor^.rgbRed   := g_SaturationTable[intSaturationValue, pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue, pColor^.rgbRed];
    pColor^.rgbGreen := g_SaturationTable[intSaturationValue, pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue, pColor^.rgbGreen];
    pColor^.rgbBlue  := g_SaturationTable[intSaturationValue, pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue, pColor^.rgbBlue];
    Inc(pColor);
  end;
end;

procedure Saturation_Parallel(bmp: TBitmap; const intSaturationValue: Integer);
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
      pColor: PRGBQuad;
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      for X := 0 to bmp.width - 1 do
      begin
        pColor^.rgbRed   := g_SaturationTable[intSaturationValue, pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue, pColor^.rgbRed];
        pColor^.rgbGreen := g_SaturationTable[intSaturationValue, pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue, pColor^.rgbGreen];
        pColor^.rgbBlue  := g_SaturationTable[intSaturationValue, pColor^.rgbRed + pColor^.rgbGreen + pColor^.rgbBlue, pColor^.rgbBlue];
        Inc(pColor);
      end;
    end);
end;

procedure Saturation_SSEParallel_Proc(pColor: PRGBQuad; const intSaturationValue, bmpWidth: Integer);
asm
  MOVSS   XMM0, [c_GraySSEDiv3]             // XMM0 = 00000000000000000000000000000055
  MOVD    XMM1, EDX                         // XMM1 = 00000000000000intSaturationValue
  MOVSS   XMM2, [c_PixBGRAMask]             // XMM2 = |00000000|00000000|00000000|000000FF
  SHUFPS  XMM0, XMM0, 0                     // XMM2 = |00000055|00000055|00000055|00000055
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

  // 计算饱和值                             // Gray = I - alpha[I] = I - (I * intSaturationValue) shr 8
  CMP     EDX,  255
  JGE     @Large

@Small:
  MOVAPS  XMM3, XMM5                        // XMM3  = XMM5
  PADDD   XMM5, XMM6                        // XMM5  = G+B
  PADDD   XMM5, XMM7                        // XMM5  = G+B+R
  PMULLW  XMM5, XMM0                        // XMM5  = (G+B+R)*85
  PSRLD   XMM5,  8                          // XMM5  = I = (G+B+R)*85/256 = (G+B+R) / 3
  MOVAPS  XMM4, XMM5                        // XMM4  = I = (G+B+R)*85/256 = (G+B+R) / 3
  PMULLW  XMM5, XMM1                        // XMM5  = I * intSaturationValue
  PSRLD   XMM5,  8                          // XMM5  = I * intSaturationValue >> 8
  PSUBW   XMM4, XMM5                        // XMM4  = Gray = I - alpha[I]

  PMULLW  XMM3, XMM1                        // XMM3 = pColor^.rgbBlue * intSaturationValue
  PSRLD   XMM3, 8                           // XMM3 = (pColor^.rgbBlue * intSaturationValue) >> 8 = alpha[pColor^.rgbBlue]
  PADDW   XMM3, XMM4                        // XMM3 = Gray + alpha[pColor^.rgbBlue]
  MOVAPS  XMM5, XMM3                        // XMM5 = Gray + alpha[pColor^.rgbBlue]
  PXOR    XMM3, XMM3                        // XMM3 = |00000000|00000000|00000000|00000000|
  PADDUSB XMM5, XMM3                        // XMM5 控制在 0 --- 255 之间

  PMULLW  XMM6, XMM1                        // XMM6 = pColor^.rgbGreen * intSaturationValue
  PSRLD   XMM6, 8                           // XMM6 = (pColor^.rgbGreen * intSaturationValue) >> 8 = alpha[pColor^.rgbGreen]
  PADDW   XMM6, XMM4                        // XMM6 = Gray + alpha[pColor^.rgbGreen]
  PADDUSB XMM6, XMM3                        // XMM6 控制在 0 --- 255 之间

  PMULLW  XMM7, XMM1                        // XMM7 = pColor^.rgbRed * intSaturationValue
  PSRLD   XMM7, 8                           // XMM7 = (pColor^.rgbRed * intSaturationValue) >> 8 = alpha[pColor^.rgbRed]
  PADDW   XMM7, XMM4                        // XMM7 = Gray + alpha[pColor^.rgbRed]
  PADDUSB XMM7, XMM3                        // XMM7 控制在 0 --- 255 之间
  JMP     @Result

@Large:
  MOVAPS  XMM3, XMM5                        // XMM3  = XMM5
  PADDD   XMM5, XMM6                        // XMM5  = G+B
  PADDD   XMM5, XMM7                        // XMM5  = G+B+R
  PMULLW  XMM5, XMM0                        // XMM5  = (G+B+R)*85
  PSRLD   XMM5,  8                          // XMM5  = I = (G+B+R)*85/256 = (G+B+R) / 3
  MOVAPS  XMM4, XMM5                        // XMM4  = I = (G+B+R)*85/256 = (G+B+R) / 3
  PMULLW  XMM5, XMM1                        // XMM5  = I * intSaturationValue
  PSRLD   XMM5,  8                          // XMM5  = I * intSaturationValue >> 8
  PSUBW   XMM4, XMM5                        // XMM4  = Gray = I - alpha[I]

  PMULLW  XMM3, XMM1                        // XMM3 = pColor^.rgbBlue * intSaturationValue
  PSRLD   XMM3, 8                           // XMM3 = (pColor^.rgbBlue * intSaturationValue) >> 8 = alpha[pColor^.rgbBlue]
  PADDW   XMM3, XMM4                        // XMM3 = Gray + alpha[pColor^.rgbBlue]
  MOVAPS  XMM5, XMM3                        // XMM5 = Gray + alpha[pColor^.rgbBlue]
  PXOR    XMM3, XMM3                        // XMM3 = |00000000|00000000|00000000|00000000|
  PADDUSB XMM5, XMM3                        // XMM5 控制在 0 --- 255 之间

  PMULLW  XMM6, XMM1                        // XMM6 = pColor^.rgbGreen * intSaturationValue
  PSRLD   XMM6, 8                           // XMM6 = (pColor^.rgbGreen * intSaturationValue) >> 8 = alpha[pColor^.rgbGreen]
  PADDW   XMM6, XMM4                        // XMM6 = Gray + alpha[pColor^.rgbGreen]
  PADDB   XMM6, XMM3                        // XMM6 控制在 0 --- 255 之间

  PMULLW  XMM7, XMM1                        // XMM7 = pColor^.rgbRed * intSaturationValue
  PSRLD   XMM7, 8                           // XMM7 = (pColor^.rgbRed * intSaturationValue) >> 8 = alpha[pColor^.rgbRed]
  PADDW   XMM7, XMM4                        // XMM7 = Gray + alpha[pColor^.rgbRed]
  PADDB   XMM7, XMM3                        // XMM7 控制在 0 --- 255 之间

  // 返回结果
@Result:
  PSLLD   XMM6,  8                          // XMM6  = |0000Y300|0000Y200|0000Y100|0000Y000|
  PSLLD   XMM7,  16                         // XMM7  = |00Y30000|00Y20000|00Y10000|00Y00000|
  ORPS    XMM5,  XMM6                       // XMM5  = |0000Y3Y3|0000Y2Y2|0000Y1Y1|0000Y0Y0|
  ORPS    XMM5,  XMM7                       // XMM5  = |00Y3Y3Y3|00Y2Y2Y2|00Y1Y1Y1|00Y0Y0Y0|
  MOVUPS  [EAX], XMM5                       // [EAX] = XMM5

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
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      Saturation_SSEParallel_Proc(pColor, intSaturationValue, bmp.width);
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
    stTable:
      Saturation_Table(bmp, intSaturationValue);
    stParallel:
      Saturation_Parallel(bmp, intSaturationValue);
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
