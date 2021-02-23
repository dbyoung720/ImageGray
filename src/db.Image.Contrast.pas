unit db.Image.Contrast;
{
  Func: 32位位图对比度
  Name: dbyoung@sina.com
  Date: 2021-2-22
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；
}

interface

uses Winapi.Windows, Vcl.Graphics, System.Threading, System.Math, db.Image.Common;

type
  TContrastType = (ctScanline, ctDelphi, ctTable, ctTableParallel, ctParallel, ctSSEParallel, ctSSE2, ctSSE4, ctAVX1, ctAVX2, ctAVX512knl, ctAVX512skx);

procedure Contrast(bmp: TBitmap; const intContrastValue: Integer; const ct: TContrastType = ctTableParallel);

implementation

procedure Contrast_ScanLine(bmp: TBitmap; const intContrastValue: Integer);
var
  X, Y  : Integer;
  pColor: PRGBQuad;
begin
  for Y := 0 to bmp.Height - 1 do
  begin
    pColor := bmp.ScanLine[Y];
    for X  := 0 to bmp.Width - 1 do
    begin
      pColor^.rgbRed   := EnsureRange(((pColor^.rgbRed - 128) * intContrastValue + 12800) div 100, 0, 255);
      pColor^.rgbGreen := EnsureRange(((pColor^.rgbGreen - 128) * intContrastValue + 12800) div 100, 0, 255);
      pColor^.rgbBlue  := EnsureRange(((pColor^.rgbBlue - 128) * intContrastValue + 12800) div 100, 0, 255);
      Inc(pColor);
    end;
  end;
end;

procedure Contrast_Delphi(bmp: TBitmap; const intContrastValue: Integer);
var
  I, Count: Integer;
  pColor  : PRGBQuad;
begin
  Count  := bmp.Width * bmp.Height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count - 1 do
  begin
    pColor^.rgbRed   := EnsureRange(((pColor^.rgbRed - 128) * intContrastValue div 100 + 128), 0, 255);
    pColor^.rgbGreen := EnsureRange(((pColor^.rgbGreen - 128) * intContrastValue div 100 + 128), 0, 255);
    pColor^.rgbBlue  := EnsureRange(((pColor^.rgbBlue - 128) * intContrastValue div 100 + 128), 0, 255);
    Inc(pColor);
  end;
end;

procedure Contrast_Table(bmp: TBitmap; const intContrastValue: Integer);
var
  I, Count: Integer;
  pColor  : PRGBQuad;
begin
  Count  := bmp.Width * bmp.Height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count - 1 do
  begin
    pColor^.rgbRed   := g_ContrastTable[pColor^.rgbRed, intContrastValue];
    pColor^.rgbGreen := g_ContrastTable[pColor^.rgbGreen, intContrastValue];
    pColor^.rgbBlue  := g_ContrastTable[pColor^.rgbBlue, intContrastValue];
    Inc(pColor);
  end;
end;

procedure Contrast_Table_Parallel_Proc(pColor: PRGBQuad; const intContrastValue, bmpWidth: Integer);
var
  X: Integer;
begin
  for X := 0 to bmpWidth - 1 do
  begin
    pColor^.rgbRed   := g_ContrastTable[pColor^.rgbRed, intContrastValue];
    pColor^.rgbGreen := g_ContrastTable[pColor^.rgbGreen, intContrastValue];
    pColor^.rgbBlue  := g_ContrastTable[pColor^.rgbBlue, intContrastValue];
    Inc(pColor);
  end;
end;

procedure Contrast_TableParallel(bmp: TBitmap; const intContrastValue: Integer);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      pColor: PRGBQuad;
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      Contrast_Table_Parallel_Proc(pColor, intContrastValue, bmp.Width);
    end);
end;

procedure Contrast_Parallel(bmp: TBitmap; const intContrastValue: Integer);
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
        pColor^.rgbRed := EnsureRange(((pColor^.rgbRed - 128) * intContrastValue div 100 + 128), 0, 255);
        pColor^.rgbGreen := EnsureRange(((pColor^.rgbGreen - 128) * intContrastValue div 100 + 128), 0, 255);
        pColor^.rgbBlue := EnsureRange(((pColor^.rgbBlue - 128) * intContrastValue div 100 + 128), 0, 255);
        Inc(pColor);
      end;
    end);
end;

procedure Contrast_SSEParallel_Proc(pColor: PRGBQuad; const intContrastValue, bmpWidth: Integer);
asm
  MOVSS   XMM0, [c_PixBGRAMask]             // XMM0 = |00000000|00000000|00000000|000000FF
  MOVSS   XMM1, [c_ContSSEMask]             // XMM1 = |00000000|00000000|00000000|00000080
  MOVD    XMM2, EDX                         // XMM2 = |00000000|00000000|00000000|intContrastValue
  MOVSS   XMM3, [c_ContSSETENX]             // XMM3 = |00000000|00000000|00000000|0000000A
  SHUFPS  XMM0, XMM0, 0                     // XMM0 = |000000FF|000000FF|000000FF|000000FF
  SHUFPS  XMM1, XMM1, 0                     // XMM1 = |00000080|00000080|00000080|00000080
  SHUFPS  XMM2, XMM2, 0                     // XMM2 = |intContrastValue|intContrastValue|intContrastValue|intContrastValue
  SHUFPS  XMM3, XMM3, 0                     // XMM3 = |0000000A|0000000A|0000000A|0000000A
  PXOR    XMM4, XMM4

@LOOP:
  MOVUPS  XMM5, [EAX]                       // XMM5 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM6, XMM5                        // XMM6 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM7, XMM5                        // XMM7 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|

  // 获取 4 个像素的 B3, B2, B1, B0
  ANDPS   XMM5, XMM0                        // XMM5 = |000000B3|000000B2|000000B1|000000B0|

  // 获取 4 个像素的 G3, G2, G1, G0
  PSRLD   XMM6, 8                           // XMM6 = |00A3R3G3|00A2R2G2|00A1R1G1|00A0R0G0|
  ANDPS   XMM6, XMM0                        // XMM6 = |000000G3|000000G2|000000G1|000000G0|

  // 获取 4 个像素的 R3, R2, R1, R0
  PSRLD   XMM7, 16                          // XMM7 = |0000A3R3|0000A2R2|0000A1R1|0000A0R0|
  ANDPS   XMM7, XMM0                        // XMM7 = |000000R3|000000R2|000000R1|000000R0|

  // 计算对比度
  PSUBD   XMM5, XMM1                        // XMM5 = (pColor^.rgbBlue - 128)
  PMULLD  XMM5, XMM2                        // XMM5 = (pColor^.rgbBlue - 128) * intContrastValue
  PMULLD  XMM5, XMM3                        // XMM5 = (pColor^.rgbBlue - 128) * intContrastValue * 655
  PSRAD   XMM5, 16                          // XMM5 = (pColor^.rgbBlue - 128) * intContrastValue * 655 / 65536  =  (pColor^.rgbBlue - 128) * intContrastValue / 100
  PADDD   XMM5, XMM1                        // XMM5 = (pColor^.rgbBlue - 128) * intContrastValue / 100 + 128
  PADDUSB XMM5, XMM4                        // 饱和加法，控制在 0---255 直接

  PSUBD   XMM6, XMM1                        // XMM6 = (pColor^.rgbGreen - 128)
  PMULLD  XMM6, XMM2                        // XMM6 = (pColor^.rgbGreen - 128) * intContrastValue
  PMULLD  XMM6, XMM3                        // XMM6 = (pColor^.rgbGreen - 128) * intContrastValue * 655
  PSRAD   XMM6, 16                          // XMM6 = (pColor^.rgbGreen - 128) * intContrastValue * 655 / 65536  =  (pColor^.rgbGreen - 128) * intContrastValue / 100
  PADDD   XMM6, XMM1                        // XMM6 = (pColor^.rgbGreen - 128) * intContrastValue / 100 + 128
  PADDUSB XMM6, XMM4

  PSUBD   XMM7, XMM1                        // XMM7 = (pColor^.rgbRed - 128)
  PMULLD  XMM7, XMM2                        // XMM7 = (pColor^.rgbRed - 128) * intContrastValue
  PMULLD  XMM7, XMM3                        // XMM7 = (pColor^.rgbRed - 128) * intContrastValue * 655
  PSRLD   XMM7, 16                          // XMM7 = (pColor^.rgbRed - 128) * intContrastValue * 655 / 65536  =  (pColor^.rgbRed - 128) * intContrastValue / 100
  PADDD   XMM7, XMM1                        // XMM7 = (pColor^.rgbRed - 128) * intContrastValue / 100 + 128
  PADDUSB XMM7, XMM4

  // 返回结果
  PSLLD   XMM6,  8                          // XMM6  = |0000Y300|0000Y200|0000Y100|0000Y000|
  PSLLD   XMM7,  16                         // XMM7  = |00Y30000|00Y20000|00Y10000|00Y00000|
  ORPS    XMM5,  XMM6                       // XMM5  = |0000Y3Y3|0000Y2Y2|0000Y1Y1|0000Y0Y0|
  ORPS    XMM5,  XMM7                       // XMM5  = |00Y3Y3Y3|00Y2Y2Y2|00Y1Y1Y1|00Y0Y0Y0|
  MOVUPS  [EAX], XMM5                       // [EAX] = XMM5

  ADD     EAX, 16                           // pColor 地址加 16，EAX 指向下4个像素的地址
  SUB     ECX, 4                            // Width 减 4, 每 4 个像素一循环
  JNZ     @LOOP                             // 循环
end;

procedure Contrast_SSEParallel(bmp: TBitmap; const intContrastValue: Integer);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      pColor: PRGBQuad;
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      Contrast_SSEParallel_Proc(pColor, intContrastValue, bmp.Width);
    end);
end;

procedure Contrast(bmp: TBitmap; const intContrastValue: Integer; const ct: TContrastType = ctTableParallel);
var
  pColor: PByte;
  pContr: PDWORD;
begin
  pColor := GetBitsPointer(bmp);
  pContr := GetBitsPointer(bmp);

  case ct of
    ctScanline:
      Contrast_ScanLine(bmp, intContrastValue);      // 105 ms
    ctDelphi:                                        //
      Contrast_Delphi(bmp, intContrastValue);        // 100 ms
    ctTable:                                         //
      Contrast_Table(bmp, intContrastValue);         //
    ctTableParallel:                                 //
      Contrast_TableParallel(bmp, intContrastValue); //
    ctParallel:                                      //
      Contrast_Parallel(bmp, intContrastValue);      //
    ctSSEParallel:
      Contrast_SSEParallel(bmp, intContrastValue);                                     //
    ctSSE2:                                                                            //
      bgraContrast_sse2(pColor, pContr, bmp.Width, bmp.Height, intContrastValue);      // 62 ms
    ctSSE4:                                                                            //
      bgraContrast_sse4(pColor, pContr, bmp.Width, bmp.Height, intContrastValue);      // 44 ms
    ctAVX1:                                                                            //
      bgraContrast_avx1(pColor, pContr, bmp.Width, bmp.Height, intContrastValue);      // 50 ms
    ctAVX2:                                                                            //
      bgraContrast_avx2(pColor, pContr, bmp.Width, bmp.Height, intContrastValue);      //
    ctAVX512knl:                                                                       //
      bgraContrast_avx512knl(pColor, pContr, bmp.Width, bmp.Height, intContrastValue); //
    ctAVX512skx:                                                                       //
      bgraContrast_avx512knl(pColor, pContr, bmp.Width, bmp.Height, intContrastValue); //
  end;
end;

end.
