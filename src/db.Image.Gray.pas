unit db.Image.Gray;
{
  Func: 32位位图灰值化
  Name: dbyoung@sina.com
  Date: 2020-10-01
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；

  基本原理：
  Gray = (R + G + B) / 3  = (R + G + B) * 85 / 255 = (R + G + B) * 85 >> 8
  Gray = R*0.299 + G*0.587 + B*0.114

  定点优化：
  Gray = (R*77   + G*151  + B*28)   >> 8
  Gray = (R*0x4D + G*0x97 + B*0x1C) >> 8

  查表优化：
  R、G、B，都在 0---255 之间，可以将 R(0---255)*77、G(0---255)*151、B(0---255)*28 预先计算好，存放在表中，优化掉乘法
}

{$IFDEF WIN32}
{$LINK obj\x86\gray.obj}
{$LINK obj\x86\gray_sse2.obj}
{$LINK obj\x86\gray_sse4.obj}
{$LINK obj\x86\gray_avx.obj}
{$LINK obj\x86\gray_avx2.obj}
{$LINK obj\x86\gray_avx512knl.obj}
{$LINK obj\x86\gray_avx512skx.obj}
{$ELSE}
{$LINK obj\x64\gray.obj}
{$LINK obj\x64\gray_sse2.obj}
{$LINK obj\x64\gray_sse4.obj}
{$LINK obj\x64\gray_avx.obj}
{$LINK obj\x64\gray_avx2.obj}
{$LINK obj\x64\gray_avx512knl.obj}
{$LINK obj\x64\gray_avx512skx.obj}
{$IFEND}

interface

uses Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils, System.Threading, System.Diagnostics, System.SyncObjs, Vcl.Graphics, Winapi.GDIPOBJ, Winapi.GDIPAPI, db.Image.Common;

type
  TGrayType = (gtAPI, gtScanLine, gtDelphi, gtFourPoint, gtParallel, gtGDIPLUS, gtTable, gtASM, gtMMX, gtSSE, gtSSE2, gtSSE4, gtAVX, gtAVX2, gtAVX512, gtGPU, gtOther);

procedure Gray(bmp: TBitmap; const gt: TGrayType = gtSSE4);

implementation

procedure rgb2gray_sse2(src: PByte; Width, Height: Integer); cdecl; external {$IFDEF WIN32}name '_rgb2gray_sse2'{$IFEND};
procedure rgb2gray_sse4(src: PByte; Width, Height: Integer); cdecl; external {$IFDEF WIN32}name '_rgb2gray_sse4'{$IFEND};
procedure rgb2gray_avx(src: PByte; Width, Height: Integer); cdecl; external {$IFDEF WIN32}name '_rgb2gray_avx'{$IFEND};
procedure rgb2gray_avx2(src: PByte; Width, Height: Integer); cdecl; external {$IFDEF WIN32}name '_rgb2gray_avx2'{$IFEND};
procedure rgb2gray_avx512skx(src: PByte; Width, Height: Integer); cdecl; external {$IFDEF WIN32}name '_rgb2gray_avx512skx'{$IFEND};
procedure rgb2gray_avx512knl(src: PByte; Width, Height: Integer); cdecl; external {$IFDEF WIN32}name '_rgb2gray_avx512knl'{$IFEND};

{ 220 ms }
procedure Gray_API(bmp: TBitmap);
var
  I, Count: Cardinal;
  pColor  : PRGBQuad;
  byeGray : Byte;
begin
  Count := bmp.Width * bmp.Height;
  GetMem(pColor, Count * 4);
  try
    GetBitmapBits(bmp.Handle, Count * 4, pColor);
    for I := 0 to Count - 1 do
    begin
      byeGray := Round(0.3 * pColor^.rgbRed + 0.59 * pColor^.rgbGreen + 0.11 * pColor^.rgbBlue);
      pColor^ := TRGBQuad(RGB(byeGray, byeGray, byeGray));
      Inc(pColor);
    end;
    Dec(pColor, Count);
    SetBitmapBits(bmp.Handle, Count * 4, pColor);
  finally
    FreeMem(pColor);
  end;
end;

{ 119 ms }
procedure Gray_ScanLine(bmp: TBitmap);
var
  I, J  : Integer;
  pColor: PRGBQuad;
begin
  for I := 0 to bmp.Height - 1 do
  begin
    pColor := bmp.ScanLine[I];
    for J  := 0 to bmp.Width - 1 do
    begin
      pColor^ := GetPixelGray(pColor^.rgbRed, pColor^.rgbGreen, pColor^.rgbBlue);
      Inc(pColor);
    end;
  end;
end;

{ 88 ms }
procedure Gray_Delphi(bmp: TBitmap);
var
  I, Count: Integer;
  pColor  : PRGBQuad;
begin
  Count  := bmp.Width * bmp.Height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count - 1 do
  begin
    pColor^ := GetPixelGray(pColor^.rgbRed, pColor^.rgbGreen, pColor^.rgbBlue);
    Inc(pColor);
  end;
end;

{ 92 ms }
procedure Gray_FourPoint(bmp: TBitmap);
var
  I, J, Count: Integer;
  pColor     : PRGBQuad;
begin
  Count  := bmp.Width * bmp.Height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count div 4 - 1 do
  begin
    for J := 0 to 3 do
    begin
      pColor^ := GetPixelGray(pColor^.rgbRed, pColor^.rgbGreen, pColor^.rgbBlue);
      Inc(pColor);
    end;
  end;
end;

{ 45 ms  需要脱离 IDE 执行 }
procedure Gray_Parallel(bmp: TBitmap);
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
        pColor^ := GetPixelGray(pColor^.rgbRed, pColor^.rgbGreen, pColor^.rgbBlue);
        Inc(pColor);
      end;
    end);
end;

{ 1036 ms }
procedure Gray_GDIPLUS(bmp: TBitmap);
var
  img: TGPImage;
  iab: TGPImageAttributes;
  gpg: TGPGraphics;
begin
  img := TGPBitmap.Create(bmp.Handle, bmp.Palette);
  gpg := TGPGraphics.Create(bmp.Canvas.Handle);
  iab := TGPImageAttributes.Create;
  try
    iab.SetColorMatrix(c_GrayColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
    gpg.DrawImage(img, TGPRect(bmp.Canvas.ClipRect), 0, 0, bmp.Width, bmp.Height, UnitPixel, iab);
  finally
    iab.Free;
    gpg.Free;
    img.Free;
  end;
end;

{ 80 ms }
procedure Gray_Table(bmp: TBitmap);
var
  I, Count: Integer;
  pColor  : PRGBQuad;
begin
  Count  := bmp.Width * bmp.Height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count - 1 do
  begin
    pColor^ := GetPixelGray(pColor^.rgbRed, pColor^.rgbGreen, pColor^.rgbBlue);
    Inc(pColor);
  end;
end;

procedure Gray_ASM_Proc_x86(pColor: PRGBQuad; const Count: Integer); register;
asm
  {$IFDEF WIN32}
  MOV    ECX, EDX                       // ECX = Count 循环计数 EDX (Count) 赋给 ECX；通常将 ECX 作为计数器来使用。只是约定俗成，不是标准
@LOOP:                                  // 循环；EAX 中存在着 pColor 的首地址
  MOVZX  EBX, [EAX].TRGBQuad.RGBRed     // EBX = pColor^.rgbRed
  MOVZX  EDX, [EAX].TRGBQuad.rgbGreen   // EDX = pColor^.rgbGreen
  MOVZX  ESI, [EAX].TRGBQuad.rgbBlue    // ESI = pColor^.rgbBlue
  MOV    EBX, [EBX*4 + c_GrayR77]       // EBX = c_GrayR77[pColor^.rgbRed]
  MOV    EDX, [EDX*4 + c_GrayG151]      // EDX = c_GrayG151[pColor^.rgbGreen]
  MOV    ESI, [ESI*4 + c_GrayB28]       // ESI = c_GrayB28[pColor^.rgbBlue]
  ADD    EBX, EDX                       // EBX = c_GrayR77[pColor^.rgbRed] + c_GrayG151[pColor^.rgbGreen]
  ADD    EBX, ESI                       // EBX = c_GrayR77[pColor^.rgbRed] + c_GrayG151[pColor^.rgbGreen] + c_GrayB28[pColor^.rgbBlue]
  SHR    EBX, 8                         // EBX = (c_GrayR77[pColor^.rgbRed] + c_GrayG151[pColor^.rgbGreen] + c_GrayB28[pColor^.rgbBlue]) shr 8;
  MOV    EBX, [EBX*4 + c_GrayValue]     // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV    [EAX],  EBX                    // [EAX] = TRGBQuad(c_GrayValue[byeGray])
  ADD    EAX, 4                         // EAX   = 指向下一个像素
  DEC    ECX                            // Count 减一
  JNZ    @LOOP                          // 循环
  {$IFEND}
end;

{ 26 ms }
procedure Gray_ASM(bmp: TBitmap);
begin
  Gray_ASM_Proc_x86(GetBitsPointer(bmp), bmp.Width * bmp.Height);
end;

{ 36 ms }
procedure Gray_MMX_Proc_P0(pColor: PByte; const Count: Integer); register;
asm
  {$IFDEF WIN32}
  EMMS
  MOV        ECX,   EDX                          // ECX = Count 循环计数 EDX (Count) 赋给 ECX；
  PXOR       MM7,   MM7                          // MM7 = $0000000000000000
  MOVQ       MM6,   c_GrayMMXARGB                // MM6 = $0000004D0095001C

@LOOP:                                           // 循环；EAX 中存在着 pColor 的首地址
  MOVD       MM0,   [EAX]                        // MM0 = $0000000000RRGGBB
  PUNPCKLBW  MM0,   MM7                          // MM0 = $000000RR00GG00BB
  PMADDWD    MM0,   MM6                          // MM0 = A * 0 + R * 77 | G * 151 + B * 28

  // 取出 MM0 的低位和高位相加，即为灰度值
  MOVD       EDX,   MM0                          // EDX = G * 151 + B * 28
  PSRLQ      MM0,   32                           // MM0 = R * 77
  MOVD       EBX,   MM0                          // EBX = R * 77
  ADD        EBX,   EDX                          // EBX = R * 77 + G * 151 + B * 28
  SHR        EBX,   8                            // EBX = (R * 77 + G * 151 + B * 28) >> 8

  // 用查表法，计算像素的灰度值
  MOV        EBX,   [EBX*4 + c_GrayValue]        // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV        [EAX], EBX                          // [EAX] = TRGBQuad(c_GrayValue[byeGray])

  // 循环，计算下一个像素灰度值
  ADD        EAX,   4                            // EAX = 指向下一个像素
  SUB        ECX,   4                            // ECX = Count 减 4
  JNZ        @LOOP                               // 循环

  EMMS
  {$IFEND}
end;

{ 25 ms }
procedure Gray_MMX_Proc_P1(pColor: PByte; const Count: Integer); register;
asm
  EMMS
  MOV        ECX,  EDX             // ECX = Count 循环计数 EDX (Count) 赋给 ECX；
  PXOR       MM7,  MM7             // MM7 = $0000000000000000
  MOVQ       MM6,  c_GrayMMXARGB   // MM6 = $0000004D0095001C
  MOVQ       MM5,  c_GrayMMXAdd    // MM5 = $0001000100010001
  MOVQ       MM4,  c_GrayMMXAMask  // MM4 = $FF000000FF000000

@LOOP:                             // 循环；EAX 中存在着 pColor 的首地址
  MOVD       MM0,   [EAX+0]        // MM0 = $0000000000R0G0B0
  PUNPCKLBW  MM0,   MM7            // MM0 = $000000R000G000B0
  PMADDWD    MM0,   MM6            // MM0 = A0 * 0 + R0 * 77 | G0 * 151 + B0 * 28

  MOVD       MM1,   [EAX+4]        // MM1 = $0000000000R1G1B1
  PUNPCKLBW  MM1,   MM7            // MM1 = $000000R100G100B1
  PMADDWD    MM1,   MM6            // MM1 = A1 * 0 + R1 * 77 | G1 * 151 + B1 * 28

  PACKSSDW   MM0,   MM1            // MM0 = A1 * 0 + R1 * 77 | G1 * 151 + B1 * 28 | A0 * 0 + R0 * 77 | G0 * 151 + B0 * 28;  MM0, MM1 合并进 MM0
  PMADDWD    MM0,   MM5            // MM0 = A1 * 0 + R1 * 77 + G1 * 151 + B1 * 28 | A0 * 0 + R0 * 77 + G0 * 151 + B0 * 28;  MM0 高位、地位相加，得到 P0,P1 的灰度值

  // 取 P0, P1 的灰度值，放入 MM0 中，返回结果
  PSRLD      MM0,   8              // MM0 = 00 00 00 Gray1  00 00 00 Gray0
  MOVQ       MM1,   MM0            // MM1 = 00 00 00 Gray1  00 00 00 Gray0
  MOVQ       MM2,   MM0            // MM2 = 00 00 00 Gray1  00 00 00 Gray0
  PSLLD      MM1,   8              // MM1 = 00 00 Gray1 00  00 00 Gray0 00
  POR        MM0,   MM4            // MM0 = FF 00 00 GRAY1 FF 00 00 GRAY0
  PSLLD      MM2,   16             // MM2 = 00 Gray1 00 00  00 Gray0 00 00
  POR        MM0,   MM1            // MM0 = FF 00 GRAY1 GRAY1 FF 00 GRAY0 GRAY0
  POR        MM0,   MM2            // MM0 = FF Gray1 Gray1 Gray1  FF Gray0 Gray0 Gray0
  MOVQ       [EAX], MM0            // [EAX] = 返回结果

  // 循环，计算下一个像素灰度值
  ADD        EAX,   8              // EAX = 指向下 2 个像素
  SUB        ECX,   8              // ECX = Count 减 8
  JNZ        @LOOP                 // 循环
  EMMS
end;

{ 23 ms }
procedure Gray_MMX_Proc_P2(pColor: PByte; Count: Integer); register;
asm
  {$IFDEF WIN32}
  EMMS
  SUB        EDX,  16                             // Count 需要能被 24 整除 4096*4096*4 mod 24 = 16
  MOV        ECX,  EDX                            // ECX = Count 循环计数 EDX (Count) 赋给 ECX；
  PXOR       MM7,  MM7                            // MM7 = $0000000000000000
  MOVQ       MM6,  c_GrayMMXARGB                  // MM6 = $0000004D0095001C

@LOOP:                                            // 循环；EAX 中存在着 pColor 的首地址
  MOVD       MM0,   [EAX+4*0]                     // MM0 = $0000000000R0G0B0
  MOVD       MM1,   [EAX+4*1]                     // MM1 = $0000000000R1G1B1
  MOVD       MM2,   [EAX+4*2]                     // MM2 = $0000000000R2G2B2
  MOVD       MM3,   [EAX+4*3]                     // MM3 = $0000000000R3G3B3
  MOVD       MM4,   [EAX+4*4]                     // MM4 = $0000000000R4G4B4
  MOVD       MM5,   [EAX+4*5]                     // MM5 = $0000000000R5G5B5

  PUNPCKLBW  MM0,   MM7                           // MM0 = $000000R000G000B0
  PMADDWD    MM0,   MM6                           // MM0 = A0 * 00 + R0 * 77 | G0 * 151 + B0 * 28
  PUNPCKLBW  MM1,   MM7                           // MM1 = $000000R100G100B1
  PMADDWD    MM1,   MM6                           // MM1 = A1 * 00 + R1 * 77 | G1 * 151 + B1 * 28
  PUNPCKLBW  MM2,   MM7                           // MM2 = $000000R200G200B2
  PMADDWD    MM2,   MM6                           // MM2 = A2 * 00 + R2 * 77 | G2 * 151 + B2 * 28
  PUNPCKLBW  MM3,   MM7                           // MM3 = $000000R300G300B3
  PMADDWD    MM3,   MM6                           // MM3 = A3 * 00 + R3 * 77 | G3 * 151 + B3 * 28
  PUNPCKLBW  MM4,   MM7                           // MM4 = $000000R400G400B4
  PMADDWD    MM4,   MM6                           // MM4 = A4 * 00 + R4 * 77 | G4 * 151 + B4 * 28
  PUNPCKLBW  MM5,   MM7                           // MM5 = $000000R500G500B5
  PMADDWD    MM5,   MM6                           // MM5 = A5 * 00 + R5 * 77 | G5 * 151 + B5 * 28

  MOVD       EDX,   MM0                           // EDX = G0 * 151 + B0 * 28
  PSRLQ      MM0,   32                            // MM0 = R0 * 77
  MOVD       EBX,   MM0                           // EBX = R0 * 77
  ADD        EBX,   EDX                           // EBX = R0 * 77 + G0 * 151 + B0 * 28
  SHR        EBX,   8                             // EBX = (R0 * 77 + G0 * 151 + B0 * 28) >> 8
  MOV        EBX,   [EBX*4 + c_GrayValue]         // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV        [EAX+4*0], EBX                       // [EAX+4*0] = TRGBQuad(c_GrayValue[byeGray])

  MOVD       EDX,   MM1                           // EDX = G1 * 151 + B1 * 28
  PSRLQ      MM1,   32                            // MM1 = R1 * 77
  MOVD       EBX,   MM1                           // EBX = R1 * 77
  ADD        EBX,   EDX                           // EBX = R1 * 77 + G1 * 151 + B1 * 28
  SHR        EBX,   8                             // EBX = (R1 * 77 + G1 * 151 + B1 * 28) >> 8
  MOV        EBX,   [EBX*4 + c_GrayValue]         // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV        [EAX+4*1], EBX                       // [EAX+4*1] = TRGBQuad(c_GrayValue[byeGray])

  MOVD       EDX,   MM2                           // EDX = G2 * 151 + B2 * 28
  PSRLQ      MM2,   32                            // MM2 = R2 * 77
  MOVD       EBX,   MM2                           // EBX = R2 * 77
  ADD        EBX,   EDX                           // EBX = R2 * 77 + G2 * 151 + B2 * 28
  SHR        EBX,   8                             // EBX = (R2 * 77 + G2 * 151 + B2 * 28) >> 8
  MOV        EBX,   [EBX*4 + c_GrayValue]         // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV        [EAX+4*2], EBX                       // [EAX+4*2] = TRGBQuad(c_GrayValue[byeGray])

  MOVD       EDX,   MM3                           // EDX = G3 * 151 + B3 * 28
  PSRLQ      MM3,   32                            // MM3 = R3 * 77
  MOVD       EBX,   MM3                           // EBX = R3 * 77
  ADD        EBX,   EDX                           // EBX = R3 * 77 + G3 * 151 + B3 * 28
  SHR        EBX,   8                             // EBX = (R3 * 77 + G3 * 151 + B3 * 28) >> 8
  MOV        EBX,   [EBX*4 + c_GrayValue]         // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV        [EAX+4*3], EBX                       // [EAX+4*3] = TRGBQuad(c_GrayValue[byeGray])

  MOVD       EDX,   MM4                           // EDX = G4 * 151 + B4 * 28
  PSRLQ      MM4,   32                            // MM4 = R4 * 77
  MOVD       EBX,   MM4                           // EBX = R4 * 77
  ADD        EBX,   EDX                           // EBX = R4 * 77 + G4 * 151 + B4 * 28
  SHR        EBX,   8                             // EBX = (R4 * 77 + G4 * 151 + B4 * 28) >> 8
  MOV        EBX,   [EBX*4 + c_GrayValue]         // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV        [EAX+4*4], EBX                       // [EAX+4*4] = TRGBQuad(c_GrayValue[byeGray])

  MOVD       EDX,   MM5                           // EDX = G5 * 151 + B5 * 28
  PSRLQ      MM5,   32                            // MM5 = R5 * 77
  MOVD       EBX,   MM5                           // EBX = R5 * 77
  ADD        EBX,   EDX                           // EBX = R5 * 77 + G5 * 151 + B5 * 28
  SHR        EBX,   8                             // EBX = (R5 * 77 + G5 * 151 + B5 * 28) >> 8
  MOV        EBX,   [EBX*4 + c_GrayValue]         // EBX = TRGBQuad(c_GrayValue[byeGray])
  MOV        [EAX+4*5], EBX                       // [EAX+4*5] = TRGBQuad(c_GrayValue[byeGray])

  ADD        EAX,   24                            // EAX   = 指向下6个像素
  SUB        ECX,   24                            // Count 减 24
  JNZ        @LOOP                                // 循环
  EMMS
  {$IFEND}
end;

{ 35 ms }
procedure Gray_MMX_Proc_P3(pColor: PByte; const Count: Integer); register;
asm
  EMMS
  MOVQ     MM7, c_GrayMMXMask0
  MOVQ     MM6, c_GrayMMXRB
  MOVQ     MM5, c_GrayMMXKG_
  MOVQ     MM4, c_GrayMMXMask1
  MOV      ECX, EDX

@LOOP:                           // 循环；EAX 中存在着 pColor 的首地址
  MOVQ     MM0, [EAX+0]          // MM0 = |A1|R1|G1|B1|A0|R0|G0|B0|
  MOVQ     MM1, MM0              // MM1 = |A1|R1|G1|B1|A0|R0|G0|B0|
  MOVQ     MM2, [EAX+8]          // MM2 = |A3|R3|G3|B3|A2|R2|G2|B2|
  MOVQ     MM3, MM2              // MM3 = |A3|R3|G3|B3|A2|R2|G2|B2|

  // 77*R + 28*B
  PAND     MM0, MM7              // MM0 = |00|R1|00|B1|00|R0|00|B0|
  PAND     MM2, MM7              // MM2 = |00|R3|00|B3|00|R2|00|B2|
  PMADDWD  MM0, MM6              // MM0 = |R1*77 + B1*28|R0*77 + B0*28|
  PMADDWD  MM2, MM6              // MM2 = |R3*77 + B3*28|R2*77 + B2*28|

  // 151*G
  PAND     MM1, MM4              // MM1 = |0 |0 |G1|0 |0 |0 |G0|0 |
  PAND     MM3, MM4              // MM3 = |0 |0 |G3|0 |0 |0 |G2|0 |
  PSRLW    MM1, 8                // MM1 = |0 |0 |0 |G1|0 |0 |0 |G0|
  PSRLW    MM3, 8                // MM3 = |0 |0 |0 |G3|0 |0 |0 |G2|
  PACKSSDW MM1, MM3              // MM1 = |0 |G3|0 |G2|0 |G1|0 |G0|
  PMULLW   MM1, MM5              // MM1 = |G3*151 |G2*151 |G1*151 |G0*151 |

  // 77*R + 28*B + 151*G
  PACKSSDW MM0, MM2              // MM0 = |  RB3  |  RB2  |  RB1  |  RB0  |
  PADDW    MM0, MM1              // MM0 = |  RGB3 |  RGB2 |  RGB1 |  RGB0 |
  PSRLW    MM0, 8                // MM0 = | GRAY3 | GRAY2 | GRAY1 | GRAY0 |

  // 分别取出 GRAY3, GRAY2 / GRAY1, GRAY0
  MOVQ     MM3,   MM0            // MM1 = | GRAY3 | GRAY2 | GRAY1 | GRAY0 |
  MOVQ     MM1,   MM0            // MM1 = | GRAY3 | GRAY2 | GRAY1 | GRAY0 |

  // 取出 GRAY1, GRAY0
  PSLLQ        MM0,   32         // MM0 = | GRAY1 | GRAY0 | 0 | 0 |
  PSRLQ        MM0,   32         // MM0 = | 0 | 0 | GRAY1 | GRAY0 |
  PUNPCKLBW    MM0,   MM4        // MM0 = | 0 | GRAY1 | 0 | GRAY0 |
  MOVQ         MM1,   MM0        // MM1 = 00 00 00 Gray1  00 00 00 Gray0
  MOVQ         MM2,   MM0        // MM2 = 00 00 00 Gray1  00 00 00 Gray0
  PSLLD        MM1,   8          // MM1 = 00 00 Gray1 00  00 00 Gray0 00
  PSLLD        MM2,   16         // MM2 = 00 Gray1 00 00  00 Gray0 00 00
  POR          MM0,   MM1        // MM0 = 00 00 GRAY1 GRAY1 00 00 GRAY0 GRAY0
  POR          MM0,   MM2        // MM0 = 00 Gray1 Gray1 Gray1 00 Gray0 Gray0 Gray0
  MOVQ         [EAX], MM0        // [EAX] = 返回结果

  // 取出 GRAY3, GRAY2
  PSLLQ        MM3,   32         // MM0 = | GRAY3 | GRAY2 | 0 | 0 |
  PSRLQ        MM3,   32         // MM0 = | 0 | 0 | GRAY3 | GRAY2 |
  PUNPCKLBW    MM3,   MM4        // MM0 = | 0 | GRAY3 | 0 | GRAY2 |
  MOVQ         MM1,   MM3        // MM1 = 00 00 00 Gray3  00 00 00 Gray2
  MOVQ         MM2,   MM3        // MM2 = 00 00 00 Gray3  00 00 00 Gray2
  PSLLD        MM1,   8          // MM1 = 00 00 Gray3 00  00 00 Gray2 00
  PSLLD        MM2,   16         // MM2 = 00 Gray3 00 00  00 Gray2 00 00
  POR          MM3,   MM1        // MM0 = 00 00 GRAY3 GRAY3 00 00 GRAY2 GRAY2
  POR          MM3,   MM2        // MM0 = 00 Gray3 Gray3 Gray3 00 Gray2 Gray2 Gray2
  MOVQ        [EAX+8], MM3       // [EAX + 8] = 返回结果

  // 循环，计算下 4 个像素灰度值
  ADD      EAX, 16               // EAX = Inc(pColor);    pColor 地址加16，即下4个像素
  SUB      ECX, 16               // ECX = Count 减 16
  JNZ      @LOOP                 // 循环
  EMMS
end;

{ 36 ms }
procedure Gray_MMX(bmp: TBitmap);
begin
  Gray_MMX_Proc_P2(GetBitsPointer(bmp), bmp.Width * bmp.Height * 4);
end;

{
  SSE 优化
  XMM0-----XMM7 8 个 128bit 寄存器
  4个像素可以同时运算
  GRAY = (R+G+B) div 3 = (R+G+B) * 85 / 255 = (R+G+B) * $55 >> 8
}

procedure Gray_SSE_Proc_01(pColor: PRGBQuad; const Count: Integer); register;
asm
  {$IFDEF WIN64}
  MOV     RAX,  RCX
  {$IFEND}
  MOV     ECX,  EDX
  MOVSS   XMM1, [c_GraySSEMask]           // XMM1 = 000000000000000000000000000000FF
  SHUFPS  XMM1, XMM1, 0                   // XMM1 = 000000FF000000FF000000FF000000FF
  MOVSS   XMM2, [c_GraySSEDiv3]           // XMM2 = 00000000000000000000000000000055
  SHUFPS  XMM2, XMM2, 0                   // XMM2 = 00000055000000550000005500000055
  MOVAPS  XMM3, XMM1                      // XMM3 = 000000FF000000FF000000FF000000FF
  PSLLD   XMM3, 24                        // XMM3 = FF000000FF000000FF000000FF000000

@LOOP:
  MOVUPS  XMM0, [EAX]                     // XMM0 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM4, XMM0                      // XMM4 = XMM0
  MOVAPS  XMM5, XMM0                      // XMM5 = XMM0
  MOVAPS  XMM6, XMM0                      // XMM6 = XMM0

  // 获取 4 个像素的 B3, B2, B1, B0
  ANDPS   XMM4, XMM1                      // XMM4 = |000000B3|000000B2|000000B1|000000B0|

  // 获取 4 个像素的 G3, G2, G1, G0
  PSRLD   XMM5, 8                         // XMM5 = |00A3R3G3|00A2R2G2|00A1R1G1|00A0R0G0|
  ANDPS   XMM5, XMM1                      // XMM5 = |000000G3|000000G2|000000G1|000000G0|

  // 获取 4 个像素的 R3, R2, R1, R0
  PSRLD   XMM6, 16                        // XMM6 = |0000A3R3|0000A2R2|0000A1R1|0000A0R0|
  ANDPS   XMM6, XMM1                      // XMM6 = |000000R3|000000R2|000000R1|000000R0|

  // 计算灰度值
  PADDD   XMM4, XMM5                      // XMM4  = G+B
  PADDD   XMM4, XMM6                      // XMM4  = G+B+R
  PMULLW  XMM4, XMM2                      // XMM4  = (G+B+R) * $55
  PSRLD   XMM4, 8                         // XMM4  = (G+B+R) * $55 >> 8 = (G+B+R) /3 = |000000Y3|000000Y2|000000Y1|000000Y0|

  // 返回结果
  MOVAPS  XMM5, XMM4                      // XMM5  = |000000Y3|000000Y2|000000Y1|000000Y0|
  PSLLD   XMM5, 8                         // XMM5  = |0000Y300|0000Y200|0000Y100|0000Y000|
  ORPS    XMM4, XMM5                      // XMM4  = |0000Y3Y3|0000Y2Y2|0000Y1Y1|0000Y0Y0|
  PSLLD   XMM5, 8                         // XMM5  = |00Y30000|00Y20000|00Y10000|00Y00000|
  ORPS    XMM4, XMM5                      // XMM4  = |00Y3Y3Y3|00Y2Y2Y2|00Y1Y1Y1|00Y0Y0Y0|
  ANDPS   XMM0, XMM3                      // XMM0  = |FF000000|FF000000|FF000000|FF000000|
  ORPS    XMM0, XMM4                      // XMM0  = |FFRGBGR3|FFRGBGR2|FFRGBGR1|FFRGBGR0|
  MOVUPS  [EAX], XMM0                     // [EAX] = XMM0

  ADD     EAX, 16                         // pColor 地址加 16，EAX 指向下4个像素的地址
  SUB     ECX, 16                         // Count 减 16
  JNZ     @LOOP                           // 循环
end;

{ 17 ms }
procedure Gray_SSE(bmp: TBitmap);
begin
  Gray_SSE_Proc_01(GetBitsPointer(bmp), bmp.Width * bmp.Height * 4);
end;

{ 30 ms }
procedure Gray_SSE2(bmp: TBitmap);
var
  pSrc: PByte;
begin
  pSrc := GetBitsPointer(bmp);
  rgb2gray_sse2(pSrc, bmp.Width, bmp.Height);
end;

{ 30 ms }
procedure Gray_SSE4(bmp: TBitmap);
var
  pSrc: PByte;
begin
  pSrc := GetBitsPointer(bmp);
  rgb2gray_sse4(pSrc, bmp.Width, bmp.Height);
end;

{ 30 ms }
procedure Gray_AVX(bmp: TBitmap);
var
  pSrc: PByte;
begin
  pSrc := GetBitsPointer(bmp);
  rgb2gray_avx(pSrc, bmp.Width, bmp.Height);
end;

{ 30 ms }
procedure Gray_AVX2(bmp: TBitmap);
var
  pSrc: PByte;
begin
  pSrc := GetBitsPointer(bmp);
  rgb2gray_avx2(pSrc, bmp.Width, bmp.Height);
end;

procedure Gray_AVX512(bmp: TBitmap);
var
  pSrc: PByte;
begin
  pSrc := GetBitsPointer(bmp);
  rgb2gray_avx512knl(pSrc, bmp.Width, bmp.Height);
end;

procedure Gray_GPU(bmp: TBitmap);
begin

end;

procedure Gray_Other_Proc(pColor: Pointer; const Width: Integer; var p32t);
asm
  {$IFDEF WIN32}
  PUSH  EDI
  PUSH  EBX

  MOV   EDI,   EAX
  MOV   EBX,   EDX
  SHL   EBX,   2
  ADD   EBX,   EAX
@LOOP:
  MOVZX EAX,   BYTE[EDI+2]
  MOVZX EDX,   BYTE[EDI+1]
  SHL   EDX,   2
  ADD   EAX,   EDX
  MOVZX EDX,   BYTE[EDI+0]
  ADD   EAX,   EDX
  ADD   EAX,   EDX
  MOV   EAX,   [EAX*4+ECX]
  MOV   [EDI], EAX
  ADD   EDI,   4
  CMP   EBX,   EDI
  JNZ   @LOOP

  POP   EBX
  POP   EDI
  {$ELSE}
  PUSH  RDI
  PUSH  RBX

  MOV   RDI,   RCX
  MOV   RBX,   RDX
  SHL   RBX,   2
  ADD   RBX,   RAX
@LOOP:
  MOVZX RAX,   BYTE[RDI+2]
  MOVZX RDX,   BYTE[RDI+1]
  SHL   RDX,   2
  ADD   RAX,   RDX
  MOVZX RDX,   BYTE[RDI+0]
  ADD   RAX,   RDX
  ADD   RAX,   RDX
  MOV   RAX,   [RAX*4+RCX]
  MOV   [RDI], RAX
  ADD   RDI,   4
  CMP   RBX,   RDI
  JNZ   @LOOP

  POP   RBX
  POP   RDI
  {$IFEND}
end;

{ 47 ms }
procedure Gray_Other(bmp: TBitmap);
var
  I            : Integer;
  pColor       : PRGBQuad;
  intWidthBytes: Integer;
begin
  pColor        := bmp.ScanLine[0];
  intWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);
  for I         := 0 to bmp.Height - 1 do
  begin
    Gray_Other_Proc(pColor, bmp.Width, g_GrayTable);
    pColor := Pointer(Integer(pColor) + intWidthBytes);
  end;
end;

procedure Gray(bmp: TBitmap; const gt: TGrayType = gtSSE4);
begin
  case gt of
    gtAPI:
      Gray_API(bmp);
    gtScanLine:
      Gray_ScanLine(bmp);
    gtDelphi:
      Gray_Delphi(bmp);
    gtFourPoint:
      Gray_FourPoint(bmp);
    gtParallel:
      Gray_Parallel(bmp);
    gtGDIPLUS:
      Gray_GDIPLUS(bmp);
    gtTable:
      Gray_Table(bmp);
    gtASM:
      Gray_ASM(bmp);
    gtMMX:
      Gray_MMX(bmp);
    gtSSE:
      Gray_SSE(bmp);
    gtSSE2:
      Gray_SSE2(bmp);
    gtSSE4:
      Gray_SSE4(bmp);
    gtAVX:
      Gray_AVX(bmp);
    gtAVX2:
      Gray_AVX2(bmp);
    gtAVX512:
      Gray_AVX512(bmp);
    gtGPU:
      Gray_GPU(bmp);
    gtOther:
      Gray_Other(bmp);
  end;
end;

end.
