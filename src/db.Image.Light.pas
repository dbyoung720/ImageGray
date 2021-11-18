unit db.Image.Light;
{
  Func: 32位位图亮度调节
  Name: dbyoung@sina.com
  Date: 2020-10-01
  Vers: Delphi 11

  基本原理：
  R、G、B 同时添加/减少一个值。并保证 R、G、B 在 0 --- 255 之间。
}

interface

uses Winapi.Windows, Vcl.Graphics, System.Threading, System.Math, db.Image.Common;

type
  TLightType = (ltScanline, ltDelphi, ltTable, ltASM, ltParallel, ltParallel_SSE, ltParallel_AVX2);

procedure Light(bmp: TBitmap; const intLightValue: Integer; const lt: TLightType = ltParallel_SSE);

implementation

procedure Light_ScanLine(bmp: TBitmap; const intLightValue: Integer);
var
  I, J  : Integer;
  pColor: PRGBQuad;
begin
  for I := 0 to bmp.Height - 1 do
  begin
    pColor := bmp.ScanLine[I];
    for J  := 0 to bmp.Width - 1 do
    begin
      pColor^.rgbRed   := EnsureRange(pColor^.rgbRed + intLightValue, 0, 255);
      pColor^.rgbGreen := EnsureRange(pColor^.rgbGreen + intLightValue, 0, 255);
      pColor^.rgbBlue  := EnsureRange(pColor^.rgbBlue + intLightValue, 0, 255);
      Inc(pColor);
    end;
  end;
end;

{ 120ms ---- 170ms }
procedure Light_Delphi(bmp: TBitmap; const intLightValue: Integer);
var
  I, Count: Integer;
  pColor  : PRGBQuad;
begin
  Count  := bmp.Width * bmp.Height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count - 1 do
  begin
    pColor^.rgbRed   := EnsureRange(pColor^.rgbRed + intLightValue, 0, 255);
    pColor^.rgbGreen := EnsureRange(pColor^.rgbGreen + intLightValue, 0, 255);
    pColor^.rgbBlue  := EnsureRange(pColor^.rgbBlue + intLightValue, 0, 255);
    Inc(pColor);
  end;
end;

{ 87ms }
procedure Light_Table(bmp: TBitmap; const intLightValue: Integer);
var
  I, Count: Integer;
  pColor  : PRGBQuad;
begin
  Count  := bmp.Width * bmp.Height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count - 1 do
  begin
    pColor^.rgbRed   := g_LightTable[pColor^.rgbRed, intLightValue];
    pColor^.rgbGreen := g_LightTable[pColor^.rgbGreen, intLightValue];
    pColor^.rgbBlue  := g_LightTable[pColor^.rgbBlue, intLightValue];
    Inc(pColor);
  end;
end;

{ 60ms ---- 90ms }
procedure Light_ASM_Proc(pColor: PRGBQuad; const intLightValue: Integer; const Count: Integer); register;
asm
  {$IFDEF WIN32}
  PUSH   EDI
  MOV    EDI, EDX

@LOOP:
  MOVZX  EBX, [EAX].TRGBQuad.RGBRed     // EBX = pColor^.rgbRed
  MOVZX  EDX, [EAX].TRGBQuad.rgbGreen   // EDX = pColor^.rgbGreen
  MOVZX  ESI, [EAX].TRGBQuad.rgbBlue    // ESI = pColor^.rgbBlue

  // R G B 增加 intLightValue
  ADD    ESI, EDI
  ADD    EDX, EDI
  ADD    EBX, EDI

  // 判断 R 是否在 0---255 之间
  CMP    EBX, 0
  JL     @RRL
  CMP    EBX, 255
  JG     @RRG
  JMP    @RValue
@RRL:
  MOV    EBX, 0
  JMP    @RValue
@RRG:
  MOV    EBX, 255

  // 判断 G 是否在 0---255 之间
@RValue:
  CMP    EDX, 0
  JL     @GGL
  CMP    EDX, 255
  JG     @GGG
  JMP    @GValue
@GGL:
  MOV    EDX, 0
  JMP    @GValue
@GGG:
  MOV    EDX, 255

  // 判断 B 是否在 0---255 之间
@GValue:
  CMP    ESI, 0
  JL     @BBL
  CMP    ESI, 255
  JG     @BBG
  JMP    @BValue
@BBL:
  MOV    ESI, 0
  JMP    @BValue
@BBG:
  MOV    ESI, 255

  // 组合 R G B
@BValue:
  SHL EBX, 16   // EBX = 00RR0000
  SHL EDX, 8    // EDX = 0000GG00
  OR EBX, EDX   // EBX = 00RRGG00
  OR EBX, ESI   // EBX = 00RRGGBB
  MOV [EAX], EBX

  ADD    EAX, 4
  DEC    ECX
  JNZ    @LOOP

  POP    EDI
  {$IFEND}
end;

procedure Light_ASM(bmp: TBitmap; const intLightValue: Integer);
var
  pColor: PRGBQuad;
  Count : Integer;
begin
  pColor := GetBitsPointer(bmp);
  Count  := bmp.Width * bmp.Height;
  Light_ASM_Proc(pColor, intLightValue, Count);
end;

procedure Light_Parallel_Proc(pColor: PRGBQuad; const bmpWidth, intLightValue: Integer);
var
  I: Integer;
begin
  for I := 0 to bmpWidth - 1 do
  begin
    pColor^.rgbRed   := EnsureRange(pColor^.rgbRed + intLightValue, 0, 255);
    pColor^.rgbGreen := EnsureRange(pColor^.rgbGreen + intLightValue, 0, 255);
    pColor^.rgbBlue  := EnsureRange(pColor^.rgbBlue + intLightValue, 0, 255);
    Inc(pColor);
  end;
end;

{ 20ms --- 40ms  需要脱离 IDE 执行 / ScanLine 不能用于 TParallel.For 中 }
procedure Light_Parallel(bmp: TBitmap; const intLightValue: Integer);
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
      Light_Parallel_Proc(pColor, bmp.Width, intLightValue);
    end);
end;

procedure Light_Parallel_SSE_Proc01(pColor: PByte; const intLightValue, bmpWidth: Integer);
asm
  {$IFDEF WIN64}
  XCHG    RAX,  RCX
  {$IFEND}
  MOVSS   XMM1, [c_PixBGRAMask]             // XMM1 = 000000000000000000000000000000FF
  MOVD    XMM2, EDX                         // XMM2 = 0000000000000000000intLightValue
  SHUFPS  XMM1, XMM1, 0                     // XMM1 = |000000FF|000000FF|000000FF|000000FF|
  SHUFPS  XMM2, XMM2, 0                     // XMM2 = |intLightValue|intLightValue|intLightValue|intLightValue|
  MOVAPS  XMM3, XMM1                        // XMM3 = |000000FF|000000FF|000000FF|000000FF|
  PSUBB   XMM3, XMM2                        // XMM3 = |000000FF - intLightValue|000000FF - intLightValue|000000FF - intLightValue|000000FF - intLightValue|

@LOOP:
  MOVUPS  XMM4, [EAX]                       // XMM4 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM5, XMM4                        // XMM5 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM6, XMM4                        // XMM6 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM7, XMM4                        // XMM7 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|

  // 获取 4 个像素的 B3, B2, B1, B0
  ANDPS   XMM5, XMM1                        // XMM5 = |000000B3|000000B2|000000B1|000000B0|

  // 获取 4 个像素的 G3, G2, G1, G0
  PSRLD   XMM6, 8                           // XMM6 = |00A3R3G3|00A2R2G2|00A1R1G1|00A0R0G0|
  ANDPS   XMM6, XMM1                        // XMM6 = |000000G3|000000G2|000000G1|000000G0|

  // 获取 4 个像素的 R3, R2, R1, R0
  PSRLD   XMM7, 16                          // XMM7 = |0000A3R3|0000A2R2|0000A1R1|0000A0R0|
  ANDPS   XMM7, XMM1                        // XMM7 = |000000R3|000000R2|000000R1|000000R0|

  // 计算亮度值(饱和加法)
  CMP EDX, 0
  JL  @Small

@Large:
  PADDUSB   XMM5, XMM2                      // XMM5 = |B3+intLightValue|B2+intLightValue|B1+intLightValue|B0+intLightValue|
  PADDUSB   XMM6, XMM2                      // XMM6 = |G3+intLightValue|G2+intLightValue|G1+intLightValue|G0+intLightValue|
  PADDUSB   XMM7, XMM2                      // XMM7 = |R3+intLightValue|R2+intLightValue|R1+intLightValue|R0+intLightValue|
  JMP       @RValue

@Small:
  PSUBUSB   XMM5, XMM3                      // XMM5 = |B3 - (000000FF - intLightValue)|B2 - (000000FF - intLightValue)|B1 - (000000FF - intLightValue)|B0 - (000000FF - intLightValue)|
  PSUBUSB   XMM6, XMM3                      // XMM6 = |G3 - (000000FF - intLightValue)|G2 - (000000FF - intLightValue)|G1 - (000000FF - intLightValue)|G0 - (000000FF - intLightValue)|
  PSUBUSB   XMM7, XMM3                      // XMM7 = |R3 - (000000FF - intLightValue)|R2 - (000000FF - intLightValue)|R1 - (000000FF - intLightValue)|R0 - (000000FF - intLightValue)|

  // 返回结果
@RValue:
  PSLLD   XMM6, 8                           // XMM6  = |0000G300|0000G200|0000G100|0000G000|
  PSLLD   XMM7, 16                          // XMM7  = |00R30000|00R20000|00R10000|00R00000|
  ORPS    XMM5, XMM6                        // XMM5  = |0000G3B3|0000G2B2|0000G1B1|0000G0B0|
  ORPS    XMM5, XMM7                        // XMM5  = |00R3G3B3|00R2G2B2|00R1G1B1|00R0G0B0|
  MOVUPS  [EAX], XMM5                       // [EAX] = XMM5

  ADD     EAX, 16                           // pColor 地址加 16，EAX 指向下4个像素的地址
  SUB     ECX, 4                            // Width 减 4, 每 4 个像素一循环
  JNZ     @LOOP                             // 循环
end;

procedure Light_Parallel_SSE_Proc02(pColor: PByte; const intLightValue, bmpWidth: Integer);
asm
  {$IFDEF WIN64}
  XCHG RAX,  RCX
  {$IFEND}

  CMP  EDX, 0
  JLE  @Small

@Large:
  MOV     DH,    DL
  PINSRW  XMM1,  EDX,  0
  PINSRW  XMM1,  EDX,  1
  PSHUFD  XMM1,  XMM1, 0
@LOOP01:
  MOVUPS  XMM4,  [EAX]
  PADDUSB XMM4,  XMM1
  MOVUPS  [EAX], XMM4
  ADD     EAX,   16
  SUB     ECX,   4
  JNZ     @LOOP01
  ret

@Small:
  NEG     DL
  MOV     DH,    DL
  PINSRW  XMM1,  EDX,  0
  PINSRW  XMM1,  EDX,  1
  PSHUFD  XMM1,  XMM1, 0
@LOOP02:
  MOVUPS  XMM4,  [EAX]
  PSUBUSB XMM4,  XMM1
  MOVUPS  [EAX], XMM4
  ADD     EAX,   16
  SUB     ECX,   4
  JNZ     @LOOP02
end;

{ 4 ms  需要脱离 IDE 执行 / ScanLine 不能用于 TParallel.For 中 }
procedure Light_Parallel_SSE(bmp: TBitmap; const intLightValue: Integer);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      pColor: PByte;
    begin
      pColor := PByte(StartScanLine + Y * bmpWidthBytes);
      Light_Parallel_SSE_Proc02(pColor, intLightValue, bmp.Width);
    end);
end;

procedure Light_Parallel_AVX2_Proc(pColor: PByte; const intLightValue, bmpWidth: Integer);
asm
  {$IFDEF WIN64}
  XCHG RAX,  RCX
  {$IFEND}

  CMP  EDX, 0
  JLE  @Small

@Large:
  MOV          DH,    DL
  PINSRW       XMM1,  EDX,  0
  PINSRW       XMM1,  EDX,  1
  PSHUFD       XMM1,  XMM1, 0
  VINSERTF128  YMM1,  YMM1, XMM1, 1
@Loop01:
  VMOVUPS  YMM0,  [EAX]
  VPADDUSB YMM0,  YMM1, YMM0
  VMOVUPS  [EAX], YMM0
  ADD      EAX,   32
  SUB      ECX,   8
  JNZ      @Loop01
  ret

@Small:
  NEG          DL
  MOV          DH,    DL
  PINSRW       XMM1,  EDX,  0
  PINSRW       XMM1,  EDX,  1
  PSHUFD       XMM1,  XMM1, 0
  VINSERTF128  YMM1,  YMM1, XMM1, 1
@Loop02:
  VMOVUPS  YMM0,  [EAX]
  VPSUBUSB YMM0,  YMM0, YMM1
  VMOVUPS  [EAX], YMM0
  ADD      EAX,   32
  SUB      ECX,   8
  JNZ      @Loop02
end;

{ 4 ms  需要脱离 IDE 执行 / ScanLine 不能用于 TParallel.For 中 }
procedure Light_Parallel_AVX2(bmp: TBitmap; const intLightValue: Integer);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      pColor: PByte;
    begin
      pColor := PByte(StartScanLine + Y * bmpWidthBytes);
      Light_Parallel_AVX2_Proc(pColor, intLightValue, bmp.Width);
    end);
end;

procedure Light(bmp: TBitmap; const intLightValue: Integer; const lt: TLightType = ltParallel_SSE);
begin
  case lt of
    ltScanline:
      Light_ScanLine(bmp, intLightValue);
    ltDelphi:
      Light_Delphi(bmp, intLightValue);
    ltTable:
      Light_Table(bmp, intLightValue);
    ltASM:
      Light_ASM(bmp, intLightValue);
    ltParallel:
      Light_Parallel(bmp, intLightValue);
    ltParallel_SSE:
      Light_Parallel_SSE(bmp, intLightValue);
    ltParallel_AVX2:
      Light_Parallel_AVX2(bmp, intLightValue);
  end;
end;

end.
