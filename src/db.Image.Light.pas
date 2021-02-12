unit db.Image.Light;

interface

uses Winapi.Windows, Vcl.Graphics, System.Math, db.Image.Common;

type
  TLightType = (ltScanline, ltDelphi, ltTable, ltASM, ltMMX, ltSSE, ltSSE2, ltSSE4, ltAVX1, ltAVX2, ltAVX512knl, ltAVX512skx);

procedure Light(bmp: TBitmap; const intLightValue: Integer; const lt: TLightType = ltAVX1);

implementation

procedure Light_ScanLine(bmp: TBitmap; const intLightValue: Integer);
var
  I, J  : Integer;
  pColor: PRGBQuad;
begin
  for I := 0 to bmp.height - 1 do
  begin
    pColor := bmp.ScanLine[I];
    for J  := 0 to bmp.width - 1 do
    begin
      pColor^.rgbRed   := CheckValue(pColor^.rgbRed, intLightValue);
      pColor^.rgbGreen := CheckValue(pColor^.rgbGreen, intLightValue);
      pColor^.rgbBlue  := CheckValue(pColor^.rgbBlue, intLightValue);
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
  Count  := bmp.width * bmp.height;
  pColor := GetBitsPointer(bmp);
  for I  := 0 to Count - 1 do
  begin
    pColor^.rgbRed   := CheckValue(pColor^.rgbRed, intLightValue);
    pColor^.rgbGreen := CheckValue(pColor^.rgbGreen, intLightValue);
    pColor^.rgbBlue  := CheckValue(pColor^.rgbBlue, intLightValue);
    Inc(pColor);
  end;
end;

{ 87ms }
procedure Light_Table(bmp: TBitmap; const intLightValue: Integer);
var
  I, Count: Integer;
  pColor  : PRGBQuad;
begin
  Count  := bmp.width * bmp.height;
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
end;

procedure Light_ASM(bmp: TBitmap; const intLightValue: Integer);
var
  pColor: PRGBQuad;
  Count : Integer;
begin
  pColor := GetBitsPointer(bmp);
  Count  := bmp.width * bmp.height;
  Light_ASM_Proc(pColor, intLightValue, Count);
end;

procedure Light(bmp: TBitmap; const intLightValue: Integer; const lt: TLightType = ltAVX1);
var
  pColor: PByte;
  pLight: PDWORD;
begin
  pColor := GetBitsPointer(bmp);
  pLight := GetBitsPointer(bmp);
  case lt of
    ltScanline:
      Light_ScanLine(bmp, intLightValue);
    ltDelphi:
      Light_Delphi(bmp, intLightValue);
    ltTable:
      Light_Table(bmp, intLightValue);
    ltASM:
      Light_ASM(bmp, intLightValue);
    ltMMX:
      ;
    ltSSE:
      ;
    ltSSE2:
      bgraLight_sse2(pColor, pLight, bmp.width, bmp.height, intLightValue);
    ltSSE4:
      bgraLight_sse4(pColor, pLight, bmp.width, bmp.height, intLightValue);
    ltAVX1:
      bgraLight_avx1(pColor, pLight, bmp.width, bmp.height, intLightValue);
    ltAVX2:
      bgraLight_avx2(pColor, pLight, bmp.width, bmp.height, intLightValue);
    ltAVX512knl:
      bgraLight_avx512knl(pColor, pLight, bmp.width, bmp.height, intLightValue);
    ltAVX512skx:
      bgraLight_avx512skx(pColor, pLight, bmp.width, bmp.height, intLightValue);
  end;
end;

end.
