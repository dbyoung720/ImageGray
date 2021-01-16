unit db.Image.Light;

interface

uses Winapi.Windows, Vcl.Graphics, db.Image.Common;

{$IFDEF WIN32}
{$LINK obj\x86\light.obj}
{$LINK obj\x86\light_sse2.obj}
{$LINK obj\x86\light_sse4.obj}
{$LINK obj\x86\light_avx.obj}
{$LINK obj\x86\light_avx2.obj}
{$LINK obj\x86\light_avx512knl.obj}
{$LINK obj\x86\light_avx512skx.obj}
{$ELSE}
{$LINK obj\x64\light.obj}
{$LINK obj\x64\light_sse2.obj}
{$LINK obj\x64\light_sse4.obj}
{$LINK obj\x64\light_avx.obj}
{$LINK obj\x64\light_avx2.obj}
{$LINK obj\x64\light_avx512knl.obj}
{$LINK obj\x64\light_avx512skx.obj}
{$IFEND}

type
  TLightType = (ltScanline, ltDelphi, ltASM, ltMMX, ltSSE, ltSSE2, ltSSE4, ltAVX, ltAVX2, ltAVX512);

procedure Light(bmp: TBitmap; const intLightValue: Integer; const lt: TLightType = ltAVX);

implementation

procedure light_sse2(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_light_sse2'{$IFEND};
procedure light_sse4(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_light_sse4'{$IFEND};
procedure light_avx(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_light_avx'{$IFEND};
procedure light_avx2(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_light_avx2'{$IFEND};
procedure light_avx512skx(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_light_avx512skx'{$IFEND};
procedure light_avx512knl(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_light_avx512knl'{$IFEND};

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
      pColor^.rgbRed   := g_LightTable[pColor^.rgbRed, intLightValue];
      pColor^.rgbGreen := g_LightTable[pColor^.rgbGreen, intLightValue];
      pColor^.rgbBlue  := g_LightTable[pColor^.rgbBlue, intLightValue];
      Inc(pColor);
    end;
  end;
end;

procedure Light_Delphi(bmp: TBitmap; const intLightValue: Integer);
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

procedure Light_ASM_Proc(pSrc: PRGBQuad; const intLightValue, Count: Integer); register;
asm

end;

procedure Light_ASM(bmp: TBitmap; const intLightValue: Integer);
begin
  Light_ASM_Proc(GetBitsPointer(bmp), intLightValue, bmp.width * bmp.height);
end;

procedure Light(bmp: TBitmap; const intLightValue: Integer; const lt: TLightType = ltAVX);
begin
  case lt of
    ltScanline:
      Light_ScanLine(bmp, intLightValue);                                         // 105 ms
    ltDelphi:                                                                     //
      Light_Delphi(bmp, intLightValue);                                           // 100 ms
    ltASM:                                                                        //
      Light_ASM(bmp, intLightValue);                                              //
    ltMMX:                                                                        //
      ;                                                                           //
    ltSSE:                                                                        //
      ;                                                                           //
    ltSSE2:                                                                       //
      light_sse2(GetBitsPointer(bmp), bmp.width, bmp.height, intLightValue);      // 62 ms
    ltSSE4:                                                                       //
      light_sse4(GetBitsPointer(bmp), bmp.width, bmp.height, intLightValue);      // 44 ms
    ltAVX:                                                                        //
      light_avx(GetBitsPointer(bmp), bmp.width, bmp.height, intLightValue);       // 50 ms
    ltAVX2:                                                                       //
      light_avx2(GetBitsPointer(bmp), bmp.width, bmp.height, intLightValue);      //
    ltAVX512:                                                                     //
      light_avx512knl(GetBitsPointer(bmp), bmp.width, bmp.height, intLightValue); //
  end;
end;

end.
