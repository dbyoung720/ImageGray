unit db.Image.Contrast;

interface

uses Winapi.Windows, Vcl.Graphics, System.Math, db.Image.Common;

{$IFDEF WIN32}
{$LINK obj\x86\Contrast.obj}
{$LINK obj\x86\Contrast_sse2.obj}
{$LINK obj\x86\Contrast_sse4.obj}
{$LINK obj\x86\Contrast_avx.obj}
{$LINK obj\x86\Contrast_avx2.obj}
{$LINK obj\x86\Contrast_avx512knl.obj}
{$LINK obj\x86\Contrast_avx512skx.obj}
{$ELSE}
{$LINK obj\x64\Contrast.obj}
{$LINK obj\x64\Contrast_sse2.obj}
{$LINK obj\x64\Contrast_sse4.obj}
{$LINK obj\x64\Contrast_avx.obj}
{$LINK obj\x64\Contrast_avx2.obj}
{$LINK obj\x64\Contrast_avx512knl.obj}
{$LINK obj\x64\Contrast_avx512skx.obj}
{$IFEND}

type
  TContrastType = (ctScanline, ctDelphi, ctASM, ctMMX, ctSSE, ctSSE2, ctSSE4, ctAVX, ctAVX2, ctAVX512);

procedure Contrast(bmp: TBitmap; const intContrastValue: Integer; const ct: TContrastType = ctAVX);

implementation

procedure Contrast_sse2(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_Contrast_sse2'{$IFEND};
procedure Contrast_sse4(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_Contrast_sse4'{$IFEND};
procedure Contrast_avx(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_Contrast_avx'{$IFEND};
procedure Contrast_avx2(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_Contrast_avx2'{$IFEND};
procedure Contrast_avx512skx(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_Contrast_avx512skx'{$IFEND};
procedure Contrast_avx512knl(src: PByte; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_Contrast_avx512knl'{$IFEND};

procedure Contrast_ScanLine(bmp: TBitmap; const intContrastValue: Integer);
var
  X, Y  : Integer;
  pColor: PRGBQuad;
  value : Integer;
begin
  for Y := 0 to bmp.height - 1 do
  begin
    pColor := bmp.ScanLine[Y];
    for X  := 0 to bmp.width - 1 do
    begin
      value          := ((pColor^.rgbRed - 128) * intContrastValue + 12800) div 100;
      pColor^.rgbRed := EnsureRange(value, 0, 255);

      value            := ((pColor^.rgbGreen - 128) * intContrastValue + 12800) div 100;
      pColor^.rgbGreen := EnsureRange(value, 0, 255);

      value           := ((pColor^.rgbBlue - 128) * intContrastValue + 12800) div 100;
      pColor^.rgbBlue := EnsureRange(value, 0, 255);

      Inc(pColor);
    end;
  end;
end;

procedure Contrast_Delphi(bmp: TBitmap; const intContrastValue: Integer);
begin

end;

procedure Contrast_ASM(bmp: TBitmap; const intContrastValue: Integer);
begin

end;

procedure Contrast(bmp: TBitmap; const intContrastValue: Integer; const ct: TContrastType = ctAVX);
begin
  case ct of
    ctScanline:
      Contrast_ScanLine(bmp, intContrastValue);                                         // 105 ms
    ctDelphi:                                                                           //
      Contrast_Delphi(bmp, intContrastValue);                                           // 100 ms
    ctASM:                                                                              //
      Contrast_ASM(bmp, intContrastValue);                                              //
    ctMMX:                                                                              //
      ;                                                                                 //
    ctSSE:                                                                              //
      ;                                                                                 //
    ctSSE2:                                                                             //
      Contrast_sse2(GetBitsPointer(bmp), bmp.width, bmp.height, intContrastValue);      // 62 ms
    ctSSE4:                                                                             //
      Contrast_sse4(GetBitsPointer(bmp), bmp.width, bmp.height, intContrastValue);      // 44 ms
    ctAVX:                                                                              //
      Contrast_avx(GetBitsPointer(bmp), bmp.width, bmp.height, intContrastValue);       // 50 ms
    ctAVX2:                                                                             //
      Contrast_avx2(GetBitsPointer(bmp), bmp.width, bmp.height, intContrastValue);      //
    ctAVX512:                                                                           //
      Contrast_avx512knl(GetBitsPointer(bmp), bmp.width, bmp.height, intContrastValue); //
  end;
end;

end.
