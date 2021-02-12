unit db.Image.Contrast;

interface

uses Winapi.Windows, Vcl.Graphics, System.Math, db.Image.Common;

type
  TContrastType = (ctScanline, ctDelphi, ctASM, ctMMX, ctSSE, ctSSE2, ctSSE4, ctAVX1, ctAVX2, ctAVX512knl, ctAVX512skx);

procedure Contrast(bmp: TBitmap; const intContrastValue: Integer; const ct: TContrastType = ctAVX1);

implementation

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

procedure Contrast(bmp: TBitmap; const intContrastValue: Integer; const ct: TContrastType = ctAVX1);
var
  pColor: PByte;
  pContr: PDWORD;
begin
  pColor := GetBitsPointer(bmp);
  pContr := GetBitsPointer(bmp);

  case ct of
    ctScanline:
      Contrast_ScanLine(bmp, intContrastValue);                                        // 105 ms
    ctDelphi:                                                                          //
      Contrast_Delphi(bmp, intContrastValue);                                          // 100 ms
    ctASM:                                                                             //
      Contrast_ASM(bmp, intContrastValue);                                             //
    ctMMX:                                                                             //
      ;                                                                                //
    ctSSE:                                                                             //
      ;                                                                                //
    ctSSE2:                                                                            //
      bgraContrast_sse2(pColor, pContr, bmp.width, bmp.height, intContrastValue);      // 62 ms
    ctSSE4:                                                                            //
      bgraContrast_sse4(pColor, pContr, bmp.width, bmp.height, intContrastValue);      // 44 ms
    ctAVX1:                                                                            //
      bgraContrast_avx1(pColor, pContr, bmp.width, bmp.height, intContrastValue);      // 50 ms
    ctAVX2:                                                                            //
      bgraContrast_avx2(pColor, pContr, bmp.width, bmp.height, intContrastValue);      //
    ctAVX512knl:                                                                       //
      bgraContrast_avx512knl(pColor, pContr, bmp.width, bmp.height, intContrastValue); //
    ctAVX512skx:                                                                       //
      bgraContrast_avx512knl(pColor, pContr, bmp.width, bmp.height, intContrastValue); //
  end;
end;

end.
