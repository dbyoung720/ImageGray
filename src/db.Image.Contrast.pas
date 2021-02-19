unit db.Image.Contrast;

interface

uses Winapi.Windows, Vcl.Graphics, System.Threading, System.Math, db.Image.Common;

type
  TContrastType = (ctScanline, ctDelphi, ctTable, ctTableParallel, ctParallel, ctSSE2, ctSSE4, ctAVX1, ctAVX2, ctAVX512knl, ctAVX512skx);

procedure Contrast(bmp: TBitmap; const intContrastValue: Integer; const ct: TContrastType = ctTableParallel);

implementation

procedure Contrast_ScanLine(bmp: TBitmap; const intContrastValue: Integer);
var
  X, Y  : Integer;
  pColor: PRGBQuad;
begin
  for Y := 0 to bmp.height - 1 do
  begin
    pColor := bmp.ScanLine[Y];
    for X  := 0 to bmp.width - 1 do
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
  Count  := bmp.width * bmp.height;
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
  Count  := bmp.width * bmp.height;
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

  TParallel.For(0, bmp.height - 1,
    procedure(Y: Integer)
    var
      pColor: PRGBQuad;
    begin
      pColor := PRGBQuad(StartScanLine + Y * bmpWidthBytes);
      Contrast_Table_Parallel_Proc(pColor, intContrastValue, bmp.width);
    end);
end;

procedure Contrast_Parallel(bmp: TBitmap; const intContrastValue: Integer);
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
        pColor^.rgbRed := EnsureRange(((pColor^.rgbRed - 128) * intContrastValue div 100 + 128), 0, 255);
        pColor^.rgbGreen := EnsureRange(((pColor^.rgbGreen - 128) * intContrastValue div 100 + 128), 0, 255);
        pColor^.rgbBlue := EnsureRange(((pColor^.rgbBlue - 128) * intContrastValue div 100 + 128), 0, 255);
        Inc(pColor);
      end;
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
      Contrast_ScanLine(bmp, intContrastValue);                                        // 105 ms
    ctDelphi:                                                                          //
      Contrast_Delphi(bmp, intContrastValue);                                          // 100 ms
    ctTable:                                                                           //
      Contrast_Table(bmp, intContrastValue);                                           //
    ctTableParallel:                                                                   //
      Contrast_TableParallel(bmp, intContrastValue);                                   //
    ctParallel:                                                                        //
      Contrast_Parallel(bmp, intContrastValue);                                        //
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
