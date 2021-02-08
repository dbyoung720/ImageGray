unit db.Image.Saturation;

interface

uses Winapi.Windows, System.Math, Vcl.Graphics, System.Win.Crtl, db.Image.Common;

type
  TSaturationType = (stScanline, stDelphi, stASM, stMMX, stSSE, stSSE2, stSSE4, stAVX1, stAVX2, stAVX512knl, stAVX512skx);

procedure Saturation(bmp: TBitmap; const intSaturationValue: Integer; const st: TSaturationType = stAVX1);

implementation

type
  TAlpha = array [0 .. 255] of Word;
  TGrays = array [0 .. 767] of Integer;

procedure GetGrayAlpha(const intSaturationValue: Integer; var alpha: TAlpha; var grays: TGrays);
var
  X   : Integer;
  I   : Integer;
  Gray: Integer;
begin
  X := 0;

  for I := 0 to 255 do
  begin
    alpha[I] := (I * intSaturationValue) shr 8;
  end;

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

procedure Saturation(bmp: TBitmap; const intSaturationValue: Integer; const st: TSaturationType = stAVX1);
var
  alpha: TAlpha;
  grays: TGrays;
begin
  GetGrayAlpha(intSaturationValue, alpha, grays);

  case st of
    stScanline:
      Saturation_Scanline(bmp, alpha, grays);
    stDelphi:
      Saturation_Delphi(bmp, alpha, grays);
    stASM:
      ;
    stMMX:
      ;
    stSSE:
      ;
    stSSE2:
      bgraSaturation_sse2(GetBitsPointer(bmp), bmp.width, bmp.height, intSaturationValue);
    stSSE4:
      bgraSaturation_sse4(GetBitsPointer(bmp), bmp.width, bmp.height, intSaturationValue);
    stAVX1:
      bgraSaturation_avx1(GetBitsPointer(bmp), bmp.width, bmp.height, intSaturationValue);
    stAVX2:
      bgraSaturation_avx2(GetBitsPointer(bmp), bmp.width, bmp.height, intSaturationValue);
    stAVX512knl:
      bgraSaturation_avx512knl(GetBitsPointer(bmp), bmp.width, bmp.height, intSaturationValue);
    stAVX512skx:
      bgraSaturation_avx512skx(GetBitsPointer(bmp), bmp.width, bmp.height, intSaturationValue);
  end;
end;

end.
