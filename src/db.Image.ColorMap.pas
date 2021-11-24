unit db.Image.ColorMap;
{
  Func: 32Î»Î»Í¼Í¼ÏñÉ«²Ê
  Name: dbyoung@sina.com
  Date: 2021-2-24
  Vers: Delphi 11
}

interface

uses Winapi.Windows, System.Threading, System.Math, Vcl.Graphics, db.Image.Common;

type
  TColorMapType   = (cmtScanline, cmtParallel);

procedure ColorMap(bmp: TBitmap; const intColorMapValue: Integer; const cmt: TColorMapType = cmtParallel);

implementation

procedure RGBToHSV(const R, G, B: Byte; var H, S, V: Integer);
var
  Delta, iMax, iMin: Integer;
begin
  iMax  := MaxIntValue([R, G, B]);
  iMin  := MinIntValue([R, G, B]);
  Delta := iMax - iMin;

  V := iMax;
  S := Ifthen(V = 0, 0, Round(255 * (Delta / iMax)));

  if S = 0 then
  begin
    H := 0;
  end
  else
  begin
    if R = iMax then
      H := Round(000 + 60 * (G - B) / Delta)
    else if G = iMax then
      H := Round(120 + 60 * (B - R) / Delta)
    else if B = iMax then
      H := Round(240 + 60 * (R - G) / Delta);
    if H < 0 then
      H := H + 360;
  end;
end;

procedure HSVToRGB(H, S, V: Integer; var R, G, B: Byte);
var
  f, I, p, q, t, VS: Integer;
begin
  if H > 360 then
    H := H - 360;
  if H < 0 then
    H := H + 360;

  if S = 0 then
  begin
    R := V;
    G := V;
    B := V;
  end
  else
  begin
    if H = 360 then
      I := 0
    else
      I := H;
    f   := I mod 60;
    I   := I div 60;
    VS  := V * S;
    p   := V - VS div 255;
    q   := V - (VS * f) div 15300;
    t   := V - (VS * (60 - f)) div 15300;
    case I of
      0:
        begin
          R := V;
          G := t;
          B := p
        end;
      1:
        begin
          R := q;
          G := V;
          B := p
        end;
      2:
        begin
          R := p;
          G := V;
          B := t
        end;
      3:
        begin
          R := p;
          G := q;
          B := V
        end;
      4:
        begin
          R := t;
          G := p;
          B := V
        end;
      5:
        begin
          R := V;
          G := p;
          B := q
        end
    end
  end;
end;

procedure RGBToYUV(R, G, B: Byte; var Y, U, V: Byte);
begin
  Y := EnsureRange(Round(0.257 * R + 0.504 * G + 0.098 * B) + 16, 0, 255);
  V := EnsureRange(Round(0.439 * R - 0.368 * G - 0.071 * B) + 128, 0, 255);
  U := EnsureRange(Round(-0.148 * R - 0.291 * G + 0.439 * B) + 128, 0, 255);
end;

procedure YUVToRGB(Y, U, V: Byte; var R, G, B: Byte);
var
  CY, CU, CV: LongInt;
begin
  CY := Y - 16;
  CU := U - 128;
  CV := V - 128;
  R  := EnsureRange(Round(1.164 * CY - 0.002 * CU + 1.596 * CV), 0, 255);
  G  := EnsureRange(Round(1.164 * CY - 0.391 * CU - 0.813 * CV), 0, 255);
  B  := EnsureRange(Round(1.164 * CY + 2.018 * CU - 0.001 * CV), 0, 255);
end;

procedure RGBToYCbCr(R, G, B: Byte; var Y, Cb, Cr: Byte);
begin
  Y  := EnsureRange(Round(0.29900 * R + 0.58700 * G + 0.11400 * B), 0, 255);
  Cb := EnsureRange(Round(-0.16874 * R - 0.33126 * G + 0.50000 * B + 128), 0, 255);
  Cr := EnsureRange(Round(0.50000 * R - 0.41869 * G - 0.08131 * B + 128), 0, 255);
end;

procedure YCbCrToRGB(Y, Cb, Cr: Byte; var R, G, B: Byte);
begin
  R := EnsureRange(Round(Y + 1.40200 * (Cr - 128)), 0, 255);
  G := EnsureRange(Round(Y - 0.34414 * (Cb - 128) - 0.71414 * (Cr - 128)), 0, 255);
  B := EnsureRange(Round(Y + 1.77200 * (Cb - 128)), 0, 255);
end;

procedure RGBToYCbCr16(R, G, B: Word; var Y, Cb, Cr: Word);
begin
  Y  := EnsureRange(Round(0.29900 * R + 0.58700 * G + 0.11400 * B), 0, 65535);
  Cb := EnsureRange(Round(-0.16874 * R - 0.33126 * G + 0.50000 * B + 32768), 0, 65535);
  Cr := EnsureRange(Round(0.50000 * R - 0.41869 * G - 0.08131 * B + 32768), 0, 65535);
end;

procedure YCbCrToRGB16(Y, Cb, Cr: Word; var R, G, B: Word);
begin
  R := EnsureRange(Round(Y + 1.40200 * (Cr - 32768)), 0, 65535);
  G := EnsureRange(Round(Y - 0.34414 * (Cb - 32768) - 0.71414 * (Cr - 32768)), 0, 65535);
  B := EnsureRange(Round(Y + 1.77200 * (Cb - 32768)), 0, 65535);
end;

procedure RGBToCMY(R, G, B: Byte; var C, M, Y: Byte);
begin
  C := 255 - R;
  M := 255 - G;
  Y := 255 - B;
end;

procedure CMYToRGB(C, M, Y: Byte; var R, G, B: Byte);
begin
  R := 255 - C;
  G := 255 - M;
  B := 255 - Y;
end;

procedure RGBToCMY16(R, G, B: Word; var C, M, Y: Word);
begin
  C := 65535 - R;
  M := 65535 - G;
  Y := 65535 - B;
end;

procedure CMYToRGB16(C, M, Y: Word; var R, G, B: Word);
begin
  R := 65535 - C;
  G := 65535 - M;
  B := 65535 - Y;
end;

procedure RGBToCMYK(R, G, B: Byte; var C, M, Y, K: Byte);
begin
  RGBToCMY(R, G, B, C, M, Y);
  K := Min(C, Min(M, Y));
  if K = 255 then
  begin
    C := 0;
    M := 0;
    Y := 0;
  end
  else
  begin
    C := EnsureRange(Round((C - K) / (255 - K) * 255), 0, 255);
    M := EnsureRange(Round((M - K) / (255 - K) * 255), 0, 255);
    Y := EnsureRange(Round((Y - K) / (255 - K) * 255), 0, 255);
  end;
end;

procedure CMYKToRGB(C, M, Y, K: Byte; var R, G, B: Byte);
begin
  R := (255 - (C - MulDiv(C, K, 255) + K));
  G := (255 - (M - MulDiv(M, K, 255) + K));
  B := (255 - (Y - MulDiv(Y, K, 255) + K));
end;

procedure RGBToCMYK16(R, G, B: Word; var C, M, Y, K: Word);
begin
  RGBToCMY16(R, G, B, C, M, Y);
  K := Min(C, Min(M, Y));
  if K = 65535 then
  begin
    C := 0;
    M := 0;
    Y := 0;
  end
  else
  begin
    C := EnsureRange(Round((C - K) / (65535 - K) * 65535), 0, 65535);
    M := EnsureRange(Round((M - K) / (65535 - K) * 65535), 0, 65535);
    Y := EnsureRange(Round((Y - K) / (65535 - K) * 65535), 0, 65535);
  end;
end;

procedure CMYKToRGB16(C, M, Y, K: Word; var R, G, B: Word);
begin
  R := 65535 - (C - MulDiv(C, K, 65535) + K);
  G := 65535 - (M - MulDiv(M, K, 65535) + K);
  B := 65535 - (Y - MulDiv(Y, K, 65535) + K);
end;

procedure RGBToYCoCg(R, G, B: Byte; var Y, Co, Cg: Byte);
begin
  Y  := EnsureRange((R + G shl 1 + B + 2) div 4, 0, 255);
  Co := EnsureRange((R shl 1 - B shl 1 + 2) div 4 + 128, 0, 255);
  Cg := EnsureRange((-R + G shl 1 - B + 2) div 4 + 128, 0, 255);
end;

procedure YCoCgToRGB(Y, Co, Cg: Byte; var R, G, B: Byte);
var
  CoInt, CgInt: Integer;
begin
  CoInt := Co - 128;
  CgInt := Cg - 128;
  R     := EnsureRange(Y + CoInt - CgInt, 0, 255);
  G     := EnsureRange(Y + CgInt, 0, 255);
  B     := EnsureRange(Y - CoInt - CgInt, 0, 255);
end;

procedure ColorMap_Scanline(bmp: TBitmap; const intValue: Integer);
var
  pColor : PRGBQuad;
  I, J   : Integer;
  R, G, B: Byte;
  H, S, V: Integer;
begin
  H     := intValue;
  S     := 0;
  V     := 0;
  for I := 0 to bmp.Height - 1 do
  begin
    pColor := bmp.ScanLine[I];
    for J  := 0 to bmp.Width - 1 do
    begin
      B := pColor^.rgbBlue;
      G := pColor^.rgbGreen;
      R := pColor^.rgbRed;
      RGBToHSV(R, G, B, H, S, V);
      H := EnsureRange(H + intValue, 0, 360);
      if S = 0.0 then
        H := 0;

      HSVToRGB(H, S, V, R, G, B);
      pColor^.rgbRed   := R;
      pColor^.rgbGreen := G;
      pColor^.rgbBlue  := B;
      Inc(pColor);
    end;
  end;
end;

procedure ColorMap_Parallel_Proc(pColor: PRGBQuad; const bmpWidth, intValue: Integer);
var
  I      : Integer;
  R, G, B: Byte;
  H, S, V: Integer;
begin
  for I := 0 to bmpWidth - 1 do
  begin
    B := pColor^.rgbBlue;
    G := pColor^.rgbGreen;
    R := pColor^.rgbRed;
    RGBToHSV(R, G, B, H, S, V);
    H := EnsureRange(H + intValue, 0, 360);
    if S = 0 then
      H := 0;

    HSVToRGB(H, S, V, R, G, B);
    pColor^.rgbRed   := R;
    pColor^.rgbGreen := G;
    pColor^.rgbBlue  := B;
    Inc(pColor);
  end;
end;

procedure ColorMap_Parallel(bmp: TBitmap; const intValue: Integer);
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
      ColorMap_Parallel_Proc(pColor, bmp.Width, intValue);
    end);
end;

procedure ColorMap(bmp: TBitmap; const intColorMapValue: Integer; const cmt: TColorMapType = cmtParallel);
begin
  case cmt of
    cmtScanline:
      ColorMap_Scanline(bmp, intColorMapValue);
    cmtParallel:
      ColorMap_Parallel(bmp, intColorMapValue);
  end;
end;

end.
