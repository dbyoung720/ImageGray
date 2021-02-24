unit db.Image.ColorMode;

interface

uses Winapi.Windows, Vcl.Graphics, System.Threading, System.Math, db.Image.Common;

type
  TColorModeType = (cmtScanline, cmtParallel, cmtSSEParallel, cmtSSE2, cmtSSE4, cmtAVX1, cmtAVX2, cmtAVX512knl, cmtAVX512skx);

procedure ColorMode(bmp: TBitmap; const intColorModeValue: Integer; const cmt: TColorModeType = cmtSSEParallel);

implementation

procedure RGBToHSV(const R, G, B: Single; var H, S, V: Single);
var
  Delta: Single;
  iMin : Single;
begin
  iMin  := MinValue([R, G, B]);
  V     := MaxValue([R, G, B]);
  Delta := V - iMin;

  if V = 0.0 then
    S := 0
  else
    S := Delta / V;

  if S = 0.0 then
  begin
    H := NaN;
  end
  else
  begin
    if R = V then
      H := 60.0 * (G - B) / Delta
    else if G = V then
      H := 120.0 + 60.0 * (B - R) / Delta
    else if B = V then
      H := 240.0 + 60.0 * (R - G) / Delta;

    if H < 0.0 then
      H := H + 360.0
  end
end;

procedure HSVtoRGB(const H, S, V: Single; var R, G, B: Single);
var
  f      : Single;
  I      : Integer;
  hTemp  : Single;
  p, q, t: Single;
begin
  if S = 0.0 then
  begin
    if H = 0 then
    begin
      R := V;
      G := V;
      B := V
    end;
  end
  else
  begin
    if H = 360.0 then
      hTemp := 0.0
    else
      hTemp := H;

    hTemp := hTemp / 60;
    I     := TRUNC(hTemp);
    f     := hTemp - I;

    p := V * (1.0 - S);
    q := V * (1.0 - (S * f));
    t := V * (1.0 - (S * (1.0 - f)));

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
  end
end;

procedure ColorMode_Scanline(bmp: TBitmap; intValue: Integer);
var
  pColor    : PRGBQuad;
  I, J      : Integer;
  R, G, B   : byte;
  FR, FG, FB: Single;
  FH, FS, FV: Single;
begin
  FH    := intValue;
  FS    := 0;
  FV    := 0;
  for I := 0 to bmp.Height - 1 do
  begin
    pColor := bmp.ScanLine[I];
    for J  := 0 to bmp.Width - 1 do
    begin
      B  := pColor^.rgbBlue;
      G  := pColor^.rgbGreen;
      R  := pColor^.rgbRed;
      FR := R;
      FG := G;
      FB := B;
      RGBToHSV(FR, FG, FB, FH, FS, FV);
      FH := intValue;
      if FS = 0.0 then
        FH := 0;

      HSVtoRGB(FH, FS, FV, FR, FG, FB);
      pColor^.rgbRed   := Round(FR);
      pColor^.rgbGreen := Round(FG);
      pColor^.rgbBlue  := Round(FB);
      Inc(pColor);
    end;
  end;
end;

procedure ColorMode_Parallel_Proc(pColor: PRGBQuad; const bmpWidth, intValue: Integer);
var
  I         : Integer;
  R, G, B   : byte;
  FR, FG, FB: Single;
  FH, FS, FV: Single;
begin
  for I := 0 to bmpWidth - 1 do
  begin
    B  := pColor^.rgbBlue;
    G  := pColor^.rgbGreen;
    R  := pColor^.rgbRed;
    FR := R;
    FG := G;
    FB := B;
    RGBToHSV(FR, FG, FB, FH, FS, FV);
    FH := intValue;
    if FS = 0.0 then
      FH := 0;

    HSVtoRGB(FH, FS, FV, FR, FG, FB);
    pColor^.rgbRed   := Round(FR);
    pColor^.rgbGreen := Round(FG);
    pColor^.rgbBlue  := Round(FB);
    Inc(pColor);
  end;
end;

procedure ColorMode_Parallel(bmp: TBitmap; intValue: Integer);
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
      ColorMode_Parallel_Proc(pColor, bmp.Width, intValue);
    end);
end;

procedure ColorMode(bmp: TBitmap; const intColorModeValue: Integer; const cmt: TColorModeType = cmtSSEParallel);
begin
  case cmt of
    cmtScanline:
      ColorMode_Scanline(bmp, intColorModeValue);
    cmtParallel:
      ColorMode_Parallel(bmp, intColorModeValue);
    cmtSSEParallel:
      ;
    cmtSSE2:
      ;
    cmtSSE4:
      ;
    cmtAVX1:
      ;
    cmtAVX2:
      ;
    cmtAVX512knl:
      ;
    cmtAVX512skx:
      ;
  end;
end;

end.
