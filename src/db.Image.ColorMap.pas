unit db.Image.ColorMap;
{
  Func: 32λλͼͼ��ɫ��
  Name: dbyoung@sina.com
  Date: 2021-2-24
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note��Delphi �� Release ģʽ�����Ż��ģ�Debug ��û�еģ������ʱ�䣬������ DEBUG ģʽ�µ���ʱ��
  Note: ���г��򣬲����� IDE �����в鿴Ч������������ IDE ִ�в鿴Ч����
}

interface

uses Winapi.Windows, System.Threading, System.Math, Vcl.Graphics, db.Image.Common;

type
  TColorMapType = (cmtScanline, cmtParallel, cmtSSEParallel, cmtSSE2, cmtSSE4, cmtAVX1, cmtAVX2, cmtAVX512knl, cmtAVX512skx);

procedure ColorMap(bmp: TBitmap; const intColorMapValue: Integer; const cmt: TColorMapType = cmtSSEParallel);

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

procedure HSVtoRGB(H, S, V: Integer; var R, G, B: Byte);
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

      HSVtoRGB(H, S, V, R, G, B);
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
    // S := EnsureRange(S + intValue, 0, 255);  // ���ڱ��Ͷ� (��Χ�� -225 --- 255 ֮��)
    if S = 0 then
      H := 0;

    HSVtoRGB(H, S, V, R, G, B);
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

procedure ColorMap_SSEParallel_Proc(pColor: PRGBQuad; const intValue, bmpWidth: Integer);
asm
  {$IFDEF WIN64}
  XCHG    RAX,  RCX
  {$IFEND}
  MOVSS   XMM1, [c_PixBGRAMask]             // XMM1 = |00000000|00000000|00000000|000000FF|
  MOVD    XMM2, EDX                         // XMM2 = |000000000000000000000000000intValue|
  SHUFPS  XMM1, XMM1, 0                     // XMM1 = |000000FF|000000FF|000000FF|000000FF|
  SHUFPS  XMM2, XMM2, 0                     // XMM2 = |intValue|intValue|intValue|intValue|
  MOVAPS  XMM3, XMM1                        // XMM3 = |000000FF|000000FF|000000FF|000000FF|
  PSUBB   XMM3, XMM2                        // XMM3 = |000000FF - intValue|000000FF - intValue|000000FF - intValue|000000FF - intValue|

@LOOP:
  MOVUPS  XMM4, [EAX]                       // XMM4 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM5, XMM4                        // XMM5 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM6, XMM4                        // XMM6 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|
  MOVAPS  XMM7, XMM4                        // XMM7 = |A3R3G3B3|A2R2G2B2|A1R1G1B1|A0R0G0B0|

  // ��ȡ 4 �����ص� B3, B2, B1, B0
  ANDPS   XMM5, XMM1                        // XMM5 = |000000B3|000000B2|000000B1|000000B0|

  // ��ȡ 4 �����ص� G3, G2, G1, G0
  PSRLD   XMM6, 8                           // XMM6 = |00A3R3G3|00A2R2G2|00A1R1G1|00A0R0G0|
  ANDPS   XMM6, XMM1                        // XMM6 = |000000G3|000000G2|000000G1|000000G0|

  // ��ȡ 4 �����ص� R3, R2, R1, R0
  PSRLD   XMM7, 16                          // XMM7 = |0000A3R3|0000A2R2|0000A1R1|0000A0R0|
  ANDPS   XMM7, XMM1                        // XMM7 = |000000R3|000000R2|000000R1|000000R0|

  // ����ɫ��ģʽ

  // ���ؽ��
@RValue:
  PSLLD   XMM6, 8                           // XMM6  = |0000Y300|0000Y200|0000Y100|0000Y000|
  PSLLD   XMM7, 16                          // XMM7  = |00Y30000|00Y20000|00Y10000|00Y00000|
  ORPS    XMM5, XMM6                        // XMM5  = |0000Y3Y3|0000Y2Y2|0000Y1Y1|0000Y0Y0|
  ORPS    XMM5, XMM7                        // XMM5  = |00Y3Y3Y3|00Y2Y2Y2|00Y1Y1Y1|00Y0Y0Y0|
  MOVUPS  [EAX], XMM5                       // [EAX] = XMM5

  ADD     EAX, 16                           // pColor ��ַ�� 16��EAX ָ����4�����صĵ�ַ
  SUB     ECX, 4                            // Width �� 4, ÿ 4 ������һѭ��
  JNZ     @LOOP                             // ѭ��
end;

procedure ColorMap_SSEParallel(bmp: TBitmap; const intValue: Integer);
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
      ColorMap_SSEParallel_Proc(pColor, intValue, bmp.Width);
    end);
end;

procedure ColorMap(bmp: TBitmap; const intColorMapValue: Integer; const cmt: TColorMapType = cmtSSEParallel);
begin
  case cmt of
    cmtScanline:
      ColorMap_Scanline(bmp, intColorMapValue);
    cmtParallel:
      ColorMap_Parallel(bmp, intColorMapValue);
    cmtSSEParallel:
      ColorMap_SSEParallel(bmp, intColorMapValue);
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