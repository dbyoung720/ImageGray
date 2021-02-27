unit db.Image.ColorMap;
{
  Func: 32位位图图像色彩
  Name: dbyoung@sina.com
  Date: 2021-2-24
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；
  Note: 并行程序，不能在 IDE 下运行查看效果。必须脱离 IDE 执行查看效果。
}

interface

uses Winapi.Windows, System.Threading, System.Math, Vcl.Graphics, db.Image.Common;

type
  TColorMapType   = (cmtScanline, cmtParallel, cmtSSEParallel, cmtSSE2, cmtSSE4, cmtAVX1, cmtAVX2, cmtAVX512knl, cmtAVX512skx);
  TColorTransType = (cttScanline, cttParallel, cttSSEParallel, cttSSE2, cttSSE4, cttAVX1, cttAVX2, cttAVX512knl, cttAVX512skx);

procedure ColorMap(bmp: TBitmap; const intColorMapValue: Integer; const cmt: TColorMapType = cmtSSEParallel);
procedure ColorTrans(bmpDst, bmpSrc: TBitmap; const intTransValue: Integer; const ctt: TColorTransType = cttSSEParallel);

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

  // 获取 4 个像素的 B3, B2, B1, B0
  ANDPS   XMM5, XMM1                        // XMM5 = |000000B3|000000B2|000000B1|000000B0|

  // 获取 4 个像素的 G3, G2, G1, G0
  PSRLD   XMM6, 8                           // XMM6 = |00A3R3G3|00A2R2G2|00A1R1G1|00A0R0G0|
  ANDPS   XMM6, XMM1                        // XMM6 = |000000G3|000000G2|000000G1|000000G0|

  // 获取 4 个像素的 R3, R2, R1, R0
  PSRLD   XMM7, 16                          // XMM7 = |0000A3R3|0000A2R2|0000A1R1|0000A0R0|
  ANDPS   XMM7, XMM1                        // XMM7 = |000000R3|000000R2|000000R1|000000R0|

  // 计算色彩模式

  // 返回结果
@RValue:
  PSLLD   XMM6, 8                           // XMM6  = |0000Y300|0000Y200|0000Y100|0000Y000|
  PSLLD   XMM7, 16                          // XMM7  = |00Y30000|00Y20000|00Y10000|00Y00000|
  ORPS    XMM5, XMM6                        // XMM5  = |0000Y3Y3|0000Y2Y2|0000Y1Y1|0000Y0Y0|
  ORPS    XMM5, XMM7                        // XMM5  = |00Y3Y3Y3|00Y2Y2Y2|00Y1Y1Y1|00Y0Y0Y0|
  MOVUPS  [EAX], XMM5                       // [EAX] = XMM5

  ADD     EAX, 16                           // pColor 地址加 16，EAX 指向下4个像素的地址
  SUB     ECX, 4                            // Width 减 4, 每 4 个像素一循环
  JNZ     @LOOP                             // 循环
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

procedure ColorTrans_Scanline(bmpDst, bmpSrc: TBitmap; const intTransValue: Integer);
var
  X, Y       : Integer;
  iLeft, iTop: Integer;
  pColorDst  : PRGBQuad;
  pColorSrc  : PRGBQuad;
  K          : Integer;
begin
  if bmpSrc.Width > bmpDst.Width then
    Exit;

  if bmpSrc.Height > bmpDst.Height then
    Exit;

  iTop  := (bmpDst.Height - bmpSrc.Height) div 2;
  iLeft := (bmpDst.Width - bmpSrc.Width) div 2;
  K     := (intTransValue + 1) * 256;

  for Y := iTop to iTop + bmpSrc.Height - 1 do
  begin
    pColorDst := bmpDst.ScanLine[Y];
    pColorSrc := bmpSrc.ScanLine[Y - iTop];
    Inc(pColorDst, iLeft);
    for X := 0 to bmpSrc.Width - 1 do
    begin
      pColorDst^.rgbRed   := (pColorDst^.rgbRed * (65536 - K) + pColorSrc^.rgbRed * K) shr 16;
      pColorDst^.rgbGreen := (pColorDst^.rgbGreen * (65536 - K) + pColorSrc^.rgbGreen * K) shr 16;
      pColorDst^.rgbBlue  := (pColorDst^.rgbBlue * (65536 - K) + pColorSrc^.rgbBlue * K) shr 16;
      Inc(pColorDst);
      Inc(pColorSrc);
    end;
  end;
end;

{ 5 ms  需要脱离 IDE 执行 / ScanLine 不能用于 TParallel.For 中 }
procedure ColorTrans_Parallel(bmpDst, bmpSrc: TBitmap; const intTransValue: Integer);
var
  iLeft, iTop     : Integer;
  K               : Integer;
  bmpWidthBytesSrc: Integer;
  bmpWidthBytesDst: Integer;
  StartScanLineDst: Integer;
  StartScanLineSrc: Integer;
begin
  if bmpSrc.Width > bmpDst.Width then
    Exit;

  if bmpSrc.Height > bmpDst.Height then
    Exit;

  iTop  := (bmpDst.Height - bmpSrc.Height) div 2;
  iLeft := (bmpDst.Width - bmpSrc.Width) div 2;
  K     := (intTransValue + 1) * 256;

  StartScanLineSrc := Integer(bmpSrc.ScanLine[0]);
  StartScanLineDst := Integer(bmpDst.ScanLine[iTop]);
  bmpWidthBytesSrc := Integer(bmpSrc.ScanLine[1]) - Integer(bmpSrc.ScanLine[0]);
  bmpWidthBytesDst := Integer(bmpDst.ScanLine[1]) - Integer(bmpDst.ScanLine[0]);

  TParallel.For(0, bmpSrc.Height - 1,
    procedure(Y: Integer)
    var
      X: Integer;
      pColorDst: PRGBQuad;
      pColorSrc: PRGBQuad;
    begin
      pColorDst := PRGBQuad(StartScanLineDst + Y * bmpWidthBytesDst);
      pColorSrc := PRGBQuad(StartScanLineSrc + Y * bmpWidthBytesSrc);
      Inc(pColorDst, iLeft);
      for X := 0 to bmpSrc.Width - 1 do
      begin
        pColorDst^.rgbRed := (pColorDst^.rgbRed * (65536 - K) + pColorSrc^.rgbRed * K) shr 16;
        pColorDst^.rgbGreen := (pColorDst^.rgbGreen * (65536 - K) + pColorSrc^.rgbGreen * K) shr 16;
        pColorDst^.rgbBlue := (pColorDst^.rgbBlue * (65536 - K) + pColorSrc^.rgbBlue * K) shr 16;
        Inc(pColorDst);
        Inc(pColorSrc);
      end;
    end);
end;

procedure ColorTrans(bmpDst, bmpSrc: TBitmap; const intTransValue: Integer; const ctt: TColorTransType = cttSSEParallel);
begin
  case ctt of
    cttScanline:
      ColorTrans_Scanline(bmpDst, bmpSrc, intTransValue);
    cttParallel:
      ColorTrans_Parallel(bmpDst, bmpSrc, intTransValue);
    cttSSEParallel:
      ;
    cttSSE2:
      ;
    cttSSE4:
      ;
    cttAVX1:
      ;
    cttAVX2:
      ;
    cttAVX512knl:
      ;
    cttAVX512skx:
      ;
  end;
end;

end.
