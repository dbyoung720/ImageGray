unit db.Image.Blend;
{
  Func: 32位位图图像透明度
  Name: dbyoung@sina.com
  Date: 2021-11-24
  Vers: Delphi 11
}

interface

uses Winapi.Windows, System.Threading, System.Math, Vcl.Graphics, db.Image.Common;

type
  TColorBlendType = (cbtScanline, cbtTable, cbtParallel);

procedure ColorBlend(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer; const cbt: TColorBlendType = cbtParallel);

implementation

{ 100 ms }
procedure ColorBlend_Scanline(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer);
var
  X, Y       : Integer;
  iLeft, iTop: Integer;
  pColorDst  : PRGBQuad;
  pColorSrc  : PRGBQuad;
begin
  if bmpSrc.Width > bmpDst.Width then
    Exit;

  if bmpSrc.Height > bmpDst.Height then
    Exit;

  iTop  := (bmpDst.Height - bmpSrc.Height) div 2;
  iLeft := (bmpDst.Width  - bmpSrc.Width)  div 2;

  for Y := iTop to iTop + bmpSrc.Height - 1 do
  begin
    pColorDst := bmpDst.ScanLine[Y];
    pColorSrc := bmpSrc.ScanLine[Y - iTop];
    Inc(pColorDst, iLeft);
    for X := 0 to bmpSrc.Width - 1 do
    begin
      pColorDst^.rgbRed   := (pColorDst^.rgbRed   * (256 - intBlendValue) + pColorSrc^.rgbRed   * intBlendValue) shr 8;
      pColorDst^.rgbGreen := (pColorDst^.rgbGreen * (256 - intBlendValue) + pColorSrc^.rgbGreen * intBlendValue) shr 8;
      pColorDst^.rgbBlue  := (pColorDst^.rgbBlue  * (256 - intBlendValue) + pColorSrc^.rgbBlue  * intBlendValue) shr 8;
      Inc(pColorDst);
      Inc(pColorSrc);
    end;
  end;
end;

{ 30 ms }
procedure ColorBlend_Table(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer);
var
  X, Y       : Integer;
  iLeft, iTop: Integer;
  pColorDst  : PRGBQuad;
  pColorSrc  : PRGBQuad;
begin
  if bmpSrc.Width > bmpDst.Width then
    Exit;

  if bmpSrc.Height > bmpDst.Height then
    Exit;

  iTop  := (bmpDst.Height - bmpSrc.Height) div 2;
  iLeft := (bmpDst.Width  - bmpSrc.Width)  div 2;

  for Y := iTop to iTop + bmpSrc.Height - 1 do
  begin
    pColorDst := bmpDst.ScanLine[Y];
    pColorSrc := bmpSrc.ScanLine[Y - iTop];
    Inc(pColorDst, iLeft);
    for X := 0 to bmpSrc.Width - 1 do
    begin
      pColorDst^.rgbRed   := (g_BlendTable[pColorDst^.rgbRed,   (256 - intBlendValue)] + g_BlendTable[pColorSrc^.rgbRed,   intBlendValue]) shr 8;
      pColorDst^.rgbGreen := (g_BlendTable[pColorDst^.rgbGreen, (256 - intBlendValue)] + g_BlendTable[pColorSrc^.rgbGreen, intBlendValue]) shr 8;
      pColorDst^.rgbBlue  := (g_BlendTable[pColorDst^.rgbBlue,  (256 - intBlendValue)] + g_BlendTable[pColorSrc^.rgbBlue,  intBlendValue]) shr 8;
      Inc(pColorDst);
      Inc(pColorSrc);
    end;
  end;
end;

{ 5 ms  需要脱离 IDE 执行 / ScanLine 不要用于 TParallel.For 中 }
procedure ColorBlend_Parallel(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer);
var
  iLeft, iTop     : Integer;
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
  iLeft := (bmpDst.Width  - bmpSrc.Width)  div 2;

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
        pColorDst^.rgbRed   := (pColorDst^.rgbRed   * (256 - intBlendValue) + pColorSrc^.rgbRed   * intBlendValue) shr 8;
        pColorDst^.rgbGreen := (pColorDst^.rgbGreen * (256 - intBlendValue) + pColorSrc^.rgbGreen * intBlendValue) shr 8;
        pColorDst^.rgbBlue  := (pColorDst^.rgbBlue  * (256 - intBlendValue) + pColorSrc^.rgbBlue  * intBlendValue) shr 8;
        Inc(pColorDst);
        Inc(pColorSrc);
      end;
    end);
end;

procedure ColorBlend(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer; const cbt: TColorBlendType = cbtParallel);
var
  K: Integer;
begin
  K := Round(intBlendValue * 2.56);
  case cbt of
    cbtScanline:
      ColorBlend_Scanline(bmpDst, bmpSrc, K);
    cbtTable:
      ColorBlend_Table(bmpDst, bmpSrc, K);
    cbtParallel:
      ColorBlend_Parallel(bmpDst, bmpSrc, K);
  end;
end;

end.
