unit db.Image.Blend;
{
  Func: 32λλͼͼ��͸����
  Name: dbyoung@sina.com
  Date: 2021-11-24
  Vers: Delphi 11
}

interface

uses Winapi.Windows, System.Threading, System.Math, Vcl.Graphics, db.Image.Common;

type
  TColorBlendType = (cbtScanline, cbtParallel);

procedure ColorBlend(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer; const ctt: TColorBlendType = cbtParallel);

implementation

{ 100 ms }
procedure ColorBlend_Scanline(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer);
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
  K     := (intBlendValue + 1) * 256;

  for Y := iTop to iTop + bmpSrc.Height - 1 do
  begin
    pColorDst := bmpDst.ScanLine[Y];
    pColorSrc := bmpSrc.ScanLine[Y - iTop];
    Inc(pColorDst, iLeft);
    for X := 0 to bmpSrc.Width - 1 do
    begin
      pColorDst^.rgbRed   := (pColorDst^.rgbRed   * (65536 - K) + pColorSrc^.rgbRed   * K) shr 16;
      pColorDst^.rgbGreen := (pColorDst^.rgbGreen * (65536 - K) + pColorSrc^.rgbGreen * K) shr 16;
      pColorDst^.rgbBlue  := (pColorDst^.rgbBlue  * (65536 - K) + pColorSrc^.rgbBlue  * K) shr 16;
      Inc(pColorDst);
      Inc(pColorSrc);
    end;
  end;
end;

{ 5 ms  ��Ҫ���� IDE ִ�� / ScanLine �������� TParallel.For �� }
procedure ColorBlend_Parallel(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer);
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
  K     := (intBlendValue + 1) * 256;

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
        pColorDst^.rgbRed   := (pColorDst^.rgbRed   * (65536 - K) + pColorSrc^.rgbRed   * K) shr 16;
        pColorDst^.rgbGreen := (pColorDst^.rgbGreen * (65536 - K) + pColorSrc^.rgbGreen * K) shr 16;
        pColorDst^.rgbBlue  := (pColorDst^.rgbBlue  * (65536 - K) + pColorSrc^.rgbBlue  * K) shr 16;
        Inc(pColorDst);
        Inc(pColorSrc);
      end;
    end);
end;

procedure ColorBlend(bmpDst, bmpSrc: TBitmap; const intBlendValue: Integer; const ctt: TColorBlendType = cbtParallel);
begin
  case ctt of
    cbtScanline:
      ColorBlend_Scanline(bmpDst, bmpSrc, intBlendValue);
    cbtParallel:
      ColorBlend_Parallel(bmpDst, bmpSrc, intBlendValue);
  end;
end;

end.