unit db.Image.GeometricTrans;
{
  Func: 32位位图几何变换
  Name: dbyoung@sina.com
  Date: 2021-2-22
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；
  Note: 并行程序，不能在 IDE 下运行查看效果。必须脱离 IDE 执行查看效果。
}

interface

uses Winapi.Windows, System.Threading, System.Classes, System.Math, Vcl.Graphics, db.Image.Common;

{ 水平翻转 并行模式，需要脱离 IDE 执行 }
procedure HorizMirror(bmp: TBitmap);

{ 垂直翻转 }
procedure VertiMirror(bmp: TBitmap);

{ 转置翻转 并行模式，需要脱离 IDE 执行 }
procedure HAndVMirror(bmp: TBitmap);

implementation

{ 水平翻转 并行模式，需要脱离 IDE 执行 }
procedure HorizMirror(bmp: TBitmap);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      X: Integer;
      K: DWORD;
      pColor01: PDWORD;
      pColor02: PDWORD;
    begin
      pColor01 := PDWORD(StartScanLine + Y * bmpWidthBytes);
      pColor02 := PDWORD(StartScanLine + Y * bmpWidthBytes);
      for X := 0 to bmp.Width div 2 - 1 do
      begin
        Inc(pColor02, bmp.Width - X - 1);
        K := pColor02^;
        pColor02^ := pColor01^;
        pColor01^ := K;
        Inc(pColor01);
        Dec(pColor02, bmp.Width - X - 1);
      end;
    end);
end;

{ 垂直翻转 }
procedure VertiMirror(bmp: TBitmap);
var
  Count, Y: Integer;
  pColor01: PByte;
  pColor02: PByte;
  tmpColor: PByte;
begin
  Count    := Integer(bmp.ScanLine[0]) - Integer(bmp.ScanLine[1]);
  tmpColor := AllocMem(Count);
  try
    for Y := 0 to bmp.Height div 2 - 1 do
    begin
      pColor01 := bmp.ScanLine[Y];
      pColor02 := bmp.ScanLine[bmp.Height - Y - 1];
      Move(pColor01^, tmpColor^, Count);
      Move(pColor02^, pColor01^, Count);
      Move(tmpColor^, pColor02^, Count);
    end;
  finally
    FreeMem(tmpColor);
  end;
end;

{ 转置翻转 并行模式，需要脱离 IDE 执行 }
procedure HAndVMirror(bmp: TBitmap);
begin
  HorizMirror(bmp);
  VertiMirror(bmp);
end;

end.
