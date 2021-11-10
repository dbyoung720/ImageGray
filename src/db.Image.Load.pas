unit db.Image.Load;
{
  Func: 图像加载，统一解码为32位位图格式
  Name: dbyoung@sina.com
  Date: 2020-10-01
  Ver : Delphi 11
  Note: GDI+ JPEG 解码效果最好
}

interface

uses Winapi.Windows, Winapi.GDIPOBJ, System.Classes, System.SysUtils, {$IFDEF WIN32}db.Image.jpegdec, {$ENDIF} Vcl.ExtCtrls, Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.GIFImg, Vcl.Imaging.jpeg;

type
  TJpegDecType = (jdtGDI, jdtGDIPLUS, jdtSSE);

procedure LoadImage(const strFileName: string; var bmp: TBitmap; const JpegDecType: TJpegDecType = jdtGDIPLUS); overload;
procedure LoadImage(const strFileName: string; imgShow_: TImage; const JpegDecType: TJpegDecType = jdtGDIPLUS); overload;

implementation

function LoadJpeg_GDI(const strFileName: String; var bmp: TBitmap): Boolean;
begin
  Result := False;
end;

function LoadJpeg_GDIPLUS(const strFileName: String; var bmp: TBitmap): Boolean;
var
  jpg: TGPImage;
  gpg: TGPGraphics;
begin
  Result := True;

  jpg := TGPImage.Create(strFileName);
  try
    try
      bmp.PixelFormat := pf32bit;
      bmp.SetSize(jpg.GetWidth, jpg.GetHeight);
      gpg:= TGPGraphics.Create(bmp.Canvas.Handle);
      try
        gpg.DrawImage(jpg, 0, 0, bmp.Width, bmp.Height);
      finally
        gpg.Free;
      end;
    except
      Result := False;
    end;
  finally
    jpg.Free;
  end;
end;

function LoadJpeg_SSE(const strFileName: String; var bmp: TBitmap): Boolean;
begin
  Result := False;
{$IFDEF  WIN32}
  with TMemoryStream.Create do
  begin
    try
      LoadFromFile(strFileName);
      bmp := JpegDecode(Memory, Size);
      if bmp <> nil then
      begin
        Result          := True;
        bmp.PixelFormat := pf32bit;
      end;
    finally
      Free;
    end;
  end;
{$IFEND}
end;

function CheckJpeg(const strFileName: String; var bmp: TBitmap; const JpegDecType: TJpegDecType = jdtGDIPLUS): Boolean;
var
  hFile : THandle;
  Buffer: array [0 .. 15] of AnsiChar;
begin
  hFile := FileOpen(strFileName, fmOpenRead);
  try
    FileRead(hFile, Buffer, 16);
    Result := (Buffer[6] = 'J') and (Buffer[7] = 'F') and (Buffer[8] = 'I') and (Buffer[9] = 'F');
  finally
    FileClose(hFile);
  end;

  if not Result then
    Exit;

  case JpegDecType of
    jdtGDI:
      Result := LoadJpeg_GDI(strFileName, bmp);
    jdtGDIPLUS:
      Result := LoadJpeg_GDIPLUS(strFileName, bmp);
    jdtSSE:
      Result := LoadJpeg_SSE(strFileName, bmp);
  end;
end;

procedure LoadImage(const strFileName: string; var bmp: TBitmap; const JpegDecType: TJpegDecType = jdtGDIPLUS);
begin
  if CheckJpeg(strFileName, bmp, JpegDecType) then
    Exit;

  with TPicture.Create do
  begin
    LoadFromFile(strFileName);
    bmp.PixelFormat := pf32bit;
    bmp.Width       := Width;
    bmp.Height      := Height;
    bmp.Canvas.Draw(0, 0, Graphic);
    Free;
  end;
end;

procedure LoadImage(const strFileName: string; imgShow_: TImage; const JpegDecType: TJpegDecType = jdtGDIPLUS);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    LoadImage(strFileName, bmp, JpegDecType);
    imgShow_.Picture.Bitmap.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

end.
