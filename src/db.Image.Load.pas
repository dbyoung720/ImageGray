unit db.Image.Load;
{
  Func: 图像加载，统一解码为 32位位图格式
  Name: dbyoung@sina.com
  Date: 2020-10-01
  Ver : Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note: GDI+ JPEG 解码效果最好
}

interface

uses Winapi.Windows, Winapi.GDIPOBJ, System.Classes, System.SysUtils, {$IFDEF WIN32}db.Image.jpegdec, {$ENDIF} Vcl.ExtCtrls, Vcl.Graphics, Vcl.Imaging.pngimage, Vcl.Imaging.GIFImg, Vcl.Imaging.jpeg;

type
  TJpegDecType = (jdtGDI, jdtGDIPLUS, jdtSSE);

procedure LoadImage(const strFileName: string; img: TImage; const JpegDecType: TJpegDecType = jdtGDIPLUS);

implementation

function LoadJpeg_GDI(const strFileName: String; img: TImage): Boolean;
begin
  Result := False;
end;

function LoadJpeg_GDIPLUS(const strFileName: String; img: TImage): Boolean;
var
  jpg: TGPImage;
  gp : TGPGraphics;
  bmp: TBitmap;
begin
  Result := True;

  jpg := TGPImage.Create(strFileName);
  try
    try
      bmp := TBitmap.Create;
      try
        bmp.PixelFormat := pf32bit;
        bmp.SetSize(jpg.GetWidth, jpg.GetHeight);
        gp := TGPGraphics.Create(bmp.Canvas.Handle);
        try
          gp.DrawImage(jpg, 0, 0, bmp.Width, bmp.Height);
          img.Picture.Bitmap.Assign(bmp);
        finally
          gp.Free;
        end;
      finally
        bmp.Free;
      end;
    except
      Result := False;
    end;
  finally
    jpg.Free;
  end;
end;

function LoadJpeg_SSE(const strFileName: String; img: TImage): Boolean;
{$IFDEF  WIN32}
var
  bmp: TBitmap;
{$IFEND}
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
        try
          Result          := True;
          bmp.PixelFormat := pf32bit;
          img.Picture.Bitmap.Assign(bmp);
        finally
          bmp.Free;
        end;
      end;
    finally
      Free;
    end;
  end;
{$IFEND}
end;

function CheckJpeg(const strFileName: String; img: TImage; const JpegDecType: TJpegDecType = jdtGDIPLUS): Boolean;
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
      Result := LoadJpeg_GDI(strFileName, img);
    jdtGDIPLUS:
      Result := LoadJpeg_GDIPLUS(strFileName, img);
    jdtSSE:
      Result := LoadJpeg_SSE(strFileName, img);
  end;
end;

procedure LoadImage(const strFileName: string; img: TImage; const JpegDecType: TJpegDecType = jdtGDIPLUS);
var
  bmp: TBitmap;
begin
  if CheckJpeg(strFileName, img, JpegDecType) then
    Exit;

  with TPicture.Create do
  begin
    LoadFromFile(strFileName);
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32bit;
      bmp.Width       := Width;
      bmp.Height      := Height;
      bmp.Canvas.Draw(0, 0, Graphic);
      img.Picture.Bitmap.Assign(bmp);
    finally
      bmp.Free;
    end;
    Free;
  end;
end;

end.
