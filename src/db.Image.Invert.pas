unit db.Image.Invert;
{
  Func: 32位位图反色
  Name: dbyoung@sina.com
  Date: 2021-01-07
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；

  基本原理：
  R = 255 - R;
  G = 255 - G;
  B = 255 - B;
  ARGB = 16777215 - ARGB
}

{$IFDEF WIN32}
{$LINK obj\x86\Invert.obj}
{$LINK obj\x86\Invert_sse2.obj}
{$LINK obj\x86\Invert_sse4.obj}
{$LINK obj\x86\Invert_avx.obj}
{$LINK obj\x86\Invert_avx2.obj}
{$LINK obj\x86\Invert_avx512knl.obj}
{$LINK obj\x86\Invert_avx512skx.obj}
{$ELSE}
{$LINK obj\x64\Invert.obj}
{$LINK obj\x64\Invert_sse2.obj}
{$LINK obj\x64\Invert_sse4.obj}
{$LINK obj\x64\Invert_avx.obj}
{$LINK obj\x64\Invert_avx2.obj}
{$LINK obj\x64\Invert_avx512knl.obj}
{$LINK obj\x64\Invert_avx512skx.obj}
{$IFEND}

interface

uses Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils, {$IF CompilerVersion >= 24.0} System.Threading, {$IFEND} System.Diagnostics, System.SyncObjs, Vcl.Graphics, Winapi.GDIPOBJ, Winapi.GDIPAPI, db.Image.Common;

type
  TInvertType = (itDelphi, itASM, itMMX, itSSE, itSSE2, itSSE4, itAVX, itAVX2, itAVX512);

procedure Invert(bmp: TBitmap; const gt: TInvertType = itSSE4);

implementation

procedure Invert_sse2(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_Invert_sse2'{$IFEND};
procedure Invert_sse4(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_Invert_sse4'{$IFEND};
procedure Invert_avx(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_Invert_avx'{$IFEND};
procedure Invert_avx2(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_Invert_avx2'{$IFEND};
procedure Invert_avx512skx(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_Invert_avx512skx'{$IFEND};
procedure Invert_avx512knl(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_Invert_avx512knl'{$IFEND};

procedure Invert_Delphi(bmp: TBitmap);
var
  pColor  : PDWORD;
  I, Count: Integer;
begin
  pColor := GetBitsPointer(bmp);
  Count  := bmp.width * bmp.height;
  for I  := 0 to Count - 1 do
  begin
    pColor^ := c_InvertMask - pColor^;
    Inc(pColor);
  end;
end;

procedure Invert_ASM_Proc(pColor: PRGBQuad; const Count: Integer); register;
asm
  MOV   ECX, EDX

@LOOP:
  MOV   EDX,   c_InvertMask
  SUB   EDX,   [EAX]
  MOV   [EAX], EDX

  ADD   EAX, 4
  DEC   ECX
  JNZ   @LOOP
end;

procedure Invert_ASM(bmp: TBitmap);
begin
  Invert_ASM_Proc(GetBitsPointer(bmp), bmp.width * bmp.height);
end;

procedure Invert_MMX_Proc(pColor: PByte; const Count: Integer); register;
asm
  MOV   ECX, EDX

@LOOP:
  MOVQ   MM0,   c_InvertMask2
  PSUBD  MM0,   [EAX]
  MOVQ   [EAX], MM0

  ADD   EAX, 8
  SUB   ECX, 8
  JNZ   @LOOP

  EMMS
end;

procedure Invert_MMX(bmp: TBitmap);
begin
  Invert_MMX_Proc(GetBitsPointer(bmp), bmp.width * bmp.height * 4);
end;

procedure Invert_SSE_Proc(pColor: PByte; const Count: Integer); register;
asm
  {$IFDEF WIN64}
  MOV     RAX,  RCX
  {$IFEND}
  MOV   ECX, EDX

@LOOP:
  MOVSS   XMM0, [c_InvertMask]
  SHUFPS  XMM0, XMM0, 0

  PSUBD  XMM0,  [EAX]
  MOVUPS [EAX], XMM0

  ADD   EAX, 16
  SUB   ECX, 16
  JNZ   @LOOP
end;

procedure Invert_SSE(bmp: TBitmap);
begin
  Invert_SSE_Proc(GetBitsPointer(bmp), bmp.width * bmp.height * 4);
end;

procedure Invert(bmp: TBitmap; const gt: TInvertType = itSSE4);
begin
  case gt of
    itDelphi:                                                       //
      Invert_Delphi(bmp);                                           // 42 ms
    itASM:                                                          //
      Invert_ASM(bmp);                                              // 13 ms
    itMMX:                                                          //
      Invert_MMX(bmp);                                              // 9 ms
    itSSE:                                                          //
      Invert_SSE(bmp);                                              // 7 ms
    itSSE2:                                                         //
      Invert_sse2(GetBitsPointer(bmp), bmp.width, bmp.height);      // 7 ms
    itSSE4:                                                         //
      Invert_sse4(GetBitsPointer(bmp), bmp.width, bmp.height);      // 7 ms
    itAVX:                                                          //
      Invert_avx(GetBitsPointer(bmp), bmp.width, bmp.height);       // 7 ms
    itAVX2:                                                         //
      Invert_avx2(GetBitsPointer(bmp), bmp.width, bmp.height);      // 7 ms
    itAVX512:                                                       //
      Invert_avx512skx(GetBitsPointer(bmp), bmp.width, bmp.height); // 7 ms
  end;
end;

end.
