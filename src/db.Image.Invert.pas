unit db.Image.Invert;
{
  Func: 32位位图反色
  Name: dbyoung@sina.com
  Date: 2021-01-07
  Vers: Delphi 11
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 X86、DEBUG 模式下的用时；
  Note: 并行程序，不能在 IDE 下运行查看效果。必须脱离 IDE 执行查看效果。

  基本原理：
  R = 255 - R;
  G = 255 - G;
  B = 255 - B;
  BGRA = 16777215 - BGRA  = NOT BGRA

  Delphi 参数寄存器顺序：
  X86: EAX, EDX, ECX
  X64: ECX, EDX, EAX

  通用寄存器：
  CPU  :
  EAX/EBX/ECX/EDX/EDI/ESI           32位 (x86)
  RAX/RBX/RCX/RDX/RDI/RSI           64位 (x64, EAX 寄存器是 RAX 寄存器的低 32 位)

  SIMD寄存器：
  MMX    :   MM0 --- MM7                             064位                                         ( 主要针对浮点运算 )
  SSE2   :  XMM0--- XMM7                             128位                                         ( 浮点 + 整数 )
  SSE4   :  XMM0--- XMM7(X86)  XMM0--- XMM15(X64)    128位                                         ( 浮点 + 整数 )
  AVX    :  YMM0--- YMM7(X86)  YMM0--- YMM15(X64)    256位 (XMM 寄存器是 YMM 寄存器的低 128 位)    ( 浮点 )
  AVX2   :  YMM0---YMM15                             256位 (XMM 寄存器是 YMM 寄存器的低 128 位)    ( 浮点 + 整数 )
  AVX512 :  ZMM0---ZMM31                             512位 (YMM 寄存器是 ZMM 寄存器的低 256 位)    ( 浮点 + 整数 )
}

interface

uses Winapi.Windows, Vcl.Graphics, System.Threading, db.Image.Common;

type
  TInvertType = (itScanLine, itDelphi, itASM, itMMX, itSSE, itAVX, itParallel, itParallel_SSE, itParallel_AVX);

procedure Invert(bmp: TBitmap; const gt: TInvertType = itParallel_SSE);

implementation

{ 51 ms }
procedure Invert_ScanLine(bmp: TBitmap);
var
  H, W  : Integer;
  pColor: PDWORD;
begin
  for H := 0 to bmp.Height - 1 do
  begin
    pColor := bmp.ScanLine[H];
    for W  := 0 to bmp.Width - 1 do
    begin
      pColor^ := not pColor^;
      Inc(pColor);
    end;
  end;
end;

{ 23 ms }
procedure Invert_Delphi(bmp: TBitmap);
var
  pColor  : PUInt64;
  I, count: Integer;
begin
  pColor := GetBitsPointer(bmp);
  count  := bmp.Width * bmp.Height div 2;
  for I  := 0 to count - 1 do
  begin
    pColor^ := not pColor^;
    Inc(pColor);
  end;
end;

procedure Invert_ASM_Proc(pColor: PRGBQuad; const count: Integer); register;
asm
  {$IFDEF WIN64}
  MOV   RAX, RCX
  {$IFEND}
  MOV   ECX, EDX

@LOOP:
  NOT   [EAX]

  ADD   EAX, 4
  DEC   ECX
  JNZ   @LOOP
end;

{ 14 ms }
procedure Invert_ASM(bmp: TBitmap);
begin
  Invert_ASM_Proc(GetBitsPointer(bmp), bmp.Width * bmp.Height);
end;

procedure Invert_MMX_Proc(pColor: PByte; const count: Integer); register;
asm
  {$IFDEF WIN64}
  MOV   RAX, RCX
  {$IFEND}
  MOV   ECX, EDX

@LOOP:
  PCMPEQD  MM0,  MM0
  PSUBD    MM0,  [EAX]
  MOVQ    [EAX], MM0

  ADD   EAX, 8
  SUB   ECX, 8
  JNZ   @LOOP

  EMMS
end;

{ 7 ms }
procedure Invert_MMX(bmp: TBitmap);
begin
  Invert_MMX_Proc(GetBitsPointer(bmp), bmp.Width * bmp.Height * 4);
end;

procedure Invert_SSE_Proc(pColor: PByte; const count: Integer); register;
asm
  {$IFDEF WIN64}
  MOV   RAX, RCX
  {$IFEND}
  MOV   ECX, EDX

@LOOP:
  PCMPEQD  XMM0, XMM0
  PSUBD    XMM0, [EAX]
  MOVUPS  [EAX], XMM0

  ADD   EAX, 16
  SUB   ECX, 16
  JNZ   @LOOP
end;

{ 7 ms }
procedure Invert_SSE(bmp: TBitmap);
begin
  Invert_SSE_Proc(GetBitsPointer(bmp), bmp.Width * bmp.Height * 4);
end;

procedure Invert_AVX_Proc(pColor: PByte; count: Integer);
asm
  {$IFDEF WIN64}
  MOV   RAX, RCX
  {$IFEND}
  MOV ECX, EDX

  VXORPS  XMM1, XMM1, XMM1
  VCMPPS  YMM1, YMM1, YMM1, 0

@LOOP:
  VMOVUPS YMM0,  [EAX]
  VXORPS  YMM0,  YMM0, YMM1
  VMOVDQU [EAX], YMM0

  ADD EAX, 32
  SUB ECX, 32
  JNZ @loop
end;

procedure Invert_AVX(bmp: TBitmap);
var
  pColor: PByte;
begin
  pColor := GetBitsPointer(bmp);
  Invert_AVX_Proc(pColor, bmp.Width * bmp.Height * 4);
end;

{ 7 ms }
procedure Invert_Parallel(bmp: TBitmap);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      I: Integer;
      pColor: PDWORD;
    begin
      pColor := PDWORD(StartScanLine + Y * bmpWidthBytes);
      for I := 0 to bmp.Width div 2 - 1 do
      begin
        pColor^ := not pColor^;
        Inc(pColor);
      end;
    end);
end;

procedure Invert_Parallel_SSE(bmp: TBitmap);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      pColor: PByte;
    begin
      pColor := PByte(StartScanLine + Y * bmpWidthBytes);
      Invert_SSE_Proc(pColor, bmp.Width * 4);
    end);
end;

procedure Invert_Parallel_AVX(bmp: TBitmap);
var
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
begin
  StartScanLine := Integer(bmp.ScanLine[0]);
  bmpWidthBytes := Integer(bmp.ScanLine[1]) - Integer(bmp.ScanLine[0]);

  TParallel.For(0, bmp.Height - 1,
    procedure(Y: Integer)
    var
      pColor: PByte;
    begin
      pColor := PByte(StartScanLine + Y * bmpWidthBytes);
      Invert_AVX_Proc(pColor, bmp.Width * 4);
    end);
end;

procedure Invert(bmp: TBitmap; const gt: TInvertType = itParallel_SSE);
begin
  case gt of
    itScanLine:
      Invert_ScanLine(bmp);     // 51 ms
    itDelphi:                   //
      Invert_Delphi(bmp);       // 23 ms
    itASM:                      //
      Invert_ASM(bmp);          // 11 ms
    itMMX:                      //
      Invert_MMX(bmp);          // 7 ms
    itSSE:                      //
      Invert_SSE(bmp);          // 5 ms
    itAVX:                      //
      Invert_AVX(bmp);          // 5 ms
    itParallel:                 //
      Invert_Parallel(bmp);     // 6 ms
    itParallel_SSE:             //
      Invert_Parallel_SSE(bmp); // 6 ms
    itParallel_AVX:             //
      Invert_Parallel_AVX(bmp); // 5 ms
  end;
end;

end.
