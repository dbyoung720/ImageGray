unit db.Memory.Move;
{
  Func: 内存复制优化
  Name: dbyoung@sina.com
  Date: 2021-3-2
  Vers: Delphi 10.4.2

  未按 16 字节对齐
  1   Move______Byte
  2   Move______WORD
  4   Move_____DWORD
  8   Move____UINT64
  16  Move_SSE2_016U
  32  Move_SSE2_032U
  48  Move_SSE2_048U
  64  Move_SSE2_064U
  80  Move_SSE2_080U
  96  Move_SSE2_096U
  112 Move_SSE2_112U
  128 Move_SSE2_128U
  144 Move_SSE2_144U
  160 Move_SSE2_160U
  176 Move_SSE2_176U
  192 Move_SSE2_192U
  208 Move_SSE2_208U
  224 Move_SSE2_224U
  240 Move_SSE2_240U
  256 Move_SSE2_256U
  32  Move_AVX1_032U
  64  Move_AVX1_064U
  96  Move_AVX1_096U
  128 Move_AVX1_128U
  160 Move_AVX1_160U
  192 Move_AVX1_192U
  224 Move_AVX1_224U
  256 Move_AVX1_256U
  288 Move_AVX1_288U
  320 Move_AVX1_320U
  352 Move_AVX1_352U
  384 Move_AVX1_384U
  416 Move_AVX1_416U
  448 Move_AVX1_448U
  480 Move_AVX1_480U
  512 Move_AVX1_512U

  按 16(SSE) / 32(AVX) 字节对齐
  16  Move_SSE2_016A
  32  Move_SSE2_032A
  64  Move_SSE2_064A
  128 Move_SSE2_128A
  256 Move_SSE2_256A
  16  Move_SSE4_016A
  32  Move_SSE4_032A
  64  Move_SSE4_064A
  128 Move_SSE4_128A
  256 Move_SSE4_256A
  32  Move_AVX1_032A
  64  Move_AVX1_064A
  128 Move_AVX1_128A
  256 Move_AVX1_256A
  512 Move_AVX1_512A
  32  Move_AVX2_032A
  64  Move_AVX2_064A
  128 Move_AVX2_128A
  256 Move_AVX2_256A
  512 Move_AVX2_512A


  x64 默认内存地址按 16 字节对齐；

  复制速度：
  1、16 字节对齐  >  未 16 字节对齐
  2、AVX2 > AVX1 > SSE4 > SSE2

  什么是 16 字节对齐？
  内存地址能被 16 整除。
  内存地址的最后一位必须是 0；
  操作的俩个数的内存地址，都要是 16 字节对齐，才能使用 SSE 对齐指令；

  什么是 32 字节对齐？
  内存地址能被 32 整除。
  内存地址的最后两位必须类似于： $..20, $..40, $..60, $..80, $..A0, $..C0, $..E0 。即内存地址的最后一位必须是 0，倒数第二位必须是偶数。
  操作的俩个数的内存地址，都要是 32 字节对齐，才能使用 AVX 对齐指令；
}

interface

{ 未按 16 字节对齐 }
procedure Move______Byte(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move______WORD(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_____DWORD(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move____UINT64(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_016U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_032U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_048U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_064U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_080U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_096U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_112U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_128U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_144U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_160U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_176U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_192U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_208U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_224U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_240U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_256U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_032U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_064U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_096U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_128U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_160U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_192U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_224U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_256U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_288U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_320U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_352U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_384U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_416U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_448U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_480U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_512U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;

{ 按 16(SSE) / 32(AVX) 字节对齐 }
procedure Move_SSE2_016A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE2_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE4_016A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE4_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE4_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE4_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_SSE4_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX1_512A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX2_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX2_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX2_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX2_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
procedure Move_AVX2_512A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;

implementation

{ ---------------------------------------------------------------------------------------------------------------------------------------- }
{ --------------------------------------------------------- 数据的内存地址，未按 16 字节对齐 --------------------------------------------- }
{ ---------------------------------------------------------------------------------------------------------------------------------------- }

type
  DWORD  = FixedUInt;
  PDWORD = ^DWORD;

  { 每一次复制 1 个字节 <Byte> }
procedure Move______Byte(const src: Pointer; dst: Pointer; const len: NativeInt);
var
  pSrc: PByte;
  pDst: PByte;
  I   : NativeInt;
begin
  pSrc  := PByte(src);     // 取 src 首地址
  pDst  := PByte(dst);     // 取 dst 首地址
  for I := 0 to len - 1 do //
  begin                    //
    pDst^ := pSrc^;        // 把 pSrc 源地址中的值 ---> 赋给 pDst 目地址的值；按字节(BYTE)赋值
    Inc(pSrc);             // 源地址加 1
    Inc(pDst);             // 目地址加 1
  end;
end;

{ 每一次复制 2 个字节 <WORD> <长度必须是 2 的倍数> }
procedure Move______WORD(const src: Pointer; dst: Pointer; const len: NativeInt);
var
  pSrc: PWORD;
  pDst: PWORD;
  I   : NativeInt;
begin
  pSrc  := PWORD(src);           // 取 src 首地址
  pDst  := PWORD(dst);           // 取 dst 首地址
  for I := 0 to len div 2 - 1 do //
  begin                          //
    pDst^ := pSrc^;              // 把 pSrc 源地址中的值 ---> 赋给 pDst 目地址的值；按双字节(WORD)赋值
    Inc(pSrc);                   // 源地址加 2
    Inc(pDst);                   // 目地址加 2
  end;
end;

{ 每一次复制 4 个字节 <DWORD> <长度必须是 4 的倍数> }
procedure Move_____DWORD(const src: Pointer; dst: Pointer; const len: NativeInt);
var
  pSrc: PDWORD;
  pDst: PDWORD;
  I   : NativeInt;
begin
  pSrc  := PDWORD(src);          // 取 src 首地址
  pDst  := PDWORD(dst);          // 取 dst 首地址
  for I := 0 to len div 4 - 1 do //
  begin                          //
    pDst^ := pSrc^;              // 把 pSrc 源地址中的值 ---> 赋给 pDst 目地址的值；按4字节(DWORD)赋值
    Inc(pSrc);                   // 源地址加 4
    Inc(pDst);                   // 目地址加 4
  end;
end;

{ 每一次复制 8 个字节 <UINT64> <长度必须是 8 的倍数> }
procedure Move____UINT64(const src: Pointer; dst: Pointer; const len: NativeInt);
var
  pSrc: PUInt64;
  pDst: PUInt64;
  I   : NativeInt;
begin
  pSrc  := PUInt64(src);         // 取 src 首地址
  pDst  := PUInt64(dst);         // 取 dst 首地址
  for I := 0 to len div 8 - 1 do //
  begin                          //
    pDst^ := pSrc^;              // 把 pSrc 源地址中的值 ---> 赋给 pDst 目地址的值；按8字节(UINT64)赋值
    Inc(pSrc);                   // 源地址加 8
    Inc(pDst);                   // 目地址加 8
  end;
end;

{ 每一次复制 16 个字节 <128 位> <长度必须是 16 的倍数> }
procedure Move_SSE2_016U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX]
  MOVDQU  [EDX], XMM0

  ADD    EAX, 16
  ADD    EDX, 16
  SUB    ECX, 16
  JNZ    @LOOP
end;

{ 每一次复制 32 个字节 <256 位> <长度必须是 32 的倍数> }
procedure Move_SSE2_032U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX+00]
  LDDQU   XMM1, [EAX+16]
  MOVDQU  [EDX+00], XMM0
  MOVDQU  [EDX+16], XMM1

  ADD    EAX, 32
  ADD    EDX, 32
  SUB    ECX, 32
  JNZ    @LOOP
end;

{ 每一次复制 48 个字节 <512 位> <长度必须是 48 的倍数> }
procedure Move_SSE2_048U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX+00]
  LDDQU   XMM1, [EAX+16]
  LDDQU   XMM2, [EAX+32]
  MOVDQU  [EDX+00], XMM0
  MOVDQU  [EDX+16], XMM1
  MOVDQU  [EDX+32], XMM2

  ADD    EAX, 48
  ADD    EDX, 48
  SUB    ECX, 48
  JNZ    @LOOP
end;

{ 每一次复制 64 个字节 <512 位> <长度必须是 64 的倍数> }
procedure Move_SSE2_064U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX+00]
  LDDQU   XMM1, [EAX+16]
  LDDQU   XMM2, [EAX+32]
  LDDQU   XMM3, [EAX+48]
  MOVDQU  [EDX+00], XMM0
  MOVDQU  [EDX+16], XMM1
  MOVDQU  [EDX+32], XMM2
  MOVDQU  [EDX+48], XMM3

  ADD    EAX, 64
  ADD    EDX, 64
  SUB    ECX, 64
  JNZ    @LOOP
end;

{ 每一次复制 80 个字节 <640 位> <长度必须是 80 的倍数> }
procedure Move_SSE2_080U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX+00]
  LDDQU   XMM1, [EAX+16]
  LDDQU   XMM2, [EAX+32]
  LDDQU   XMM3, [EAX+48]
  LDDQU   XMM4, [EAX+64]
  MOVDQU  [EDX+00], XMM0
  MOVDQU  [EDX+16], XMM1
  MOVDQU  [EDX+32], XMM2
  MOVDQU  [EDX+48], XMM3
  MOVDQU  [EDX+64], XMM4

  ADD    EAX, 80
  ADD    EDX, 80
  SUB    ECX, 80
  JNZ    @LOOP
end;

{ 每一次复制 96 个字节 <768 位> <长度必须是 96 的倍数> }
procedure Move_SSE2_096U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX+00]
  LDDQU   XMM1, [EAX+16]
  LDDQU   XMM2, [EAX+32]
  LDDQU   XMM3, [EAX+48]
  LDDQU   XMM4, [EAX+64]
  LDDQU   XMM5, [EAX+80]
  MOVDQU  [EDX+00], XMM0
  MOVDQU  [EDX+16], XMM1
  MOVDQU  [EDX+32], XMM2
  MOVDQU  [EDX+48], XMM3
  MOVDQU  [EDX+64], XMM4
  MOVDQU  [EDX+80], XMM5

  ADD    EAX, 96
  ADD    EDX, 96
  SUB    ECX, 96
  JNZ    @LOOP
end;

{ 每一次复制 112 个字节 <896 位> <长度必须是 112 的倍数> }
procedure Move_SSE2_112U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX+000]
  LDDQU   XMM1, [EAX+016]
  LDDQU   XMM2, [EAX+032]
  LDDQU   XMM3, [EAX+048]
  LDDQU   XMM4, [EAX+064]
  LDDQU   XMM5, [EAX+080]
  LDDQU   XMM6, [EAX+096]
  MOVDQU  [EDX+000], XMM0
  MOVDQU  [EDX+016], XMM1
  MOVDQU  [EDX+032], XMM2
  MOVDQU  [EDX+048], XMM3
  MOVDQU  [EDX+064], XMM4
  MOVDQU  [EDX+080], XMM5
  MOVDQU  [EDX+096], XMM6

  ADD    EAX, 112
  ADD    EDX, 112
  SUB    ECX, 112
  JNZ    @LOOP
end;

{ 每一次复制 128 个字节 <1024 位> <长度必须是 128 的倍数> }
procedure Move_SSE2_128U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  LDDQU   XMM0, [EAX+000]
  LDDQU   XMM1, [EAX+016]
  LDDQU   XMM2, [EAX+032]
  LDDQU   XMM3, [EAX+048]
  LDDQU   XMM4, [EAX+064]
  LDDQU   XMM5, [EAX+080]
  LDDQU   XMM6, [EAX+096]
  LDDQU   XMM7, [EAX+112]
  MOVDQU  [EDX+000], XMM0
  MOVDQU  [EDX+016], XMM1
  MOVDQU  [EDX+032], XMM2
  MOVDQU  [EDX+048], XMM3
  MOVDQU  [EDX+064], XMM4
  MOVDQU  [EDX+080], XMM5
  MOVDQU  [EDX+096], XMM6
  MOVDQU  [EDX+112], XMM7

  ADD    EAX, 128
  ADD    EDX, 128
  SUB    ECX, 128
  JNZ    @LOOP
end;

{ 每一次复制 144 个字节 <1152 位> <长度必须是 144 的倍数> }
procedure Move_SSE2_144U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  MOVDQU  [EDX+128],  XMM0
  {$ENDIF}

  ADD    EAX, 144
  ADD    EDX, 144
  SUB    ECX, 144
  JNZ    @LOOP
end;

{ 每一次复制 160 个字节 <1280 位> <长度必须是 160 的倍数> }
procedure Move_SSE2_160U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]
  LDDQU   XMM9,  [EAX+144]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  MOVDQU  [EDX+144],  XMM9
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  LDDQU   XMM1,  [EAX+144]
  MOVDQU  [EDX+128],  XMM0
  MOVDQU  [EDX+144],  XMM1
  {$ENDIF}

  ADD    EAX, 160
  ADD    EDX, 160
  SUB    ECX, 160
  JNZ    @LOOP
end;

{ 每一次复制 176 个字节 <1408 位> <长度必须是 176 的倍数> }
procedure Move_SSE2_176U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]
  LDDQU   XMM9,  [EAX+144]
  LDDQU   XMM10, [EAX+160]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  MOVDQU  [EDX+144],  XMM9
  MOVDQU  [EDX+160], XMM10
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  LDDQU   XMM1,  [EAX+144]
  LDDQU   XMM2,  [EAX+160]
  MOVDQU  [EDX+128],  XMM0
  MOVDQU  [EDX+144],  XMM1
  MOVDQU  [EDX+160],  XMM2
  {$ENDIF}

  ADD    EAX, 176
  ADD    EDX, 176
  SUB    ECX, 176
  JNZ    @LOOP
end;

{ 每一次复制 192 个字节 <1536 位> <长度必须是 192 的倍数> }
procedure Move_SSE2_192U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]
  LDDQU   XMM9,  [EAX+144]
  LDDQU   XMM10, [EAX+160]
  LDDQU   XMM11, [EAX+176]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  MOVDQU  [EDX+144],  XMM9
  MOVDQU  [EDX+160], XMM10
  MOVDQU  [EDX+176], XMM11
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  LDDQU   XMM1,  [EAX+144]
  LDDQU   XMM2,  [EAX+160]
  LDDQU   XMM3,  [EAX+176]
  MOVDQU  [EDX+128],  XMM0
  MOVDQU  [EDX+144],  XMM1
  MOVDQU  [EDX+160],  XMM2
  MOVDQU  [EDX+176],  XMM3
  {$ENDIF}

  ADD    EAX, 192
  ADD    EDX, 192
  SUB    ECX, 192
  JNZ    @LOOP
end;

{ 每一次复制 208 个字节 <1664 位> <长度必须是 208 的倍数> }
procedure Move_SSE2_208U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]
  LDDQU   XMM9,  [EAX+144]
  LDDQU   XMM10, [EAX+160]
  LDDQU   XMM11, [EAX+176]
  LDDQU   XMM12, [EAX+192]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  MOVDQU  [EDX+144],  XMM9
  MOVDQU  [EDX+160], XMM10
  MOVDQU  [EDX+176], XMM11
  MOVDQU  [EDX+192], XMM12
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  LDDQU   XMM1,  [EAX+144]
  LDDQU   XMM2,  [EAX+160]
  LDDQU   XMM3,  [EAX+176]
  LDDQU   XMM4,  [EAX+192]
  MOVDQU  [EDX+128],  XMM0
  MOVDQU  [EDX+144],  XMM1
  MOVDQU  [EDX+160],  XMM2
  MOVDQU  [EDX+176],  XMM3
  MOVDQU  [EDX+192],  XMM4
  {$ENDIF}

  ADD    EAX, 208
  ADD    EDX, 208
  SUB    ECX, 208
  JNZ    @LOOP
end;

{ 每一次复制 224 个字节 <1792 位> <长度必须是 224 的倍数> }
procedure Move_SSE2_224U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]
  LDDQU   XMM9,  [EAX+144]
  LDDQU   XMM10, [EAX+160]
  LDDQU   XMM11, [EAX+176]
  LDDQU   XMM12, [EAX+192]
  LDDQU   XMM13, [EAX+208]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  MOVDQU  [EDX+144],  XMM9
  MOVDQU  [EDX+160], XMM10
  MOVDQU  [EDX+176], XMM11
  MOVDQU  [EDX+192], XMM12
  MOVDQU  [EDX+208], XMM13
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  LDDQU   XMM1,  [EAX+144]
  LDDQU   XMM2,  [EAX+160]
  LDDQU   XMM3,  [EAX+176]
  LDDQU   XMM4,  [EAX+192]
  LDDQU   XMM5,  [EAX+208]
  MOVDQU  [EDX+128],  XMM0
  MOVDQU  [EDX+144],  XMM1
  MOVDQU  [EDX+160],  XMM2
  MOVDQU  [EDX+176],  XMM3
  MOVDQU  [EDX+192],  XMM4
  MOVDQU  [EDX+208],  XMM5
  {$ENDIF}

  ADD    EAX, 224
  ADD    EDX, 224
  SUB    ECX, 224
  JNZ    @LOOP
end;

{ 每一次复制 240 个字节 <1920 位> <长度必须是 240 的倍数> }
procedure Move_SSE2_240U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]
  LDDQU   XMM9,  [EAX+144]
  LDDQU   XMM10, [EAX+160]
  LDDQU   XMM11, [EAX+176]
  LDDQU   XMM12, [EAX+192]
  LDDQU   XMM13, [EAX+208]
  LDDQU   XMM14, [EAX+224]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  MOVDQU  [EDX+144],  XMM9
  MOVDQU  [EDX+160], XMM10
  MOVDQU  [EDX+176], XMM11
  MOVDQU  [EDX+192], XMM12
  MOVDQU  [EDX+208], XMM13
  MOVDQU  [EDX+224], XMM14
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  LDDQU   XMM1,  [EAX+144]
  LDDQU   XMM2,  [EAX+160]
  LDDQU   XMM3,  [EAX+176]
  LDDQU   XMM4,  [EAX+192]
  LDDQU   XMM5,  [EAX+208]
  LDDQU   XMM6,  [EAX+224]
  MOVDQU  [EDX+128],  XMM0
  MOVDQU  [EDX+144],  XMM1
  MOVDQU  [EDX+160],  XMM2
  MOVDQU  [EDX+176],  XMM3
  MOVDQU  [EDX+192],  XMM4
  MOVDQU  [EDX+208],  XMM5
  MOVDQU  [EDX+224],  XMM6
  {$ENDIF}

  ADD    EAX, 256
  ADD    EDX, 256
  SUB    ECX, 256
  JNZ    @LOOP
end;

{ 每一次复制 256 个字节 <2048 位> <长度必须是 256 的倍数> }
procedure Move_SSE2_256U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  LDDQU   XMM8,  [EAX+128]
  LDDQU   XMM9,  [EAX+144]
  LDDQU   XMM10, [EAX+160]
  LDDQU   XMM11, [EAX+176]
  LDDQU   XMM12, [EAX+192]
  LDDQU   XMM13, [EAX+208]
  LDDQU   XMM14, [EAX+224]
  LDDQU   XMM15, [EAX+240]

  MOVDQU  [EDX+000],  XMM0
  MOVDQU  [EDX+016],  XMM1
  MOVDQU  [EDX+032],  XMM2
  MOVDQU  [EDX+048],  XMM3
  MOVDQU  [EDX+064],  XMM4
  MOVDQU  [EDX+080],  XMM5
  MOVDQU  [EDX+096],  XMM6
  MOVDQU  [EDX+112],  XMM7
  MOVDQU  [EDX+128],  XMM8
  MOVDQU  [EDX+144],  XMM9
  MOVDQU  [EDX+160], XMM10
  MOVDQU  [EDX+176], XMM11
  MOVDQU  [EDX+192], XMM12
  MOVDQU  [EDX+208], XMM13
  MOVDQU  [EDX+224], XMM14
  MOVDQU  [EDX+240], XMM15
  {$ELSE}
@LOOP:
  LDDQU   XMM0,  [EAX+000]
  LDDQU   XMM1,  [EAX+016]
  LDDQU   XMM2,  [EAX+032]
  LDDQU   XMM3,  [EAX+048]
  LDDQU   XMM4,  [EAX+064]
  LDDQU   XMM5,  [EAX+080]
  LDDQU   XMM6,  [EAX+096]
  LDDQU   XMM7,  [EAX+112]
  MOVDQU [EDX+000],  XMM0
  MOVDQU [EDX+016],  XMM1
  MOVDQU [EDX+032],  XMM2
  MOVDQU [EDX+048],  XMM3
  MOVDQU [EDX+064],  XMM4
  MOVDQU [EDX+080],  XMM5
  MOVDQU [EDX+096],  XMM6
  MOVDQU [EDX+112],  XMM7

  LDDQU   XMM0,  [EAX+128]
  LDDQU   XMM1,  [EAX+144]
  LDDQU   XMM2,  [EAX+160]
  LDDQU   XMM3,  [EAX+176]
  LDDQU   XMM4,  [EAX+192]
  LDDQU   XMM5,  [EAX+208]
  LDDQU   XMM6,  [EAX+224]
  LDDQU   XMM7,  [EAX+240]
  MOVDQU  [EDX+128],  XMM0
  MOVDQU  [EDX+144],  XMM1
  MOVDQU  [EDX+160],  XMM2
  MOVDQU  [EDX+176],  XMM3
  MOVDQU  [EDX+192],  XMM4
  MOVDQU  [EDX+208],  XMM5
  MOVDQU  [EDX+224],  XMM6
  MOVDQU  [EDX+240],  XMM7
  {$ENDIF}

  ADD    EAX, 256
  ADD    EDX, 256
  SUB    ECX, 256
  JNZ    @LOOP
end;

{ 每一次复制 32 个字节 <256 位> <长度必须是 32 的倍数> }
procedure Move_AVX1_032U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0, [EAX]
  VMOVDQU  [EDX], YMM0
  {$ENDIF}

  {$IFDEF WIN64}
  DB  $67, $C5, $FF, $F0, $00
  DB  $67, $C5, $FE, $7F, $02
  {$ELSE}
  DB  $C5, $FF, $F0, $00
  DB  $C5, $FE, $7F, $02
  {$ENDIF}

  ADD    EAX, 32
  ADD    EDX, 32
  SUB    ECX, 32
  JNZ    @LOOP
end;

{ 每一次复制 64 个字节 <512 位> <长度必须是 64 的倍数> }
procedure Move_AVX1_064U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0, [EAX+00]
  VLDDQU   YMM1, [EAX+32]
  VMOVDQU  [EDX+00], YMM0
  VMOVDQU  [EDX+32], YMM1
  {$ENDIF}

  {$IFDEF WIN64}
  DB  $67, $C5, $FF, $F0, $00
  DB  $67, $C5, $FF, $F0, $48, $20
  DB  $67, $C5, $FE, $7F, $02
  DB  $67, $C5, $FE, $7F, $4A, $20
  {$ELSE}
  DB  $C5, $FF, $F0, $00
  DB  $C5, $FF, $F0, $48, $20
  DB  $C5, $FE, $7F, $02
  DB  $C5, $FE, $7F, $4A, $20
  {$ENDIF}

  ADD    EAX, 64
  ADD    EDX, 64
  SUB    ECX, 64
  JNZ    @LOOP
end;

{ 每一次复制 96 个字节 <768 位> <长度必须是 96 的倍数> }
procedure Move_AVX1_096U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0, [EAX+00]
  VLDDQU   YMM1, [EAX+32]
  VLDDQU   YMM2, [EAX+64]
  VMOVDQU  [EDX+00], YMM0
  VMOVDQU  [EDX+32], YMM1
  VMOVDQU  [EDX+64], YMM2
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  {$ENDIF}

  ADD    EAX, 96
  ADD    EDX, 96
  SUB    ECX, 96
  JNZ    @LOOP
end;

{ 每一次复制 128 个字节 <1024 位> <长度必须是 128 的倍数> }
procedure Move_AVX1_128U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0, [EAX+00]
  VLDDQU   YMM1, [EAX+32]
  VLDDQU   YMM2, [EAX+64]
  VLDDQU   YMM3, [EAX+96]
  VMOVDQU  [EDX+00], YMM0
  VMOVDQU  [EDX+32], YMM1
  VMOVDQU  [EDX+64], YMM2
  VMOVDQU  [EDX+96], YMM3
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  {$ENDIF}

  ADD    EAX, 128
  ADD    EDX, 128
  SUB    ECX, 128
  JNZ    @LOOP
end;

{ 每一次复制 160 个字节 <1280 位> <长度必须是 160 的倍数> }
procedure Move_AVX1_160U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  {$ENDIF}

  ADD    EAX, 160
  ADD    EDX, 160
  SUB    ECX, 160
  JNZ    @LOOP
end;

{ 每一次复制 192 个字节 <1536 位> <长度必须是 192 的倍数> }
procedure Move_AVX1_192U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  {$ENDIF}

  ADD    EAX, 192
  ADD    EDX, 192
  SUB    ECX, 192
  JNZ    @LOOP
end;

{ 每一次复制 224 个字节 <1792 位> <长度必须是 224 的倍数> }
procedure Move_AVX1_224U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  {$ENDIF}

  ADD    EAX, 224
  ADD    EDX, 224
  SUB    ECX, 224
  JNZ    @LOOP
end;

{ 每一次复制 256 个字节 <2048 位> <长度必须是 256 的倍数> }
procedure Move_AVX1_256U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VLDDQU   YMM0, [EAX+000]
  VLDDQU   YMM1, [EAX+032]
  VLDDQU   YMM2, [EAX+064]
  VLDDQU   YMM3, [EAX+096]
  VLDDQU   YMM4, [EAX+128]
  VLDDQU   YMM5, [EAX+160]
  VLDDQU   YMM6, [EAX+192]
  VLDDQU   YMM7, [EAX+224]
  VMOVDQU  [EDX+000], YMM0
  VMOVDQU  [EDX+032], YMM1
  VMOVDQU  [EDX+064], YMM2
  VMOVDQU  [EDX+096], YMM3
  VMOVDQU  [EDX+128], YMM4
  VMOVDQU  [EDX+160], YMM5
  VMOVDQU  [EDX+192], YMM6
  VMOVDQU  [EDX+224], YMM7
  {$ENDIF}

  {$IFDEF WIN64}
  DB  $67, $C5, $FF, $F0, $00
  DB  $67, $C5, $FF, $F0, $48, $20
  DB  $67, $C5, $FF, $F0, $50, $40
  DB  $67, $C5, $FF, $F0, $58, $60
  DB  $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB  $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB  $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB  $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB  $67, $C5, $FE, $7F, $02
  DB  $67, $C5, $FE, $7F, $4A, $20
  DB  $67, $C5, $FE, $7F, $52, $40
  DB  $67, $C5, $FE, $7F, $5A, $60
  DB  $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB  $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB  $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB  $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  {$ELSE}
  DB  $C5, $FF, $F0, $00
  DB  $C5, $FF, $F0, $48, $20
  DB  $C5, $FF, $F0, $50, $40
  DB  $C5, $FF, $F0, $58, $60
  DB  $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB  $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB  $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB  $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB  $C5, $FE, $7F, $02
  DB  $C5, $FE, $7F, $4A, $20
  DB  $C5, $FE, $7F, $52, $40
  DB  $C5, $FE, $7F, $5A, $60
  DB  $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB  $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB  $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB  $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  {$ENDIF}

  ADD    EAX, 256
  ADD    EDX, 256
  SUB    ECX, 256
  JNZ    @LOOP
end;

{ 每一次复制 288 个字节 <2304 位> <长度必须是 288 的倍数> }
procedure Move_AVX1_288U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0,  [EAX+256]
  VMOVDQU  [EDX+256],  YMM0
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 288
  ADD    EDX, 288
  SUB    ECX, 288
  JNZ    @LOOP
end;

{ 每一次复制 320 个字节 <2560 位> <长度必须是 320 的倍数> }
procedure Move_AVX1_320U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VLDDQU   YMM9,  [EAX+288]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  VMOVDQU  [EDX+288],  YMM9
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0, [EAX+256]
  VLDDQU   YMM1, [EAX+288]
  VMOVDQU  [EDX+256], YMM0
  VMOVDQU  [EDX+288], YMM1
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $7F, $F0, $88, $20, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7E, $7F, $8A, $20, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FF, $F0, $88, $20, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  DB $C5, $FE, $7F, $8A, $20, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 320
  ADD    EDX, 320
  SUB    ECX, 320
  JNZ    @LOOP
end;

{ 每一次复制 352 个字节 <2816 位> <长度必须是 352 的倍数> }
procedure Move_AVX1_352U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VLDDQU   YMM9,  [EAX+288]
  VLDDQU   YMM10, [EAX+320]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  VMOVDQU  [EDX+288],  YMM9
  VMOVDQU  [EDX+320], YMM10
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0, [EAX+256]
  VLDDQU   YMM1, [EAX+288]
  VLDDQU   YMM2, [EAX+320]
  VMOVDQU  [EDX+256], YMM0
  VMOVDQU  [EDX+288], YMM1
  VMOVDQU  [EDX+320], YMM2
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $7F, $F0, $88, $20, $01, $00, $00
  DB $67, $C5, $7F, $F0, $90, $40, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7E, $7F, $8A, $20, $01, $00, $00
  DB $67, $C5, $7E, $7F, $92, $40, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FF, $F0, $88, $20, $01, $00, $00
  DB $C5, $FF, $F0, $90, $40, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  DB $C5, $FE, $7F, $8A, $20, $01, $00, $00
  DB $C5, $FE, $7F, $92, $40, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 352
  ADD    EDX, 352
  SUB    ECX, 352
  JNZ    @LOOP
end;

{ 每一次复制 384 个字节 <3072 位> <长度必须是 384 的倍数> }
procedure Move_AVX1_384U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VLDDQU   YMM9,  [EAX+288]
  VLDDQU   YMM10, [EAX+320]
  VLDDQU   YMM11, [EAX+352]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  VMOVDQU  [EDX+288],  YMM9
  VMOVDQU  [EDX+320], YMM10
  VMOVDQU  [EDX+352], YMM11
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0, [EAX+256]
  VLDDQU   YMM1, [EAX+288]
  VLDDQU   YMM2, [EAX+320]
  VLDDQU   YMM3, [EAX+352]
  VMOVDQU  [EDX+256], YMM0
  VMOVDQU  [EDX+288], YMM1
  VMOVDQU  [EDX+320], YMM2
  VMOVDQU  [EDX+352], YMM3
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $7F, $F0, $88, $20, $01, $00, $00
  DB $67, $C5, $7F, $F0, $90, $40, $01, $00, $00
  DB $67, $C5, $7F, $F0, $98, $60, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7E, $7F, $8A, $20, $01, $00, $00
  DB $67, $C5, $7E, $7F, $92, $40, $01, $00, $00
  DB $67, $C5, $7E, $7F, $9A, $60, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FF, $F0, $88, $20, $01, $00, $00
  DB $C5, $FF, $F0, $90, $40, $01, $00, $00
  DB $C5, $FF, $F0, $98, $60, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  DB $C5, $FE, $7F, $8A, $20, $01, $00, $00
  DB $C5, $FE, $7F, $92, $40, $01, $00, $00
  DB $C5, $FE, $7F, $9A, $60, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 384
  ADD    EDX, 384
  SUB    ECX, 384
  JNZ    @LOOP
end;

{ 每一次复制 416 个字节 <3328 位> <长度必须是 416 的倍数> }
procedure Move_AVX1_416U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VLDDQU   YMM9,  [EAX+288]
  VLDDQU   YMM10, [EAX+320]
  VLDDQU   YMM11, [EAX+352]
  VLDDQU   YMM12, [EAX+384]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  VMOVDQU  [EDX+288],  YMM9
  VMOVDQU  [EDX+320], YMM10
  VMOVDQU  [EDX+352], YMM11
  VMOVDQU  [EDX+384], YMM12
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0, [EAX+256]
  VLDDQU   YMM1, [EAX+288]
  VLDDQU   YMM2, [EAX+320]
  VLDDQU   YMM3, [EAX+352]
  VLDDQU   YMM4, [EAX+384]
  VMOVDQU  [EDX+256], YMM0
  VMOVDQU  [EDX+288], YMM1
  VMOVDQU  [EDX+320], YMM2
  VMOVDQU  [EDX+352], YMM3
  VMOVDQU  [EDX+384], YMM4
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $7F, $F0, $88, $20, $01, $00, $00
  DB $67, $C5, $7F, $F0, $90, $40, $01, $00, $00
  DB $67, $C5, $7F, $F0, $98, $60, $01, $00, $00
  DB $67, $C5, $7F, $F0, $A0, $80, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7E, $7F, $8A, $20, $01, $00, $00
  DB $67, $C5, $7E, $7F, $92, $40, $01, $00, $00
  DB $67, $C5, $7E, $7F, $9A, $60, $01, $00, $00
  DB $67, $C5, $7E, $7F, $A2, $80, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FF, $F0, $88, $20, $01, $00, $00
  DB $C5, $FF, $F0, $90, $40, $01, $00, $00
  DB $C5, $FF, $F0, $98, $60, $01, $00, $00
  DB $C5, $FF, $F0, $A0, $80, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  DB $C5, $FE, $7F, $8A, $20, $01, $00, $00
  DB $C5, $FE, $7F, $92, $40, $01, $00, $00
  DB $C5, $FE, $7F, $9A, $60, $01, $00, $00
  DB $C5, $FE, $7F, $A2, $80, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 416
  ADD    EDX, 416
  SUB    ECX, 416
  JNZ    @LOOP
end;

{ 每一次复制 448 个字节 <3584 位> <长度必须是 448 的倍数> }
procedure Move_AVX1_448U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VLDDQU   YMM9,  [EAX+288]
  VLDDQU   YMM10, [EAX+320]
  VLDDQU   YMM11, [EAX+352]
  VLDDQU   YMM12, [EAX+384]
  VLDDQU   YMM13, [EAX+416]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  VMOVDQU  [EDX+288],  YMM9
  VMOVDQU  [EDX+320], YMM10
  VMOVDQU  [EDX+352], YMM11
  VMOVDQU  [EDX+384], YMM12
  VMOVDQU  [EDX+416], YMM13
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0, [EAX+256]
  VLDDQU   YMM1, [EAX+288]
  VLDDQU   YMM2, [EAX+320]
  VLDDQU   YMM3, [EAX+352]
  VLDDQU   YMM4, [EAX+384]
  VLDDQU   YMM5, [EAX+416]
  VMOVDQU  [EDX+256], YMM0
  VMOVDQU  [EDX+288], YMM1
  VMOVDQU  [EDX+320], YMM2
  VMOVDQU  [EDX+352], YMM3
  VMOVDQU  [EDX+384], YMM4
  VMOVDQU  [EDX+416], YMM5
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $7F, $F0, $88, $20, $01, $00, $00
  DB $67, $C5, $7F, $F0, $90, $40, $01, $00, $00
  DB $67, $C5, $7F, $F0, $98, $60, $01, $00, $00
  DB $67, $C5, $7F, $F0, $A0, $80, $01, $00, $00
  DB $67, $C5, $7F, $F0, $A8, $A0, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7E, $7F, $8A, $20, $01, $00, $00
  DB $67, $C5, $7E, $7F, $92, $40, $01, $00, $00
  DB $67, $C5, $7E, $7F, $9A, $60, $01, $00, $00
  DB $67, $C5, $7E, $7F, $A2, $80, $01, $00, $00
  DB $67, $C5, $7E, $7F, $AA, $A0, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FF, $F0, $88, $20, $01, $00, $00
  DB $C5, $FF, $F0, $90, $40, $01, $00, $00
  DB $C5, $FF, $F0, $98, $60, $01, $00, $00
  DB $C5, $FF, $F0, $A0, $80, $01, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  DB $C5, $FE, $7F, $8A, $20, $01, $00, $00
  DB $C5, $FE, $7F, $92, $40, $01, $00, $00
  DB $C5, $FE, $7F, $9A, $60, $01, $00, $00
  DB $C5, $FE, $7F, $A2, $80, $01, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 448
  ADD    EDX, 448
  SUB    ECX, 448
  JNZ    @LOOP
end;

{ 每一次复制 480 个字节 <3840 位> <长度必须是 480 的倍数> }
procedure Move_AVX1_480U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VLDDQU   YMM9,  [EAX+288]
  VLDDQU   YMM10, [EAX+320]
  VLDDQU   YMM11, [EAX+352]
  VLDDQU   YMM12, [EAX+384]
  VLDDQU   YMM13, [EAX+416]
  VLDDQU   YMM14, [EAX+448]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  VMOVDQU  [EDX+288],  YMM9
  VMOVDQU  [EDX+320], YMM10
  VMOVDQU  [EDX+352], YMM11
  VMOVDQU  [EDX+384], YMM12
  VMOVDQU  [EDX+416], YMM13
  VMOVDQU  [EDX+448], YMM14
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0, [EAX+256]
  VLDDQU   YMM1, [EAX+288]
  VLDDQU   YMM2, [EAX+320]
  VLDDQU   YMM3, [EAX+352]
  VLDDQU   YMM4, [EAX+384]
  VLDDQU   YMM5, [EAX+416]
  VLDDQU   YMM6, [EAX+448]
  VMOVDQU  [EDX+256], YMM0
  VMOVDQU  [EDX+288], YMM1
  VMOVDQU  [EDX+320], YMM2
  VMOVDQU  [EDX+352], YMM3
  VMOVDQU  [EDX+384], YMM4
  VMOVDQU  [EDX+416], YMM5
  VMOVDQU  [EDX+448], YMM6
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $7F, $F0, $88, $20, $01, $00, $00
  DB $67, $C5, $7F, $F0, $90, $40, $01, $00, $00
  DB $67, $C5, $7F, $F0, $98, $60, $01, $00, $00
  DB $67, $C5, $7F, $F0, $A0, $80, $01, $00, $00
  DB $67, $C5, $7F, $F0, $A8, $A0, $01, $00, $00
  DB $67, $C5, $7F, $F0, $B0, $C0, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7E, $7F, $8A, $20, $01, $00, $00
  DB $67, $C5, $7E, $7F, $92, $40, $01, $00, $00
  DB $67, $C5, $7E, $7F, $9A, $60, $01, $00, $00
  DB $67, $C5, $7E, $7F, $A2, $80, $01, $00, $00
  DB $67, $C5, $7E, $7F, $AA, $A0, $01, $00, $00
  DB $67, $C5, $7E, $7F, $B2, $C0, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FF, $F0, $88, $20, $01, $00, $00
  DB $C5, $FF, $F0, $90, $40, $01, $00, $00
  DB $C5, $FF, $F0, $98, $60, $01, $00, $00
  DB $C5, $FF, $F0, $A0, $80, $01, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $01, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  DB $C5, $FE, $7F, $8A, $20, $01, $00, $00
  DB $C5, $FE, $7F, $92, $40, $01, $00, $00
  DB $C5, $FE, $7F, $9A, $60, $01, $00, $00
  DB $C5, $FE, $7F, $A2, $80, $01, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $01, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 480
  ADD    EDX, 480
  SUB    ECX, 480
  JNZ    @LOOP
end;

{ 每一次复制 512 个字节 <4096 位> <长度必须是 512 的倍数> }
procedure Move_AVX1_512U(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VLDDQU   YMM8,  [EAX+256]
  VLDDQU   YMM9,  [EAX+288]
  VLDDQU   YMM10, [EAX+320]
  VLDDQU   YMM11, [EAX+352]
  VLDDQU   YMM12, [EAX+384]
  VLDDQU   YMM13, [EAX+416]
  VLDDQU   YMM14, [EAX+448]
  VLDDQU   YMM15, [EAX+480]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VMOVDQU  [EDX+256],  YMM8
  VMOVDQU  [EDX+288],  YMM9
  VMOVDQU  [EDX+320], YMM10
  VMOVDQU  [EDX+352], YMM11
  VMOVDQU  [EDX+384], YMM12
  VMOVDQU  [EDX+416], YMM13
  VMOVDQU  [EDX+448], YMM14
  VMOVDQU  [EDX+480], YMM15
  {$ELSE}
  VLDDQU   YMM0,  [EAX+000]
  VLDDQU   YMM1,  [EAX+032]
  VLDDQU   YMM2,  [EAX+064]
  VLDDQU   YMM3,  [EAX+096]
  VLDDQU   YMM4,  [EAX+128]
  VLDDQU   YMM5,  [EAX+160]
  VLDDQU   YMM6,  [EAX+192]
  VLDDQU   YMM7,  [EAX+224]
  VMOVDQU  [EDX+000],  YMM0
  VMOVDQU  [EDX+032],  YMM1
  VMOVDQU  [EDX+064],  YMM2
  VMOVDQU  [EDX+096],  YMM3
  VMOVDQU  [EDX+128],  YMM4
  VMOVDQU  [EDX+160],  YMM5
  VMOVDQU  [EDX+192],  YMM6
  VMOVDQU  [EDX+224],  YMM7
  VLDDQU   YMM0, [EAX+256]
  VLDDQU   YMM1, [EAX+288]
  VLDDQU   YMM2, [EAX+320]
  VLDDQU   YMM3, [EAX+352]
  VLDDQU   YMM4, [EAX+384]
  VLDDQU   YMM5, [EAX+416]
  VLDDQU   YMM6, [EAX+448]
  VLDDQU   YMM7, [EAX+480]
  VMOVDQU  [EDX+256], YMM0
  VMOVDQU  [EDX+288], YMM1
  VMOVDQU  [EDX+320], YMM2
  VMOVDQU  [EDX+352], YMM3
  VMOVDQU  [EDX+384], YMM4
  VMOVDQU  [EDX+416], YMM5
  VMOVDQU  [EDX+448], YMM6
  VMOVDQU  [EDX+480], YMM7
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FF, $F0, $00
  DB $67, $C5, $FF, $F0, $48, $20
  DB $67, $C5, $FF, $F0, $50, $40
  DB $67, $C5, $FF, $F0, $58, $60
  DB $67, $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $67, $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7F, $F0, $80, $00, $01, $00, $00
  DB $67, $C5, $7F, $F0, $88, $20, $01, $00, $00
  DB $67, $C5, $7F, $F0, $90, $40, $01, $00, $00
  DB $67, $C5, $7F, $F0, $98, $60, $01, $00, $00
  DB $67, $C5, $7F, $F0, $A0, $80, $01, $00, $00
  DB $67, $C5, $7F, $F0, $A8, $A0, $01, $00, $00
  DB $67, $C5, $7F, $F0, $B0, $C0, $01, $00, $00
  DB $67, $C5, $7F, $F0, $B8, $E0, $01, $00, $00
  DB $67, $C5, $FE, $7F, $02
  DB $67, $C5, $FE, $7F, $4A, $20
  DB $67, $C5, $FE, $7F, $52, $40
  DB $67, $C5, $FE, $7F, $5A, $60
  DB $67, $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7E, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7E, $7F, $8A, $20, $01, $00, $00
  DB $67, $C5, $7E, $7F, $92, $40, $01, $00, $00
  DB $67, $C5, $7E, $7F, $9A, $60, $01, $00, $00
  DB $67, $C5, $7E, $7F, $A2, $80, $01, $00, $00
  DB $67, $C5, $7E, $7F, $AA, $A0, $01, $00, $00
  DB $67, $C5, $7E, $7F, $B2, $C0, $01, $00, $00
  DB $67, $C5, $7E, $7F, $BA, $E0, $01, $00, $00
  {$ELSE}
  DB $C5, $FF, $F0, $00
  DB $C5, $FF, $F0, $48, $20
  DB $C5, $FF, $F0, $50, $40
  DB $C5, $FF, $F0, $58, $60
  DB $C5, $FF, $F0, $A0, $80, $00, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $00, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $00, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $00, $00, $00
  DB $C5, $FE, $7F, $02
  DB $C5, $FE, $7F, $4A, $20
  DB $C5, $FE, $7F, $52, $40
  DB $C5, $FE, $7F, $5A, $60
  DB $C5, $FE, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $FF, $F0, $80, $00, $01, $00, $00
  DB $C5, $FF, $F0, $88, $20, $01, $00, $00
  DB $C5, $FF, $F0, $90, $40, $01, $00, $00
  DB $C5, $FF, $F0, $98, $60, $01, $00, $00
  DB $C5, $FF, $F0, $A0, $80, $01, $00, $00
  DB $C5, $FF, $F0, $A8, $A0, $01, $00, $00
  DB $C5, $FF, $F0, $B0, $C0, $01, $00, $00
  DB $C5, $FF, $F0, $B8, $E0, $01, $00, $00
  DB $C5, $FE, $7F, $82, $00, $01, $00, $00
  DB $C5, $FE, $7F, $8A, $20, $01, $00, $00
  DB $C5, $FE, $7F, $92, $40, $01, $00, $00
  DB $C5, $FE, $7F, $9A, $60, $01, $00, $00
  DB $C5, $FE, $7F, $A2, $80, $01, $00, $00
  DB $C5, $FE, $7F, $AA, $A0, $01, $00, $00
  DB $C5, $FE, $7F, $B2, $C0, $01, $00, $00
  DB $C5, $FE, $7F, $BA, $E0, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 512
  ADD    EDX, 512
  SUB    ECX, 512
  JNZ    @LOOP
end;

{ ---------------------------------------------------------------------------------------------------------------------------------------- }
{ --------------------------------------------------------- 数据的内存地址，按 16 字节对齐 ----------------------------------------------- }
{ ---------------------------------------------------------------------------------------------------------------------------------------- }

{ 每一次复制 16 个字节 <128 位> <长度必须是 16 的倍数> }
procedure Move_SSE2_016A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVDQA  XMM0, [EAX]
  MOVDQA [EDX],  XMM0

  ADD    EAX, 16
  ADD    EDX, 16
  SUB    ECX, 16
  JNZ    @LOOP
end;

{ 每一次复制 32 个字节 <256 位> <长度必须是 32 的倍数> }
procedure Move_SSE2_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVDQA  XMM0, [EAX+00]
  MOVDQA  XMM1, [EAX+16]
  MOVDQA  [EDX+00], XMM0
  MOVDQA  [EDX+16], XMM1

  ADD    EAX, 32
  ADD    EDX, 32
  SUB    ECX, 32
  JNZ    @LOOP
end;

{ 每一次复制 64 个字节 <512 位> <长度必须是 64 的倍数> }
procedure Move_SSE2_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVDQA   XMM0, [EAX+00]
  MOVDQA   XMM1, [EAX+16]
  MOVDQA   XMM2, [EAX+32]
  MOVDQA   XMM3, [EAX+48]
  MOVDQA  [EDX+00],  XMM0
  MOVDQA  [EDX+16],  XMM1
  MOVDQA  [EDX+32],  XMM2
  MOVDQA  [EDX+48],  XMM3

  ADD    EAX, 64
  ADD    EDX, 64
  SUB    ECX, 64
  JNZ    @LOOP
end;

{ 每一次复制 128 个字节 <1024 位> <长度必须是 128 的倍数> }
procedure Move_SSE2_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVDQA   XMM0, [EAX+000]
  MOVDQA   XMM1, [EAX+016]
  MOVDQA   XMM2, [EAX+032]
  MOVDQA   XMM3, [EAX+048]
  MOVDQA   XMM4, [EAX+064]
  MOVDQA   XMM5, [EAX+080]
  MOVDQA   XMM6, [EAX+096]
  MOVDQA   XMM7, [EAX+112]
  MOVDQA  [EDX+000],  XMM0
  MOVDQA  [EDX+016],  XMM1
  MOVDQA  [EDX+032],  XMM2
  MOVDQA  [EDX+048],  XMM3
  MOVDQA  [EDX+064],  XMM4
  MOVDQA  [EDX+080],  XMM5
  MOVDQA  [EDX+096],  XMM6
  MOVDQA  [EDX+112],  XMM7

  ADD    EAX, 128
  ADD    EDX, 128
  SUB    ECX, 128
  JNZ    @LOOP
end;

{ 每一次复制 256 个字节 <2048 位> <长度必须是 256 的倍数> }
procedure Move_SSE2_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  MOVDQA   XMM0,  [EAX+000]
  MOVDQA   XMM1,  [EAX+016]
  MOVDQA   XMM2,  [EAX+032]
  MOVDQA   XMM3,  [EAX+048]
  MOVDQA   XMM4,  [EAX+064]
  MOVDQA   XMM5,  [EAX+080]
  MOVDQA   XMM6,  [EAX+096]
  MOVDQA   XMM7,  [EAX+112]
  MOVDQA   XMM8,  [EAX+128]
  MOVDQA   XMM9,  [EAX+144]
  MOVDQA   XMM10, [EAX+160]
  MOVDQA   XMM11, [EAX+176]
  MOVDQA   XMM12, [EAX+192]
  MOVDQA   XMM13, [EAX+208]
  MOVDQA   XMM14, [EAX+224]
  MOVDQA   XMM15, [EAX+240]

  MOVDQA  [EDX+000],  XMM0
  MOVDQA  [EDX+016],  XMM1
  MOVDQA  [EDX+032],  XMM2
  MOVDQA  [EDX+048],  XMM3
  MOVDQA  [EDX+064],  XMM4
  MOVDQA  [EDX+080],  XMM5
  MOVDQA  [EDX+096],  XMM6
  MOVDQA  [EDX+112],  XMM7
  MOVDQA  [EDX+128],  XMM8
  MOVDQA  [EDX+144],  XMM9
  MOVDQA  [EDX+160],  XMM10
  MOVDQA  [EDX+176],  XMM11
  MOVDQA  [EDX+192],  XMM12
  MOVDQA  [EDX+208],  XMM13
  MOVDQA  [EDX+224],  XMM14
  MOVDQA  [EDX+240],  XMM15
  {$ELSE}
@LOOP:
  MOVDQA   XMM0,  [EAX+000]
  MOVDQA   XMM1,  [EAX+016]
  MOVDQA   XMM2,  [EAX+032]
  MOVDQA   XMM3,  [EAX+048]
  MOVDQA   XMM4,  [EAX+064]
  MOVDQA   XMM5,  [EAX+080]
  MOVDQA   XMM6,  [EAX+096]
  MOVDQA   XMM7,  [EAX+112]
  MOVDQA   [EDX+000],  XMM0
  MOVDQA   [EDX+016],  XMM1
  MOVDQA   [EDX+032],  XMM2
  MOVDQA   [EDX+048],  XMM3
  MOVDQA   [EDX+064],  XMM4
  MOVDQA   [EDX+080],  XMM5
  MOVDQA   [EDX+096],  XMM6
  MOVDQA   [EDX+112],  XMM7

  MOVDQA   XMM0,  [EAX+128]
  MOVDQA   XMM1,  [EAX+144]
  MOVDQA   XMM2,  [EAX+160]
  MOVDQA   XMM3,  [EAX+176]
  MOVDQA   XMM4,  [EAX+192]
  MOVDQA   XMM5,  [EAX+208]
  MOVDQA   XMM6,  [EAX+224]
  MOVDQA   XMM7,  [EAX+240]
  MOVDQA   [EDX+128],  XMM0
  MOVDQA   [EDX+144],  XMM1
  MOVDQA   [EDX+160],  XMM2
  MOVDQA   [EDX+176],  XMM3
  MOVDQA   [EDX+192],  XMM4
  MOVDQA   [EDX+208],  XMM5
  MOVDQA   [EDX+224],  XMM6
  MOVDQA   [EDX+240],  XMM7
  {$ENDIF}

  ADD    EAX, 256
  ADD    EDX, 256
  SUB    ECX, 256
  JNZ    @LOOP
end;

{ 每一次复制 16 个字节 <128 位> <长度必须是 16 的倍数> }
procedure Move_SSE4_016A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVNTDQA  XMM0, [EAX]
  MOVNTDQ  [EDX],  XMM0

  ADD    EAX, 16
  ADD    EDX, 16
  SUB    ECX, 16
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 32 个字节 <256 位> <长度必须是 32 的倍数> }
procedure Move_SSE4_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVNTDQA  XMM0, [EAX + 00]
  MOVNTDQA  XMM1, [EAX + 16]
  MOVNTDQ  [EDX + 00],  XMM0
  MOVNTDQ  [EDX + 16],  XMM1

  ADD    EAX, 32
  ADD    EDX, 32
  SUB    ECX, 32
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 64 个字节 <512 位> <长度必须是 64 的倍数> }
procedure Move_SSE4_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVNTDQA  XMM0, [EAX + 00]
  MOVNTDQA  XMM1, [EAX + 16]
  MOVNTDQA  XMM2, [EAX + 32]
  MOVNTDQA  XMM3, [EAX + 48]
  MOVNTDQ  [EDX + 00],  XMM0
  MOVNTDQ  [EDX + 16],  XMM1
  MOVNTDQ  [EDX + 32],  XMM2
  MOVNTDQ  [EDX + 48],  XMM3

  ADD    EAX, 64
  ADD    EDX, 64
  SUB    ECX, 64
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 128 个字节 <1024 位> <长度必须是 128 的倍数> }
procedure Move_SSE4_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  MOVNTDQA  XMM0, [EAX + 00]
  MOVNTDQA  XMM1, [EAX + 16]
  MOVNTDQA  XMM2, [EAX + 32]
  MOVNTDQA  XMM3, [EAX + 48]
  MOVNTDQA  XMM4, [EAX + 64]
  MOVNTDQA  XMM5, [EAX + 80]
  MOVNTDQA  XMM6, [EAX + 96]
  MOVNTDQA  XMM7, [EAX +112]
  MOVNTDQ  [EDX + 00],  XMM0
  MOVNTDQ  [EDX + 16],  XMM1
  MOVNTDQ  [EDX + 32],  XMM2
  MOVNTDQ  [EDX + 48],  XMM3
  MOVNTDQ  [EDX + 64],  XMM4
  MOVNTDQ  [EDX + 80],  XMM5
  MOVNTDQ  [EDX + 96],  XMM6
  MOVNTDQ  [EDX +112],  XMM7

  ADD    EAX, 128
  ADD    EDX, 128
  SUB    ECX, 128
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 256 个字节 <1024 位> <长度必须是 256 的倍数> }
procedure Move_SSE4_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
@LOOP:
  MOVNTDQA  XMM0,  [EAX + 00]
  MOVNTDQA  XMM1,  [EAX + 16]
  MOVNTDQA  XMM2,  [EAX + 32]
  MOVNTDQA  XMM3,  [EAX + 48]
  MOVNTDQA  XMM4,  [EAX + 64]
  MOVNTDQA  XMM5,  [EAX + 80]
  MOVNTDQA  XMM6,  [EAX + 96]
  MOVNTDQA  XMM7,  [EAX +112]
  MOVNTDQA  XMM8,  [EAX +128]
  MOVNTDQA  XMM9,  [EAX +144]
  MOVNTDQA  XMM10, [EAX +160]
  MOVNTDQA  XMM11, [EAX +176]
  MOVNTDQA  XMM12, [EAX +192]
  MOVNTDQA  XMM13, [EAX +208]
  MOVNTDQA  XMM14, [EAX +224]
  MOVNTDQA  XMM15, [EAX +240]
  MOVNTDQ   [EDX + 00],  XMM0
  MOVNTDQ   [EDX + 16],  XMM1
  MOVNTDQ   [EDX + 32],  XMM2
  MOVNTDQ   [EDX + 48],  XMM3
  MOVNTDQ   [EDX + 64],  XMM4
  MOVNTDQ   [EDX + 80],  XMM5
  MOVNTDQ   [EDX + 96],  XMM6
  MOVNTDQ   [EDX +112],  XMM7
  MOVNTDQ   [EDX +128],  XMM8
  MOVNTDQ   [EDX +144],  XMM9
  MOVNTDQ   [EDX +160],  XMM10
  MOVNTDQ   [EDX +176],  XMM11
  MOVNTDQ   [EDX +192],  XMM12
  MOVNTDQ   [EDX +208],  XMM13
  MOVNTDQ   [EDX +224],  XMM14
  MOVNTDQ   [EDX +240],  XMM15
  {$ELSE}
@LOOP:
  MOVNTDQA  XMM0,  [EAX + 00]
  MOVNTDQA  XMM1,  [EAX + 16]
  MOVNTDQA  XMM2,  [EAX + 32]
  MOVNTDQA  XMM3,  [EAX + 48]
  MOVNTDQA  XMM4,  [EAX + 64]
  MOVNTDQA  XMM5,  [EAX + 80]
  MOVNTDQA  XMM6,  [EAX + 96]
  MOVNTDQA  XMM7,  [EAX +112]
  MOVNTDQ   [EDX + 00],  XMM0
  MOVNTDQ   [EDX + 16],  XMM1
  MOVNTDQ   [EDX + 32],  XMM2
  MOVNTDQ   [EDX + 48],  XMM3
  MOVNTDQ   [EDX + 64],  XMM4
  MOVNTDQ   [EDX + 80],  XMM5
  MOVNTDQ   [EDX + 96],  XMM6
  MOVNTDQ   [EDX +112],  XMM7
  MOVNTDQA  XMM0,  [EAX +128]
  MOVNTDQA  XMM1,  [EAX +144]
  MOVNTDQA  XMM2,  [EAX +160]
  MOVNTDQA  XMM3,  [EAX +176]
  MOVNTDQA  XMM4,  [EAX +192]
  MOVNTDQA  XMM5,  [EAX +208]
  MOVNTDQA  XMM6,  [EAX +224]
  MOVNTDQA  XMM7,  [EAX +240]
  MOVNTDQ   [EDX +128],  XMM0
  MOVNTDQ   [EDX +144],  XMM1
  MOVNTDQ   [EDX +160],  XMM2
  MOVNTDQ   [EDX +176],  XMM3
  MOVNTDQ   [EDX +192],  XMM4
  MOVNTDQ   [EDX +208],  XMM5
  MOVNTDQ   [EDX +224],  XMM6
  MOVNTDQ   [EDX +240],  XMM7
  {$ENDIF}

  ADD    EAX, 256
  ADD    EDX, 256
  SUB    ECX, 256
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 32 个字节 <256 位> <长度必须是 32 的倍数> }
procedure Move_AVX1_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVDQA  YMM0, [EAX]
  VMOVDQA  [EDX], YMM0
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FD, $6F, $00
  DB $67, $C5, $FD, $7F, $02
  {$ELSE}
  DB $C5, $FD, $6F, $00
  DB $C5, $FD, $7F, $02
  {$ENDIF}

  ADD    EAX, 32
  ADD    EDX, 32
  SUB    ECX, 32
  JNZ    @LOOP
end;

{ 每一次复制 64 个字节 <512 位> <长度必须是 64 的倍数> }
procedure Move_AVX1_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVDQA   YMM0, [EAX + 00]
  VMOVDQA   YMM1, [EAX + 32]
  VMOVDQA   [EDX + 00], YMM0
  VMOVDQA   [EDX + 32], YMM1
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FD, $6F, $00
  DB $67, $C5, $FD, $6F, $48, $20
  DB $67, $C5, $FD, $7F, $02
  DB $67, $C5, $FD, $7F, $4A, $20
  {$ELSE}
  DB $C5, $FD, $6F, $00
  DB $C5, $FD, $6F, $48, $20
  DB $C5, $FD, $7F, $02
  DB $C5, $FD, $7F, $4A, $20
  {$ENDIF}

  ADD    EAX, 64
  ADD    EDX, 64
  SUB    ECX, 64
  JNZ    @LOOP
end;

{ 每一次复制 128 个字节 <1024 位> <长度必须是 128 的倍数> }
procedure Move_AVX1_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVDQA   YMM0, [EAX + 00]
  VMOVDQA   YMM1, [EAX + 32]
  VMOVDQA   YMM2, [EAX + 64]
  VMOVDQA   YMM3, [EAX + 96]
  VMOVDQA   [EDX + 00], YMM0
  VMOVDQA   [EDX + 32], YMM1
  VMOVDQA   [EDX + 64], YMM2
  VMOVDQA   [EDX + 96], YMM3
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FD, $6F, $00
  DB $67, $C5, $FD, $6F, $48, $20
  DB $67, $C5, $FD, $6F, $50, $40
  DB $67, $C5, $FD, $6F, $58, $60
  DB $67, $C5, $FD, $7F, $02
  DB $67, $C5, $FD, $7F, $4A, $20
  DB $67, $C5, $FD, $7F, $52, $40
  DB $67, $C5, $FD, $7F, $5A, $60
  {$ELSE}
  DB $C5, $FD, $6F, $00
  DB $C5, $FD, $6F, $48, $20
  DB $C5, $FD, $6F, $50, $40
  DB $C5, $FD, $6F, $58, $60
  DB $C5, $FD, $7F, $02
  DB $C5, $FD, $7F, $4A, $20
  DB $C5, $FD, $7F, $52, $40
  DB $C5, $FD, $7F, $5A, $60
  {$ENDIF}

  ADD    EAX, 128
  ADD    EDX, 128
  SUB    ECX, 128
  JNZ    @LOOP
end;

{ 每一次复制 256 个字节 <2048 位> <长度必须是 256 的倍数> }
procedure Move_AVX1_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVDQA   YMM0, [EAX+000]
  VMOVDQA   YMM1, [EAX+032]
  VMOVDQA   YMM2, [EAX+064]
  VMOVDQA   YMM3, [EAX+096]
  VMOVDQA   YMM4, [EAX+128]
  VMOVDQA   YMM5, [EAX+160]
  VMOVDQA   YMM6, [EAX+192]
  VMOVDQA   YMM7, [EAX+224]
  VMOVDQA   [EDX+000], YMM0
  VMOVDQA   [EDX+032], YMM1
  VMOVDQA   [EDX+064], YMM2
  VMOVDQA   [EDX+096], YMM3
  VMOVDQA   [EDX+128], YMM4
  VMOVDQA   [EDX+160], YMM5
  VMOVDQA   [EDX+192], YMM6
  VMOVDQA   [EDX+224], YMM7
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FD, $6F, $00
  DB $67, $C5, $FD, $6F, $48, $20
  DB $67, $C5, $FD, $6F, $50, $40
  DB $67, $C5, $FD, $6F, $58, $60
  DB $67, $C5, $FD, $6F, $A0, $80, $00, $00, $00
  DB $67, $C5, $FD, $6F, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FD, $6F, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FD, $6F, $B8, $E0, $00, $00, $00
  DB $67, $C5, $FD, $7F, $02
  DB $67, $C5, $FD, $7F, $4A, $20
  DB $67, $C5, $FD, $7F, $52, $40
  DB $67, $C5, $FD, $7F, $5A, $60
  DB $67, $C5, $FD, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FD, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FD, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FD, $7F, $BA, $E0, $00, $00, $00
  {$ELSE}
  DB $C5, $FD, $6F, $00
  DB $C5, $FD, $6F, $48, $20
  DB $C5, $FD, $6F, $50, $40
  DB $C5, $FD, $6F, $58, $60
  DB $C5, $FD, $6F, $A0, $80, $00, $00, $00
  DB $C5, $FD, $6F, $A8, $A0, $00, $00, $00
  DB $C5, $FD, $6F, $B0, $C0, $00, $00, $00
  DB $C5, $FD, $6F, $B8, $E0, $00, $00, $00
  DB $C5, $FD, $7F, $02
  DB $C5, $FD, $7F, $4A, $20
  DB $C5, $FD, $7F, $52, $40
  DB $C5, $FD, $7F, $5A, $60
  DB $C5, $FD, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FD, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FD, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FD, $7F, $BA, $E0, $00, $00, $00
  {$ENDIF}

  ADD    EAX, 256
  ADD    EDX, 256
  SUB    ECX, 256
  JNZ    @LOOP
end;

{ 每一次复制 512 个字节 <4096 位> <长度必须是 512 的倍数> }
procedure Move_AVX1_512A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VMOVDQA   YMM0,  [EAX+000]
  VMOVDQA   YMM1,  [EAX+032]
  VMOVDQA   YMM2,  [EAX+064]
  VMOVDQA   YMM3,  [EAX+096]
  VMOVDQA   YMM4,  [EAX+128]
  VMOVDQA   YMM5,  [EAX+160]
  VMOVDQA   YMM6,  [EAX+192]
  VMOVDQA   YMM7,  [EAX+224]
  VMOVDQA   YMM8,  [EAX+256]
  VMOVDQA   YMM9,  [EAX+288]
  VMOVDQA   YMM10, [EAX+320]
  VMOVDQA   YMM11, [EAX+352]
  VMOVDQA   YMM12, [EAX+384]
  VMOVDQA   YMM13, [EAX+416]
  VMOVDQA   YMM14, [EAX+448]
  VMOVDQA   YMM15, [EAX+480]
  VMOVDQA   [EDX+000],  YMM0
  VMOVDQA   [EDX+032],  YMM1
  VMOVDQA   [EDX+064],  YMM2
  VMOVDQA   [EDX+096],  YMM3
  VMOVDQA   [EDX+128],  YMM4
  VMOVDQA   [EDX+160],  YMM5
  VMOVDQA   [EDX+192],  YMM6
  VMOVDQA   [EDX+224],  YMM7
  VMOVDQA   [EDX+256],  YMM8
  VMOVDQA   [EDX+288],  YMM9
  VMOVDQA   [EDX+320],  YMM10
  VMOVDQA   [EDX+352],  YMM11
  VMOVDQA   [EDX+384],  YMM12
  VMOVDQA   [EDX+416],  YMM13
  VMOVDQA   [EDX+448],  YMM14
  VMOVDQA   [EDX+480],  YMM15
  {$ELSE}
  VMOVDQA   YMM0,  [EAX+000]
  VMOVDQA   YMM1,  [EAX+032]
  VMOVDQA   YMM2,  [EAX+064]
  VMOVDQA   YMM3,  [EAX+096]
  VMOVDQA   YMM4,  [EAX+128]
  VMOVDQA   YMM5,  [EAX+160]
  VMOVDQA   YMM6,  [EAX+192]
  VMOVDQA   YMM7,  [EAX+224]
  VMOVDQA   [EDX+000],  YMM0
  VMOVDQA   [EDX+032],  YMM1
  VMOVDQA   [EDX+064],  YMM2
  VMOVDQA   [EDX+096],  YMM3
  VMOVDQA   [EDX+128],  YMM4
  VMOVDQA   [EDX+160],  YMM5
  VMOVDQA   [EDX+192],  YMM6
  VMOVDQA   [EDX+224],  YMM7
  VMOVDQA   YMM0,  [EAX+256]
  VMOVDQA   YMM1,  [EAX+288]
  VMOVDQA   YMM2,  [EAX+320]
  VMOVDQA   YMM3,  [EAX+352]
  VMOVDQA   YMM4,  [EAX+384]
  VMOVDQA   YMM5,  [EAX+416]
  VMOVDQA   YMM6,  [EAX+448]
  VMOVDQA   YMM7,  [EAX+480]
  VMOVDQA   [EDX+256],  YMM0
  VMOVDQA   [EDX+288],  YMM1
  VMOVDQA   [EDX+320],  YMM2
  VMOVDQA   [EDX+352],  YMM3
  VMOVDQA   [EDX+384],  YMM4
  VMOVDQA   [EDX+416],  YMM5
  VMOVDQA   [EDX+448],  YMM6
  VMOVDQA   [EDX+480],  YMM7
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C5, $FD, $6F, $00
  DB $67, $C5, $FD, $6F, $48, $20
  DB $67, $C5, $FD, $6F, $50, $40
  DB $67, $C5, $FD, $6F, $58, $60
  DB $67, $C5, $FD, $6F, $A0, $80, $00, $00, $00
  DB $67, $C5, $FD, $6F, $A8, $A0, $00, $00, $00
  DB $67, $C5, $FD, $6F, $B0, $C0, $00, $00, $00
  DB $67, $C5, $FD, $6F, $B8, $E0, $00, $00, $00
  DB $67, $C5, $7D, $6F, $80, $00, $01, $00, $00
  DB $67, $C5, $7D, $6F, $88, $20, $01, $00, $00
  DB $67, $C5, $7D, $6F, $90, $40, $01, $00, $00
  DB $67, $C5, $7D, $6F, $98, $60, $01, $00, $00
  DB $67, $C5, $7D, $6F, $A0, $80, $01, $00, $00
  DB $67, $C5, $7D, $6F, $A8, $A0, $01, $00, $00
  DB $67, $C5, $7D, $6F, $B0, $C0, $01, $00, $00
  DB $67, $C5, $7D, $6F, $B8, $E0, $01, $00, $00
  DB $67, $C5, $FD, $7F, $02
  DB $67, $C5, $FD, $7F, $4A, $20
  DB $67, $C5, $FD, $7F, $52, $40
  DB $67, $C5, $FD, $7F, $5A, $60
  DB $67, $C5, $FD, $7F, $A2, $80, $00, $00, $00
  DB $67, $C5, $FD, $7F, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FD, $7F, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FD, $7F, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7D, $7F, $82, $00, $01, $00, $00
  DB $67, $C5, $7D, $7F, $8A, $20, $01, $00, $00
  DB $67, $C5, $7D, $7F, $92, $40, $01, $00, $00
  DB $67, $C5, $7D, $7F, $9A, $60, $01, $00, $00
  DB $67, $C5, $7D, $7F, $A2, $80, $01, $00, $00
  DB $67, $C5, $7D, $7F, $AA, $A0, $01, $00, $00
  DB $67, $C5, $7D, $7F, $B2, $C0, $01, $00, $00
  DB $67, $C5, $7D, $7F, $BA, $E0, $01, $00, $00
  {$ELSE}
  DB $C5, $FD, $6F, $00
  DB $C5, $FD, $6F, $48, $20
  DB $C5, $FD, $6F, $50, $40
  DB $C5, $FD, $6F, $58, $60
  DB $C5, $FD, $6F, $A0, $80, $00, $00, $00
  DB $C5, $FD, $6F, $A8, $A0, $00, $00, $00
  DB $C5, $FD, $6F, $B0, $C0, $00, $00, $00
  DB $C5, $FD, $6F, $B8, $E0, $00, $00, $00
  DB $C5, $7D, $6F, $80, $00, $01, $00, $00
  DB $C5, $7D, $6F, $88, $20, $01, $00, $00
  DB $C5, $7D, $6F, $90, $40, $01, $00, $00
  DB $C5, $7D, $6F, $98, $60, $01, $00, $00
  DB $C5, $7D, $6F, $A0, $80, $01, $00, $00
  DB $C5, $7D, $6F, $A8, $A0, $01, $00, $00
  DB $C5, $7D, $6F, $B0, $C0, $01, $00, $00
  DB $C5, $7D, $6F, $B8, $E0, $01, $00, $00
  DB $C5, $FD, $7F, $02
  DB $C5, $FD, $7F, $4A, $20
  DB $C5, $FD, $7F, $52, $40
  DB $C5, $FD, $7F, $5A, $60
  DB $C5, $FD, $7F, $A2, $80, $00, $00, $00
  DB $C5, $FD, $7F, $AA, $A0, $00, $00, $00
  DB $C5, $FD, $7F, $B2, $C0, $00, $00, $00
  DB $C5, $FD, $7F, $BA, $E0, $00, $00, $00
  DB $C5, $7D, $7F, $82, $00, $01, $00, $00
  DB $C5, $7D, $7F, $8A, $20, $01, $00, $00
  DB $C5, $7D, $7F, $92, $40, $01, $00, $00
  DB $C5, $7D, $7F, $9A, $60, $01, $00, $00
  DB $C5, $7D, $7F, $A2, $80, $01, $00, $00
  DB $C5, $7D, $7F, $AA, $A0, $01, $00, $00
  DB $C5, $7D, $7F, $B2, $C0, $01, $00, $00
  DB $C5, $7D, $7F, $BA, $E0, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 512
  ADD    EDX, 512
  SUB    ECX, 512
  JNZ    @LOOP
end;

{ 每一次复制 32 个字节 <256 位> <长度必须是 32 的倍数> }
procedure Move_AVX2_032A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVNTDQA  YMM0, [EAX]
  VMOVNTDQ   [EDX], YMM0
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C4, $E2, $7D, $2A, $00
  DB $67, $C5, $FD, $E7, $02
  {$ELSE}
  DB $C4, $E2, $7D, $2A, $00
  DB $C5, $FD, $E7, $02
  {$ENDIF}

  ADD    EAX, 32
  ADD    EDX, 32
  SUB    ECX, 32
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 64 个字节 <512 位> <长度必须是 64 的倍数> }
procedure Move_AVX2_064A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVNTDQA  YMM0, [EAX+00]
  VMOVNTDQA  YMM1, [EAX+32]
  VMOVNTDQ   [EDX+00], YMM0
  VMOVNTDQ   [EDX+32], YMM1
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C4, $E2, $7D, $2A, $00
  DB $67, $C4, $E2, $7D, $2A, $48, $20
  DB $67, $C5, $FD, $E7, $02
  DB $67, $C5, $FD, $E7, $4A, $20
  {$ELSE}
  DB $C4, $E2, $7D, $2A, $00
  DB $C4, $E2, $7D, $2A, $48, $20
  DB $C5, $FD, $E7, $02
  DB $C5, $FD, $E7, $4A, $20
  {$ENDIF}

  ADD    EAX, 64
  ADD    EDX, 64
  SUB    ECX, 64
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 128 个字节 <512 位> <长度必须是 128 的倍数> }
procedure Move_AVX2_128A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVNTDQA  YMM0, [EAX+00]
  VMOVNTDQA  YMM1, [EAX+32]
  VMOVNTDQA  YMM2, [EAX+64]
  VMOVNTDQA  YMM3, [EAX+96]
  VMOVNTDQ   [EDX+00], YMM0
  VMOVNTDQ   [EDX+32], YMM1
  VMOVNTDQ   [EDX+64], YMM2
  VMOVNTDQ   [EDX+96], YMM3
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C4, $E2, $7D, $2A, $00
  DB $67, $C4, $E2, $7D, $2A, $48, $20
  DB $67, $C4, $E2, $7D, $2A, $50, $40
  DB $67, $C4, $E2, $7D, $2A, $58, $60
  DB $67, $C5, $FD, $E7, $02
  DB $67, $C5, $FD, $E7, $4A, $20
  DB $67, $C5, $FD, $E7, $52, $40
  DB $67, $C5, $FD, $E7, $5A, $60
  {$ELSE}
  DB $C4, $E2, $7D, $2A, $00
  DB $C4, $E2, $7D, $2A, $48, $20
  DB $C4, $E2, $7D, $2A, $50, $40
  DB $C4, $E2, $7D, $2A, $58, $60
  DB $C5, $FD, $E7, $02
  DB $C5, $FD, $E7, $4A, $20
  DB $C5, $FD, $E7, $52, $40
  DB $C5, $FD, $E7, $5A, $60
  {$ENDIF}

  ADD    EAX, 128
  ADD    EDX, 128
  SUB    ECX, 128
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 256 个字节 <1024 位> <长度必须是 256 的倍数> }
procedure Move_AVX2_256A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  VMOVNTDQA  YMM0, [EAX+00]
  VMOVNTDQA  YMM1, [EAX+32]
  VMOVNTDQA  YMM2, [EAX+64]
  VMOVNTDQA  YMM3, [EAX+96]
  VMOVNTDQA  YMM4, [EAX+128]
  VMOVNTDQA  YMM5, [EAX+160]
  VMOVNTDQA  YMM6, [EAX+192]
  VMOVNTDQA  YMM7, [EAX+224]
  VMOVNTDQ   [EDX+00],  YMM0
  VMOVNTDQ   [EDX+32],  YMM1
  VMOVNTDQ   [EDX+64],  YMM2
  VMOVNTDQ   [EDX+96],  YMM3
  VMOVNTDQ   [EDX+128], YMM4
  VMOVNTDQ   [EDX+160], YMM5
  VMOVNTDQ   [EDX+192], YMM6
  VMOVNTDQ   [EDX+224], YMM7
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C4, $E2, $7D, $2A, $00
  DB $67, $C4, $E2, $7D, $2A, $48, $20
  DB $67, $C4, $E2, $7D, $2A, $50, $40
  DB $67, $C4, $E2, $7D, $2A, $58, $60
  DB $67, $C4, $E2, $7D, $2A, $A0, $80, $00, $00, $00
  DB $67, $C4, $E2, $7D, $2A, $A8, $A0, $00, $00, $00
  DB $67, $C4, $E2, $7D, $2A, $B0, $C0, $00, $00, $00
  DB $67, $C4, $E2, $7D, $2A, $B8, $E0, $00, $00, $00
  DB $67, $C5, $FD, $E7, $02
  DB $67, $C5, $FD, $E7, $4A, $20
  DB $67, $C5, $FD, $E7, $52, $40
  DB $67, $C5, $FD, $E7, $5A, $60
  DB $67, $C5, $FD, $E7, $A2, $80, $00, $00, $00
  DB $67, $C5, $FD, $E7, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FD, $E7, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FD, $E7, $BA, $E0, $00, $00, $00
  {$ELSE}
  DB $C4, $E2, $7D, $2A, $00
  DB $C4, $E2, $7D, $2A, $48, $20
  DB $C4, $E2, $7D, $2A, $50, $40
  DB $C4, $E2, $7D, $2A, $58, $60
  DB $C4, $E2, $7D, $2A, $A0, $80, $00, $00, $00
  DB $C4, $E2, $7D, $2A, $A8, $A0, $00, $00, $00
  DB $C4, $E2, $7D, $2A, $B0, $C0, $00, $00, $00
  DB $C4, $E2, $7D, $2A, $B8, $E0, $00, $00, $00
  DB $C5, $FD, $E7, $02
  DB $C5, $FD, $E7, $4A, $20
  DB $C5, $FD, $E7, $52, $40
  DB $C5, $FD, $E7, $5A, $60
  DB $C5, $FD, $E7, $A2, $80, $00, $00, $00
  DB $C5, $FD, $E7, $AA, $A0, $00, $00, $00
  DB $C5, $FD, $E7, $B2, $C0, $00, $00, $00
  DB $C5, $FD, $E7, $BA, $E0, $00, $00, $00
  {$ENDIF}

  ADD    EAX, 256
  ADD    EDX, 256
  SUB    ECX, 256
  JNZ    @LOOP
  SFENCE
end;

{ 每一次复制 512 个字节 <1024 位> <长度必须是 512 的倍数> }
procedure Move_AVX2_512A(const src: Pointer; dst: Pointer; const len: NativeInt); assembler; register;
asm
  {$IFDEF WIN64}
  XCHG  RAX, RCX
  {$ENDIF}
@LOOP:
  {$IFDEF FPC}
  {$IFDEF WIN64}
  VMOVNTDQA  YMM0,  [EAX+000]
  VMOVNTDQA  YMM1,  [EAX+032]
  VMOVNTDQA  YMM2,  [EAX+064]
  VMOVNTDQA  YMM3,  [EAX+096]
  VMOVNTDQA  YMM4,  [EAX+128]
  VMOVNTDQA  YMM5,  [EAX+160]
  VMOVNTDQA  YMM6,  [EAX+192]
  VMOVNTDQA  YMM7,  [EAX+224]
  VMOVNTDQA  YMM8,  [EAX+256]
  VMOVNTDQA  YMM9,  [EAX+288]
  VMOVNTDQA  YMM10, [EAX+320]
  VMOVNTDQA  YMM11, [EAX+352]
  VMOVNTDQA  YMM12, [EAX+384]
  VMOVNTDQA  YMM13, [EAX+416]
  VMOVNTDQA  YMM14, [EAX+448]
  VMOVNTDQA  YMM15, [EAX+480]
  VMOVNTDQ   [EDX+000],  YMM0
  VMOVNTDQ   [EDX+032],  YMM1
  VMOVNTDQ   [EDX+064],  YMM2
  VMOVNTDQ   [EDX+096],  YMM3
  VMOVNTDQ   [EDX+128],  YMM4
  VMOVNTDQ   [EDX+160],  YMM5
  VMOVNTDQ   [EDX+192],  YMM6
  VMOVNTDQ   [EDX+224],  YMM7
  VMOVNTDQ   [EDX+256],  YMM8
  VMOVNTDQ   [EDX+288],  YMM9
  VMOVNTDQ   [EDX+320],  YMM10
  VMOVNTDQ   [EDX+352],  YMM11
  VMOVNTDQ   [EDX+384],  YMM12
  VMOVNTDQ   [EDX+416],  YMM13
  VMOVNTDQ   [EDX+448],  YMM14
  VMOVNTDQ   [EDX+480],  YMM15
  {$ELSE}
  VMOVNTDQA  YMM0,  [EAX+000]
  VMOVNTDQA  YMM1,  [EAX+032]
  VMOVNTDQA  YMM2,  [EAX+064]
  VMOVNTDQA  YMM3,  [EAX+096]
  VMOVNTDQA  YMM4,  [EAX+128]
  VMOVNTDQA  YMM5,  [EAX+160]
  VMOVNTDQA  YMM6,  [EAX+192]
  VMOVNTDQA  YMM7,  [EAX+224]
  VMOVNTDQ   [EDX+000],  YMM0
  VMOVNTDQ   [EDX+032],  YMM1
  VMOVNTDQ   [EDX+064],  YMM2
  VMOVNTDQ   [EDX+096],  YMM3
  VMOVNTDQ   [EDX+128],  YMM4
  VMOVNTDQ   [EDX+160],  YMM5
  VMOVNTDQ   [EDX+192],  YMM6
  VMOVNTDQ   [EDX+224],  YMM7
  VMOVNTDQA  YMM0,  [EAX+256]
  VMOVNTDQA  YMM1,  [EAX+288]
  VMOVNTDQA  YMM2,  [EAX+320]
  VMOVNTDQA  YMM3,  [EAX+352]
  VMOVNTDQA  YMM4,  [EAX+384]
  VMOVNTDQA  YMM5,  [EAX+416]
  VMOVNTDQA  YMM6,  [EAX+448]
  VMOVNTDQA  YMM7,  [EAX+480]
  VMOVNTDQ   [EDX+256],  YMM0
  VMOVNTDQ   [EDX+288],  YMM1
  VMOVNTDQ   [EDX+320],  YMM2
  VMOVNTDQ   [EDX+352],  YMM3
  VMOVNTDQ   [EDX+384],  YMM4
  VMOVNTDQ   [EDX+416],  YMM5
  VMOVNTDQ   [EDX+448],  YMM6
  VMOVNTDQ   [EDX+480],  YMM7
  {$ENDIF}
  {$ENDIF}

  {$IFDEF WIN64}
  DB $67, $C4, $E2, $7D, $2A, $00
  DB $67, $C4, $E2, $7D, $2A, $48, $20
  DB $67, $C4, $E2, $7D, $2A, $50, $40
  DB $67, $C4, $E2, $7D, $2A, $58, $60
  DB $67, $C4, $E2, $7D, $2A, $A0, $80, $00, $00, $00
  DB $67, $C4, $E2, $7D, $2A, $A8, $A0, $00, $00, $00
  DB $67, $C4, $E2, $7D, $2A, $B0, $C0, $00, $00, $00
  DB $67, $C4, $E2, $7D, $2A, $B8, $E0, $00, $00, $00
  DB $67, $C4, $62, $7D, $2A, $80, $00, $01, $00, $00
  DB $67, $C4, $62, $7D, $2A, $88, $20, $01, $00, $00
  DB $67, $C4, $62, $7D, $2A, $90, $40, $01, $00, $00
  DB $67, $C4, $62, $7D, $2A, $98, $60, $01, $00, $00
  DB $67, $C4, $62, $7D, $2A, $A0, $80, $01, $00, $00
  DB $67, $C4, $62, $7D, $2A, $A8, $A0, $01, $00, $00
  DB $67, $C4, $62, $7D, $2A, $B0, $C0, $01, $00, $00
  DB $67, $C4, $62, $7D, $2A, $B8, $E0, $01, $00, $00
  DB $67, $C5, $FD, $E7, $02
  DB $67, $C5, $FD, $E7, $4A, $20
  DB $67, $C5, $FD, $E7, $52, $40
  DB $67, $C5, $FD, $E7, $5A, $60
  DB $67, $C5, $FD, $E7, $A2, $80, $00, $00, $00
  DB $67, $C5, $FD, $E7, $AA, $A0, $00, $00, $00
  DB $67, $C5, $FD, $E7, $B2, $C0, $00, $00, $00
  DB $67, $C5, $FD, $E7, $BA, $E0, $00, $00, $00
  DB $67, $C5, $7D, $E7, $82, $00, $01, $00, $00
  DB $67, $C5, $7D, $E7, $8A, $20, $01, $00, $00
  DB $67, $C5, $7D, $E7, $92, $40, $01, $00, $00
  DB $67, $C5, $7D, $E7, $9A, $60, $01, $00, $00
  DB $67, $C5, $7D, $E7, $A2, $80, $01, $00, $00
  DB $67, $C5, $7D, $E7, $AA, $A0, $01, $00, $00
  DB $67, $C5, $7D, $E7, $B2, $C0, $01, $00, $00
  DB $67, $C5, $7D, $E7, $BA, $E0, $01, $00, $00
  {$ELSE}
  DB $C4, $E2, $7D, $2A, $00
  DB $C4, $E2, $7D, $2A, $48, $20
  DB $C4, $E2, $7D, $2A, $50, $40
  DB $C4, $E2, $7D, $2A, $58, $60
  DB $C4, $E2, $7D, $2A, $A0, $80, $00, $00, $00
  DB $C4, $E2, $7D, $2A, $A8, $A0, $00, $00, $00
  DB $C4, $E2, $7D, $2A, $B0, $C0, $00, $00, $00
  DB $C4, $E2, $7D, $2A, $B8, $E0, $00, $00, $00
  DB $C5, $FD, $E7, $02
  DB $C5, $FD, $E7, $4A, $20
  DB $C5, $FD, $E7, $52, $40
  DB $C5, $FD, $E7, $5A, $60
  DB $C5, $FD, $E7, $A2, $80, $00, $00, $00
  DB $C5, $FD, $E7, $AA, $A0, $00, $00, $00
  DB $C5, $FD, $E7, $B2, $C0, $00, $00, $00
  DB $C5, $FD, $E7, $BA, $E0, $00, $00, $00
  DB $C4, $E2, $7D, $2A, $80, $00, $01, $00, $00
  DB $C4, $E2, $7D, $2A, $88, $20, $01, $00, $00
  DB $C4, $E2, $7D, $2A, $90, $40, $01, $00, $00
  DB $C4, $E2, $7D, $2A, $98, $60, $01, $00, $00
  DB $C4, $E2, $7D, $2A, $A0, $80, $01, $00, $00
  DB $C4, $E2, $7D, $2A, $A8, $A0, $01, $00, $00
  DB $C4, $E2, $7D, $2A, $B0, $C0, $01, $00, $00
  DB $C4, $E2, $7D, $2A, $B8, $E, $001, $00, $00
  DB $C5, $FD, $E7, $82, $00, $01, $00, $00
  DB $C5, $FD, $E7, $8A, $20, $01, $00, $00
  DB $C5, $FD, $E7, $92, $40, $01, $00, $00
  DB $C5, $FD, $E7, $9A, $60, $01, $00, $00
  DB $C5, $FD, $E7, $A2, $80, $01, $00, $00
  DB $C5, $FD, $E7, $AA, $A0, $01, $00, $00
  DB $C5, $FD, $E7, $B2, $C0, $01, $00, $00
  DB $C5, $FD, $E7, $BA, $E0, $01, $00, $00
  {$ENDIF}

  ADD    EAX, 512
  ADD    EDX, 512
  SUB    ECX, 512
  JNZ    @LOOP
  SFENCE
end;

end.
