unit db.Memory.Call;
{
  Func: 内存复制优化函数的封装调用
  Name: dbyoung@sina.com
  Date: 2021-3-2
  Vers: Delphi 11

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

procedure dbMove(const src: Pointer; var dst: Pointer; const len: NativeInt);

implementation

uses db.Memory.Move;

{ ---------------------------------------------------------------------------------------------------------------------------------------- }
{ -------------------------------------------------------- Move 封装调用 ----------------------------------------------------------------- }
{ ---------------------------------------------------------------------------------------------------------------------------------------- }

type
  TCPUIDREG = record
    rEAX, rEBX, rECX, rEDX: Cardinal;
  end;

  TMoveType = ( //
    { 未按 16 字节对齐 }
    Byte, WORD, DWORD, UINT64, SSE2_16U, SSE2_32U, SSE2_64U, SSE2_128U, SSE2_256U, AVX1_32U, AVX1_64U, AVX1_128U, AVX1_256U, AVX1_512U, //
    { 按   16 字节对齐 }
    SSE2_16A, SSE2_32A, SSE2_64A, SSE2_128A, SSE2_256A, SSE4_16A, SSE4_32A, SSE4_64A, SSE4_128A, SSE4_256A, AVX1_32A, AVX1_64A, AVX1_128A, AVX1_256A, AVX1_512A, AVX2_32A, AVX2_64A, AVX2_128A, AVX2_256A, AVX2_512A);

  TMove<Pointer, NativeInt> = reference to procedure(const src: Pointer; dst: Pointer; const len: NativeInt);

procedure GetCPUID(const iNumber: Integer; var rValue: TCPUIDREG; const leaf: Integer = 0); assembler;
asm
  {$IFDEF WIN64}
  PUSH RSI
  MOV  RSI, RDX
  XCHG RAX, RCX
  {$ELSE}
  PUSH ESI
  MOV  ESI, EDX
  {$ENDIF}
  CPUID
  MOV TCPUIDREG[ESI].rEAX, EAX
  MOV TCPUIDREG[ESI].rEBX, EBX
  MOV TCPUIDREG[ESI].rECX, ECX
  MOV TCPUIDREG[ESI].rEDX, EDX
  {$IFDEF WIN64}
  POP RSI
  {$ELSE}
  POP ESI
  {$ENDIF}
end;

var
  FbAVX512: Boolean = False;
  FbAVX2  : Boolean = False;
  FbAVX1  : Boolean = False;
  FbSSE4  : Boolean = False;
  FbSSE2  : Boolean = False;

procedure CheckCUPID;
var
  rValue: TCPUIDREG;
begin
  GetCPUID(1, rValue);
  FbSSE2 := rValue.rEDX AND (1 SHL 26) <> 0;
  FbSSE4 := rValue.rECX AND (1 SHL 20) <> 0;
  FbAVX1 := rValue.rECX AND (1 SHL 28) <> 0;

  GetCPUID(7, rValue);
  FbAVX2   := rValue.rEBX AND (1 SHL 05) <> 0;
  FbAVX512 := rValue.rEBX AND (1 SHL 16) <> 0;
end;

procedure dbMove_x64_AVX2(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin

end;

procedure dbMove_x64_AVX1(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin

end;

procedure dbMove_x64_SSE4(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin

end;

procedure dbMove_x64_SSE2(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin

end;

procedure dbMove_x64(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin
  if FbAVX2 then
  begin
    { 如果 CPU 支持 AVX2，则优先使用 AVX2 }
    dbMove_x64_AVX2(src, dst, len);
  end
  else
  begin
    if FbAVX1 then
    begin
      { 如果 CPU 支持 AVX1，则优先使用 AVX1 }
      dbMove_x64_AVX1(src, dst, len);
    end
    else
    begin
      if FbSSE4 then
      begin
        { 如果 CPU 支持 SSE4，则优先使用 SSE4 }
        dbMove_x64_SSE4(src, dst, len);
      end
      else
      begin
        if FbSSE2 then
        begin
          { 如果 CPU 支持 SSE2，则优先使用 SSE2 }
          dbMove_x64_SSE2(src, dst, len);
        end;
      end;
    end;
  end;
end;

procedure dbMove_x86_AVX2(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin
  //
end;

procedure dbMove_x86_AVX1(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin

end;

procedure dbMove_x86_SSE4(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin

end;

procedure dbMove_x86_SSE2(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin

end;

procedure dbMove_x86(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin
  if FbAVX2 then
  begin
    { 如果 CPU 支持 AVX2，则优先使用 AVX2 }
    dbMove_x86_AVX2(src, dst, len);
  end
  else
  begin
    if FbAVX1 then
    begin
      { 如果 CPU 支持 AVX1，则优先使用 AVX1 }
      dbMove_x86_AVX1(src, dst, len);
    end
    else
    begin
      if FbSSE4 then
      begin
        { 如果 CPU 支持 SSE4，则优先使用 SSE4 }
        dbMove_x86_SSE4(src, dst, len);
      end
      else
      begin
        if FbSSE2 then
        begin
          { 如果 CPU 支持 SSE2，则优先使用 SSE2 }
          dbMove_x86_SSE2(src, dst, len);
        end;
      end;
    end;
  end;
end;

procedure dbMove(const src: Pointer; var dst: Pointer; const len: NativeInt);
begin
{$IFDEF WIN64}
  dbMove_x64(src, dst, len);
{$ELSE}
  dbMove_x86(src, dst, len);
{$ENDIF}
end;

initialization
  CheckCUPID;

end.
