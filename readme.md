# Delphi digital image processing optimization MMX/SSE/SSE2/SSE4/AVX/AVX2/AVX512

- [简体中文](readmeCN.md)

## Description：
    This is my record of learning SIMD
    Function: 32bit bitmap, gray、invert color、mirror、rotate、adjust brightness、saturation、contrast、transparency, etc.
    DevTools：Delphi 11.0
    Platform：Win10X64；Support X86、X64
    Test    ：4096*4096
    
## Remarks:
    1.Due to the problem of Delphi 11.1 x64 compiler, Delphi 11.1, x64 mode cannot be used normally. Thank 阿木 for his feedback;
		2.Because when creating a large bitmap in memory, vcl.graphics.pas is time-consuming, so it needs to be modified.
		  See: https://stackoverflow.com/questions/2500498/delphi-fast-large-bitmap-creation-without-clearing/21281835#21281835

## BLOG：
    https://blog.csdn.net/dbyoung/category_10762408.html
