# RGB2Gray MMX/SSE/SSE2/SSE4/AVX/AVX2/AVX512 优化

- [English](readme.md)

## 说明：
    功能：32bit Pixel RGB to Gray
    开发：Delphi 10.3
    平台：Win10X64；支持 X86/X64

### Delphi 下实现的函数：
```
type
  TGrayType = (gtAPI, gtScanLine, gtDelphi, gtFourPoint, gtParallel, gtGDIPLUS, gtTable, gtASM, gtMMX, gtSSE, gtSSE2, gtSSE4, gtAVX, gtAVX2, gtAVX512, gtGPU, gtOther);
```
