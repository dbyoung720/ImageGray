# Delphi 数字图像处理优化 MMX/SSE/SSE2/SSE4/AVX/AVX2/AVX512

- [English](readme.md)

## 说明：
    这是我学习 SIMD 的过程记录
    功能：32bit Pixel RGB to Gray
    开发：Delphi 11
    平台：Win10X64；支持 X86/X64

### Delphi 下实现的函数：
```
type
  TGrayType = (gtAPI, gtScanLine, gtDelphi, gtFourPoint, gtParallel, gtGDIPLUS, gtTable, gtASM, gtMMX, gtSSE, gtSSE2, gtSSE4, gtSSEParallel, gtAVX1, gtAVX2, gtAVX512knl, gtAVX512skx, gtGPU, gtOther);
```
## 最优的方案： 
    gtSSEParallel
