# RGB2Gray MMX/SSE/SSE2/SSE4/AVX/AVX2/AVX512 optimization

- [简体中文](readmeCN.md)

## Description：
    I record the process of learning SIMD
    Function：32bit Pixel RGB to Gray
    DevTools：Delphi 11
    Platform：Win10X64；Support X86、X64

### Delphi implemented function：
```
type
  TGrayType = (gtAPI, gtScanLine, gtDelphi, gtFourPoint, gtParallel, gtGDIPLUS, gtTable, gtASM, gtMMX, gtSSE, gtSSE2, gtSSE4, gtSSEParallel, gtAVX, gtAVX2, gtAVX512, gtGPU, gtOther);
```
## Best Optimization： 
    gtSSEParallel
  
