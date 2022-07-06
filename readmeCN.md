# Delphi 数字图像处理优化 MMX/SSE/SSE2/SSE4/AVX/AVX2/AVX512

- [English](readme.md)

## 说明：
    这是我学习 SIMD 的过程记录
    功能：32bit 位图，灰度化、反色、镜像、旋转、调节亮度、饱和度、对比度、透明，等等。
    开发：Delphi 11.0
    平台：Win10X64；支持 X86/X64
    测试：4096*4096

## 备注：
    1.由于 Delphi 11.1 x64 编译器的问题，Delphi 11.1, x64 模式下无法正常使用。感谢阿木的反馈
	  2.由于在内存中创建大的位图时，vcl.graphics.pas，会很耗时，所以要作修改
	  详见：https://stackoverflow.com/questions/2500498/delphi-fast-large-bitmap-creation-without-clearing/21281835#21281835
  
## 博客：
    https://blog.csdn.net/dbyoung/category_10762408.html
