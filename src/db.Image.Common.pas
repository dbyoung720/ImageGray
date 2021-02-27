unit db.Image.Common;
{
  Func: 32位位图公共单元
  Name: dbyoung@sina.com
  Date: 2020-10-01
  Vers: Delphi 10.3.2
  Test: 4096 * 4096 * 32
  Note：Delphi 的 Release 模式是有优化的，Debug 是没有的；下面的时间，都是在 DEBUG 模式下的用时；
  Note: 并行程序，不能在 IDE 下运行查看效果。必须脱离 IDE 执行查看效果。
}

interface

uses Winapi.Windows, Winapi.GDIPAPI, System.Classes, System.SysUtils, System.UITypes, System.Math, Vcl.Forms, Vcl.StdCtrls, Vcl.Graphics, Vcl.ComCtrls;

const
  c_intMinMaxValue: array [0 .. 4, 0 .. 1] of Integer = ((-255, 255), (-128, 127), (-255, 255), (0, 360), (0, 255));
  c_strShowTips: array [0 .. 4, 0 .. 1] of String     = (('调节亮度：', '亮度：'), ('调节对比度：', '对比度：'), ('调节饱和度：', '饱和度：'), ('调节图片色彩：', '色彩：'), ('调节透明度：', '透明：'));
  c_strShowTime: array [0 .. 4] of String             = ('调节亮度用时：%d 毫秒', '调节对比度用时：%d 毫秒', '调节饱和度用时：%d 毫秒', '调节图片色彩用时：%d 毫秒', '调节透明度用时：%d 毫秒');

  c_GrayR77: array [0 .. 255] of DWORD = (                                                                          //
    0, 77, 154, 231, 308, 385, 462, 539, 616, 693, 770, 847, 924, 1001, 1078, 1155,                                 //
    1232, 1309, 1386, 1463, 1540, 1617, 1694, 1771, 1848, 1925, 2002, 2079, 2156, 2233, 2310, 2387,                 //
    2464, 2541, 2618, 2695, 2772, 2849, 2926, 3003, 3080, 3157, 3234, 3311, 3388, 3465, 3542, 3619,                 //
    3696, 3773, 3850, 3927, 4004, 4081, 4158, 4235, 4312, 4389, 4466, 4543, 4620, 4697, 4774, 4851,                 //
    4928, 5005, 5082, 5159, 5236, 5313, 5390, 5467, 5544, 5621, 5698, 5775, 5852, 5929, 6006, 6083,                 //
    6160, 6237, 6314, 6391, 6468, 6545, 6622, 6699, 6776, 6853, 6930, 7007, 7084, 7161, 7238, 7315,                 //
    7392, 7469, 7546, 7623, 7700, 7777, 7854, 7931, 8008, 8085, 8162, 8239, 8316, 8393, 8470, 8547,                 //
    8624, 8701, 8778, 8855, 8932, 9009, 9086, 9163, 9240, 9317, 9394, 9471, 9548, 9625, 9702, 9779,                 //
    9856, 9933, 10010, 10087, 10164, 10241, 10318, 10395, 10472, 10549, 10626, 10703, 10780, 10857, 10934, 11011,   //
    11088, 11165, 11242, 11319, 11396, 11473, 11550, 11627, 11704, 11781, 11858, 11935, 12012, 12089, 12166, 12243, //
    12320, 12397, 12474, 12551, 12628, 12705, 12782, 12859, 12936, 13013, 13090, 13167, 13244, 13321, 13398, 13475, //
    13552, 13629, 13706, 13783, 13860, 13937, 14014, 14091, 14168, 14245, 14322, 14399, 14476, 14553, 14630, 14707, //
    14784, 14861, 14938, 15015, 15092, 15169, 15246, 15323, 15400, 15477, 15554, 15631, 15708, 15785, 15862, 15939, //
    16016, 16093, 16170, 16247, 16324, 16401, 16478, 16555, 16632, 16709, 16786, 16863, 16940, 17017, 17094, 17171, //
    17248, 17325, 17402, 17479, 17556, 17633, 17710, 17787, 17864, 17941, 18018, 18095, 18172, 18249, 18326, 18403, //
    18480, 18557, 18634, 18711, 18788, 18865, 18942, 19019, 19096, 19173, 19250, 19327, 19404, 19481, 19558, 19635);
  c_GrayG151: array [0 .. 255] of DWORD = (                                                                         //
    0, 151, 302, 453, 604, 755, 906, 1057, 1208, 1359, 1510, 1661, 1812, 1963, 2114, 2265,                          //
    2416, 2567, 2718, 2869, 3020, 3171, 3322, 3473, 3624, 3775, 3926, 4077, 4228, 4379, 4530, 4681,                 //
    4832, 4983, 5134, 5285, 5436, 5587, 5738, 5889, 6040, 6191, 6342, 6493, 6644, 6795, 6946, 7097,                 //
    7248, 7399, 7550, 7701, 7852, 8003, 8154, 8305, 8456, 8607, 8758, 8909, 9060, 9211, 9362, 9513,                 //
    9664, 9815, 9966, 10117, 10268, 10419, 10570, 10721, 10872, 11023, 11174, 11325, 11476, 11627, 11778, 11929,    //
    12080, 12231, 12382, 12533, 12684, 12835, 12986, 13137, 13288, 13439, 13590, 13741, 13892, 14043, 14194, 14345, //
    14496, 14647, 14798, 14949, 15100, 15251, 15402, 15553, 15704, 15855, 16006, 16157, 16308, 16459, 16610, 16761, //
    16912, 17063, 17214, 17365, 17516, 17667, 17818, 17969, 18120, 18271, 18422, 18573, 18724, 18875, 19026, 19177, //
    19328, 19479, 19630, 19781, 19932, 20083, 20234, 20385, 20536, 20687, 20838, 20989, 21140, 21291, 21442, 21593, //
    21744, 21895, 22046, 22197, 22348, 22499, 22650, 22801, 22952, 23103, 23254, 23405, 23556, 23707, 23858, 24009, //
    24160, 24311, 24462, 24613, 24764, 24915, 25066, 25217, 25368, 25519, 25670, 25821, 25972, 26123, 26274, 26425, //
    26576, 26727, 26878, 27029, 27180, 27331, 27482, 27633, 27784, 27935, 28086, 28237, 28388, 28539, 28690, 28841, //
    28992, 29143, 29294, 29445, 29596, 29747, 29898, 30049, 30200, 30351, 30502, 30653, 30804, 30955, 31106, 31257, //
    31408, 31559, 31710, 31861, 32012, 32163, 32314, 32465, 32616, 32767, 32918, 33069, 33220, 33371, 33522, 33673, //
    33824, 33975, 34126, 34277, 34428, 34579, 34730, 34881, 35032, 35183, 35334, 35485, 35636, 35787, 35938, 36089, //
    36240, 36391, 36542, 36693, 36844, 36995, 37146, 37297, 37448, 37599, 37750, 37901, 38052, 38203, 38354, 38505);
  c_GrayB28: array [0 .. 255] of DWORD = (                                                          //
    0, 28, 56, 84, 112, 140, 168, 196, 224, 252, 280, 308, 336, 364, 392, 420,                      //
    448, 476, 504, 532, 560, 588, 616, 644, 672, 700, 728, 756, 784, 812, 840, 868,                 //
    896, 924, 952, 980, 1008, 1036, 1064, 1092, 1120, 1148, 1176, 1204, 1232, 1260, 1288, 1316,     //
    1344, 1372, 1400, 1428, 1456, 1484, 1512, 1540, 1568, 1596, 1624, 1652, 1680, 1708, 1736, 1764, //
    1792, 1820, 1848, 1876, 1904, 1932, 1960, 1988, 2016, 2044, 2072, 2100, 2128, 2156, 2184, 2212, //
    2240, 2268, 2296, 2324, 2352, 2380, 2408, 2436, 2464, 2492, 2520, 2548, 2576, 2604, 2632, 2660, //
    2688, 2716, 2744, 2772, 2800, 2828, 2856, 2884, 2912, 2940, 2968, 2996, 3024, 3052, 3080, 3108, //
    3136, 3164, 3192, 3220, 3248, 3276, 3304, 3332, 3360, 3388, 3416, 3444, 3472, 3500, 3528, 3556, //
    3584, 3612, 3640, 3668, 3696, 3724, 3752, 3780, 3808, 3836, 3864, 3892, 3920, 3948, 3976, 4004, //
    4032, 4060, 4088, 4116, 4144, 4172, 4200, 4228, 4256, 4284, 4312, 4340, 4368, 4396, 4424, 4452, //
    4480, 4508, 4536, 4564, 4592, 4620, 4648, 4676, 4704, 4732, 4760, 4788, 4816, 4844, 4872, 4900, //
    4928, 4956, 4984, 5012, 5040, 5068, 5096, 5124, 5152, 5180, 5208, 5236, 5264, 5292, 5320, 5348, //
    5376, 5404, 5432, 5460, 5488, 5516, 5544, 5572, 5600, 5628, 5656, 5684, 5712, 5740, 5768, 5796, //
    5824, 5852, 5880, 5908, 5936, 5964, 5992, 6020, 6048, 6076, 6104, 6132, 6160, 6188, 6216, 6244, //
    6272, 6300, 6328, 6356, 6384, 6412, 6440, 6468, 6496, 6524, 6552, 6580, 6608, 6636, 6664, 6692, //
    6720, 6748, 6776, 6804, 6832, 6860, 6888, 6916, 6944, 6972, 7000, 7028, 7056, 7084, 7112, 7140);
  c_GrayValue: array [0 .. 255] of DWORD = (                                                                                                                        //
    0, 65793, 131586, 197379, 263172, 328965, 394758, 460551, 526344, 592137, 657930, 723723, 789516, 855309, 921102, 986895,                                       //
    1052688, 1118481, 1184274, 1250067, 1315860, 1381653, 1447446, 1513239, 1579032, 1644825, 1710618, 1776411, 1842204, 1907997, 1973790, 2039583,                 //
    2105376, 2171169, 2236962, 2302755, 2368548, 2434341, 2500134, 2565927, 2631720, 2697513, 2763306, 2829099, 2894892, 2960685, 3026478, 3092271,                 //
    3158064, 3223857, 3289650, 3355443, 3421236, 3487029, 3552822, 3618615, 3684408, 3750201, 3815994, 3881787, 3947580, 4013373, 4079166, 4144959,                 //
    4210752, 4276545, 4342338, 4408131, 4473924, 4539717, 4605510, 4671303, 4737096, 4802889, 4868682, 4934475, 5000268, 5066061, 5131854, 5197647,                 //
    5263440, 5329233, 5395026, 5460819, 5526612, 5592405, 5658198, 5723991, 5789784, 5855577, 5921370, 5987163, 6052956, 6118749, 6184542, 6250335,                 //
    6316128, 6381921, 6447714, 6513507, 6579300, 6645093, 6710886, 6776679, 6842472, 6908265, 6974058, 7039851, 7105644, 7171437, 7237230, 7303023,                 //
    7368816, 7434609, 7500402, 7566195, 7631988, 7697781, 7763574, 7829367, 7895160, 7960953, 8026746, 8092539, 8158332, 8224125, 8289918, 8355711,                 //
    8421504, 8487297, 8553090, 8618883, 8684676, 8750469, 8816262, 8882055, 8947848, 9013641, 9079434, 9145227, 9211020, 9276813, 9342606, 9408399,                 //
    9474192, 9539985, 9605778, 9671571, 9737364, 9803157, 9868950, 9934743, 10000536, 10066329, 10132122, 10197915, 10263708, 10329501, 10395294, 10461087,         //
    10526880, 10592673, 10658466, 10724259, 10790052, 10855845, 10921638, 10987431, 11053224, 11119017, 11184810, 11250603, 11316396, 11382189, 11447982, 11513775, //
    11579568, 11645361, 11711154, 11776947, 11842740, 11908533, 11974326, 12040119, 12105912, 12171705, 12237498, 12303291, 12369084, 12434877, 12500670, 12566463, //
    12632256, 12698049, 12763842, 12829635, 12895428, 12961221, 13027014, 13092807, 13158600, 13224393, 13290186, 13355979, 13421772, 13487565, 13553358, 13619151, //
    13684944, 13750737, 13816530, 13882323, 13948116, 14013909, 14079702, 14145495, 14211288, 14277081, 14342874, 14408667, 14474460, 14540253, 14606046, 14671839, //
    14737632, 14803425, 14869218, 14935011, 15000804, 15066597, 15132390, 15198183, 15263976, 15329769, 15395562, 15461355, 15527148, 15592941, 15658734, 15724527, //
    15790320, 15856113, 15921906, 15987699, 16053492, 16119285, 16185078, 16250871, 16316664, 16382457, 16448250, 16514043, 16579836, 16645629, 16711422, 16777215);
  c_PixBGRAMask: DWORD            = $FF;
  c_GraySSERioR: DWORD            = $4D;
  c_GraySSERioG: DWORD            = $97;
  c_GraySSERioB: DWORD            = $1C;
  c_GraySSEDiv3: DWORD            = $55;
  c_ContSSERaio: DWORD            = $64;
  c_ContSSEMask: DWORD            = $80;
  c_ContSSETENX: DWORD            = $028F;
  c_GrayMMXAdd: UINT64            = $0001000100010001;
  c_GrayMMXARGB: UINT64           = $0000004D0095001C;
  c_GrayMMXAMask: UINT64          = $FF000000FF000000;
  c_GrayMMXMask0: UINT64          = $00FF00FF00FF00FF;
  c_GrayMMXMask1: UINT64          = $0000FF000000FF00;
  c_GrayMMXRB: UINT64             = $004D001C004D001C;
  c_GrayMMXKG_: UINT64            = $0097009700970097;
  c_GrayColorMatrix: TColorMatrix = ( //
    (0.299, 0.299, 0.299, 0, 0),      //
    (0.587, 0.587, 0.587, 0, 0),      //
    (0.114, 0.114, 0.114, 0, 0),      //
    (0, 0, 0, 1, 0),                  //
    (0, 0, 0, 0, 1)                   //
    );

type
  TTwoLight255 = array [0 .. 255, -255 .. 255] of Byte;
  TGrayTable   = array [0 .. 1785] of Integer;
  TLightTable  = TTwoLight255;
  TColorChange = (ccLight, ccContrast, ccSaturation, ccColorMode, ccTranslate);
  TAlpha       = array [0 .. 255] of Integer;
  TGrays       = array [0 .. 767] of Integer;
  TVec4i       = array [0 .. 3] of Integer;
  TVec4f       = array [0 .. 3] of Single;
  PVec4i       = ^TVec4i;
  PVec4f       = ^TVec4f;

function GetBmpWidthBytes(bmp: TBitmap):DWORD;
function GetBitsPointer(bmp: TBitmap): Pointer;
function GetPixelGray(const r, g, b: Byte): TRGBQuad; inline;
function CheckValue(const intValue, intRange: Integer): Byte; inline;
procedure ShowColorChange(frmMain: TForm; cc: TColorChange; OnChangeLight, OnLightResetClick, OnLightCancelClick, OnLightOKClick: TNotifyEvent; var lblValueShow: TLabel; const intMinValue, intMaxValue: Integer; const strCaption, strTip: string);

function CRC32_Calculate(Buffer: PChar; len: Cardinal): Cardinal; cdecl; external name {$IFDEF win32} '_sse42_calculate'; {$ELSE} 'sse42_calculate'; {$ENDIF}

procedure bgraGray_sse2(src: PByte; dst: PDWORD; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraGray_sse2' {$ELSE} name 'bgraGray_sse2' {$IFEND};
procedure bgraGray_sse4(src: PByte; dst: PDWORD; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraGray_sse4' {$ELSE} name 'bgraGray_sse4' {$IFEND};
procedure bgraGray_avx1(src: PByte; dst: PDWORD; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraGray_avx' {$ELSE} name 'bgraGray_avx' {$IFEND};
procedure bgraGray_avx2(src: PByte; dst: PDWORD; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraGray_avx2' {$ELSE} name 'bgraGray_avx2' {$IFEND};
procedure bgraGray_avx512skx(src: PByte; dst: PDWORD; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraGray_avx512skx' {$ELSE} name 'bgraGray_avx512skx' {$IFEND};
procedure bgraGray_avx512knl(src: PByte; dst: PDWORD; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraGray_avx512knl' {$ELSE} name 'bgraGray_avx512knl' {$IFEND};

procedure bgraInvert_sse2(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraInvert_sse2' {$ELSE} name 'bgraInvert_sse2' {$IFEND};
procedure bgraInvert_sse4(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraInvert_sse4' {$ELSE} name 'bgraInvert_sse4' {$IFEND};
procedure bgraInvert_avx1(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraInvert_avx' {$ELSE} name 'bgraInvert_avx' {$IFEND};
procedure bgraInvert_avx2(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraInvert_avx2' {$ELSE} name 'bgraInvert_avx2' {$IFEND};
procedure bgraInvert_avx512skx(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraInvert_avx512skx' {$ELSE} name 'bgraInvert_avx512skx' {$IFEND};
procedure bgraInvert_avx512knl(src: PByte; width, height: Integer); cdecl; external {$IFDEF WIN32}name '_bgraInvert_avx512knl' {$ELSE} name 'bgraInvert_avx512knl' {$IFEND};

procedure bgraLight_sse2(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraLight_sse2' {$ELSE} name 'bgraLight_sse2' {$IFEND};
procedure bgraLight_sse4(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraLight_sse4' {$ELSE} name 'bgraLight_sse4' {$IFEND};
procedure bgraLight_avx1(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraLight_avx' {$ELSE} name 'bgraLight_avx' {$IFEND};
procedure bgraLight_avx2(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraLight_avx2' {$ELSE} name 'bgraLight_avx2' {$IFEND};
procedure bgraLight_avx512skx(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraLight_avx512skx' {$ELSE} name 'bgraLight_avx512skx' {$IFEND};
procedure bgraLight_avx512knl(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraLight_avx512knl' {$ELSE} name 'bgraLight_avx512knl' {$IFEND};

procedure bgraContrast_sse2(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraContrast_sse2' {$ELSE} name 'bgraContrast_sse2' {$IFEND};
procedure bgraContrast_sse4(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraContrast_sse4' {$ELSE} name 'bgraContrast_sse4' {$IFEND};
procedure bgraContrast_avx1(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraContrast_avx' {$ELSE} name 'bgraContrast_avx' {$IFEND};
procedure bgraContrast_avx2(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraContrast_avx2' {$ELSE} name 'bgraContrast_avx2' {$IFEND};
procedure bgraContrast_avx512skx(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraContrast_avx512skx' {$ELSE} name 'bgraContrast_avx512skx' {$IFEND};
procedure bgraContrast_avx512knl(src: PByte; dst: PDWORD; width, height, keyValue: Integer); cdecl; external {$IFDEF WIN32}name '_bgraContrast_avx512knl' {$ELSE} name 'bgraContrast_avx512knl' {$IFEND};

procedure bgraSaturation_sse2(src: PByte; dst: PDWORD; width, height, keyValue: Integer; alpha: TAlpha; grays: TGrays); cdecl; external {$IFDEF WIN32}name '_bgraSaturation_sse2' {$ELSE} name 'bgraSaturation_sse2' {$IFEND};
procedure bgraSaturation_sse4(src: PByte; dst: PDWORD; width, height, keyValue: Integer; alpha: TAlpha; grays: TGrays); cdecl; external {$IFDEF WIN32}name '_bgraSaturation_sse4' {$ELSE} name 'bgraSaturation_sse4' {$IFEND};
procedure bgraSaturation_avx1(src: PByte; dst: PDWORD; width, height, keyValue: Integer; alpha: TAlpha; grays: TGrays); cdecl; external {$IFDEF WIN32}name '_bgraSaturation_avx' {$ELSE} name 'bgraSaturation_avx' {$IFEND};
procedure bgraSaturation_avx2(src: PByte; dst: PDWORD; width, height, keyValue: Integer; alpha: TAlpha; grays: TGrays); cdecl; external {$IFDEF WIN32}name '_bgraSaturation_avx2' {$ELSE} name 'bgraSaturation_avx2' {$IFEND};
procedure bgraSaturation_avx512skx(src: PByte; dst: PDWORD; width, height, keyValue: Integer; alpha: TAlpha; grays: TGrays); cdecl; external {$IFDEF WIN32}name '_bgraSaturation_avx512skx' {$ELSE} name 'bgraSaturation_avx512skx' {$IFEND};
procedure bgraSaturation_avx512knl(src: PByte; dst: PDWORD; width, height, keyValue: Integer; alpha: TAlpha; grays: TGrays); cdecl; external {$IFDEF WIN32}name '_bgraSaturation_avx512knl' {$ELSE} name 'bgraSaturation_avx512knl' {$IFEND};

procedure _abort; cdecl; external 'msvcrt.dll' name 'abort';
procedure abort; cdecl; external 'msvcrt.dll' name 'abort';
function __alldiv(a, b: Int64): Int64; stdcall; external 'ntdll.dll' name '_alldiv';
function __aulldiv(a, b: UINT64): UINT64; stdcall; external 'ntdll.dll' name '_aulldiv';

var
  g_GrayTable    : TGrayTable;
  g_LightTable   : TLightTable;
  g_ContrastTable: array [0 .. 255, 0 .. 255] of Byte;

{$IFDEF WIN32}
  __fltused: Integer;
{$ELSE}
  _fltused: Integer;
{$IFEND}

implementation

{$IFDEF WIN32}
{$LINK obj\crc32_x86.obj}
{$LINK obj\DAVX_X86.obj}
{$LINK obj\DAVX_X86_sse2.obj}
{$LINK obj\DAVX_X86_sse4.obj}
{$LINK obj\DAVX_X86_avx.obj}
{$LINK obj\DAVX_X86_avx2.obj}
{$LINK obj\DAVX_X86_avx512knl.obj}
{$LINK obj\DAVX_X86_avx512skx.obj}
{$ELSE}
{$LINK obj\crc32_x64.obj}
{$LINK obj\DAVX_X64.obj}
{$LINK obj\DAVX_X64_sse2.obj}
{$LINK obj\DAVX_X64_sse4.obj}
{$LINK obj\DAVX_X64_avx.obj}
{$LINK obj\DAVX_X64_avx2.obj}
{$LINK obj\DAVX_X64_avx512knl.obj}
{$LINK obj\DAVX_X64_avx512skx.obj}
{$ENDIF}

{ 存取类的保护成员变量 }
type
  TBMPAccess         = class(TBitmap);
  TBitmapImageAccess = class(TBitmapImage);

function GetBmpWidthBytes(bmp: TBitmap):DWORD;
{$IF CompilerVersion < 24.0}
var
  FImage: PDWORD;
  FDIB  : PDIBSection;
begin
  FImage := Pointer(@bmp.IgnorePalette);
  Dec(FImage, SizeOf(TCanvas) - 2);
  FDIB   := Pointer(FImage^ + 24);
  Result := FDIB^.dsBm.bmWidthBytes;
{$ELSE}
begin
  Result := TBitmapImageAccess(TBMPAccess(bmp).FImage).FDIB.dsBm.bmWidthBytes;
{$IFEND}
end;

function GetBitsPointer(bmp: TBitmap): Pointer;
{$IF CompilerVersion < 24.0}
var
  FImage: PDWORD;
  FDIB  : PDIBSection;
begin
  FImage := Pointer(@bmp.IgnorePalette);
  Dec(FImage, SizeOf(TCanvas) - 2);
  FDIB   := Pointer(FImage^ + 24);
  Result := FDIB^.dsBm.bmBits;
{$ELSE}
begin
  Result := TBitmapImageAccess(TBMPAccess(bmp).FImage).FDIB.dsBm.bmBits;
{$IFEND}
end;

function GetPixelGray(const r, g, b: Byte): TRGBQuad; inline;
var
  byeGray: Byte;
begin
  byeGray := (c_GrayR77[r] + c_GrayG151[g] + c_GrayB28[b]) shr 8;
  Result  := TRGBQuad(c_GrayValue[byeGray]);
end;

function CheckValue(const intValue, intRange: Integer): Byte; inline;
begin
  Result := EnsureRange(intValue + intRange, 0, 255);
end;

procedure InitGrayTable;
var
  I      : Integer;
  bytGray: Integer;
begin
  for I := Low(g_GrayTable) to High(g_GrayTable) do
  begin
    bytGray        := Round(I / 7);
    g_GrayTable[I] := RGB(bytGray, bytGray, bytGray);
  end;
end;

procedure InitLightTable;
var
  I, J: Integer;
begin
  for I := 0 to 255 do
  begin
    for J := -255 to 255 do
    begin
      g_LightTable[I, J] := EnsureRange(I + J, 0, 255);
    end;
  end;
end;

procedure ShowColorChange(frmMain: TForm; cc: TColorChange; OnChangeLight, OnLightResetClick, OnLightCancelClick, OnLightOKClick: TNotifyEvent; var lblValueShow: TLabel; const intMinValue, intMaxValue: Integer; const strCaption, strTip: string);
var
  frmLight                  : TForm;
  trckbrLight               : TTrackBar;
  lblTip, lblValue          : TLabel;
  btnReset, btnCancel, btnOK: TButton;
begin
  frmLight             := TForm.Create(nil);
  frmLight.BorderStyle := bsSingle;
  frmLight.Position    := poDesigned;
  frmLight.BorderIcons := frmLight.BorderIcons - [biMaximize, biMinimize];
  frmLight.width       := 600;
  frmLight.height      := 140;
  frmLight.Caption     := strCaption;
  frmLight.Left        := frmMain.Left + frmMain.width - frmLight.width - 10;
  frmLight.Top         := frmMain.Top + 55;
  frmLight.OnClose     := frmMain.OnClose;

  trckbrLight             := TTrackBar.Create(frmLight);
  trckbrLight.Parent      := frmLight;
  trckbrLight.width       := 440;
  trckbrLight.Left        := 70;
  trckbrLight.Top         := 20;
  trckbrLight.Min         := intMinValue;
  trckbrLight.Max         := intMaxValue;
  trckbrLight.LineSize    := 10;
  trckbrLight.PageSize    := 10;
  trckbrLight.Frequency   := 10;
  trckbrLight.ThumbLength := 20;
  trckbrLight.Tag         := Integer(cc);
  trckbrLight.OnChange    := OnChangeLight;

  lblTip            := TLabel.Create(frmLight);
  lblTip.Parent     := frmLight;
  lblTip.Left       := 8;
  lblTip.Top        := 16;
  lblTip.Caption    := strTip;
  lblTip.Font.Color := clRed;
  lblTip.Font.size  := 14;
  lblTip.Font.Style := lblTip.Font.Style + [fsBold];

  lblValue            := TLabel.Create(frmLight);
  lblValue.Parent     := frmLight;
  lblValue.Left       := trckbrLight.Left + trckbrLight.width + 20;
  lblValue.Top        := 16;
  lblValue.Caption    := '0';
  lblValue.Font.Color := clRed;
  lblValue.Font.size  := 14;
  lblValue.Font.Style := lblValue.Font.Style + [fsBold];

  btnReset         := TButton.Create(frmLight);
  btnReset.Parent  := frmLight;
  btnReset.Left    := trckbrLight.Left + 5;
  btnReset.Top     := trckbrLight.Top + 40;
  btnReset.width   := 90;
  btnReset.height  := 30;
  btnReset.Caption := '复位';
  btnReset.OnClick := OnLightResetClick;

  btnCancel         := TButton.Create(frmLight);
  btnCancel.Parent  := frmLight;
  btnCancel.Left    := btnReset.Left + 170;
  btnCancel.Top     := trckbrLight.Top + 40;
  btnCancel.width   := 90;
  btnCancel.height  := 30;
  btnCancel.Caption := '取消';
  btnCancel.OnClick := OnLightCancelClick;

  btnOK         := TButton.Create(frmLight);
  btnOK.Parent  := frmLight;
  btnOK.Left    := btnCancel.Left + 170;
  btnOK.Top     := trckbrLight.Top + 40;
  btnOK.width   := 90;
  btnOK.height  := 30;
  btnOK.Caption := '确定';
  btnOK.OnClick := OnLightOKClick;

  lblValueShow := lblValue;
  frmLight.Show;
end;

procedure InitContrastTable;
var
  I, J: Integer;
begin
  for I := 0 to 255 do
  begin
    for J := 0 to 255 do
    begin
      g_ContrastTable[I, J] := EnsureRange((I - 128) * J div 100 + 128, 0, 255);
    end;
  end;
end;

initialization
  InitGrayTable;
  InitLightTable;
  InitContrastTable;

end.
