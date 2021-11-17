{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2021 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit Vcl.Graphics;

{$HPPEMIT LEGACYHPP}
{$P+,S-,W-,R-,T-,X+,H+,B-}

{$IFDEF CPUX64}
  {$DEFINE PUREPASCAL}
{$ENDIF CPUX64}

interface

uses
{$IF DEFINED(CLR)}
  System.Drawing, System.Drawing.Imaging, System.Reflection, System.Globalization,
{$ENDIF}
{$IF DEFINED(LINUX)}
  WinUtils, Libc,
{$ENDIF}
  Winapi.Windows, System.UITypes, System.SysUtils, System.Classes
{$IF NOT DEFINED(CLR)}
  , Winapi.Wincodec
{$ENDIF}
  ;

type
  PColor = System.UITypes.PColor;
  {$NODEFINE PColor}
  TColor = System.UITypes.TColor;
  {$NODEFINE TColor}
  (*$HPPEMIT OPENNAMESPACE *)
  (*$HPPEMIT 'using System::Uitypes::TColor;' *)
  (*$HPPEMIT 'using System::Uitypes::PColor;' *)
  (*$HPPEMIT CLOSENAMESPACE *)

const
  clSystemColor = TColors.SystemColor;

  clScrollBar = TColors.SysScrollBar;
  clBackground = TColors.SysBackground;
  clActiveCaption = TColors.SysActiveCaption;
  clInactiveCaption = TColors.SysInactiveCaption;
  clMenu = TColors.SysMenu;
  clWindow = TColors.SysWindow;
  clWindowFrame = TColors.SysWindowFrame;
  clMenuText = TColors.SysMenuText;
  clWindowText = TColors.SysWindowText;
  clCaptionText = TColors.SysCaptionText;
  clActiveBorder = TColors.SysActiveBorder;
  clInactiveBorder = TColors.SysInactiveBorder;
  clAppWorkSpace = TColors.SysAppWorkSpace;
  clHighlight = TColors.SysHighlight;
  clHighlightText = TColors.SysHighlightText;
  clBtnFace = TColors.SysBtnFace;
  clBtnShadow = TColors.SysBtnShadow;
  clGrayText = TColors.SysGrayText;
  clBtnText = TColors.SysBtnText;
  clInactiveCaptionText = TColors.SysInactiveCaptionText;
  clBtnHighlight = TColors.SysBtnHighlight;
  cl3DDkShadow = TColors.Sys3DDkShadow;
  cl3DLight = TColors.Sys3DLight;
  clInfoText = TColors.SysInfoText;
  clInfoBk = TColors.SysInfoBk;
  clHotLight = TColors.SysHotLight;
  clGradientActiveCaption = TColors.SysGradientActiveCaption;
  clGradientInactiveCaption = TColors.SysGradientInactiveCaption;
  clMenuHighlight = TColors.SysMenuHighlight;
  clMenuBar = TColors.SysMenuBar;

  clBlack = TColors.Black;
  clMaroon = TColors.Maroon;
  clGreen = TColors.Green;
  clOlive = TColors.Olive;
  clNavy = TColors.Navy;
  clPurple = TColors.Purple;
  clTeal = TColors.Teal;
  clGray = TColors.Gray;
  clSilver = TColors.Silver;
  clRed = TColors.Red;
  clLime = TColors.Lime;
  clYellow = TColors.Yellow;
  clBlue = TColors.Blue;
  clFuchsia = TColors.Fuchsia;
  clAqua = TColors.Aqua;
  clLtGray = TColors.LtGray;
  clDkGray = TColors.DkGray;
  clWhite = TColors.White;
  StandardColorsCount = 16;

  clMoneyGreen = TColors.MoneyGreen;
  clSkyBlue = TColors.LegacySkyBlue;
  clCream = TColors.Cream;
  clMedGray = TColors.MedGray;
  ExtendedColorsCount = 4;

  clNone = TColors.SysNone;
  clDefault = TColors.SysDefault;

  { The following "cl" values come from the Web Named Color palette and
    are stored in the Windows COLORREF byte order x00bbggrr }
  clWebSnow = TColors.Snow;
  clWebFloralWhite = TColors.FloralWhite;
  clWebLavenderBlush = TColors.LavenderBlush;
  clWebOldLace = TColors.OldLace;
  clWebIvory = TColors.Ivory;
  clWebCornSilk = TColors.CornSilk;
  clWebBeige = TColors.Beige;
  clWebAntiqueWhite = TColors.AntiqueWhite;
  clWebWheat = TColors.Wheat;
  clWebAliceBlue = TColors.AliceBlue;
  clWebGhostWhite = TColors.GhostWhite;
  clWebLavender = TColors.Lavender;
  clWebSeashell = TColors.Seashell;
  clWebLightYellow = TColors.LightYellow;
  clWebPapayaWhip = TColors.PapayaWhip;
  clWebNavajoWhite = TColors.NavajoWhite;
  clWebMoccasin = TColors.Moccasin;
  clWebBurlywood = TColors.Burlywood;
  clWebAzure = TColors.Azure;
  clWebMintcream = TColors.Mintcream;
  clWebHoneydew = TColors.Honeydew;
  clWebLinen = TColors.Linen;
  clWebLemonChiffon = TColors.LemonChiffon;
  clWebBlanchedAlmond = TColors.BlanchedAlmond;
  clWebBisque = TColors.Bisque;
  clWebPeachPuff = TColors.PeachPuff;
  clWebTan = TColors.Tan;
  // yellows/reds yellow -> rosybrown
  clWebYellow = TColors.Yellow;
  clWebDarkOrange = TColors.DarkOrange;
  clWebRed = TColors.Red;
  clWebDarkRed = TColors.DarkRed;
  clWebMaroon = TColors.Maroon;
  clWebIndianRed = TColors.IndianRed;
  clWebSalmon = TColors.Salmon;
  clWebCoral = TColors.Coral;
  clWebGold = TColors.Gold;
  clWebTomato = TColors.Tomato;
  clWebCrimson = TColors.Crimson;
  clWebBrown = TColors.Brown;
  clWebChocolate = TColors.Chocolate;
  clWebSandyBrown = TColors.SandyBrown;
  clWebLightSalmon = TColors.LightSalmon;
  clWebLightCoral = TColors.LightCoral;
  clWebOrange = TColors.Orange;
  clWebOrangeRed = TColors.OrangeRed;
  clWebFirebrick = TColors.Firebrick;
  clWebSaddleBrown = TColors.SaddleBrown;
  clWebSienna = TColors.Sienna;
  clWebPeru = TColors.Peru;
  clWebDarkSalmon = TColors.DarkSalmon;
  clWebRosyBrown = TColors.RosyBrown;
  // greens palegoldenrod -> darkseagreen
  clWebPaleGoldenrod = TColors.PaleGoldenrod;
  clWebLightGoldenrodYellow = TColors.LightGoldenrodYellow;
  clWebOlive = TColors.Olive;
  clWebForestGreen = TColors.ForestGreen;
  clWebGreenYellow = TColors.GreenYellow;
  clWebChartreuse = TColors.Chartreuse;
  clWebLightGreen = TColors.LightGreen;
  clWebAquamarine = TColors.Aquamarine;
  clWebSeaGreen = TColors.SeaGreen;
  clWebGoldenRod = TColors.GoldenRod;
  clWebKhaki = TColors.Khaki;
  clWebOliveDrab = TColors.OliveDrab;
  clWebGreen = TColors.Green;
  clWebYellowGreen = TColors.YellowGreen;
  clWebLawnGreen = TColors.LawnGreen;
  clWebPaleGreen = TColors.PaleGreen;
  clWebMediumAquamarine = TColors.MediumAquamarine;
  clWebMediumSeaGreen = TColors.MediumSeaGreen;
  clWebDarkGoldenRod = TColors.DarkGoldenRod;
  clWebDarkKhaki = TColors.DarkKhaki;
  clWebDarkOliveGreen = TColors.DarkOliveGreen;
  clWebDarkgreen = TColors.Darkgreen;
  clWebLimeGreen = TColors.LimeGreen;
  clWebLime = TColors.Lime;
  clWebSpringGreen = TColors.SpringGreen;
  clWebMediumSpringGreen = TColors.MediumSpringGreen;
  clWebDarkSeaGreen = TColors.DarkSeaGreen;
  // greens/blues lightseagreen -> navy
  clWebLightSeaGreen = TColors.LightSeaGreen;
  clWebPaleTurquoise = TColors.PaleTurquoise;
  clWebLightCyan = TColors.LightCyan;
  clWebLightBlue = TColors.LightBlue;
  clWebLightSkyBlue = TColors.LightSkyBlue;
  clWebCornFlowerBlue = TColors.CornFlowerBlue;
  clWebDarkBlue = TColors.DarkBlue;
  clWebIndigo = TColors.Indigo;
  clWebMediumTurquoise = TColors.MediumTurquoise;
  clWebTurquoise = TColors.Turquoise;
  clWebCyan = TColors.Cyan; //   clWebAqua
  clWebAqua = TColors.Aqua;
  clWebPowderBlue = TColors.PowderBlue;
  clWebSkyBlue = TColors.SkyBlue;
  clWebRoyalBlue = TColors.RoyalBlue;
  clWebMediumBlue = TColors.MediumBlue;
  clWebMidnightBlue = TColors.MidnightBlue;
  clWebDarkTurquoise = TColors.DarkTurquoise;
  clWebCadetBlue = TColors.CadetBlue;
  clWebDarkCyan = TColors.DarkCyan;
  clWebTeal = TColors.Teal;
  clWebDeepskyBlue = TColors.DeepskyBlue;
  clWebDodgerBlue = TColors.DodgerBlue;
  clWebBlue = TColors.Blue;
  clWebNavy = TColors.Navy;
  // violets/pinks darkviolet -> pink
  clWebDarkViolet = TColors.DarkViolet;
  clWebDarkOrchid = TColors.DarkOrchid;
  clWebMagenta = TColors.Magenta; //   clWebFuchsia
  clWebFuchsia = TColors.Fuchsia;
  clWebDarkMagenta = TColors.DarkMagenta;
  clWebMediumVioletRed = TColors.MediumVioletRed;
  clWebPaleVioletRed = TColors.PaleVioletRed;
  clWebBlueViolet = TColors.BlueViolet;
  clWebMediumOrchid = TColors.MediumOrchid;
  clWebMediumPurple = TColors.MediumPurple;
  clWebPurple = TColors.Purple;
  clWebDeepPink = TColors.DeepPink;
  clWebLightPink = TColors.LightPink;
  clWebViolet = TColors.Violet;
  clWebOrchid = TColors.Orchid;
  clWebPlum = TColors.Plum;
  clWebThistle = TColors.Thistle;
  clWebHotPink = TColors.HotPink;
  clWebPink = TColors.Pink;
  // blue/gray/black lightsteelblue -> black
  clWebLightSteelBlue = TColors.LightSteelBlue;
  clWebMediumSlateBlue = TColors.MediumSlateBlue;
  clWebLightSlateGray = TColors.LightSlateGray;
  clWebWhite = TColors.White;
  clWebLightgrey = TColors.Lightgrey;
  clWebGray = TColors.Gray;
  clWebSteelBlue = TColors.SteelBlue;
  clWebSlateBlue = TColors.SlateBlue;
  clWebSlateGray = TColors.SlateGray;
  clWebWhiteSmoke = TColors.WhiteSmoke;
  clWebSilver = TColors.Silver;
  clWebDimGray = TColors.DimGray;
  clWebMistyRose = TColors.MistyRose;
  clWebDarkSlateBlue = TColors.DarkSlateBlue;
  clWebDarkSlategray = TColors.DarkSlategray;
  clWebGainsboro = TColors.Gainsboro;
  clWebDarkGray = TColors.DarkGray;
  clWebBlack = TColors.Black;
  WebColorsCount = 140;  { Two of which are duplicates Aqua/Cyan Fuchsia/Magenta }

const
  cmBlackness = BLACKNESS;
  cmDstInvert = DSTINVERT;
  cmMergeCopy = MERGECOPY;
  cmMergePaint = MERGEPAINT;
  cmNotSrcCopy = NOTSRCCOPY;
  cmNotSrcErase = NOTSRCERASE;
  cmPatCopy = PATCOPY;
  cmPatInvert = PATINVERT;
  cmPatPaint = PATPAINT;
  cmSrcAnd = SRCAND;
  cmSrcCopy = SRCCOPY;
  cmSrcErase = SRCERASE;
  cmSrcInvert = SRCINVERT;
  cmSrcPaint = SRCPAINT;
  cmWhiteness = WHITENESS;

  { Icon and cursor types }
  rc3_StockIcon = 0;
  rc3_Icon = 1;
  rc3_Cursor = 2;

const
  // TFontStyle
  fsBold      = System.UITypes.TFontStyle.fsBold;
  {$EXTERNALSYM fsBold}
  fsItalic    = System.UITypes.TFontStyle.fsItalic;
  {$EXTERNALSYM fsItalic}
  fsUnderline = System.UITypes.TFontStyle.fsUnderline;
  {$EXTERNALSYM fsUnderline}
  fsStrikeOut = System.UITypes.TFontStyle.fsStrikeOut;
  {$EXTERNALSYM fsStrikeOut}

  // TFontPitch
  fpDefault  = System.UITypes.TFontPitch.fpDefault;
  {$EXTERNALSYM fpDefault}
  fpVariable = System.UITypes.TFontPitch.fpVariable;
  {$EXTERNALSYM fpVariable}
  fpFixed    = System.UITypes.TFontPitch.fpFixed;
  {$EXTERNALSYM fpFixed}

  // TFontQuality
  fqDefault          = System.UITypes.TFontQuality.fqDefault;
  {$EXTERNALSYM fqDefault}
  fqDraft            = System.UITypes.TFontQuality.fqDraft;
  {$EXTERNALSYM fqDraft}
  fqProof            = System.UITypes.TFontQuality.fqProof;
  {$EXTERNALSYM fqProof}
  fqNonAntialiased   = System.UITypes.TFontQuality.fqNonAntialiased;
  {$EXTERNALSYM fqNonAntialiased}
  fqAntialiased      = System.UITypes.TFontQuality.fqAntialiased;
  {$EXTERNALSYM fqAntialiased}
  fqClearType        = System.UITypes.TFontQuality.fqClearType;
  {$EXTERNALSYM fqClearType}
  fqClearTypeNatural = System.UITypes.TFontQuality.fqClearTypeNatural;
  {$EXTERNALSYM fqClearTypeNatural}

// MASK for tfComposited
  MASK_TF_COMPOSITED = $00800000;
  {$EXTERNALSYM MASK_TF_COMPOSITED}

type
{$IF NOT DEFINED(CLR)}
  PCursorOrIcon = ^TCursorOrIcon;
{$ENDIF}
  TCursorOrIcon = record
    Reserved: Word;
    wType: Word;
    Count: Word;
  end;

{$IF NOT DEFINED(CLR)}
  PIconRec = ^TIconRec;
{$ENDIF}
  TIconRec = record
    Width: Byte;
    Height: Byte;
    Colors: Word;
    Reserved1: Word;
    Reserved2: Word;
    DIBSize: Longint;
    DIBOffset: Longint;
  end;

{$IF NOT DEFINED(CLR)}
  {$EXTERNALSYM HMETAFILE}
{$ENDIF}
  HMETAFILE = THandle;
{$IF NOT DEFINED(CLR)}
  {$EXTERNALSYM HENHMETAFILE}
{$ENDIF}
  HENHMETAFILE = THandle;

  EInvalidGraphic = class(Exception);
  EInvalidGraphicOperation = class(Exception);

  TGraphic = class;
  TBitmap = class;
  TIcon = class;
  TMetafile = class;
  TWICImage = class;
  TGraphicClass = class of TGraphic;
  TScaledGraphicDrawer = class;
  TScaledGraphicDrawerClass = class of TScaledGraphicDrawer;


{$IF DEFINED(CLR)}
  TResData = class
  protected
    function GetHandle: THandle; virtual; abstract;
    procedure ClearHandle; virtual; abstract;
  strict protected
    procedure Finalize; override;
  public
    RefCount: Integer;
    destructor Destroy; override;
    function Clone: TResData;
    property Handle: THandle read GetHandle;
  end;
{$ELSE}
  TResData = record
    Handle: THandle;
  end;
{$ENDIF}


  TFontStyle = System.UITypes.TFontStyle;
  {$NODEFINE TFontStyle}
  TFontStyles = System.UITypes.TFontStyles;
  {$NODEFINE TFontStyles}
  TFontName = System.UITypes.TFontName;
  {$NODEFINE TFontName}
  TFontCharset = System.UITypes.TFontCharset;
  {$NODEFINE TFontCharset}
  TFontPitch = System.UITypes.TFontPitch;
  {$NODEFINE TFontPitch}
  TFontQuality = System.UITypes.TFontQuality;
  {$NODEFINE TFontQuality}

  {$HPPEMIT OPENNAMESPACE}
  {$HPPEMIT 'using System::Uitypes::TFontStyle;'}
  {$HPPEMIT 'using System::Uitypes::TFontStyles;'}
  {$HPPEMIT 'using System::Uitypes::TFontName;'}
  {$HPPEMIT 'using System::Uitypes::TFontCharset;'}
  {$HPPEMIT 'using System::Uitypes::TFontPitch;'}
  {$HPPEMIT 'using System::Uitypes::TFontQuality;'}
  {$HPPEMIT CLOSENAMESPACE}



{$IF DEFINED(CLR)}
  TFontData = class(TResData)
  protected
    procedure ClearHandle; override;
    function GetHandle: THandle; override;
  public
    FontHandle: HFont;
    Height: Integer;
    Orientation: Integer;
    Pitch: TFontPitch;
    Style: TFontStyles;
    Charset: TFontCharset;
    Name: TFontDataName;
    Quality: TFontQuality;
    function Clone: TFontData;
    function GetHashCode: Integer; override;
    function Equals(Value: TObject): Boolean; override;
  end;
{$ELSE}
  TFontData = record
    Handle: HFont;
    Height: Integer;
    Orientation: Integer;
    Pitch: TFontPitch;
    Style: TFontStylesBase;
    Charset: TFontCharset;
    Name: TFontDataName;
    Quality: TFontQuality;
  end;
{$ENDIF}

{$IF DEFINED(CLR)}
  TPaletteColoredData = class(TResData)
  protected
    function GetColor: TColor; virtual; abstract;
  public
    property Color: TColor read GetColor;
  end;
{$ENDIF}

  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear,
    psInsideFrame, psUserStyle, psAlternate);
  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
    pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
    pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);

{$IF DEFINED(CLR)}
  TPenData = class(TPaletteColoredData)
  protected
    function GetColor: TColor; override;
    procedure ClearHandle; override;
  public
    PenHandle: HPen;
    Color: TColor;
    Width: Integer;
    Style: TPenStyle;
    function Clone: TPenData;
    function GetHandle: THandle; override;
    function GetHashCode: Integer; override;
    function Equals(Value: TObject): Boolean; override;
  end;
{$ELSE}
  TPenData = record
    Handle: HPen;
    Color: TColor;
    Width: Integer;
    Style: TPenStyle;
  end;
{$ENDIF}

  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical,
    bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);

{$IF DEFINED(CLR)}
  TBrushData = class(TPaletteColoredData)
  protected
    function GetColor: TColor; override;
    procedure ClearHandle; override;
  public
    BrushHandle: HBrush;
    Color: TColor;
    Bitmap: TBitmap;
    Style: TBrushStyle;
    function Clone: TBrushData;
    function GetHandle: THandle; override;
    function GetHashCode: Integer; override;
    function Equals(Value: TObject): Boolean; override;
  end;
{$ELSE}
  TBrushData = record
    Handle: HBrush;
    Color: TColor;
    Bitmap: TBitmap;
    Style: TBrushStyle;
    OwnsBitmap: Boolean;
  end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
  PResource = ^TResource;
  TResource = record
    Next: PResource;
    RefCount: Integer;
    Handle: THandle;
    HashCode: Word;
    Owner: TThreadID;
    case Integer of
      0: (Data: TResData);
      1: (Font: TFontData);
      2: (Pen: TPenData);
      3: (Brush: TBrushData);
  end;
{$ENDIF}

  TGraphicsObject = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
{$IF DEFINED(CLR)}
    FResource: TResData;
    FOwnerLock: TObject;
{$ELSE}
    FResource: PResource;
    FOwnerLock: PRTLCriticalSection;
{$ENDIF}
  protected
    procedure Changed; virtual;
    procedure Lock;
    procedure Unlock;
  public
    function HandleAllocated: Boolean;
{$IF DEFINED(CLR)}
    function GetHashCode: Integer; override;
    property OwnerLock: TObject read FOwnerLock write FOwnerLock;
{$ELSE}
    property OwnerCriticalSection: PRTLCriticalSection read FOwnerLock write FOwnerLock;
{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  IChangeNotifier = interface
    ['{1FB62321-44A7-11D0-9E93-0020AF3D82DA}']
    procedure Changed;
  end;

  TFont = class(TGraphicsObject)
  private
    FColor: TColor;
    FPixelsPerInch: Integer;
    FNotify: IChangeNotifier;
{$IF DEFINED(CLR)}
    function GetFontData: TFontData;
    procedure SetFontData(FontData: TFontData);
{$ELSE}
    procedure GetData(var FontData: TFontData);
    procedure SetData(const FontData: TFontData);
{$ENDIF}
  protected
    procedure Changed; override;
    function GetHandle: HFont;
    function GetHeight: Integer; inline;
    function GetName: TFontName;
    function GetOrientation: Integer;
    function GetPitch: TFontPitch;
    function GetSize: Integer;
    function GetStyle: TFontStyles; inline;
    function GetCharset: TFontCharset;
    function GetQuality: TFontQuality;
    procedure SetColor(const Value: TColor);
    procedure SetHandle(const Value: HFont);
    procedure SetHeight(const Value: Integer);
    procedure SetOrientation(const Value: Integer);
    procedure SetName(const Value: TFontName);
    procedure SetPitch(const Value: TFontPitch);
    procedure SetSize(const Value: Integer);
    procedure SetStyle(const Value: TFontStyles);
    procedure SetCharset(const Value: TFontCharset);
    procedure SetQuality(const Value: TFontQuality);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property FontAdapter: IChangeNotifier read FNotify write FNotify;
    property Handle: HFont read GetHandle write SetHandle;
    property PixelsPerInch: Integer read FPixelsPerInch write FPixelsPerInch;
  published
    property Charset: TFontCharset read GetCharset write SetCharset;
    property Color: TColor read FColor write SetColor;
    property Height: Integer read GetHeight write SetHeight;
    property Name: TFontName read GetName write SetName;
    property Orientation: Integer read GetOrientation write SetOrientation default 0;
    property Pitch: TFontPitch read GetPitch write SetPitch default fpDefault;
    property Size: Integer read GetSize write SetSize stored False;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Quality: TFontQuality read GetQuality write SetQuality default fqDefault;
  end;

  TPen = class(TGraphicsObject)
  private
    FMode: TPenMode;
{$IF DEFINED(CLR)}
    function GetPenData: TPenData;
    procedure SetPenData(PenData: TPenData);
{$ELSE}
    procedure GetData(var PenData: TPenData);
    procedure SetData(const PenData: TPenData);
{$ENDIF}
  protected
    function GetColor: TColor; inline;
    procedure SetColor(Value: TColor);
    function GetHandle: HPen;
    procedure SetHandle(Value: HPen);
    procedure SetMode(Value: TPenMode);
    function GetStyle: TPenStyle;
    procedure SetStyle(Value: TPenStyle);
    function GetWidth: Integer; inline;
    procedure SetWidth(Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Handle: HPen read GetHandle write SetHandle;
  published
    property Color: TColor read GetColor write SetColor default clBlack;
    property Mode: TPenMode read FMode write SetMode default pmCopy;
    property Style: TPenStyle read GetStyle write SetStyle default psSolid;
    property Width: Integer read GetWidth write SetWidth default 1;
  end;

  TBrush = class(TGraphicsObject)
  private
{$IF DEFINED(CLR)}
    function GetBrushData: TBrushData;
    procedure SetBrushData(BrushData: TBrushData);
{$ELSE}
    procedure GetData(var BrushData: TBrushData);
    procedure SetData(const BrushData: TBrushData);
{$ENDIF}
  protected
    function GetBitmap: TBitmap;
    procedure SetBitmap(Value: TBitmap);
    function GetColor: TColor; inline;
    procedure SetColor(Value: TColor);
    function GetHandle: HBrush;
    procedure SetHandle(Value: HBrush);
    function GetStyle: TBrushStyle; inline;
    procedure SetStyle(Value: TBrushStyle);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Handle: HBrush read GetHandle write SetHandle;
  published
    property Color: TColor read GetColor write SetColor default clWhite;
    property Style: TBrushStyle read GetStyle write SetStyle default bsSolid;
  end;

  TFontRecall = class(TRecall)
  public
    constructor Create(AFont: TFont);
  end;

  TPenRecall = class(TRecall)
  public
    constructor Create(APen: TPen);
  end;

  TBrushRecall = class(TRecall)
  public
    constructor Create(ABrush: TBrush);
  end;

{$IF DEFINED(CLR)}
  TResHandleWrapper = class
  private
    FHandle: THandle;
  strict protected
    procedure Finalize; override;
  public
    destructor Destroy; override;
    property Handle: THandle read FHandle write FHandle;
  end;
{$ENDIF}

  TFillStyle = (fsSurface, fsBorder);
  TFillMode = (fmAlternate, fmWinding);

  TCopyMode = Longint;

  TCanvasStates = (csHandleValid, csFontValid, csPenValid, csBrushValid);
  TCanvasState = set of TCanvasStates;
  TCanvasOrientation = (coLeftToRight, coRightToLeft);
  // Note: tfComposited only supported by ThemeServices.DrawText
  TTextFormats = (tfBottom, tfCalcRect, tfCenter, tfEditControl, tfEndEllipsis,
    tfPathEllipsis, tfExpandTabs, tfExternalLeading, tfLeft, tfModifyString,
    tfNoClip, tfNoPrefix, tfRight, tfRtlReading, tfSingleLine, tfTop,
    tfVerticalCenter, tfWordBreak, tfHidePrefix, tfNoFullWidthCharBreak,
    tfPrefixOnly, tfTabStop, tfWordEllipsis, tfComposited);
  TTextFormat = set of TTextFormats;

  TDrawTextFlags = Cardinal;
  TTextFormatFlags = record
  private
    FValue: TTextFormat;
  public
    class operator Implicit(Value: TTextFormat): TTextFormatFlags;
    class operator Implicit(Value: TTextFormatFlags): TTextFormat;
    class operator Implicit(Value: TDrawTextFlags): TTextFormatFlags;
    class operator Implicit(Value: TTextFormatFlags): TDrawTextFlags;
  end;

  TCustomCanvas = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FTextFlags: Longint;
{$IF NOT DEFINED(CLR)}
    FLock: TRTLCriticalSection;
    FLockCount: Integer;
{$ENDIF}
  protected
    function GetCanvasOrientation: TCanvasOrientation; virtual; abstract;
    function GetClipRect: TRect; virtual; abstract;
    function GetPenPos: TPoint; virtual; abstract;
    function GetPixel(X, Y: Integer): TColor; virtual; abstract;
    procedure SetPenPos(Value: TPoint); virtual; abstract;
    procedure SetPixel(X, Y: Integer; Value: TColor); virtual; abstract;
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure RequiredState(ReqState: TCanvasState); virtual; abstract;
  public
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); virtual; abstract;
    procedure ArcTo(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); virtual; abstract;
    procedure AngleArc(X, Y: Integer; Radius: Cardinal; StartAngle, SweepAngle: Single); virtual; abstract;
    procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap;
      const Source: TRect; Color: TColor); virtual; abstract;
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); virtual; abstract;
    procedure Draw(X, Y: Integer; Graphic: TGraphic); overload; virtual; abstract;
    procedure Draw(X, Y: Integer; Graphic: TGraphic; Opacity: Byte); overload; virtual; abstract;
    procedure DrawFocusRect(const Rect: TRect); virtual; abstract;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); overload; virtual; abstract;
    procedure Ellipse(const Rect: TRect); overload;
    procedure FillRect(const Rect: TRect); virtual; abstract;
    procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle); virtual; abstract;
    procedure FrameRect(const Rect: TRect); virtual; abstract;
    procedure LineTo(X, Y: Integer); virtual; abstract;
    procedure Lock;
    procedure MoveTo(X, Y: Integer); virtual; abstract;
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); virtual; abstract;
    procedure Polygon(const Points: array of TPoint); virtual; abstract;
    procedure Polyline(const Points: array of TPoint); virtual; abstract;
    procedure PolyBezier(const Points: array of TPoint); virtual; abstract;
    procedure PolyBezierTo(const Points: array of TPoint); virtual; abstract;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload; virtual; abstract;
    procedure Rectangle(const Rect: TRect); overload;
    procedure Refresh; virtual; abstract;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); overload; virtual; abstract;
    procedure RoundRect(const Rect: TRect; CX, CY: Integer); overload;
    procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); virtual; abstract;
    function TextExtent(const Text: string): TSize; virtual; abstract;
    function TextHeight(const Text: string): Integer;
    procedure TextOut(X, Y: Integer; const Text: string); virtual; abstract;
    procedure TextRect(var Rect: TRect; var Text: string; TextFormat: TTextFormat = []); overload; virtual; abstract;
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string); overload; virtual; abstract;
    function TextWidth(const Text: string): Integer;
    function TryLock: Boolean;
    procedure Unlock;
    property ClipRect: TRect read GetClipRect;
{$IF NOT DEFINED(CLR)}
    property LockCount: Integer read FLockCount;
{$ENDIF}
    property CanvasOrientation: TCanvasOrientation read GetCanvasOrientation;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
    property TextFlags: Longint read FTextFlags write FTextFlags;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  TCanvas = class(TCustomCanvas)
  private
    FHandle: HDC;
    State: TCanvasState;
    FFont: TFont;
    FPen: TPen;
    FBrush: TBrush;
    FPenPos: TPoint;
    FCopyMode: TCopyMode;
    procedure CreateBrush;
    procedure CreateFont;
    procedure CreatePen;
    procedure BrushChanged(ABrush: TObject);
    procedure DeselectHandles;
    procedure FontChanged(AFont: TObject);
    procedure PenChanged(APen: TObject);
  protected
    function GetCanvasOrientation: TCanvasOrientation; override;
    function GetClipRect: TRect; override;
    function GetPenPos: TPoint; override;
    function GetPixel(X, Y: Integer): TColor; override;
    function GetHandle: HDC;
    procedure SetBrush(Value: TBrush);
    procedure SetFont(Value: TFont);
    procedure SetHandle(Value: HDC);
    procedure SetPen(Value: TPen);
    procedure SetPenPos(Value: TPoint); override;
    procedure SetPixel(X, Y: Integer; Value: TColor); override;
    procedure CreateHandle; virtual;
    procedure RequiredState(ReqState: TCanvasState); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
    procedure ArcTo(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
    procedure AngleArc(X, Y: Integer; Radius: Cardinal; StartAngle, SweepAngle: Single); override;
    procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap;
      const Source: TRect; Color: TColor); override;
    procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
    procedure CopyRect(const Dest: TRect; Canvas: TCanvas;
      const Source: TRect);
    procedure Draw(X, Y: Integer; Graphic: TGraphic); overload; override;
    procedure Draw(X, Y: Integer; Graphic: TGraphic; Opacity: Byte); overload; override;
    procedure DrawFocusRect(const Rect: TRect); override;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
    procedure FillRect(const Rect: TRect); override;
    procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle); override;
    procedure FrameRect(const Rect: TRect); override;
    function HandleAllocated: Boolean;
    procedure LineTo(X, Y: Integer); override;
    procedure MoveTo(X, Y: Integer); override;
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); override;
    procedure Polygon(const Points: array of TPoint); override;
    procedure Polyline(const Points: array of TPoint); override;
    procedure PolyBezier(const Points: array of TPoint); override;
    procedure PolyBezierTo(const Points: array of TPoint); override;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); override;
    procedure Refresh; override;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer); override;
    procedure StretchDraw(const Rect: TRect; Graphic: TGraphic); override;
    function TextExtent(const Text: string): TSize; override;
    procedure TextOut(X, Y: Integer; const Text: string); override;
    procedure TextRect(var Rect: TRect; var Text: string; TextFormat: TTextFormat = []); overload; override;
    procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string); overload; override;
    property Handle: HDC read GetHandle write SetHandle;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property CopyMode: TCopyMode read FCopyMode write FCopyMode default cmSrcCopy;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
  end;

  TScaledGraphicDrawer = class(TPersistent)
  private
    FGraphic: TGraphic;
  protected
    function GetInitialized: Boolean; virtual;
    property Graphic: TGraphic read FGraphic;
  public
    constructor Create(AGraphic: TGraphic; AInitialize: Boolean); virtual;
    procedure Initialize; virtual; abstract;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); overload; virtual; abstract;
    property Initialized: Boolean read GetInitialized;
  end;

  { TProgressEvent is a generic progress notification event which may be
        used by TGraphic classes with computationally intensive (slow)
        operations, such as loading, storing, or transforming image data.
    Event params:
      Stage - Indicates whether this call to the OnProgress event is to
        prepare for, process, or clean up after a graphic operation.  If
        OnProgress is called at all, the first call for a graphic operation
        will be with Stage = psStarting, to allow the OnProgress event handler
        to allocate whatever resources it needs to process subsequent progress
        notifications.  After Stage = psStarting, you are guaranteed that
        OnProgress will be called again with Stage = psEnding to allow you
        to free those resources, even if the graphic operation is aborted by
        an exception.  Zero or more calls to OnProgress with Stage = psRunning
        may occur between the psStarting and psEnding calls.
      PercentDone - The ratio of work done to work remaining, on a scale of
        0 to 100.  Values may repeat or even regress (get smaller) in
        successive calls.  PercentDone is usually only a guess, and the
        guess may be dramatically altered as new information is discovered
        in decoding the image.
      RedrawNow - Indicates whether the graphic can be/should be redrawn
        immediately.  Useful for showing successive approximations of
        an image as data is available instead of waiting for all the data
        to arrive before drawing anything.  Since there is no message loop
        activity during graphic operations, you should call Update to force
        a control to be redrawn immediately in the OnProgress event handler.
        Redrawing a graphic when RedrawNow = False could corrupt the image
        and/or cause exceptions.
      Rect - Area of image that has changed and needs to be redrawn.
      Msg - Optional text describing in one or two words what the graphic
        class is currently working on.  Ex:  "Loading" "Storing"
        "Reducing colors".  The Msg string can also be empty.
        Msg strings should be resourced for translation,  should not
        contain trailing periods, and should be used only for
        display purposes.  (do not: if Msg = 'Loading' then...)
  }

  TProgressStage = (psStarting, psRunning, psEnding);
  TProgressEvent = procedure (Sender: TObject; Stage: TProgressStage;
    PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string) of object;

  { The TGraphic class is a abstract base class for dealing with graphic images
    such as metafile, bitmaps, icons, and other image formats.
      LoadFromFile - Read the graphic from the file system.  The old contents of
        the graphic are lost.  If the file is not of the right format, an
        exception will be generated.
      SaveToFile - Writes the graphic to disk in the file provided.
      LoadFromStream - Like LoadFromFile except source is a stream (e.g.
        TBlobStream).
      SaveToStream - stream analogue of SaveToFile.
      LoadFromClipboardFormat - Replaces the current image with the data
        provided.  If the TGraphic does not support that format it will generate
        an exception.
      SaveToClipboardFormats - Converts the image to a clipboard format.  If the
        image does not support being translated into a clipboard format it
        will generate an exception.
      Height - The native, unstretched, height of the graphic.
      Palette - Color palette of image.  Zero if graphic doesn't need/use palettes.
      Transparent - Image does not completely cover its rectangular area
      Width - The native, unstretched, width of the graphic.
      OnChange - Called whenever the graphic changes
      PaletteModified - Indicates in OnChange whether color palette has changed.
        Stays true until whoever's responsible for realizing this new palette
        (ex: TImage) sets it to False.
      OnProgress - Generic progress indicator event. Propagates out to TPicture
        and TImage OnProgress events.}

  TGraphic = class(TInterfacedPersistent, IStreamPersist)
  private
    FOnChange: TNotifyEvent;
    FOnProgress: TProgressEvent;
    FModified: Boolean;
    FTransparent: Boolean;
    FPaletteModified: Boolean;
    FScaledDrawer: TScaledGraphicDrawer;
    procedure SetModified(Value: Boolean);
  protected
    procedure Changed(Sender: TObject); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); virtual; abstract;
    procedure DrawTransparent(ACanvas: TCanvas; const Rect: TRect; Opacity: Byte); virtual;
    function Equals(Graphic: TGraphic): Boolean; reintroduce; overload; virtual;
    function GetEmpty: Boolean; virtual; abstract;
    function GetHeight: Integer; virtual; abstract;
    function GetPalette: HPALETTE; virtual;
    function GetTransparent: Boolean; virtual;
    function GetWidth: Integer; virtual; abstract;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte;  RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    procedure ReadData(Stream: TStream); virtual;
    procedure SetHeight(Value: Integer); virtual; abstract;
    procedure SetPalette(Value: HPALETTE); virtual;
    procedure SetTransparent(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual; abstract;
    procedure WriteData(Stream: TStream); virtual;
    function GetSupportsPartialTransparency: Boolean; virtual;
  public
    constructor Create; overload; virtual;
    destructor Destroy; override;
    function Equals(Obj: TObject): Boolean; overload; override;
    procedure LoadFromFile(const Filename: string); virtual;
    procedure SaveToFile(const Filename: string); virtual;
    class function CanLoadFromStream(Stream: TStream): Boolean; virtual;
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); virtual; abstract;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); virtual; abstract;
    procedure SetSize(AWidth, AHeight: Integer); virtual;

    procedure EnableScaledDrawer(AGraphicScalerClass: TScaledGraphicDrawerClass; AInitialize: Boolean = True); virtual;
    procedure DisableScaledDrawer; virtual;
    procedure UpdateScaledDrawer; virtual;

    property Empty: Boolean read GetEmpty;
    property Height: Integer read GetHeight write SetHeight;
    property Modified: Boolean read FModified write SetModified;
    property Palette: HPALETTE read GetPalette write SetPalette;
    property PaletteModified: Boolean read FPaletteModified write FPaletteModified;
    property Transparent: Boolean read GetTransparent write SetTransparent;
    property Width: Integer read GetWidth write SetWidth;
    property ScaledDrawer: TScaledGraphicDrawer read FScaledDrawer;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property SupportsPartialTransparency: Boolean read GetSupportsPartialTransparency;
  end;

  TGraphicArray = array of TGraphic;

  TFindGraphicClassSource = (gsStream, gsFileName, gsClipboard);
  TFindGraphicClassContext = record
  public
    FSource: TFindGraphicClassSource;
    FFileName: String;
    FClipboardFormat: Word;
    FStream: TStream;
  end;
  TFindGraphicClassEvent = procedure (Sender: TObject;
    const Context: TFindGraphicClassContext; var GraphicClass: TGraphicClass) of object;

  { TPicture }
  { TPicture is a TGraphic container.  It is used in place of a TGraphic if the
    graphic can be of any TGraphic class.  LoadFromFile and SaveToFile are
    polymorphic. For example, if the TPicture is holding an Icon, you can
    LoadFromFile a bitmap file, where if the class was TIcon you could only read
    .ICO files.
      LoadFromFile - Reads a picture from disk.  The TGraphic class created
        determined by the file extension of the file.  If the file extension is
        not recognized an exception is generated.
      SaveToFile - Writes the picture to disk.
      LoadFromClipboardFormat - Reads the picture from the handle provided in
        the given clipboard format.  If the format is not supported, an
        exception is generated.
      SaveToClipboardFormats - Allocates a global handle and writes the picture
        in its native clipboard format (CF_BITMAP for bitmaps, CF_METAFILE
        for metafiles, etc.).  Formats will contain the formats written.
        Returns the number of clipboard items written to the array pointed to
        by Formats and Datas or would be written if either Formats or Datas are
        nil.
      SupportsClipboardFormat - Returns true if the given clipboard format
        is supported by LoadFromClipboardFormat.
      Assign - Copys the contents of the given TPicture.  Used most often in
        the implementation of TPicture properties.
      RegisterFileFormat - Register a new TGraphic class for use in
        LoadFromFile.
      RegisterClipboardFormat - Registers a new TGraphic class for use in
        LoadFromClipboardFormat.
      UnRegisterGraphicClass - Removes all references to the specified TGraphic
        class and all its descendents from the file format and clipboard format
        internal lists.
      Height - The native, unstretched, height of the picture.
      Width - The native, unstretched, width of the picture.
      Graphic - The TGraphic object contained by the TPicture
      Bitmap - Returns a bitmap.  If the contents is not already a bitmap, the
        contents are thrown away and a blank bitmap is returned.
      Icon - Returns an icon.  If the contents is not already an icon, the
        contents are thrown away and a blank icon is returned.
      Metafile - Returns a metafile.  If the contents is not already a metafile,
        the contents are thrown away and a blank metafile is returned. }

  TPicture = class(TInterfacedPersistent, IStreamPersist)
  private type
    TLoadProc = reference to procedure (Graphic: TGraphic);
  private
    FGraphic: TGraphic;
    FOnChange: TNotifyEvent;
    FNotify: IChangeNotifier;
    FOnProgress: TProgressEvent;
    FOnFindGraphicClass: TFindGraphicClassEvent;
    procedure ForceType(GraphicType: TGraphicClass);
    procedure Load(GraphicClass: TGraphicClass; LoadProc: TLoadProc);
    function GetBitmap: TBitmap;
    function GetHeight: Integer;
    function GetIcon: TIcon;
    function GetMetafile: TMetafile;
    function GetWidth: Integer;
    function GetWICImage: TWICImage;
    procedure ReadData(Stream: TStream);
    procedure SetBitmap(Value: TBitmap);
    procedure SetGraphic(Value: TGraphic);
    procedure SetIcon(Value: TIcon);
    procedure SetMetafile(Value: TMetafile);
    procedure SetWICImage(Value: TWICImage);
    procedure WriteData(Stream: TStream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Changed(Sender: TObject); dynamic;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Progress(Sender: TObject; Stage: TProgressStage;
      PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string); dynamic;
    procedure FindGraphicClass(const Context: TFindGraphicClassContext;
      var GraphicClass: TGraphicClass); dynamic;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE);
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE);
    class function SupportsClipboardFormat(AFormat: Word): Boolean;
    procedure Assign(Source: TPersistent); override;
    class procedure RegisterFileFormat(const AExtension, ADescription: string;
      AGraphicClass: TGraphicClass);
    class procedure RegisterClipboardFormat(AFormat: Word;
      AGraphicClass: TGraphicClass);
    class procedure UnregisterGraphicClass(AClass: TGraphicClass);
{$IF DEFINED(CLR)}
    class procedure RegisterFileFormatRes(const AExtension: String;
      ADescriptionResID: string; AGraphicClass: TGraphicClass);
{$ELSE}
    class procedure RegisterFileFormatRes(const AExtension: String;
      ADescriptionResID: Integer; AGraphicClass: TGraphicClass);
{$ENDIF}
    property Bitmap: TBitmap read GetBitmap write SetBitmap;
    property Graphic: TGraphic read FGraphic write SetGraphic;
    property PictureAdapter: IChangeNotifier read FNotify write FNotify;
    property Height: Integer read GetHeight;
    property Icon: TIcon read GetIcon write SetIcon;
    property Metafile: TMetafile read GetMetafile write SetMetafile;
    property WICImage: TWICImage read GetWICImage write SetWICImage;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property OnFindGraphicClass: TFindGraphicClassEvent read FOnFindGraphicClass
      write FOnFindGraphicClass;
  end;

  { TMetafile }
  { TMetafile is an encapsulation of the Win32 Enhanced metafile.
      Handle - The metafile handle.
      Enhanced - determines how the metafile will be stored on disk.
        Enhanced = True (default) stores as EMF (Win32 Enhanced Metafile),
        Enhanced = False stores as WMF (Windows 3.1 Metafile, with Aldus header).
        The in-memory format is always EMF.  WMF has very limited capabilities;
        storing as WMF will lose information that would be retained by EMF.
        This property is set to match the metafile type when loaded from a
        stream or file.  This maintains form file compatibility with 16 bit
        Delphi (If loaded as WMF, then save as WMF).
      Inch - The units per inch assumed by a WMF metafile.  Used to alter
        scale when writing as WMF, but otherwise this property is obsolete.
        Enhanced metafiles maintain complete scale information internally.
      MMWidth,
      MMHeight: Width and Height in 0.01 millimeter units, the native
        scale used by enhanced metafiles.  The Width and Height properties
        are always in screen device pixel units; you can avoid loss of
        precision in converting between device pixels and mm by setting
        or reading the dimentions in mm with these two properties.
      CreatedBy - Optional name of the author or application used to create
        the metafile.
      Description - Optional text description of the metafile.
      You can set the CreatedBy and Description of a new metafile by calling
      TMetafileCanvas.CreateWithComment.

    TMetafileCanvas
      To create a metafile image from scratch, you must draw the image in
      a metafile canvas.  When the canvas is destroyed, it transfers the
      image into the metafile object provided to the canvas constructor.
      After the image is drawn on the canvas and the canvas is destroyed,
      the image is 'playable' in the metafile object.  Like this:

      MyMetafile := TMetafile.Create;
      MyMetafile.Width := 200;
      MyMetafile.Height := 200;
      with TMetafileCanvas.Create(MyMetafile, 0) do
      try
        Brush.Color := clRed;
        Ellipse(0,0,100,100);
        ...
      finally
        Free;
      end;
      Form1.Canvas.Draw(0,0,MyMetafile);  (* 1 red circle  *)

      To add to an existing metafile image, create a metafile canvas
      and play the source metafile into the metafile canvas.  Like this:

      (* continued from previous example, so MyMetafile contains an image *)
      with TMetafileCanvas.Create(MyMetafile, 0) do
      try
        Draw(0,0,MyMetafile);
        Brush.Color := clBlue;
        Ellipse(100,100,200,200);
        ...
      finally
        Free;
      end;
      Form1.Canvas.Draw(0,0,MyMetafile);  (* 1 red circle and 1 blue circle *)
  }

{$IF DEFINED(CLR)}
  TMetafileDC = class(TResHandleWrapper)
  strict protected
    procedure Finalize; override;
  end;
{$ENDIF}

  TMetafileCanvas = class(TCanvas)
  private
    FMetafile: TMetafile;
{$IF DEFINED(CLR)}
    FMetafileDC: TMetafileDC;
{$ENDIF}
  public
    constructor Create(AMetafile: TMetafile; ReferenceDevice: HDC);
    constructor CreateWithComment(AMetafile: TMetafile; ReferenceDevice: HDC;
      const CreatedBy, Description: String);
    destructor Destroy; override;
  end;

  TSharedImage = class
  private
    FRefCount: Integer;
{$IF DEFINED(CLR)}
  strict protected
    procedure Finalize; override;
{$ENDIF}
  protected
    procedure Reference; inline;
    procedure Release;
    procedure FreeHandle; virtual; abstract;
    property RefCount: Integer read FRefCount;
  public
    destructor Destroy; override;
  end;

  TMetafileImage = class(TSharedImage)
  private
    FHandle: HENHMETAFILE;
    FWidth: Integer;      // FWidth and FHeight are in 0.01 mm logical pixels
    FHeight: Integer;     // These are converted to device pixels in TMetafile
    FPalette: HPALETTE;
    FInch: Word;          // Used only when writing WMF files.
    FTempWidth: Integer;  // FTempWidth and FTempHeight are in device pixels
    FTempHeight: Integer; // Used only when width/height are set when FHandle = 0
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

  TMetafile = class(TGraphic)
  private
    FImage: TMetafileImage;
    FEnhanced: Boolean;
    function GetAuthor: String;
    function GetDesc: String;
    function GetHandle: HENHMETAFILE;
    function GetInch: Word;
    function GetMMHeight: Integer;
    function GetMMWidth: Integer;
    procedure NewImage;
    procedure SetHandle(Value: HENHMETAFILE);
    procedure SetInch(Value: Word);
    procedure SetMMHeight(Value: Integer);
    procedure SetMMWidth(Value: Integer);
    procedure UniqueImage;
  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure ReadData(Stream: TStream); override;
    procedure ReadEMFStream(Stream: TStream);
    procedure ReadWMFStream(Stream: TStream; Length: Longint);
    procedure SetHeight(Value: Integer); override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
    class function TestEMF(Stream: TStream): Boolean;
    procedure WriteData(Stream: TStream); override;
    procedure WriteEMFStream(Stream: TStream);
    procedure WriteWMFStream(Stream: TStream);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    function HandleAllocated: Boolean;
    class function CanLoadFromStream(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToFile(const Filename: String); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    procedure Assign(Source: TPersistent); override;
    function ReleaseHandle: HENHMETAFILE;
    procedure SetSize(AWidth, AHeight: Integer); override;
    property CreatedBy: String read GetAuthor;
    property Description: String read GetDesc;
    property Enhanced: Boolean read FEnhanced write FEnhanced default True;
    property Handle: HENHMETAFILE read GetHandle write SetHandle;
    property MMWidth: Integer read GetMMWidth write SetMMWidth;
    property MMHeight: Integer read GetMMHeight write SetMMHeight;
    property Inch: Word read GetInch write SetInch;
  end;

  { TBitmap }
  { TBitmap is an encapsulation of a Windows HBITMAP and HPALETTE.  It manages
    the palette realizing automatically as well as having a Canvas to allow
    modifications to the image.  Creating copies of a TBitmap is very fast
    since the handle is copied not the image.  If the image is modified, and
    the handle is shared by more than one TBitmap object, the image is copied
    before the modification is performed (i.e. copy on write).
      Canvas - Allows drawing on the bitmap.
      Handle - The HBITMAP encapsulated by the TBitmap.  Grabbing the handle
        directly should be avoided since it causes the HBITMAP to be copied if
        more than one TBitmap share the handle.
      Palette - The HPALETTE realized by the TBitmap.  Grabbing this handle
        directly should be avoided since it causes the HPALETTE to be copied if
        more than one TBitmap share the handle.
      Monochrome - True if the bitmap is a monochrome bitmap }

{$IF DEFINED(CLR)}
  TImageFormat = System.Drawing.Imaging.ImageFormat;
{$ENDIF}

  TBitmapImage = class(TSharedImage)
  private
    FHandle: HBITMAP;     // DDB or DIB handle, used for drawing
    FMaskHandle: HBITMAP; // DDB handle
    FPalette: HPALETTE;
    FDIBHandle: HBITMAP;  // DIB handle corresponding to TDIBSection
    FSaveStream: TMemoryStream; // Save original RLE stream until image is modified
    FHalftone: Boolean;   // FPalette is halftone; don't write to file
{$IF DEFINED(CLR)}
    FImageFormat: TImageFormat;
{$ELSE}
    FOS2Format: Boolean;  // Write BMP file header, color table in OS/2 format
{$ENDIF}
  protected
    FDIB: TDIBSection;
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
{$IF DEFINED(CLR)}
    function GetHashCode: Integer; override;
{$ENDIF}
  end;

  TBitmapHandleType = (bmDIB, bmDDB);
  TPixelFormat = (pfDevice, pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit, pfCustom);
  TAlphaFormat = (afIgnored, afDefined, afPremultiplied);
  TTransparentMode = (tmAuto, tmFixed);

  TBitmap = class(TGraphic)
  private
    FCanvas: TCanvas;
    FIgnorePalette: Boolean;
    FMaskBitsValid: Boolean;
    FMaskValid: Boolean;
    FTransparentColor: TColor;
    FTransparentMode: TTransparentMode;
    procedure Changing(Sender: TObject);
    procedure CopyImage(AHandle: HBITMAP; APalette: HPALETTE; DIB: TDIBSection);
    procedure DIBNeeded;
    procedure FreeContext;
    function GetCanvas: TCanvas;
    function GetHandle: HBITMAP; virtual;
    function GetHandleType: TBitmapHandleType;
    function GetMaskHandle: HBITMAP; virtual;
    function GetMonochrome: Boolean;
    function GetPixelFormat: TPixelFormat;
    function GetTransparentColor: TColor;
    procedure ReadStream(Stream: TStream; Size: Longint);
    procedure SetHandle(Value: HBITMAP);
    procedure SetHandleType(Value: TBitmapHandleType); virtual;
    procedure SetMaskHandle(Value: HBITMAP);
    procedure SetMonochrome(Value: Boolean);
    procedure SetPixelFormat(Value: TPixelFormat);
    procedure SetTransparentColor(Value: TColor);
    procedure SetTransparentMode(Value: TTransparentMode);
    function TransparentColorStored: Boolean;
    procedure WriteStream(Stream: TStream; WriteSize: Boolean);
{$IF DEFINED(CLR)}
    function GetImageFormat: TImageFormat;
    function GetScanline(Row: Integer): IntPtr;
    procedure InternalLoadFromBitmap(Bitmap: System.Drawing.Bitmap;
      var BMHandle: HBITMAP; var APalette: HPALETTE; var DIB: TDIBSection);
    procedure NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
      const NewDIB: TDIBSection; NewImageFormat: TImageFormat = nil;
      NewSaveStream: TMemoryStream = nil);
    procedure ReadDIB(Stream: TStream; ImageSize: LongWord);
    procedure SetImageFormat(Value: TImageFormat);
{$ELSE}
    function GetScanline(Row: Integer): Pointer;
    procedure NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
      const NewDIB: TDIBSection; OS2Format: Boolean; RLEStream: TStream = nil);
    procedure ReadDIB(Stream: TStream; ImageSize: LongWord; bmf: PBitmapFileHeader = nil);
{$ENDIF}
    procedure PreMultiplyAlpha;
    procedure UnPreMultiplyAlpha;
    procedure SetAlphaFormat(Value: TAlphaFormat);
  protected
    FImage: TBitmapImage;
    FAlphaFormat: TAlphaFormat;
    function GetSupportsPartialTransparency: Boolean; override;
    procedure Changed(Sender: TObject); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    procedure DrawTransparent(ACanvas: TCanvas; const Rect: TRect; Opacity: Byte); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    procedure HandleNeeded;
    procedure MaskHandleNeeded;
    procedure PaletteNeeded;
    procedure ReadData(Stream: TStream); override;
    procedure SetHeight(Value: Integer); override;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetWidth(Value: Integer); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; overload; override;
    constructor Create(AWidth, AHeight: Integer); overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Dormant;
    procedure FreeImage;
    function HandleAllocated: Boolean;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    class function CanLoadFromStream(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure Mask(TransparentColor: TColor);
    function ReleaseHandle: HBITMAP;
    function ReleaseMaskHandle: HBITMAP;
    function ReleasePalette: HPALETTE;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SetSize(AWidth, AHeight: Integer); override;
{$IF DEFINED(CLR)}
    function GetHashCode: Integer; override;
    procedure LoadFromBitmap(Bitmap: System.Drawing.Bitmap);
    procedure LoadFromResourceName(const ResName, BaseName: String;
      ResourceAssembly: Assembly; Culture: CultureInfo = nil); overload;
    procedure LoadFromResourceName(const ResName, BaseName: String;
      ResourceAssembly: Assembly; ResourceSet: System.Type;
      Culture: CultureInfo = nil); overload;
    procedure LoadFromResourceName(const ResName: String;
      AType: System.Type; Culture: CultureInfo = nil); overload;
    procedure LoadFromResourceName(Instance: THandle;
      const ResName: String); overload; deprecated;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); deprecated;
{$ELSE}
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
{$IFDEF MSWINDOWS}
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
{$ENDIF}
{$ENDIF}
    property Canvas: TCanvas read GetCanvas;
    property Handle: HBITMAP read GetHandle write SetHandle;
    property HandleType: TBitmapHandleType read GetHandleType write SetHandleType;
    property IgnorePalette: Boolean read FIgnorePalette write FIgnorePalette;
    property MaskHandle: HBITMAP read GetMaskHandle write SetMaskHandle;
    property Monochrome: Boolean read GetMonochrome write SetMonochrome;
    property PixelFormat: TPixelFormat read GetPixelFormat write SetPixelFormat;
    property TransparentColor: TColor read GetTransparentColor
      write SetTransparentColor stored TransparentColorStored;
    property TransparentMode: TTransparentMode read FTransparentMode
      write SetTransparentMode default tmAuto;
{$IF DEFINED(CLR)}
    property ScanLine[Row: Integer]: IntPtr read GetScanLine;
    property ImageFormat: TImageFormat read GetImageFormat write SetImageFormat;
{$ELSE}
    property ScanLine[Row: Integer]: Pointer read GetScanLine;
{$ENDIF}
    property AlphaFormat: TAlphaFormat read FAlphaFormat write SetAlphaFormat;
  end;

  { TIcon }
  { TIcon encapsulates window HICON handle. Drawing of an icon does not stretch
    so calling stretch draw is not meaningful.
      Handle - The HICON used by the TIcon. }

  TIconImage = class(TSharedImage)
  private
    FMemoryImage: TCustomMemoryStream;
    FSize: TPoint;
{$IF DEFINED(CLR)}
    FIcon: System.Drawing.Icon;
{$ELSE}
    FHandle: HICON;
{$ENDIF}
  protected
    procedure FreeHandle; override;
  public
    destructor Destroy; override;
  end;

  TIcon = class(TGraphic)
  private
    FImage: TIconImage;
    FRequestedSize: TPoint;
    function GetHandle: HICON;
    procedure HandleNeeded;
    procedure ImageNeeded;
    procedure SetHandle(Value: HICON);
{$IF DEFINED(CLR)}
    procedure NewImage(NewIcon: System.Drawing.Icon; NewImage: TMemoryStream);
{$ELSE}
    procedure NewImage(NewHandle: HICON; NewImage: TMemoryStream);
{$ENDIF}
  protected
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetTransparent(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$IFNDEF CLR}
    procedure AssignTo(Dest: TPersistent); override;
{$ENDIF}
    function HandleAllocated: Boolean;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    class function CanLoadFromStream(Stream: TStream): Boolean; override;
    procedure LoadFromStream(Stream: TStream); override;
    function ReleaseHandle: HICON;
    procedure SaveToClipboardFormat(var Format: Word; var Data: THandle;
      var APalette: HPALETTE); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SetSize(AWidth, AHeight: Integer); override;
{$IF DEFINED(CLR)}
    procedure LoadFromResourceName(const ResName, BaseName: String;
      ResourceAssembly: Assembly; Culture: CultureInfo = nil); overload;
    procedure LoadFromResourceName(const ResName, BaseName: String;
      ResourceAssembly: Assembly; ResourceSet: System.Type;
      Culture: CultureInfo = nil); overload;
    procedure LoadFromResourceName(const ResName: String;
      AType: System.Type; Culture: CultureInfo = nil); overload;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String); overload; deprecated;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer); deprecated;
{$ELSE}
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);
{$IFDEF MSWINDOWS}
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer);
{$ENDIF}
{$ENDIF}
    property Handle: HICON read GetHandle write SetHandle;
  end;

  {TWICImage}
  { TWICImage encapsulates the Microsoft Windows Imaging Component, allowing
    loading image formats that have been registered through WIC. Supports:
    BMP, GIF, ICO, JPEG, PNG, TIFF, and Windows Media Photo.
    Requires Windows XP SP2 with .NET 3.0.}
{$IF NOT DEFINED(CLR)}
  TWICImageFormat = (wifBmp, wifPng, wifJpeg, wifGif, wifTiff, wifWMPhoto, wifOther);
  TWICImageInterpolationMode = (wipmNone, wipmHighQualityCubic, wipmFant,  wipmCubic,  wipmLinear,  wipmNearestNeighbor);

  TWICImage = class (TGraphic)
  private
    FWidth, FHeight: Cardinal;
    FBitmap: TBitmap;
    FData: TMemoryStream;
    FWicBitmap: IWICBitmap;
    FScaledBuffer: TWICImage;

    FFrameCount: LongWord;
    FFrameIndex: LongWord;
    FImageFormat: TWICImageFormat;
    FEncoderContainerFormat: TGUID;
    FFormatChanged: Boolean;
    FInterpolationMode: TWICImageInterpolationMode;

    procedure SetImageFormat(const Value: TWICImageFormat);
    procedure SetInterpolationMode(Value: TWICImageInterpolationMode);
    procedure SetEncoderContainerFormat(const Value: TGUID);
    procedure CreateWicBitmap;
    function GetHandle: IWicBitmap;
    procedure SetHandle(const Value: IWicBitmap);

    class var FImagingFactory: IWICImagingFactory;
    class function GetImagingFactory: IWICImagingFactory; static;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
    function GetEmpty: Boolean; override;
    function GetFrameCount: LongWord;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;
    procedure RequireBitmap;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function CreateScaledCopy(ANewWidth, ANewHeight: Integer; AInterpolationMode: TWICImageInterpolationMode = wipmHighQualityCubic): TWICImage;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
      var APalette: HPALETTE); override;
    procedure LoadFromResourceName(Instance: THandle; const ResName: String);

    property FrameCount: LongWord read GetFrameCount;
    property FrameIndex: LongWord read FFrameIndex write FFrameIndex;
    property Handle: IWicBitmap read GetHandle write SetHandle;
    property ImageFormat: TWICImageFormat  read FImageFormat write SetImageFormat;
    property InterpolationMode: TWICImageInterpolationMode read FInterpolationMode write SetInterpolationMode;
    property EncoderContainerFormat: TGUID  read FEncoderContainerFormat write SetEncoderContainerFormat;

    class property ImagingFactory: IWICImagingFactory read GetImagingFactory;
  end;

  TWICScaledGraphicDrawer = class(TScaledGraphicDrawer)
  private
    FScaledBuffer: TWICImage;
    FInterpolationMode: TWICImageInterpolationMode;
    procedure SetInterpolationMode(AValue: TWICImageInterpolationMode);
  protected
    function GetInitialized: Boolean; override;
  public
    constructor Create(AGraphic: TGraphic; AInitialize: Boolean); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    property InterpolationMode: TWICImageInterpolationMode
      read FInterpolationMode write SetInterpolationMode;
  end;

{$ENDIF}

type
  TGDIHandleRecall = class
  private
    FCanvas: TCanvas;
    FDC: HDC;
    FGDIObj: HGDIOBJ;
  public
    constructor Create(DC: HDC; GDIObject: Cardinal);
    destructor Destroy; override;
    property Canvas: TCanvas read FCanvas;
  end;

var
{$IF DEFINED(CLR)}
  DefFontData: TFontData;
{$ELSE}
  // New TFont instances are intialized with the values in this structure:
  DefFontData: TFontData = (
    Handle: 0;
    Height: 0;
    Orientation: 0; { No rotation }
    Pitch: fpDefault;
    Style: [];
    Charset : DEFAULT_CHARSET;
    Name: 'Segoe UI');

  SystemPalette16: HPalette; // 16 color palette that maps to the system palette
{$ENDIF}

var
  DDBsOnly: Boolean = False; // True = Load all BMPs as device bitmaps.
                             // Not recommended.

function GraphicFilter(GraphicClass: TGraphicClass): string;
function GraphicExtension(GraphicClass: TGraphicClass): string;
function GraphicFileMask(GraphicClass: TGraphicClass): string;

function ColorToRGB(Color: TColor): Longint;
function ColorToString(Color: TColor): string;// inline;
function StringToColor(const S: string): TColor;// inline;
procedure GetColorValues(Proc: TGetStrProc);
function ColorToIdent(Color: Longint; var Ident: string): Boolean;// inline;
function IdentToColor(const Ident: string; var Color: Longint): Boolean;// inline;

procedure GetCharsetValues(Proc: TGetStrProc);
function CharsetToIdent(Charset: Longint; var Ident: string): Boolean;
function IdentToCharset(const Ident: string; var Charset: Longint): Boolean;

procedure GetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD);
{$IF DEFINED(CLR)}
function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; BitmapInfo: IntPtr; var Bits: TBytes): Boolean;
{$ELSE}
function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits): Boolean;
{$ENDIF}

function CopyPalette(Palette: HPALETTE): HPALETTE;

procedure PaletteChanged;
procedure FreeMemoryContexts;

function GetDefFontCharSet: TFontCharSet;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;

{$IF NOT DEFINED(CLR)}
function CreateMappedBmp(Handle: HBITMAP; const OldColors, NewColors: array of TColor): HBITMAP;
function CreateMappedRes(Instance: THandle; ResName: PChar; const OldColors, NewColors: array of TColor): HBITMAP;
function CreateGrayMappedBmp(Handle: HBITMAP): HBITMAP;
function CreateGrayMappedRes(Instance: THandle; ResName: PChar): HBITMAP;
{$ENDIF}

function AllocPatternBitmap(BkColor, FgColor: TColor): TBitmap;

// Alignment must be a power of 2.  Color BMPs require DWORD alignment (32).
function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;

implementation

{ Things left out
  ---------------
  Regions
  PatBlt
  Tabbed text
  Clipping regions
  Coordinate transformations
  Paths
  Beziers }

uses
{$IF DEFINED(CLR)}
  System.Text, System.Collections, System.Threading, System.Resources,
  System.IO, System.Runtime.InteropServices, System.Drawing.Drawing2D,
  Microsoft.Win32, System.Security.Permissions,
  WinUtils, StrUtils,
{$ENDIF}
{$IF NOT DEFINED(CLR)}
  System.Win.Registry,
{$ENDIF}
  System.Types, System.UIConsts, Vcl.Consts, Winapi.ActiveX, System.TypInfo, System.RTLConsts;

const
  csAllValid = [csHandleValid..csBrushValid];
  PNGSignature: array[0..7] of Byte = ($89, $50, $4E, $47, $0D, $0A, $1A, $0A);

var
  ScreenLogPixels: Integer;
  StockPen: HPEN;
  StockBrush: HBRUSH;
  StockFont: HFONT;
  StockIcon: HICON;
{$IF DEFINED(CLR)}
  BitmapImageLock: TObject;
  SystemPalette16: TResHandleWrapper; // 16 color palette that maps to the system palette
{$ELSE}
  BitmapImageLock: TRTLCriticalSection;
  CounterLock: TRTLCriticalSection;
  WicImageLock: TRTLCriticalSection;
{$ENDIF}

procedure InternalDeletePalette(Pal: HPalette);
begin
{$IF DEFINED(CLR)}
  if (Pal <> 0) and Assigned(SystemPalette16) and (THandle(Pal) <> SystemPalette16.Handle) then
{$ELSE}
  if (Pal <> 0) and (Pal <> SystemPalette16) then
{$ENDIF}
    DeleteObject(Pal);
end;

{$IF DEFINED(CLR)}
function CompareMem(P1, P2: TBytes; Size: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Size - 1 do
  begin
    Result := P1[I] = P2[I];
    if not Result then Break;
  end;
end;
{$ENDIF}

{ Resource managers }

{$IF NOT DEFINED(CLR)}
const
  ResInfoSize = SizeOf(TResource) - SizeOf(TFontData);
{$ENDIF}

type
{$IF DEFINED(CLR)}
  TResourceManager = class
  public
    Resources: TAtomicValues;
    constructor Create;
    function EnterResource(ResData: TResData): TResData;
    procedure ChangeResource(GraphicsObject: TGraphicsObject; ResData: TResData);
    procedure FreeResource(ResData: TResData);
    procedure Lock;
    procedure Unlock;
  end;
{$ELSE}
  TResourceManager = class(TObject)
  protected
    procedure FreeObjects(Resource: PResource); virtual;
  public
    ResList: PResource;
    FLock: TRTLCriticalSection;
    ResDataSize: Word;
    constructor Create(AResDataSize: Word);
    destructor Destroy; override;
    function AllocResource(const ResData): PResource;
    procedure FreeResource(Resource: PResource);
    procedure ChangeResource(GraphicsObject: TGraphicsObject; const ResData);
    procedure AssignResource(GraphicsObject: TGraphicsObject;
      AResource: PResource);
    procedure Lock; inline;
    procedure Unlock; inline;
  end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
  TBrushResourceManager = class(TResourceManager)
  protected
    procedure FreeObjects(Resource: PResource); override;
  end;
{$ENDIF}

                                                                                                
{$IF NOT DEFINED(CLR)}
function GetHashCode(const Buffer; Count: Integer): Word; assembler;
{$IFDEF PUREPASCAL}
var
  I: Integer;
  P: PByte;
begin
  P := @Buffer;
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    Result := ((Result and $F800) shr 11) or (Result shl 5);
    Result := Result xor P^;
    Inc(P);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF CLR}

{ TResourceManager }

{$IF DEFINED(CLR)}
constructor TResourceManager.Create;
begin
  inherited Create;
  Resources := TAtomicValues.Create;
end;
{$ELSE}
constructor TResourceManager.Create(AResDataSize: Word);
begin
  ResDataSize := AResDataSize;
  InitializeCriticalSection(FLock);
end;

destructor TResourceManager.Destroy;
begin
  DeleteCriticalSection(FLock);
end;
{$ENDIF}

procedure TResourceManager.Lock;
begin
{$IF DEFINED(CLR)}
  System.Threading.Monitor.Enter(Self);
{$ELSE}
  EnterCriticalSection(FLock);
{$ENDIF}
end;

procedure TResourceManager.Unlock;
begin
{$IF DEFINED(CLR)}
  System.Threading.Monitor.Exit(Self);
{$ELSE}
  LeaveCriticalSection(FLock);
{$ENDIF}
end;

{$IF NOT DEFINED(CLR)}
function TResourceManager.AllocResource(const ResData): PResource;
var
  ResHash: Word;
  LOwner: TThreadID;
begin
  ResHash := Vcl.Graphics.GetHashCode(ResData, ResDataSize);
  Lock;
  try
    LOwner := TThread.CurrentThread.ThreadID;
    Result := ResList;
    while (Result <> nil) and ((Result^.Owner <> LOwner) or (Result^.HashCode <> ResHash) or
      not CompareMem(@Result^.Data, @ResData, ResDataSize)) do
      Result := Result^.Next;
    if Result = nil then
    begin
      GetMem(Result, ResDataSize + ResInfoSize);
      with Result^ do
      begin
        Next := ResList;
        RefCount := 0;
        Handle := TResData(ResData).Handle;
        HashCode := ResHash;
        Owner := LOwner;
        Move(ResData, Data, ResDataSize);
      end;
      ResList := Result;
    end;
    Inc(Result^.RefCount);
  finally
    Unlock;
  end;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TResourceManager.FreeResource(Resource: PResource);
var
  P: PResource;
  DeleteIt: Boolean;
begin
  if Resource <> nil then
    with Resource^ do
    begin
      Lock;
      try
        Dec(RefCount);
        DeleteIt := RefCount = 0;
        if DeleteIt then
        begin
          if Resource = ResList then
            ResList := Resource^.Next
          else
          begin
            P := ResList;
            while P^.Next <> Resource do P := P^.Next;
            P^.Next := Resource^.Next;
          end;
        end;
      finally
        Unlock;
      end;
      if DeleteIt then
      begin  // this is outside the critsect to minimize lock time
        if Handle <> 0 then DeleteObject(Handle);
        FreeObjects(Resource);
        FreeMem(Resource);
      end;
    end;
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TResourceManager.ChangeResource(GraphicsObject: TGraphicsObject; ResData: TResData);
var
  P: TResData;
{$ELSE}
procedure TResourceManager.ChangeResource(GraphicsObject: TGraphicsObject; const ResData);
var
  P: PResource;
{$ENDIF}
begin
  Lock;
  try  // prevent changes to GraphicsObject.FResource pointer between steps
    P := GraphicsObject.FResource;
{$IF DEFINED(CLR)}
    GraphicsObject.FResource := EnterResource(ResData);
    if not GraphicsObject.FResource.Equals(P) then
{$ELSE}
    GraphicsObject.FResource := AllocResource(ResData);
    if GraphicsObject.FResource <> P then
{$ENDIF}
      GraphicsObject.Changed;
    FreeResource(P);
  finally
    Unlock;
  end;
end;

{$IF DEFINED(CLR)}
function TResourceManager.EnterResource(ResData: TResData): TResData;
begin
  Lock;
  try
    Result := Resources.GetAtomicValueOf(ResData) as TResData;
    Inc(Result.RefCount);
  finally
    Unlock;
  end;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TResourceManager.FreeObjects(Resource: PResource);
begin
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TResourceManager.FreeResource(ResData: TResData);
begin
  Lock;
  try
    Dec(ResData.RefCount);
    if ResData.RefCount = 0 then
    begin
      Resources.RemoveAtomicValueOf(ResData);
      ResData.Free; // Free GDI handle
    end;
  finally
    Unlock;
  end;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TResourceManager.AssignResource(GraphicsObject: TGraphicsObject;
  AResource: PResource);
var
  P: PResource;
begin
  Lock;
  try
    P := GraphicsObject.FResource;
    if P <> AResource then
    begin
      if P^.Owner = AResource.Owner then
      begin
        Inc(AResource^.RefCount);
        GraphicsObject.FResource := AResource;
        GraphicsObject.Changed;
        FreeResource(P);
      end
      else
        ChangeResource(GraphicsObject, AResource.Data);
    end;
  finally
    Unlock;
  end;
end;
{$ENDIF}

var
  FontManager: TResourceManager;
  PenManager: TResourceManager;
{$IF DEFINED(CLR)}
  BrushManager: TResourceManager;
{$ELSE}
  BrushManager: TBrushResourceManager;
{$ENDIF}


{ TBrushResourceManager }

{$IF NOT DEFINED(CLR)}
procedure TBrushResourceManager.FreeObjects(Resource: PResource);
begin
  if Resource^.Brush.OwnsBitmap then
    FreeAndNil(Resource^.Brush.Bitmap);
end;
{$ENDIF}

{ TResData }

{$IF DEFINED(CLR)}
destructor TResData.Destroy;
var
  LHandle: THandle;
begin
  System.GC.SuppressFinalize(Self);
  LHandle := Handle;
  if LHandle <> 0 then
  begin
    DeleteObject(LHandle);
    ClearHandle;
  end;
  inherited;
end;

function TResData.Clone: TResData;
begin
  Result := MemberwiseClone as TResData;
  Result.ClearHandle;
  Result.RefCount := 0;
end;

procedure TResData.Finalize;
var
  LHandle: THandle;
begin
  LHandle := Handle;
  if LHandle <> 0 then
    DeleteObject(LHandle);
  inherited;
end;
{$ENDIF}

{ TFontData }

{$IF DEFINED(CLR)}
procedure TFontData.ClearHandle;
begin
  FontHandle := 0;
end;

function TFontData.Clone: TFontData;
begin
  Result := inherited Clone as TFontData;
end;

function TFontData.Equals(Value: TObject): Boolean;
var
  V: TFontData;
begin
  if Value is TFontData then
  begin
    V := TFontData(Value);
    Result := (Height = V.Height) and (Style = V.Style) and
      (CharSet = V.CharSet) and (Name = V.Name) and
      (Orientation = V.Orientation) and
      (Quality = V.Quality);
  end
  else
    Result := False;
end;

function TFontData.GetHandle: THandle;
begin
  Result := THandle(FontHandle);
end;

function TFontData.GetHashCode: Integer;
begin
  Result :=
    Height xor
    (Ord(Pitch) shl 1) xor
    (Integer(Style) shl 2) xor
    (Ord(CharSet) shl 3) xor
    (Ord(Quality) shl 4) xor
    (Orientation shl 5) xor
    System.String(Name).GetHashCode;
end;
{$ENDIF}

{ TPenData }

{$IF DEFINED(CLR)}
procedure TPenData.ClearHandle;
begin
  PenHandle := 0;
end;

function TPenData.Clone: TPenData;
begin
  Result := inherited Clone as TPenData;
end;

function TPenData.Equals(Value: TObject): Boolean;
var
  V: TPenData;
begin
  if Value is TPenData then
  begin
    V := TPenData(Value);
    Result := (Color = V.Color) and (Width = V.Width) and (Style = V.Style);
  end
  else
    Result := False;
end;

function TPenData.GetColor: TColor;
begin
  Result := Color;
end;

function TPenData.GetHandle: THandle;
begin
  Result := THandle(PenHandle);
end;

function TPenData.GetHashCode: Integer;
begin
  Result := Color xor (Width shl 1) xor (Ord(Style) shl 2);
end;
{$ENDIF}

{ TBrushData }

{$IF DEFINED(CLR)}
procedure TBrushData.ClearHandle;
begin
  BrushHandle := 0;
end;

function TBrushData.Clone: TBrushData;
begin
  Result := inherited Clone as TBrushData;
end;

function TBrushData.Equals(Value: TObject): Boolean;
var
  V: TBrushData;
begin
  if Value is TBrushData then
  begin
    V := TBrushData(Value);
    Result := (Bitmap = V.Bitmap) and (Color = V.Color) and
      (Style = V.Style);
  end
  else
    Result := False;
end;

function TBrushData.GetColor: TColor;
begin
  Result := Color;
end;

function TBrushData.GetHandle: THandle;
begin
  Result := THandle(BrushHandle);
end;

function TBrushData.GetHashCode: Integer;
begin
  Result := 0;
  if Assigned(Bitmap) then
    Result := Bitmap.GetHashCode;
  Result := Result xor (Color shl 1) xor (Ord(Style) shl 2);
end;
{$ENDIF}

var
  CanvasList: TThreadList;

procedure PaletteChanged;

{$IF DEFINED(CLR)}
  procedure ClearColor(ResMan: TResourceManager);
  var
    Enumerator: IEnumerator;
    Entry: DictionaryEntry;
    ResData: TPaletteColoredData;
  begin
    ResMan.Lock;
    try
      Enumerator := (ResMan.Resources as IEnumerable).GetEnumerator;
      while Enumerator.MoveNext do
      begin
        Entry := DictionaryEntry(Enumerator.Current);
        ResData := ResMan.Resources.GetAtomicValueFromEntry(Entry) as TPaletteColoredData;
        if (ResData <> nil) and (ResData.Handle <> 0) and (ResData.Color < 0) then
        begin
          DeleteObject(ResData.Handle);
          ResData.ClearHandle;
        end;
      end;
    finally
      ResMan.Unlock;
    end;
  end;
{$ELSE}
  procedure ClearColor(ResMan: TResourceManager);
  var
    Resource: PResource;
  begin
    ResMan.Lock;
    try
      Resource := ResMan.ResList;
      while Resource <> nil do
      begin
        with Resource^ do
        { Assumes Pen.Color and Brush.Color share the same location }
          if (Handle <> 0) and (Pen.Color < 0) then
          begin
            DeleteObject(Handle);
            Handle := 0;
          end;
        Resource := Resource^.Next;
      end;
    finally
      ResMan.Unlock;
    end;
  end;
{$ENDIF}

var
  I,J: Integer;
begin
  { Called when the system palette has changed (WM_SYSCOLORCHANGE) }
  I := 0;
  with CanvasList.LockList do
  try
    while I < Count do
    begin
{$IF DEFINED(CLR)}
      with Items[I] as TCanvas do
{$ELSE}
      with TCanvas(Items[I]) do
{$ENDIF}
      begin
        Lock;
        Inc(I);
        DeselectHandles;
      end;
    end;
    ClearColor(PenManager);
    ClearColor(BrushManager);
  finally
    for J := 0 to I-1 do  // Only unlock the canvases we actually locked
      TCanvas(Items[J]).Unlock;
    CanvasList.UnlockList;
  end;
end;

{ Color mapping routines }

function ColorToRGB(Color: TColor): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF) else
    Result := Color;
end;

function ColorToString(Color: TColor): string;
begin
  Result := System.UIConsts.ColorToString(Color);
end;

function StringToColor(const S: string): TColor;
begin
  Result := System.UIConsts.StringToColor(S);
end;

procedure GetColorValues(Proc: TGetStrProc);
begin
  System.UIConsts.GetColorValues(Proc);
end;

function ColorToIdent(Color: Longint; var Ident: string): Boolean;
begin
  Result := System.UIConsts.ColorToIdent(Color, Ident);
end;

function IdentToColor(const Ident: string; var Color: Longint): Boolean;
begin
  Result := System.UIConsts.IdentToColor(Ident, Color);
end;

{ TGraphicsObject }

procedure TGraphicsObject.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGraphicsObject.Lock;
begin
  if Assigned(FOwnerLock) then
{$IF DEFINED(CLR)}
    System.Threading.Monitor.Enter(FOwnerLock);
{$ELSE}
    EnterCriticalSection(FOwnerLock^);
{$ENDIF}
end;

procedure TGraphicsObject.Unlock;
begin
  if Assigned(FOwnerLock) then
{$IF DEFINED(CLR)}
    System.Threading.Monitor.Exit(FOwnerLock);
{$ELSE}
    LeaveCriticalSection(FOwnerLock^);
{$ENDIF}
end;

{$IF DEFINED(CLR)}
function TGraphicsObject.GetHashCode: Integer;
begin
  Result := inherited GetHashCode;
  if Assigned(FResource) then
    Result := Result xor (FResource.GetHashCode shl 1);
end;
{$ENDIF}

function TGraphicsObject.HandleAllocated: Boolean;
begin
  Result := (FResource <> nil) and (FResource.Handle <> 0);
end;

{ TFont }

const
  FontCharsets: array[0..17] of TIdentMapEntry = (
    (Value: 0; Name: 'ANSI_CHARSET'),
    (Value: 1; Name: 'DEFAULT_CHARSET'),
    (Value: 2; Name: 'SYMBOL_CHARSET'),
    (Value: 77; Name: 'MAC_CHARSET'),
    (Value: 128; Name: 'SHIFTJIS_CHARSET'),
    (Value: 129; Name: 'HANGEUL_CHARSET'),
    (Value: 130; Name: 'JOHAB_CHARSET'),
    (Value: 134; Name: 'GB2312_CHARSET'),
    (Value: 136; Name: 'CHINESEBIG5_CHARSET'),
    (Value: 161; Name: 'GREEK_CHARSET'),
    (Value: 162; Name: 'TURKISH_CHARSET'),
    (Value: 177; Name: 'HEBREW_CHARSET'),
    (Value: 178; Name: 'ARABIC_CHARSET'),
    (Value: 186; Name: 'BALTIC_CHARSET'),
    (Value: 204; Name: 'RUSSIAN_CHARSET'),
    (Value: 222; Name: 'THAI_CHARSET'),
    (Value: 238; Name: 'EASTEUROPE_CHARSET'),
    (Value: 255; Name: 'OEM_CHARSET'));

procedure GetCharsetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(FontCharsets) to High(FontCharsets) do Proc(FontCharsets[I].Name);
end;

function CharsetToIdent(Charset: Longint; var Ident: string): Boolean;
begin
  Result := IntToIdent(Charset, Ident, FontCharsets);
end;

function IdentToCharset(const Ident: string; var Charset: Longint): Boolean;
begin
  Result := IdentToInt(Ident, CharSet, FontCharsets);
end;

{$IF DEFINED(CLR)}
function GetHandleFontData(Font: HFont): TFontData;
{$ELSE}
function GetFontData(Font: HFont): TFontData;
{$ENDIF}
var
  LogFont: TLogFont;
begin
{$IF DEFINED(CLR)}
  Result := DefFontData.Clone;
{$ELSE}
  Result := DefFontData;
{$ENDIF}
  if Font <> 0 then
  begin
{$IF DEFINED(CLR)}
    if GetObject(Font, Marshal.SizeOf(TypeOf(TLogFont)), LogFont) <> 0 then
{$ELSE}
    if GetObject(Font, SizeOf(LogFont), @LogFont) <> 0 then
{$ENDIF}
    with Result, LogFont do
    begin
      Height := lfHeight;
      if lfWeight >= FW_BOLD then
        Include(Style, fsBold);
      if lfItalic = 1 then
        Include(Style, fsItalic);
      if lfUnderline = 1 then
        Include(Style, fsUnderline);
      if lfStrikeOut = 1 then
        Include(Style, fsStrikeOut);
      Charset := TFontCharset(lfCharSet);
{$IF DEFINED(CLR)}
      Name := lfFaceName;
{$ELSE}
      Name := UTF8EncodeToShortString(lfFaceName);
{$ENDIF}
      case lfPitchAndFamily and $F of
        VARIABLE_PITCH: Pitch := fpVariable;
        FIXED_PITCH: Pitch := fpFixed;
      else
        Pitch := fpDefault;
      end;
{$IF DEFINED(CLR)}
      FontHandle := Font;
{$ELSE}
      Handle := Font;
{$ENDIF}
      Orientation := lfOrientation;
    end;
  end;
end;

constructor TFont.Create;
begin
  inherited Create;
{$IF DEFINED(CLR)}
  FResource := FontManager.EnterResource(DefFontData);
{$ELSE}
  DefFontData.Handle := 0;
  FResource := FontManager.AllocResource(DefFontData);
{$ENDIF}
  FColor := clWindowText;
  FPixelsPerInch := ScreenLogPixels;
end;

destructor TFont.Destroy;
begin
{$IF NOT DEFINED(CLR)}
  FontManager.FreeResource(FResource);
{$ENDIF}
  inherited;
end;

procedure TFont.Changed;
begin
  inherited Changed;
  if FNotify <> nil then FNotify.Changed;
end;

procedure TFont.Assign(Source: TPersistent);
{$IF DEFINED(CLR)}
var
  FSource: TFont;
{$ENDIF}
begin
  if Source is TFont then
  begin
    Lock;
    try
{$IF DEFINED(CLR)}
      FSource := Source as TFont;
      FSource.Lock;
      try
        SetFontData(FSource.FResource as TFontData);
        Color := FSource.Color;
        if PixelsPerInch <> FSource.PixelsPerInch then
          Size := FSource.Size;
      finally
        FSource.Unlock;
      end;
{$ELSE}
      TFont(Source).Lock;
      try
        FontManager.AssignResource(Self, TFont(Source).FResource);
        Color := TFont(Source).Color;
        if PixelsPerInch <> TFont(Source).PixelsPerInch then
          Size := TFont(Source).Size;
      finally
        TFont(Source).Unlock;
      end;
{$ENDIF}
    finally
      Unlock;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{$IF DEFINED(CLR)}
function TFont.GetFontData: TFontData;
begin
  Result := (FResource as TFontData).Clone;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TFont.GetData(var FontData: TFontData);
begin
  FontData := FResource^.Font;
  FontData.Handle := 0;
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TFont.SetFontData(FontData: TFontData);
{$ELSE}
procedure TFont.SetData(const FontData: TFontData);
{$ENDIF}
begin
  Lock;
  try
    FontManager.ChangeResource(Self, FontData);
  finally
    Unlock;
  end;
end;

procedure TFont.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

function IsDefaultFont(const FontData: TFontData) : Boolean;
begin
  Result := (DefFontData.Charset = FontData.Charset) and
    (DefFontData.Name = FontData.Name);
end;

function TFont.GetHandle: HFont;
var
  LogFont: TLogFont;
begin
{$IF DEFINED(CLR)}
  with FResource as TFontData do
{$ELSE}
  with FResource^ do
{$ENDIF}
  begin
    if Handle = 0 then
    begin
      FontManager.Lock;
      with LogFont do
      try
        if Handle = 0 then
        begin
{$IF DEFINED(CLR)}
          lfHeight := Height;
          lfWidth := 0; { have font mapper choose }
          lfEscapement := Orientation;
          lfOrientation := Orientation;
          if fsBold in Style then
            lfWeight := FW_BOLD
          else
            lfWeight := FW_NORMAL;
          lfItalic := Byte(fsItalic in Style);
          lfUnderline := Byte(fsUnderline in Style);
          lfStrikeOut := Byte(fsStrikeOut in Style);
          lfCharSet := Byte(Charset);
          if SameText(Name, 'Default') then // do not localize
            lfFaceName := DefFontData.Name
          else
            lfFaceName := Name;
          if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
            (GetDefFontCharset = SHIFTJIS_CHARSET) and IsDefaultFont(FResource as TFontData) then
            lfCharSet := SHIFTJIS_CHARSET;
{$ELSE}
          lfHeight := Font.Height;
          lfWidth := 0; { have font mapper choose }
          lfEscapement := Font.Orientation;
          lfOrientation := Font.Orientation;
          if fsBold in Font.Style then
            lfWeight := FW_BOLD
          else
            lfWeight := FW_NORMAL;
          lfItalic := Byte(fsItalic in Font.Style);
          lfUnderline := Byte(fsUnderline in Font.Style);
          lfStrikeOut := Byte(fsStrikeOut in Font.Style);
          if (Font.CharSet = DEFAULT_CHARSET) and
             (DefFontData.Charset <> DEFAULT_CHARSET) then
            lfCharSet := DefFontData.Charset
          else
            lfCharSet := Byte(Font.Charset);
          if CompareText(string(Font.Name), 'Default') = 0 then  // do not localize
            StrPLCopy(lfFaceName, UTF8ToString(DefFontData.Name), Length(lfFaceName) - 1)
          else
            StrPLCopy(lfFaceName, UTF8ToString(Font.Name), Length(lfFaceName) - 1);
          if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
            (GetDefFontCharset = SHIFTJIS_CHARSET) and IsDefaultFont(Font) then
            lfCharSet := SHIFTJIS_CHARSET;
{$ENDIF}
          lfQuality := Ord(Quality);
          { Everything else as default }
          { Only True Type fonts support the angles }
          if lfOrientation <> 0 then
            lfOutPrecision := OUT_TT_ONLY_PRECIS
          else
           lfOutPrecision := OUT_DEFAULT_PRECIS;
          lfClipPrecision := CLIP_DEFAULT_PRECIS;
          case Pitch of
            fpVariable: lfPitchAndFamily := VARIABLE_PITCH;
            fpFixed: lfPitchAndFamily := FIXED_PITCH;
          else
            lfPitchAndFamily := DEFAULT_PITCH;
          end;
{$IF DEFINED(CLR)}
          FontHandle := CreateFontIndirect(LogFont);
{$ELSE}
          Handle := CreateFontIndirect(LogFont);
{$ENDIF}
        end;
      finally
        FontManager.Unlock;
      end;
    end;
    Result := Handle;
  end;
end;

procedure TFont.SetHandle(const Value: HFont);
begin
{$IF DEFINED(CLR)}
  if Handle <> Value then
    SetFontData(GetHandleFontData(Value));
{$ELSE}
  SetData(GetFontData(Value));
{$ENDIF}
end;

function TFont.GetHeight: Integer;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TFontData).Height;
{$ELSE}
  Result := FResource^.Font.Height;
{$ENDIF}
end;

procedure TFont.SetHeight(const Value: Integer);
var
  FontData: TFontData;
begin
  if Value <> Height then
  begin
{$IF DEFINED(CLR)}
    FontData := GetFontData;
    FontData.Height := Value;
    SetFontData(FontData);
    if FontData.RefCount = 0 then
      FontData.Free;
{$ELSE}
    GetData(FontData);
    FontData.Height := Value;
    SetData(FontData);
{$ENDIF}
  end;
end;

function TFont.GetName: TFontName;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TFontData).Name;
{$ELSE}
  Result := UTF8ToString(FResource^.Font.Name);
{$ENDIF}
end;

procedure TFont.SetName(const Value: TFontName);
var
  FontData: TFontData;
begin
  if Value <> '' then
  begin
{$IF DEFINED(CLR)}
    FontData := GetFontData;
    FontData.Name := Value;
    SetFontData(FontData);
    if FontData.RefCount = 0 then
      FontData.Free;
{$ELSE}
    GetData(FontData);
    // This sizeof is correct.  FontData.Name is a shortstring and theirfor
    // length * sizeof(char) will produce an overwrite.
    FillChar(FontData.Name, SizeOf(FontData.Name), 0);
    FontData.Name := UTF8EncodeToShortString(Value);
    SetData(FontData);
{$ENDIF}
  end;
end;

function TFont.GetSize: Integer;
begin
  Result := -MulDiv(Height, 72, FPixelsPerInch);
end;

procedure TFont.SetSize(const Value: Integer);
begin
  Height := -MulDiv(Value, FPixelsPerInch, 72);
end;

function TFont.GetStyle: TFontStyles;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TFontData).Style;
{$ELSE}
  Result := FResource^.Font.Style;
{$ENDIF}
end;

procedure TFont.SetStyle(const Value: TFontStyles);
var
  FontData: TFontData;
begin
  if Value <> Style then
  begin
{$IF DEFINED(CLR)}
    FontData := GetFontData;
    FontData.Style := Value;
    SetFontData(FontData);
    if FontData.RefCount = 0 then
      FontData.Free;
{$ELSE}
    GetData(FontData);
    FontData.Style := Value;
    SetData(FontData);
{$ENDIF}
  end;
end;

function TFont.GetPitch: TFontPitch;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TFontData).Pitch;
{$ELSE}
  Result := FResource^.Font.Pitch;
{$ENDIF}
end;

function TFont.GetQuality: TFontQuality;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TFontData).Quality;
{$ELSE}
  Result := FResource^.Font.Quality;
{$ENDIF}
end;

procedure TFont.SetPitch(const Value: TFontPitch);
var
  FontData: TFontData;
begin
  if Value <> Pitch then
  begin
{$IF DEFINED(CLR)}
    FontData := GetFontData;
    FontData.Pitch := Value;
    SetFontData(FontData);
    if FontData.RefCount = 0 then
      FontData.Free;
{$ELSE}
    GetData(FontData);
    FontData.Pitch := Value;
    SetData(FontData);
{$ENDIF}
  end;
end;

procedure TFont.SetQuality(const Value: TFontQuality);
var
  FontData: TFontData;
begin
  if Value <> Quality then
  begin
{$IF DEFINED(CLR)}
    FontData := GetFontData;
    FontData.Quality := Value;
    SetFontData(FontData);
    if FontData.RefCount = 0 then
      FontData.Free;
{$ELSE}
    GetData(FontData);
    FontData.Quality := Value;
    SetData(FontData);
{$ENDIF}
  end;
end;

function TFont.GetCharset: TFontCharset;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TFontData).Charset;
{$ELSE}
  Result := FResource^.Font.Charset;
{$ENDIF}
end;

procedure TFont.SetCharset(const Value: TFontCharset);
var
  FontData: TFontData;
begin
  if Value <> CharSet then
  begin
{$IF DEFINED(CLR)}
    FontData := GetFontData;
    FontData.Charset := Value;
    SetFontData(FontData);
    if FontData.RefCount = 0 then
      FontData.Free;
{$ELSE}
    GetData(FontData);
    FontData.Charset := Value;
    SetData(FontData);
{$ENDIF}
  end;
end;

function TFont.GetOrientation: Integer;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TFontData).Orientation;
{$ELSE}
  Result := FResource^.Font.Orientation;
{$ENDIF}
end;

procedure TFont.SetOrientation(const Value: Integer);
var
  FontData: TFontData;
begin
  if Value <> Orientation then
  begin
{$IF DEFINED(CLR)}
    FontData := GetFontData;
    FontData.Orientation := Value;
    SetFontData(FontData);
    if FontData.RefCount = 0 then
      FontData.Free;
{$ELSE}
    GetData(FontData);
    FontData.Orientation := Value;
    SetData(FontData);
{$ENDIF}
  end;
end;

{ TPen }

{$IF DEFINED(CLR)}
function DefPenData: TPenData;
begin
  Result := TPenData.Create;
  Result.Color := clBlack;
  Result.Width := 1;
  Result.Style := psSolid;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
const
  DefPenData: TPenData = (
    Handle: 0;
    Color: clBlack;
    Width: 1;
    Style: psSolid);
{$ENDIF}

{$IF DEFINED(CLR)}
function GetHandlePenData(Pen: HPen): TPenData;
const
  LogPenStyles: array[0..8] of TPenStyle = (psSolid, psDash,
    psDot, psDashDot, psDashDotDot, psClear, psInsideFrame,
    psUserStyle, psAlternate);
var
  LogPen: TLogPen;
  ExtLogPen: TExtLogPen;
  PExtLogPen: IntPtr;
  LPenSize: Integer;
  ExtLogPenSize: Integer;
begin
  Result := DefPenData;
  if Pen <> 0 then
  begin
    LPenSize := GetObject(Pen, 0, nil); // Determine size of structure
    if LPenSize = Marshal.SizeOf(TypeOf(TLogPen)) then // Logical Pen
    begin
      if GetObject(Pen, Marshal.SizeOf(TypeOf(TLogPen)), LogPen) <> 0 then
      with Result, LogPen do
      begin
        Style := LogPenStyles[lopnStyle];
        Width := lopnWidth.X;
        Color := lopnColor;
        PenHandle := Pen;
      end;
    end
    else
    begin
      ExtLogPenSize := Marshal.SizeOf(TypeOf(TExtLogPen));
      if LPenSize >= (ExtLogPenSize - 4) then // Extended Logical Pen
      begin
        if LPenSize > ExtLogPenSize then // With optional extended dash style
        begin
          PExtLogPen := Marshal.AllocHGlobal(LPenSize);
          try
            if GetObject(Pen, LPenSize, PExtLogPen) <> 0 then
            with Result, ExtLogPen do
            begin
              ExtLogPen := TExtLogPen(Marshal.PtrToStructure(PExtLogPen, TypeOf(TExtLogPen)));
              Style := LogPenStyles[elpPenStyle and PS_STYLE_MASK];
              Width := elpWidth;
              Color := elpColor;
              PenHandle := Pen;
            end;
          finally
            Marshal.FreeHGlobal(PExtLogPen);
          end;
        end
        else // Without optional extended dash style
          if GetObject(Pen, Marshal.SizeOf(TypeOf(TExtLogPen)), ExtLogPen) <> 0 then
            with Result, ExtLogPen do
            begin
              Style := LogPenStyles[elpPenStyle and PS_STYLE_MASK];
              Width := elpWidth;
              Color := elpColor;
              PenHandle := Pen;
            end
      end;
    end;
  end;
end;
{$ELSE}
function GetPenData(Pen: HPen): TPenData;
const
  LogPenStyles: array[0..8] of TPenStyle = (psSolid, psDash,
    psDot, psDashDot, psDashDotDot, psClear, psInsideFrame,
    psUserStyle, psAlternate);
  LogPenSize = SizeOf(TLogPen);
  ExtLogPenSize = SizeOf(TExtLogPen);
var
  LogPen: TLogPen;
  ExtLogPen: TExtLogPen;
  PExtLogPen: ^TExtLogPen;
  LPenSize: Integer;
begin
  Result := DefPenData;
  if Pen <> 0 then
  begin
    LPenSize := GetObject(Pen, 0, nil); // Determine size of structure
    if LPenSize = LogPenSize then // Logical Pen
    begin
      if GetObject(Pen, SizeOf(TLogPen), @LogPen) <> 0 then
      with Result, LogPen do
      begin
        Style := LogPenStyles[lopnStyle];
        Width := lopnWidth.X;
        Color := lopnColor;
        Handle := Pen;
      end;
    end
    else
      if LPenSize >= (ExtLogPenSize - 4) then // Extended Logical Pen
      begin
        if LPenSize > ExtLogPenSize then // With optional extended dash style
        begin
          GetMem(Pointer(PExtLogPen), LPenSize);
          try
            if GetObject(Pen, LPenSize, PExtLogPen) <> 0 then
            with Result, PExtLogPen^ do
            begin
              Style := LogPenStyles[elpPenStyle and PS_STYLE_MASK];
              Width := elpWidth;
              Color := elpColor;
              Handle := Pen;
            end;
          finally
            FreeMem(Pointer(PExtLogPen));
          end;
        end
        else // Without optional extended dash style
          if GetObject(Pen, SizeOf(TExtLogPen), @ExtLogPen) <> 0 then
            with Result, ExtLogPen do
            begin
              Style := LogPenStyles[elpPenStyle and PS_STYLE_MASK];
              Width := elpWidth;
              Color := elpColor;
              Handle := Pen;
            end
      end;
  end;
end;
{$ENDIF}

constructor TPen.Create;
begin
  inherited Create;
{$IF DEFINED(CLR)}
  FResource := PenManager.EnterResource(DefPenData);
  FMode := pmCopy;
{$ELSE}
  FResource := PenManager.AllocResource(DefPenData);
  FMode := pmCopy;
{$ENDIF}
end;

destructor TPen.Destroy;
begin
{$IF NOT DEFINED(CLR)}
  PenManager.FreeResource(FResource);
{$ENDIF}
  inherited;
end;

procedure TPen.Assign(Source: TPersistent);
{$IF DEFINED(CLR)}
var
  LSource: TPen;
{$ENDIF}
begin
  if Source is TPen then
  begin
{$IF DEFINED(CLR)}
    LSource := Source as TPen;
    Lock;
    try
      LSource.Lock;
      try
        SetPenData(LSource.FResource as TPenData);
        SetMode(LSource.FMode);
      finally
        LSource.Unlock;
      end;
{$ELSE}
    Lock;
    try
      TPen(Source).Lock;
      try
        PenManager.AssignResource(Self, TPen(Source).FResource);
        SetMode(TPen(Source).FMode);
      finally
        TPen(Source).Unlock;
      end;
{$ENDIF}
    finally
      Unlock;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{$IF DEFINED(CLR)}
function TPen.GetPenData: TPenData;
begin
  Result := (FResource as TPenData).Clone;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TPen.GetData(var PenData: TPenData);
begin
  PenData := FResource^.Pen;
  PenData.Handle := 0;
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TPen.SetPenData(PenData: TPenData);
{$ELSE}
procedure TPen.SetData(const PenData: TPenData);
{$ENDIF}
begin
  Lock;
  try
    PenManager.ChangeResource(Self, PenData);
  finally
    Unlock;
  end;
end;

function TPen.GetColor: TColor;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TPenData).Color;
{$ELSE}
  Result := FResource^.Pen.Color;
{$ENDIF}
end;

procedure TPen.SetColor(Value: TColor);
var
  PenData: TPenData;
begin
  if Value <> Color then
  begin
{$IF DEFINED(CLR)}
    PenData := GetPenData;
    PenData.Color := Value;
    SetPenData(PenData);
    if PenData.RefCount = 0 then
      PenData.Free;
{$ELSE}
    GetData(PenData);
    PenData.Color := Value;
    SetData(PenData);
{$ENDIF}
  end;
end;

function TPen.GetHandle: HPen;
const
  PenStyles: array[TPenStyle] of Word =
    (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
     PS_INSIDEFRAME, PS_USERSTYLE, PS_ALTERNATE);
var
  LogPen: TLogPen;
begin
{$IF DEFINED(CLR)}
  with FResource as TPenData do
  begin
    if PenHandle = 0 then
    begin
      PenManager.Lock;
      with LogPen do
      try
        if PenHandle = 0 then
        begin
          lopnStyle := PenStyles[Style];
          lopnWidth.X := Width;
          lopnColor := ColorToRGB(Color);
          PenHandle := CreatePenIndirect(LogPen);
        end;
      finally
        PenManager.Unlock;
      end;
    end;
    Result := Handle;
  end;
{$ELSE}
  with FResource^ do
  begin
    if Handle = 0 then
    begin
      PenManager.Lock;
      with LogPen do
      try
        if Handle = 0 then
        begin
          lopnStyle := PenStyles[Pen.Style];
          lopnWidth.X := Pen.Width;
          lopnColor := ColorToRGB(Pen.Color);
          Handle := CreatePenIndirect(LogPen);
        end;
      finally
        PenManager.Unlock;
      end;
    end;
    Result := Handle;
  end;
{$ENDIF}
end;

procedure TPen.SetHandle(Value: HPen);
begin
{$IF DEFINED(CLR)}
  if Value <> Handle then
    SetPenData(GetHandlePenData(Value));
{$ELSE}
  SetData(GetPenData(Value));
{$ENDIF}
end;

procedure TPen.SetMode(Value: TPenMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    Changed;
  end;
end;

function TPen.GetStyle: TPenStyle;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TPenData).Style;
{$ELSE}
  Result := FResource^.Pen.Style;
{$ENDIF}
end;

procedure TPen.SetStyle(Value: TPenStyle);
var
  PenData: TPenData;
begin
  if Value <> Style then
  begin
{$IF DEFINED(CLR)}
    PenData := GetPenData;
    PenData.Style := Value;
    SetPenData(PenData);
    if PenData.RefCount = 0 then
      PenData.Free;
{$ELSE}
    GetData(PenData);
    PenData.Style := Value;
    SetData(PenData);
{$ENDIF}
  end;
end;

function TPen.GetWidth: Integer;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TPenData).Width;
{$ELSE}
  Result := FResource^.Pen.Width;
{$ENDIF}
end;

procedure TPen.SetWidth(Value: Integer);
var
  PenData: TPenData;
begin
  if (Value >= 0) and (Value <> Width) then
  begin
{$IF DEFINED(CLR)}
    PenData := GetPenData;
    PenData.Width := Value;
    SetPenData(PenData);
    if PenData.RefCount = 0 then
      PenData.Free;
{$ELSE}
    GetData(PenData);
    PenData.Width := Value;
    SetData(PenData);
{$ENDIF}
  end;
end;

{ TBrush }

{$IF DEFINED(CLR)}
function DefBrushData: TBrushData;
begin
  Result := TBrushData.Create;
  Result.Color := clWhite;
  Result.Style := bsSolid;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
const
  DefBrushData: TBrushData = (
    Handle: 0;
    Color: clWhite;
    Bitmap: nil;
    Style: bsSolid;
    OwnsBitmap: False);
{$ENDIF}

{$IF DEFINED(CLR)}
function GetHandleBrushData(Brush: HBrush): TBrushData;
{$ELSE}
function GetBrushData(Brush: HBrush): TBrushData;
{$ENDIF}
var
  LogBrush: TLogBrush;
begin
  Result := DefBrushData;
  if Brush <> 0 then
  begin
{$IF DEFINED(CLR)}
    if GetObject(Brush, Marshal.SizeOf(TypeOf(TLogBrush)), LogBrush) <> 0 then
{$ELSE}
    if GetObject(Brush, SizeOf(TLogBrush), @LogBrush) <> 0 then
{$ENDIF}
    with Result, LogBrush do
    begin
      case lbStyle of
        BS_SOLID: Style := bsSolid;
        BS_HOLLOW: Style := bsClear;
        BS_PATTERN:
          begin
            if not Assigned(Bitmap) then
            begin
              Bitmap := TBitmap.Create;
{$IF NOT DEFINED(CLR)}
              OwnsBitmap := True;
{$ENDIF}
            end;
            Bitmap.Handle := lbHatch
          end;
      else
        Style := TBrushStyle(lbHatch + Ord(bsHorizontal));
      end;
      Color := lbColor;
{$IF DEFINED(CLR)}
      BrushHandle := Brush;
{$ELSE}
      Handle := Brush;
{$ENDIF}
    end;
  end;
end;

constructor TBrush.Create;
begin
  inherited Create;
{$IF DEFINED(CLR)}
  FResource := BrushManager.EnterResource(DefBrushData);
{$ELSE}
  FResource := BrushManager.AllocResource(DefBrushData);
{$ENDIF}
end;

destructor TBrush.Destroy;
begin
{$IF NOT DEFINED(CLR)}
  BrushManager.FreeResource(FResource);
{$ENDIF}
  inherited;
end;

procedure TBrush.Assign(Source: TPersistent);
{$IF DEFINED(CLR)}
var
  LSource: TBrush;
{$ENDIF}
begin
  if Source is TBrush then
  begin
    Lock;
    try
{$IF DEFINED(CLR)}
      LSource := Source as TBrush;
      LSource.Lock;
      try
        SetBrushData(LSource.FResource as TBrushData);
      finally
        LSource.Unlock;
      end;
{$ELSE}
      TBrush(Source).Lock;
      try
        BrushManager.AssignResource(Self, TBrush(Source).FResource);
      finally
        TBrush(Source).Unlock;
      end;
{$ENDIF}
    finally
      Unlock;
    end;
    Exit;
  end;
  inherited Assign(Source);
end;

{$IF DEFINED(CLR)}
function TBrush.GetBrushData: TBrushData;
begin
  Result := (FResource as TBrushData).Clone;
  Result.Bitmap := nil;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TBrush.GetData(var BrushData: TBrushData);
begin
  BrushData := FResource^.Brush;
  BrushData.Handle := 0;
  BrushData.Bitmap := nil;
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TBrush.SetBrushData(BrushData: TBrushData);
{$ELSE}
procedure TBrush.SetData(const BrushData: TBrushData);
{$ENDIF}
begin
  Lock;
  try
    BrushManager.ChangeResource(Self, BrushData);
  finally
    Unlock;
  end;
end;

function TBrush.GetBitmap: TBitmap;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TBrushData).Bitmap;
{$ELSE}
  Result := FResource^.Brush.Bitmap;
{$ENDIF}
end;

procedure TBrush.SetBitmap(Value: TBitmap);
var
  BrushData: TBrushData;
begin
  BrushData := DefBrushData;
  BrushData.Bitmap := Value;
{$IF DEFINED(CLR)}
  SetBrushData(BrushData);
  if BrushData.RefCount = 0 then
    BrushData.Free;
{$ELSE}
  SetData(BrushData);
{$ENDIF}
end;

function TBrush.GetColor: TColor;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TBrushData).Color;
{$ELSE}
  Result := FResource^.Brush.Color;
{$ENDIF}
end;

procedure TBrush.SetColor(Value: TColor);
var
  BrushData: TBrushData;
begin
  if (Value <> Color) or ((Style = bsClear) and (Style <> bsSolid)) then
  begin
{$IF DEFINED(CLR)}
    BrushData := GetBrushData;
    BrushData.Color := Value;
    if BrushData.Style = bsClear then
      BrushData.Style := bsSolid;
    SetBrushData(BrushData);
    if BrushData.RefCount = 0 then
      BrushData.Free;
{$ELSE}
    GetData(BrushData);
    BrushData.Color := Value;
    if BrushData.Style = bsClear then
      BrushData.Style := bsSolid;
    SetData(BrushData);
{$ENDIF}
  end;
end;

function TBrush.GetHandle: HBrush;
var
  LogBrush: TLogBrush;
begin
{$IF DEFINED(CLR)}
  with FResource as TBrushData do
{$ELSE}
  with FResource^ do
{$ENDIF}
  begin
    if Handle = 0 then
    begin
      BrushManager.Lock;
      try
{$IF DEFINED(CLR)}
        if Handle = 0 then
        begin
          with LogBrush do
          begin
            if Bitmap <> nil then
            begin
              lbStyle := BS_PATTERN;
              Bitmap.HandleType := bmDDB;
              lbHatch := Bitmap.Handle;
            end else
            begin
              lbHatch := 0;
              case Style of
                bsSolid: lbStyle := BS_SOLID;
                bsClear: lbStyle := BS_HOLLOW;
              else
                lbStyle := BS_HATCHED;
                lbHatch := Ord(Style) - Ord(bsHorizontal);
              end;
            end;
            lbColor := ColorToRGB(Color);
          end;
          BrushHandle := CreateBrushIndirect(LogBrush);
        end;
{$ELSE}
        if Handle = 0 then
        begin
          with LogBrush do
          begin
            if Brush.Bitmap <> nil then
            begin
              lbStyle := BS_PATTERN;
              Brush.Bitmap.HandleType := bmDDB;
              lbHatch := Brush.Bitmap.Handle;
            end else
            begin
              lbHatch := 0;
              case Brush.Style of
                bsSolid: lbStyle := BS_SOLID;
                bsClear: lbStyle := BS_HOLLOW;
              else
                lbStyle := BS_HATCHED;
                lbHatch := Ord(Brush.Style) - Ord(bsHorizontal);
              end;
            end;
            lbColor := ColorToRGB(Brush.Color);
          end;
          Handle := CreateBrushIndirect(LogBrush);
        end;
{$ENDIF}
      finally
        BrushManager.Unlock;
      end;
    end;
    Result := Handle;
  end;
end;

procedure TBrush.SetHandle(Value: HBrush);
begin
{$IF DEFINED(CLR)}
  if Value <> Handle then
    SetBrushData(GetHandleBrushData(Value));
{$ELSE}
  SetData(GetBrushData(Value));
{$ENDIF}
end;

function TBrush.GetStyle: TBrushStyle;
begin
{$IF DEFINED(CLR)}
  Result := (FResource as TBrushData).Style;
{$ELSE}
  Result := FResource^.Brush.Style;
{$ENDIF}
end;

procedure TBrush.SetStyle(Value: TBrushStyle);
var
  BrushData: TBrushData;
begin
  if (Value <> Style) or ((Value = bsClear) and (Color <> clWhite)) then
  begin
{$IF DEFINED(CLR)}
    BrushData := GetBrushData;
    BrushData.Style := Value;
    if BrushData.Style = bsClear then
      BrushData.Color := clWhite;
    SetBrushData(BrushData);
    if BrushData.RefCount = 0 then
      BrushData.Free;
{$ELSE}
    GetData(BrushData);
    BrushData.Style := Value;
    if BrushData.Style = bsClear then
      BrushData.Color := clWhite;
    SetData(BrushData);
{$ENDIF}
  end;
end;

{ TFontRecall }

constructor TFontRecall.Create(AFont: TFont);
begin
  inherited Create(TFont.Create, AFont);
end;

{ TPenRecall }

constructor TPenRecall.Create(APen: TPen);
begin
  inherited Create(TPen.Create, APen);
end;

{ TBrushRecall }

constructor TBrushRecall.Create(ABrush: TBrush);
begin
  inherited Create(TBrush.Create, ABrush);
end;

{ TResHandleWrapper }

{$IF DEFINED(CLR)}
destructor TResHandleWrapper.Destroy;
begin
  if FHandle <> 0 then
  begin
    DeleteObject(FHandle);
    FHandle := 0;
  end;
  System.GC.SuppressFinalize(Self);
  inherited;
end;

procedure TResHandleWrapper.Finalize;
begin
  if FHandle <> 0 then
  begin
    DeleteObject(FHandle);
    FHandle := 0;
  end;
  inherited;
end;
{$ENDIF}

{ TCustomCanvas }

procedure TCustomCanvas.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomCanvas.Changing;
begin
  if Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TCustomCanvas.Ellipse(const Rect: TRect);
begin
  Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TCustomCanvas.Lock;
begin
{$IF DEFINED(CLR)}
  System.Threading.Monitor.Enter(Self);
{$ELSE}
  EnterCriticalSection(CounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(CounterLock);
  EnterCriticalSection(FLock);
{$ENDIF}
end;

procedure TCustomCanvas.Rectangle(const Rect: TRect);
begin
  Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TCustomCanvas.RoundRect(const Rect: TRect; CX, CY: Integer);
begin
  RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, CX, CY);
end;

function TCustomCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cY;
end;

function TCustomCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cX;
end;

function TCustomCanvas.TryLock: Boolean;
begin
{$IF DEFINED(CLR)}
  Result := System.Threading.Monitor.TryEnter(Self);
{$ELSE}
  EnterCriticalSection(CounterLock);
  try
    Result := FLockCount = 0;
    if Result then Lock;
  finally
    LeaveCriticalSection(CounterLock);
  end;
{$ENDIF}
end;

procedure TCustomCanvas.Unlock;
begin
{$IF DEFINED(CLR)}
  System.Threading.Monitor.Exit(Self);
{$ELSE}
  LeaveCriticalSection(FLock);
  EnterCriticalSection(CounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(CounterLock);
{$ENDIF}
end;

{ TCanvas }

constructor TCanvas.Create;
begin
  inherited Create;
{$IF DEFINED(CLR)}
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFont.OwnerLock := Self;
  FPen := TPen.Create;
  FPen.OnChange := PenChanged;
  FPen.OwnerLock := Self;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FBrush.OwnerLock := Self;
{$ELSE}
  InitializeCriticalSection(FLock);
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFont.OwnerCriticalSection := @FLock;
  FPen := TPen.Create;
  FPen.OnChange := PenChanged;
  FPen.OwnerCriticalSection := @FLock;
  FBrush := TBrush.Create;
  FBrush.OnChange := BrushChanged;
  FBrush.OwnerCriticalSection := @FLock;
{$ENDIF}
  FCopyMode := cmSrcCopy;
  State := [];
  CanvasList.Add(Self);
end;

destructor TCanvas.Destroy;
begin
  CanvasList.RemoveItem(Self, TList.TDirection.FromEnd);
  SetHandle(0);
{$IF NOT DEFINED(CLR)}
  FFont.Free;
  FPen.Free;
  FBrush.Free;
  DeleteCriticalSection(FLock);
{$ENDIF}
  inherited Destroy;
end;

procedure TCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Winapi.Windows.Arc(FHandle, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  Changed;
end;

procedure TCanvas.ArcTo(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Winapi.Windows.ArcTo(FHandle, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  Changed;
end;

procedure TCanvas.AngleArc(X, Y: Integer; Radius: Cardinal;
  StartAngle, SweepAngle: Single);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Winapi.Windows.AngleArc(FHandle, X, Y, Radius, StartAngle, SweepAngle);
  Changed;
end;

procedure TCanvas.BrushCopy(const Dest: TRect; Bitmap: TBitmap;
  const Source: TRect; Color: TColor);
const
  ROP_DSPDxax = $00E20746;
var
  SrcW, SrcH, DstW, DstH: Integer;
  crBack, crText: TColorRef;
  MaskDC: HDC;
  Mask: TBitmap;
  MaskHandle: HBITMAP;
begin
  if Bitmap = nil then Exit;
  Lock;
  try
    Changing;
    RequiredState([csHandleValid, csBrushValid]);
    Bitmap.Canvas.Lock;
    try
      DstW := Dest.Right - Dest.Left;
      DstH := Dest.Bottom - Dest.Top;
      SrcW := Source.Right - Source.Left;
      SrcH := Source.Bottom - Source.Top;

      if Bitmap.TransparentColor = Color then
      begin
        Mask := nil;
        MaskHandle := Bitmap.MaskHandle;
        MaskDC := CreateCompatibleDC(0);
        MaskHandle := SelectObject(MaskDC, MaskHandle);
      end
      else
      begin
        Mask := TBitmap.Create;
        Mask.Assign(Bitmap);
        { Replace Color with black and all other colors with white }
        Mask.Mask(Color);
        Mask.Canvas.RequiredState([csHandleValid]);
        MaskDC := Mask.Canvas.FHandle;
        MaskHandle := 0;
      end;

      try
        Bitmap.Canvas.RequiredState([csHandleValid]);
        { Draw transparently or use brush color to fill background }
        if Brush.Style = bsClear then
        begin
          TransparentStretchBlt(FHandle, Dest.Left, Dest.Top, DstW, DstH,
            Bitmap.Canvas.FHandle, Source.Left, Source.Top, SrcW, SrcH,
            MaskDC, Source.Left, Source.Top);
        end
        else
        begin
          StretchBlt(FHandle, Dest.Left, Dest.Top, DstW, DstH,
            Bitmap.Canvas.FHandle, Source.Left, Source.Top, SrcW, SrcH, SrcCopy);
          crText := SetTextColor(Self.FHandle, 0);
          crBack := SetBkColor(Self.FHandle, $FFFFFF);
          StretchBlt(Self.FHandle, Dest.Left, Dest.Top, DstW, DstH,
            MaskDC, Source.Left, Source.Top, SrcW, SrcH, ROP_DSPDxax);
          SetTextColor(Self.FHandle, crText);
          SetBkColor(Self.FHandle, crBack);
        end;
      finally
        if Assigned(Mask) then Mask.Free
        else
        begin
          if MaskHandle <> 0 then SelectObject(MaskDC, MaskHandle);
          DeleteDC(MaskDC);
        end;
      end;
    finally
      Bitmap.Canvas.Unlock;
    end;
    Changed;
  finally
    Unlock;
  end;
end;

procedure TCanvas.Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Winapi.Windows.Chord(FHandle, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  Changed;
end;

procedure TCanvas.CopyRect(const Dest: TRect; Canvas: TCanvas;
  const Source: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Canvas.RequiredState([csHandleValid, csBrushValid]);
  StretchBlt(FHandle, Dest.Left, Dest.Top, Dest.Right - Dest.Left,
    Dest.Bottom - Dest.Top, Canvas.FHandle, Source.Left, Source.Top,
    Source.Right - Source.Left, Source.Bottom - Source.Top, CopyMode);
  Changed;
end;

procedure TCanvas.Draw(X, Y: Integer; Graphic: TGraphic);
begin
  if (Graphic <> nil) and not Graphic.Empty then
  begin
    Changing;
    RequiredState([csHandleValid]);
    SetBkColor(FHandle, ColorToRGB(FBrush.Color));
    SetTextColor(FHandle, ColorToRGB(FFont.Color));
    Graphic.Draw(Self, Rect(X, Y, X + Graphic.Width, Y + Graphic.Height));
    Changed;
  end;
end;

procedure TCanvas.Draw(X, Y: Integer; Graphic: TGraphic; Opacity: Byte);
begin
  if (Graphic <> nil) and not Graphic.Empty then
  begin
    Changing;
    RequiredState([csHandleValid]);
    SetBkColor(FHandle, ColorToRGB(FBrush.Color));
    SetTextColor(FHandle, ColorToRGB(FFont.Color));
    Graphic.DrawTransparent(Self, Rect(X, Y, X + Graphic.Width, Y + Graphic.Height), Opacity);
    Changed;
  end;
end;

procedure TCanvas.DrawFocusRect(const Rect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Winapi.Windows.DrawFocusRect(FHandle, Rect);
  Changed;
end;

procedure TCanvas.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Winapi.Windows.Ellipse(FHandle, X1, Y1, X2, Y2);
  Changed;
end;

procedure TCanvas.FillRect(const Rect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Winapi.Windows.FillRect(FHandle, Rect, Brush.GetHandle);
  Changed;
end;

procedure TCanvas.FloodFill(X, Y: Integer; Color: TColor;
  FillStyle: TFillStyle);
const
  FillStyles: array[TFillStyle] of Word = (FLOODFILLSURFACE, FLOODFILLBORDER);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Winapi.Windows.ExtFloodFill(FHandle, X, Y, Color, FillStyles[FillStyle]);
  Changed;
end;

procedure TCanvas.FrameRect(const Rect: TRect);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid]);
  Winapi.Windows.FrameRect(FHandle, Rect, Brush.GetHandle);
  Changed;
end;

function TCanvas.HandleAllocated: Boolean;
begin
  Result := FHandle <> 0;
end;

procedure TCanvas.LineTo(X, Y: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Winapi.Windows.LineTo(FHandle, X, Y);
  Changed;
end;

procedure TCanvas.MoveTo(X, Y: Integer);
begin
  RequiredState([csHandleValid]);
  Winapi.Windows.MoveToEx(FHandle, X, Y, nil);
end;

procedure TCanvas.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
  Winapi.Windows.Pie(FHandle, X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  Changed;
end;

{$IF NOT DEFINED(CLR)}
type
  PPoints = ^TPoints;
  TPoints = array[0..0] of TPoint;
{$ENDIF}

procedure TCanvas.Polygon(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
{$IF DEFINED(CLR)}
  Winapi.Windows.Polygon(FHandle, Points, High(Points) + 1);
{$ELSE}
  Winapi.Windows.Polygon(FHandle, PPoints(@Points)^, High(Points) + 1);
{$ENDIF}
  Changed;
end;

procedure TCanvas.Polyline(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
{$IF DEFINED(CLR)}
  Winapi.Windows.Polyline(FHandle, Points, High(Points) + 1);
{$ELSE}
  Winapi.Windows.Polyline(FHandle, PPoints(@Points)^, High(Points) + 1);
{$ENDIF}
  Changed;
end;

procedure TCanvas.PolyBezier(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
{$IF DEFINED(CLR)}
  Winapi.Windows.PolyBezier(FHandle, Points, High(Points) + 1);
{$ELSE}
  Winapi.Windows.PolyBezier(FHandle, PPoints(@Points)^, High(Points) + 1);
{$ENDIF}
  Changed;
end;

procedure TCanvas.PolyBezierTo(const Points: array of TPoint);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid, csBrushValid]);
{$IF DEFINED(CLR)}
  Winapi.Windows.PolyBezierTo(FHandle, Points, High(Points) + 1);
{$ELSE}
  Winapi.Windows.PolyBezierTo(FHandle, PPoints(@Points)^, High(Points) + 1);
{$ENDIF}
  Changed;
end;

procedure TCanvas.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  Winapi.Windows.Rectangle(FHandle, X1, Y1, X2, Y2);
  Changed;
end;

procedure TCanvas.Refresh;
begin
  DeselectHandles;
end;

procedure TCanvas.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  Changing;
  RequiredState([csHandleValid, csBrushValid, csPenValid]);
  Winapi.Windows.RoundRect(FHandle, X1, Y1, X2, Y2, X3, Y3);
  Changed;
end;

procedure TCanvas.StretchDraw(const Rect: TRect; Graphic: TGraphic);
begin
  if Graphic <> nil then
  begin
    Changing;
    RequiredState(csAllValid);
    if (Graphic.ScaledDrawer <> nil) and Graphic.ScaledDrawer.Initialized then
      Graphic.ScaledDrawer.Draw(Self, Rect)
    else
      Graphic.Draw(Self, Rect);
    Changed;
  end;
end;

function TCanvas.GetCanvasOrientation: TCanvasOrientation;
var
  Point: TPoint;
begin
  Result := coLeftToRight;
  if (FTextFlags and ETO_RTLREADING) <> 0 then
  begin
    GetWindowOrgEx(Handle, Point);
    if Point.X <> 0 then Result := coRightToLeft
  end;
end;

procedure TCanvas.TextOut(X, Y: Integer; const Text: String);
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  if CanvasOrientation = coRightToLeft then Inc(X, TextWidth(Text) + 1);
  Winapi.Windows.ExtTextOut(FHandle, X, Y, FTextFlags, nil, Text,
   Length(Text), nil);
  MoveTo(X + TextWidth(Text), Y);
  Changed;
end;

procedure TCanvas.TextRect(Rect: TRect; X, Y: Integer; const Text: string);
var
  Options: Longint;
begin
  Changing;
  RequiredState([csHandleValid, csFontValid, csBrushValid]);
  Options := ETO_CLIPPED or FTextFlags;
  if Brush.Style <> bsClear then
    Options := Options or ETO_OPAQUE;
  if ((FTextFlags and ETO_RTLREADING) <> 0) and
     (CanvasOrientation = coRightToLeft) then Inc(X, TextWidth(Text) + 1);
  Winapi.Windows.ExtTextOut(FHandle, X, Y, Options, Rect, Text,
    Length(Text), nil);
  Changed;
end;

procedure TCanvas.TextRect(var Rect: TRect; var Text: string;
  TextFormat: TTextFormat = []);
var
  Format: TDrawTextFlags;
{$IF DEFINED(CLR)}
  SB: StringBuilder;
{$ENDIF}
begin
  if tfComposited in TextFormat then
    raise EInvalidOperation.CreateResFmt({$IFNDEF CLR}@{$ENDIF}SInvalidTextFormatFlag,
      [GetEnumName(TypeInfo(TTextFormats), Integer(tfComposited))]);
  Format := TTextFormatFlags(TextFormat);
{$IF DEFINED(CLR)}
  if tfModifyString in TextFormat then
  begin
    SB := StringBuilder.Create(Text, Length(Text) + 4);
    DrawTextEx(Handle, SB, Length(Text), Rect, Format, nil);
    Text := SB.ToString;
  end
  else
    DrawTextEx(Handle, Text, Length(Text), Rect, Format, nil);
{$ELSE}
  if tfModifyString in TextFormat then
    Text := Text + #0#0#0#0;
  DrawTextEx(Handle, PChar(Text), Length(Text), Rect, Format, nil);
  if tfModifyString in TextFormat then
    SetLength(Text, StrLen(PChar(Text)));
{$ENDIF}
end;

function TCanvas.TextExtent(const Text: string): TSize;
begin
  RequiredState([csHandleValid, csFontValid]);
  Result.cX := 0;
  Result.cY := 0;
  Winapi.Windows.GetTextExtentPoint32(FHandle, Text, Length(Text), Result);
end;

procedure TCanvas.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCanvas.SetPen(Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TCanvas.SetBrush(Value: TBrush);
begin
  FBrush.Assign(Value);
end;

function TCanvas.GetPenPos: TPoint;
begin
  RequiredState([csHandleValid]);
  Winapi.Windows.GetCurrentPositionEx(FHandle, {$IFNDEF CLR}@{$ENDIF}Result);
end;

procedure TCanvas.SetPenPos(Value: TPoint);
begin
  MoveTo(Value.X, Value.Y);
end;

function TCanvas.GetPixel(X, Y: Integer): TColor;
begin
  RequiredState([csHandleValid]);
  GetPixel := Winapi.Windows.GetPixel(FHandle, X, Y);
end;

procedure TCanvas.SetPixel(X, Y: Integer; Value: TColor);
begin
  Changing;
  RequiredState([csHandleValid, csPenValid]);
  Winapi.Windows.SetPixel(FHandle, X, Y, ColorToRGB(Value));
  Changed;
end;

function TCanvas.GetClipRect: TRect;
begin
  RequiredState([csHandleValid]);
  GetClipBox(FHandle, Result);
end;

function TCanvas.GetHandle: HDC;
begin
  Changing;
  RequiredState(csAllValid);
  Result := FHandle;
end;

procedure TCanvas.DeselectHandles;
begin
  if (FHandle <> 0) and (State - [csPenValid, csBrushValid, csFontValid] <> State) then
  begin
    SelectObject(FHandle, StockPen);
    SelectObject(FHandle, StockBrush);
    SelectObject(FHandle, StockFont);
    State := State - [csPenValid, csBrushValid, csFontValid];
  end;
end;

procedure TCanvas.CreateHandle;
begin
end;

procedure TCanvas.SetHandle(Value: HDC);
begin
  if FHandle <> Value then
  begin
    if FHandle <> 0 then
    begin
      DeselectHandles;
      FPenPos := GetPenPos;
      FHandle := 0;
      Exclude(State, csHandleValid);
    end;
    if Value <> 0 then
    begin
      Include(State, csHandleValid);
      FHandle := Value;
      SetPenPos(FPenPos);
    end;
  end;
end;

procedure TCanvas.RequiredState(ReqState: TCanvasState);
var
  NeededState: TCanvasState;
begin
  NeededState := ReqState - State;
  if NeededState <> [] then
  begin
    if csHandleValid in NeededState then
    begin
      CreateHandle;
      if FHandle = 0 then
        raise EInvalidOperation.CreateRes({$IFNDEF CLR}@{$ENDIF}SNoCanvasHandle);
    end;
    if csFontValid in NeededState then CreateFont;
    if csPenValid in NeededState then CreatePen;
    if csBrushValid in NeededState then CreateBrush;
    State := State + NeededState;
  end;
end;

procedure TCanvas.CreateFont;
begin
  SelectObject(FHandle, Font.GetHandle);
  SetTextColor(FHandle, ColorToRGB(Font.Color));
end;

procedure TCanvas.CreatePen;
const
  PenModes: array[TPenMode] of Word =
    (R2_BLACK, R2_WHITE, R2_NOP, R2_NOT, R2_COPYPEN, R2_NOTCOPYPEN, R2_MERGEPENNOT,
     R2_MASKPENNOT, R2_MERGENOTPEN, R2_MASKNOTPEN, R2_MERGEPEN, R2_NOTMERGEPEN,
     R2_MASKPEN, R2_NOTMASKPEN, R2_XORPEN, R2_NOTXORPEN);
begin
  SelectObject(FHandle, Pen.GetHandle);
  SetROP2(FHandle, PenModes[Pen.Mode]);
end;

procedure TCanvas.CreateBrush;
begin
  UnrealizeObject(Brush.Handle);
  SelectObject(FHandle, Brush.Handle);
  if Brush.Style = bsSolid then
  begin
    SetBkColor(FHandle, ColorToRGB(Brush.Color));
    SetBkMode(FHandle, OPAQUE);
  end
  else
  begin
    { Win95 doesn't draw brush hatches if bkcolor = brush color }
    { Since bkmode is transparent, nothing should use bkcolor anyway }
    SetBkColor(FHandle, not ColorToRGB(Brush.Color));
    SetBkMode(FHandle, TRANSPARENT);
  end;
end;

procedure TCanvas.FontChanged(AFont: TObject);
begin
  if csFontValid in State then
  begin
    Exclude(State, csFontValid);
    SelectObject(FHandle, StockFont);
  end;
end;

procedure TCanvas.PenChanged(APen: TObject);
begin
  if csPenValid in State then
  begin
    Exclude(State, csPenValid);
    SelectObject(FHandle, StockPen);
  end;
end;

procedure TCanvas.BrushChanged(ABrush: TObject);
begin
  if csBrushValid in State then
  begin
    Exclude(State, csBrushValid);
    SelectObject(FHandle, StockBrush);
  end;
end;

{ Picture support }

{ Metafile types }

const
  WMFKey  = Integer($9AC6CDD7);
  WMFWord = Word($CDD7);

type
{$IF NOT DEFINED(CLR)}
  PMetafileHeader = ^TMetafileHeader;
{$ENDIF}
{$ALIGN 1}
  TMetafileHeader = record
    Key: Longint;
    Handle: SmallInt;
    Box: TSmallRect;
    Inch: Word;
    Reserved: Longint;
    CheckSum: Word;
  end;
{$ALIGN ON}

{ Exception routines }

{$IF DEFINED(CLR)}
procedure InvalidOperation(const Str: string);
begin
  raise EInvalidGraphicOperation.Create(Str);
end;

procedure InvalidGraphic(const Str: string);
begin
  raise EInvalidGraphic.Create(Str);
end;
{$ELSE}
procedure InvalidOperation(Str: PResStringRec);
begin
  raise EInvalidGraphicOperation.CreateRes(Str);
end;

procedure InvalidGraphic(Str: PResStringRec);
begin
  raise EInvalidGraphic.CreateRes(Str);
end;
{$ENDIF}

procedure InvalidBitmap;
begin
  InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SInvalidBitmap);
end;

procedure InvalidIcon;
begin
  InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SInvalidIcon);
end;

procedure InvalidMetafile;
begin
  InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SInvalidMetafile);
end;

procedure OutOfResources;
begin
  raise EOutOfResources.Create(SOutOfResources);
end;

procedure GDIError;
const
  BufSize = 256;
var
  ErrorCode: Integer;
{$IF DEFINED(CLR)}
  Buf: StringBuilder;
{$ELSE}
  Buf: array [Byte] of Char;
{$ENDIF}
begin
{$IF DEFINED(CLR)}
  Buf := StringBuilder.Create(BufSize);
{$ENDIF}
  ErrorCode := GetLastError;
  if (ErrorCode <> 0) and (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil,
    ErrorCode, LOCALE_USER_DEFAULT, Buf, BufSize, nil) <> 0) then
{$IF DEFINED(CLR)}
    raise EOutOfResources.Create(Buf.ToString)
{$ELSE}
    raise EOutOfResources.Create(Buf)
{$ENDIF}
  else
    OutOfResources;
end;

function GDICheck(Value: THandle): THandle;
begin
  if Value = 0 then GDIError;
  Result := Value;
end;

{$IF NOT DEFINED(CLR)}
function DupBits(Src: HBITMAP; Size: TPoint; Mono: Boolean): HBITMAP;
var
  DC, Mem1, Mem2: HDC;
  Old1, Old2: HBITMAP;
  Bitmap: Winapi.Windows.TBitmap;
begin
  Mem1 := CreateCompatibleDC(0);
  Mem2 := CreateCompatibleDC(0);

  try
    GetObject(Src, SizeOf(Bitmap), @Bitmap);
    if Mono then
      Result := CreateBitmap(Size.X, Size.Y, 1, 1, nil)
    else
    begin
      DC := GetDC(0);
      if DC = 0 then GDIError;
      try
        Result := CreateCompatibleBitmap(DC, Size.X, Size.Y);
        if Result = 0 then GDIError;
      finally
        ReleaseDC(0, DC);
      end;
    end;

    if Result <> 0 then
    begin
      Old1 := SelectObject(Mem1, Src);
      Old2 := SelectObject(Mem2, Result);

      StretchBlt(Mem2, 0, 0, Size.X, Size.Y, Mem1, 0, 0, Bitmap.bmWidth,
        Bitmap.bmHeight, SrcCopy);
      if Old1 <> 0 then SelectObject(Mem1, Old1);
      if Old2 <> 0 then SelectObject(Mem2, Old2);
    end;
  finally
    DeleteDC(Mem1);
    DeleteDC(Mem2);
  end;
end;

function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;
{$ENDIF}

function BytesPerScanline(PixelsPerScanline, BitsPerPixel, Alignment: Longint): Longint;
begin
  Dec(Alignment);
  Result := ((PixelsPerScanline * BitsPerPixel) + Alignment) and not Alignment;
  Result := Result div 8;
end;

function TransparentStretchBlt(DstDC: HDC; DstX, DstY, DstW, DstH: Integer;
  SrcDC: HDC; SrcX, SrcY, SrcW, SrcH: Integer; MaskDC: HDC; MaskX,
  MaskY: Integer): Boolean;
const
  ROP_DstCopy = $00AA0029;
var
  MemDC: HDC;
  MemBmp: HBITMAP;
  Save: THandle;
  crText, crBack: TColorRef;
  SavePal: HPALETTE;
begin
  Result := True;
  if (Win32Platform = VER_PLATFORM_WIN32_NT) and (SrcW = DstW) and (SrcH = DstH) then
  begin
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, 1, 1));
    MemBmp := SelectObject(MaskDC, MemBmp);
    try
      MaskBlt(DstDC, DstX, DstY, DstW, DstH, SrcDC, SrcX, SrcY, MemBmp, MaskX,
        MaskY, MakeRop4(ROP_DstCopy, SrcCopy));
    finally
      MemBmp := SelectObject(MaskDC, MemBmp);
      DeleteObject(MemBmp);
    end;
    Exit;
  end;
  SavePal := 0;
  MemDC := GDICheck(CreateCompatibleDC(0));
  try
    MemBmp := GDICheck(CreateCompatibleBitmap(SrcDC, SrcW, SrcH));
    Save := SelectObject(MemDC, MemBmp);
{$IF DEFINED(CLR)}
    SavePal := SelectPalette(SrcDC, SystemPalette16.Handle, False);
{$ELSE}
    SavePal := SelectPalette(SrcDC, SystemPalette16, False);
{$ENDIF}
    SelectPalette(SrcDC, SavePal, False);
    if SavePal <> 0 then
      SavePal := SelectPalette(MemDC, SavePal, True)
    else
{$IF DEFINED(CLR)}
      SavePal := SelectPalette(MemDC, SystemPalette16.Handle, True);
{$ELSE}
      SavePal := SelectPalette(MemDC, SystemPalette16, True);
{$ENDIF}
    RealizePalette(MemDC);

    StretchBlt(MemDC, 0, 0, SrcW, SrcH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcCopy);
    StretchBlt(MemDC, 0, 0, SrcW, SrcH, SrcDC, SrcX, SrcY, SrcW, SrcH, SrcErase);
    crText := SetTextColor(DstDC, $0);
    crBack := SetBkColor(DstDC, $FFFFFF);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MaskDC, MaskX, MaskY, SrcW, SrcH, SrcAnd);
    StretchBlt(DstDC, DstX, DstY, DstW, DstH, MemDC, 0, 0, SrcW, SrcH, SrcInvert);
    SetTextColor(DstDC, crText);
    SetBkColor(DstDC, crBack);

    if Save <> 0 then SelectObject(MemDC, Save);
    DeleteObject(MemBmp);
  finally
    if SavePal <> 0 then SelectPalette(MemDC, SavePal, False);
    DeleteDC(MemDC);
  end;
end;

{$IF NOT DEFINED(CLR)}
type
  PRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array [Byte] of TRGBTriple;
  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array [Byte] of TRGBQuad;

{ RGBTripleToQuad performs in-place conversion of an OS2 color
  table into a DIB color table.   }
procedure RGBTripleToQuad(var ColorTable);
var
  I: Integer;
  P3: PRGBTripleArray;
  P4: PRGBQuadArray;
begin
  P3 := PRGBTripleArray(@ColorTable);
  P4 := Pointer(P3);
  for I := 255 downto 1 do  // don't move zeroth item
    with P4^[I], P3^[I] do
    begin                     // order is significant for last item moved
      rgbRed := rgbtRed;
      rgbGreen := rgbtGreen;
      rgbBlue := rgbtBlue;
      rgbReserved := 0;
    end;
  P4^[0].rgbReserved := 0;
end;

{ RGBQuadToTriple performs the inverse of RGBTripleToQuad. }
procedure RGBQuadToTriple(var ColorTable; var ColorCount: Integer);
var
  I: Integer;
  P3: PRGBTripleArray;
  P4: PRGBQuadArray;
begin
  P3 := PRGBTripleArray(@ColorTable);
  P4 := Pointer(P3);
  for I := 1 to ColorCount-1 do  // don't move zeroth item
    with P4^[I], P3^[I] do
    begin
      rgbtRed := rgbRed;
      rgbtGreen := rgbGreen;
      rgbtBlue := rgbBlue;
    end;
  if ColorCount < 256 then
  begin
    FillChar(P3^[ColorCount], (256 - ColorCount) * SizeOf(TRGBTriple), 0);
    ColorCount := 256;   // OS2 color tables always have 256 entries
  end;
end;
{$ENDIF}


                                                                                            
{$IF DEFINED(CLR)}
procedure ByteSwapColors(var Colors: array of COLORREF; Count: Integer);
var   // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
  I: Integer;
  Color: COLORREF;
begin
  for I := 0 to Count - 1 do
  begin
    Color := Colors[I];

    { Alpha = HiByte(HiWord(Color)) = (Color shr 16) shr 8
      Red   = LoByte(Hiword(Color)) = Byte(Color shr 16)
      Green = HiByte(LoWord(Color)) = Word(Color) shr 8
      Blue  = LoByte(LoWord(Color)) = Byte(Word(Color))
      Colors[I] := MakeLong(MakeWord(Red, Green), MakeWord(Blue, Alpha)); }

    Colors[I] := (Byte(Color shr 16) or (Word(Color) shr 8) shl 8) or
      (Byte(Word(Color)) or ((Color shr 16) shr 8) shl 8) shl 16;
  end;
end;
{$ELSE}
procedure ByteSwapColors(var Colors; Count: Integer);
{$IFDEF PUREPASCAL}
var
  C: PDWORD;
  Color: DWORD;
  I: Integer;
begin
  C := @Colors;
  I := 0;
  while I < Count do
  begin
    Color := C^;
    C^ := (Byte(Color shr 16) or (Word(Color) shr 8) shl 8) or
      (Byte(Word(Color)) shl 16);
    Inc(I);
    Inc(C);
  end;
end;
{$ELSE !PUREPASCAL}
{$IFDEF CPUX86}
var  // convert RGB to BGR and vice-versa.  TRGBQuad <-> TPaletteEntry
  localCPU: Integer;
begin
  localCPU := Test8086;
  asm
        MOV   EDX, Colors
        MOV   ECX, Count
        DEC   ECX
        JS    @@END
        CMP   localCPU, CPUi386
        JLE    @@386
  @@1:  MOV   EAX, [EDX+ECX*4]
        BSWAP EAX
        SHR   EAX,8
        MOV   [EDX+ECX*4],EAX
        DEC   ECX
        JNS   @@1
        JMP   @@END
  @@386:
        PUSH  EBX
  @@2:  XOR   EBX,EBX
        MOV   EAX, [EDX+ECX*4]
        MOV   BH, AL
        MOV   BL, AH
        SHR   EAX,16
        SHL   EBX,8
        MOV   BL, AL
        MOV   [EDX+ECX*4],EBX
        DEC   ECX
        JNS   @@2
        POP   EBX
    @@END:
  end;
end;
{$ENDIF CPUX86}
{$ENDIF !PUREPASCAL}
{$ENDIF}

function CreateSystemPalette(const Entries: array of TColor): HPALETTE;
{$IF DEFINED(CLR)}
var
  DC: HDC;
  SysPalSize: Integer;
  Pal: TMaxLogPalette;
  I: Integer;
  PalEntries: array of ColorRef;
begin
  Pal.palVersion := $300;
  Pal.palNumEntries := 16;
  for I := 0 to 15 do
    Pal.palPalEntry[I] := Entries[I];
  DC := GetDC(0);
  try
    SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
    { Ignore the disk image of the palette for 16 color bitmaps.
      Replace with the first and last 8 colors of the system palette }
    if SysPalSize >= 16 then
    begin
      SetLength(PalEntries, 8);
      GetSystemPaletteEntries(DC, 0, 8, PalEntries);
      System.Array.Copy(PalEntries, Pal.palPalEntry, 8);
      { Is light and dark gray swapped? }
      if TColor(Pal.palPalEntry[7]) = clSilver then
      begin
        GetSystemPaletteEntries(DC, SysPalSize - 7, 7, PalEntries);
        System.Array.Copy(PalEntries, 0, Pal.palPalEntry, Pal.palNumEntries - 7, 7);

        SetLength(PalEntries, 1);
        GetSystemPaletteEntries(DC, SysPalSize - 8, 1, PalEntries);
        Pal.palPalEntry[7] := PalEntries[0];

        GetSystemPaletteEntries(DC, 7, 1, PalEntries);
        Pal.palPalEntry[8] := PalEntries[0];
      end
      else
      begin
        GetSystemPaletteEntries(DC, SysPalSize - 8, 8, PalEntries);
        System.Array.Copy(PalEntries, 0, Pal.palPalEntry, Pal.palNumEntries - 8, 8);
      end;
    end
    else
    begin
    end;
  finally
    ReleaseDC(0,DC);
  end;
  Result := CreatePalette(Pal);
end;
{$ELSE}
var
  DC: HDC;
  SysPalSize: Integer;
  Pal: TMaxLogPalette;
begin
  Pal.palVersion := $300;
  Pal.palNumEntries := 16;
  Move(Entries, Pal.palPalEntry, 16 * SizeOf(TColor));
  DC := GetDC(0);
  try
    SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
    { Ignore the disk image of the palette for 16 color bitmaps.
      Replace with the first and last 8 colors of the system palette }
    if SysPalSize >= 16 then
    begin
      GetSystemPaletteEntries(DC, 0, 8, Pal.palPalEntry);
      { Is light and dark gray swapped? }
      if TColor(Pal.palPalEntry[7]) = clSilver then
      begin
        GetSystemPaletteEntries(DC, SysPalSize - 8, 1, Pal.palPalEntry[7]);
        GetSystemPaletteEntries(DC, SysPalSize - 7, 7, Pal.palPalEntry[Pal.palNumEntries - 7]);
        GetSystemPaletteEntries(DC, 7, 1, Pal.palPalEntry[8]);
      end
      else
        GetSystemPaletteEntries(DC, SysPalSize - 8, 8, Pal.palPalEntry[Pal.palNumEntries - 8]);
    end
    else
    begin
    end;
  finally
    ReleaseDC(0,DC);
  end;
  Result := CreatePalette(PLogPalette(@Pal));
end;
{$ENDIF}

function SystemPaletteOverride(var Pal: TMaxLogPalette): Boolean;
var
  DC: HDC;
  SysPalSize: Integer;
{$IF DEFINED(CLR)}
  PalEntries: array of ColorRef;
{$ENDIF}
begin
  Result := False;
{$IF DEFINED(CLR)}
  if SystemPalette16.Handle <> 0 then
{$ELSE}
  if SystemPalette16 <> 0 then
{$ENDIF}
  begin
    DC := GetDC(0);
    try
      SysPalSize := GetDeviceCaps(DC, SIZEPALETTE);
      if SysPalSize >= 16 then
      begin
        { Ignore the disk image of the palette for 16 color bitmaps.
          Replace with the first and last 8 colors of the system palette }
{$IF DEFINED(CLR)}
        SetLength(PalEntries, 8);
        GetPaletteEntries(SystemPalette16.Handle, 0, 8, PalEntries); //Pal.palPalEntry);
        System.Array.Copy(PalEntries, Pal.palPalEntry, 8);
        GetPaletteEntries(SystemPalette16.Handle, 8, 8, PalEntries); //Pal.palPalEntry[Pal.palNumEntries - 8]);
        System.Array.Copy(PalEntries, 0, Pal.palPalEntry, Pal.palNumEntries - 8, 8);
{$ELSE}
        GetPaletteEntries(SystemPalette16, 0, 8, Pal.palPalEntry);
        GetPaletteEntries(SystemPalette16, 8, 8, Pal.palPalEntry[Pal.palNumEntries - 8]);
{$ENDIF}
        Result := True;
      end
    finally
      ReleaseDC(0,DC);
    end;
  end;
end;

{$IF DEFINED(CLR)}
function PaletteFromDIBColorTable(DIBHandle: THandle;
  const ColorTable: array of COLORREF; ColorCount: Integer): HPalette;
{$ELSE}
function PaletteFromDIBColorTable(DIBHandle: THandle; ColorTable: Pointer;
  ColorCount: Integer): HPalette;
{$ENDIF}
var
  DC: HDC;
  Save: THandle;
  Pal: TMaxLogPalette;
{$IF DEFINED(CLR)}
  I: Integer;
{$ENDIF}
begin
  Result := 0;
  Pal.palVersion := $300;
  if DIBHandle <> 0 then
  begin
    DC := CreateCompatibleDC(0);
    Save := SelectObject(DC, DIBHandle);
    Pal.palNumEntries := GetDIBColorTable(DC, 0, 256, Pal.palPalEntry);
    SelectObject(DC, Save);
    DeleteDC(DC);
  end
  else
  begin
    if ColorCount > 256 then
      InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SInvalidBitmap);
    Pal.palNumEntries := ColorCount;
{$IF DEFINED(CLR)}
    if Length(ColorTable) > 0 then
      for I := 0 to ColorCount - 1 do
        Pal.palPalEntry[I] := ColorTable[I];
{$ELSE}
    Move(ColorTable^, Pal.palPalEntry, ColorCount * 4);
{$ENDIF}
  end;
  if Pal.palNumEntries = 0 then Exit;
  if (Pal.palNumEntries <> 16) or not SystemPaletteOverride(Pal) then
    ByteSwapColors(Pal.palPalEntry, Pal.palNumEntries);
{$IF DEFINED(CLR)}
  Result := CreatePalette(Pal);
{$ELSE}
  Result := CreatePalette(PLogPalette(@Pal)^);
{$ENDIF}
end;

{$IF DEFINED(CLR)}
function PaletteToDIBColorTable(Pal: HPalette;
  var ColorTable: array of COLORREF): Integer;
{$ELSE}
function PaletteToDIBColorTable(Pal: HPalette;
  var ColorTable: array of TRGBQuad): Integer;
{$ENDIF}
begin
  Result := 0;
  if (Pal = 0) or
     (GetObject(Pal, SizeOf(Result), {$IFNDEF CLR}@{$ENDIF}Result) = 0) or
     (Result = 0) then Exit;
  if Result > High(ColorTable) + 1 then
    Result := High(ColorTable) + 1;
  GetPaletteEntries(Pal, 0, Result, ColorTable);
  ByteSwapColors(ColorTable, Result);
end;

{$IF NOT DEFINED(CLR)}
procedure TwoBitsFromDIB(var BI: TBitmapInfoHeader; var XorBits, AndBits: HBITMAP;
  const IconSize: TPoint);
type
  PLongArray = ^TLongArray;
  TLongArray = array[0..1] of Longint;
var
  Temp: HBITMAP;
  NumColors: Integer;
  DC: HDC;
  Bits: Pointer;
  Colors: PLongArray;
begin
  with BI do
  begin
    biHeight := biHeight shr 1; { Size in record is doubled }
    biSizeImage := BytesPerScanline(biWidth, biBitCount, 32) * biHeight;
    NumColors := GetDInColors(biBitCount);
  end;
  DC := GetDC(0);
  if DC = 0 then OutOfResources;
  try
    Bits := Pointer(PByte(@BI) + SizeOf(BI) + NumColors * SizeOf(TRGBQuad));
    Temp := GDICheck(CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS));
    try
      XorBits := DupBits(Temp, IconSize, False);
    finally
      DeleteObject(Temp);
    end;
    with BI do
    begin
      Inc(PByte(Bits), biSizeImage);
      biBitCount := 1;
      biSizeImage := BytesPerScanline(biWidth, biBitCount, 32) * biHeight;
      biClrUsed := 2;
      biClrImportant := 2;
    end;
    Colors := Pointer(PByte(@BI) + SizeOf(BI));
    Colors^[0] := 0;
    Colors^[1] := $FFFFFF;
    Temp := GDICheck(CreateDIBitmap(DC, BI, CBM_INIT, Bits, PBitmapInfo(@BI)^, DIB_RGB_COLORS));
    try
      AndBits := DupBits(Temp, IconSize, True);
    finally
      DeleteObject(Temp);
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure ReadIcon(Stream: TStream; var Icon: HICON; ImageCount: Integer;
  StartOffset: Integer; const RequestedSize: TPoint; var IconSize: TPoint);
type
  PIconRecArray = ^TIconRecArray;
  TIconRecArray = array[0..300] of TIconRec;
var
  List: PIconRecArray;
  HeaderLen, Length: Integer;
  BitsPerPixel: Word;
  Colors, BestColor, C1, N, Index: Integer;
  DC: HDC;
  BI: PBitmapInfoHeader;
  ResData: Pointer;
  XorBits, AndBits: HBITMAP;
  XorInfo, AndInfo: Winapi.Windows.TBitmap;
  XorMem, AndMem: Pointer;
  XorLen, AndLen: Integer;
  WICImage: TWICImage;
  IconBitmap, MaskBitmap: TBitmap;
  IconInfo: TIconInfo;
(*
var
  P: PChar;
begin
  P := Pointer(PByte((Stream as TCustomMemoryStream).Memory) + Stream.Position);
//  N := LookupIconIdFromDirectoryEx(Pointer(P), True, 0, 0, LR_DEFAULTCOLOR);
  Icon := GDICheck(CreateIconFromResourceEx(
    Pointer(P + PIconRec(P)^.DIBOffset - StartOffset),
    PIconRec(P)^.DIBSize, True, $00030000, 0, 0, LR_DEFAULTCOLOR));
end;
*)

  function AdjustColor(I: Integer): Integer; inline;
  begin
    if I = 0 then
      Result := MaxInt
    else
      Result := I;
  end;

  function BetterSize(const Old, New: TIconRec): Boolean;
  var
    NewX, NewY, OldX, OldY: Integer;
  begin
    if New.Width = 0 then
      Newx := 256 - IconSize.X
    else
      NewX := New.Width - IconSize.X;
    if New.Height = 0 then
      NewY := 256 - IconSize.Y
    else
      NewY := New.Height - IconSize.Y;
    if Old.Width = 0 then
      OldX := 256 - IconSize.X
    else
      OldX := Old.Width - IconSize.X;
    if Old.Height = 0 then
      OldY := 256 - IconSize.Y
    else
      OldY := Old.Height - IconSize.Y;
    Result := (Abs(NewX) <= Abs(OldX)) and ((NewX <= 0) or (NewX <= OldX)) and
       (Abs(NewY) <= Abs(OldY)) and ((NewY <= 0) or (NewY <= OldY));
  end;

begin
  Icon := 0;
  HeaderLen := SizeOf(TIconRec) * ImageCount;
  List := AllocMem(HeaderLen);
  try
    Stream.Read(List^, HeaderLen);
    if (RequestedSize.X or RequestedSize.Y) = 0 then
    begin
      IconSize.X := GetSystemMetrics(SM_CXICON);
      IconSize.Y := GetSystemMetrics(SM_CYICON);
    end
    else
      IconSize := RequestedSize;
    DC := GetDC(0);
    if DC = 0 then OutOfResources;
    try
      BitsPerPixel := GetDeviceCaps(DC, PLANES) * GetDeviceCaps(DC, BITSPIXEL);
      if BitsPerPixel > 8 then
        Colors := MaxInt
      else
        Colors := 1 shl BitsPerPixel;
    finally
      ReleaseDC(0, DC);
    end;

    { Find the image that most closely matches (<=) the current screen color
      depth and the requested image size.  }
    Index := 0;
    BestColor := AdjustColor(List^[0].Colors);
    for N := 1 to ImageCount-1 do
    begin
      C1 := AdjustColor(List^[N].Colors);
      if (C1 <= Colors) and (C1 >= BestColor) and
        BetterSize(List^[Index], List^[N]) then
      begin
        Index := N;
        BestColor := C1;
      end;
    end;

    { the following code determines which image most closely matches the
      current device. It is not meant to absolutely match Windows
      (known broken) algorithm }
(*    C2 := 0;
    for N := 0 to ImageCount - 1 do
    begin
      C1 := List^[N].Colors;
      if C1 = Colors then
      begin
        Index := N;
        if (IconSize.X = List^[N].Width) and (IconSize.Y = List^[N].Height) then
          Break;  // exact match on size and color
      end
      else if Index = -1 then
      begin            // take the first icon with fewer colors than screen
        if C1 <= Colors then
        begin
          Index := N;
          C2 := C1;
        end;
      end
      else if C1 > C2 then  // take icon with more colors than first match
        Index := N;
    end;
    if Index = -1 then Index := 0;
*)
    with List^[Index] do
    begin
      if Width = 0 then
        IconSize.X := 256
      else
        IconSize.X := Width;
      if Height = 0 then
        IconSize.Y := 256
      else
        IconSize.Y := Height;
      BI := AllocMem(DIBSize);
      try
        Stream.Seek(DIBOffset  - (HeaderLen + StartOffset), 1);
        Stream.Read(BI^, DIBSize);

        if CompareMem(BI, @PNGSignature[0], 8) then
        begin
          WICImage := TWICImage.Create;
          try
            WICImage.LoadFromStream(Stream);
            IconBitmap := TBitmap.Create;
            try
              IconBitmap.Assign(WICImage);
              MaskBitmap := TBitmap.Create;
              try
                MaskBitmap.Monochrome := True;
                MaskBitmap.Canvas.Brush.Color := clBlack;
                MaskBitmap.SetSize(IconBitmap.Width, IconBitmap.Height);
                FillChar(IconInfo, SizeOf(IconInfo), 0);
                IconInfo.fIcon := True;
                IconInfo.hbmMask := MaskBitmap.Handle;
                IconInfo.hbmColor := IconBitmap.Handle;
                Icon := CreateIconIndirect(IconInfo);
              finally
                MaskBitmap.Free;
              end;
            finally
              IconBitmap.Free;
            end;
          finally
            WICImage.Free;
          end;
        end
        else
        begin
          TwoBitsFromDIB(BI^, XorBits, AndBits, IconSize);
          GetObject(AndBits, SizeOf(Winapi.Windows.TBitmap), @AndInfo);
          GetObject(XorBits, SizeOf(Winapi.Windows.TBitmap), @XorInfo);
          with AndInfo do
            AndLen := bmWidthBytes * bmHeight * bmPlanes;
          with XorInfo do
            XorLen :=  bmWidthBytes * bmHeight * bmPlanes;
          Length := AndLen + XorLen;
          ResData := AllocMem(Length);
          try
            AndMem := ResData;
            with AndInfo do
              XorMem := Pointer(PByte(ResData) + AndLen);
            GetBitmapBits(AndBits, AndLen, AndMem);
            GetBitmapBits(XorBits, XorLen, XorMem);
            DeleteObject(XorBits);
            DeleteObject(AndBits);
            Icon := CreateIcon(HInstance, IconSize.X, IconSize.Y,
              XorInfo.bmPlanes, XorInfo.bmBitsPixel, AndMem, XorMem);
          finally
            FreeMem(ResData, Length);
          end;
        end;
      finally
        FreeMem(BI, DIBSize);
      end;
    end;
  finally
    FreeMem(List, HeaderLen);
  end;
  if Icon = 0 then
    GDIError;
end;
{$ENDIF}

function ComputeAldusChecksum(var WMF: TMetafileHeader): Word;
begin
  Result := 0;
  with WMF, Box do
  begin
    Result := Result xor Word(Key);
    Result := Result xor HiWord(Key);
    Result := Result xor Word(Handle);
    Result := Result xor Word(Left);
    Result := Result xor Word(Top);
    Result := Result xor Word(Right);
    Result := Result xor Word(Bottom);
    Result := Result xor Inch;
    Result := Result xor Word(Reserved);
    Result := Result xor HiWord(Reserved);
  end;
end;

procedure InitializeBitmapInfoHeader(Bitmap: HBITMAP; var BI: TBitmapInfoHeader;
  Colors: Integer);
var
  DS: TDIBSection;
  Bytes: Integer;
begin
  DS.dsbmih.biSize := 0;
{$IF DEFINED(CLR)}
  Bytes := GetObject(Bitmap, Marshal.SizeOf(TypeOf(DS)), DS);
{$ELSE}
  Bytes := GetObject(Bitmap, SizeOf(DS), @DS);
{$ENDIF}
  if Bytes = 0 then InvalidBitmap
  else if (Bytes >= (SizeOf(DS.dsbm) + SizeOf(DS.dsbmih))) and
    (DS.dsbmih.biSize >= DWORD(SizeOf(DS.dsbmih))) then
    BI := DS.dsbmih
  else
  begin
{$IF NOT DEFINED(CLR)}
    FillChar(BI, SizeOf(BI), 0);
{$ENDIF}
    with BI, DS.dsbm do
    begin
      biSize := SizeOf(BI);
      biWidth := bmWidth;
      biHeight := bmHeight;
      // Emulate FillChar(BI, SizeOf(BI), 0)
{$IF DEFINED(CLR)}
      biPlanes := 0;
      biBitCount := 0;
      biCompression := 0;
      biSizeImage := 0;
      biXPelsPerMeter := 0;
      biYPelsPerMeter := 0;
      biClrUsed := 0;
      biClrImportant := 0;
{$ENDIF}
    end;
  end;
  case Colors of
    2: BI.biBitCount := 1;
    3..16:
      begin
        BI.biBitCount := 4;
        BI.biClrUsed := Colors;
      end;
    17..256:
      begin
        BI.biBitCount := 8;
        BI.biClrUsed := Colors;
      end;
  else
    BI.biBitCount := DS.dsbm.bmBitsPixel * DS.dsbm.bmPlanes;
  end;
  BI.biPlanes := 1;
  if BI.biClrImportant > BI.biClrUsed then
    BI.biClrImportant := BI.biClrUsed;
  if BI.biSizeImage = 0 then
    BI.biSizeImage := BytesPerScanLine(BI.biWidth, BI.biBitCount, 32) * Abs(BI.biHeight);
end;

procedure InternalGetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD; Colors: Integer);
var
  BI: TBitmapInfoHeader;
begin
  InitializeBitmapInfoHeader(Bitmap, BI, Colors);
  if BI.biBitCount > 8 then
  begin
    InfoHeaderSize := SizeOf(TBitmapInfoHeader);
    if (BI.biCompression and BI_BITFIELDS) <> 0 then
      Inc(InfoHeaderSize, 12);
  end
  else
    if BI.biClrUsed = 0 then
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * (1 shl BI.biBitCount)
    else
      InfoHeaderSize := SizeOf(TBitmapInfoHeader) +
        SizeOf(TRGBQuad) * BI.biClrUsed;
  ImageSize := BI.biSizeImage;
end;

procedure GetDIBSizes(Bitmap: HBITMAP; var InfoHeaderSize: DWORD;
  var ImageSize: DWORD);
begin
  InternalGetDIBSizes(Bitmap, InfoHeaderSize, ImageSize, 0);
end;

{$IF DEFINED(CLR)}
function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  BitmapInfoPtr: IntPtr; var Bits: TBytes; Colors: Integer): Boolean;
{$ELSE}
function InternalGetDIB(Bitmap: HBITMAP; Palette: HPALETTE;
  var BitmapInfo; var Bits; Colors: Integer): Boolean;
{$ENDIF}
var
  OldPal: HPALETTE;
  DC: HDC;
{$IF DEFINED(CLR)}
  BitmapInfo: TBitmapInfoHeader;
{$ENDIF}
begin
{$IF DEFINED(CLR)}
  BitmapInfo := TBitmapInfoHeader(Marshal.PtrToStructure(BitmapInfoPtr,
    TypeOf(TBitmapInfoHeader)));
{$ENDIF}
  InitializeBitmapInfoHeader(Bitmap, TBitmapInfoHeader(BitmapInfo), Colors);
  OldPal := 0;
  DC := CreateCompatibleDC(0);
  try
    if Palette <> 0 then
    begin
      OldPal := SelectPalette(DC, Palette, False);
      RealizePalette(DC);
    end;
{$IF DEFINED(CLR)}
    Marshal.StructureToPtr(TObject(BitmapInfo), BitmapInfoPtr, True);
    Result := GetDIBits(DC, Bitmap, 0, BitmapInfo.biHeight, Bits,
      BitmapInfoPtr, DIB_RGB_COLORS) <> 0;
{$ELSE}
    Result := GetDIBits(DC, Bitmap, 0, TBitmapInfoHeader(BitmapInfo).biHeight, @Bits,
      TBitmapInfo(BitmapInfo), DIB_RGB_COLORS) <> 0;
{$ENDIF}
  finally
    if OldPal <> 0 then SelectPalette(DC, OldPal, False);
    DeleteDC(DC);
  end;
end;

{$IF DEFINED(CLR)}
function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; BitmapInfo: IntPtr;
  var Bits: TBytes): Boolean;
{$ELSE}
function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; var BitmapInfo; var Bits): Boolean;
{$ENDIF}
begin
  Result := InternalGetDIB(Bitmap, Palette, BitmapInfo, Bits, 0);
end;

procedure WinError;
begin
end;

procedure CheckBool(Result: Bool);
begin
  if not Result then WinError;
end;

{$IF NOT DEFINED(CLR)}
procedure WriteIcon(Stream: TStream; Icon: HICON; WriteLength: Boolean);
var
  IconInfo: TIconInfo;
  MonoInfoSize, ColorInfoSize: DWORD;
  MonoBitsSize, ColorBitsSize: DWORD;
  MonoInfo, MonoBits, ColorInfo, ColorBits: Pointer;
  CI: TCursorOrIcon;
  List: TIconRec;
  Length: Longint;
begin
  FillChar(CI, SizeOf(CI), 0);
  FillChar(List, SizeOf(List), 0);
  CheckBool(GetIconInfo(Icon, IconInfo));
  try
    InternalGetDIBSizes(IconInfo.hbmMask, MonoInfoSize, MonoBitsSize, 2);
    InternalGetDIBSizes(IconInfo.hbmColor, ColorInfoSize, ColorBitsSize, -1);
    MonoInfo := nil;
    MonoBits := nil;
    ColorInfo := nil;
    ColorBits := nil;
    try
      MonoInfo := AllocMem(MonoInfoSize);
      MonoBits := AllocMem(MonoBitsSize);
      ColorInfo := AllocMem(ColorInfoSize);
      ColorBits := AllocMem(ColorBitsSize);
      InternalGetDIB(IconInfo.hbmMask, 0, MonoInfo^, MonoBits^, 2);
      InternalGetDIB(IconInfo.hbmColor, 0, ColorInfo^, ColorBits^, -1);
      if WriteLength then
      begin
        Length := SizeOf(CI) + SizeOf(List) + ColorInfoSize +
          ColorBitsSize + MonoBitsSize;
        Stream.Write(Length, SizeOf(Length));
      end;
      with CI do
      begin
        CI.wType := RC3_ICON;
        CI.Count := 1;
      end;
      Stream.Write(CI, SizeOf(CI));
      with List, PBitmapInfoHeader(ColorInfo)^ do
      begin
        Width := biWidth;
        Height := biHeight;
        Colors := biPlanes * biBitCount;
        DIBSize := ColorInfoSize + ColorBitsSize + MonoBitsSize;
        DIBOffset := SizeOf(CI) + SizeOf(List);
      end;
      Stream.Write(List, SizeOf(List));
      with PBitmapInfoHeader(ColorInfo)^ do
        Inc(biHeight, biHeight); { color height includes mono bits }
      Stream.Write(ColorInfo^, ColorInfoSize);
      Stream.Write(ColorBits^, ColorBitsSize);
      Stream.Write(MonoBits^, MonoBitsSize);
    finally
      FreeMem(ColorInfo, ColorInfoSize);
      FreeMem(ColorBits, ColorBitsSize);
      FreeMem(MonoInfo, MonoInfoSize);
      FreeMem(MonoBits, MonoBitsSize);
    end;
  finally
    DeleteObject(IconInfo.hbmColor);
    DeleteObject(IconInfo.hbmMask);
  end;
end;
{$ENDIF}

{$IF DEFINED(CLR)}
function GetStoredIconType(Stream: TStream): Word;
var
  Size: Integer;
  Int32: Cardinal;
begin
  Size := SizeOf(Int32);
  // Read the 1st two words of the TCursorOrIcon structure
  Stream.ReadBuffer(Int32, Size);
  // Return the 2nd word, which is the "wType" field
  Result := HiWord(Int32);
  Stream.Seek(0 - Size, soCurrent);
end;
{$ENDIF}

{ TGraphic }

constructor TGraphic.Create;
begin
  inherited Create;
end;

destructor TGraphic.Destroy;
begin
  FreeAndNil(FScaledDrawer);
  inherited;
end;

procedure TGraphic.Changed(Sender: TObject);
begin
  FModified := True;
  UpdateScaledDrawer;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{$IFDEF CLR}[SecurityPermission(SecurityAction.Assert, UnmanagedCode=True)]{$ENDIF}
procedure TGraphic.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not (Filer.Ancestor is TGraphic) or
        not Equals(TGraphic(Filer.Ancestor))
    else
      Result := not Empty;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

procedure TGraphic.DrawTransparent(ACanvas: TCanvas; const Rect: TRect;
  Opacity: Byte);
begin
  Draw(ACanvas, Rect);
end;

function TGraphic.Equals(Graphic: TGraphic): Boolean;
var
  MyImage, GraphicsImage: TMemoryStream;
begin
  Result := (Graphic <> nil) and (ClassType = Graphic.ClassType);
  if Empty or Graphic.Empty then
  begin
    Result := Empty and Graphic.Empty;
    Exit;
  end;
  if Result then
  begin
    MyImage := TMemoryStream.Create;
    try
      WriteData(MyImage);
      GraphicsImage := TMemoryStream.Create;
      try
        Graphic.WriteData(GraphicsImage);
        Result := (MyImage.Size = GraphicsImage.Size) and
          CompareMem(MyImage.Memory, GraphicsImage.Memory, MyImage.Size);
      finally
        GraphicsImage.Free;
      end;
    finally
      MyImage.Free;
    end;
  end;
end;

function TGraphic.Equals(Obj: TObject): Boolean;
begin
  if Obj is TGraphic then
    Result := Equals(TGraphic(Obj))
  else
    Result := inherited Equals(Obj);
end;

function TGraphic.GetPalette: HPALETTE;
begin
  Result := 0;
end;

function TGraphic.GetSupportsPartialTransparency: Boolean;
begin
  Result := False;
end;

function TGraphic.GetTransparent: Boolean;
begin
  Result := FTransparent;
end;

class function TGraphic.CanLoadFromStream(Stream: TStream): Boolean;
begin
  Result := False;
end;

procedure TGraphic.LoadFromFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TGraphic.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TGraphic.SaveToFile(const Filename: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(Filename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGraphic.SetPalette(Value: HPalette);
begin
end;

procedure TGraphic.SetModified(Value: Boolean);
begin
  if Value then
    Changed(Self) else
    FModified := False;
end;

procedure TGraphic.SetSize(AWidth, AHeight: Integer);
begin
  Width := AWidth;
  Height := AHeight;
end;

procedure TGraphic.SetTransparent(Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    Changed(Self);
  end;
end;

procedure TGraphic.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TGraphic.EnableScaledDrawer(AGraphicScalerClass: TScaledGraphicDrawerClass; AInitialize: Boolean = True);
begin
  if AGraphicScalerClass = nil then
    raise EArgumentNilException.CreateRes(@SArgumentNil);

  FreeAndNil(FScaledDrawer);
  FScaledDrawer := AGraphicScalerClass.Create(Self, AInitialize);
end;

procedure TGraphic.UpdateScaledDrawer;
begin
  if FScaledDrawer <> nil then
    FScaledDrawer.Initialize;
end;

procedure TGraphic.DisableScaledDrawer;
begin
  FreeAndNil(FScaledDrawer);
end;

{ TPicture }

type
{$IF DEFINED(CLR)}
  TFileFormat = class(TObject)
  public
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: string;
  end;

  TFileFormatType = TFileFormat;
{$ELSE}
  PFileFormat = ^TFileFormat;
  TFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

  TFileFormatType = PFileFormat;
{$ENDIF}

  TFileFormatsList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
{$IF DEFINED(CLR)}
    procedure Add(const Ext, Desc: String; DescID: string; AClass: TGraphicClass);
{$ELSE}
    procedure Add(const Ext, Desc: String; DescID: Integer; AClass: TGraphicClass);
{$ENDIF}
    function FindExt(Ext: string): TGraphicClass;
    function FindFormat(Stream: TStream): TGraphicClass;
    function FindClassName(const Classname: string): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    procedure BuildFilterStrings(AGraphicClass: TGraphicClass;
      var Descriptions, Filters: string);
  end;

constructor TFileFormatsList.Create;
begin
  inherited Create;
{$IF DEFINED(CLR)}
  Add('tiff', SVTIFFImages, '', TBitmap);
  Add('tif', SVTIFFImages, '', TBitmap);
  Add('wmf', SVMetafiles, '', TMetafile);
  Add('emf', SVEnhMetafiles, '', TMetafile);
  Add('ico', SVIcons, '', TIcon);
  Add('png', SVPNGImages, '', TBitmap);
  Add('gif', SVGIFImages, '', TBitmap);
  Add('jpeg', SVJPGImages, '', TBitmap);
  Add('jpg', SVJPGImages, '', TBitmap);
  Add('bmp', SVBitmaps, '', TBitmap);
{$ELSE}
  Add('tiff', SVTIFFImages, 0, TWICImage);
  Add('tif', SVTIFFImages, 0, TWICImage);
  Add('wmf', SVMetafiles, 0, TMetafile);
  Add('emf', SVEnhMetafiles, 0, TMetafile);
  Add('ico', SVIcons, 0, TIcon);
  Add('bmp', SVBitmaps, 0, TBitmap);
{$ENDIF}
end;

destructor TFileFormatsList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count-1 do
{$IF DEFINED(CLR)}
    Items[I].Free;
{$ELSE}
    Dispose(PFileFormat(Items[I]));
{$ENDIF}
  inherited Destroy;
end;

{$IF DEFINED(CLR)}
procedure TFileFormatsList.Add(const Ext, Desc: String; DescID: string;
  AClass: TGraphicClass);
{$ELSE}
procedure TFileFormatsList.Add(const Ext, Desc: String; DescID: Integer;
  AClass: TGraphicClass);
{$ENDIF}
var
  NewFormat: TFileFormatType;
begin
{$IF DEFINED(CLR)}
  NewFormat := TFileFormat.Create;
{$ELSE}
  New(NewFormat);
{$ENDIF}
  with NewFormat{$IFNDEF CLR}^{$ENDIF} do
  begin
    Extension := AnsiLowerCase(Ext);
    GraphicClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(NewFormat);
end;

function TFileFormatsList.FindExt(Ext: string): TGraphicClass;
var
  I: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for I := Count - 1 downto 0 do
    with TFileFormatType(Items[I]){$IFNDEF CLR}^{$ENDIF} do
      if Extension = Ext then
      begin
        Result := GraphicClass;
        Exit;
      end;
  Result := nil;
end;

function TFileFormatsList.FindFormat(Stream: TStream): TGraphicClass;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with TFileFormatType(Items[I]){$IFNDEF CLR}^{$ENDIF} do
      if GraphicClass.CanLoadFromStream(Stream) then
      begin
        Result := GraphicClass;
        Exit;
      end;
  Result := nil;
end;

function TFileFormatsList.FindClassName(const ClassName: string): TGraphicClass;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
  begin
    Result := TFileFormatType(Items[I]).GraphicClass;
    if Result.ClassName = Classname then Exit;
  end;
  Result := nil;
end;

procedure TFileFormatsList.Remove(AClass: TGraphicClass);
var
  I: Integer;
  LFormat: TFileFormatType;
begin
  for I := Count-1 downto 0 do
  begin
    LFormat := TFileFormatType(Items[I]);
    if LFormat.GraphicClass.InheritsFrom(AClass) then
    begin
{$IF DEFINED(CLR)}
      LFormat.Free;
{$ELSE}
      Dispose(LFormat);
{$ENDIF}
      Delete(I);
    end;
  end;
end;

procedure TFileFormatsList.BuildFilterStrings(AGraphicClass: TGraphicClass;
  var Descriptions, Filters: string);
var
  C, I: Integer;
  LFormat: TFileFormatType;
  LDescriptions, LFilters: TStringBuilder;
begin
  LDescriptions := TStringBuilder.Create;
  LFilters := TStringBuilder.Create;
  try
    C := 0;
    for I := Count-1 downto 0 do
    begin
      LFormat := TFileFormatType(Items[I]);
      with LFormat{$IFNDEF CLR}^{$ENDIF} do
      begin
        if GraphicClass.InheritsFrom(AGraphicClass) and (Extension <> '') then
        begin
          if C <> 0 then
          begin
            LDescriptions.Append('|');
            LFilters.Append(';');
          end;
  {$IF DEFINED(CLR)}
          if (Description = '') and (DescResID <> '') then
            Description := LoadResString(DescResID);
  {$ELSE}
          if (Description = '') and (DescResID <> 0) then
            Description := LoadStr(DescResID);
  {$ENDIF}
          LDescriptions.Append(Format('%s (*.%s)|*.%1:s', [Description, Extension]));
          LFilters.Append(Format('*.%s', [Extension]));
          Inc(C);
        end;
      end;
    end;
    if C > 1 then
      LDescriptions.Insert(0, Format('%s (%s)|%1:s|', [sAllFilter, LFilters.ToString(True)]));
    Descriptions := LDescriptions.ToString(True);
    Filters := LFilters.ToString(True);
  finally
    LDescriptions.Free;
    LFilters.Free;
  end;
end;

type
  TClipboardFormats = class
  private
    FClasses: TList;
    FFormats: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Fmt: Word; AClass: TGraphicClass);
    function FindFormat(Fmt: Word): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
  end;

constructor TClipboardFormats.Create;
begin
  inherited Create;
  FClasses := TList.Create;
  FFormats := TList.Create;
  Add(CF_METAFILEPICT, TMetafile);
  Add(CF_ENHMETAFILE, TMetafile);
  Add(CF_BITMAP, TBitmap);
end;

destructor TClipboardFormats.Destroy;
begin
  FClasses.Free;
  FFormats.Free;
  inherited Destroy;
end;

procedure TClipboardFormats.Add(Fmt: Word; AClass: TGraphicClass);
var
  I: Integer;
begin
{$IF DEFINED(CLR)}
  I := FClasses.Add(TObject(AClass));
  try
     FFormats.Add(TObject(Integer(Fmt)));
  except
    FClasses.Delete(I);
    raise;
  end;
{$ELSE}
  I := FClasses.Add(AClass);
  try
    FFormats.Add(PInteger(Fmt));
  except
    FClasses.Delete(I);
    raise;
  end;
{$ENDIF}
end;

function TClipboardFormats.FindFormat(Fmt: Word): TGraphicClass;
var
  I: Integer;
begin
  for I := FFormats.Count-1 downto 0 do
    if Integer(FFormats[I]) = Fmt then
    begin
      Result := TGraphicClass(FClasses[I]);
      Exit;
    end;
  Result := nil;
end;

procedure TClipboardFormats.Remove(AClass: TGraphicClass);
var
  I: Integer;
begin
  for I := FClasses.Count-1 downto 0 do
    if TGraphicClass(FClasses[I]).InheritsFrom(AClass) then
    begin
      FClasses.Delete(I);
      FFormats.Delete(I);
    end;
end;

var
  ClipboardFormats: TClipboardFormats = nil;
  FileFormats: TFileFormatsList = nil;

function GetFileFormats: TFileFormatsList;
begin
  if FileFormats = nil then FileFormats := TFileFormatsList.Create;
  Result := FileFormats;
end;

function GetClipboardFormats: TClipboardFormats;
begin
  if ClipboardFormats = nil then ClipboardFormats := TClipboardFormats.Create;
  Result := ClipboardFormats;
end;

constructor TScaledGraphicDrawer.Create(AGraphic: TGraphic; AInitialize: Boolean);
begin
  inherited Create;
  FGraphic := AGraphic;
  if AInitialize then
    Initialize;
end;

function TScaledGraphicDrawer.GetInitialized: Boolean;
begin
  Result := False;
end;

constructor TPicture.Create;
begin
  inherited Create;
  GetFileFormats;
  GetClipboardFormats;
end;

destructor TPicture.Destroy;
begin
  FGraphic.Free;
  inherited Destroy;
end;

procedure TPicture.AssignTo(Dest: TPersistent);
begin
  if Graphic is Dest.ClassType then
    Dest.Assign(Graphic)
  else
    inherited AssignTo(Dest);
end;

procedure TPicture.ForceType(GraphicType: TGraphicClass);
begin
  if not (Graphic is GraphicType) then
  begin
    FGraphic.Free;
    FGraphic := nil;
    FGraphic := GraphicType.Create;
    FGraphic.OnChange := Changed;
    FGraphic.OnProgress := Progress;
    Changed(Self);
  end;
end;

procedure TPicture.Load(GraphicClass: TGraphicClass; LoadProc: TLoadProc);
var
  NewGraphic: TGraphic;
begin
  NewGraphic := nil;
  try
    if GraphicClass <> nil then
    begin
      NewGraphic := GraphicClass.Create;
      NewGraphic.OnProgress := Progress;
      LoadProc(NewGraphic);
    end;
    FGraphic.Free;
  except
    NewGraphic.Free;
    raise;
  end;
  FGraphic := NewGraphic;
  if FGraphic <> nil then
    FGraphic.OnChange := Changed;
  Changed(Self);
end;

function TPicture.GetBitmap: TBitmap;
begin
  ForceType(TBitmap);
  Result := TBitmap(Graphic);
end;

function TPicture.GetWICImage: TWICImage;
begin
  ForceType(TWICImage);
  Result := TWICImage(Graphic);
end;

function TPicture.GetIcon: TIcon;
begin
  ForceType(TIcon);
  Result := TIcon(Graphic);
end;

function TPicture.GetMetafile: TMetafile;
begin
  ForceType(TMetafile);
  Result := TMetafile(Graphic);
end;

procedure TPicture.SetBitmap(Value: TBitmap);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetWICImage(Value: TWICImage);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetIcon(Value: TIcon);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetMetafile(Value: TMetafile);
begin
  SetGraphic(Value);
end;

procedure TPicture.SetGraphic(Value: TGraphic);
var
  GraphicClass: TGraphicClass;
begin
  if Value <> nil then
    GraphicClass := TGraphicClass(Value.ClassType)
  else
    GraphicClass := nil;

  Load(GraphicClass,
    procedure (Graphic: TGraphic)
    begin
      Graphic.Assign(Value);
    end
  );
end;

{ Based on the extension of Filename, create the cooresponding TGraphic class
  and call its LoadFromFile method. }

procedure TPicture.LoadFromFile(const Filename: string);
var
  Ext: string;
  GraphicClass: TGraphicClass;
  Context: TFindGraphicClassContext;
begin
  Ext := ExtractFileExt(Filename).Remove(0, 1);
  GraphicClass := FileFormats.FindExt(Ext);
  Context.FSource := gsFileName;
  Context.FFileName := Filename;
  FindGraphicClass(Context, GraphicClass);
  if GraphicClass = nil then
    raise EInvalidGraphic.CreateFmt(SUnknownExtension, [Ext]);

  Load(GraphicClass,
    procedure (Graphic: TGraphic)
    begin
      Graphic.LoadFromFile(Filename);
    end
  );
end;

procedure TPicture.SaveToFile(const Filename: string);
begin
  if FGraphic <> nil then FGraphic.SaveToFile(Filename);
end;

{$IFDEF CLR}[UIPermission(SecurityAction.LinkDemand, Clipboard=UIPermissionClipboard.AllClipboard)]{$ENDIF}
procedure TPicture.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  GraphicClass: TGraphicClass;
  Context: TFindGraphicClassContext;
begin
  GraphicClass := ClipboardFormats.FindFormat(AFormat);
  Context.FSource := gsClipboard;
  Context.FClipboardFormat := AFormat;
  FindGraphicClass(Context, GraphicClass);
  if GraphicClass = nil then
    InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SUnknownClipboardFormat);

  Load(GraphicClass,
    procedure (Graphic: TGraphic)
    begin
      Graphic.LoadFromClipboardFormat(AFormat, AData, APalette);
    end
  );
end;

procedure TPicture.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
begin
  if FGraphic <> nil then
    FGraphic.SaveToClipboardFormat(AFormat, AData, APalette);
end;

class function TPicture.SupportsClipboardFormat(AFormat: Word): Boolean;
begin
  Result := GetClipboardFormats.FindFormat(AFormat) <> nil;
end;

procedure TPicture.LoadFromStream(Stream: TStream);
var
  GraphicClass: TGraphicClass;
  Context: TFindGraphicClassContext;
begin
  if Stream.Size - Stream.Position = 0 then
    GraphicClass := TBitmap
  else
    GraphicClass := FileFormats.FindFormat(Stream);
  Context.FSource := gsStream;
  Context.FStream := Stream;
  FindGraphicClass(Context, GraphicClass);
  if GraphicClass = nil then
    InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SUnknownStreamFormat);

  Load(GraphicClass,
    procedure (Graphic: TGraphic)
    begin
      Graphic.LoadFromStream(Stream);
    end
  );
end;

procedure TPicture.SaveToStream(Stream: TStream);
begin
  if FGraphic <> nil then FGraphic.SaveToStream(Stream);
end;

procedure TPicture.Assign(Source: TPersistent);
begin
  if Source = nil then
    SetGraphic(nil)
  else if Source is TPicture then
    SetGraphic(TPicture(Source).Graphic)
  else if Source is TGraphic then
    SetGraphic(TGraphic(Source))
  else
    inherited Assign(Source);
end;

class procedure TPicture.RegisterFileFormat(const AExtension,
  ADescription: string; AGraphicClass: TGraphicClass);
begin
{$IF DEFINED(CLR)}
  GetFileFormats.Add(AExtension, ADescription, '', AGraphicClass);
{$ELSE}
  GetFileFormats.Add(AExtension, ADescription, 0, AGraphicClass);
{$ENDIF}
end;

{$IF DEFINED(CLR)}
class procedure TPicture.RegisterFileFormatRes(const AExtension: String;
  ADescriptionResID: string; AGraphicClass: TGraphicClass);
{$ELSE}
class procedure TPicture.RegisterFileFormatRes(const AExtension: String;
  ADescriptionResID: Integer; AGraphicClass: TGraphicClass);
{$ENDIF}
begin
  GetFileFormats.Add(AExtension, '', ADescriptionResID, AGraphicClass);
end;

class procedure TPicture.RegisterClipboardFormat(AFormat: Word;
  AGraphicClass: TGraphicClass);
begin
  GetClipboardFormats.Add(AFormat, AGraphicClass);
end;

class procedure TPicture.UnRegisterGraphicClass(AClass: TGraphicClass);
begin
  if FileFormats <> nil then FileFormats.Remove(AClass);
  if ClipboardFormats <> nil then ClipboardFormats.Remove(AClass);
end;

procedure TPicture.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
  if FNotify <> nil then FNotify.Changed;
end;

procedure TPicture.Progress(Sender: TObject; Stage: TProgressStage;
  PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if Assigned(FOnProgress) then FOnProgress(Sender, Stage, PercentDone, RedrawNow, R, Msg);
end;

procedure TPicture.FindGraphicClass(const Context: TFindGraphicClassContext;
  var GraphicClass: TGraphicClass);
begin
  if Assigned(FOnFindGraphicClass) then FOnFindGraphicClass(Self, Context, GraphicClass);
end;

procedure TPicture.ReadData(Stream: TStream);
var
  GraphicClass: TGraphicClass;
  LClassName: string;
  LBytes: TBytes;
  LNameLen: Byte;
begin
  Stream.Read(LNameLen, 1);
  SetLength(LBytes, LNameLen);
  Stream.Read(LBytes{$IFNDEF CLR}[0]{$ENDIF}, LNameLen);
  LClassName := TEncoding.UTF8.GetString(LBytes);
  GraphicClass := FileFormats.FindClassName(LClassName);

  Load(GraphicClass,
    procedure (Graphic: TGraphic)
    begin
      Graphic.ReadData(Stream);
    end
  );
end;

procedure TPicture.WriteData(Stream: TStream);
var
  LClassName: string;
  LBytes: TBytes;
  LNameLen: Integer;
begin
  with Stream do
  begin
    if Graphic <> nil then
      LClassName := Graphic.ClassName
    else
      LClassName := '';
    LBytes := TEncoding.UTF8.GetBytes(LClassName);
    LNameLen := Length(LBytes);
    Write(LNameLen, 1);  // Only write 1 byte (length of string)
    Write(LBytes{$IFNDEF CLR}[0]{$ENDIF}, LNameLen);

    if Graphic <> nil then
      Graphic.WriteData(Stream);
  end;
end;

{$IFDEF CLR}[SecurityPermission(SecurityAction.Assert, UnmanagedCode=True)]{$ENDIF}
procedure TPicture.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    Ancestor: TPicture;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TPicture then
      begin
        Ancestor := TPicture(Filer.Ancestor);
        Result := not ((Graphic = Ancestor.Graphic) or
          ((Graphic <> nil) and (Ancestor.Graphic <> nil) and
          Graphic.Equals(Ancestor.Graphic)));
      end;
    end
    else Result := Graphic <> nil;
  end;

begin
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, DoWrite);
end;

function TPicture.GetWidth: Integer;
begin
  Result := 0;
  if FGraphic <> nil then Result := FGraphic.Width;
end;

function TPicture.GetHeight: Integer;
begin
  Result := 0;
  if FGraphic <> nil then Result := FGraphic.Height;
end;

{ TMetafileImage }

destructor TMetafileImage.Destroy;
begin
{$IF DEFINED(CLR)}
  FreeHandle;
{$ELSE}
  if FHandle <> 0 then DeleteEnhMetafile(FHandle);
  InternalDeletePalette(FPalette);
{$ENDIF}
  inherited Destroy;
end;

procedure TMetafileImage.FreeHandle;
begin
{$IF DEFINED(CLR)}
  if FHandle <> 0 then
  begin
    DeleteEnhMetafile(FHandle);
    FHandle := 0;
  end;
  if FPalette <> 0 then
  begin
    DeleteObject(FPalette);
    FPalette := 0;
  end;
{$ENDIF}
end;

{ TMetafileDC }

{$IF DEFINED(CLR)}
procedure TMetafileDC.Finalize;
begin
  if Handle <> 0 then
  begin
    SelectObject(Handle, StockPen);
    SelectObject(Handle, StockBrush);
    SelectObject(Handle, StockFont);
    DeleteObject(CloseEnhMetafile(Handle));
    Handle := 0;
  end;
  inherited;
end;
{$ENDIF}

{ TMetafileCanvas }

constructor TMetafileCanvas.Create(AMetafile: TMetafile; ReferenceDevice: HDC);
begin
  CreateWithComment(AMetafile, ReferenceDevice, AMetafile.CreatedBy,
    AMetafile.Description);
end;

constructor TMetafileCanvas.CreateWithComment(AMetafile : TMetafile;
  ReferenceDevice: HDC; const CreatedBy, Description: String);
var
  RefDC: HDC;
  R: TRect;
{$IF DEFINED(CLR)}
  P: StringBuilder;
{$ELSE}
  Temp: HDC;
  P: PChar;
{$ENDIF}
begin
  inherited Create;
  FMetafile := AMetafile;
  RefDC := ReferenceDevice;
  if ReferenceDevice = 0 then RefDC := GetDC(0);
  try
    if FMetafile.MMWidth = 0 then
      if FMetafile.Width = 0 then
        FMetafile.MMWidth := GetDeviceCaps(RefDC, HORZSIZE) * 100
      else
        FMetafile.MMWidth := MulDiv(FMetafile.Width,
          GetDeviceCaps(RefDC, HORZSIZE) * 100, GetDeviceCaps(RefDC, HORZRES));
    if FMetafile.MMHeight = 0 then
      if FMetafile.Height = 0 then
        FMetafile.MMHeight := GetDeviceCaps(RefDC, VERTSIZE) * 100
      else
        FMetafile.MMHeight := MulDiv(FMetafile.Height,
          GetDeviceCaps(RefDC, VERTSIZE) * 100, GetDeviceCaps(RefDC, VERTRES));
    R := Rect(0, 0, FMetafile.MMWidth, FMetafile.MMHeight);
    if (Length(CreatedBy) > 0) or (Length(Description) > 0) then
{$IF DEFINED(CLR)}
    begin
      P := StringBuilder.Create(1024);
      P.Append(CreatedBy);
      P.Append(#0);
      P.Append(Description);
      P.Append(#0#0);
    end
{$ELSE}
      P := PChar(CreatedBy+#0+Description+#0#0)
{$ENDIF}
    else
      P := nil;
{$IF DEFINED(CLR)}
    FMetafileDC := TMetafileDC.Create;
    FMetafileDC.Handle := CreateEnhMetafile(RefDC, nil, R, P);
    if FMetafileDC.Handle = 0 then
      GDIError;
    Handle := FMetafileDC.Handle;
{$ELSE}
    Temp := CreateEnhMetafile(RefDC, nil, @R, P);
    if Temp = 0 then GDIError;
    Handle := Temp;
{$ENDIF}
  finally
    if ReferenceDevice = 0 then
      ReleaseDC(0, RefDC);
  end;
end;

destructor TMetafileCanvas.Destroy;
{$IF NOT DEFINED(CLR)}
var
  Temp: HDC;
{$ENDIF}
begin
{$IF DEFINED(CLR)}
  if FMetafileDC.Handle <> 0 then
  begin
    Handle := 0;
    FMetafile.Handle := CloseEnhMetafile(FMetafileDC.Handle);
    FMetafileDC.Handle := 0;
  end;
  System.GC.SuppressFinalize(FMetafileDC);
{$ELSE}
  Temp := Handle;
  Handle := 0;
  FMetafile.Handle := CloseEnhMetafile(Temp);
{$ENDIF}
  inherited Destroy;
end;

{ TMetafile }

constructor TMetafile.Create;
begin
  inherited Create;
  FEnhanced := True;
  FTransparent := True;
  Assign(nil);
end;

destructor TMetafile.Destroy;
begin
  FImage.Release;
  inherited Destroy;
end;

procedure TMetafile.Assign(Source: TPersistent);
var
  Pal: HPalette;
begin
  if (Source = nil) or (Source is TMetafile) then
  begin
    Pal := 0;
    if FImage <> nil then
    begin
      Pal := FImage.FPalette;
      FImage.Release;
    end;
    if Assigned(Source) then
    begin
      FImage := TMetafile(Source).FImage;
      FEnhanced := TMetafile(Source).Enhanced;
    end
    else
    begin
      FImage := TMetafileImage.Create;
      FEnhanced := True;
    end;
    FImage.Reference;
    PaletteModified := (Pal <> Palette) and (Palette <> 0);
    Changed(Self);
  end
  else
    inherited Assign(Source);
end;

procedure TMetafile.Clear;
begin
  NewImage;
end;

procedure TMetafile.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  MetaPal, OldPal: HPALETTE;
  R: TRect;
begin
  if FImage = nil then Exit;
  MetaPal := Palette;
  OldPal := 0;
  if MetaPal <> 0 then
  begin
    OldPal := SelectPalette(ACanvas.Handle, MetaPal, True);
    RealizePalette(ACanvas.Handle);
  end;
  R := Rect;
  Dec(R.Right);  // Metafile rect includes right and bottom coords
  Dec(R.Bottom);
  PlayEnhMetaFile(ACanvas.Handle, FImage.FHandle, R);
  if MetaPal <> 0 then
    SelectPalette(ACanvas.Handle, OldPal, True);
end;

function TMetafile.GetAuthor: String;
var
{$IF DEFINED(CLR)}
  Buf: StringBuilder;
  BufLength: Integer;
{$ELSE}
  Temp: Integer;
{$ENDIF}
begin
  Result := '';
  if (FImage = nil) or (FImage.FHandle = 0) then Exit;
{$IF DEFINED(CLR)}
  BufLength := GetEnhMetafileDescription(FImage.FHandle, 0, StringBuilder(nil));
  if BufLength <= 0 then Exit;
  Buf := StringBuilder.Create(BufLength);
  GetEnhMetafileDescription(FImage.FHandle, BufLength, Buf);
  Result := Buf.ToString;
{$ELSE}
  Temp := GetEnhMetafileDescription(FImage.FHandle, 0, nil);
  if Temp <= 0 then Exit;
  SetLength(Result, Temp);
  GetEnhMetafileDescription(FImage.FHandle, Temp, PChar(Result));
  SetLength(Result, StrLen(PChar(Result)));
{$ENDIF}
end;

function TMetafile.GetDesc: String;
{$IF DEFINED(CLR)}
var
  S: string;
  P, L: Integer;
  Buf: TBytes;
  BufLength: Integer;
begin
  Result := '';
  if (FImage = nil) or (FImage.FHandle = 0) then Exit;
  BufLength := GetEnhMetafileDescription(FImage.FHandle, 0, StringBuilder(nil));
  if BufLength <= 0 then Exit;
  SetLength(Buf, BufLength * Marshal.SystemDefaultCharSize);
  GetEnhMetafileDescription(FImage.FHandle, BufLength, Buf);

  S := PlatformStringOf(Buf);
  P := Pos(#0, S);
  if P > 0 then
  begin
    L := PosEx(#0, S, P + 1) - 1;
    if L > 0 then
      Result := Copy(S, P + 1, L - P)
  end;
end;
{$ELSE}
var
  Temp: Integer;
begin
  Result := '';
  if (FImage = nil) or (FImage.FHandle = 0) then Exit;
  Temp := GetEnhMetafileDescription(FImage.FHandle, 0, nil);
  if Temp <= 0 then Exit;
  SetLength(Result, Temp);
  GetEnhMetafileDescription(FImage.FHandle, Temp, PChar(Result));
  Delete(Result, 1, StrLen(PChar(Result))+1);
  SetLength(Result, StrLen(PChar(Result)));
end;
{$ENDIF}

function TMetafile.GetEmpty;
begin
  Result := FImage = nil;
end;

function TMetafile.GetHandle: HENHMETAFILE;
begin
  if Assigned(FImage) then
    Result := FImage.FHandle
  else
    Result := 0;
end;

function TMetaFile.HandleAllocated: Boolean;
begin
  Result := Assigned(FImage) and (FImage.FHandle <> 0);
end;

const
  HundredthMMPerInch = 2540;

function TMetafile.GetHeight: Integer;
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
   if FInch = 0 then
     if FHandle = 0 then
       Result := FTempHeight
     else
     begin               { convert 0.01mm units to referenceDC device pixels }
{$IF DEFINED(CLR)}
       GetEnhMetaFileHeader(FHandle, Marshal.SizeOf(TypeOf(EMFHeader)), EMFHeader);
{$ELSE}
       GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
{$ENDIF}
       Result := MulDiv(FHeight,                      { metafile height in 0.01mm }
         EMFHeader.szlDevice.cy,                      { device height in pixels }
         EMFHeader.szlMillimeters.cy * 100);          { device height in mm }
     end
   else          { for WMF files, convert to font dpi based device pixels }
     Result := MulDiv(FHeight, ScreenLogPixels, HundredthMMPerInch);
end;

function TMetafile.GetInch: Word;
begin
  Result := 0;
  if FImage <> nil then Result := FImage.FInch;
end;

function TMetafile.GetMMHeight: Integer;
begin
  if FImage = nil then NewImage;
  Result := FImage.FHeight;
end;

function TMetafile.GetMMWidth: Integer;
begin
  if FImage = nil then NewImage;
  Result := FImage.FWidth;
end;

function TMetafile.GetPalette: HPALETTE;
var
  LogPal: TMaxLogPalette;
  Count: Integer;
begin
  Result := 0;
  if (FImage = nil) or (FImage.FHandle = 0) then Exit;
  if FImage.FPalette = 0 then
  begin
    Count := GetEnhMetaFilePaletteEntries(FImage.FHandle, 0, nil);
    if Count = 0 then
      Exit
    else if Count > 256 then
      Count := Count and $FF;
    InternalDeletePalette(FImage.FPalette);
    LogPal.palVersion := $300;
    LogPal.palNumEntries := Count;
{$IF DEFINED(CLR)}
    GetEnhMetaFilePaletteEntries(FImage.FHandle, Count, LogPal.palPalEntry);
    FImage.FPalette := CreatePalette(LogPal);
{$ELSE}
    GetEnhMetaFilePaletteEntries(FImage.FHandle, Count, @LogPal.palPalEntry);
    FImage.FPalette := CreatePalette(PLogPalette(@LogPal)^);
{$ENDIF}
  end;
  Result := FImage.FPalette;
end;

function TMetafile.GetWidth: Integer;
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
    if FInch = 0 then
      if FHandle = 0 then
        Result := FTempWidth
      else
      begin     { convert 0.01mm units to referenceDC device pixels }
{$IF DEFINED(CLR)}
        GetEnhMetaFileHeader(FHandle, Marshal.SizeOf(TypeOf(EMFHeader)), EMFHeader);
{$ELSE}
        GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
{$ENDIF}
        Result := MulDiv(FWidth,                       { metafile width in 0.01mm }
          EMFHeader.szlDevice.cx,                      { device width in pixels }
          EMFHeader.szlMillimeters.cx * 100);          { device width in 0.01mm }
      end
    else      { for WMF files, convert to font dpi based device pixels }
      Result := MulDiv(FWidth, ScreenLogPixels, HundredthMMPerInch);
end;

class function TMetafile.CanLoadFromStream(Stream: TStream): Boolean;
var
  WMF: TMetafileHeader;
  P: Int64;
begin
  P := Stream.Position;
  try
    Result := TestEMF(Stream);
    if not Result then
      Result := (Stream.Read(WMF, SizeOf(WMF)) = SizeOf(WMF)) and
        (WMF.Key = WMFKEY) and (ComputeAldusChecksum(WMF) = WMF.CheckSum);
  finally
    Stream.Position := P;
  end;
end;

procedure TMetafile.LoadFromStream(Stream: TStream);
begin
  if TestEMF(Stream) then
    ReadEMFStream(Stream)
  else
    ReadWMFStream(Stream, Stream.Size - Stream.Position);
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.NewImage;
begin
  FImage.Release;
  FImage := TMetafileImage.Create;
  FImage.Reference;
end;

procedure TMetafile.ReadData(Stream: TStream);
var
  Length: Longint;
begin
  Stream.Read(Length, SizeOf(Longint));
  if Length <= 4 then
    Assign(nil)
  else
    if TestEMF(Stream) then
      ReadEMFStream(Stream)
    else
      ReadWMFStream(Stream, Length - Sizeof(Length));
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.ReadEMFStream(Stream: TStream);
{$IF DEFINED(CLR)}
var
  Buffer: TBytes;
  HeaderSize: Integer;
  EnhHeader: TEnhMetaheader;
begin
  NewImage;
  HeaderSize := SizeOf(TEnhMetaheader);
  SetLength(Buffer, HeaderSize);
  Stream.ReadBuffer(Buffer, HeaderSize);
  EnhHeader := TEnhMetaheader(BytesToStructure(Buffer, TypeOf(TEnhMetaheader)));
  if EnhHeader.dSignature <> ENHMETA_SIGNATURE then InvalidMetafile;
  SetLength(Buffer, EnhHeader.nBytes);
  with FImage do
  begin
    Stream.Read(Buffer, HeaderSize, EnhHeader.nBytes - DWORD(HeaderSize));
    FHandle := SetEnhMetafileBits(EnhHeader.nBytes, Buffer);
    if FHandle = 0 then InvalidMetafile;
    FInch := 0;
    with EnhHeader.rclFrame do
    begin
      FWidth := Right - Left;    { in 0.01 mm units }
      FHeight := Bottom - Top;
    end;
    Enhanced := True;
  end;
end;
{$ELSE}
var
  EnhHeader: TEnhMetaheader;
  Buf: PByte;
begin
  NewImage;
  Stream.ReadBuffer(EnhHeader, Sizeof(EnhHeader));
  if EnhHeader.dSignature <> ENHMETA_SIGNATURE then InvalidMetafile;
  if EnhHeader.nBytes < Sizeof(EnhHeader) then
    InvalidMetafile;
  GetMem(Buf, EnhHeader.nBytes);
  with FImage do
  try
    Move(EnhHeader, Buf^, Sizeof(EnhHeader));
    Stream.ReadBuffer(PByte(Buf + Sizeof(EnhHeader))^,
      EnhHeader.nBytes - Sizeof(EnhHeader));
    FHandle := SetEnhMetafileBits(EnhHeader.nBytes, Buf);
    if FHandle = 0 then InvalidMetafile;
    FInch := 0;
    with EnhHeader.rclFrame do
    begin
      FWidth := Right - Left;    { in 0.01 mm units }
      FHeight := Bottom - Top;
    end;
    Enhanced := True;
  finally
    FreeMem(Buf, EnhHeader.nBytes);
  end;
end;
{$ENDIF}

procedure TMetafile.ReadWMFStream(Stream: TStream; Length: Longint);
var
  WMF: TMetafileHeader;
  MFP: TMetaFilePict;
  EMFHeader: TEnhMetaheader;
{$IF DEFINED(CLR)}
  Buffer, BitMem: TBytes;
{$ELSE}
  BitMem: Pointer;
{$ENDIF}
begin
  NewImage;
{$IF DEFINED(CLR)}
  SetLength(Buffer, SizeOf(WMF));
  Stream.Read(Buffer, SizeOf(WMF));
  WMF := TMetafileHeader(BytesToStructure(Buffer, TypeOf(TMetafileHeader)));
{$ELSE}
  Stream.Read(WMF, SizeOf(WMF));
{$ENDIF}
  if (WMF.Key <> WMFKEY) or (ComputeAldusChecksum(WMF) <> WMF.CheckSum) then
    InvalidMetafile;
  Dec(Length, SizeOf(WMF));
{$IF DEFINED(CLR)}
  SetLength(BitMem, Length);
{$ELSE}
  GetMem(Bitmem, Length);
{$ENDIF}
  with FImage do
  try
    Stream.Read(BitMem{$IFNDEF CLR}^{$ENDIF}, Length);
    FImage.FInch := WMF.Inch;
    if WMF.Inch = 0 then WMF.Inch := 96;
    FWidth := MulDiv(WMF.Box.Right - WMF.Box.Left, HundredthMMPerInch, WMF.Inch);
    FHeight := MulDiv(WMF.Box.Bottom - WMF.Box.Top, HundredthMMPerInch, WMF.Inch);
    with MFP do
    begin
      MM := MM_ANISOTROPIC;
      xExt := 0;
      yExt := 0;
      hmf := 0;
    end;
    FHandle := SetWinMetaFileBits(Length, BitMem, 0, MFP);
    if FHandle = 0 then InvalidMetafile;
    // Get the maximum extent actually used by the metafile output
    // and re-convert the wmf data using the new extents.
    // This helps preserve whitespace margins in WMFs
{$IF DEFINED(CLR)}
    GetEnhMetaFileHeader(FHandle, Marshal.SizeOf(TypeOf(EMFHeader)), EMFHeader);
{$ELSE}
    GetEnhMetaFileHeader(FHandle, SizeOf(EMFHeader), @EMFHeader);
{$ENDIF}
    with MFP, EMFHeader.rclFrame do
    begin
      MM := MM_ANISOTROPIC;
      xExt := Right;
      yExt := Bottom;
      hmf := 0;
    end;
    DeleteEnhMetafile(FHandle);
    FHandle := SetWinMetaFileBits(Length, BitMem, 0, MFP);
    if FHandle = 0 then InvalidMetafile;
    Enhanced := False;
  finally
{$IF NOT DEFINED(CLR)}
    Freemem(BitMem, Length);
{$ENDIF}
  end;
end;

procedure TMetafile.SaveToFile(const Filename: String);
var
  SaveEnh: Boolean;
begin
  SaveEnh := Enhanced;
  try
    if AnsiLowerCase(ExtractFileExt(Filename)) = '.wmf' then
      Enhanced := False;              { For 16 bit compatibility }
    inherited SaveToFile(Filename);
  finally
    Enhanced := SaveEnh;
  end;
end;

procedure TMetafile.SaveToStream(Stream: TStream);
begin
  if FImage <> nil then
    if Enhanced then
      WriteEMFStream(Stream)
    else
      WriteWMFStream(Stream);
end;

procedure TMetafile.SetHandle(Value: HENHMETAFILE);
var
  EnhHeader: TEnhMetaHeader;
begin
  if (Value <> 0) and
{$IF DEFINED(CLR)}
    (GetEnhMetafileHeader(Value, Marshal.SizeOf(TypeOf(EnhHeader)), EnhHeader) = 0) then
{$ELSE}
    (GetEnhMetafileHeader(Value, SizeOf(EnhHeader), @EnhHeader) = 0) then
{$ENDIF}
    InvalidMetafile;
  UniqueImage;
  if FImage.FHandle <> 0 then DeleteEnhMetafile(FImage.FHandle);
  InternalDeletePalette(FImage.FPalette);
  FImage.FPalette := 0;
  FImage.FHandle := Value;
  FImage.FTempWidth := 0;
  FImage.FTempHeight := 0;
  if Value <> 0 then
    with EnhHeader.rclFrame do
    begin
      FImage.FWidth := Right - Left;
      FImage.FHeight := Bottom - Top;
    end;
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.SetHeight(Value: Integer);
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
    if FInch = 0 then
      if FHandle = 0 then
        FTempHeight := Value
      else
      begin                 { convert device pixels to 0.01mm units }
{$IF DEFINED(CLR)}
        GetEnhMetaFileHeader(FHandle, Marshal.SizeOf(TypeOf(EMFHeader)), EMFHeader);
{$ELSE}
        GetEnhMetaFileHeader(FHandle, SizeOf(EMFHeader), @EMFHeader);
{$ENDIF}
        MMHeight := MulDiv(Value,                      { metafile height in pixels }
          EMFHeader.szlMillimeters.cy * 100,           { device height in 0.01mm }
          EMFHeader.szlDevice.cy);                     { device height in pixels }
      end
    else
      MMHeight := MulDiv(Value, HundredthMMPerInch, ScreenLogPixels);
end;

procedure TMetafile.SetInch(Value: Word);
begin
  if FImage = nil then NewImage;
  if FImage.FInch <> Value then
  begin
    UniqueImage;
    FImage.FInch := Value;
    Changed(Self);
  end;
end;

procedure TMetafile.SetMMHeight(Value: Integer);
begin
  if FImage = nil then NewImage;
  FImage.FTempHeight := 0;
  if FImage.FHeight <> Value then
  begin
    UniqueImage;
    FImage.FHeight := Value;
    Changed(Self);
  end;
end;

procedure TMetafile.SetMMWidth(Value: Integer);
begin
  if FImage = nil then NewImage;
  FImage.FTempWidth := 0;
  if FImage.FWidth <> Value then
  begin
    UniqueImage;
    FImage.FWidth := Value;
    Changed(Self);
  end;
end;

procedure TMetafile.SetTransparent(Value: Boolean);
begin
  // Ignore assignments to this property.
  // Metafiles must always be considered transparent.
end;

procedure TMetafile.SetWidth(Value: Integer);
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
    if FInch = 0 then
      if FHandle = 0 then
        FTempWidth := Value
      else
      begin                 { convert device pixels to 0.01mm units }
{$IF DEFINED(CLR)}
        GetEnhMetaFileHeader(FHandle, Marshal.SizeOf(TypeOf(EMFHeader)), EMFHeader);
{$ELSE}
        GetEnhMetaFileHeader(FHandle, SizeOf(EMFHeader), @EMFHeader);
{$ENDIF}
        MMWidth := MulDiv(Value,                      { metafile width in pixels }
          EMFHeader.szlMillimeters.cx * 100,          { device width in mm }
          EMFHeader.szlDevice.cx);                    { device width in pixels }
      end
    else
      MMWidth := MulDiv(Value, HundredthMMPerInch, ScreenLogPixels);
end;

class function TMetafile.TestEMF(Stream: TStream): Boolean;
{$IF DEFINED(CLR)}
var
  Size, HeaderSize: Longint;
  Buffer: TBytes;
  Header: TEnhMetaHeader;
begin
  Size := Stream.Size - Stream.Position;
  HeaderSize := SizeOf(Header);
  if Size > HeaderSize then
  begin
    SetLength(Buffer, HeaderSize);
    Stream.Read(Buffer, HeaderSize);
    Stream.Seek(-HeaderSize, soCurrent);
    Header := TEnhMetaHeader(BytesToStructure(Buffer, TypeOf(TEnhMetaHeader)));
  end;
  Result := (Size > SizeOf(Header)) and
    (Header.iType = EMR_HEADER) and (Header.dSignature = ENHMETA_SIGNATURE);
end;
{$ELSE}
var
  Size: Longint;
  Header: TEnhMetaHeader;
begin
  Size := Stream.Size - Stream.Position;
  if Size > Sizeof(Header) then
  begin
    Stream.Read(Header, Sizeof(Header));
    Stream.Seek(-Sizeof(Header), soFromCurrent);
  end;
  Result := (Size > Sizeof(Header)) and
    (Header.iType = EMR_HEADER) and (Header.dSignature = ENHMETA_SIGNATURE);
end;
{$ENDIF}

procedure TMetafile.UniqueImage;
var
  NewImage: TMetafileImage;
begin
  if FImage = nil then
    Self.NewImage
  else
    if FImage.FRefCount > 1 then
    begin
      NewImage:= TMetafileImage.Create;
      if FImage.FHandle <> 0 then
        NewImage.FHandle := CopyEnhMetafile(FImage.FHandle, nil);
      NewImage.FHeight := FImage.FHeight;
      NewImage.FWidth := FImage.FWidth;
      NewImage.FInch := FImage.FInch;
      NewImage.FTempWidth := FImage.FTempWidth;
      NewImage.FTempHeight := FImage.FTempHeight;
      FImage.Release;
      FImage := NewImage;
      FImage.Reference;
    end;
end;

procedure TMetafile.WriteData(Stream: TStream);
var
  SavePos: Longint;
begin
  if FImage <> nil then
  begin
    SavePos := 0;
    Stream.Write(SavePos, Sizeof(SavePos));
    SavePos := Stream.Position - Sizeof(SavePos);
    if Enhanced then
      WriteEMFStream(Stream)
    else
      WriteWMFStream(Stream);
    Stream.Seek(SavePos, soBeginning);
    SavePos := Stream.Size - SavePos;
    Stream.Write(SavePos, Sizeof(SavePos));
    Stream.Seek(0, soEnd);
  end;
end;

procedure TMetafile.WriteEMFStream(Stream: TStream);
var
{$IF DEFINED(CLR)}
  Buf: TBytes;
{$ELSE}
  Buf: Pointer;
{$ENDIF}
  Length: Longint;
begin
  if FImage = nil then Exit;
  Length := GetEnhMetaFileBits(FImage.FHandle, 0, nil);
  if Length = 0 then Exit;
{$IF DEFINED(CLR)}
  SetLength(Buf, Length);
{$ELSE}
  GetMem(Buf, Length);
{$ENDIF}
  try
    GetEnhMetaFileBits(FImage.FHandle, Length, Buf);
    Stream.WriteBuffer(Buf{$IFNDEF CLR}^{$ENDIF}, Length);
  finally
{$IF NOT DEFINED(CLR)}
    FreeMem(Buf, Length);
{$ENDIF}
  end;
end;

procedure TMetafile.WriteWMFStream(Stream: TStream);
var
  WMF: TMetafileHeader;
  Length: UINT;
  RefDC: HDC;
{$IF DEFINED(CLR)}
  Bits: TBytes;
{$ELSE}
  Bits: Pointer;
{$ENDIF}
begin
  if FImage = nil then Exit;
{$IF NOT DEFINED(CLR)}
  FillChar(WMF, SizeOf(WMF), 0);
{$ENDIF}
  with FImage do
  begin
    with WMF do
    begin
      Key := WMFKEY;
      if FInch = 0 then
        Inch := 96          { WMF defaults to 96 units per inch }
      else
        Inch := FInch;
      with Box do
      begin
        Right := MulDiv(FWidth, WMF.Inch, HundredthMMPerInch);
        Bottom := MulDiv(FHeight, WMF.Inch, HundredthMMPerInch);
      end;
      CheckSum := ComputeAldusChecksum(WMF);
    end;
    RefDC := GetDC(0);
    try
      Length := GetWinMetaFileBits(FHandle, 0, nil, MM_ANISOTROPIC, RefDC);
{$IF DEFINED(CLR)}
      SetLength(Bits, Length);
      if GetWinMetaFileBits(FHandle, Length, Bits, MM_ANISOTROPIC,
        RefDC) < Length then GDIError;
      Stream.WriteBuffer(StructureToBytes(TObject(WMF)),
        Marshal.SizeOf(TypeOf(TMetafileHeader)));
      Stream.WriteBuffer(Bits, Length);
{$ELSE}
      GetMem(Bits, Length);
      try
        if GetWinMetaFileBits(FHandle, Length, Bits, MM_ANISOTROPIC,
          RefDC) < Length then GDIError;
        Stream.WriteBuffer(WMF, SizeOf(WMF));
        Stream.WriteBuffer(Bits^, Length);
      finally
        FreeMem(Bits, Length);
      end;
{$ENDIF}
    finally
      ReleaseDC(0, RefDC);
    end;
  end;
end;

{$IFDEF CLR}[UIPermission(SecurityAction.LinkDemand, Clipboard=UIPermissionClipboard.AllClipboard)]{$ENDIF}
procedure TMetafile.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  EnhHeader: TEnhMetaHeader;
begin
  AData := GetClipboardData(CF_ENHMETAFILE); // OS will convert WMF to EMF
  if AData = 0 then
    InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SUnknownClipboardFormat);
  NewImage;
  with FImage do
  begin
    FHandle := CopyEnhMetafile(AData, nil);
{$IF DEFINED(CLR)}
    GetEnhMetaFileHeader(FHandle, Marshal.SizeOf(TypeOf(EnhHeader)), EnhHeader);
{$ELSE}
    GetEnhMetaFileHeader(FHandle, SizeOf(EnhHeader), @EnhHeader);
{$ENDIF}
    with EnhHeader.rclFrame do
    begin
      FWidth := Right - Left;
      FHeight := Bottom - Top;
    end;
    FInch := 0;
  end;
  Enhanced := True;
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

procedure TMetafile.SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
  var APalette: HPALETTE);
begin
  if FImage = nil then Exit;
  AFormat := CF_ENHMETAFILE;
  APalette := 0;
  AData := CopyEnhMetaFile(FImage.FHandle, nil);
end;

function TMetafile.ReleaseHandle: HENHMETAFILE;
begin
  UniqueImage;
  Result := FImage.FHandle;
  FImage.FHandle := 0;
end;

procedure TMetafile.SetSize(AWidth, AHeight: Integer);
var
  EMFHeader: TEnhMetaHeader;
begin
  if FImage = nil then NewImage;
  with FImage do
    if FInch = 0 then
      if FHandle = 0 then
      begin
        FTempWidth := AWidth;
        FTempHeight := AHeight;
      end
      else
      begin                 { convert device pixels to 0.01mm units }
{$IF DEFINED(CLR)}
        GetEnhMetaFileHeader(FHandle, Marshal.SizeOf(TypeOf(EMFHeader)), EMFHeader);
{$ELSE}
        GetEnhMetaFileHeader(FHandle, Sizeof(EMFHeader), @EMFHeader);
{$ENDIF}
        MMWidth := MulDiv(AWidth,                     { metafile width in pixels }
          EMFHeader.szlMillimeters.cx*100,            { device width in mm }
          EMFHeader.szlDevice.cx);                    { device width in pixels }
        MMHeight := MulDiv(AHeight,                   { metafile height in pixels }
          EMFHeader.szlMillimeters.cy*100,            { device height in 0.01mm }
          EMFHeader.szlDevice.cy);                    { device height in pixels }
      end
    else
    begin
      MMWidth := MulDiv(AWidth, HundredthMMPerInch, ScreenLogPixels);
      MMHeight := MulDiv(AHeight, HundredthMMPerInch, ScreenLogPixels);
    end;
end;

var
  BitmapCanvasList: TThreadList = nil;

{ TBitmapDC }

{$IF DEFINED(CLR)}
type
  TBitmapDC = class(TResHandleWrapper)
  private
    FOldBitmap: HBITMAP;
    FOldPalette: HPALETTE;
  strict protected
    procedure Finalize; override;
  public
    property OldBitmap: HBITMAP read FOldBitmap write FOldBitmap;
    property OldPalette: HPALETTE read FOldPalette write FOldPalette;
  end;
{$ENDIF}

{ TBitmapCanvas }

{ Create a canvas that gets its DC from the memory DC cache }
type
  TBitmapCanvas = class(TCanvas)
  private
    FBitmap: TBitmap;
{$IF DEFINED(CLR)}
    FBitmapDC: TBitmapDC;
    FChangeCount: Integer;
{$ELSE}
    FOldBitmap: HBITMAP;
    FOldPalette: HPALETTE;
{$ENDIF}
    procedure FreeContext;
  protected
{$IF DEFINED(CLR)}
    procedure Changed; override;
{$ENDIF}
    procedure CreateHandle; override;
  public
    constructor Create(ABitmap: TBitmap);
    destructor Destroy; override;
{$IF DEFINED(CLR)}
    function GetHashCode: Integer; override;
{$ENDIF}
  end;

{ FreeMemoryContexts is called by the VCL main winproc to release
  memory DCs after every message is processed (garbage collection).
  Only memory DCs not locked by other threads will be freed. }
procedure FreeMemoryContexts;
var
  I: Integer;
begin
  with BitmapCanvasList.LockList do
  try
    for I := Count-1 downto 0 do
    with TBitmapCanvas(Items[I]) do
      if TryLock then
      try
        FreeContext;
      finally
        Unlock;
      end;
  finally
    BitmapCanvasList.UnlockList;
  end;
end;

{ DeselectBitmap is called to ensure that a bitmap handle is not
  selected into any memory DC anywhere in the system.  If the bitmap
  handle is in use by a locked canvas, DeselectBitmap must wait for
  the canvas to unlock. }
procedure DeselectBitmap(AHandle: HBITMAP);
var
  I: Integer;
begin
  if AHandle = 0 then Exit;
  with BitmapCanvasList.LockList do
  try
    for I := Count - 1 downto 0 do
      with TBitmapCanvas(Items[I]) do
        if (FBitmap <> nil) and (FBitmap.FImage.FHandle = AHandle) then
          FreeContext;
  finally
    BitmapCanvasList.UnlockList;
  end;
end;

{ TBitmapDC }

{$IF DEFINED(CLR)}
procedure TBitmapDC.Finalize;
begin
  if Handle <> 0 then
  begin
    if FOldBitmap <> 0 then SelectObject(Handle, FOldBitmap);
    if FOldPalette <> 0 then SelectPalette(Handle, FOldPalette, True);
    SelectObject(Handle, StockPen);
    SelectObject(Handle, StockBrush);
    SelectObject(Handle, StockFont);
    DeleteDC(Handle);
    Handle := 0;
  end;
  inherited;
end;
{$ENDIF}

{ TBitmapCanvas }

{$IF DEFINED(CLR)}
procedure TBitmapCanvas.Changed;
begin
  inherited;
  Inc(FChangeCount);
end;
{$ENDIF}

constructor TBitmapCanvas.Create(ABitmap: TBitmap);
begin
  inherited Create;
{$IF DEFINED(CLR)}
  FBitmapDC := TBitmapDC.Create;
  FChangeCount := 0;
{$ENDIF}
  FBitmap := ABitmap;
end;

destructor TBitmapCanvas.Destroy;
begin
  FreeContext;
{$IF DEFINED(CLR)}
  System.GC.SuppressFinalize(FBitmapDC);
{$ENDIF}
  inherited Destroy;
end;

procedure TBitmapCanvas.FreeContext;
{$IF DEFINED(CLR)}
begin
  if FBitmapDC.Handle <> 0 then
  begin
    Lock;
    try
      if FBitmapDC.OldBitmap <> 0 then
        SelectObject(FBitmapDC.Handle, FBitmapDC.OldBitmap);
      if FBitmapDC.OldPalette <> 0 then
        SelectPalette(FBitmapDC.Handle, FBitmapDC.OldPalette, True);
      Self.Handle := 0;
      DeleteDC(FBitmapDC.Handle);
      BitmapCanvasList.RemoveItem(Self, TList.TDirection.FromEnd);
    finally
      Unlock;
    end;
  end;
{$ELSE}
var
  H: HBITMAP;
begin
  if FHandle <> 0 then
  begin
    Lock;
    try
      if FOldBitmap <> 0 then SelectObject(FHandle, FOldBitmap);
      if FOldPalette <> 0 then SelectPalette(FHandle, FOldPalette, True);
      H := FHandle;
      Handle := 0;
      DeleteDC(H);
      BitmapCanvasList.RemoveItem(Self, TList.TDirection.FromEnd);
    finally
      Unlock;
    end;
  end;
{$ENDIF}
end;

{$IF DEFINED(CLR)}
function TBitmapCanvas.GetHashCode: Integer;
begin
  Result := inherited GetHashCode xor (FChangeCount shl 1);
  if Assigned(Brush) then
    Result := Result xor (Brush.GetHashCode shl 2);
  if Assigned(Font) then
    Result := Result xor (Font.GetHashCode shl 3);
  if Assigned(Pen) then
    Result := Result xor (Pen.GetHashCode shl 4);
end;
{$ENDIF}

procedure TBitmapCanvas.CreateHandle;
{$IF NOT DEFINED(CLR)}
var
  H: HBITMAP;
{$ENDIF}
begin
  if FBitmap <> nil then
  begin
    Lock;
    try
      FBitmap.HandleNeeded;
      DeselectBitmap(FBitmap.FImage.FHandle);
                           
//!!      DeselectBitmap(FBitmap.FImage.FMaskHandle);
      FBitmap.PaletteNeeded;
{$IF DEFINED(CLR)}
      FBitmapDC.Handle := CreateCompatibleDC(0);
      if FBitmap.FImage.FHandle <> 0 then
        FBitmapDC.OldBitmap := SelectObject(FBitmapDC.Handle, FBitmap.FImage.FHandle)
      else
        FBitmapDC.OldBitmap := 0;
      Handle := FBitmapDC.Handle;
{$ELSE}
      H := CreateCompatibleDC(0);
      if FBitmap.FImage.FHandle <> 0 then
        FOldBitmap := SelectObject(H, FBitmap.FImage.FHandle) else
        FOldBitmap := 0;
      if FBitmap.FImage.FPalette <> 0 then
      begin
        FOldPalette := SelectPalette(H, FBitmap.FImage.FPalette, True);
        RealizePalette(H);
      end
      else
        FOldPalette := 0;
      Handle := H;
{$ENDIF}
      BitmapCanvasList.Add(Self);
    finally
      Unlock;
    end;
  end;
end;

{ TSharedImage }

destructor TSharedImage.Destroy;
begin
{$IF DEFINED(CLR)}
  FreeHandle;
  System.GC.SuppressFinalize(Self);
{$ENDIF}
  inherited Destroy;
end;

{$IF DEFINED(CLR)}
procedure TSharedImage.Finalize;
begin
  FreeHandle;
  inherited;
end;
{$ENDIF}

procedure TSharedImage.Reference;
begin
  Inc(FRefCount);
end;

procedure TSharedImage.Release;
begin
  if Self <> nil then
  begin
    Dec(FRefCount);
    if FRefCount = 0 then
    begin
      FreeHandle;
      Free;
    end;
  end;
end;

{ TBitmapImage }

destructor TBitmapImage.Destroy;
begin
{$IF NOT DEFINED(CLR)}
  if FDIBHandle <> 0 then
  begin
    DeselectBitmap(FDIBHandle);
    DeleteObject(FDIBHandle);
    FDIBHandle := 0;
  end;
{$ENDIF}
  FreeHandle;
{$IF NOT DEFINED(CLR)}
  if FDIB.dshSection <> 0 then
    CloseHandle(FDIB.dshSection);
{$ENDIF}
  FreeAndNil(FSaveStream);
  inherited Destroy;
end;

procedure TBitmapImage.FreeHandle;
begin
{$IF DEFINED(CLR)}
  if (FDIBHandle <> 0) and (FDIBHandle <> FHandle) then
  begin
    DeselectBitmap(FDIBHandle);
    DeleteObject(FDIBHandle);
    FDIBHandle := 0;
  end;
{$ENDIF}
  if FHandle <> 0 then
  begin
    DeselectBitmap(FHandle);
{$IF NOT DEFINED(CLR)}
    if FHandle <> FDIBHandle  then
{$ENDIF}
      DeleteObject(FHandle);
{$IF DEFINED(CLR)}
    if FHandle = FDIBHandle then FDIBHandle := 0;
    FHandle := 0;
{$ENDIF}
  end;
  if FMaskHandle <> 0 then
  begin
    DeselectBitmap(FMaskHandle);
    DeleteObject(FMaskHandle);
    FMaskHandle := 0;
  end;
{$IF DEFINED(CLR)}
  if FPalette <> 0 then
  begin
    DeleteObject(FPalette);
    FPalette := 0;
  end;
{$ELSE}
  InternalDeletePalette(FPalette);
  FHandle := 0;
  FPalette := 0;
{$ENDIF}
end;

{$IF DEFINED(CLR)}
function TBitmapImage.GetHashCode: Integer;
begin
  with FDIB.dsBm do
    Result := inherited GetHashCode xor (bmWidth shl 1) xor (bmHeight shl 2) xor
      (bmWidthBytes shl 3) xor (bmPlanes shl 4) xor (bmBitsPixel shl 5);
end;
{$ENDIF}

{ TBitmap }

{$IF NOT DEFINED(CLR)}
const
  { Mapping from color in DIB to system color }
  Grays: array[0..3] of TColor = (clWhite, clSilver, clGray, clBlack);
  SysGrays: array[0..3] of TColor = (clBtnHighlight, clBtnFace, clBtnShadow,
    clBtnText);
{$ENDIF}

{ This function will replace OldColors in Handle's colortable with NewColors and
  return a new DDB which uses that color table.  For bitmap's with more than
  256 colors (8bpp) this function returns the original bitmap. }
{$IF NOT DEFINED(CLR)}
function CreateMappedBmp(Handle: HBITMAP; const OldColors, NewColors: array of TColor): HBITMAP;
var
  Bitmap: PBitmapInfoHeader;
  ColorCount: Integer;
  BitmapInfoSize: DWORD;
  BitmapBitsSize: DWORD;
  Bits: Pointer;
  Colors: PRGBQuadArray;
  I, J: Integer;
  OldColor, NewColor: Integer;
  ScreenDC, DC: HDC;
  Save: HBITMAP;
begin
  Result := Handle;
  if Handle = 0 then Exit;
  InternalGetDIBSizes(Handle, BitmapInfoSize, BitmapBitsSize, 0);
  Bitmap := AllocMem(BitmapInfoSize + BitmapBitsSize);
  try
    Bits := Pointer(PByte(Bitmap) + BitmapInfoSize);
    InternalGetDIB(Handle, 0, Bitmap^, Bits^, 0);
    if Bitmap^.biBitCount <= 8 then
    begin
      ColorCount := 1 shl (Bitmap^.biBitCount);
      Colors := Pointer(PByte(Bitmap) + Bitmap^.biSize);
      ByteSwapColors(Colors^, ColorCount);
      for I := 0 to ColorCount - 1 do
        for J := Low(OldColors) to High(OldColors) do
        begin
          OldColor := ColorToRGB(OldColors[J]);
          if Integer(Colors[I]) = OldColor then
          begin
            NewColor := ColorToRGB(NewColors[J]);
            Integer(Colors[I]) := NewColor;
          end;
        end;
      ByteSwapColors(Colors^, ColorCount);
      ScreenDC := GetDC(0);
      try
        DC := CreateCompatibleDC(ScreenDC);
        if DC <> 0 then
          with Bitmap^ do
          begin
            Result := CreateCompatibleBitmap(ScreenDC, biWidth, biHeight);
            if Result <> 0 then
            begin
              Save := SelectObject(DC, Result);
              StretchDIBits(DC, 0, 0, biWidth, biHeight, 0, 0, biWidth, biHeight,
                Bits, PBitmapInfo(Bitmap)^, DIB_RGB_COLORS, SrcCopy);
              SelectObject(DC, Save);
            end;
          end;
          DeleteDC(DC);
      finally
        ReleaseDC(0, ScreenDC);
      end;
    end;
  finally
    FreeMem(Bitmap, BitmapInfoSize + BitmapBitsSize);
  end;
end;
{$ENDIF}

{ This function will create a new DDB from the bitmap resource, replacing
  OldColors in the colortable with NewColors.  If the bitmap resource has more
  than 256 colors (8bpp) this function returns the new DDB without color
  modifications. }
{$IF NOT DEFINED(CLR)}
function CreateMappedRes(Instance: THandle; ResName: PChar;
  const OldColors, NewColors: array of TColor): HBITMAP;
var
  Rsrc: HRSRC;
  Res: THandle;
  ColorCount: DWORD;
  BitmapInfoSize: Cardinal;
  Bitmap: PBitmapInfoHeader;
  BitmapInfo: PBitmapInfoHeader;
  Colors: PRGBQuadArray;
  I, J: Integer;
  OldColor, NewColor: Integer;
  Bits: Pointer;
  ScreenDC, DC: HDC;
  Save: HBITMAP;
  Temp: TBitmap;
begin
  Result := 0;
  Rsrc := FindResource(Instance, ResName, RT_BITMAP);
  if Rsrc = 0 then Exit;
  Res := LoadResource(Instance, Rsrc);
  try
    { Lock the bitmap and get a pointer to the color table. }
    Bitmap := LockResource(Res);
    if Bitmap <> nil then
    try
      if (Bitmap^.biBitCount * Bitmap^.biPlanes) <= 8 then
      begin
        ColorCount := 1 shl (Bitmap^.biBitCount);
        BitmapInfoSize := Bitmap^.biSize + ColorCount * SizeOf(TRGBQuad);
        GetMem(BitmapInfo, BitmapInfoSize);
        try
          Move(Bitmap^, BitmapInfo^, BitmapInfoSize);
          if Bitmap^.biBitCount <= 8 then
          begin
            Colors := Pointer(PByte(BitmapInfo) + BitmapInfo^.biSize);
            ByteSwapColors(Colors^, ColorCount);
            for I := 0 to ColorCount - 1 do
              for J := Low(OldColors) to High(OldColors) do
              begin
                OldColor := ColorToRGB(OldColors[J]);
                if Integer(Colors[I]) = OldColor then
                begin
                  NewColor := ColorToRGB(NewColors[J]);
                  Integer(Colors[I]) := NewColor;
                end;
              end;
            ByteSwapColors(Colors^, ColorCount);
          end;
          { First skip over the header structure and color table entries, if any. }
          Bits := Pointer(PByte(Bitmap) + BitmapInfoSize);
          { Create a color bitmap compatible with the display device. }
          ScreenDC := GetDC(0);
          try
            DC := CreateCompatibleDC(ScreenDC);
            if DC <> 0 then
              with BitmapInfo^ do
              begin
                Result := CreateCompatibleBitmap(ScreenDC, biWidth, biHeight);
                if Result <> 0 then
                begin
                  Save := SelectObject(DC, Result);
                  StretchDIBits(DC, 0, 0, biWidth, biHeight, 0, 0, biWidth, biHeight,
                    Bits, PBitmapInfo(BitmapInfo)^, DIB_RGB_COLORS, SrcCopy);
                  SelectObject(DC, Save);
                end;
              end;
              DeleteDC(DC);
          finally
            ReleaseDC(0, ScreenDC);
          end;
        finally
          FreeMem(BitmapInfo, BitmapInfoSize);
        end;
      end
      else
      begin
        Temp := TBitmap.Create;
        try
{$IFDEF MSWINDOWS}
          Temp.LoadFromResourceID(Instance, Integer(ResName)); // Ok for x64
{$ELSE}
          Temp.LoadFromResourceName(Instance, ResName);
{$ENDIF}
          Result := Temp.ReleaseHandle;
        finally
          Temp.Free;
        end;
      end;
    finally
      UnlockResource(Res);
    end;
  finally
    FreeResource(Res);
  end;
end;
{$ENDIF}

{ This function replaces the standard gray colors in a bitmap with the system
  grays (Grays, SysGrays). }
{$IF NOT DEFINED(CLR)}
function CreateGrayMappedBmp(Handle: HBITMAP): HBITMAP;
begin
  Result := CreateMappedBmp(Handle, Grays, SysGrays);
end;
{$ENDIF}

{ This function replaces the standard gray colors in a bitmap resource with the
  system grays (Grays, SysGrays). }
{$IF NOT DEFINED(CLR)}
function CreateGrayMappedRes(Instance: THandle; ResName: PChar): HBITMAP;
begin
  Result := CreateMappedRes(Instance, ResName, Grays, SysGrays);
end;
{$ENDIF}

procedure UpdateDIBColorTable(DIBHandle: HBITMAP; Pal: HPalette;
  const DIB: TDIBSection);
var
  ScreenDC, DC: HDC;
  OldBM: HBitmap;
  ColorCount: Integer;
{$IF DEFINED(CLR)}
  Colors: array [Byte] of COLORREF;
{$ELSE}
  Colors: array [Byte] of TRGBQuad;
{$ENDIF}
begin
  if (DIBHandle <> 0) and (DIB.dsbmih.biBitCount <= 8) then
  begin
    ColorCount := PaletteToDIBColorTable(Pal, Colors);
    if ColorCount = 0 then Exit;
    ScreenDC := GetDC(0);
    DC := CreateCompatibleDC(ScreenDC);
    OldBM := SelectObject(DC, DIBHandle);
    try
      SetDIBColorTable(DC, 0, ColorCount, Colors);
    finally
      SelectObject(DC, OldBM);
      DeleteDC(DC);
      ReleaseDC(0, ScreenDC);
    end;
  end;
end;

procedure FixupBitFields(var DIB: TDIBSection);
begin
  if (DIB.dsbmih.biCompression and BI_BITFIELDS <> 0) and
    (DIB.dsBitFields[0] = 0) then
    if DIB.dsbmih.biBitCount = 16 then
    begin
      // fix buggy 16 bit color drivers
      DIB.dsBitFields[0] := $F800;
      DIB.dsBitFields[1] := $07E0;
      DIB.dsBitFields[2] := $001F;
    end else if DIB.dsbmih.biBitCount = 32 then
    begin
      // fix buggy 32 bit color drivers
      DIB.dsBitFields[0] := $00FF0000;
      DIB.dsBitFields[1] := $0000FF00;
      DIB.dsBitFields[2] := $000000FF;
    end;
end;

function CopyBitmap(Handle: HBITMAP; OldPalette, NewPalette: HPALETTE;
  var DIB: TDIBSection; Canvas: TCanvas): HBITMAP;
var
  OldScr, NewScr: HBITMAP;
  ScreenDC, NewImageDC, OldImageDC: HDC;
  SrcDIB: TDIBSection;
  Pal1, Pal2: HPalette;
{$IF DEFINED(CLR)}
  BI: TBitmapInfo;
  BitsMem: IntPtr;
  MonoColors: array [0..1] of COLORREF;
{$ELSE}
  BI: PBitmapInfo;
  BitsMem: Pointer;
  MonoColors: array [0..1] of Integer;
{$ENDIF}
begin
  Result := 0;
  with DIB, dsbm, dsbmih do
  begin
    if (biSize <> 0) and ((biWidth = 0) or (biHeight = 0)) then Exit;
    if (biSize = 0) and ((bmWidth = 0) or (bmHeight = 0)) then Exit;
  end;

  DeselectBitmap(Handle);

  SrcDIB.dsbmih.biSize := 0;
  if Handle <> 0 then
{$IF DEFINED(CLR)}
    if GetObject(Handle, Marshal.SizeOf(TypeOf(SrcDIB)), SrcDIB) < SizeOf(SrcDIB.dsbm) then
{$ELSE}
    if GetObject(Handle, SizeOf(SrcDIB), @SrcDIB) < SizeOf(SrcDIB.dsbm) then
{$ENDIF}
      InvalidBitmap;

  ScreenDC := GDICheck(GetDC(0));
  NewImageDC := GDICheck(CreateCompatibleDC(ScreenDC));
  with DIB.dsbm do
  try
    if DIB.dsbmih.biSize < DWORD(SizeOf(DIB.dsbmih)) then
      if (bmPlanes or bmBitsPixel) = 1 then // monochrome
        Result := GDICheck(CreateBitmap(bmWidth, bmHeight, 1, 1, nil))
      else  // Create DDB
        Result := GDICheck(CreateCompatibleBitmap(ScreenDC, bmWidth, bmHeight))
    else  // Create DIB
    begin
{$IF NOT DEFINED(CLR)}
      GetMem(BI, SizeOf(TBitmapInfo) + 256 * SizeOf(TRGBQuad));
{$ENDIF}
      with DIB.dsbmih do
      try
        biSize := SizeOf(BI.bmiHeader);
        biPlanes := 1;
{$IF DEFINED(CLR)}
        if bmBitsPixel = 0 then
          biBitCount := GetDeviceCaps(ScreenDC, BITSPIXEL) * GetDeviceCaps(ScreenDC, PLANES);
        BI.bmiHeader := DIB.dsbmih;
        biWidth := bmWidth;
        biHeight := bmHeight;
{$ELSE}
        if biBitCount = 0 then
          biBitCount := GetDeviceCaps(ScreenDC, BITSPIXEL) * GetDeviceCaps(ScreenDC, PLANES);
        BI.bmiHeader := DIB.dsbmih;
        bmWidth := biWidth;
        bmHeight := biHeight;
{$ENDIF}

        if (biBitCount <= 8) then
        begin
          if (biBitCount = 1) and ((Handle = 0) or (SrcDIB.dsbm.bmBits = nil)) then
          begin  // set mono DIB to white/black when converting from DDB.
{$IF DEFINED(CLR)}
            BI.bmiColors[0] := 0;
            BI.bmiColors[1] := $FFFFFF;
{$ELSE}
            Integer(BI^.bmiColors[0]) := 0;
            PInteger(PByte(@BI^.bmiColors) + SizeOf(Integer))^ := $FFFFFF;
{$ENDIF}
          end
          else if (NewPalette <> 0) then
{$IF DEFINED(CLR)}
            PaletteToDIBColorTable(NewPalette, BI.bmiColors)
{$ELSE}
            PaletteToDIBColorTable(NewPalette, PRGBQuadArray(@BI.bmiColors)^)
{$ENDIF}
          else if Handle <> 0 then
          begin
            NewScr := SelectObject(NewImageDC, Handle);
{$IF DEFINED(CLR)}
            if SrcDIB.dsbm.bmBits <> nil then
              biClrUsed := GetDIBColorTable(NewImageDC, 0, 1 shl biBitCount, BI.bmiColors)
            else
              GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), nil, BI, DIB_RGB_COLORS);
{$ELSE}
            if (SrcDIB.dsbmih.biSize > 0) and (SrcDIB.dsbm.bmBits <> nil) then
              biClrUsed := GetDIBColorTable(NewImageDC, 0, 256, BI^.bmiColors)
            else
              GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), nil, BI^, DIB_RGB_COLORS);
{$ENDIF}
            SelectObject(NewImageDC, NewScr);
          end;
        end
        else if ((biBitCount = 16) or (biBitCount = 32)) and
          ((biCompression and BI_BITFIELDS) <> 0) then
        begin
          FixupBitFields(DIB);
{$IF DEFINED(CLR)}
          BI.bmiColors[0] := DIB.dsBitFields[0];
          BI.bmiColors[1] := DIB.dsBitFields[1];
          BI.bmiColors[2] := DIB.dsBitFields[2];
{$ELSE}
          Move(DIB.dsBitFields, BI.bmiColors, SizeOf(DIB.dsBitFields));
{$ENDIF}
        end;

        Result := GDICheck(CreateDIBSection(ScreenDC, BI{$IFNDEF CLR}^{$ENDIF}, DIB_RGB_COLORS, BitsMem, 0, 0));
        if (BitsMem = nil) then GDIError;

        if (Handle <> 0) and (SrcDIB.dsbm.bmWidth = biWidth) and
          (SrcDIB.dsbm.bmHeight = biHeight) and (biBitCount > 8) then
        begin    // shortcut bitblt steps
          GetDIBits(NewImageDC, Handle, 0, Abs(biHeight), BitsMem, BI{$IFNDEF CLR}^{$ENDIF}, DIB_RGB_COLORS);
          Exit;
        end;
      finally
{$IF NOT DEFINED(CLR)}
        FreeMem(BI);
{$ENDIF}
      end;
    end;

    GDICheck(Result);
    NewScr := GDICheck(SelectObject(NewImageDC, Result));
    try
      try
        Pal1 := 0;
        Pal2 := 0;
        if NewPalette <> 0 then
        begin
          Pal1 := SelectPalette(NewImageDC, NewPalette, False);
          RealizePalette(NewImageDC);
        end;
        try
          if Canvas <> nil then
          begin
            FillRect(NewImageDC, Rect(0, 0, bmWidth, bmHeight),
              Canvas.Brush.Handle);
            SetTextColor(NewImageDC, ColorToRGB(Canvas.Font.Color));
            SetBkColor(NewImageDC, ColorToRGB(Canvas.Brush.Color));
            if (DIB.dsbmih.biBitCount = 1) and (DIB.dsbm.bmBits <> nil) then
            begin
              MonoColors[0] := ColorToRGB(Canvas.Font.Color);
              MonoColors[1] := ColorToRGB(Canvas.Brush.Color);
              SetDIBColorTable(NewImageDC, 0, 2, MonoColors);
            end;
          end
          else
           ;// PatBlt(NewImageDC, 0, 0, bmWidth, bmHeight, WHITENESS);

          if Handle <> 0 then
          begin
            OldImageDC := GDICheck(CreateCompatibleDC(ScreenDC));
            try
              OldScr := GDICheck(SelectObject(OldImageDC, Handle));
              if OldPalette <> 0 then
              begin
                Pal2 := SelectPalette(OldImageDC, OldPalette, False);
                RealizePalette(OldImageDC);
              end;
              if Canvas <> nil then
              begin
                SetTextColor(OldImageDC, ColorToRGB(Canvas.Font.Color));
                SetBkColor(OldImageDC, ColorToRGB(Canvas.Brush.Color));
              end;
              BitBlt(NewImageDC, 0, 0, bmWidth, bmHeight, OldImageDC, 0, 0, SRCCOPY);
              if OldPalette <> 0 then
                SelectPalette(OldImageDC, Pal2, True);
              GDICheck(SelectObject(OldImageDC, OldScr));
            finally
              DeleteDC(OldImageDC);
            end;
          end;
        finally
          if NewPalette <> 0 then
            SelectPalette(NewImageDC, Pal1, True);
        end;
      finally
        SelectObject(NewImageDC, NewScr);
      end;
    except
      DeleteObject(Result);
      raise;
    end;
  finally
    DeleteDC(NewImageDC);
    ReleaseDC(0, ScreenDC);
    if (Result <> 0) then
{$IF DEFINED(CLR)}
      GetObject(Result, Marshal.SizeOf(TypeOf(DIB)), DIB);
{$ELSE}
      GetObject(Result, SizeOf(DIB), @DIB);
{$ENDIF}
  end;
end;

function CopyPalette(Palette: HPALETTE): HPALETTE;
var
  PaletteSize: Integer;
  LogPal: TMaxLogPalette;
begin
  Result := 0;
  if Palette = 0 then
    Exit;
  PaletteSize := 0;
  if GetObject(Palette, SizeOf(PaletteSize), {$IFNDEF CLR}@{$ENDIF}PaletteSize) = 0 then
    Exit;
  if PaletteSize = 0 then
    Exit;
  with LogPal do
  begin
    palVersion := $0300;
    palNumEntries := PaletteSize;
    GetPaletteEntries(Palette, 0, PaletteSize, palPalEntry);
  end;
{$IF DEFINED(CLR)}
  Result := CreatePalette(LogPal);
{$ELSE}
  Result := CreatePalette(PLogPalette(@LogPal)^);
{$ENDIF}
end;

function CopyBitmapAsMask(Handle: HBITMAP; Palette: HPALETTE;
  TransparentColor: TColorRef): HBITMAP;
var
  DIB: TDIBSection;
  ScreenDC, BitmapDC, MonoDC: HDC;
  BkColor: TColorRef;
  Remove: Boolean;
  SaveBitmap, SaveMono: HBITMAP;
begin
  Result := 0;
{$IF DEFINED(CLR)}
  if (Handle <> 0) and (GetObject(Handle, Marshal.SizeOf(TypeOf(DIB)), DIB) <> 0) then
{$ELSE}
  if (Handle <> 0) and (GetObject(Handle, SizeOf(DIB), @DIB) <> 0) then
{$ENDIF}
  begin
    DeselectBitmap(Handle);
    ScreenDC := 0;
    MonoDC := 0;
    try
      ScreenDC := GDICheck(GetDC(0));
      MonoDC := GDICheck(CreateCompatibleDC(ScreenDC));
      with DIB, dsBm do
      begin
        Result := CreateBitmap(bmWidth, bmHeight, 1, 1, nil);
        if Result <> 0 then
        begin
          SaveMono := SelectObject(MonoDC, Result);
          if TransparentColor = TColorRef(clNone) then
            PatBlt(MonoDC, 0, 0, bmWidth, bmHeight, Blackness)
          else
          begin
            BitmapDC := GDICheck(CreateCompatibleDC(ScreenDC));
            try
              { Convert DIB to DDB }
              if bmBits <> nil then
              begin
                Remove := True;
                DIB.dsbmih.biSize := 0;
                Handle := CopyBitmap(Handle, Palette, Palette, DIB, nil);
              end
              else Remove := False;
              SaveBitmap := SelectObject(BitmapDC, Handle);
              if Palette <> 0 then
              begin
                SelectPalette(BitmapDC, Palette, False);
                RealizePalette(BitmapDC);
                SelectPalette(MonoDC, Palette, False);
                RealizePalette(MonoDC);
              end;
              BkColor := SetBkColor(BitmapDC, TransparentColor);
              BitBlt(MonoDC, 0, 0, bmWidth, bmHeight, BitmapDC, 0, 0, SrcCopy);
              SetBkColor(BitmapDC, BkColor);
              if SaveBitmap <> 0 then SelectObject(BitmapDC, SaveBitmap);
              if Remove then DeleteObject(Handle);
            finally
              DeleteDC(BitmapDC);
            end;
          end;
          if SaveMono <> 0 then SelectObject(MonoDC, SaveMono);
        end;
      end;
    finally
      if MonoDC <> 0 then DeleteDC(MonoDC);
      if ScreenDC <> 0 then ReleaseDC(0, ScreenDC);
    end;
  end;
end;

constructor TBitmap.Create;
begin
  inherited Create;
  FTransparentColor := clDefault;
  FAlphaFormat := afIgnored;
  FImage := TBitmapImage.Create;
  FImage.Reference;
  if DDBsOnly then HandleType := bmDDB;
end;

constructor TBitmap.Create(AWidth, AHeight: Integer);
begin
  Create;
  SetSize(AWidth, AHeight);
end;

destructor TBitmap.Destroy;
begin
  FreeContext;
  FImage.Release;
  FCanvas.Free;
  inherited Destroy;
end;

procedure TBitmap.Assign(Source: TPersistent);
var
  DIB: TDIBSection;
{$IF DEFINED(CLR)}
  NewHandle: HBITMAP;
  NewPalette: HPALETTE;
{$ENDIF}
begin
  if (Source = nil) or (Source is TBitmap) then
  begin
    FreeContext;
{$IF DEFINED(CLR)}
    System.Threading.Monitor.Enter(BitmapImageLock);
{$ELSE}
    EnterCriticalSection(BitmapImageLock);
{$ENDIF}
    try
      if Source <> nil then
      begin
        TBitmap(Source).FImage.Reference;
        FImage.Release;
        FImage := TBitmap(Source).FImage;
        FTransparent := TBitmap(Source).FTransparent;
        FTransparentColor := TBitmap(Source).FTransparentColor;
        FTransparentMode := TBitmap(Source).FTransparentMode;
        PixelFormat := TBitmap(Source).PixelFormat;
        FAlphaFormat := TBitmap(Source).AlphaFormat;
      end
      else
{$IF DEFINED(CLR)}
        NewImage(0, 0, DIB);
{$ELSE}
      begin
        FillChar(DIB, Sizeof(DIB), 0);
        NewImage(0, 0, DIB, False);
      end;
{$ENDIF}
    finally
{$IF DEFINED(CLR)}
      System.Threading.Monitor.Exit(BitmapImageLock);
{$ELSE}
      LeaveCriticalSection(BitmapImageLock);
{$ENDIF}
    end;
    PaletteModified := Palette <> 0;
    Changed(Self);
  end
  else
  begin
{$IF DEFINED(CLR)}
    if Source is System.Drawing.Bitmap then
    begin
      FreeContext;
      InternalLoadFromBitmap(System.Drawing.Bitmap(Source), NewHandle, NewPalette, DIB);
      NewImage(NewHandle, NewPalette, DIB, System.Drawing.Bitmap(Source).RawFormat);
      PaletteModified := Palette <> 0;
      Changed(Self);
    end
    else
{$ENDIF}
      inherited Assign(Source);
  end;
end;

procedure TBitmap.CopyImage(AHandle: HBITMAP; APalette: HPALETTE; DIB: TDIBSection);
var
  NewHandle, NewPalette: THandle;
begin
  FreeContext;
  NewHandle := 0;
  NewPalette := 0;
  try
{$IF DEFINED(CLR)}
    if THandle(APalette) = SystemPalette16.Handle then
{$ELSE}
    if APalette = SystemPalette16 then
{$ENDIF}
      NewPalette := APalette
    else
      NewPalette := CopyPalette(APalette);
    NewHandle := CopyBitmap(AHandle, APalette, NewPalette, DIB, FCanvas);
{$IF DEFINED(CLR)}
    NewImage(NewHandle, NewPalette, DIB, FImage.FImageFormat);
{$ELSE}
    NewImage(NewHandle, NewPalette, DIB, FImage.FOS2Format);
{$ENDIF}
  except
    InternalDeletePalette(NewPalette);
    if NewHandle <> 0 then DeleteObject(NewHandle);
    raise;
  end;
end;

{ Called by the FCanvas whenever an operation is going to be performed on the
  bitmap that would modify it.  Since modifications should only affect this
  TBitmap, the handle needs to be 'cloned' if it is being refered to by more
  than one TBitmap }
procedure TBitmap.Changing(Sender: TObject);
begin
  FreeImage;
  FImage.FDIB.dsbmih.biClrUsed := 0;
  FImage.FDIB.dsbmih.biClrImportant := 0;
  FreeAndNil(FImage.FSaveStream);
end;

procedure TBitmap.Changed(Sender: TObject);
begin
  FMaskBitsValid := False;
  inherited Changed(Sender);
end;

procedure TBitmap.Dormant;
var
  S: TMemoryStream;
  DIB: TDIBSection;
begin
  S := TMemoryStream.Create;
  SaveToStream(S);
  S.Size := S.Size;  // compact to minimum buffer
  DIB := FImage.FDIB;
  DIB.dsBm.bmBits := nil;
  FreeContext; // InternalDeletePalette requires this
  FreeAndNil(FCanvas);
{$IF DEFINED(CLR)}
  NewImage(0, 0, DIB, FImage.FImageFormat, S);
{$ELSE}
  NewImage(0, 0, DIB, FImage.FOS2Format, S);
{$ENDIF}
end;

procedure TBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
  MaskDC: HDC;
  Save: THandle;
  BF: TBlendFunction;
  OldStretchBltMode: Integer;
begin
  with Rect, FImage do
  begin
    ACanvas.RequiredState(csAllValid);
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if FPalette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.FHandle, FPalette, True);
      RealizePalette(ACanvas.FHandle);
      RestorePalette := True;
    end;

    OldStretchBltMode := GetStretchBltMode(ACanvas.Handle);
    BPP := GetDeviceCaps(ACanvas.FHandle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.FHandle, PLANES);
    DoHalftone := (BPP <= 8) and (BPP < (FDIB.dsBm.bmBitsPixel * FDIB.dsBm.bmPlanes));
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.FHandle, pt);
      SetStretchBltMode(ACanvas.FHandle, HALFTONE);
      SetBrushOrgEx(ACanvas.FHandle, pt.x, pt.y, {$IFNDEF CLR}@{$ENDIF}pt);
    end
    else
      if not Monochrome then
        SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      { Call MaskHandleNeeded prior to creating the canvas handle since
        it causes FreeContext to be called. }
      if Transparent then MaskHandleNeeded;
      Canvas.RequiredState(csAllValid);
      if Transparent then
      begin
        Save := 0;
        MaskDC := 0;
        try
          MaskDC := GDICheck(CreateCompatibleDC(0));
          Save := SelectObject(MaskDC, FMaskHandle);
          TransparentStretchBlt(ACanvas.FHandle, Left, Top, Right - Left,
            Bottom - Top, Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth,
            FDIB.dsbm.bmHeight, MaskDC, 0, 0);
        finally
          if Save <> 0 then SelectObject(MaskDC, Save);
          if MaskDC <> 0 then DeleteDC(MaskDC);
        end;
      end
      else if SupportsPartialTransparency then
      begin
        //We have a 32 bit bitmap with alpha, render with AlphaBlend
        BF.BlendOp := AC_SRC_OVER;
        BF.BlendFlags := 0;
        BF.SourceConstantAlpha := 255;
        BF.AlphaFormat := AC_SRC_ALPHA;

        Winapi.Windows.AlphaBlend(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth, FDIB.dsbm.bmHeight, BF);
      end
      else
        StretchBlt(ACanvas.FHandle, Left, Top, Right - Left, Bottom - Top,
          Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth,
          FDIB.dsbm.bmHeight, ACanvas.CopyMode);
    finally
      if RestorePalette then
        SelectPalette(ACanvas.FHandle, OldPalette, True);
      SetStretchBltMode(ACanvas.FHandle, OldStretchBltMode);
    end;
  end;
end;

procedure TBitmap.DrawTransparent(ACanvas: TCanvas; const Rect: TRect;
  Opacity: Byte);
var
  OldPalette: HPalette;
  RestorePalette: Boolean;
  DoHalftone: Boolean;
  Pt: TPoint;
  BPP: Integer;
  MaskDC: HDC;
  Save: THandle;
  BF: TBlendFunction;
begin
  with Rect, FImage do
  begin
    ACanvas.RequiredState(csAllValid);
    PaletteNeeded;
    OldPalette := 0;
    RestorePalette := False;

    if FPalette <> 0 then
    begin
      OldPalette := SelectPalette(ACanvas.FHandle, FPalette, True);
      RealizePalette(ACanvas.FHandle);
      RestorePalette := True;
    end;

    BPP := GetDeviceCaps(ACanvas.FHandle, BITSPIXEL) *
      GetDeviceCaps(ACanvas.FHandle, PLANES);
    DoHalftone := (BPP <= 8) and (BPP < (FDIB.dsBm.bmBitsPixel * FDIB.dsBm.bmPlanes));
    if DoHalftone then
    begin
      GetBrushOrgEx(ACanvas.FHandle, pt);
      SetStretchBltMode(ACanvas.FHandle, HALFTONE);
      SetBrushOrgEx(ACanvas.FHandle, pt.x, pt.y, {$IFNDEF CLR}@{$ENDIF}pt);
    end
    else
      if not Monochrome then
        SetStretchBltMode(ACanvas.Handle, STRETCH_DELETESCANS);
    try
      { Call MaskHandleNeeded prior to creating the canvas handle since
        it causes FreeContext to be called. }
      if Transparent then MaskHandleNeeded;
      Canvas.RequiredState(csAllValid);
      if Transparent then
      begin
        Save := 0;
        MaskDC := 0;
        try
          MaskDC := GDICheck(CreateCompatibleDC(0));
          Save := SelectObject(MaskDC, FMaskHandle);
          TransparentStretchBlt(ACanvas.FHandle, Left, Top, Right - Left,
            Bottom - Top, Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth,
            FDIB.dsbm.bmHeight, MaskDC, 0, 0);
        finally
          if Save <> 0 then SelectObject(MaskDC, Save);
          if MaskDC <> 0 then DeleteDC(MaskDC);
        end;
      end
      else if FDIB.dsBmih.biBitCount = 32 then
      begin
        //We have a 32 bit bitmap, render with AlphaBlend
        BF.BlendOp := AC_SRC_OVER;
        BF.BlendFlags := 0;
        BF.SourceConstantAlpha := Opacity;
        BF.AlphaFormat := AC_SRC_ALPHA;

        Winapi.Windows.AlphaBlend(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth, FDIB.dsbm.bmHeight, BF);
      end
      else
      begin
        BF.BlendOp := AC_SRC_OVER;
        BF.BlendFlags := 0;
        BF.SourceConstantAlpha := Opacity;
        BF.AlphaFormat := 0;

        Winapi.Windows.AlphaBlend(ACanvas.Handle, Left, Top, Right - Left, Bottom - Top,
          Canvas.FHandle, 0, 0, FDIB.dsbm.bmWidth, FDIB.dsbm.bmHeight, BF);
      end;
    finally
      if RestorePalette then
        SelectPalette(ACanvas.FHandle, OldPalette, True);
    end;
  end;
end;

{ FreeImage:
  If there are multiple references to the image, create a unique copy of the image.
  If FHandle = FDIBHandle, the DIB memory will be updated when the drawing
  handle is drawn upon, so no changes are needed to maintain image integrity.
  If FHandle <> FDIBHandle, the DIB will not track with changes made to
  the DDB, so destroy the DIB handle (but keep the DIB pixel format info).  }

procedure TBitmap.FreeImage;
var
  P: HPalette;
begin
  with FImage do
    if FRefCount > 1 then
    begin
      HandleNeeded;
      if FHalftone then
        P := 0
      else
        P := FPalette;
      CopyImage(FHandle, P, FDIB)
    end
    else
      if (FHandle <> 0) and (FHandle <> FDIBHandle) then
      begin
        if FDIBHandle <> 0 then
          if not DeleteObject(FDIBHandle) then GDIError;
        FDIBHandle := 0;
        FDIB.dsbm.bmBits := nil;
      end;
end;

{$IF DEFINED(CLR)}
function TBitmap.GetHashCode: Integer;
begin
  Result := inherited GetHashCode xor (FImage.GetHashCode shl 1);
  if Assigned(FCanvas) then
    Result := Result xor (FCanvas.GetHashCode shl 2);
end;
{$ENDIF}

function TBitmap.GetEmpty: Boolean;
begin
  with FImage do
    Result := (FHandle = 0) and (FDIBHandle = 0) and (FSaveStream = nil);
end;

function TBitmap.GetCanvas: TCanvas;
begin
  if FCanvas = nil then
  begin
    HandleNeeded;
    if FCanvas = nil then    // possible recursion
    begin
      FCanvas := TBitmapCanvas.Create(Self);
      FCanvas.OnChange := Changed;
      FCanvas.OnChanging := Changing;
    end;
  end;
  Result := FCanvas;
end;

{ Since the user might modify the contents of the HBITMAP it must not be
  shared by another TBitmap when given to the user nor should it be selected
  into a DC. }
function TBitmap.GetHandle: HBITMAP;
begin
  FreeContext;
  HandleNeeded;
  Changing(Self);
  Result := FImage.FHandle;
end;

function TBitmap.HandleAllocated: Boolean;
begin
  Result := Assigned(FImage) and (FImage.FHandle <> 0);
end;

function TBitmap.GetHandleType: TBitmapHandleType;
begin
  with FImage do
  begin
    if (FHandle = 0) or (FHandle = FDIBHandle) then
      if FDIBHandle = 0 then
        if FDIB.dsbmih.biSize = 0 then
          Result := bmDDB
        else
          Result := bmDIB
      else
        Result := bmDIB
    else
      Result := bmDDB;
  end;
end;

function TBitmap.GetHeight: Integer;
begin
  Result := Abs(FImage.FDIB.dsBm.bmHeight);
end;

{$IF DEFINED(CLR)}
function TBitmap.GetImageFormat: TImageFormat;
begin
  Result := FImage.FImageFormat;
end;
{$ENDIF}

function TBitmap.GetMaskHandle: HBITMAP;
begin
  MaskHandleNeeded;
  Result := FImage.FMaskHandle;
end;

function TBitmap.GetMonochrome: Boolean;
begin
  with FImage.FDIB.dsBm do
    Result := (bmPlanes = 1) and (bmBitsPixel = 1);
end;

function TBitmap.GetPalette: HPALETTE;
begin
  PaletteNeeded;
  Result := FImage.FPalette;
end;

function TBitmap.GetPixelFormat: TPixelFormat;
begin
  Result := pfCustom;
  if HandleType = bmDDB then
    Result := pfDevice
  else
{$IF DEFINED(CLR)}
    with FImage.FDIB, dsBm, dsbmih do
      case bmBitsPixel of
{$ELSE}
    with FImage.FDIB, dsbmih do
      case biBitCount of
{$ENDIF}
        1: Result := pf1Bit;
        4: Result := pf4Bit;
        8: Result := pf8Bit;
       16: case biCompression of
             BI_RGB : Result := pf15Bit;
             BI_BITFIELDS: if dsBitFields[1] = $7E0 then Result := pf16Bit;
           end;
       24: Result := pf24Bit;
       32: if biCompression = BI_RGB then Result := pf32Bit;
      end;
end;

{$IF DEFINED(CLR)}
function TBitmap.GetScanLine(Row: Integer): IntPtr;
begin
  Changing(Self);
  with FImage.FDIB.dsBm do
  begin
    if (Row < 0) or (Row >= bmHeight) then
      InvalidOperation(SScanLine);
    DIBNeeded;
    GDIFlush;
    Row := bmHeight - Row - 1; // Always bottom-up DIB
    Result := IntPtr.Create(bmBits.ToInt64 +
      Row * BytesPerScanline(bmWidth, bmBitsPixel, 32));
  end;
end;
{$ELSE}
function TBitmap.GetScanLine(Row: Integer): Pointer;
begin
  Changing(Self);
  with FImage.FDIB, dsbm, dsbmih do
  begin
    if (Row < 0) or (Row >= bmHeight) then
      InvalidOperation(@SScanLine);
    DIBNeeded;
    GDIFlush;
    if biHeight > 0 then  // bottom-up DIB
      Row := biHeight - Row - 1;
    Result := PByte(bmBits) +
      Row * BytesPerScanline(biWidth, biBitCount, 32);
  end;
end;
{$ENDIF}

function TBitmap.GetSupportsPartialTransparency: Boolean;
begin
  Result := (FImage.FDIB.dsBMIh.biBitCount = 32) and (FAlphaFormat in [afDefined, afPremultiplied]);
end;

procedure TBitmap.PreMultiplyAlpha;
var
  Alpha: Word;
{$IFNDEF CLR}
  ImageData: PByte;
{$ELSE}
  ImageData: IntPtr;
{$ENDIF}
begin
  if (FImage.FDIB.dsBmih.biBitCount = 32) then //Premultiply the alpha into the color
  begin
    if FImage.FRefCount > 1 then
    with FImage do
    begin
      HandleNeeded;
      CopyImage(FHandle, FPalette, FDIB);
    end;
    if FImage.FDIB.dsBm.bmBits = nil then
      Exit;
    ImageData := FImage.FDIB.dsBm.bmBits;
    while UIntPtr(ImageData) < UIntPtr(FImage.FDIB.dsBm.bmBits) + FImage.FDIB.dsBmih.biSizeImage do
    begin
{$IFNDEF CLR}
      Alpha := PByte(ImageData + 3)^;
      PByte(ImageData)^ := MulDiv(PByte(ImageData)^, Alpha, 255);
      PByte(ImageData + 1)^ := MulDiv(PByte(ImageData + 1)^, Alpha, 255);
      PByte(ImageData + 2)^ := MulDiv(PByte(ImageData + 2)^, Alpha, 255);
      Inc(ImageData, 4);
{$ELSE}
      Alpha := Marshal.ReadByte(ImageData, 3);
      Marshal.WriteByte(ImageData, 0, MulDiv(Marshal.ReadByte(ImageData, 0), Alpha, 255));
      Marshal.WriteByte(ImageData, 1, MulDiv(Marshal.ReadByte(ImageData, 1), Alpha, 255));
      Marshal.WriteByte(ImageData, 2, MulDiv(Marshal.ReadByte(ImageData, 2), Alpha, 255));
      ImageData := IntPtr(PByte(ImageData) + 4);
{$ENDIF}
    end;
  end;
end;

procedure TBitmap.UnPreMultiplyAlpha;
var
  Alpha: Word;
{$IFNDEF CLR}
  ImageData: PByte;
{$ELSE}
  ImageData: IntPtr;
{$ENDIF}
begin
  if FImage.FDIB.dsBmih.biBitCount = 32 then //Unpremultiply the alpha value from the bitmap
  begin
    if FImage.FRefCount > 1 then
    with FImage do
    begin
      HandleNeeded;
      CopyImage(FHandle, FPalette, FDIB);
    end;
    if FImage.FDIB.dsBm.bmBits = nil then
      Exit;
    ImageData := FImage.FDIB.dsBm.bmBits;
    while UIntPtr(ImageData) < UIntPtr(FImage.FDIB.dsBm.bmBits) + FImage.FDIB.dsbmih.biSizeImage do
    begin
{$IFNDEF CLR}
      if (PByte(ImageData + 3)^) = 0 then
      begin
        PByte(ImageData)^ := 0;
        PByte(ImageData + 1)^ := 0;
        PByte(ImageData + 2)^ := 0;
      end
      else
      begin
        Alpha := PByte(ImageData + 3)^;
        PByte(ImageData)^ := MulDiv(PByte(ImageData)^, 255, Alpha);
        PByte(ImageData + 1)^ := MulDiv(PByte(ImageData + 1)^, 255, Alpha);
        PByte(ImageData + 2)^ := MulDiv(PByte(ImageData + 2)^, 255, Alpha);
      end;
      Inc(ImageData, 4);
{$ELSE}
      Alpha := Marshal.ReadByte(ImageData, 3);
      if Alpha = 0 then
      begin
        Marshal.WriteByte(ImageData, 0, 0);
        Marshal.WriteByte(ImageData, 1, 0);
        Marshal.WriteByte(ImageData, 2, 0);
      end
      else
      begin
        Marshal.WriteByte(ImageData, 0, MulDiv(Marshal.ReadByte(ImageData, 0), 255, Alpha));
        Marshal.WriteByte(ImageData, 1, MulDiv(Marshal.ReadByte(ImageData, 1), 255, Alpha));
        Marshal.WriteByte(ImageData, 2, MulDiv(Marshal.ReadByte(ImageData, 2), 255, Alpha));
      end;
      ImageData := IntPtr(PByte(ImageData) + 4);
{$ENDIF}
    end;
  end;
end;

procedure TBitmap.SetAlphaFormat(Value: TAlphaFormat);
begin
  if FAlphaFormat <> Value then
  begin
    if FImage.FRefCount > 1 then
    with FImage do
    begin
      HandleNeeded;
      CopyImage(FHandle, FPalette, FDIB);
    end;

    if FAlphaFormat = afIgnored then // Premultiply the alpha if the last format was ignore alpha
      PreMultiplyAlpha
    else if Value = afIgnored then   // Unpremultiply the alpha if the new format is ignore alpha
      UnPreMultiplyAlpha;

    FAlphaFormat := Value;
    Changed(Self);
  end;
end;

function TBitmap.GetTransparentColor: TColor;
begin
  if FTransparentColor = clDefault then
  begin
    if Monochrome then
      Result := clWhite
    else
      Result := Canvas.Pixels[0, Height - 1];
  end
  else Result := ColorToRGB(FTransparentColor);
  Result := Result or $02000000;
end;

function TBitmap.GetWidth: Integer;
begin
  Result := FImage.FDIB.dsBm.bmWidth;
end;

procedure TBitmap.DIBNeeded;
begin
  with FImage do
  begin
    if (FHandle = 0) or (FDIBHandle <> 0) then Exit;
    PaletteNeeded;
    if FDIB.dsbmih.biSize = 0 then
    begin
{$IF DEFINED(CLR)}
      GetObject(FHandle, Marshal.SizeOf(TypeOf(FDIB)), FDIB);
{$ELSE}
      GetObject(FHandle, SizeOf(FDIB), @FDIB);
{$ENDIF}
      with FDIB, dsbm, dsbmih do
      begin
        biSize := SizeOf(dsbmih);
        biWidth := bmWidth;
        biHeight := bmHeight;
        biPlanes := 1;
        biBitCount := bmPlanes * bmBitsPixel;
      end;
    end;
    FDIBHandle := CopyBitmap(FHandle, FPalette, FPalette, FDIB, nil);
  end;
end;

procedure TBitmap.FreeContext;
begin
  if (FCanvas <> nil) then TBitmapCanvas(FCanvas).FreeContext;
end;

procedure TBitmap.HandleNeeded;
var
  vChange: TNotifyEvent;
begin
  if (FImage.FHandle = 0) and (FImage.FDIBHandle = 0) and (FImage.FSaveStream <> nil) then
  begin
    FImage.FSaveStream.Position := 0;
    vChange := OnChange;
    try
      OnChange := nil;
      LoadFromStream(FImage.FSaveStream);  // Current FImage may be destroyed here
    finally
      OnChange := vChange;
    end;
  end;

  with FImage do
    if FHandle = 0 then
      FHandle := FDIBHandle;
end;

procedure TBitmap.Mask(TransparentColor: TColor);
var
  NewHandle, NewPalette: THandle;
  DIB: TDIBSection;
begin
  NewHandle := 0;
  NewPalette := 0;
  try
    FreeContext;
    HandleNeeded;
    NewHandle := CopyBitmapAsMask(FImage.FHandle, FImage.FPalette,
      ColorToRGB(TransparentColor));
{$IF DEFINED(CLR)}
    GetObject(NewHandle, Marshal.SizeOf(TypeOf(DIB)), DIB);
    if THandle(FImage.FPalette) = SystemPalette16.Handle then
{$ELSE}
    FillChar(DIB, SizeOf(DIB), 0);
    GetObject(NewHandle, SizeOf(DIB), @DIB);
    if FImage.FPalette = SystemPalette16 then
{$ENDIF}
      NewPalette := FImage.FPalette
    else
      NewPalette := CopyPalette(FImage.FPalette);
{$IF DEFINED(CLR)}
    NewImage(NewHandle, NewPalette, DIB);
{$ELSE}
    NewImage(NewHandle, NewPalette, DIB, FImage.FOS2Format);
{$ENDIF}
  except
    InternalDeletePalette(NewPalette);
    if NewHandle <> 0 then DeleteObject(NewHandle);
    raise;
  end;
  Changed(Self);
end;

procedure TBitmap.MaskHandleNeeded;
begin
  if FMaskValid and FMaskBitsValid then Exit;
  with FImage do
  begin
  { Delete existing mask if any }
    if FMaskHandle <> 0 then
    begin
      DeselectBitmap(FMaskHandle);
      DeleteObject(FMaskHandle);
      FMaskHandle := 0;
    end;
    FreeContext;
    HandleNeeded;  // may change FImage instance pointer
  end;
  with FImage do   // use new FImage from here on
  begin
    FMaskHandle := CopyBitmapAsMask(FHandle, FPalette, GetTransparentColor);
    FMaskValid := True;
    FMaskBitsValid := True;
  end;
end;

procedure TBitmap.PaletteNeeded;
var
  DC: HDC;
begin
  with FImage do
  begin
    if FIgnorePalette or (FPalette <> 0) or (FDIBHandle = 0) then Exit;
    if FHandle = FDIBHandle then DeselectBitmap(FDIBHandle);
{$IF DEFINED(CLR)}
    FPalette := PaletteFromDIBColorTable(FDIBHandle, [], 1 shl FDIB.dsbmih.biBitCount);
{$ELSE}
    FPalette := PaletteFromDIBColorTable(FDIBHandle, nil, 1 shl FDIB.dsbmih.biBitCount);
{$ENDIF}
    if FPalette <> 0 then Exit;
    DC := GDICheck(GetDC(0));
    FHalftone := FHalftone or
      ((GetDeviceCaps(DC, BITSPIXEL) * GetDeviceCaps(DC, PLANES)) <
      (FDIB.dsbm.bmBitsPixel * FDIB.dsbm.bmPlanes));
    if FHalftone then FPalette := CreateHalftonePalette(DC);
    ReleaseDC(0, DC);
    if FPalette = 0 then IgnorePalette := True;
  end;
end;

{$IF DEFINED(CLR)}
procedure TBitmap.InternalLoadFromBitmap(Bitmap: System.Drawing.Bitmap;
      var BMHandle: HBITMAP; var APalette: HPALETTE; var DIB: TDIBSection);
var
  BmData: System.Drawing.Imaging.BitmapData;
  BitsMem: IntPtr;
  BI: TBitmapInfo;
  ScreenDC, NewImageDC: HDC;
  NewGraphics: System.Drawing.Graphics;
  OldHandle: THandle;
  ColorCount, I: Integer;
  OldPalette: HPalette;
  LPalette: ColorPalette;
begin
  // Calculate bits per pixel, setup BitmapInfo structure
  BmData := Bitmap.LockBits(System.Drawing.Rectangle.FromLTRB(0, 0,
    Bitmap.Width, Bitmap.Height), ImageLockMode.ReadOnly, Bitmap.PixelFormat);
  with BI, bmiHeader, Bitmap do
  begin
    try
      biSize := Marshal.SizeOf(TypeOf(TBitmapInfoHeader));
      biPlanes := 1;

      biBitCount := Abs(BmData.Stride) div Width;
      if biBitCount > 0 then
        biBitCount := biBitCount * 8
      else
      begin
        if BmData.PixelFormat = System.Drawing.Imaging.PixelFormat.Format1bppIndexed then
          biBitCount := 1
        else
          if BmData.PixelFormat = System.Drawing.Imaging.PixelFormat.Format4bppIndexed then
            biBitCount := 4
          else
            if BmData.PixelFormat = System.Drawing.Imaging.PixelFormat.Format8bppIndexed then
              biBitCount := 8
            else
              InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SInvalidPixelFormat);
      end;

      biWidth := Width;
      biHeight := Height;
      biCompression := BI_RGB;
    finally
      Bitmap.UnlockBits(BmData);
    end;

    // Copy color table and invert alpha channel
    LPalette := Bitmap.Palette;
    ColorCount := Length(LPalette.Entries);
    if ColorCount > 0 then
      for I := 0 to ColorCount - 1 do
        bmiColors[I] := LPalette.Entries[I].ToArgb xor Longint($FF000000);
  end;

  OldPalette := 0;
  APalette := 0;
  ScreenDC := GetDC(0);
  NewImageDC := CreateCompatibleDC(ScreenDC);
  try
    // Create Bitmap
    BMHandle := CreateDIBSection(NewImageDC, BI, DIB_RGB_COLORS, BitsMem, 0, 0);
    if (BitsMem = nil) then GDIError;
    GDICheck(BMHandle);
    OldHandle := SelectObject(NewImageDC, BMHandle);
    try
      // Create palette
      if ColorCount > 0 then
      begin
        SetDIBColorTable(NewImageDC, 0, ColorCount, BI.bmiColors);
        APalette := PaletteFromDIBColorTable(0, BI.bmiColors, ColorCount);
        OldPalette := SelectPalette(NewImageDC, APalette, False);
        RealizePalette(NewImageDC);
      end;

      // Draw image on device context
      NewGraphics := System.Drawing.Graphics.FromHDC(NewImageDC);
      NewGraphics.DrawImage(Bitmap, 0, 0, Bitmap.Width, Bitmap.Height);
    finally
      if OldHandle <> 0 then SelectObject(NewImageDC, OldHandle);
    end;
  finally
    if OldPalette <> 0 then SelectPalette(NewImageDC, OldPalette, True);
    if NewImageDC <> 0 then DeleteDC(NewImageDC);
    if ScreenDC <> 0 then ReleaseDC(0, ScreenDC);
  end;

  GetObject(BMHandle, Marshal.SizeOf(TypeOf(DIB)), DIB);
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TBitmap.LoadFromBitmap(Bitmap: System.Drawing.Bitmap);
var
  DIB: TDIBSection;
  BMHandle: HBITMAP;
  APalette: HPALETTE;
begin
  InternalLoadFromBitmap(Bitmap, BMHandle, APalette, DIB);
  NewImage(BMHandle, APalette, DIB, Bitmap.RawFormat);
  Changed(Self);
end;
{$ENDIF}

                                                           
{$IFDEF CLR}[UIPermission(SecurityAction.LinkDemand, Clipboard=UIPermissionClipboard.AllClipboard)]{$ENDIF}
procedure TBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  DIB: TDIBSection;
begin
  if (AFormat <> CF_BITMAP) or (AData = 0) then
{$IF DEFINED(CLR)}
    InvalidGraphic(SUnknownClipboardFormat);
  FreeContext;
  GetObject(AData, Marshal.SizeOf(TypeOf(DIB)), DIB);
  if DIB.dsbm.bmBits = nil then DIB.dsbmih.biSize := 0;
  CopyImage(AData, APalette, DIB);
{$ELSE}
    InvalidGraphic(@SUnknownClipboardFormat);
  FreeContext;
  FillChar(DIB, SizeOf(DIB), 0);
  GetObject(AData, SizeOf(DIB), @DIB);
  if DIB.dsbm.bmBits = nil then DIB.dsbmih.biSize := 0;
  CopyImage(AData, APalette, DIB);
  FImage.FOS2Format := False;
{$ENDIF}
  PaletteModified := Palette <> 0;
  Changed(Self);
end;

{$IF DEFINED(CLR)}
procedure TBitmap.LoadFromResourceName(const ResName, BaseName: String;
  ResourceAssembly: Assembly; Culture: CultureInfo);
var
  ResMgr: ResourceManager;
begin
  ResMgr := ResourceManager.Create(BaseName, ResourceAssembly);
  LoadFromBitmap(System.Drawing.Bitmap(ResMgr.GetObject(ResName, Culture)));
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TBitmap.LoadFromResourceName(const ResName, BaseName: String;
  ResourceAssembly: Assembly; ResourceSet: System.Type; Culture: CultureInfo = nil);
var
  ResMgr: ResourceManager;
begin
  ResMgr := ResourceManager.Create(BaseName, ResourceAssembly, ResourceSet);
  LoadFromBitmap(System.Drawing.Bitmap(ResMgr.GetObject(ResName, Culture)));
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TBitmap.LoadFromResourceName(const ResName: String;
  AType: System.Type; Culture: CultureInfo);
var
  ResMgr: ResourceManager;
begin
  ResMgr := ResourceManager.Create(AType);
  LoadFromBitmap(System.Drawing.Bitmap(ResMgr.GetObject(ResName, Culture)));
end;
{$ENDIF}

procedure TBitmap.LoadFromResourceName(Instance: THandle; const ResName: string);
{$IF DEFINED(CLR)}
var
  DIB: TDIBSection;
  NewHandle: HBITMAP;
begin
  NewHandle := LoadImage(Instance, ResName, IMAGE_BITMAP, 0, 0,
    LR_CREATEDIBSECTION or LR_DEFAULTSIZE);
  GetObject(NewHandle, Marshal.SizeOf(TypeOf(DIB)), DIB);
  NewImage(NewHandle, 0, DIB);
  Changed(Self);
end;
{$ELSE}
var
  Stream: TCustomMemoryStream;
begin
  FreeContext;
  Stream := TResourceStream.Create(Instance, ResName, RT_BITMAP);
  try
    ReadDIB(Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}

{$IF DEFINED(CLR) OR DEFINED(MSWINDOWS)}
procedure TBitmap.LoadFromResourceID(Instance: THandle; ResID: Integer);
{$IF DEFINED(CLR)}
var
  DIB: TDIBSection;
  NewHandle: HBITMAP;
begin
  NewHandle := LoadImage(Instance, ResID, IMAGE_BITMAP, 0, 0,
    LR_CREATEDIBSECTION or LR_DEFAULTSIZE);
  GetObject(NewHandle, Marshal.SizeOf(TypeOf(DIB)), DIB);
  NewImage(NewHandle, 0, DIB);
  Changed(Self);
end;
{$ELSE}
var
  Stream: TCustomMemoryStream;
begin
  FreeContext;
  Stream := TResourceStream.CreateFromID(Instance, ResID, RT_BITMAP);
  try
    ReadDIB(Stream, Stream.Size);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

class function TBitmap.CanLoadFromStream(Stream: TStream): Boolean;
var
  Bmf: TBitmapFileHeader;
  P: Int64;
begin
  P := Stream.Position;
  try
    Result := (Stream.Size - Stream.Position = 0) or
      (Stream.Read(Bmf, SizeOf(Bmf)) = SizeOf(Bmf)) and
      (Bmf.bfType = $4D42);
  finally
    Stream.Position := P;
  end;
end;

procedure TBitmap.LoadFromStream(Stream: TStream);
begin
  ReadStream(Stream, Stream.Size - Stream.Position);
end;

{$IF DEFINED(CLR)}
procedure TBitmap.NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
  const NewDIB: TDIBSection; NewImageFormat: TImageFormat = nil;
  NewSaveStream: TMemoryStream = nil);
{$ELSE}
procedure TBitmap.NewImage(NewHandle: HBITMAP; NewPalette: HPALETTE;
  const NewDIB: TDIBSection; OS2Format: Boolean; RLEStream: TStream = nil);
{$ENDIF}
var
  Image: TBitmapImage;
begin
  Image := TBitmapImage.Create;
  with Image do
  try
    FHandle := NewHandle;
    FPalette := NewPalette;
    FDIB := NewDIB;
{$IF DEFINED(CLR)}
    FImageFormat := NewImageFormat;
    if FDIB.dsbm.bmBits <> nil then FDIBHandle := FHandle;
    if FSaveStream <> nil then FreeAndNil(FSaveStream);
    FSaveStream := NewSaveStream;
{$ELSE}
    FOS2Format := OS2Format;
    if FDIB.dsbm.bmBits <> nil then FDIBHandle := FHandle;
    FSaveStream := RLEStream as TMemoryStream;
{$ENDIF}
  except
    Image.Free;
    raise;
  end;

{$IF DEFINED(CLR)}
  System.Threading.Monitor.Enter(BitmapImageLock);
{$ELSE}
  //!! replace with InterlockedExchange()
  EnterCriticalSection(BitmapImageLock);
{$ENDIF}
  try
    FImage.Release;
    FImage := Image;
    FImage.Reference;
  finally
{$IF DEFINED(CLR)}
    System.Threading.Monitor.Exit(BitmapImageLock);
{$ELSE}
    LeaveCriticalSection(BitmapImageLock);
{$ENDIF}
  end;
  FMaskValid := False;
end;

procedure TBitmap.ReadData(Stream: TStream);
var
  Size: Longint;
begin
  Stream.Read(Size, SizeOf(Size));
  ReadStream(Stream, Size);
end;

{$IF DEFINED(CLR)}
procedure TBitmap.ReadDIB(Stream: TStream; ImageSize: LongWord);
var
  BMHandle: HBITMAP;
  APalette: HPALETTE;
  DIB: TDIBSection;
  ImageFormat: TImageFormat;
  SaveStream: TMemoryStream;
  Image: System.Drawing.Bitmap;
begin
  // Load image from stream, create DIB from image
  Image := System.Drawing.Bitmap(System.Drawing.Image.FromStream(Stream));
  InternalLoadFromBitmap(Image, BMHandle, APalette, DIB);
  ImageFormat := Image.RawFormat;
  Image.Dispose;

  // Preserve original stream
  SaveStream := TMemoryStream.Create;
  Stream.Position := Stream.Size - ImageSize;
  SaveStream.SetSize(ImageSize);
  SaveStream.CopyFrom(Stream, ImageSize);

  NewImage(BMHandle, APalette, DIB, ImageFormat, SaveStream);
  Changed(Self);
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
procedure TBitmap.ReadDIB(Stream: TStream; ImageSize: LongWord; bmf: PBitmapFileHeader);
const
  DIBPalSizes: array [Boolean] of Byte = (SizeOf(TRGBQuad), SizeOf(TRGBTriple));
var
  DC, MemDC: HDC;
  BitsMem: Pointer;
  OS2Header: TBitmapCoreHeader;
  BitmapInfo: PBitmapInfo;
  ColorTable: Pointer;
  HeaderSize: Integer;
  OS2Format: Boolean;
  BMHandle, OldBMP: HBITMAP;
  DIB: TDIBSection;
  Pal, OldPal: HPalette;
  RLEStream: TStream;
  vbmf: TBitmapFileHeader;
{$IFDEF LINUX}
  I: Integer;
{$ENDIF}
begin
  SetLastError(0);
  Pal := 0;
  RLEStream := nil;
  Stream.Read(HeaderSize, SizeOf(HeaderSize));
  OS2Format := HeaderSize = SizeOf(OS2Header);
  if OS2Format then
    HeaderSize := SizeOf(TBitmapInfoHeader);
  GetMem(BitmapInfo, HeaderSize + 12 + 256 * SizeOf(TRGBQuad));
  with BitmapInfo^ do
  try
    try
      if OS2Format then  // convert OS2 DIB to Win DIB
      begin
        Stream.Read(Pointer(PByte(@OS2Header) + SizeOf(HeaderSize))^,
          SizeOf(OS2Header) - SizeOf(HeaderSize));
        FillChar(bmiHeader, SizeOf(bmiHeader), 0);
        with bmiHeader, OS2Header do
        begin
          biWidth := bcWidth;
          biHeight := bcHeight;
          biPlanes := bcPlanes;
          biBitCount := bcBitCount;
        end;
        Dec(ImageSize, SizeOf(OS2Header));
      end
      else
      begin // support bitmap headers larger than TBitmapInfoHeader
        Stream.Read(Pointer(PByte(BitmapInfo) + SizeOf(HeaderSize))^,
          HeaderSize - SizeOf(HeaderSize));
        Dec(ImageSize, HeaderSize);

        if (bmiHeader.biCompression <> BI_BITFIELDS) and
          (bmiHeader.biCompression <> BI_RGB) then
        begin // Preserve funky non-DIB data (like RLE) until modified
          RLEStream := TMemoryStream.Create;
          // source stream could be unidirectional.  don't reverse seek
          if bmf = nil then
          begin
            FillChar(vbmf, SizeOf(vbmf), 0);
            vbmf.bfType := $4D42;
            vbmf.bfSize := ImageSize + Cardinal(HeaderSize);
            bmf := @vbmf;
          end;
          RLEStream.Write(bmf^, SizeOf(bmf^));
          RLEStream.Write(HeaderSize, SizeOf(HeaderSize));
          RLEStream.Write(Pointer(PByte(BitmapInfo) + SizeOf(HeaderSize))^,
            HeaderSize - SizeOf(HeaderSize));
          RLEStream.CopyFrom(Stream, ImageSize);
          { Cast ImageSize (long word) to integer to avoid integer overflow when negating. }
          RLEStream.Seek(-Integer(ImageSize), soFromEnd);
          Stream := RLEStream;  // the rest of the proc reads from RLEStream
        end;
      end;

      with bmiHeader do
      begin
        biSize := HeaderSize;
        ColorTable := Pointer(PByte(BitmapInfo) + HeaderSize);

        { check number of planes. DIBs must be 1 color plane (packed pixels) }
        if biPlanes <> 1 then InvalidBitmap;

        // 3 DWORD color element bit masks (ie 888 or 565) can precede colors
        // TBitmapInfoHeader sucessors include these masks in the headersize
        if (HeaderSize = SizeOf(TBitmapInfoHeader)) and
          ((biBitCount = 16) or (biBitCount = 32)) and
          (biCompression = BI_BITFIELDS) then
        begin
          Stream.ReadBuffer(ColorTable^, 3 * SizeOf(DWORD));
          Inc(PByte(ColorTable), 3 * SizeOf(DWORD));
          Dec(ImageSize, 3 * SizeOf(DWORD));
        end;

        // Read the color palette
        if biClrUsed = 0 then
          biClrUsed := GetDInColors(biBitCount);

        if (biClrUsed * DIBPalSizes[OS2Format]) > (256 * SizeOf(TRGBQuad)) then
          InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SInvalidBitmap);

        Stream.ReadBuffer(ColorTable^, biClrUsed * DIBPalSizes[OS2Format]);
        Dec(ImageSize, biClrUsed * DIBPalSizes[OS2Format]);

        // biSizeImage can be zero. If zero or RGB, compute the size.
        if (biSizeImage = 0) or (biCompression = BI_RGB) then // top-down DIBs have negative height
          biSizeImage := BytesPerScanLine(biWidth, biBitCount, 32) * Abs(biHeight);

        if biSizeImage < ImageSize then
          ImageSize := biSizeImage;
      end;

      { convert OS2 color table to DIB color table }
      if OS2Format then RGBTripleToQuad(ColorTable^);

      DC := GDICheck(GetDC(0));
      try
        if ((bmiHeader.biCompression <> BI_RGB) and
          (bmiHeader.biCompression <> BI_BITFIELDS)) or DDBsOnly then
        begin
          MemDC := 0;
          GetMem(BitsMem, ImageSize);
          try
            Stream.ReadBuffer(BitsMem^, ImageSize);
            MemDC := GDICheck(CreateCompatibleDC(DC));
            OldBMP := SelectObject(MemDC, CreateCompatibleBitmap(DC, 1, 1));
            OldPal := 0;
            if bmiHeader.biClrUsed > 0 then
            begin
              Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);
              OldPal := SelectPalette(MemDC, Pal, False);
              RealizePalette(MemDC);
            end;

            try
              BMHandle := CreateDIBitmap(MemDC, BitmapInfo^.bmiHeader, CBM_INIT, BitsMem,
                BitmapInfo^, DIB_RGB_COLORS);
              if (BMHandle = 0) then
                if GetLastError = 0 then
                  InvalidBitmap else RaiseLastOSError;
            finally
              if OldPal <> 0 then
                SelectPalette(MemDC, OldPal, True);
              DeleteObject(SelectObject(MemDC, OldBMP));
            end;
          finally
            if MemDC <> 0 then DeleteDC(MemDC);
            FreeMem(BitsMem);
          end;
        end
        else
        begin
          BMHandle := CreateDIBSection(DC, BitmapInfo^, DIB_RGB_COLORS, BitsMem, 0, 0);
          if (BMHandle = 0) or (BitsMem = nil) then
            if GetLastError = 0 then
              InvalidBitmap else RaiseLastOSError;

          try
{$IFDEF LINUX}
            // I need to pre-touch the memory in 4096 byte increments to ensure
            // the read will succeed. WINE marks this memory as not present to
            // catch when we make changes to it. If we read directly into it
            // Linux will (correctly) terminate the read with a failure since an
            // exception occured during the read. We need to make sure these
            // exceptions are triggered in user space instead of kernel.
            for I := 1 to (ImageSize + 4095) div 4096 do
              PByteArray(BitsMem)^[(I - 1) * 4096] := 0;
{$ENDIF}
            Stream.ReadBuffer(BitsMem^, ImageSize);
          except
            DeleteObject(BMHandle);
            raise;
          end;
        end;
      finally
        ReleaseDC(0, DC);
      end;
      // Hi-color DIBs don't preserve color table, so create palette now
      // 16 bit or more do not have a color palette.
      if (bmiHeader.biBitCount > 8) and (bmiHeader.biBitCount <= 16) and
         (bmiHeader.biClrUsed > 0) and (Pal = 0)then
          Pal := PaletteFromDIBColorTable(0, ColorTable, bmiHeader.biClrUsed);
      FillChar(DIB, SizeOf(DIB), 0);
      GetObject(BMHandle, Sizeof(DIB), @DIB);
      // GetObject / CreateDIBSection don't preserve these info values
      DIB.dsBmih.biXPelsPerMeter := bmiHeader.biXPelsPerMeter;
      DIB.dsBmih.biYPelsPerMeter := bmiHeader.biYPelsPerMeter;
      DIB.dsBmih.biClrUsed := bmiHeader.biClrUsed;
      DIB.dsBmih.biClrImportant := bmiHeader.biClrImportant;
    except
      RLEStream.Free;
      raise;
    end;
  finally
    FreeMem(BitmapInfo);
  end;
  NewImage(BMHandle, Pal, DIB, OS2Format, RLEStream);

  if (FImage.FDIB.dsBMIh.biBitCount = 32) and (FAlphaFormat = afDefined) then
    PreMultiplyAlpha;

  PaletteModified := Palette <> 0;
  Changed(Self);
end;
{$ENDIF}

procedure TBitmap.ReadStream(Stream: TStream; Size: Longint);
{$IF DEFINED(CLR)}
var
  DIB: TDIBSection;
  MemStream: TMemoryStream;
begin
  FreeContext;
  if Size = 0 then
    NewImage(0, 0, DIB)
  else
  begin
    // System.Drawing.Image always reads from the beginning of the
    // stream so we need to copy the stream into a temporary memory
    // stream before calling ReadDIB if Stream.Position isn't 0.
    if Stream.Position = 0 then
      ReadDIB(Stream, Size)
    else
    begin
      MemStream := TMemoryStream.Create;
      MemStream.SetSize(Size);
      MemStream.CopyFrom(Stream, Size);
      ReadDIB(MemStream, Size);
    end;
  end;
end;
{$ELSE}
var
  Bmf: TBitmapFileHeader;
  DIB: TDIBSection;
begin
  FreeContext;
  if Size = 0 then
  begin
    FillChar(DIB, SizeOf(DIB), 0);
    NewImage(0, 0, DIB, False);
  end
  else
  begin
    Stream.ReadBuffer(Bmf, SizeOf(Bmf));
    if Bmf.bfType <> $4D42 then InvalidBitmap;
    ReadDIB(Stream, Size - SizeOf(Bmf), @Bmf);
  end;
end;
{$ENDIF}

procedure TBitmap.SetHandle(Value: HBITMAP);
var
  DIB: TDIBSection;
  APalette: HPALETTE;
begin
  with FImage do
    if FHandle <> Value then
    begin
      FreeContext;
{$IF NOT DEFINED(CLR)}
      FillChar(DIB, SizeOf(DIB), 0);
{$ENDIF}
      if Value <> 0 then
{$IF DEFINED(CLR)}
        GetObject(Value, Marshal.SizeOf(TypeOf(DIB)), DIB);
{$ELSE}
        GetObject(Value, SizeOf(DIB), @DIB);
{$ENDIF}
      if FRefCount = 1 then
      begin
        APalette := FPalette;
        FPalette := 0;
      end
      else
{$IF DEFINED(CLR)}
        if THandle(FPalette) = SystemPalette16.Handle then
          APalette := SystemPalette16.Handle
{$ELSE}
        if FPalette = SystemPalette16 then
          APalette := SystemPalette16
{$ENDIF}
        else
          APalette := CopyPalette(FPalette);
      try
{$IF DEFINED(CLR)}
        NewImage(Value, APalette, DIB);
{$ELSE}
        NewImage(Value, APalette, DIB, False);
{$ENDIF}
      except
        InternalDeletePalette(APalette);
        raise;
      end;
      Changed(Self);
    end;
end;

procedure TBitmap.SetHandleType(Value: TBitmapHandleType);
var
  DIB: TDIBSection;
  AHandle: HBITMAP;
  NewPalette: HPALETTE;
  DoCopy: Boolean;
begin
  if Value = GetHandleType then Exit;
  with FImage do
  begin
    if (FHandle = 0) and (FDIBHandle = 0) then
      if Value = bmDDB then
        FDIB.dsbmih.biSize := 0
      else
        FDIB.dsbmih.biSize := SizeOf(FDIB.dsbmih)
    else
    begin
      if Value = bmDIB then
      begin
        if (FDIBHandle <> 0) and (FDIBHandle = FHandle) then Exit;
        FreeContext;
        PaletteNeeded;
        DIBNeeded;
        if FRefCount = 1 then
        begin
          AHandle := FDIBHandle;
          FDIBHandle := 0;
          NewPalette := FPalette;
          FPalette := 0;
{$IF DEFINED(CLR)}
          NewImage(AHandle, NewPalette, FDIB, FImageFormat);
{$ELSE}
          NewImage(AHandle, NewPalette, FDIB, FOS2Format);
{$ENDIF}
        end
        else
          CopyImage(FDIBHandle, FPalette, FDIB);
      end
      else
      begin
        if (FHandle <> 0) and (FHandle <> FDIBHandle) then Exit;
        FreeContext;
        PaletteNeeded;
        DIB := FDIB;
        DIB.dsbmih.biSize := 0;   // flag to tell CopyBitmap to create a DDB
        DoCopy := FRefCount = 1;
        if DoCopy then
          NewPalette := FPalette
        else
          NewPalette := CopyPalette(FPalette);
        AHandle := CopyBitmap(FDIBHandle, FPalette, NewPalette, DIB, nil);
        if DoCopy then
          FHandle := AHandle
        else
{$IF DEFINED(CLR)}
          NewImage(AHandle, NewPalette, DIB, FImageFormat);
{$ELSE}
          NewImage(AHandle, NewPalette, DIB, FOS2Format);
{$ENDIF}
      end;
      Changed(Self);
    end;
  end;
end;

procedure TBitmap.SetHeight(Value: Integer);
begin
  SetSize(FImage.FDIB.dsBm.bmWidth, Value);
end;

procedure TBitmap.SetMaskHandle(Value: HBITMAP);
begin
  with FImage do
    if FMaskHandle <> Value then
    begin
      FMaskHandle := Value;
      FMaskValid := True;
      FMaskBitsValid := True;
    end;
end;

procedure TBitmap.SetMonochrome(Value: Boolean);
var
  DIB: TDIBSection;
begin
  with FImage, FDIB.dsbmih do
    if Value <> ((biPlanes = 1) and (biBitCount = 1)) then
    begin
      HandleNeeded;
      DIB := FDIB;
      with DIB.dsbmih, DIB.dsbm do
      begin
        biSize := 0;   // request DDB handle
        biPlanes := Byte(Value);  // 0 = request screen BMP format
        biBitCount := Byte(Value);
        bmPlanes := Byte(Value);
        bmBitsPixel := Byte(Value);
      end;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;

procedure TBitmap.SetPalette(Value: HPALETTE);
var
  AHandle: HBITMAP;
  DIB: TDIBSection;
begin
  if FImage.FPalette <> Value then
  begin
    with FImage do
      if (Value = 0) and (FRefCount = 1) then
      begin
        InternalDeletePalette(FPalette);
        FPalette := 0;
      end
      else
      begin
        FreeContext;
        HandleNeeded;
        DIB := FDIB;
        AHandle := CopyBitmap(FHandle, FPalette, Value, DIB, nil);
        try
{$IF DEFINED(CLR)}
          NewImage(AHandle, Value, DIB);
{$ELSE}
          NewImage(AHandle, Value, DIB, FOS2Format);
{$ENDIF}
        except
          DeleteObject(AHandle);
          raise;
        end;
      end;
    UpdateDIBColorTable(FImage.FDIBHandle, Value, FImage.FDIB);
    PaletteModified := True;
    Changed(Self);
  end;
end;

procedure TBitmap.SetPixelFormat(Value: TPixelFormat);
const
  BitCounts: array [pf1Bit..pf32Bit] of Byte = (1,4,8,16,16,24,32);
var
  DIB: TDIBSection;
  Pal: HPalette;
  DC: HDC;
  KillPal: Boolean;
begin
  if Value = GetPixelFormat then Exit;
  case Value of
    pfDevice:
      begin
        HandleType := bmDDB;
        Exit;
      end;
    pfCustom: InvalidGraphic({$IFNDEF CLR}@{$ENDIF}SInvalidPixelFormat);
  else
{$IF NOT DEFINED(CLR)}
    FillChar(DIB, SizeOf(DIB), 0);
{$ENDIF}
    DIB.dsbm := FImage.FDIB.dsbm;
    KillPal := False;
    with DIB, dsbm, dsbmih do
    begin
      bmBits := nil;
      biSize := SizeOf(DIB.dsbmih);
      biWidth := bmWidth;
      biHeight := bmHeight;
      biPlanes := 1;
      biBitCount := BitCounts[Value];
      Pal := FImage.FPalette;
      case Value of
        // Copy Palette to prevent holding the same handle as SystemPalette16,
        // which would cause problems with the undetermined sequence of
        // finalizers in .NET
{$IF DEFINED(CLR)}
        pf4Bit:
          begin
            Pal := CopyPalette(SystemPalette16.Handle);
            KillPal := True;
          end;
{$ELSE}
        pf4Bit: Pal := SystemPalette16;
{$ENDIF}
        pf8Bit:
          begin
            DC := GDICheck(GetDC(0));
            Pal := CreateHalftonePalette(DC);
            KillPal := True;
            ReleaseDC(0, DC);
          end;
        pf16Bit:
          begin
            biCompression := BI_BITFIELDS;
            dsBitFields[0] := $F800;
            dsBitFields[1] := $07E0;
            dsBitFields[2] := $001F;
          end;
      end;
      try
        CopyImage(Handle, Pal, DIB);
        PaletteModified := Pal <> 0;
      finally
        if KillPal then
          DeleteObject(Pal);
      end;
      Changed(Self);
    end;
  end;
end;

{$IF DEFINED(CLR)}
procedure TBitmap.SetImageFormat(Value: TImageFormat);
begin
  with FImage do
    if Value <> FImageFormat then
    begin
      FImageFormat := Value;
      FreeAndNil(FImage.FSaveStream);
    end;
end;
{$ENDIF}

procedure TBitmap.SetTransparentColor(Value: TColor);
begin
  if Value <> FTransparentColor then
  begin
    if Value = clDefault then
      FTransparentMode := tmAuto else
      FTransparentMode := tmFixed;
    FTransparentColor := Value;
    if FImage.FRefCount > 1 then
    with FImage do
    begin
      HandleNeeded;
      CopyImage(FHandle, FPalette, FDIB);
    end;
    Changed(Self);
  end;
end;

procedure TBitmap.SetTransparentMode(Value: TTransparentMode);
begin
  if Value <> FTransparentMode then
  begin
    if Value = tmAuto then
      SetTransparentColor(clDefault) else
      SetTransparentColor(GetTransparentColor);
  end;
end;

procedure TBitmap.SetWidth(Value: Integer);
begin
  SetSize(Value, FImage.FDIB.dsbm.bmHeight);
end;

procedure TBitmap.WriteData(Stream: TStream);
begin
  WriteStream(Stream, True);
end;

procedure TBitmap.WriteStream(Stream: TStream; WriteSize: Boolean);
{$IF DEFINED(CLR)}
var
  Size: DWORD;
  MemStream: TMemoryStream;
  Format: TImageFormat;
  Bitmap: System.Drawing.Bitmap;
begin
  if FImage.FSaveStream <> nil then
  begin
    Size := FImage.FSaveStream.Size;
    if WriteSize then
      Stream.WriteBuffer(Size, SizeOf(Size));
    Stream.Write(FImage.FSaveStream.Memory, FImage.FSaveStream.Size);
    Exit;
  end;

  DIBNeeded;
  if FImage.FDIBHandle <> 0 then
  begin
    Bitmap := System.Drawing.Bitmap.FromHBitmap(FImage.FDIBHandle);

    if FImage.FImageFormat = nil then
      Format := System.Drawing.Imaging.ImageFormat.Bmp
    else
      Format := FImage.FImageFormat;

    if WriteSize then
    begin
      MemStream := TMemoryStream.Create;
      Bitmap.Save(MemStream, Format);
      Size := MemStream.Size;
      Stream.WriteBuffer(Size, SizeOf(Size));
      Stream.Write(MemStream.Memory, MemStream.Size);
    end
    else
      Bitmap.Save(Stream, Format);
  end;
end;
{$ELSE}
const
  PalSize: array [Boolean] of Byte = (SizeOf(TRGBQuad), SizeOf(TRGBTriple));
var
  Size, ColorCount: DWORD;
  HeaderSize: DWORD;
  BMF: TBitmapFileHeader;
  Save: THandle;
  BC: TBitmapCoreHeader;
  Colors: array [Byte] of TRGBQuad;
begin
  FillChar(BMF, SizeOf(BMF), 0);
  BMF.bfType := $4D42;
  if FImage.FSaveStream <> nil then
  begin
    Size := FImage.FSaveStream.Size;
    if WriteSize then
      Stream.WriteBuffer(Size, SizeOf(Size));
    Stream.Write(FImage.FSaveStream.Memory^, FImage.FSaveStream.Size);
    Exit;
  end;
  DIBNeeded;
  if (FImage.FDIB.dsBMIh.biBitCount = 32) and (FAlphaFormat = afDefined) then
    UnPreMultiplyAlpha;
  with FImage do
  begin
    Size := 0;
    if FDIBHandle <> 0 then
    begin
      InternalGetDIBSizes(FDIBHandle, HeaderSize, Size, FDIB.dsbmih.biClrUsed);
      if FOS2Format then
      begin // OS2 format cannot have partial palette
        HeaderSize := SizeOf(BC);
        if FDIB.dsbmih.biBitCount <= 8 then
          Inc(HeaderSize, SizeOf(TRGBTriple) * (1 shl FDIB.dsbmih.biBitCount));
      end;
      Inc(Size, HeaderSize + SizeOf(BMF));

      FillChar(BMF, SizeOf(BMF), 0);
      BMF.bfType := $4D42;
      if FDIB.dsBmih.biBitCount <= 8 then
      begin
        Canvas.RequiredState([csHandleValid]);
        Save := GDICheck(SelectObject(FCanvas.FHandle, FDIBHandle));
        ColorCount := GetDIBColorTable(FCanvas.FHandle, 0, 256, Colors);
        SelectObject(FCanvas.FHandle, Save);
      end
      else
        ColorCount := 0;
      // GetDIBColorTable always reports the full palette; trim it back for partial palettes
      if (0 < FDIB.dsbmih.biClrUsed) and (FDIB.dsbmih.biClrUsed < ColorCount) then
        ColorCount := FDIB.dsbmih.biClrUsed;
      if (not FOS2Format) and (ColorCount = 0) and (FPalette <> 0) and not FHalftone then
      begin
        ColorCount := PaletteToDIBColorTable(FPalette, Colors);
        if FDIB.dsbmih.biBitCount > 8 then
        begin  // optional color palette for hicolor images (non OS2)
          Inc(Size, ColorCount * SizeOf(TRGBQuad));
          Inc(HeaderSize, ColorCount * SizeOf(TRGBQuad));
        end;
      end;

      BMF.bfSize := Size;
      BMF.bfOffBits := SizeOf(BMF) + HeaderSize;
    end;

    if WriteSize then Stream.WriteBuffer(Size, SizeOf(Size));

    if Size <> 0 then
    begin
      FixupBitFields(FDIB);
      if (ColorCount <> 0) then
      begin
        if (FDIB.dsbmih.biClrUsed = 0) or (FDIB.dsbmih.biClrUsed <> ColorCount) then
          FDIB.dsbmih.biClrUsed := ColorCount;
        if FOS2Format then RGBQuadToTriple(Colors, Integer(ColorCount));
      end;
      if FOS2Format then
      begin
        with BC, FDIB.dsbmih do
        begin
          bcSize := SizeOf(BC);
          bcWidth := biWidth;
          bcHeight := biHeight;
          bcPlanes := 1;
          bcBitCount := biBitCount;
        end;
        Stream.WriteBuffer(BMF, SizeOf(BMF));
        Stream.WriteBuffer(BC, SizeOf(BC));
      end
      else
      begin
        Stream.WriteBuffer(BMF, Sizeof(BMF));
        Stream.WriteBuffer(FDIB.dsbmih, Sizeof(FDIB.dsbmih));
        if (FDIB.dsbmih.biBitCount > 8) and
          ((FDIB.dsbmih.biCompression and BI_BITFIELDS) <> 0) then
          Stream.WriteBuffer(FDIB.dsBitfields, 12);
      end;
      Stream.WriteBuffer(Colors, ColorCount * PalSize[FOS2Format]);
      Stream.WriteBuffer(FDIB.dsbm.bmBits^, FDIB.dsbmih.biSizeImage);
    end;
  end;
end;
{$ENDIF}

{ ReleaseHandle gives up ownership of the bitmap handle the TBitmap contains. }
function TBitmap.ReleaseHandle: HBITMAP;
begin
  HandleNeeded;
  Changing(Self);
  with FImage do
  begin
    Result := FHandle;
    if FHandle = FDIBHandle then
    begin
      FDIBHandle := 0;
      FDIB.dsbm.bmBits := nil;
    end;
    FHandle := 0;
  end;
end;

function TBitmap.ReleaseMaskHandle: HBITMAP;
begin
  Result := GetMaskHandle;
  FImage.FMaskHandle := 0;
end;

function TBitmap.ReleasePalette: HPALETTE;
begin
  FreeContext;
  HandleNeeded;
  Changing(Self);
  Result := FImage.FPalette;
  FImage.FPalette := 0;
end;

procedure TBitmap.SaveToStream(Stream: TStream);
begin
  WriteStream(Stream, False);
end;

                                                           
procedure TBitmap.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
var
  DIB: TDIBSection;
begin
  Format := CF_BITMAP;
  HandleNeeded;
  with FImage do
  begin
    DIB := FDIB;
    DIB.dsbmih.biSize := 0;   // copy to device bitmap
    DIB.dsbm.bmBits := nil;
    Data := CopyBitmap(FHandle, FPalette, FPalette, DIB, FCanvas);
  end;
  try
    APalette := CopyPalette(FImage.FPalette);
  except
    DeleteObject(Data);
    raise;
  end;
end;

procedure TBitmap.SetSize(AWidth, AHeight: Integer);
var
  DIB: TDIBSection;
begin
  HandleNeeded;
  with FImage do
    if (FDIB.dsbm.bmWidth <> AWidth) or (FDIB.dsbm.bmHeight <> AHeight) then
    begin
      DIB := FDIB;
      DIB.dsbm.bmWidth := AWidth;
      DIB.dsbm.bmHeight := AHeight;
      DIB.dsbmih.biWidth := AWidth;
      DIB.dsbmih.biHeight := AHeight;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;

function TBitmap.TransparentColorStored: Boolean;
begin
  Result := FTransparentMode = tmFixed;
end;

{ TIconImage }

destructor TIconImage.Destroy;
begin
  FMemoryImage.Free;
{$IF DEFINED(CLR)}
  if Assigned(FIcon) then
    FIcon.Free;
{$ENDIF}
  inherited Destroy;
end;

procedure TIconImage.FreeHandle;
begin
{$IF DEFINED(CLR)}
  if Assigned(FIcon) then
    FreeAndNil(FIcon);
{$ELSE}
  if FHandle <> 0 then DestroyIcon(FHandle);
  FHandle := 0;
{$ENDIF}
end;

{ TIcon }

{$IFNDEF CLR}
procedure TIcon.AssignTo(Dest: TPersistent);
  function HasAlpha(Buf : PRGBQuad; CX : Integer; CY : Integer) : Boolean;
  var
    X,Y: Integer;
  begin
    Result := False;
    for Y := 0 to CY - 1 do
    begin
      for X := 0 to CY - 1 do
      begin
        if (DWORD(buf^) and $FF000000) <> 0 then
        begin
          Result := True;
          Exit;
        end;
        Inc(Buf, 1);
      end;
    end;
  end;
var
  pBitmap, pBitmapBit : PRGBQuad;
  pMask, pMaskBit : PRGBQuad;
  Info : TIconInfo;
  Bmi : TBitmapInfo;
  X,Y: Integer;
  Bmp : TBitmap;
  hBmp : HBITMAP;
begin
  if Dest is TBitmap then
  begin
    Bmp := TBitmap(Dest);
    Bmp.SetSize(Width, Height);
    Bmp.PixelFormat := pf32bit;
    Bmp.AlphaFormat := afDefined;
    Bmp.Canvas.Brush.Color := clBlack;
    Bmp.Canvas.FillRect(Rect(0,0,Width, Height));
    DrawIconEx(Bmp.Canvas.Handle, 0,0, Handle, Width, Height, 0,0, DI_NORMAL);

    Bmi.bmiHeader.biSize := SizeOf(Bmi);
    Bmi.bmiHeader.biPlanes := 1;
    Bmi.bmiHeader.biHeight := Width;
    Bmi.bmiHeader.biWidth := Height;
    Bmi.bmiHeader.biCompression := BI_RGB;
    Bmi.bmiHeader.biBitCount := 32;

    pBitmap := AllocMem(Bmi.bmiHeader.biWidth * 4 * Bmi.bmiHeader.biHeight);
    try
      { FreeContext must be called first }
      // Forces evaluation of Bitmap.Handle before Bitmap.Canvas.Handle
      hBmp := Bmp.Handle;
      if (GetDIBits(Bmp.Canvas.Handle, hBmp, 0, Height, pBitmap,
        Bmi, DIB_RGB_COLORS) = Height) and not HasAlpha(pBitmap,Width,Height)  then
      begin
        pMask := AllocMem(Bmi.bmiHeader.biWidth * 4 * Bmi.bmiHeader.biHeight);
        try
          GetIconInfo(Handle, Info);
          try
            if GetDIBits(Bmp.Canvas.Handle, Info.hbmMask, 0, Height, pMask,
              Bmi, DIB_RGB_COLORS) = Height then
            begin
              pbitmapBit := pBitmap;
              pMaskBit := pMask;
              for y := 0 to Bmi.bmiHeader.biHeight - 1 do
              begin
                for x := 0 to Bmi.bmiHeader.biWidth - 1 do
                begin
                  if Cardinal(pMaskBit^) <> 0 then
                    Cardinal(pBitmapBit^) := 0
                  else
                    pBitmapBit.rgbReserved := 255;
                  Inc(pBitmapBit,1);
                  Inc(pMaskBit,1);
                end;
              end;
              { dc parameter not used since Usage = DIB_RGB_COLORS }
              SetDIBits(0, Bmp.Handle, 0, Height,
                pBitmap, Bmi, DIB_RGB_COLORS);
            end;
          finally
            DeleteObject(Info.hbmMask);
            DeleteObject(Info.hbmColor);
          end;
        finally
          FreeMem(pmask);
        end;
      end;
    finally
      FreeMem(pBitmap);
    end;
  end
  else
    inherited;

end;
{$ENDIF}

constructor TIcon.Create;
begin
  inherited Create;
  FTransparent := True;
  FImage := TIconImage.Create;
  FImage.Reference;
end;

destructor TIcon.Destroy;
begin
  FImage.Release;
  inherited Destroy;
end;

procedure TIcon.Assign(Source: TPersistent);
begin
  if (Source = nil) or (Source is TIcon) then
  begin
    if Source <> nil then
    begin
      TIcon(Source).FImage.Reference;
      FImage.Release;
      FImage := TIcon(Source).FImage;
    end
    else
{$IF DEFINED(CLR)}
      NewImage(nil, nil);
{$ELSE}
      NewImage(0, nil);
{$ENDIF}
    Changed(Self);
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TIcon.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  with Rect do
  begin
    ACanvas.RequiredState([csHandleValid]);
    DrawIconEx(ACanvas.FHandle, Left, Top, Handle, 0, 0, 0, 0, DI_NORMAL);
  end;
end;

function TIcon.GetEmpty: Boolean;
begin
  with FImage do
{$IF DEFINED(CLR)}
    Result := (not Assigned(FIcon)) and (FMemoryImage = nil);
{$ELSE}
    Result := (FHandle = 0) and (FMemoryImage = nil);
{$ENDIF}
end;

function TIcon.GetHandle: HICON;
begin
  HandleNeeded;
{$IF DEFINED(CLR)}
  if Assigned(FImage.FIcon) then
    Result := HICON(FImage.FIcon.Handle)
  else
    Result := 0;
{$ELSE}
  Result := FImage.FHandle;
{$ENDIF}
end;

function TIcon.HandleAllocated: Boolean;
begin
{$IF DEFINED(CLR)}
  Result := Assigned(FImage) and Assigned(FImage.FIcon) and
    (FImage.FIcon.Handle <> nil);
{$ELSE}
  Result := Assigned(FImage) and (FImage.FHandle <> 0);
{$ENDIF}
end;

function TIcon.GetHeight: Integer;
begin
  Result := FImage.FSize.Y;
  if Result = 0 then
  begin
    if FImage.FMemoryImage <> nil then
    begin
      HandleNeeded;
      Result := FImage.FSize.Y;
    end
    else
      Result := GetSystemMetrics(SM_CYICON);
  end;
end;

function TIcon.GetWidth: Integer;
begin
  Result := FImage.FSize.X;
  if Result = 0 then
  begin
    if FImage.FMemoryImage <> nil then
    begin
      HandleNeeded;
      Result := FImage.FSize.X;
    end
    else
      Result := GetSystemMetrics(SM_CYICON);
  end;
end;

procedure TIcon.HandleNeeded;
{$IF DEFINED(CLR)}
var
  IconType: Word;
  NewIcon: System.Drawing.Icon;
begin
  NewIcon := nil;
  with FImage do
  begin
    if Assigned(FIcon) then Exit;
    if FMemoryImage = nil then Exit;
    FMemoryImage.Position := 0;
    IconType := GetStoredIconType(FMemoryImage);
    case IconType of
      RC3_STOCKICON: NewIcon := System.Drawing.Icon.FromHandle(StockIcon);
      RC3_ICON: NewIcon := System.Drawing.Icon.Create(FMemoryImage);
    else
      InvalidIcon;
    end;
    FSize.X := NewIcon.Size.Width;
    FSize.Y := NewIcon.Size.Height;
    FIcon := NewIcon;
  end;
{$ELSE}
var
  CI: TCursorOrIcon;
  NewHandle: HICON;
begin
  with FImage do
  begin
    if FHandle <> 0 then Exit;
    if FMemoryImage = nil then Exit;
    FMemoryImage.Position := 0;
    FMemoryImage.ReadBuffer(CI, SizeOf(CI));
    case CI.wType of
      RC3_STOCKICON: NewHandle := StockIcon;
      RC3_ICON: ReadIcon(FMemoryImage, NewHandle, CI.Count, SizeOf(CI),
        FRequestedSize, FSize);
    else
      InvalidIcon;
    end;
    FHandle := NewHandle;
  end;
{$ENDIF}
end;

procedure TIcon.ImageNeeded;
{$IF DEFINED(CLR)}
var
  Image: TMemoryStream;
begin
  with FImage do
  begin
    if FMemoryImage <> nil then Exit;
    if (not Assigned(FIcon)) or (Assigned(FIcon) and (FIcon.Handle = nil)) then
      InvalidIcon;
    Image := TMemoryStream.Create;
    try
      if GetHandle = StockIcon then
      begin
        // Stream out blank TCursorOrIcon structure
        Image.WriteBuffer(0, SizeOf(Word)); // TCursorOrIcon.Reserved
        Image.WriteBuffer(0, SizeOf(Word)); // TCursorOrIcon.wType
        Image.WriteBuffer(0, SizeOf(Word)); // TCursorOrIcon.Count
      end
      else
        FIcon.Save(Image);
    except
      Image.Free;
      raise;
    end;
    FMemoryImage := Image;
  end;
{$ELSE}
var
  Image: TMemoryStream;
  CI: TCursorOrIcon;
begin
  with FImage do
  begin
    if FMemoryImage <> nil then Exit;
    if FHandle = 0 then InvalidIcon;
    Image := TMemoryStream.Create;
    try
      if GetHandle = StockIcon then
      begin
        FillChar(CI, SizeOf(CI), 0);
        Image.WriteBuffer(CI, SizeOf(CI));
      end
      else
        WriteIcon(Image, Handle, False);
    except
      Image.Free;
      raise;
    end;
    FMemoryImage := Image;
  end;
{$ENDIF}
end;

{$IF DEFINED(CLR) OR DEFINED(MSWINDOWS)}
procedure TIcon.LoadFromResourceID(Instance: THandle; ResID: Integer);
begin
  SetHandle(LoadIcon(Instance, MakeIntResource(ResID)));
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TIcon.LoadFromResourceName(const ResName, BaseName: String;
  ResourceAssembly: Assembly; Culture: CultureInfo);
var
  ResMgr: ResourceManager;
begin
  ResMgr := ResourceManager.Create(BaseName, ResourceAssembly);
  NewImage(System.Drawing.Icon(ResMgr.GetObject(ResName, Culture)), nil);
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TIcon.LoadFromResourceName(const ResName, BaseName: String;
  ResourceAssembly: Assembly; ResourceSet: System.Type; Culture: CultureInfo = nil);
var
  ResMgr: ResourceManager;
begin
  ResMgr := ResourceManager.Create(BaseName, ResourceAssembly, ResourceSet);
  NewImage(System.Drawing.Icon(ResMgr.GetObject(ResName, Culture)), nil);
end;
{$ENDIF}

{$IF DEFINED(CLR)}
procedure TIcon.LoadFromResourceName(const ResName: String;
  AType: System.Type; Culture: CultureInfo);
var
  ResMgr: ResourceManager;
begin
  ResMgr := ResourceManager.Create(AType);
  NewImage(System.Drawing.Icon(ResMgr.GetObject(ResName, Culture)), nil);
end;
{$ENDIF}

procedure TIcon.LoadFromResourceName(Instance: THandle; const ResName: String);
begin
{$IF DEFINED(CLR)}
  SetHandle(LoadIcon(Instance, ResName));
{$ELSE}
  SetHandle(LoadIcon(Instance, PChar(ResName)));
{$ENDIF}
end;

class function TIcon.CanLoadFromStream(Stream: TStream): Boolean;
var
  CI: TCursorOrIcon;
  P: Int64;
begin
  P := Stream.Position;
  try
    Result := (Stream.Read(CI, SizeOf(CI)) = SizeOf(CI)) and
      (CI.Reserved = 0) and (CI.wType in [RC3_STOCKICON, RC3_ICON]);
  finally
    Stream.Position := P;
  end;
end;

procedure TIcon.LoadFromStream(Stream: TStream);
var
  Image: TMemoryStream;
{$IF DEFINED(CLR)}
  IconType: Word;
{$ELSE}
  CI: TCursorOrIcon;
{$ENDIF}
begin
  Image := TMemoryStream.Create;
  try
    Image.SetSize(Stream.Size - Stream.Position);
{$IF DEFINED(CLR)}
    Stream.ReadBuffer(Image.Memory, Image.Size);
    IconType := GetStoredIconType(Image);
    if not (IconType in [RC3_STOCKICON, RC3_ICON]) then
      InvalidIcon;
    NewImage(nil, Image);
{$ELSE}
    Stream.ReadBuffer(Image.Memory^, Image.Size);
    Image.ReadBuffer(CI, SizeOf(CI));
    if not (CI.wType in [RC3_STOCKICON, RC3_ICON]) then InvalidIcon;
    NewImage(0, Image);
{$ENDIF}
  except
    Image.Free;
    raise;
  end;
  Changed(Self);
end;

{$IF DEFINED(CLR)}
procedure TIcon.NewImage(NewIcon: System.Drawing.Icon; NewImage: TMemoryStream);
{$ELSE}
procedure TIcon.NewImage(NewHandle: HICON; NewImage: TMemoryStream);
{$ENDIF}
var
  Image: TIconImage;
begin
  Image := TIconImage.Create;
  try
{$IF DEFINED(CLR)}
    Image.FIcon := NewIcon;
{$ELSE}
    Image.FHandle := NewHandle;
{$ENDIF}
    Image.FMemoryImage := NewImage;
  except
    Image.Free;
    raise;
  end;
  Image.Reference;
  FImage.Release;
  FImage := Image;
end;

function TIcon.ReleaseHandle: HICON;
begin
  with FImage do
  begin
    if FRefCount > 1 then
{$IF DEFINED(CLR)}
      NewImage(System.Drawing.Icon(FIcon.Clone), nil);
    // Return copy of icon since the original will be destroyed
    // when the icon object is freed.
    Result := CopyIcon(HICON(FIcon.Handle));
    FIcon.Free;
    FIcon := nil;
{$ELSE}
      NewImage(CopyIcon(FHandle), nil);
    Result := FHandle;
    FHandle := 0;
{$ENDIF}
  end;
  Changed(Self);
end;

procedure TIcon.SetHandle(Value: HICON);
{$IF NOT DEFINED(CLR)}
var
  IconInfo: TIconInfo;
  BitmapInfo: Winapi.Windows.TBitmap;
{$ENDIF}
begin
{$IF DEFINED(CLR)}
  if Value = 0 then
    NewImage(nil, nil)
  else
    NewImage(System.Drawing.Icon.FromHandle(Value), nil);
{$ELSE}
  NewImage(Value, nil);
  if (Value <> 0) and GetIconInfo(Value, IconInfo) then
    try
      if GetObject(IconInfo.hbmColor, SizeOf(BitmapInfo), @BitmapInfo) <> 0 then
      begin
        FImage.FSize.X := BitmapInfo.bmWidth;
        FImage.FSize.Y := BitmapInfo.bmHeight;
      end;
    finally
      DeleteObject(IconInfo.hbmMask);
      DeleteObject(IconInfo.hbmColor);
    end;
{$ENDIF}
  Changed(Self);
end;

procedure TIcon.SetHeight(Value: Integer);
begin
{$IF DEFINED(CLR)}
  if not Assigned(FImage.FIcon) then
{$ELSE}
  if FImage.FHandle = 0 then
{$ENDIF}
    FRequestedSize.Y := Value
  else
    InvalidOperation({$IFNDEF CLR}@{$ENDIF}SChangeIconSize);
end;

procedure TIcon.SetSize(AWidth, AHeight: Integer);
begin
{$IF DEFINED(CLR)}
  if not Assigned(FImage.FIcon) then
{$ELSE}
  if FImage.FHandle = 0 then
{$ENDIF}
  begin
    FRequestedSize.X := AWidth;
    FRequestedSize.Y := AHeight;
  end
  else
    InvalidOperation({$IFNDEF CLR}@{$ENDIF}SChangeIconSize);
end;

procedure TIcon.SetTransparent(Value: Boolean);
begin
  // Ignore assignments to this property.
  // Icons are always transparent.
end;

procedure TIcon.SetWidth(Value: Integer);
begin
{$IF DEFINED(CLR)}
  if not Assigned(FImage.FIcon) then
{$ELSE}
  if FImage.FHandle = 0 then
{$ENDIF}
    FRequestedSize.X := Value
  else
    InvalidOperation({$IFNDEF CLR}@{$ENDIF}SChangeIconSize);
end;

procedure TIcon.SaveToStream(Stream: TStream);
begin
  ImageNeeded;
  with FImage.FMemoryImage do
    Stream.WriteBuffer(Memory{$IFNDEF CLR}^{$ENDIF}, Size);
end;

{$IFDEF CLR}[UIPermission(SecurityAction.LinkDemand, Clipboard=UIPermissionClipboard.AllClipboard)]{$ENDIF}
procedure TIcon.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
begin
  InvalidOperation({$IFNDEF CLR}@{$ENDIF}SIconToClipboard);
end;

procedure TIcon.SaveToClipboardFormat(var Format: Word; var Data: THandle;
  var APalette: HPALETTE);
begin
  InvalidOperation({$IFNDEF CLR}@{$ENDIF}SIconToClipboard);
end;

{ TWICGraphic }

{$IF NOT DEFINED(CLR)}
procedure TWICImage.Assign(Source: TPersistent);
var
  LImage: TWICImage;
begin
  if Source is TWICImage then
  begin
    LImage := TWICImage(Source);

    if Assigned(LImage.FWicBitmap) then
      FImagingFactory.CreateBitmapFromSource(LImage.FWicBitmap, WICBitmapNoCache, FWicBitmap);

    if Assigned(LImage.FBitmap) then
    begin
      if FBitmap = nil then FBitmap := TBitmap.Create;

      try
        FBitmap.Assign(LImage.FBitmap);
      except
        FreeAndNil(FBitmap);
        raise;
      end;
    end;

    FEncoderContainerFormat := LImage.FEncoderContainerFormat;
    FInterpolationMode := LImage.FInterpolationMode;
    FImageFormat := LImage.FImageFormat;
    FWidth := LImage.FWidth;
    FHeight := LImage.FHeight;
    FData.Clear;
    TWICImage(Source).FData.Position := 0;
    FData.CopyFrom(TWICImage(Source).FData, TWICImage(Source).FData.Size);

    FFormatChanged := TWICImage(Source).FFormatChanged;
    FreeAndNil(FScaledBuffer);
 end
  else if Source is TBitmap then
  begin
    FWicBitmap := nil;
    var LReleaseBitmap: Boolean := False;
    if FBitmap = nil then
    begin
      LReleaseBitmap := True;
      FBitmap := TBitmap.Create;
    end;
    try
      FBitmap.Assign(Source);
      CreateWicBitmap;
      ImageFormat := wifBmp;
      FreeAndNil(FScaledBuffer);
    finally
      if LReleaseBitmap then
        FreeAndNil(FBitmap);
    end;
  end
  else
    inherited;
end;

procedure TWICImage.AssignTo(Dest: TPersistent);
begin
  if Dest is TBitmap then
  begin
    RequireBitmap;
    Dest.Assign(FBitmap);
  end
  else
    inherited;
end;

class function TWICImage.GetImagingFactory: IWICImagingFactory;
begin
  Result := FImagingFactory;
end;

constructor TWICImage.Create;
var
  LResult: HResult;
begin
  inherited;
  FInterpolationMode := wipmNone;

  EnterCriticalSection(WicImageLock);
  try
    if FImagingFactory = nil then
    begin
      LResult := CoCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER or
        CLSCTX_LOCAL_SERVER, IUnknown, FImagingFactory);
      if Failed(LResult) then
        raise EInvalidGraphicOperation.CreateFmt(SWinRTInstanceError + ' (%X)', ['CLSID_WICImagingFactory', LResult]);
    end
    else
      FImagingFactory._AddRef;
  finally
    LeaveCriticalSection(WicImageLock);
  end;


  FEncoderContainerFormat := GUID_ContainerFormatBmp;
  FImageFormat := wifBmp;
  FData := TMemoryStream.Create;
  FFormatChanged := False;

end;

destructor TWICImage.Destroy;
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FScaledBuffer);
  FData.Free;

  EnterCriticalSection(WicImageLock);
  try
    if (FImagingFactory <> nil) and (FImagingFactory._Release = 0) then
      Pointer(FImagingFactory) := nil;
  finally
    LeaveCriticalSection(WicImageLock);
  end;

  inherited;
end;

procedure TWICImage.CreateWicBitmap;
var
  PixelFormat: TGUID;
  BitmapInfo: TBitmapInfo;
  Buffer: array of byte;
  hBMP: HBITMAP;
begin
  FData.Clear;
  FFormatChanged := True;

  if FBitmap.FAlphaFormat in [afDefined, afPremultiplied] then
    PixelFormat := GUID_WICPixelFormat32bppPBGRA
  else
    PixelFormat := GUID_WICPixelFormat32bppBGR;
  
  FBitmap.PixelFormat := pf32bit;

  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;

  SetLength(Buffer, FWidth * 4 * FHeight);

  FillChar(BitmapInfo, sizeof(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := SizeOf(BitmapInfo);
  BitmapInfo.bmiHeader.biWidth := FWidth;
  BitmapInfo.bmiHeader.biHeight := -FHeight;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;
  // Forces evaluation of Bitmap.Handle before Bitmap.Canvas.Handle
  hBMP := FBitmap.Handle;
  GetDIBits(FBitmap.Canvas.Handle,  hBMP, 0, FHeight, @Buffer[0],
    BitmapInfo, DIB_RGB_COLORS);

  FImagingFactory.CreateBitmapFromMemory(FWidth, FHeight, PixelFormat,
    FWidth * 4, Length(Buffer), @Buffer[0], FWicBitmap);
end;

function TWICImage.CreateScaledCopy(ANewWidth, ANewHeight: Integer; AInterpolationMode: TWICImageInterpolationMode = wipmHighQualityCubic): TWICImage;
const
  IMode: array[TWICImageInterpolationMode] of Integer =
    (-1,
     WICBitmapInterpolationModeHighQualityCubic,
     WICBitmapInterpolationModeFant,
     WICBitmapInterpolationModeCubic,
     WICBitmapInterpolationModeLinear,
     WICBitmapInterpolationModeNearestNeighbor);
var
  Factory: IWICImagingFactory;
  Scaler: IWICBitmapScaler;
begin
  Result := TWICImage.Create;
  if AInterpolationMode = wipmNone then
  begin
    Result.Assign(Self);
    Result.InterpolationMode := wipmNone;
    Exit;
  end;
  Factory := Result.ImagingFactory;
  Factory.CreateBitmapScaler(Scaler);
  Scaler.Initialize(Handle, ANewWidth, ANewHeight, IMode[AInterpolationMode]);
  Result.Handle := IWICBitmap(Scaler);
  Scaler := nil;
  Factory := nil;
end;

procedure TWICImage.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  if Rect.IsEmpty then
    Exit;

  if (FInterpolationMode <> wipmNone) and ((Rect.Width <> Width) or (Rect.Height <> Height)) then
  begin
    if (FScaledBuffer = nil) or (FScaledBuffer.Width <> Rect.Width) or (FScaledBuffer.Height <> Rect.Height) then
    begin
      FreeAndNil(FScaledBuffer);
      FScaledBuffer := CreateScaledCopy(Rect.Width, Rect.Height, FInterpolationMode);
    end;
    if FScaledBuffer <> nil then
      FScaledBuffer.Draw(ACanvas, Rect);
  end
  else
  begin
    RequireBitmap;
    if FBitmap <> nil then
      ACanvas.StretchDraw(Rect, FBitmap);
  end;
end;

function TWICImage.GetEmpty: Boolean;
begin
  Result := FWicBitmap = nil;
end;

function TWICImage.GetFrameCount: LongWord;
begin
  if FData = nil then
    Result := 0
  else
    Result := FFrameCount
end;

function TWICImage.GetHandle: IWicBitmap;
begin
  Result := FWicBitmap;
end;

function TWICImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TWICImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TWICImage.LoadFromStream(Stream: TStream);
var
  LStream: IStream;
  BitmapDecoder: IWICBitmapDecoder;
  LBitmapFrame: IWICBitmapFrameDecode;
  LGUID: TGUID;

  procedure WicCheck(Result: HRESULT);
  begin
    if Failed(Result) then
      raise EInvalidGraphic.Create(SInvalidImage);
  end;

begin
  FreeAndNil(FBitmap);
  FreeAndNil(FScaledBuffer);

  FData.Clear;
  FData.CopyFrom(Stream, Stream.Size - Stream.Position);
  FData.Position := 0;
  LStream := TStreamAdapter.Create(FData);

  WicCheck(FImagingFactory.CreateDecoderFromStream(LStream, guid_null, WICDecodeMetadataCacheOnDemand, BitmapDecoder));
  WicCheck(BitmapDecoder.GetFrameCount(FFrameCount));
  if FFrameIndex >= FrameCount then
    raise EInvalidOperation.CreateResFmt({$IFNDEF CLR}@{$ENDIF}SInvalidFrameIndex,
      [FFrameIndex, FFrameCount, Pred(FFrameCount)]);
  WicCheck(BitmapDecoder.GetContainerFormat(LGUID));
  EncoderContainerFormat := LGUID;
  WicCheck(BitmapDecoder.GetFrame(FFrameIndex, LBitmapFrame));
  WicCheck(FImagingFactory.CreateBitmapFromSource(LBitmapFrame, WICBitmapCacheOnLoad, FWicBitmap));
  WicCheck(FWicBitmap.GetSize(FWidth, FHeight));

  FFormatChanged := False;
end;

procedure TWICImage.SaveToStream(Stream: TStream);
var
  Encoder: IWICBitmapEncoder;
  Frame: IWICBitmapFrameEncode;
  Props: IPropertyBag2;
  LStreamAdapter: IStream;
  PixelFormat: TGUID;
  LStream: IWICStream;
  Palette: IWICPalette;
begin
  if FFormatChanged then
  begin
    FData.Clear;
    LStreamAdapter := TStreamAdapter.Create(FData);

    FImagingFactory.CreateStream(LStream);
    LStream.InitializeFromIStream(LStreamAdapter);
    FImagingFactory.CreateEncoder(FEncoderContainerFormat, guid_null, Encoder);

    Encoder.Initialize(LStream, WICBitmapEncoderNoCache);
    Encoder.CreateNewFrame(Frame, Props);

    Frame.Initialize(Props);
    FWicBitmap.GetPixelFormat(PixelFormat);
    Frame.SetPixelFormat(PixelFormat);

    Frame.SetSize(FWidth, FHeight);
                                       
    FImagingFactory.CreatePalette(Palette);
    FWicBitmap.CopyPalette(Palette);
    Frame.SetPalette(Palette);
    Frame.WriteSource(FWicBitmap, nil);
    Frame.Commit;
    Encoder.Commit;

    FFormatChanged := False;
  end;

  FData.Position := 0;
  Stream.CopyFrom(FData, FData.Size);
end;

procedure TWICImage.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
  FWicBitmap := nil;
  FreeAndNil(FScaledBuffer);
  if FBitmap = nil then
    FBitmap := TBitmap.Create;
  try
    FBitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
    CreateWicBitmap;
  except
    FreeAndNil(FBitmap);
    raise;
  end;
end;

procedure TWICImage.LoadFromResourceName(Instance: THandle; const ResName: String);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, RT_RCDATA);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TWICImage.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  RequireBitmap;
  FBitmap.SaveToClipboardFormat(AFormat, AData, APalette);
end;

procedure TWICImage.SetEncoderContainerFormat(const Value: TGUID);
begin
  if not IsEqualGUID(Value, FEncoderContainerFormat) then
  begin
    FFormatChanged := True;
    FEncoderContainerFormat := Value;
    if IsEqualGUID(Value, GUID_ContainerFormatBmp)       then FImageFormat := wifBmp
    else if IsEqualGuid(Value, GUID_ContainerFormatPng)  then FImageFormat := wifPng
    else if IsEqualGuid(Value, GUID_ContainerFormatJpeg) then FImageFormat := wifJpeg
    else if IsEqualGuid(Value, GUID_ContainerFormatTiff) then FImageFormat := wifTiff
    else if IsEqualGuid(Value, GUID_ContainerFormatGif)  then FImageFormat := wifGif
    else if IsEqualGuid(Value, GUID_ContainerFormatWmp)  then FImageFormat := wifWMPhoto
    else FImageFormat := wifOther;
  end;
end;

procedure TWICImage.SetImageFormat(const Value: TWICImageFormat);
begin
  if  Value <> FImageFormat then
  begin
    FFormatChanged := True;
    FImageFormat := Value;
    case Value of
      wifBmp:     FEncoderContainerFormat := GUID_ContainerFormatBmp;
      wifPng:     FEncoderContainerFormat := GUID_ContainerFormatPng;
      wifJpeg:    FEncoderContainerFormat := GUID_ContainerFormatJpeg;
      wifGif:     FEncoderContainerFormat := GUID_ContainerFormatGif;
      wifTiff:    FEncoderContainerFormat := GUID_ContainerFormatTiff;
      wifWMPhoto: FEncoderContainerFormat := GUID_ContainerFormatWmp;
      wifOther: ;
    end;
  end;
end;

procedure TWICImage.SetInterpolationMode(Value: TWICImageInterpolationMode);
begin
  if FInterpolationMode <> Value then
  begin
    FInterpolationMode := Value;
    FreeAndNil(FScaledBuffer);
  end;
end;

procedure TWICImage.SetHandle(const Value: IWicBitmap);
begin
  FreeAndNil(FBitmap);
  FreeAndNil(FScaledBuffer);
  FWicBitmap := nil;

  FData.Clear;
  FFormatChanged := True;

  FImagingFactory.CreateBitmapFromSource(Value, WICBitmapCacheOnLoad, FWicBitmap);
  FWicBitmap.GetSize(FWidth, FHeight);
end;

procedure TWICImage.SetHeight(Value: Integer);
begin
  InvalidOperation(@SChangeWicSize);
end;

procedure TWICImage.SetWidth(Value: Integer);
begin
  InvalidOperation(@SChangeWicSize);
end;

procedure TWICImage.RequireBitmap;
var
  LWicBitmap: IWICBitmapSource;
  Stride: Cardinal;
  Buffer: array of Byte;
  BitmapInfo: TBitmapInfo;
begin
  if Assigned(FBitmap) then Exit;
  if FWicBitmap = nil then Exit;

  FWicBitmap.GetSize(FWidth, FHeight);
  Stride := FWidth * 4;
  SetLength(Buffer, Stride * FHeight);
                                                                                                    
  WICConvertBitmapSource(GUID_WICPixelFormat32bppBGRA, FWicBitmap, LWicBitmap);
  LWicBitmap.CopyPixels(nil, Stride, Length(Buffer), @Buffer[0]);

  FillChar(BitmapInfo, sizeof(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := SizeOf(BitmapInfo);
  BitmapInfo.bmiHeader.biWidth := FWidth;
  BitmapInfo.bmiHeader.biHeight := -FHeight;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;

  FBitmap := TBitmap.Create;
  try
    FBitmap.PixelFormat := pf32bit;
    FBitmap.SetSize(FWidth, FHeight);
    {DC par not used (FBitmap.Canvas.Handle) since Usage = DIB_RGB_COLORS}
    SetDIBits(0, FBitmap.Handle, 0, FHeight,
      @Buffer[0], BitmapInfo, DIB_RGB_COLORS);
    FBitmap.AlphaFormat := afDefined;
  except
    FreeAndNil(FBitmap);
    raise;
  end;
end;

constructor TWICScaledGraphicDrawer.Create(AGraphic: TGraphic; AInitialize: Boolean);
begin
  FInterpolationMode := wipmHighQualityCubic;
  inherited Create(AGraphic, AInitialize);
end;

destructor  TWICScaledGraphicDrawer.Destroy;
begin
  if FScaledBuffer <> Graphic then
    FreeAndNil(FScaledBuffer)
  else
    if FScaledBuffer <> nil then
      FScaledBuffer.InterpolationMode := wipmNone;
  inherited;
end;

procedure TWICScaledGraphicDrawer.SetInterpolationMode(AValue: TWICImageInterpolationMode);
begin
  if FInterpolationMode <> AValue then
  begin
    FInterpolationMode := AValue;
    if Initialized then
      Initialize;
  end;
end;

procedure TWICScaledGraphicDrawer.Initialize;
var
  LStream: TStream;
begin
  if FScaledBuffer <> Graphic then
    FreeAndNil(FScaledBuffer)
  else
    FScaledBuffer := nil;

  if not Assigned(Graphic) or Graphic.Empty then
    Exit;

  if Graphic is TWICImage then
   FScaledBuffer := TWICImage(Graphic)
  else
  begin
    FScaledBuffer := TWICImage.Create;
    if Graphic is TBitmap then
      FScaledBuffer.Assign(Graphic)
    else
    begin
      LStream := TMemoryStream.Create;
      try
        Graphic.SaveToStream(LStream);
        LStream.Seek(0, TSeekOrigin.soBeginning);
        try
          FScaledBuffer.LoadFromStream(LStream);
        except
          FreeAndNil(FScaledBuffer);
          raise;
        end;
      finally
        LStream.Free;
      end;
    end;
   end;

  if FScaledBuffer <> nil then
    FScaledBuffer.InterpolationMode := FInterpolationMode;
end;

procedure TWICScaledGraphicDrawer.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if Initialized and not ARect.IsEmpty then
    FScaledBuffer.Draw(ACanvas, ARect);
end;

function TWICScaledGraphicDrawer.GetInitialized: Boolean;
begin
  Result := FScaledBuffer <> nil;
end;

{$ENDIF}

function GraphicFilter(GraphicClass: TGraphicClass): string;
var
  Filters: string;
begin
  GetFileFormats.BuildFilterStrings(GraphicClass, Result, Filters);
end;

function GraphicExtension(GraphicClass: TGraphicClass): string;
var
  I: Integer;
begin
  for I := GetFileFormats.Count-1 downto 0 do
    if TFileFormatType(FileFormats[I]).GraphicClass.ClassName = GraphicClass.ClassName then
    begin
      Result := TFileFormatType(FileFormats[I]).Extension;
      Exit;
    end;
  Result := '';
end;

function GraphicFileMask(GraphicClass: TGraphicClass): string;
var
  Descriptions: string;
begin
  GetFileFormats.BuildFilterStrings(GraphicClass, Descriptions, Result);
end;

procedure InitScreenLogPixels;
const
  Pal16: array [0..15] of TColor =
    (clBlack, clMaroon, clGreen, clOlive, clNavy, clPurple, clTeal, clDkGray,
     clLtGray, clRed, clLime, clYellow, clBlue, clFuchsia, clAqua, clWhite);
var
  DC: HDC;
begin
  DC := GetDC(0);
  try
    ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
  finally
    ReleaseDC(0,DC);
  end;
{$IFDEF CLR}
  SystemPalette16 := TResHandleWrapper.Create;
  SystemPalette16.Handle := CreateSystemPalette(Pal16);
{$ELSE !CLR}
  SystemPalette16 := CreateSystemPalette(Pal16);
{$ENDIF}
end;

function GetDefFontCharSet: TFontCharSet;
var
  DisplayDC: HDC;
  TxtMetric: TTEXTMETRIC;
begin
  Result := DEFAULT_CHARSET;
  DisplayDC := GetDC(0);
  if (DisplayDC <> 0) then
  begin
    if (SelectObject(DisplayDC, StockFont) <> 0) then
      if (GetTextMetrics(DisplayDC, TxtMetric)) then
        Result := TxtMetric.tmCharSet;
    ReleaseDC(0, DisplayDC);
  end;
end;

{$IFDEF CLR}[RegistryPermission(SecurityAction.Assert, Read='HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes\')]{$ENDIF}
procedure InitDefFontData;
const
  sFontSubstitutes = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes'; { do not localize }
{$IF DEFINED(CLR)}
var
  Name: string;
  Key: Microsoft.Win32.RegistryKey;
begin
  DefFontData := TFontData.Create;
  DefFontData.Pitch := fpDefault;
  DefFontData.CharSet := DEFAULT_CHARSET;
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (GetDefFontCharset = SHIFTJIS_CHARSET) then
    Name := 'Tahoma';
  Key := Microsoft.Win32.Registry.LocalMachine.OpenSubKey(sFontSubstitutes, False);
  if Assigned(Key) then
    Name := string(Key.GetValue('MS Shell Dlg 2')); { do not localize }
  if Name <> '' then
    DefFontData.Name := Name
  else
    DefFontData.Name := 'MS Sans Serif'; { do not localize }
  DefFontData.Height := -MulDiv(8, ScreenLogPixels, 72);
  DefFontData.Orientation := 0; { No rotation }
{$ELSE}
var
  Reg: TRegistry;
  Name: string;
begin
  if TOSVersion.Check(6) then
  begin
    DefFontData.Height := -MulDiv(9, ScreenLogPixels, 72);
    DefFontData.Name := 'Segoe UI';
    Exit;
  end;

  DefFontData.Height := -MulDiv(8, ScreenLogPixels, 72);
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and (GetDefFontCharset = SHIFTJIS_CHARSET) then
    Name := 'Tahoma';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly(sFontSubstitutes) then { do not localize }
    begin
      Name := Reg.ReadString('MS Shell Dlg 2'); { do not localize }
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  if Name <> '' then
    DefFontData.Name := UTF8EncodeToShortString(Name);
{$ENDIF}
end;

type
{$IF DEFINED(CLR)}
  TPattern = class
    Next: TPattern;
    Bitmap: TBitmap;
    BkColorRef: TColorRef;
    FgColorRef: TColorRef;
  end;

  TPatternType = TPattern;
{$ELSE}
  PPattern = ^TPattern;
  TPattern = record
    Next: PPattern;
    Bitmap: TBitmap;
    BkColorRef: TColorRef;
    FgColorRef: TColorRef;
  end;

  TPatternType = PPattern;
{$ENDIF}

  TPatternManager = class(TObject)
  private
    List: TPatternType;
{$IF NOT DEFINED(CLR)}
    FLock: TRTLCriticalSection;
{$ENDIF}
    function CreateBitmap(BkColor, FgColor: TColor): TBitmap;
  public
    function AllocPattern(BkColor, FgColor: TColorRef): TPatternType;
{$IF NOT DEFINED(CLR)}
    constructor Create;
    destructor Destroy; override;
    procedure FreePatterns;
{$ENDIF}
    procedure Lock;
    procedure Unlock;
  end;

{$IF NOT DEFINED(CLR)}
constructor TPatternManager.Create;
begin
  InitializeCriticalSection(FLock);
end;
{$ENDIF}

{$IF NOT DEFINED(CLR)}
destructor TPatternManager.Destroy;
begin
  FreePatterns;
  DeleteCriticalSection(FLock);
end;
{$ENDIF}

procedure TPatternManager.Lock;
begin
{$IF DEFINED(CLR)}
  System.Threading.Monitor.Enter(Self);
{$ELSE}
  EnterCriticalSection(FLock);
{$ENDIF}
end;

procedure TPatternManager.Unlock;
begin
{$IF DEFINED(CLR)}
  System.Threading.Monitor.Exit(Self);
{$ELSE}
  LeaveCriticalSection(FLock);
{$ENDIF}
end;

function TPatternManager.AllocPattern(BkColor, FgColor: TColorRef): TPatternType;
begin
  Lock;
  try
    Result := List;
    while (Result <> nil) and ((Result.BkColorRef <> BkColor) or
      (Result.FgColorRef <> FgColor)) do
      Result := Result.Next;
    if Result = nil then
    begin
{$IF DEFINED(CLR)}
      Result := TPattern.Create;
      with Result do
{$ELSE}
      GetMem(Result, SizeOf(TPattern));
      with Result^ do
{$ENDIF}
      begin
        Next := List;
        Bitmap := CreateBitmap(BkColor, FgColor);
        BkColorRef := BkColor;
        FgColorRef := FgColor;
      end;
      List := Result;
    end;
  finally
    Unlock;
  end;
end;

function TPatternManager.CreateBitmap(BkColor, FgColor: TColor): TBitmap;
var
  X, Y: Integer;
begin
  Result := TBitmap.Create;
  try
    with Result do
    begin
      Width := 8;
      Height := 8;
      with Canvas do
      begin
        Brush.Style := bsSolid;
        Brush.Color := BkColor;
        FillRect(Rect(0, 0, Width, Height));
        for Y := 0 to 8 do
          for X := 0 to 8 do
            if (Y mod 2) = (X mod 2) then  { toggles between even/odd pixles }
              Pixels[X, Y] := FgColor;     { on even/odd rows }
      end;
      Dormant;
    end;
  except
    Result.Free;
    raise;
  end;
end;

{$IF NOT DEFINED(CLR)}
procedure TPatternManager.FreePatterns;
var
  P: PPattern;
begin
  while List <> nil do
  begin
    P := List;
    with P^ do
    begin
      Lock;
      try
        List := Next
      finally
        Unlock;
      end;
      if Bitmap <> nil then Bitmap.Free;
    end;
    FreeMem(P);
  end;
end;
{$ENDIF}

var
  PatternManager: TPatternManager;


function AllocPatternBitmap(BkColor, FgColor: TColor): TBitmap;
begin
  if PatternManager <> nil then
    Result := PatternManager.AllocPattern(ColorToRGB(BkColor),
      ColorToRGB(FgColor)).Bitmap
    else
      Result := nil;
end;

{ TTextFormatFlags }

class operator TTextFormatFlags.Implicit(Value: TTextFormatFlags): TDrawTextFlags;
const
  CFlags: array[TTextFormats] of Cardinal = (
    DT_BOTTOM, DT_CALCRECT, DT_CENTER, DT_EDITCONTROL, DT_END_ELLIPSIS,
    DT_PATH_ELLIPSIS, DT_EXPANDTABS, DT_EXTERNALLEADING, DT_LEFT,
    DT_MODIFYSTRING, DT_NOCLIP, DT_NOPREFIX, DT_RIGHT, DT_RTLREADING,
    DT_SINGLELINE, DT_TOP, DT_VCENTER, DT_WORDBREAK, DT_HIDEPREFIX,
    DT_NOFULLWIDTHCHARBREAK, DT_PREFIXONLY, DT_TABSTOP, DT_WORD_ELLIPSIS,
    MASK_TF_COMPOSITED {tfComposited});
var
  LDrawTextFlag: TTextFormats;
begin
  Result := 0;
  for LDrawTextFlag := Low(TTextFormats) to High(TTextFormats) do
    if (LDrawTextFlag in Value.FValue) then
      Result := Result or CFlags[LDrawTextFlag];
end;

class operator TTextFormatFlags.Implicit(Value: TTextFormat): TTextFormatFlags;
begin
  Result.FValue := Value;
end;

class operator TTextFormatFlags.Implicit(Value: TTextFormatFlags): TTextFormat;
begin
  Result := Value.FValue;
end;

class operator TTextFormatFlags.Implicit(Value: Cardinal): TTextFormatFlags;
begin
  Result.FValue := [];
  if (Value and DT_BOTTOM) = DT_BOTTOM then
    Include(Result.FValue, tfBottom);
  if (Value and DT_CALCRECT) = DT_CALCRECT then
    Include(Result.FValue, tfCalcRect);
  if (Value and DT_CENTER) = DT_CENTER then
    Include(Result.FValue, tfCenter);
  if (Value and DT_EDITCONTROL) = DT_EDITCONTROL then
    Include(Result.FValue, tfEditControl);
  if (Value and DT_END_ELLIPSIS) = DT_END_ELLIPSIS then
    Include(Result.FValue, tfEndEllipsis);
  if (Value and DT_PATH_ELLIPSIS) = DT_PATH_ELLIPSIS then
    Include(Result.FValue, tfPathEllipsis);
  if (Value and DT_EXPANDTABS) = DT_EXPANDTABS then
    Include(Result.FValue, tfExpandTabs);
  if (Value and DT_EXTERNALLEADING) = DT_EXTERNALLEADING then
    Include(Result.FValue, tfExternalLeading);
  if (Value and DT_LEFT) = DT_LEFT then
    Include(Result.FValue, tfLeft);
  if (Value and DT_MODIFYSTRING) = DT_MODIFYSTRING then
    Include(Result.FValue, tfModifyString);
  if (Value and DT_NOCLIP) = DT_NOCLIP then
    Include(Result.FValue, tfNoClip);
  if (Value and DT_NOPREFIX) = DT_NOPREFIX then
    Include(Result.FValue, tfNoPrefix);
  if (Value and DT_RIGHT) = DT_RIGHT then
    Include(Result.FValue, tfRight);
  if (Value and DT_RTLREADING) = DT_RTLREADING then
    Include(Result.FValue, tfRtlReading);
  if (Value and DT_SINGLELINE) = DT_SINGLELINE then
    Include(Result.FValue, tfSingleLine);
  if (Value and DT_TOP) = DT_TOP then
    Include(Result.FValue, tfTop);
  if (Value and DT_VCENTER) = DT_VCENTER then
    Include(Result.FValue, tfVerticalCenter);
  if (Value and DT_WORDBREAK) = DT_WORDBREAK then
    Include(Result.FValue, tfWordBreak);
  if (Value and DT_HIDEPREFIX) = DT_HIDEPREFIX then
    Include(Result.FValue, tfHidePrefix);
  if (Value and DT_NOFULLWIDTHCHARBREAK) = DT_NOFULLWIDTHCHARBREAK then
    Include(Result.FValue, tfNoFullWidthCharBreak);
  if (Value and DT_PREFIXONLY) = DT_PREFIXONLY then
    Include(Result.FValue, tfPrefixOnly);
  if (Value and DT_TABSTOP) = DT_TABSTOP then
    Include(Result.FValue, tfTabStop);
  if (Value and DT_WORD_ELLIPSIS) = DT_WORD_ELLIPSIS then
    Include(Result.FValue, tfWordEllipsis);
  if (Value and MASK_TF_COMPOSITED) = MASK_TF_COMPOSITED then
    Include(Result.FValue, tfComposited);
end;

{ TGDIHandleRecall }

constructor TGDIHandleRecall.Create(DC: HDC; GDIObject: Cardinal);
begin
  FDC := DC;
  FGDIObj := GetCurrentObject(FDC, GDIObject);
  FCanvas := TCanvas.Create;
  FCanvas.Handle := FDC;
end;

destructor TGDIHandleRecall.Destroy;
begin
  FCanvas.Handle := 0;
  FCanvas.Free;
  SelectObject(FDC, FGDIObj);
end;

initialization
  InitScreenLogPixels;
{$IF DEFINED(CLR)}
  BitmapImageLock := TObject.Create;
{$ELSE}
  InitializeCriticalSection(BitmapImageLock);
  InitializeCriticalSection(CounterLock);
  InitializeCriticalSection(WicImageLock);
{$ENDIF}
  StockPen := GetStockObject(BLACK_PEN);
  StockBrush := GetStockObject(HOLLOW_BRUSH);
  StockFont := GetStockObject(SYSTEM_FONT);
  StockIcon := LoadIcon(0, IDI_APPLICATION);
  InitDefFontData;
{$IF DEFINED(CLR)}
  FontManager := TResourceManager.Create;
  PenManager := TResourceManager.Create;
  BrushManager := TResourceManager.Create;
{$ELSE}
  FontManager := TResourceManager.Create(SizeOf(TFontData));
  PenManager := TResourceManager.Create(SizeOf(TPenData));
  BrushManager := TBrushResourceManager.Create(SizeOf(TBrushData));
{$ENDIF}
  PatternManager := TPatternManager.Create;
  BitmapCanvasList := TThreadList.Create;
  CanvasList := TThreadList.Create;
{$IF DEFINED(CLR)}
  RegisterIntegerConsts(TypeOf(TColor), IdentToColor, ColorToIdent);
  RegisterIntegerConsts(TypeOf(TFontCharset), IdentToCharset, CharsetToIdent);
{$ELSE}
  RegisterColorIntegerConsts;
  RegisterAlphaColorIntegerConsts;
  RegisterIntegerConsts(TypeInfo(TFontCharset), IdentToCharset, CharsetToIdent);
{$ENDIF}
  System.UITypes.TColorRec.ColorToRGB := ColorToRGB;

{$IF NOT DEFINED(CLR)}
finalization
  PatternManager.Free;
  FreeAndNil(FileFormats);
  FreeAndNil(ClipboardFormats);
  FreeMemoryContexts;
  BitmapCanvasList.Free;
  CanvasList.Free;
  FontManager.Free;
  PenManager.Free;
  BrushManager.Free;
  DeleteObject(SystemPalette16);
  DeleteCriticalSection(BitmapImageLock);
  DeleteCriticalSection(CounterLock);
  DeleteCriticalSection(WicImageLock);
{$ENDIF}
end.
