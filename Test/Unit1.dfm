object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 636
  ClientWidth = 1103
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object scrlbx1: TScrollBox
    Left = 0
    Top = 0
    Width = 1103
    Height = 617
    HorzScrollBar.Smooth = True
    HorzScrollBar.Tracking = True
    VertScrollBar.Smooth = True
    VertScrollBar.Tracking = True
    Align = alClient
    TabOrder = 0
    object imgShow: TImage
      Left = 0
      Top = 0
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object statTip: TStatusBar
    Left = 0
    Top = 617
    Width = 1103
    Height = 19
    Panels = <
      item
        Width = 200
      end
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    AutoHotkeys = maManual
    Left = 144
    Top = 28
    object File1: TMenuItem
      Caption = #25991#20214
      object mniFileOepn: TMenuItem
        Caption = #25171#24320'...'
        OnClick = mniFileOepnClick
      end
      object mniFileLine01: TMenuItem
        Caption = '-'
      end
      object mniFileRestore: TMenuItem
        Caption = #24674#22797
        OnClick = mniFileRestoreClick
      end
    end
    object mniColor: TMenuItem
      Caption = #39068#33394
      object mniColorGray: TMenuItem
        Caption = #28784#24230#22270
        OnClick = mniColorGrayClick
      end
      object mniColorInvert: TMenuItem
        Caption = #21453#33394
        OnClick = mniColorInvertClick
      end
      object mniColorLine01: TMenuItem
        Caption = '-'
      end
      object mniColorLight: TMenuItem
        Caption = #20142#24230'...'
        OnClick = mniColorLightClick
      end
      object mniColorContrast: TMenuItem
        Tag = 1
        Caption = #23545#27604#24230'...'
        OnClick = mniColorLightClick
      end
      object mniColorSaturation: TMenuItem
        Tag = 2
        Caption = #39281#21644#24230'...'
        OnClick = mniColorLightClick
      end
      object mniColorMode: TMenuItem
        Tag = 3
        Caption = #33394#24425'...'
        OnClick = mniColorLightClick
      end
      object mniColorTrans: TMenuItem
        Tag = 4
        Caption = #36879#26126'...'
        OnClick = mniColorLightClick
      end
    end
    object mniEffect: TMenuItem
      Caption = #25928#26524
      object mniEffectExposure: TMenuItem
        Caption = #26333#20809
        OnClick = mniEffectExposureClick
      end
      object mniEffectEmboss: TMenuItem
        Tag = 1
        Caption = #28014#38613
        OnClick = mniEffectExposureClick
      end
      object mniEffectEngrave: TMenuItem
        Tag = 2
        Caption = #38613#21051
        OnClick = mniEffectExposureClick
      end
      object mniEffectBlur: TMenuItem
        Tag = 3
        Caption = #27169#31946
        OnClick = mniEffectExposureClick
      end
      object mniEffectSharpen: TMenuItem
        Tag = 4
        Caption = #38160#21270
        OnClick = mniEffectExposureClick
      end
      object mniEffectSponge: TMenuItem
        Tag = 5
        Caption = #27833#30011
        OnClick = mniEffectExposureClick
      end
      object mniEffectSand: TMenuItem
        Tag = 6
        Caption = #30952#30722
        OnClick = mniEffectExposureClick
      end
      object mniEffectDitherBmp: TMenuItem
        Tag = 7
        Caption = #25238#21160
        OnClick = mniEffectExposureClick
      end
    end
    object mniGeometry: TMenuItem
      Caption = #20960#20309
      object mniGeometryHMirror: TMenuItem
        Caption = #27700#24179#32763#36716
        OnClick = mniGeometryHVMirrorClick
      end
      object mniGeometryVMirror: TMenuItem
        Tag = 1
        Caption = #22402#30452#32763#36716
        OnClick = mniGeometryHVMirrorClick
      end
      object mniGeometryHVMirror: TMenuItem
        Tag = 2
        Caption = #36716#32622
        OnClick = mniGeometryHVMirrorClick
      end
      object mniGeometryRotate: TMenuItem
        Caption = #26059#36716
        OnClick = mniGeometryRotateClick
      end
    end
    object mniSize: TMenuItem
      Caption = #22823#23567
      object mniSizeActual: TMenuItem
        Caption = #23454#38469#22823#23567
        OnClick = mniSizeActualClick
      end
      object mniSizeStrecth: TMenuItem
        Caption = #25289#20280#20197#31526#21512#31383#21475
        OnClick = mniSizeStrecthClick
      end
    end
  end
  object dlgOpenPic: TOpenPictureDialog
    Left = 144
    Top = 96
  end
end
