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
  OldCreateOrder = False
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
    Left = 40
    Top = 24
  end
end
