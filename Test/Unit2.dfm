object Form2: TForm2
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #35843#33410#65306
  ClientHeight = 293
  ClientWidth = 589
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 20
    Top = 24
    Width = 36
    Height = 13
    Caption = #20142#24230#65306
  end
  object lbl2: TLabel
    Left = 20
    Top = 88
    Width = 48
    Height = 13
    Caption = #23545#27604#24230#65306
  end
  object lbl3: TLabel
    Left = 20
    Top = 136
    Width = 48
    Height = 13
    Caption = #39281#21644#24230#65306
  end
  object lbl4: TLabel
    Left = 540
    Top = 24
    Width = 24
    Height = 13
    Caption = '000'
    Font.Charset = GB2312_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = #23435#20307
    Font.Style = [fsBold]
    ParentFont = False
  end
  object trckbrLight: TTrackBar
    Left = 72
    Top = 20
    Width = 441
    Height = 29
    LineSize = 10
    Max = 255
    Min = -255
    PageSize = 10
    Frequency = 10
    TabOrder = 0
    OnChange = trckbrLightChange
  end
end
