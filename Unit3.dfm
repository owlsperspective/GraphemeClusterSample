object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  DesignSize = (
    624
    441)
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 45
    Height = 15
    Caption = 'Source : '
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 47
    Height = 15
    Caption = 'Output : '
  end
  object Edit1: TEdit
    Left = 56
    Top = 8
    Width = 553
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Edit2: TEdit
    Left = 56
    Top = 40
    Width = 553
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 609
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Button1: TButton
    Left = 8
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Convert'
    TabOrder = 3
    OnClick = Button1Click
  end
end
