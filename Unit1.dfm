object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 441
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -37
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  DesignSize = (
    624
    441)
  TextHeight = 50
  object SkLabel1: TSkLabel
    Left = 32
    Top = 80
    Width = 569
    Height = 57
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    TextSettings.Font.Size = 37.000000000000000000
    Words = <
      item
        Caption = 'SkLabel1'
      end>
  end
  object Edit1: TEdit
    Left = 24
    Top = 16
    Width = 585
    Height = 58
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object Button1: TButton
    Left = 32
    Top = 152
    Width = 185
    Height = 57
    Caption = 'RegEx'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 224
    Top = 152
    Width = 185
    Height = 57
    Caption = 'Skia'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 24
    Top = 224
    Width = 417
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ScrollBox1: TScrollBox
    Left = 456
    Top = 224
    Width = 153
    Height = 193
    Anchors = [akTop, akRight, akBottom]
    TabOrder = 4
    UseWheelForScrolling = True
    object SkLabel2: TSkLabel
      Left = 0
      Top = 0
      Width = 149
      Height = 98
      Align = alTop
      TextSettings.Font.Size = 37.000000000000000000
      TextSettings.VertAlign = Leading
      Words = <
        item
          Caption = 'test'#13#10'caption'
        end>
    end
  end
  object CheckBox1: TCheckBox
    Left = 424
    Top = 152
    Width = 177
    Height = 57
    Caption = 'EastAsia'
    TabOrder = 5
  end
end
