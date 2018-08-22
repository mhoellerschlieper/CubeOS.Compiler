object FRMSearch: TFRMSearch
  Left = 374
  Top = 193
  BorderStyle = bsDialog
  Caption = 'Text suchen'
  ClientHeight = 164
  ClientWidth = 299
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnActivate = FormActivate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 41
    Width = 299
    Height = 82
    Align = alClient
    TabOrder = 0
    TabStop = True
    object rgStartSearch: TRadioGroup
      Left = 8
      Top = 8
      Width = 281
      Height = 60
      Caption = 'Beginn'
      ItemIndex = 0
      Items.Strings = (
        'ab &Cursor'
        'ab &Textanfang')
      TabOrder = 0
      TabStop = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 299
    Height = 41
    Align = alTop
    TabOrder = 1
    TabStop = True
    object Label1: TLabel
      Left = 24
      Top = 16
      Width = 69
      Height = 13
      Caption = '&Suchen Nach:'
      OnClick = Label1Click
    end
    object edSearch: TEdit
      Left = 96
      Top = 12
      Width = 193
      Height = 21
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 123
    Width = 299
    Height = 41
    Align = alBottom
    TabOrder = 2
    TabStop = True
    object Button1: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = '&OK'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 211
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Abbrechen'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
