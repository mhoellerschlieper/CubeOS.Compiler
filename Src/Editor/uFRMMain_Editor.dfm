object frmMain: TfrmMain
  Left = 366
  Top = 35
  Width = 756
  Height = 812
  Caption = 'CubeOOP  -  CubeOS - (c) MHS 2007'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  ShowHint = True
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 26
    Width = 748
    Height = 716
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'keine Datei geladen'
      object memEditor: TMemoEx
        Left = 0
        Top = 0
        Width = 740
        Height = 624
        Cursor = crIBeam
        TabOrder = 0
        BorderStyle = bsNone
        ScrollBars = ssBoth
        GutterWidth = 50
        GutterColor = 15659506
        RightMargin = 0
        RightMarginColor = clSilver
        Completion.Enabled = False
        Completion.Separator = '='
        Completion.Identifiers.Strings = (
          'program=Program: Beginn eines Programms'
          'begin=Begin'
          'end=End'
          'function=Function'
          'procedure=Procedure'
          'if=If'
          'then=Then'
          'else=Else'
          'while=While'
          'for=For'
          'do=Do'
          'with=With'
          'repeat=Repeat'
          'var=Var'
          'type=Type'
          'const=Const'
          'until=Until'
          'string=String'
          'integer=Integer'
          'select=Select'
          'asm=Asm'
          'perm=perm'
          'include=include'
          'cut=cut'
          'uncut=uncut'
          'back=back'
          'forward=forward'
          'step=step'
          'is=is'
          'base=base'
          'implements=implements'
          'Attribute=Attribute'
          'Get=get'
          'Set=set'
          'of=of'
          'public=public'
          'private=private'
          'Pointer=Pointer'
          'Class=Class'
          'Main=Main'
          'and=and'
          'or=or')
        Completion.AutoChange.Strings = (
          'tmemoex=TMemoEx component')
        Completion.ColoringIdent = True
        Completion.ItemHeight = 13
        Completion.Interval = 10
        Completion.ListBoxStyle = lbStandard
        Completion.CaretChar = '|'
        Completion.CRLF = '/n'
        Completion.ReducePopup = True
        Completion.IdentifierColor = 33023
        TabSize = 4
        IndentSize = 4
        AutoIndentSize = 0
        KeepTrailingBlanks = True
        SelForeColor = clGray
        SelBackColor = clWhite
        Font.Charset = ANSI_CHARSET
        Font.Color = clYellow
        Font.Height = -21
        Font.Name = 'Courier New'
        Font.Pitch = fpFixed
        Font.Style = []
        StripInvisible = True
        FixedTextFontColor = clRed
        LinesOK = 0
        LinesOKFontColor = clSilver
        OnChangeStatus = memEditorChangeStatus
        Align = alClient
        Color = 11403264
        Ctl3D = True
        ParentColor = False
        TabStop = True
        UseDockManager = False
        WordWrap = False
      end
      object memErrors: TMemo
        Left = 0
        Top = 624
        Width = 740
        Height = 64
        Align = alBottom
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        Visible = False
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 748
    Height = 26
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    Flat = True
    TabOrder = 1
    object SpeedButton1: TSpeedButton
      Left = 0
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Neu'
      Flat = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        6666666666666666666666660000666660000000000666666668888888888666
        00006666687777777706666666687777777786660000666668FFFFFFF7066666
        6668F7F7F7F786660000666668FFFFFFF706666666687F7F7F77866600006666
        68FFFFFFF70666666668F7F7F7F786660000666668FFFFFFF706666666687F7F
        7F7786660000666668FFFFFFF70666666668F7F7F7F786660000666668FFFFFF
        F706666666687F7F7F778666000066F668FFFFFFF7066666F668F7F7F7F78666
        0000668F88FF8FF0000666668F887F8F7888866600006668B8F8FFF7F8666666
        6878F8F7F7F86666000066FF8FBFFFF786666666FF8F7F7F7786666600006668
        8BFF8888666666666887FF888866666600006668B8B8F66666666666687878F6
        666666660000668F68F66666666666668F68F66666666666000066F668F668F6
        66666666F668F668F66666660000666668F66666666666666668F66666666666
        0000}
      NumGlyphs = 2
      Transparent = False
      OnClick = SpeedButton1Click
    end
    object btnOpen: TSpeedButton
      Left = 23
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Laden'
      Flat = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000666666666666666666666666666666666666000066660000000000000666
        6666666666666666000066688888888888800666688888888888886600006668
        FB7B7B7B7B80066668FF7F7F7F7F88660000668FB7B7B7B7B70806668FF7F7F7
        F7F788660000668F7B7B7B7B780806668F7F7F7F7F787866000068F7B7B7B7B7
        B0880668F7F7F7F7F7F8F866000068FFFFFFFFFF80780668FFFFFFFFFF8F7866
        000068888888888888B80668888888888887F86600006668FB7B7B7B7B780666
        68FF7F7F7F7F786600006668F7B7B7FFFFF8066668F7F7F7FFFFF86600006668
        FB7B7F888888666668FF7F7F88888666000066668FFFF86666666666668FFFF8
        6666666600006666688886666666666666688886666666660000666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000}
      NumGlyphs = 2
      Transparent = False
      OnClick = btnOpenClick
    end
    object btnSave: TSpeedButton
      Left = 46
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Speichern'
      Flat = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000666000000000000066666888888888888866000066044074477704406666
        88887FF77788886600006604407447770440666688887FF77788886600006604
        407447770440666688887FF77788886600006604447777774440666688887777
        7788886600006604444444444440666688888888888888660000660440000000
        044066668888888888888866000066040FFFFFFFF0406666888FFFFFFFF88866
        000066040FFFFFFFF0406666888FFFFFFFF88866000066040F888888F0406666
        888F888888F88866000066040FFFFFFFF0406666888FFFFFFFF8886600006607
        0F888888F0006666878F888888F88866000066040FFFFFFFF0406666888FFFFF
        FFF8886600006600000000000000666688888888888888660000666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000}
      NumGlyphs = 2
      Transparent = False
      OnClick = btnSaveClick
    end
    object ToolButton1: TToolButton
      Left = 69
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object btnPrint: TSpeedButton
      Left = 77
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Drucken'
      Flat = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        66666666666666FFF66666660000666666600066666666666666F888FFF66666
        00006666600788006666666666F8866888FFF666000066600778008800666666
        688668888888FFF60000660778878800880066668F688688888888F600006688
        877788880080666688866688888888F600006887777788888800666886666688
        888888F6000068F7777F888888880668F6666F888888888F000068F77FF77788
        88880668F66FF6FF8888888F000068FFF779977788880668FFF6F8866688888F
        000068F77AA7778807880668F6688666888F888600006688F77788FF07006666
        88F666886686F8660000666688F8FFFFF06666666688F8F666686F6600006666
        6688FFFFFF0666666666886F666686FF0000666666668FFFFFF0066666666686
        F666F88600006666666668FFF8866666666666686FF886660000666666666688
        8666666666666666888666660000666666666666666666666666666666666666
        0000}
      NumGlyphs = 2
      Transparent = False
      OnClick = btnPrintClick
    end
    object ToolButton2: TToolButton
      Left = 100
      Top = 0
      Width = 13
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object btnShowTrend: TSpeedButton
      Left = 113
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Statistik'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000010000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        88880000000000000000088888888888888808CCCCCC8888888808CC88CC8888
        888808888888C8888118081188888C8881180811888888C8588808885888888C
        8888088885888858C8880888885885888C880888888118888CC8088888811888
        8CC8088888888888888808888888888888888888888888888888}
      ParentShowHint = False
      ShowHint = True
      Transparent = False
      OnClick = btnShowTrendClick
    end
    object SpeedButton7: TSpeedButton
      Left = 136
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Optionen'
      Flat = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        6666666666666666666666660000666666008080666666666666868688F66666
        0000666666080800666666666666886868F66666000066666600808066666666
        6666868688F66666000066666680000866666666666688888866666600006666
        6668F70666666666666668F68F666666000066666668F70666666666666668F6
        8F666666000066666668F70666666666666668F68F666666000066666668F706
        66666666666668F68F666666000066666668F70666666666666668F68F66666F
        000066666660000666660666666668888FF66F8F000066006608888066600666
        88668F6668FFF88F000068880087778000070668F688F66668888686000068F7
        7777777888706668F66666666666F866000068F77F77777777866668F66F6666
        666F8666000068FF88FFFFFF88666668FF88FFFFFF8866660000668866888888
        6666666688668888886666660000666666666666666666666666666666666666
        0000}
      NumGlyphs = 2
      ParentShowHint = False
      ShowHint = True
      Transparent = False
      OnClick = SpeedButton7Click
    end
    object btnStartStop: TSpeedButton
      Left = 159
      Top = 0
      Width = 23
      Height = 22
      Hint = 'Compilieren'
      Flat = True
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000666666666666666666666666666666666666000066666FFF666666666666
        666FFF666666666600006666844FFF666666666666888FFF6666666600006666
        84444FFF666666666688888FFF666666000066668444444FFF66666666888888
        8FFF666600006666844444444FFF666666888888888FFF660000666684444444
        444F66666688888888888F660000666684444444444666666688888888888666
        0000666684444444486666666688888888886666000066668444444866666666
        6688888888666666000066668444486666666666668888886666666600006666
        8448666666666666668888666666666600006666886666666666666666886666
        6666666600006666666666666666666666666666666666660000666666666666
        6666666666666666666666660000666666666666666666666666666666666666
        0000}
      NumGlyphs = 2
      Transparent = False
      OnClick = btnStartStopClick
    end
    object ToolButton3: TToolButton
      Left = 182
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 1
      Style = tbsSeparator
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 742
    Width = 748
    Height = 19
    Panels = <
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end
      item
        Width = 50
      end>
  end
  object MainMenu: TMainMenu
    Left = 228
    Top = 121
    object DAtei1: TMenuItem
      Caption = 'Datei'
      object Laden1: TMenuItem
        Caption = 'Laden'
        OnClick = Laden1Click
      end
      object Speichern1: TMenuItem
        Caption = 'Speichern'
        ShortCut = 16467
        OnClick = Speichern1Click
      end
      object Speichernunter1: TMenuItem
        Caption = 'Speichern unter'
        OnClick = Speichernunter1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Drucken1: TMenuItem
        Caption = 'Drucken'
        OnClick = Drucken1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Ende1: TMenuItem
        Caption = 'Ende'
        OnClick = Ende1Click
      end
    end
    object Bearbeiten1: TMenuItem
      Caption = 'Bearbeiten'
      object Rckgngig1: TMenuItem
        Caption = 'R'#252'ckg'#228'ngig'
        OnClick = Rckgngig1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Ausschneiden1: TMenuItem
        Caption = 'Ausschneiden'
        OnClick = Ausschneiden1Click
      end
      object Kopieren1: TMenuItem
        Caption = 'Kopieren'
        OnClick = Kopieren1Click
      end
      object Einfgen1: TMenuItem
        Caption = 'Einf'#252'gen'
        OnClick = Einfgen1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Alleskopieren1: TMenuItem
        Caption = 'Alles kopieren'
        OnClick = Alleskopieren1Click
      end
    end
    object Compilieren1: TMenuItem
      Caption = 'Kompilieren'
      OnClick = Compilieren1Click
    end
    object Hilfe1: TMenuItem
      Caption = 'Hilfe'
      object ber1: TMenuItem
        Caption = #220'ber'
        OnClick = ber1Click
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Einleitung1: TMenuItem
        Caption = 'Einleitung'
        OnClick = Einleitung1Click
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.OOP'
    Filter = 'CubeOOP-Programfiles|*.OOP'
    Left = 300
    Top = 130
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.OOP'
    Filter = 'CubeOOP-Programfiles|*.OOP'
    Left = 396
    Top = 122
  end
end
