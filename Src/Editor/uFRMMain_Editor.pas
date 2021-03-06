{*******************************************************************************
PROJECT : CubeOOP - Development Environment
AUTHOR  : Marcus H�ller-Schlieper
DATE    : 14.07.2007

COPYRIGHT: Marcus H�ller-Schlieper 1995-1996 / 2007

DESCRIPTION:

HISTORY:
*******************************************************************************}
Unit uFRMMain_Editor;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Menus,
  MemoEx,
  ComCtrls,
  ExtCtrls,
  Buttons,
  ToolWin,
  StdCtrls,
  Inifiles,
  ShellAPI,
  uCubeOOP_Compiler,
  uCubeOOP_Globals;

Type
  TfrmMain = Class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    MainMenu: TMainMenu;
    DAtei1: TMenuItem;
    Laden1: TMenuItem;
    Speichern1: TMenuItem;
    Speichernunter1: TMenuItem;
    N1: TMenuItem;
    Drucken1: TMenuItem;
    N2: TMenuItem;
    Ende1: TMenuItem;
    Bearbeiten1: TMenuItem;
    Rckgngig1: TMenuItem;
    N3: TMenuItem;
    Ausschneiden1: TMenuItem;
    Kopieren1: TMenuItem;
    Einfgen1: TMenuItem;
    Alleskopieren1: TMenuItem;
    Compilieren1: TMenuItem;
    Hilfe1: TMenuItem;
    ToolBar1: TToolBar;
    SpeedButton1: TSpeedButton;
    btnOpen: TSpeedButton;
    btnSave: TSpeedButton;
    ToolButton1: TToolButton;
    btnPrint: TSpeedButton;
    ToolButton2: TToolButton;
    btnShowTrend: TSpeedButton;
    SpeedButton7: TSpeedButton;
    btnStartStop: TSpeedButton;
    ToolButton3: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    memEditor: TMemoEx;
    StatusBar: TStatusBar;
    memErrors: TMemo;
    ber1: TMenuItem;
    Einleitung1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Procedure btnOpenClick(Sender: TObject);
    Procedure btnSaveClick(Sender: TObject);
    Procedure memEditorChangeStatus(Sender: TObject);
    Procedure btnStartStopClick(Sender: TObject);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure Speichern1Click(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure btnShowTrendClick(Sender: TObject);
    Procedure Einleitung1Click(Sender: TObject);
    Procedure ber1Click(Sender: TObject);
    Procedure Compilieren1Click(Sender: TObject);
    Procedure Laden1Click(Sender: TObject);
    Procedure Speichernunter1Click(Sender: TObject);
    Procedure Ende1Click(Sender: TObject);
    Procedure Rckgngig1Click(Sender: TObject);
    Procedure Ausschneiden1Click(Sender: TObject);
    Procedure Kopieren1Click(Sender: TObject);
    Procedure Einfgen1Click(Sender: TObject);
    Procedure Alleskopieren1Click(Sender: TObject);
    Procedure btnPrintClick(Sender: TObject);
    Procedure Drucken1Click(Sender: TObject);
    Procedure SpeedButton7Click(Sender: TObject);
  private
    { Private-Deklarationen }

  public
    { Public-Deklarationen }
    sFilename: String;
    iErrorLine: Integer;
    sErrorText: String;

    InfoStruct: TInfoStruct;

    Procedure StartCompiler;
    Procedure OpenFile(sFilename: String);
    Function Call_Assembler(sASMName: String): Boolean;
  End;

Var
  frmMain           : TfrmMain;
  r1, r2            : real;

Implementation

Uses uFRMInfoBox,
  uFRMAbout;

{$R *.dfm}

{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.btnOpenClick(Sender: TObject);
Begin
  If OpenDialog.execute Then
  Begin
    sFilename := Opendialog.Filename;
    OpenFile(sFilename);
  End;
End;

{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.btnSaveClick(Sender: TObject);
Begin
  SaveDialog.Filename := sFilename;
  If SaveDialog.execute Then
  Begin
    sFilename := SaveDialog.Filename;
    StatusBar.Panels[3].Text := sFilename;
    Tabsheet1.Caption := ExtractFilename(sFilename);
    memEditor.Lines.SaveToFile(SaveDialog.Filename);
  End;
End;

{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.memEditorChangeStatus(Sender: TObject);
Var
  s                 : String;
Begin

  //  perform interface updates depending on status changes
  StatusBar.Panels[0].Text :=
    Format('%d: %d',
    [memEditor.CaretX + 1,
    memEditor.CaretY + 1]);
  If memEditor.Recording Then
    s := 'macro recording'
  Else If memEditor.InsertMode Then
    s := 'insert'
  Else
    s := 'overwrite';

  StatusBar.Panels[1].Text := s;
  If memEditor.CanUndo Then
    s := 'can undo'
  Else
    s := 'no operations to undo';

  StatusBar.Panels[2].Text := s;

End;

{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.btnStartStopClick(Sender: TObject);
Begin
  StartCompiler;
End;

{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Function TfrmMain.Call_Assembler(sASMName: String): Boolean;
Var
  sExeName          : String;
  sPath             : String;
Begin
  sPath := ExtractFilePath(Application.ExeName) + '..\ASM';
  sExeName := ExtractFilePath(Application.ExeName) + '..\ASM\ass.bat ' +
    sASMName + ' ' + sPath;
  // Assembler ausf�hren
  winexec(Pchar(sExeName),
    SW_SHOWNORMAL);
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = vk_F9 Then
  Begin
    btnStartStop.Click;
  End;
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.StartCompiler;
Var
  sASMName          : String;
  iInfoIDX          : INteger;
  Item              : TListItem;
Begin
  btnStartStop.Enabled := False;

  memErrors.Visible := False;
  InfoStruct.sErrorText := '';

  Try
    If sFilename <> '' Then
    Begin
      StatusBar.Panels[3].Text := sFilename;
      memEditor.Lines.SaveToFile(sFilename);
      Tabsheet1.Caption := ExtractFilename(sFilename);

      FRMInfobox.lvDescriptor.Clear;
      FRMInfobox.Show;

      sASMName :=
        ExtractFilePath(Application.ExeName) + '..\ASM\' +
        ExtractFilename(ChangeFileExt(sFilename, '.asm'));

      If Call_Compiler(
        sFilename,
        sASMName,
        InfoStruct) Then
      Begin

        Call_Assembler(
          ExtractFilename(ChangeFileExt(sASMName, '')));
      End
      Else
      Begin
        memEditor.CaretY := InfoStruct.iErrorLine + 10;
        memEditor.CaretX := 0;

        memEditor.CaretY := InfoStruct.iErrorLine - 2;
        memEditor.Repaint;

        memErrors.Visible := True;
        memErrors.Clear;
        memErrors.Lines.Text := InfoStruct.sErrorText;

      End;
    End;
  Finally
    btnStartStop.Enabled := True;

    // Ausgabe aller Descriptoren
    If InfoStruct.VarList <> Nil Then
    Begin
      For iInfoIDX := 0 To InfoStruct.VarList.Count - 1 Do
      Begin
        With TcVarEnter(InfoStruct.VarList[iInfoIDX]) Do
        Begin
          Item := FRMInfobox.lvDescriptor.Items.Add;
          Item.Caption := sClass;
          Item.SubItems.Add(sName);
          Item.SubItems.Add(sProcedure);
          Item.SubItems.Add(sClassType);
          Item.SubItems.Add(cTypeText[tpType]);
          Item.SubItems.Add(cVisiblyText[tpVisibility]);
          Item.SubItems.Add(cKindText[tkKind]);
          Item.SubItems.Add(InttoStr(iByteLen));
          Item.SubItems.Add(InttoStr(iOffset));
          Item.SubItems.Add(InttoStr(iStackSize));
          Item.SubItems.Add(InttoStr(iLocalStack));
          Item.SubItems.Add(cTypeText[ResultType]);
          Item.SubItems.Add(sConstantValue);
        End;
      End;
    End;

    FRMInfobox.Show;
    FRMInfobox.btnClose.SetFocus;

  End;
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.Speichern1Click(Sender: TObject);
Begin
  If sFilename <> '' Then
    memEditor.Lines.SaveToFile(sFilename)
  Else
    btnOpen.Click
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.SpeedButton1Click(Sender: TObject);
Begin
  memEditor.Lines.Clear;

  memEditor.Lines.add('//*************************************************************');
  memEditor.Lines.add('//Environment: CubeOOP');
  memEditor.Lines.add('//Title:');
  memEditor.Lines.add('//Autor: ');
  memEditor.Lines.add('//Date: ' + DateTimeToStr(NOW));
  memEditor.Lines.add('//Description:');
  memEditor.Lines.add('//*************************************************************}');
  memEditor.Lines.add('Main;');
  memEditor.Lines.add('Begin ');
  memEditor.Lines.add('End; // of Maintask');

  sFilename := '';
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Var
  ar                : Array[1..10] Of Integer;

Procedure Test;
Var
  a                 : Single;
  b                 : Single;
  i                 : Integer;
Begin
  ar[10] := 100;
End;

Procedure TfrmMain.FormCreate(Sender: TObject);
Var
  ini               : TInifile;
  sIniFile          : String;

  s                 : String;
Begin
  Test;
  r1 := 10.0;
  R2 := 99.11;
  r2 := R1 + R2;
  sIniFile := ChangeFileExt(Application.ExeName, '.ini');
  If Fileexists(sIniFile) Then
  Begin
    ini := TInifile.Create(sIniFile);
    sFilename := ini.ReadString('LastSettings', 'Filename', '');
    Try
      OpenFile(sFilename);
    Except
    End;

    memEditor.CaretX := ini.ReadInteger('LastSettings', 'X', 0);
    memEditor.CaretY := ini.ReadInteger('LastSettings', 'Y', 0);

    ini.Free;
  End;
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.OpenFile(sFilename: String);
Begin

  StatusBar.Panels[3].Text := sFilename;

  Tabsheet1.Caption := ExtractFilename(sFilename);

  memEditor.PaintLineNumbers := True;
  memEditor.Lines.LoadFromFile(sFilename);

  memEditor.CaretX := 0;
  memEditor.CaretY := 0;

End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.FormDestroy(Sender: TObject);
Var
  ini               : TInifile;
Begin
  ini := TInifile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  ini.WriteString('LastSettings', 'Filename', sFilename);
  ini.WriteInteger('LastSettings', 'X', memEditor.CaretX);
  ini.WriteInteger('LastSettings', 'Y', memEditor.CaretY);
  ini.Free;
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.FormShow(Sender: TObject);
Begin
  memEditor.SetFocus;
End;
{*******************************************************************************
Procedure:
Description:
Author:
Date:
History
*******************************************************************************}

Procedure TfrmMain.btnShowTrendClick(Sender: TObject);
Begin
  FRMInfoBox.Show;
End;

Procedure TfrmMain.Einleitung1Click(Sender: TObject);
Var
  sDocFilename      : String;
Begin
  sDocFilename := ExtractFilePath(Application.ExeName) + '..\Doc\CubeOOP.pdf';
  ShellExecute(Handle, 'open', Pchar(sDocFilename),
    '', '', SW_SHOWNORMAL);
End;

Procedure TfrmMain.ber1Click(Sender: TObject);
Begin
  FRMAbout.Show;
End;

Procedure TfrmMain.Compilieren1Click(Sender: TObject);
Begin
  btnStartStop.Click;
End;

Procedure TfrmMain.Laden1Click(Sender: TObject);
Begin
  btnOpen.Click;
End;

Procedure TfrmMain.Speichernunter1Click(Sender: TObject);
Begin
  btnSave.Click;
End;

Procedure TfrmMain.Ende1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmMain.Rckgngig1Click(Sender: TObject);
Begin
  memEditor.Undo;
End;

Procedure TfrmMain.Ausschneiden1Click(Sender: TObject);
Begin
  memEditor.CutToClipboard;
End;

Procedure TfrmMain.Kopieren1Click(Sender: TObject);
Begin
  memEditor.CopyToClipboard
End;

Procedure TfrmMain.Einfgen1Click(Sender: TObject);
Begin
  memEditor.ClipBoardPaste
End;

Procedure TfrmMain.Alleskopieren1Click(Sender: TObject);
Begin
  memEditor.SelectAll
End;

Procedure TfrmMain.btnPrintClick(Sender: TObject);
Begin
  Showmessage('nicht implementierte Funktion');
End;

Procedure TfrmMain.Drucken1Click(Sender: TObject);
Begin
  btnPrint.Click;
End;

Procedure TfrmMain.SpeedButton7Click(Sender: TObject);
Var
  iVtableIDX        : Integer;
Begin

End;

End.

