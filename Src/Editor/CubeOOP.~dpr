program CubeOOP;

{%ToDo 'CubeOOP.todo'}

uses
  Forms,
  uFRMMain_Editor in 'uFRMMain_Editor.pas' {frmMain},
  uFRMInfoBox in 'uFRMInfoBox.pas' {FRMInfoBox},
  uFRMAbout in 'uFRMAbout.pas' {FRMAbout},
  uCubeOOP_Parser in '..\Compiler\uCubeOOP_Parser.pas',
  uCubeOOP_Compiler in '..\Compiler\uCubeOOP_Compiler.pas',
  uCubeOOP_Globals in '..\Compiler\uCubeOOP_Globals.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TFRMInfoBox, FRMInfoBox);
  Application.CreateForm(TFRMAbout, FRMAbout);
  Application.Run;
end.
