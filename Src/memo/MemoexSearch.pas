unit MemoexSearch;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls;

type
  TFRMSearch = class(TForm)
    Panel1: TPanel;
    rgStartSearch: TRadioGroup;
    Panel2: TPanel;
    Label1: TLabel;
    edSearch: TEdit;
    Panel3: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Label1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FRMSearch: TFRMSearch;

implementation

{$R *.DFM}

procedure TFRMSearch.FormActivate(Sender: TObject);
begin
  edsearch.SetFocus;
end;

procedure TFRMSearch.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if key=13 then  ModalResult := mrOK;
 if key=27 then  ModalResult := mrCancel;
end;

procedure TFRMSearch.Label1Click(Sender: TObject);
begin
   edSearch.SetFocus;
end;

procedure TFRMSearch.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TFRMSearch.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
