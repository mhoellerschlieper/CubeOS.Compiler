unit uFRMInfoBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TFRMInfoBox = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnClose: TButton;
    lvDescriptor: TListView;
    procedure btnCloseClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FRMInfoBox: TFRMInfoBox;

implementation

{$R *.dfm}

procedure TFRMInfoBox.btnCloseClick(Sender: TObject);
begin
  close;
end;

end.
