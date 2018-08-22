unit uFRMAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFRMAbout = class(TForm)
    btnClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure btnCloseClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FRMAbout: TFRMAbout;

implementation

{$R *.dfm}

procedure TFRMAbout.btnCloseClick(Sender: TObject);
begin
  Close
end;

end.
