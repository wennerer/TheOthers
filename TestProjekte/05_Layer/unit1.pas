unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  MultiPanel, MultiButton, MultiButtonStyleManager, Layer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Layer1: TLayer;
    Layer2: TLayer;
    Layer3: TLayer;
    Layer4: TLayer;
    Layer5: TLayer;
    Layer6: TLayer;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    procedure ButtonsClick(Sender: TObject);

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonsClick(Sender: TObject);
begin
 if (Sender as TButton).Name = 'Button1' then Layer4.Visible:=true;
 if (Sender as TButton).Name = 'Button4' then Layer1.Visible:=true;
 if (Sender as TButton).Name = 'Button2' then Layer5.Visible:=true;
 if (Sender as TButton).Name = 'Button5' then Layer2.Visible:=true;
 if (Sender as TButton).Name = 'Button3' then Layer3.Visible:=true;
 if (Sender as TButton).Name = 'Button6' then Layer6.Visible:=true;
end;


end.

