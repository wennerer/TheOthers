unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Layer, MultiPanel, MultiButton, MultiplexSlider;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Layer1: TLayer;
    Layer2: TLayer;
    Panel1: TPanel;
    StaticText1: TStaticText;
    procedure MultiPanel1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MultiPanel1Click(Sender: TObject);
begin

end;

end.

