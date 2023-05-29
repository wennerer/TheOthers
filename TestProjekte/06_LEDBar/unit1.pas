unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  MultiPanel, MultiButton, MultiButtonStyleManager, MultiplexSlider, LEDBar,
  DrawCircle;

type

  { TForm1 }

  TForm1 = class(TForm)
    LEDBar1: TLEDBar;
    MultiButton1: TMultiButton;
    MultiButton_Linear: TMultiButton;
    MultiButton_20: TMultiButton;
    MultiButton_50: TMultiButton;
    MultiButton_70: TMultiButton;
    MultiButton_Mono: TMultiButton;
    MultiButton_Stereo: TMultiButton;
    MultiButton_MPX: TMultiButton;
    MultiButtonStyleManager1: TMultiButtonStyleManager;
    MultiPanel_Settings: TMultiPanel;
    MultiPanel_Pre: TMultiPanel;
    MultiPanel_Audio: TMultiPanel;
    MultiplexSlider1: TMultiplexSlider;
    procedure MultiButton1Click(Sender: TObject);
    procedure MultiButton1MessageButtonClick(Sender: TObject);
    procedure MultiplexSlider1Change3x(const Val1, Val2, Val3: integer);

  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }




procedure TForm1.MultiplexSlider1Change3x(const Val1, Val2, Val3: integer);
begin
 LEDBar1.Value:=Val1;
 LEDBar1.BlinkFreq:= Val2*10;
end;

procedure TForm1.MultiButton1Click(Sender: TObject);
begin
 if LEDBar1.Blinking then LEDBar1.Blinking := false else
  LEDBar1.Blinking:=true;
end;

procedure TForm1.MultiButton1MessageButtonClick(Sender: TObject);
begin
 if LEDBar1.Font.Color = clRed then LEDBar1.Font.Color := clLime else
   LEDBar1.Font.Color := clRed;

 //LEDBar1.Invalidate;
end;

end.

