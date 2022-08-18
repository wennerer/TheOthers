unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Sizer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Sizer1: TSizer;
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure Sizer1Finished(aRect: Trect);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Sizer1.ShowSizer:= true;
end;

procedure TForm1.FormPaint(Sender: TObject);
var lv : integer;
begin
 for Lv:= 0 to 4 do
 begin
  canvas.Brush.Color:= clLime;
  canvas.Rectangle(0,0+(lv*100),700,20+(lv*100));
  canvas.Brush.Color:= clYellow;
  canvas.Rectangle(0,20+(lv*100),700,40+(lv*100));
  canvas.Brush.Color:= clRed;
  canvas.Rectangle(0,40+(lv*100),700,60+(lv*100));
  canvas.Brush.Color:= clBlue;
  canvas.Rectangle(0,60+(lv*100),700,80+(lv*100));
  canvas.Brush.Color:= clAqua;
  canvas.Rectangle(0,80+(lv*100),700,100+(lv*100));
 end;
end;

procedure TForm1.Sizer1Finished(aRect: Trect);
begin
 caption:= inttostr(aRect.Left)+'/'+inttostr(aRect.Top)+'/'+inttostr(aRect.Right)+'/'+inttostr(aRect.Bottom);
end;

end.

