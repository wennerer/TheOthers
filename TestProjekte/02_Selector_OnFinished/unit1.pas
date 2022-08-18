unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,StdCtrls,
  LclIntf, LResources, LCLTranslator, Translations, Selector, Types;

type
  TPencil     = record
    A         : array of TPoint;
    num       : Integer;
    Ident     : string;
  end;

 TPaintingList   = record
    Identifier : string;
    Pencil     : TPencil;
    aSelection : TSelection;
end;
TPaintList = array of TPaintingList;

type

  { TForm1 }

  TForm1 = class(TForm)
    Frame    : TFrame;
    PaintBox : TPaintBox;
    Button1  : TButton;
    Button2  : TButton;
    Memo1    : TMemo;
    Selector1: TSelector;
    BildPNG  :TPortableNetworkGraphic;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Selector1Change(aRect: TRect);
    procedure Selector1Finished(aSelection: TSelection);

  private
    OutPut  : TPaintList;
    idx     : integer;
    PenAktiv: boolean;
    StartPen: boolean;
    i       : integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var str : TStringlist;
  lang,fn : string;
begin
 lang:= 'de';
  SetDefaultLang(lang);

  fn := Application.Location + 'locale/selectorstrconsts.%s.po';
  Translations.TranslateUnitResourceStrings('selectorstrconsts', Format(fn, [lang]));
  //Aktualisiert die PopUp-Menüeinträge
  Selector1.UpdateLocale;

 self.Caption:='Test the Selector';
 self.SetBounds(30,30,1200,700);

 Frame        := TFrame.Create(self);
 Frame.Parent := self;
 Frame.Color  := clWhite;
 Frame.SetBounds(0,100,800,600);

 PaintBox        := TPaintBox.Create(self);
 PaintBox.Parent := Frame;
 PaintBox.SetBounds(0,0,800,600);
 PaintBox.OnPaint:= @PaintBoxPaint;
 PaintBox.OnMouseDown := @PaintBoxMouseDown;
 PaintBox.OnMouseMove := @PaintBoxMouseMove;
 PaintBox.OnMouseUp   := @PaintBoxMouseUp;

 Button1         := TButton.Create(self);
 Button1.Parent  := self;
 Button1.Caption := 'Drawing';
 Button1.SetBounds(20,20,150,25);
 Button1.OnClick := @Button1Click;

 Button2         := TButton.Create(self);
 Button2.Parent  := self;
 Button2.Caption := 'Selected';
 Button2.SetBounds(190,20,150,25);
 Button2.OnClick := @Button2Click;

 Memo1           := TMemo.Create(self);
 Memo1.Parent    := self;
 Memo1.SetBounds(800,100,400,600);

 Selector1.Parent:= Frame;
 setLength(OutPut,idx+1);

 BildPNG    := TPortableNetworkGraphic.Create;
 BildPng.LoadFromLazarusResource('Bild');
 Selector1.TransparentColor:=rgb(192,220,192);
 str      := TStringList.Create;
 str.Add('Kurzanleitung:');
 str.Add('');
 str.Add('Mit "Drawing" kann man eine Freihandlinie zeichnen');
 str.Add('"Selected" aktiviert den Auswahlrahmen');
 str.Add('Bedienung wie im Beispiel 1');
 str.Add('Hintergrund löschen ist aus');
 str.Add('Transparent ist auf true');
 str.Add('So in etwa könnte der Einsatz der Komponente aussehen');

 Memo1.Lines.Assign(str);
 str.Free;
end;

procedure TForm1.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if PenAktiv then
  begin
   StartPen :=true;
   i:=0;
   setLength(OutPut[idx].Pencil.A,1);
   OutPut[idx].Pencil.A[i].x:=x;
   OutPut[idx].Pencil.A[i].y:=y;
   PaintBox.Invalidate;
  end;
end;

procedure TForm1.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
 if StartPen then
  begin
   inc(i);
   setLength(OutPut[idx].Pencil.A,i+1);
   OutPut[idx].Pencil.A[i].x:=x;
   OutPut[idx].Pencil.A[i].y:=y;
   PaintBox.Invalidate;
  end;
end;

procedure TForm1.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if StartPen then
  begin
   inc(i);
   setLength(OutPut[idx].Pencil.A,i+1);
   OutPut[idx].Pencil.A[i].x:=x;
   OutPut[idx].Pencil.A[i].y:=y;
   StartPen:=false;
   PenAktiv:=false;
   PaintBox.Invalidate;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var lv : integer;
begin
 BildPNG.Free;
 for lv:=0 to high(output) do
  begin
   if OutPut[lv].aSelection.RtrnBmp <> nil then
   OutPut[lv].aSelection.RtrnBmp.Free;
  end;
end;

procedure TForm1.PaintBoxPaint(Sender: TObject);
var lv : integer;
begin
 with PaintBox do
  begin
   canvas.Draw(0,0,BildPng);

   for lv:=0 to idx do
    begin

     if OutPut[lv].Identifier = 'Pencil' then
      begin
       canvas.Pen.Color:=clred;
       canvas.Pen.Width:=3;
       canvas.Polyline(OutPut[lv].Pencil.A);
      end;//if Pencil

     if OutPut[lv].Identifier = 'Selection' then
      begin
       canvas.Draw(OutPut[lv].aSelection.DrawArea.Left,OutPut[lv].aSelection.DrawArea.Top,OutPut[lv].aSelection.RtrnBmp);
       if OutPut[lv].aSelection.Cutting then
       begin
        canvas.Brush.Color := OutPut[lv].aSelection.BkgrdColor;
        canvas.Pen.Color   := OutPut[lv].aSelection.BkgrdColor;
        canvas.Rectangle(OutPut[lv].aSelection.BkgrdArea);
       end;//if cutting
     end;//if Selection

    end;//for
  end;//with PaintBox

end;

procedure TForm1.Button1Click(Sender: TObject); //zeichnen
begin
 inc(idx);
 setLength(OutPut,idx+1);
 Output[idx].Identifier:='Pencil';
 PenAktiv:=true;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Selector1.Enable:=true;
end;

procedure TForm1.Selector1Change(aRect: TRect);
begin
 self.Caption:='Test the Selector  '+Selector1.SelAreaToString;
end;

procedure TForm1.Selector1Finished(aSelection: TSelection);
begin
 inc(idx);
 setLength(OutPut,idx+1);
 OutPut[idx].Identifier            := aSelection.Ident;
 OutPut[idx].aSelection.RtrnBmp    := TBitmap.Create;
 OutPut[idx].aSelection.RtrnBmp.Assign(aSelection.RtrnBmp);
 OutPut[idx].aSelection.DrawArea   := aSelection.DrawArea;
 OutPut[idx].aSelection.Cutting    := aSelection.Cutting;
 OutPut[idx].aSelection.BkgrdArea  := aSelection.BkgrdArea;
 OutPut[idx].aSelection.BkgrdColor := aSelection.BkgrdColor;

 PaintBox.Invalidate;
end;


initialization
{$I MeinBild.lrs}
end.

