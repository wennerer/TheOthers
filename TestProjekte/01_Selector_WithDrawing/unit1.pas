unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, LResources,
  StdCtrls, Selector,LclIntf, Types;


type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    RadioGroup3: TRadioGroup;
    SaveAs: TButton;
    Panel1: TPanel;
    Start: TButton;
    Image1    : TImage;
    Selector1: TSelector;
    procedure FormCreate(Sender: TObject);
    procedure Image1Paint(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure Selector1Change(aRect: TRect);
    procedure StartClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var str : TStringList;
begin
 Image1.Picture.LoadFromLazarusResource('Bild');
 Selector1.TransparentColor:=rgb(192,220,192);
 str      := TStringList.Create;
 str.Add('Kurzanleitung:');
 str.Add('');
 str.Add('Zum Starten den Button Start Selection drücken');
 str.Add('Nun irgendwo hinklicken und einen Rahmen ziehen');
 str.Add('In den Rahmen klicken und Ausschnitt bewegen');
 str.Add('An den Ecken kann die Größe des Ausschnittes verändert werden');
 str.Add('Rechte Maustaste = Fertigstellen');
 str.Add('ESC = Abbrechen');
 str.Add('Return = Fertigstellen');
 str.Add('Pfeiltasten bewegen');
 str.Add('Strg+ mit der Maustaste bewegen = Hintergrund löschen nicht aktiv');
 str.Add('Beim Start ist Hintergrund ausschneiden gesetzt');
 str.Add('und es ist keine Transparenz gesetzt');
 str.Add('Als Transparentefarbe ist der "Horizont" gesetzt');
 str.Add('');
 str.Add('Um das vorbelegte PopupMenü zu benutzen den Radiobutton with Precast Popup anwählen');
 str.Add('Einen Rahmen ziehen und rechte Maustaste klicken');
 str.Add('Jetz erscheint das Popup, einfach mal testen!');
 Memo1.Lines.Assign(str);
 str.Free;
end;

procedure TForm1.Image1Paint(Sender: TObject);
begin
 Selector1.Drawing(Image1.Canvas);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
   0 : Selector1.CuttingOut:= true;
   1 : Selector1.CuttingOut:= false;
  end;
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
 if Selector1.Transparent = true then Selector1.Transparent:= false else
    Selector1.Transparent := true;
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);
begin
 if Selector1.PrecastPopup = pmHide then Selector1.PrecastPopup:= pmShow else
    Selector1.PrecastPopup := pmHide;
end;

procedure TForm1.SaveAsClick(Sender: TObject);
var SaveDialog1 : TSaveDialog;
    TmpPng      : TPortableNetworkGraphic;
    R           : TRect;
begin
 try
  R := Rect(0,0,Image1.Width,Image1.Height);
  TmpPng      := TPortableNetworkGraphic.Create;
  TmpPng.SetSize(Image1.Width,Image1.Height);
  TmpPng.Canvas.CopyRect(R,Image1.Canvas,R);
  SaveDialog1 := TSaveDialog.Create(self);
  if SaveDialog1.Execute then
  TmpPng.SaveToFile(SaveDialog1.FileName);
 finally
  SaveDialog1.Free;
  TmpPng.Free;
 end;
end;

procedure TForm1.Selector1Change(aRect: TRect);
begin
 with Selector1 do
  begin
   self.caption:= 'My Selector Test'+
                  '  Left: '+SelAreaLeftToString+
                  '  Top: '+SelAreaTopToString+
                  '  Width: '+SelAreaWidthToString+
                  '  Height: '+SelAreaHeightToString;
  end ;
end;

procedure TForm1.StartClick(Sender: TObject);
begin
  Selector1.Enable:=true;
end;

initialization
{$I MeinBild.lrs}
end.

