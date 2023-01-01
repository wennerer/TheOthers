unit TextBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLProc;

type

  { TTextBox }

  TTextBox = class(TGraphicControl)
  private
    FCapLeft: integer;
    FCaption: TCaption;
    CaptionChange     : boolean;
    FCaptionWordbreak: boolean;
    FCapTop: integer;
    FTextStyle: TTextStyle;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetCapLeft(AValue: integer);
    procedure SetCaption(AValue: TCaption);
    procedure SetCaptionWordbreak(AValue: boolean);
    procedure SetCapTop(AValue: integer);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetTextStyle(AValue: TTextStyle);

  protected

  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Loaded; override;
   procedure Paint;override;

   property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
  published
   //The text that the user writes in the button
   //Der Text den der Benutzer in den Button schreibt
   property Caption : TCaption read FCaption write SetCaption;
    //Alignment of the text in the caption (left, center, right)
   //Ausrichtung des Textes in der Caption (Links,Mitte,Rechts)
   property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetAlignment default taCenter;
   //Alignment of the text in the caption (top, center, bottom)
   //Ausrichtung des Textes in der Caption (Oben,Mitte,Unten)
   property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
   //Allows a line break in the caption
   //Ermöglicht einen Zeilenumbruch in der Caption
   property CaptionWordbreak : boolean read FCaptionWordbreak write SetCaptionWordbreak default true;
   //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
   //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
   property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 0;
   //The vertical distance of the text in the text rectangle (only effective with tlTop)
   //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
   property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;

   property Font;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I textbox_icon.lrs}
  RegisterComponents('Others',[TTextBox]);
end;

{ TTextBox }

constructor TTextBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width  := 70;
  Height := 20;

  //fFont := TFont.Create;
  //ffont.OnChange:= @FontPropertyChanged;

  FTextStyle.Alignment := taCenter;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= false;
  FTextStyle.Wordbreak := true;
  FTextStyle.Clipping  := true;
  FCapLeft        := 0;
  FCapTop         := 0;
end;

destructor TTextBox.Destroy;
begin
  inherited Destroy;
end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Setter---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
procedure TTextBox.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
end;

procedure TTextBox.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
end;


procedure TTextBox.SetCaption(AValue: TCaption);
begin
  if aValue = '' then aValue:=' ';
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  CaptionChange:=true;
end;

procedure TTextBox.SetCaptionWordbreak(AValue: boolean);
begin
  if FCaptionWordbreak=AValue then Exit;
  FCaptionWordbreak:=AValue;

   if not  FCaptionWordbreak then
    begin
     FTextStyle.SingleLine:= true;
     FTextStyle.Wordbreak := false;
    end else
    begin
     FTextStyle.SingleLine:= false;
     FTextStyle.Wordbreak := true;
    end;
  Invalidate;
end;

procedure TTextBox.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
end;

procedure TTextBox.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
end;

procedure TTextBox.SetTextStyle(AValue: TTextStyle);
begin
 FTextStyle:=AValue;
end;
//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Setter Ende---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

procedure TTextBox.Loaded;
begin
  inherited Loaded;

end;

procedure TTextBox.Paint;
var TeReC : TRect;
begin
  inherited Paint;
 if not CaptionChange then FCaption := self.Name;

 TeRec:= rect(0,0,width,height);

 canvas.TextRect(TeRec,TeRec.Left+FCapLeft,TeRec.Top+FCapTop,
                 FCaption,FTextStyle);

end;


end.
