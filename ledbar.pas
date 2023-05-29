{ <A LED Processbar with blinking textlabel>
  <Version 0.0.0.1 TEST>
  Copyright (C) <29.05.2023> <Bernd Hübner>

  This library is free software; you can redistribute it and/or modify it under the
  terms of the GNU Library General Public License as published by the Free Software
  Foundation; either version 2 of the License, or (at your option) any later
  version with the following modification:

  As a special exception, the copyright holders of this library give you permission
  to link this library with independent modules to produce an executable,
  regardless of the license terms of these independent modules,and to copy and
  distribute the resulting executable under terms of your choice, provided that you
  also meet, for each linked independent module, the terms and conditions of the
  license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this
  exception to your version of the library, but you are not obligated to do so. If
  you do not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
  PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along
  with this library; if not, write to the Free Software Foundation, Inc., 51
  Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit LEDBar;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf, ExtCtrls;



type
  TLEDBar      = class; //TCustomControl , die eigentliche Komponente
  TRectangle   = class; //TCollectionItem, das einzelne Rechteck


//xxxxxxxxxxxxxxxxxxxxxxxxxx---Die Kollektion der Rectecke---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type

  { TRectCollection }

  TRectCollection = class(TCollection)
  private
   FDrawRect : TLEDBar;
   function GetRect(Index: Integer): TRectangle;
   function GetEnabled: Boolean;
   function GetVisibleCount: Integer;
   procedure SetRect(Index: Integer; aRect: TRectangle);
  protected
   function GetOwner: TPersistent; override;
  public
   constructor Create(aCollection: TLEDBar; aItemClass: TCollectionItemClass);
   property Items[Index: Integer]: TRectangle read GetRect write SetRect; default;
   property VisibleCount: Integer read GetVisibleCount;
   property Enabled: Boolean read GetEnabled;
  published

  end;

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Das einzelne Rechteck---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type

  { TRectangle }

  TRectangle = class(TCollectionItem)
   private
    FActiveColor: TColor;
    FAlertColor: TColor;
    FBorderColor: TColor;
    FBorderWidth: integer;
    FFaultColor: TColor;
    FRectangles     : TCollection;
    FColor: TColor;
    FEnabled: boolean;
    FVisible     : Boolean;
    FDisplayName : string;
    FLeft        : integer;
    FTop         : integer;

    procedure SetEnabled(AValue: boolean);
    procedure SetVisible(AValue: Boolean);
   protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    function GetOwner: TPersistent; override;
   public
    constructor Create(ACollection: TCollection); override;
   published
    property Color         : TColor read FColor write FColor default clSilver;
    property ActiveColor   : TColor read FActiveColor write FActiveColor default clLime;
    property AlertColor    : TColor read FAlertColor write FAlertColor default clYellow;
    property FaultColor    : TColor read FFaultColor write FFaultColor default clRed;
    property BorderColor   : TColor read FBorderColor write FBorderColor default clBlack;
    property BorderWidth   : integer read FBorderWidth write FBorderWidth default 1;
    property Enabled       : boolean read FEnabled write SetEnabled default true;
    property Visible       : Boolean read FVisible write SetVisible default true;
    property Left          : integer read FLeft write FLeft;
    property Top           : integer read FTop write FTop;

  end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Die eigentliche Komponente---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
type

  { TLEDBar }

  TLEDBar = class(TGraphicControl)
  private
    FBlinkFreq: integer;
    FBlinking: boolean;
   FBorderColor: TColor;
   FBorderWidth: integer;
   FCapLeft: integer;
   FCaption: TCaption;
   FCaptionHeight: integer;
   FCaptionWordbreak: boolean;
   FCapTop: integer;
   FFont: TFont;
   FIncrementHeight: integer;
   FRectCollection     : TRectCollection;
   FFaultBorder: integer;
   FTextStyle: TTextStyle;
   FValue: integer;
   FAlertBorder: integer;
   FStep : double;
   FCount: integer;
   FTimer : TTimer;
   ShowCaption : boolean;

   procedure SetAlignment(AValue: TAlignment);
   procedure SetBlinkFreq(AValue: integer);
   procedure SetBlinking(AValue: boolean);
   procedure SetBorderColor(AValue: TColor);
   procedure SetBorderWidth(AValue: integer);
   procedure SetCapLeft(AValue: integer);
   procedure SetCaption(AValue: TCaption);
   procedure SetCaptionHeight(AValue: integer);
   procedure SetCaptionWordbreak(AValue: boolean);
   procedure SetCapTop(AValue: integer);
   procedure SetFont(AValue: TFont);
   procedure SetIncrementHeight(AValue: integer);
   procedure SetFaultBorder(AValue: integer);
   procedure SetLayout(AValue: TTextLayout);
   procedure SetTextStyle(AValue: TTextStyle);
   procedure SetValue(AValue: integer);
   procedure SetAlertBorder(AValue: integer);
  protected
   function CreateRectangles: TRectCollection;
   function GetRect: TRectCollection;
   function IsRectStored: Boolean;
   procedure SetRect(ARectCollection: TRectCollection);
   procedure FontChanged({%H-}Sender:TObject);
   procedure BlinkCaption({%H-}Sender:TObject);
   procedure BlinkingStop({%H-}Sender:TObject);
  public
   property TextStyle: TTextStyle read FTextStyle write SetTextStyle;
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
  published
   property Rectangles : TRectCollection read GetRect write SetRect stored IsRectStored;
   property BorderColor   : TColor read FBorderColor write SetBorderColor default clGray;
   property BorderWidth   : integer read FBorderWidth write SetBorderWidth default 1;
   property AlertBorder    : integer read FAlertBorder write SetAlertBorder default 80;
   property FaultBorder   : integer read FFaultBorder write SetFaultBorder default 90;
   property Value       : integer read FValue write SetValue default 0;
   property IncrementHeight : integer read FIncrementHeight write SetIncrementHeight default 8;


   property Caption : TCaption read FCaption write SetCaption;
   property CaptionHeight : integer read FCaptionHeight write SetCaptionHeight default 25;
   property Font: TFont read FFont write SetFont;
   property CaptionAlignment:TAlignment read FTextStyle.Alignment write SetAlignment default taCenter;
   property CaptionLayout:TTextLayout read FTextStyle.Layout write SetLayout default tlCenter;
   property CaptionWordbreak : boolean read FCaptionWordbreak write SetCaptionWordbreak default true;
   property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 0;
   property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;

   property Blinking : boolean read FBlinking write SetBlinking default false;
   property BlinkFreq : integer read FBlinkFreq write SetBlinkFreq default 300;

   property BorderSpacing;
   property Constraints;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I ledbar_icon.lrs}
  RegisterComponents('Others',[TLEDBar]);
end;

{ TLEDBar }

constructor TLEDBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Left  :=  20;
  Top   :=  20;
  Width := 150;
  Height:= 150;
  FBorderWidth            := 1;
  FBorderColor            := clGray;
  FAlertBorder           := 80;
  FFaultBorder              := 90;
  FValue                  := 0;
  FIncrementHeight        := 8;

  FCaption := '';
  FCaptionWordbreak := true;
  FCaptionHeight := 25;
  FCapLeft := 0;
  FCapTop := 0;
  FTextStyle.Alignment := taCenter;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= false;
  FTextStyle.Wordbreak := true;
  FTextStyle.Clipping  := true;
  FFont := TFont.Create;
  FFont.OnChange:= @FontChanged;

  ShowCaption := true;
  FBlinking := false;
  FBlinkFreq := 300;
  FTimer  := TTimer.Create(self);
  FTimer.Interval:= FBlinkFreq;
  FTimer.Enabled:= FBlinking;
  FTimer.OnTimer:= @BlinkCaption;
  FTimer.OnStopTimer:= @BlinkingStop;

  FRectCollection := CreateRectangles;  //TCollection
  FRectCollection.Add;
end;

procedure TLEDBar.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
 Invalidate;
end;

procedure TLEDBar.SetBlinkFreq(AValue: integer);
begin
  if FBlinkFreq=AValue then Exit;
  FBlinkFreq:=AValue;
  FTimer.Interval:= aValue;
  Invalidate;
end;

procedure TLEDBar.SetBlinking(AValue: boolean);
begin
  if FBlinking=AValue then Exit;
  FBlinking:=AValue;
  if (csDesigning in ComponentState) then exit;
  FTimer.Enabled:=aValue;
  Invalidate;
end;

procedure TLEDBar.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetCaptionHeight(AValue: integer);
begin
  if FCaptionHeight=AValue then Exit;
  FCaptionHeight:=AValue;
  invalidate;
end;

procedure TLEDBar.SetCaptionWordbreak(AValue: boolean);
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
  invalidate;
end;

procedure TLEDBar.SetCapTop(AValue: integer);
begin
  if FCapTop=AValue then Exit;
  FCapTop:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetFont(AValue: TFont);
begin
 if FFont=AValue then Exit;
 fFont.Assign(aValue);
 Invalidate;
end;

procedure TLEDBar.SetIncrementHeight(AValue: integer);
begin
  if FIncrementHeight=AValue then Exit;
  FIncrementHeight:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetFaultBorder(AValue: integer);
begin
  if FFaultBorder=AValue then Exit;
  if aValue > 100 then aValue := 100;
  FFaultBorder:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetLayout(AValue: TTextLayout);
begin
  if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
 Invalidate;
end;

procedure TLEDBar.SetTextStyle(AValue: TTextStyle);
begin
 //if FTextStyle=AValue then Exit;
  FTextStyle:=AValue;
end;

procedure TLEDBar.SetValue(AValue: integer);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  FStep := (FIncrementHeight*100)/(pred(FRectCollection.Count)*FIncrementHeight);
  FCount := round(aValue/FStep);
  Invalidate;
end;

procedure TLEDBar.SetAlertBorder(AValue: integer);
begin
  if FAlertBorder=AValue then Exit;
  if aValue > 100 then aValue := 100;
  FAlertBorder:=AValue;
  Invalidate;
end;

function TLEDBar.CreateRectangles: TRectCollection;
begin
  result := TRectCollection.Create(Self, TRectangle);
end;

function TLEDBar.GetRect: TRectCollection;
begin
 result := FRectCollection;
end;

function TLEDBar.IsRectStored: Boolean;
begin
 result := Rectangles.Enabled;
end;

procedure TLEDBar.SetRect(ARectCollection: TRectCollection);
begin
  FRectCollection.Assign(ARectCollection);
end;

procedure TLEDBar.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TLEDBar.BlinkCaption(Sender: TObject);
begin
 if ShowCaption then ShowCaption:= false else ShowCaption := true;
 Invalidate;
end;

procedure TLEDBar.BlinkingStop(Sender: TObject);
begin
 ShowCaption := true;
end;


destructor TLEDBar.Destroy;
begin
  FFont.Free;
  FRectCollection.Free;
  inherited Destroy;
end;

procedure TLEDBar.Paint;
var lv : integer;
    textrect     : TRect;
begin
  inherited Paint;
  canvas.Brush.Style:= bsClear;
  canvas.Pen.Color:= FBorderColor;
  Canvas.Pen.Width:= FBorderWidth;
  canvas.Rectangle(0,0,width,height);

  for lv:= 0 to pred(FRectCollection.Count) do
   begin
    Canvas.Brush.Color := FRectCollection.Items[lv].FColor;
    Canvas.Pen.Color   := FRectCollection.Items[lv].FBorderColor;
    Canvas.Pen.Width   := FRectCollection.Items[lv].FBorderWidth;
    Canvas.Rectangle(0+FBorderWidth,Height-25-FIncrementHeight-(lv*FIncrementHeight),
                     width-FBorderWidth,Height-25-(lv*FIncrementHeight));
   end;

  for lv:= 0 to pred(FRectCollection.Count) do
   begin
    if (lv < FCount) and (lv < (FAlertBorder/FStep))
     then Canvas.Brush.Color := FRectCollection.Items[lv].FActiveColor
    else if (lv < FCount) and (lv >= (FAlertBorder/FStep)) and (lv < (FFaultBorder/FStep))
     then Canvas.Brush.Color := FRectCollection.Items[lv].FAlertColor
    else if (lv <= FCount) and (lv >= (FFaultBorder/FStep))
     then Canvas.Brush.Color := FRectCollection.Items[lv].FFaultColor
    else break;
    Canvas.Pen.Color   := FRectCollection.Items[lv].FBorderColor;
    Canvas.Pen.Width   := FRectCollection.Items[lv].FBorderWidth;
    Canvas.Rectangle(0+FBorderWidth,Height-FCaptionHeight-FIncrementHeight-(lv*FIncrementHeight),
                     width-FBorderWidth,Height-FCaptionHeight-(lv*FIncrementHeight));
   end;


  canvas.Font.Assign(FFont);
  textrect := rect(0+FBorderWidth,Height - FCaptionHeight,width-FBorderWidth,Height);
  //canvas.Brush.Color:=clWhite;
  //canvas.FillRect(textrect);
  if ShowCaption then
  Canvas.TextRect(TextRect,textrect.Left+FCapLeft,textrect.Top+FCapTop,FCaption,FTextStyle);
end;


{$Include rectcollection.inc}
{$Include rectitem.inc}
end.




























