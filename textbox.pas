{ <This component can be used to output a text>

  Copyright (C) <Version 1.0.0.3 30.05.2023> <Bernd Hübner>

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

unit TextBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLProc, ExtCtrls;

type

  { TTextBox }

  TTextBox = class(TGraphicControl)
  private
    FAutoSize: boolean;
    FBgrdColor: TColor;
    FBlinkFreq: integer;
    FBlinking: boolean;
    FShowCaption : boolean;
    FCapLeft: integer;
    FCaption: TCaption;
    CaptionChange     : boolean;
    FCaptionWordbreak: boolean;
    FCapTop: integer;
    FTextStyle: TTextStyle;
    FTimer  : TTimer;

    procedure SetAlignment(AValue: TAlignment);
    procedure SetBgrdColor(AValue: TColor);
    procedure SetBlinkFreq(AValue: integer);
    procedure SetBlinking(AValue: boolean);
    procedure SetCapLeft(AValue: integer);
    procedure SetCaption(AValue: TCaption);
    procedure SetCaptionWordbreak(AValue: boolean);
    procedure SetCapTop(AValue: integer);
    procedure SetLayout(AValue: TTextLayout);
    procedure SetTextStyle(AValue: TTextStyle);

  protected
    procedure SetAutoSize(Value: Boolean);override;
    procedure StopBlinkTimer(Sender: TObject);
    procedure BlinkTimer(Sender: TObject);
    function  GetTextWidth (AText : String ; AFont : TFont ) : Integer ;
    function  GetTextHeight (AText : String ; AFont : TFont ) : Integer ;
    procedure TriggerAutoSize;
    procedure CalculatePreferredSize(var PreferredWidth,PreferredHeight: integer;WithThemeSpace: Boolean); override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
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
   property CaptionWordbreak : boolean read FCaptionWordbreak write SetCaptionWordbreak default false;
   //The horizontal distance of the text in the text rectangle (only effective with taLeftJustify)
   //Der horizontale Abstand des Textes im Textrechteck (nur wirksam mit taLeftJustify)
   property CaptionHorMargin : integer read FCapLeft write SetCapLeft default 0;
   //The vertical distance of the text in the text rectangle (only effective with tlTop)
   //Der vertikale Abstand des Textes im Textrechteck (nur wirksam mit tlTop)
   property CaptionVerMargin : integer read FCapTop write SetCapTop default 0;
   //Allows automatic adjustment of the size for the control, according to its content
   //Ermöglicht die automatische Anpassung der Größe der Kontrolle an ihren Inhalt
   property AutoSize : boolean read FAutoSize write SetAutoSize default false;
   //The font to be used for text display in this control
   //Die Schriftart, die für die Textanzeige in diesem Steuerelement verwendet werden soll
   property Font;
   //The color of the background (clNone = transparent)
   //Die Farbe des Hintergrundes (clNone = Transparent)
   property BgrdColor : TColor read FBgrdColor write SetBgrdColor default clNone;
   //Makes the text blink
   //Lässt den Text blinken
   property Blinking : boolean Read FBlinking write SetBlinking default false;
   //Blink frequency
   //Blink Frequenz
   property BlinkFreq : integer read FBlinkFreq write SetBlinkFreq default 300;

   property Anchors;
   property BorderSpacing;
   property Constraints;
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

  FBgrdColor := clNone;

  FTextStyle.Alignment := taCenter;
  FTextStyle.Layout    := tlCenter;
  FTextStyle.SingleLine:= true;
  FTextStyle.Wordbreak := false;
  FTextStyle.Clipping  := true;
  FCapLeft        := 0;
  FCapTop         := 0;

  FTimer := TTimer.Create(self);
  FTimer.Enabled:= false;
  FTimer.Interval:= 300;
  FTimer.OnTimer:= @BlinkTimer;
  FTimer.OnStopTimer:= @StopBlinkTimer;
  FShowCaption    := true;
  Blinking        := false;
  BlinkFreq       := 300;
end;

destructor TTextBox.Destroy;
begin
  inherited Destroy;
end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Setter---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


procedure TTextBox.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value = FAutoSize then exit;
  FAutoSize := Value;
  if FAutoSize then TriggerAutoSize;
  if FAutoSize = false then InvalidatePreferredSize;
end;


procedure TTextBox.SetAlignment(AValue: TAlignment);
begin
 if fTextStyle.Alignment=AValue then exit;
 fTextStyle.Alignment:=AValue;
 if aValue <> taLeftJustify then FCapLeft:=0;
 Invalidate;
end;

procedure TTextBox.SetBgrdColor(AValue: TColor);
begin
  if FBgrdColor=AValue then Exit;
  FBgrdColor:=AValue;
  Invalidate;
end;

procedure TTextBox.SetBlinkFreq(AValue: integer);
begin
  if FBlinkFreq=AValue then Exit;
  FBlinkFreq:=AValue;
  if (csDesigning in Componentstate) then exit;
  FTimer.Interval:= aValue;
  Invalidate;
end;

procedure TTextBox.SetBlinking(AValue: boolean);
begin
  if FBlinking=AValue then Exit;
  FBlinking:=AValue;
  if (csDesigning in Componentstate) then exit;
  FTimer.Enabled:= aValue;
  Invalidate;
end;

procedure TTextBox.SetCapLeft(AValue: integer);
begin
  if FCapLeft=AValue then Exit;
  FCapLeft:=AValue;
  if FAutoSize then TriggerAutoSize;
  Invalidate;
end;


procedure TTextBox.SetCaption(AValue: TCaption);
begin
  if aValue = '' then aValue:=' ';
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  CaptionChange:=true;
  if FAutoSize then TriggerAutoSize;
  Invalidate;
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
  Invalidate;
end;

procedure TTextBox.SetLayout(AValue: TTextLayout);
begin
 if fTextStyle.Layout=AValue then exit;
 fTextStyle.Layout:=AValue;
 if aValue <> tlTop then FCapTop:=0;
 if FAutoSize then TriggerAutoSize;
 Invalidate;
end;

procedure TTextBox.SetTextStyle(AValue: TTextStyle);
begin
 FTextStyle:=AValue;
end;

procedure TTextBox.StopBlinkTimer(Sender: TObject);
begin
 FShowCaption := true;
end;

procedure TTextBox.BlinkTimer(Sender: TObject);
begin
 if FShowCaption then FShowCaption := false else FShowCaption := true;
 Invalidate;
end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---Setter Ende---xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

function TTextBox.GetTextWidth(AText: String; AFont: TFont): Integer;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextWidth(AText);
 finally
  bmp.Free;
 end;
end ;

function TTextBox.GetTextHeight(AText: String; AFont: TFont): Integer;
var bmp : TBitmap ;
begin
 Result := 0 ;
 bmp := TBitmap.Create ;
 try
  bmp.Canvas.Font.Assign(AFont);
  Result := bmp.Canvas.TextHeight(AText);
 finally
  bmp.Free;
 end;
end ;

procedure TTextBox.TriggerAutoSize;
begin
 InvalidatePreferredSize;
 if Assigned(Parent) and Parent.AutoSize then
    Parent.AdjustSize;
 AdjustSize;
end;

procedure TTextBox.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  PreferredWidth  := GetTextWidth(FCaption,Font);
  PreferredHeight := GetTextHeight(FCaption,Font);
end;

procedure TTextBox.Paint;
var TeReC : TRect;
begin
  inherited Paint;
 if FBgrdColor <> clNone then
  begin
   canvas.Brush.Color:= FBgrdColor;
   canvas.FillRect(0,0,width,height);
  end;

 if not CaptionChange then FCaption := self.Name;

 TeRec:= rect(0,0,width,height);

 if FShowCaption then   //Blinking
  canvas.TextRect(TeRec,TeRec.Left+FCapLeft,TeRec.Top+FCapTop,
                  FCaption,FTextStyle);

end;


end.
