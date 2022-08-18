{ <This component selected a area and returns a Rect>

  Copyright (C) <Version 1.0.1.1 04.05.2021> <Bernd Hübner>

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



unit Sizer;
{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType,
 LCLIntf, StdCtrls, LMessages, CustomLine;

 const
  nothing = 0;  //Rahmenmodus
  active = 1;
  marked = 2;

  LT = 10;   //LeftTop
  TC = 11;   //TopCenter
  RT = 12;   //RightTop
  RC = 13;   //RightCenter
  RB = 14;   //RightBottom
  BC = 15;   //BottomCenter
  LB = 16;   //LeftBottom
  LC = 17;   //LeftCenter


type
 TFinishedEvent =  procedure (aRect:Trect) of object;

type

{ TSizer }

TSizer = class(TGraphicControl)
 private
  FOnFinished: TFinishedEvent;
  FSelArea           : TRect;        //Ausgewählter Bereich

  CustomLine         : array [0..3] of TCustomPen;   //für benuter definierte Linien
  FPrimaryColor      : TColor;       //Untere Farbe des Auswahlrahmens
  FMarkerColor       : TColor;       //Obere Farbe des Auswahlrahmens wenn markiert

  FShowSizer: boolean;
  FSquareColor       : TColor;       //Farbe der StretchQuadrate
  FActiveColor       : TColor;       //Obere Farbe beim Ziehen des Rahmens

  modus              : byte;         //Modus markiert,aktiv
  LTSq               : TRect;        //Quadrat LinksOben
  LTBigSq            : TRect;        //BigQuadrat zum Fangen
  TCSq               : TRect;        //Quadrat ObenMitte
  TCBigSq            : TRect;
  RTSq               : TRect;        //Quadrat RechtsOben
  RTBigSq            : TRect;
  RCSq               : TRect;        //Quadrat RechtsMitte
  RCBigSq            : TRect;
  RBSq               : TRect;        //Quadrat RechtsUnten
  RBBigSq            : TRect;
  BCSq               : TRect;        //Quadrat UntenMitte
  BCBigSq            : TRect;
  LBSq               : TRect;        //Quadrat LinksUnten
  LBBigSq            : TRect;
  LCSq               : TRect;        //Quadrat LinksMitte
  LCBigSq            : TRect;
  square             : byte;         //Welches Quadrat angewählt
  moveX              : integer;      //Startpunkt beim Bewegen
  moveY              : integer;
  moveStart          : boolean;      //Bewegen gestartet
  stretchStart       : boolean;      //Größe ziehen
  moveRect           : TRect;        //Anfangskoordinaten

  procedure CalculateSquares;
  procedure ChangeSize(const X,Y:integer;const {%H-}aSquare:byte);
  procedure ActivePaint;
  procedure MarkedPaint;
  procedure SetSelArea(AValue: TRect);
  procedure SetShowSizer(AValue: boolean);
  //procedure SetShowTextSticker(AValue: boolean);
 protected
  procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
  procedure MouseDown(Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
  procedure MouseUp({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;

 public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  procedure Paint; override;
  property ShowSizer : boolean read FShowSizer write SetShowSizer default false;
  //Rechteckkoordinaten des markierten Bereichs
  property SelArea         : TRect   read FSelArea           write SetSelArea;
 published
  property Left default 30;
  property Top default 30;
  property Width default 50;
  property Height default 35;
  property OnFinished: TFinishedEvent read FOnFinished write FOnFinished;
 end;


procedure Register;

implementation

procedure Register;
begin
  {$I sizer_icon.lrs}
  RegisterComponents('Others',[TSizer]);
end;

constructor TSizer.Create(AOwner: TComponent);
var lv : integer;
begin
  inherited Create(AOwner);
  for lv:=0 to 3 do CustomLine[lv]  := TCustomPen.Create;
  FShowSizer := false;
  modus            := nothing;

  FPrimaryColor     := clwhite;
  FActiveColor      := clred;
  FMarkerColor      := clnavy;
  FSquareColor      := clblue;

  FSelArea:= rect(-20,-20,-50,-50);

end;

destructor TSizer.Destroy;
var lv : integer;
begin
  for lv:=0 to 3 do CustomLine[lv].Free;
  inherited Destroy;
end;

procedure TSizer.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var P1 : TPoint;

begin
 if Button = mbRight then
  begin
   if Assigned(OnFinished) then OnFinished(FSelArea);
   SetShowSizer(false);
   exit;
  end;


   if modus = marked then
    begin
     P1.x:=X;P1.y:=Y;

     if ptinrect(LTBigSq,P1) then square:=LT;
     if ptinrect(TCBigSq,P1) then square:=TC;
     if ptinrect(RTBigSq,P1) then square:=RT;
     if ptinrect(RCBigSq,P1) then square:=RC;
     if ptinrect(RBBigSq,P1) then square:=RB;
     if ptinrect(BCBigSq,P1) then square:=BC;
     if ptinrect(LBBigSq,P1) then square:=LB;
     if ptinrect(LCBigSq,P1) then square:=LC;
     if square <> nothing then
      begin
       moveStart:=false;
       stretchStart:= true;
       exit;
      end;
      if ptinrect(FSelArea,P1) then  //bewegen starten
        begin
         Cursor:= crSize;
         moveX:=x;moveY:=y;
         moveStart:=true;
         moveRect:=FSelArea;
         if ptinrect(FSelArea,P1) then Exit;
        end;
    end;//marked

   FSelArea.Left   := X;
   FSelArea.Top    := Y;
   FSelArea.Right  := X;   //Schaut besser aus
   FSelArea.Bottom := Y;
   modus:=active;
   invalidate;

end;

procedure TSizer.MouseMove(Shift: TShiftState; X, Y: Integer);
var P1 : TPoint;
    deltaX,deltaY : integer;
begin

 if modus = active then   //roter Rahmen
  begin
     FSelArea.Bottom := Y;
     FSelArea.Right  := X;

  end;//active
 if modus = marked then   //blauer Rahmen
  begin
   Cursor:=crdefault;
   P1.x:=X;P1.y:=Y;

   if ptinrect(LTBigSq,P1) then Cursor:= crSizeNW;
   if ptinrect(TCBigSq,P1) then Cursor:= crSizeN;
   if ptinrect(RTBigSq,P1) then Cursor:= crSizeNE;
   if ptinrect(RCBigSq,P1) then Cursor:= crSizeE;
   if ptinrect(RBBigSq,P1) then Cursor:= crSizeSE;
   if ptinrect(BCBigSq,P1) then Cursor:= crSizeS;
   if ptinrect(LBBigSq,P1) then Cursor:= crSizeSW;
   if ptinrect(LCBigSq,P1) then Cursor:= crSizeW;
   if stretchStart then ChangeSize(X,Y,square);
   if moveStart then
      begin
       if ptinrect(FSelArea,P1) then Cursor:= crSize;
       deltaX:= moveX - x; deltaY:= moveY - Y;
       FSelArea.Left   := moveRect.Left-deltaX;
       FSelArea.Right  := moveRect.Right-deltaX;
       FSelArea.Top    := moveRect.Top -deltaY;
       FSelArea.Bottom := moveRect.Bottom-deltaY;
       invalidate;
       exit;
      end;
  end;
 invalidate;
end;

procedure TSizer.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
begin
 if modus = active then
  begin
   FSelArea.Right  := X;
   FSelArea.Bottom := Y;
   modus:= marked;
  end;//active
 if modus = marked then
  begin
   square := nothing;
     if moveStart then
      begin
       moveStart:=false;
       exit;
      end;
  end;


 invalidate;
end;


procedure TSizer.CalculateSquares;
var i : integer;
begin
 with FSelArea do
  begin
   i:=6; //vergrößern um besser zu Fangen
   //Quadrat LinksOben
   LTSq := Rect(Left-2,Top-2,Left+3,Top+3);
   LTBigSq := Rect(Left-2-i,Top-2-i,Left+3+i,Top+3+i);
  //Quadrat ObenMitte
   TCSq := Rect(Left+(round(Width/2)-3),Top-2,Left+(round(Width/2)+3),Top+3);
   TCBigSq := Rect((Left+(round(Width/2)-3))-i,Top-2-i,(Left+(round(Width/2)+3))+i,Top+3+i);
  //Quadrat RechtsOben
   RTSq := Rect(right-3,Top-2,right+2,Top+3);
   RTBigSq := Rect(right-3-i,Top-2-i,right+2+i,Top+3+i);
  //Quadrat RechtsMitte
   RCSq := Rect(right-3,Top+(round(height/2)-3),right+2,Top+(round(height/2)+3));
   RCBigSq := Rect(right-3-i,Top+(round(height/2)-3)-i,right+2+i,Top+(round(height/2)+3)+i);
  //Quadrat RechtsUnten
   RBSq := Rect(right-3,Bottom-3,right+2,Bottom+2);
   RBBigSq := Rect(right-3-i,Bottom-3-i,right+2+i,Bottom+2+i);
  //Quadrat UntenMitte
   BCSq := Rect(Left+(round(Width/2)-3),Bottom-3,left+(round(Width/2)+3),Bottom+2);
   BCBigSq := Rect(Left+(round(Width/2)-3)-i,Bottom-3-i,left+(round(Width/2)+3)+i,Bottom+2+i);
  //Quadrat LinksUnten
   LBSq := Rect(Left-2,Bottom-3,Left+3,Bottom+2);
   LBBigSq := Rect(Left-2-i,Bottom-3-i,Left+3+i,Bottom+2+i);
  //Quadrat LinksMitte
   LCSq := Rect(left-2,Top+(round(height/2)-3),left+3,Top+(round(height/2)+3));
   LCBigSq := Rect(left-2-i,Top+(round(height/2)-3)-i,left+3+i,Top+(round(height/2)+3+i));
  end;
end;

procedure TSizer.ChangeSize(const X,Y:integer;const aSquare:byte);
var delta  : integer;
begin
 if square=LT then   //LinksOben ziehen
   begin
     delta:= Y - FSelArea.Top;
     FSelArea.Left   := FSelArea.Left + delta;
     FSelArea.Top    := FSelArea.Top  + delta;
    end;
 if (square=TC) then   //ObenMitte ziehen
   begin
     FSelArea.Top    := Y;
    end;
 if square=RT then   //RechtsOben ziehen
   begin
     delta:= Y - FSelArea.Top;
     FSelArea.width   := FSelArea.width - delta;
     FSelArea.Top    := FSelArea.Top  + delta;
    end;
 if (square=RC) then   //RechtsMitte ziehen
   begin
     delta:= X - FSelArea.Right;
     FSelArea.width   := FSelArea.width + delta;
    end;
 if square=RB then   //RechtsUnten ziehen
   begin
     delta:= Y - FSelArea.Bottom;
     FSelArea.width    := FSelArea.width + delta;
     FSelArea.Bottom   := FSelArea.Bottom  + delta;
    end;
 if (square=BC) then  //UntenMitte ziehen
   begin
     FSelArea.Bottom    := Y;
    end;
 if square=LB then   //LinksUnten ziehen
   begin
     delta:= FSelArea.Left -x;
     FSelArea.Left   := FSelArea.Left - delta;
     FSelArea.Bottom    := FSelArea.Bottom  + delta;
    end;
 if (square=LC) then   //LinksMitte ziehen
   begin
     FSelArea.Left    := X;
    end;
 invalidate;
end;



procedure TSizer.ActivePaint;
var OldPen : HPEN;
begin
 canvas.Brush.Style:= bsclear;

 OldPen := SelectObject(canvas.Handle,CustomLine[0].CreatePen);
 CustomLine[0].Style:=cpsSolid;
 CustomLine[0].Color  := FPrimaryColor;
 canvas.Rectangle(FSelArea);
 DeleteObject(SelectObject(canvas.Handle, OldPen));

 OldPen := SelectObject(canvas.Handle,CustomLine[1].CreatePen);
 CustomLine[1].Style:=cpsDash;
 CustomLine[1].Color  := FActiveColor;
 canvas.Rectangle(FSelArea);
 DeleteObject(SelectObject(canvas.Handle, OldPen));
end;

procedure TSizer.MarkedPaint;
var OldPen : HPEN;
    lv : integer;
begin
 canvas.Brush.Style:= bsclear;

 for lv:=0 to 1 do   //zweimal sonst keine gestrichelte Linie bis mouse mouve
  begin
   OldPen := SelectObject(canvas.Handle,CustomLine[2].CreatePen);
   CustomLine[2].Style:=cpsSolid;
   CustomLine[2].Color  := FPrimaryColor;
   canvas.Rectangle(FSelArea);
   DeleteObject(SelectObject(canvas.Handle, OldPen));

   OldPen := SelectObject(canvas.Handle,CustomLine[3].CreatePen);
   CustomLine[3].Style:=cpsDash;
   CustomLine[3].Color  := FMarkerColor;
   canvas.Rectangle(FSelArea);
   DeleteObject(SelectObject(canvas.Handle, OldPen));
  end;

 CalculateSquares;

 //Ausgabe Quadrate
 canvas.Brush.Style:= bssolid;
 canvas.Brush.Color:= FSquareColor;
 canvas.Pen.Style  := pssolid;
 canvas.Pen.Color  := FSquareColor;
 canvas.Rectangle(LTSq);
 canvas.Rectangle(TCSq);
 canvas.Rectangle(RTSq);
 canvas.Rectangle(RCSq);
 canvas.Rectangle(RBSq);
 canvas.Rectangle(BCSq);
 canvas.Rectangle(LBSq);
 canvas.Rectangle(LCSq);
end;

procedure TSizer.SetSelArea(AValue: TRect);
begin
  if FSelArea=AValue then Exit;
  FSelArea:=AValue;
end;

procedure TSizer.SetShowSizer(AValue: boolean);
begin
  if FShowSizer=AValue then Exit;
  FShowSizer:=AValue;
  if FShowSizer then
  setbounds(0,0,parent.Width,parent.Height) else
   begin
    setbounds(0,0,0,0);
    FSelArea:= rect(-10,0,-10,0);
   end;

end;




procedure TSizer.Paint;
begin

  inherited Paint;
  if csDesigning in ComponentState then
  begin
   width:= 35;
   height:=35;
   FSelArea.Left   := 2;
   FSelArea.Top    := 2;
   FSelArea.Right  :=33;
   FSelArea.Bottom :=33;
   MarkedPaint;
  end else
  begin
   //if not FShowSizer then exit;
   canvas.Pen.Width  := 1;

   if modus = active then ActivePaint else MarkedPaint;


  end;
end;

end.
