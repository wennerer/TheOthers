{ <a few simple graphics routines>
  <Version 1.1.5.1>
  Copyright (C) <16.04.2021> <Bernd Hübner>

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



unit SimpleGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, IntfGraphics, fpImage, lclIntf, LCLType, LCLProc,
  GraphType, math;

type
  TGradientInlay = (giHorizontal,giVertical,giSpread,giRadiant);
type
  TBlsh = (bs_Ellipse,bs_Rect,bs_RoundRect,bs_Circle);

  TBlendShapes = record
   BlendShape   : TBlsh;
   Rectangle    : TRect;
   Rad          : integer;
   end;
type
  TArea        = record
  aPoint       : TPoint;
  Checked      : boolean;
  end;
  TFillArea = array of TArea;

procedure flood(aPoint:TPoint;aColor:TColor;aBmp:TBitmap);
procedure MakeTranslucentBitmap(var aBitmap: TBitmap; TransVal: byte);
procedure TranslucenteBlende(var aBmp:TBitmap;TransVal:byte;aBlendShape:TBlendShapes);
function  SystemToRGB(clSys:TColor):TColor;
procedure GradientInlayBmp(var aBmp: TBitmap; aStart, AEnd: TColor; aInlay: TGradientInlay);
procedure GradientInlayBmpInCanvas(var aBmp: TBitmap; aStart, AEnd: TColor; aInlay: TGradientInlay; aRect:TRect);
function  PtInPoly(const points: array of TPoint;var p: TPoint): Boolean;
function  RotateRight(QBmp : TBitmap):TBitmap;
function  RotateLeft(QBmp : TBitmap):TBitmap;
function  Rotate180(QBmp : TBitmap):TBitmap;
function  FlipHorizontal(QBmp : TBitmap):TBitmap;
function  FlipVertical(QBmp : TBitmap):TBitmap;
procedure BmpToTransparentPNG(var aPNG:TPortableNetworkGraphic ;
          QBmp :TBitmap;aTransparentColor:TColor);
procedure FormFreeCopy(var aBmp:TBitmap;OutLine:array of TPoint);
procedure TransPngToTransBmp(aPng:TPortableNetworkGraphic;var aBmp:TBitmap;var aTransparentColor:TColor);
function  IfPngTransparent(aPng:TPortableNetworkGraphic):boolean;

implementation

procedure flood(aPoint:TPoint;aColor:TColor;aBmp:TBitmap);
var lv,i   : integer;
    alle   : boolean;
    bmp2   : TBitmap;
    Recht1 : TRect;
    Image2 : TLazIntfImage;
    varR,varG,varB     : byte;
    vR,vG,vB           : byte;
    StartColor,StColor : TColor;
    Area            : TFillArea;

  procedure Check4(CheckaPoint:TPoint);
  var x,y        : integer;
      Pixel      : TColor;
      aNewPoint  : TPoint;
  begin
   x:= CheckaPoint.X;
   y:= CheckaPoint.Y;

    varr:= PRGBQuad(Image2.GetDataLineStart(Y))[X+1].rgbRed;      //rechts scannen
    varG:= PRGBQuad(Image2.GetDataLineStart(Y))[X+1].rgbGreen;
    varB:= PRGBQuad(Image2.GetDataLineStart(Y))[X+1].rgbBlue;
    Pixel:=rgb(varR,varG,VarB);
    aNewPoint.X:=x+1;aNewPoint.Y:=y;
     if (Pixel = StartColor) and (PtinRect(Recht1,aNewPoint)=true) then
      begin
       inc(i);
       setlength(Area,i+1);
       Area[i].aPoint.X   :=x+1;
       Area[i].aPoint.Y   :=y;
       Area[i].Checked := false;
       PRGBQuad(Image2.GetDataLineStart(Y))[X+1].rgbRed    := vr;      //füllen
       PRGBQuad(Image2.GetDataLineStart(Y))[X+1].rgbGreen  := vg;
       PRGBQuad(Image2.GetDataLineStart(Y))[X+1].rgbBlue   := vb;
      end;

    varr:= PRGBQuad(Image2.GetDataLineStart(Y+1))[X].rgbRed;      //unten scannen
    varG:= PRGBQuad(Image2.GetDataLineStart(Y+1))[X].rgbGreen;
    varB:= PRGBQuad(Image2.GetDataLineStart(Y+1))[X].rgbBlue;
    Pixel:=rgb(varR,varG,VarB);
    aNewPoint.X:=x;aNewPoint.Y:=y+1;
     if (Pixel = StartColor) and (PtinRect(Recht1,aNewPoint)=true) then
      begin
       inc(i);
       setlength(Area,i+1);
       Area[i].aPoint.X   :=x;
       Area[i].aPoint.Y   :=y+1;
       Area[i].Checked := false;
       PRGBQuad(Image2.GetDataLineStart(Y+1))[X].rgbRed    := vr;      //füllen
       PRGBQuad(Image2.GetDataLineStart(Y+1))[X].rgbGreen  := vg;
       PRGBQuad(Image2.GetDataLineStart(Y+1))[X].rgbBlue   := vb;
      end;

    varr:= PRGBQuad(Image2.GetDataLineStart(Y))[X-1].rgbRed;      //links scannen
    varG:= PRGBQuad(Image2.GetDataLineStart(Y))[X-1].rgbGreen;
    varB:= PRGBQuad(Image2.GetDataLineStart(Y))[X-1].rgbBlue;
    Pixel:=rgb(varR,varG,VarB);
    aNewPoint.X:=x-1;aNewPoint.Y:=y;
     if (Pixel = StartColor) and (PtinRect(Recht1,aNewPoint)=true) then
      begin
       inc(i);
       setlength(Area,i+1);
       Area[i].aPoint.X   :=x-1;
       Area[i].aPoint.Y   :=y;
       Area[i].Checked := false;
       PRGBQuad(Image2.GetDataLineStart(Y))[X-1].rgbRed    := vr;      //füllen
       PRGBQuad(Image2.GetDataLineStart(Y))[X-1].rgbGreen  := vg;
       PRGBQuad(Image2.GetDataLineStart(Y))[X-1].rgbBlue   := vb;
      end;

     varr:= PRGBQuad(Image2.GetDataLineStart(Y-1))[X].rgbRed;      //oben scannen
     varG:= PRGBQuad(Image2.GetDataLineStart(Y-1))[X].rgbGreen;
     varB:= PRGBQuad(Image2.GetDataLineStart(Y-1))[X].rgbBlue;
     Pixel:=rgb(varR,varG,VarB);
      aNewPoint.X:=x;aNewPoint.Y:=y-1;
     if (Pixel = StartColor) and (PtinRect(Recht1,aNewPoint)=true) then
       begin
        inc(i);
        setlength(Area,i+1);
        Area[i].aPoint.X   :=x;
        Area[i].aPoint.Y   :=y-1;
        Area[i].Checked := false;
        PRGBQuad(Image2.GetDataLineStart(Y-1))[X].rgbRed    := vr;      //füllen
        PRGBQuad(Image2.GetDataLineStart(Y-1))[X].rgbGreen  := vg;
        PRGBQuad(Image2.GetDataLineStart(Y-1))[X].rgbBlue   := vb;
      end;
  end;



begin
 aBmp.Width  :=aBmp.Width+4;
 aBmp.Height :=aBmp.Height+4;
 aBmp.Canvas.Draw(1,1,aBmp);

 bmp2     := TBitmap.Create;
 bmp2.SetSize(aBmp.Width,aBmp.Height);
 bmp2.Assign(aBmp);
 Image2:= bmp2.CreateIntfImage;

 Recht1:=Rect(1,1,aBmp.Width,aBmp.Height);


 vR:=getRvalue(aColor);
 vG:=getGvalue(aColor);
 vB:=getBvalue(aColor);


 varR:= PRGBQuad(Image2.GetDataLineStart(aPoint.Y))[aPoint.X].rgbRed;
 varG:= PRGBQuad(Image2.GetDataLineStart(aPoint.Y))[aPoint.X].rgbGreen;
 varB:= PRGBQuad(Image2.GetDataLineStart(aPoint.Y))[aPoint.X].rgbBlue;
 StColor:=rgb(varR,varG,VarB);
 PRGBQuad(Image2.GetDataLineStart(aPoint.Y))[aPoint.X].rgbRed    := vr;      //füllen
 PRGBQuad(Image2.GetDataLineStart(aPoint.Y))[aPoint.X].rgbGreen  := vg;
 PRGBQuad(Image2.GetDataLineStart(aPoint.Y))[aPoint.X].rgbBlue   := vb;
 StartColor:=StColor;

 if StartColor <> aColor then
  begin
   i:=1;
   setlength(Area,i+2);
   Area[i].aPoint.X   :=aPoint.X;
   Area[i].aPoint.Y   :=aPoint.Y;
   Area[i].Checked := true;

   Check4(aPoint);
   repeat
    alle:=true;
     for lv:=1 to i do
      begin
       if Area[lv].Checked = false then
        begin
         Area[lv].Checked := true;
         Check4(Area[lv].aPoint);
        end;
      end;
     for Lv:=1 to i do
      begin
       if Area[lv].Checked = false then alle:=false;
      end;
   until (alle = true)or (lv>1500000) ;

  aBmp.LoadFromIntfImage(Image2);
  end;

 aBmp.Canvas.Draw(-1,-1,aBmp);
 aBmp.Width  :=aBmp.Width-4;
 aBmp.Height :=aBmp.Height-4;

 Bmp2.free;
 Image2.Free;
end;



procedure MakeTranslucentBitmap(var aBitmap: TBitmap; TransVal: byte);
var bmp : TBitmap;
    Image1 : TLazIntfImage;
    Image2 : TLazIntfImage;
    valR,valG,valB : byte;
    x,y            : integer;
    P              : TPoint;
begin
 try
  Bmp       := TBitmap.Create;
  Bmp.Assign(aBitmap);
  Image1:= Bmp.CreateIntfImage;
  Image2:= TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
  Image2.SetSize(Bmp.Width,Bmp.Height);
    for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQUAD(Image1.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue;

       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valR;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valG;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valB;

       if (valB+valG+valR) <> 0 then
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=TransVal
       else
        PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=0;

      end;//for x
    end;//for y
    aBitmap.LoadFromIntfImage(Image2);

 finally
  Image1.Free;
  Image2.Free;
  Bmp.Free;
 end;
end;

procedure TranslucenteBlende(var aBmp:TBitmap;TransVal:byte;aBlendShape:TBlendShapes);
var dest, trBmp, mask, bmp : TBitmap;
    Image1 : TLazIntfImage;
    Image2 : TLazIntfImage;
    valR,valG,valB : byte;
    x,y            : integer;
    P              : TPoint;
begin
 try
  trBmp := TBitmap.Create;
  trBmp.SetSize(aBmp.Width,aBmp.Height);
  trBmp.TransparentColor:= clblack;
  trBmp.Transparent:= true;
  trBmp.Canvas.Brush.Color:=clwhite;
  trBmp.Canvas.FillRect(0,0,trBmp.Width,trBmp.Height);
  trBmp.Canvas.Pen.Color:=clblack;
  trBmp.Canvas.Brush.Color:=clblack;

  case aBlendShape.BlendShape of
   bs_RoundRect : trBmp.Canvas.RoundRect(aBlendShape.Rectangle,aBlendShape.Rad,aBlendShape.Rad);
   bs_Rect      : trBmp.Canvas.Rectangle(aBlendShape.Rectangle);
   bs_Ellipse   : trBmp.Canvas.Ellipse(aBlendShape.Rectangle);
   bs_Circle    : trBmp.Canvas.Ellipse(aBlendShape.Rectangle);
  end;

  mask := TBitmap.Create;
  mask.SetSize(aBmp.Width,aBmp.Height);
  mask.Canvas.Brush.Color:=clwhite;
  mask.Canvas.FillRect(0,0,mask.Width,mask.Height);
  mask.Canvas.Brush.Color:=clblack;

  case aBlendShape.BlendShape of
   bs_RoundRect : mask.Canvas.RoundRect(aBlendShape.Rectangle,aBlendShape.Rad,aBlendShape.Rad);
   bs_Rect      : mask.Canvas.Rectangle(aBlendShape.Rectangle);
   bs_Ellipse   : mask.Canvas.Ellipse(aBlendShape.Rectangle);
   bs_Circle    : mask.Canvas.Ellipse(aBlendShape.Rectangle);
  end;

  dest  := TBitmap.Create;
  dest.SetSize(aBmp.Width,aBmp.Height);
  Dest.Canvas.copymode:=cmSrcCopy;
  Dest.Assign(aBmp);
  Dest.Canvas.Draw(0,0,trBmp);
  Dest.Canvas.copymode:=cmSrcInvert;
  Dest.Canvas.Draw(0,0,mask);

  Bmp       := TBitmap.Create;
  Bmp.Assign(Dest);
  Image1:= Bmp.CreateIntfImage;
  Image2:= TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
  Image2.SetSize(Bmp.Width,Bmp.Height);
    for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQUAD(Image1.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue;

       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valR;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valG;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valB;

       if (valB+valG+valR) <> 0 then
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=TransVal
       else
        PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=0;

      end;//for x
    end;//for y
    aBmp.LoadFromIntfImage(Image2);

 finally
  Image1.Free;
  Image2.Free;
  Bmp.Free;
  trBmp.Free;
  mask.Free;
  dest.Free;
 end;
end;

function SystemToRGB(clSys:TColor):TColor;
var FPCol :  TFPColor;
begin
  FPCol:=TColorToFPColor(ColorToRGB(clSys));
  result :=FPColorToTColor(FPCol);
end;

procedure GradientInlayBmp(var aBmp: TBitmap; aStart, AEnd: TColor; aInlay: TGradientInlay);
var lv:integer;
    StartR,StartG,StartB : integer;
    EndR,EndG,EndB    : integer;
    delR,delG,delB       : integer;
    fR,fG,fb : double;
    valR,valG,valB       : integer;
    greater,radius: integer;

begin
 aStart:=SystemToRGB(aStart);
 aEnd:=SystemToRGB(aEnd);

 StartR:=getRvalue(aStart);
 StartG:=getGvalue(aStart);
 StartB:=getBvalue(aStart);

 EndR:=getRvalue(aEnd);
 EndG:=getGvalue(aEnd);
 EndB:=getBvalue(aEnd);

 delR:= StartR-EndR;
 delG:= StartG-EndG;
 delB:= StartB-EndB;

  if aInlay = giSpread then
 begin
  if aBmp.Height >= aBmp.Width then
   greater := round(aBmp.Height/2)
  else
   greater := round(aBmp.Width/2);

  radius:= round(sqrt(sqr((aBmp.Width/2))+sqr((aBmp.Height/2))));
  fR := delR /radius;
  fG := delG /radius;
  fB := delB /radius;
  for Lv:=radius downto 0 do
   begin
    valR:= EndR+round(lv*fR);
    valG:= EndG+round(lv*fG);
    valB:= EndB+round(lv*fB);
    if valR <0 then valR:=0;
    if valG <0 then valG:=0;
    if valB <0 then valB:=0;
    if valR >255 then valR:=255;
    if valG >255 then valG:=255;
    if valB >255 then valB:=255;
    aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
    aBmp.Canvas.Pen.Color:=rgb(valR,valG,valB);
    aBmp.Canvas.Ellipse((aBmp.width div 2) -lv,(aBmp.height div 2)-lv,
                       (aBmp.width div 2)+lv,(aBmp.height div 2)+lv);
  end;
 end; //giSpread


 if aInlay = giRadiant then
 begin
  if aBmp.Height >= aBmp.Width then
   greater := round(aBmp.Height/2)
   else
   greater := round(aBmp.Width/2);

   fR := delR /greater;
   fG := delG /greater;
   fB := delB /greater;
   for Lv:=greater downto 0 do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect((aBmp.width div 2) -lv,(aBmp.height div 2)-lv,
                       (aBmp.width div 2)+lv,(aBmp.height div 2)+lv);
  end;
 end; //giRadiant


 if aInlay = giVertical then
 begin
  fR := delR /aBmp.Height;
  fG := delG /aBmp.Height;
  fB := delB /aBmp.Height;

 for Lv:=0 to aBmp.Height do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect(0,lv,aBmp.Width,lv+1);
  end;
 end;//giVertical

 if aInlay = giHorizontal then
 begin
  fR := delR /aBmp.Width;
  fG := delG /aBmp.Width;
  fB := delB /aBmp.Width;

 for Lv:=0 to aBmp.Width do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   aBmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   aBmp.Canvas.FillRect(lv,0,lv+1,aBmp.Height);
  end;

 end;//giHorizontal
end;

procedure GradientInlayBmpInCanvas(var aBmp: TBitmap; aStart, AEnd: TColor; aInlay: TGradientInlay; aRect:TRect);
var lv                   :integer;
    StartR,StartG,StartB : integer;
    EndR,EndG,EndB       : integer;
    delR,delG,delB       : integer;
    fR,fG,fb             : double;
    valR,valG,valB       : integer;
    greater,radius       : integer;
    tmpBmp               : TBitmap;

begin
 aStart:=SystemToRGB(aStart);
 aEnd:=SystemToRGB(aEnd);

 StartR:=getRvalue(aStart);
 StartG:=getGvalue(aStart);
 StartB:=getBvalue(aStart);

 EndR:=getRvalue(aEnd);
 EndG:=getGvalue(aEnd);
 EndB:=getBvalue(aEnd);

 delR:= StartR-EndR;
 delG:= StartG-EndG;
 delB:= StartB-EndB;

 tmpbmp     := TBitmap.Create;
 tmpbmp.SetSize(aRect.Width,aRect.Height);

  if aInlay = giSpread then
 begin
  if tmpbmp.Height >= tmpbmp.Width then
   greater := round(tmpbmp.Height/2)
  else
   greater := round(tmpbmp.Width/2);

  radius:= round(sqrt(sqr((tmpbmp.Width/2))+sqr((tmpbmp.Height/2))));
  fR := delR /radius;
  fG := delG /radius;
  fB := delB /radius;
  for Lv:=radius downto 0 do
   begin
    valR:= EndR+round(lv*fR);
    valG:= EndG+round(lv*fG);
    valB:= EndB+round(lv*fB);
    if valR <0 then valR:=0;
    if valG <0 then valG:=0;
    if valB <0 then valB:=0;
    if valR >255 then valR:=255;
    if valG >255 then valG:=255;
    if valB >255 then valB:=255;
    tmpbmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
    tmpbmp.Canvas.Pen.Color:=rgb(valR,valG,valB);
    tmpbmp.Canvas.Ellipse((tmpbmp.width div 2) -lv,(tmpbmp.height div 2)-lv,
                       (tmpbmp.width div 2)+lv,(tmpbmp.height div 2)+lv);
  end;
 end; //giSpread


 if aInlay = giRadiant then
 begin
  if tmpbmp.Height >= tmpbmp.Width then
   greater := round(tmpbmp.Height/2)
   else
   greater := round(tmpbmp.Width/2);

   fR := delR /greater;
   fG := delG /greater;
   fB := delB /greater;
   for Lv:=greater downto 0 do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   tmpbmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   tmpbmp.Canvas.FillRect((tmpbmp.width div 2) -lv,(tmpbmp.height div 2)-lv,
                       (tmpbmp.width div 2)+lv,(tmpbmp.height div 2)+lv);
  end;
 end; //giRadiant


 if aInlay = giVertical then
 begin
  fR := delR /tmpbmp.Height;
  fG := delG /tmpbmp.Height;
  fB := delB /tmpbmp.Height;

 for Lv:=0 to tmpbmp.Height do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   tmpbmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   tmpbmp.Canvas.FillRect(0,lv,tmpbmp.Width,lv+1);
  end;
 end;//giVertical

 if aInlay = giHorizontal then
 begin
  fR := delR /tmpbmp.Width;
  fG := delG /tmpbmp.Width;
  fB := delB /tmpbmp.Width;

 for Lv:=0 to tmpbmp.Width do
  begin
   valR:= StartR-round(lv*fR);
   valG:= StartG-round(lv*fG);
   valB:= StartB-round(lv*fB);
   if valR <0 then valR:=0;
   if valG <0 then valG:=0;
   if valB <0 then valB:=0;
   if valR >255 then valR:=255;
   if valG >255 then valG:=255;
   if valB >255 then valB:=255;
   tmpbmp.Canvas.Brush.Color:=rgb(valR,valG,valB);
   tmpbmp.Canvas.FillRect(lv,0,lv+1,tmpbmp.Height);
  end;
 end;//giHorizontal

 aBmp.TransparentColor:= rgb(1,1,1);
 aBmp.Transparent:= true;
 aBmp.Canvas.Brush.Color:= rgb(1,1,1);
 aBmp.Canvas.FillRect(0,0,aBmp.Width,aBmp.Height);

 aBmp.Canvas.Draw(aRect.Left,aRect.Top,tmpbmp);
 tmpbmp.Free;
end;

function PtInPoly(const points: array of TPoint;var p: TPoint): Boolean;
var
  i     : Integer;
  angle : Double;
  InIt  : boolean;
  p1,p2 : TPoint;

  function Angle2D(x1, y1, x2, y2: Double): Double;
  var
    dtheta, theta1, theta2: Double;
  begin
    theta1 := ArcTan2(y1, x1);
    theta2 := ArcTan2(y2, x2);
    dtheta := theta2 - theta1;
    while (dtheta > PI) do
       dtheta := dtheta - 2*pi;
    while (dtheta < -PI) do
       dtheta := dtheta + 2*pi;
    result := dtheta;
  end;

begin
  angle := 0;
  for i := 0 to High(points) do
  begin
    p1.X := points[i].X - p.X;
    p1.Y := points[i].Y - p.Y;
    p2.X := points[(i+1) mod Length(points)].X - p.X;
    p2.Y := points[(i+1) mod Length(points)].Y - p.Y;
    angle := angle + Angle2D(p1.X, p1.Y, p2.X, p2.Y);
  end;
  InIt := Abs(angle) < pi;
  if InIt then result:=false else result:=true;
end;


function RotateRight(QBmp: TBitmap):TBitmap;
var bmp2,bmp3       : TBitmap;
    Image2,Image3   : TLazIntfImage;
    varR,varG,varB  : byte;
    x,y,w           : integer;
    P               : TPoint;
begin
 try
  bmp2     := TBitmap.Create;
  bmp2.SetSize(QBmp.Width,QBmp.Height);
  bmp2.Assign(QBmp);
  Image2:= bmp2.CreateIntfImage;
  bmp3     := TBitmap.Create;
  bmp3.SetSize(QBmp.Height,QBmp.Width);
  Image3:= bmp3.CreateIntfImage;
  for x := 0 to  Image2.width- 1 do
   begin
    w:= Image2.height - 1;
    for y := 0 to Image2.height - 1 do
    begin
      {$IFDEF LINUX}
      varR:= PRGBQuad(Image2.GetDataLineStart(Y))[X].rgbRed;
      varG:= PRGBQuad(Image2.GetDataLineStart(Y))[X].rgbGreen;
      varB:= PRGBQuad(Image2.GetDataLineStart(Y))[X].rgbBlue;

      PRGBQuad(Image3.GetDataLineStart(X))[W].rgbRed    := varR;
      PRGBQuad(Image3.GetDataLineStart(X))[W].rgbGreen  := varG;
      PRGBQuad(Image3.GetDataLineStart(X))[W].rgbBlue   := varB;
      {$ENDIF}
      {$IFDEF WINDOWS}
      P.X:=x;P.Y:=y;
      varR:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtRed;
      varG:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtGreen;
      varB:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtBlue;
      P.X:=x;P.Y:=w;
      PRGBTriple(Image3.GetDataLineStart(P.X))[P.Y].rgbtRed    := varR;
      PRGBTriple(Image3.GetDataLineStart(P.X))[P.Y].rgbtGreen  := varG;
      PRGBTriple(Image3.GetDataLineStart(P.X))[P.Y].rgbtBlue   := varB;
      {$ENDIF}
      dec(w);
    end;
   end;
  Qbmp.LoadFromIntfImage(Image3);
  result:=Qbmp;
 finally
  Bmp2.free;
  Bmp3.free;
  Image2.Free;
  Image3.Free;
 end;
end;

function RotateLeft(QBmp: TBitmap):TBitmap;
var bmp2,bmp3       : TBitmap;
    Image2,Image3   : TLazIntfImage;
    varR,varG,varB  : byte;
    x,y,w           : integer;
    P               : TPoint;
begin
 try
  bmp2     := TBitmap.Create;
  bmp2.SetSize(QBmp.Width,QBmp.Height);
  bmp2.Assign(QBmp);
  Image2:= bmp2.CreateIntfImage;
  bmp3     := TBitmap.Create;
  bmp3.SetSize(QBmp.Height,QBmp.Width);
  Image3:= bmp3.CreateIntfImage;
  for y := 0 to  Image2.Height- 1 do
   begin
    w:= Image2.width - 1;
    for x := 0 to Image2.Width - 1 do
    begin
      {$IFDEF LINUX}
      varR:= PRGBQuad(Image2.GetDataLineStart(Y))[X].rgbRed;
      varG:= PRGBQuad(Image2.GetDataLineStart(Y))[X].rgbGreen;
      varB:= PRGBQuad(Image2.GetDataLineStart(Y))[X].rgbBlue;

      PRGBQuad(Image3.GetDataLineStart(W))[Y].rgbRed    := varR;
      PRGBQuad(Image3.GetDataLineStart(W))[Y].rgbGreen  := varG;
      PRGBQuad(Image3.GetDataLineStart(W))[Y].rgbBlue   := varB;
      {$ENDIF}
      {$IFDEF WINDOWS}
      P.X:=x;P.Y:=y;
      varR:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtRed;
      varG:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtGreen;
      varB:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtBlue;
      P.X:=w;P.Y:=y;
      PRGBTriple(Image3.GetDataLineStart(P.X))[P.Y].rgbtRed    := varR;
      PRGBTriple(Image3.GetDataLineStart(P.X))[P.Y].rgbtGreen  := varG;
      PRGBTriple(Image3.GetDataLineStart(P.X))[P.Y].rgbtBlue   := varB;
      {$ENDIF}
      dec(w);
    end;
   end;
  Qbmp.LoadFromIntfImage(Image3);
  result:=Qbmp;
 finally
  Bmp2.free;
  Bmp3.free;
  Image2.Free;
  Image3.Free;
 end;
end;

function Rotate180(QBmp: TBitmap):TBitmap;
begin
 RotateLeft(QBmp);
 result:=RotateLeft(QBmp);
end;

function FlipHorizontal(QBmp: TBitmap):TBitmap;
 var bmp2,bmp3       : TBitmap;
     Image2,Image3   : TLazIntfImage;
     varR,varG,varB  : byte;
     x,y,w           : integer;
     P               : TPoint;

begin
 try
  bmp2     := TBitmap.Create;
  bmp2.SetSize(QBmp.Width,QBmp.Height);
  bmp2.Assign(QBmp);
  Image2:= bmp2.CreateIntfImage;
  bmp3     := TBitmap.Create;
  bmp3.SetSize(QBmp.Width,QBmp.Height);
  Image3:= bmp3.CreateIntfImage;

  for y := 0 to  Image2.height- 1 do
   begin
    w:= Image2.width - 1;
    for x := 0 to Image2.width - 1 do
     begin
      {$IFDEF LINUX}
      P.X:=x;P.Y:=y;
      varR:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed;
      varG:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen;
      varB:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue;
      P.X:=w;P.Y:=y;
      PRGBQuad(Image3.GetDataLineStart(P.Y))[P.X].rgbRed    := varR;
      PRGBQuad(Image3.GetDataLineStart(P.Y))[P.X].rgbGreen  := varG;
      PRGBQuad(Image3.GetDataLineStart(P.Y))[P.X].rgbBlue   := varB;
      {$ENDIF}
      {$IFDEF WINDOWS}
      P.X:=x;P.Y:=y;
      varR:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtRed;
      varG:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtGreen;
      varB:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtBlue;
      P.X:=w;P.Y:=y;
      PRGBTriple(Image3.GetDataLineStart(P.Y))[P.X].rgbtRed    := varR;
      PRGBTriple(Image3.GetDataLineStart(P.Y))[P.X].rgbtGreen  := varG;
      PRGBTriple(Image3.GetDataLineStart(P.Y))[P.X].rgbtBlue   := varB;
      {$ENDIF}
      dec(w);
     end;
    end;
  Qbmp.LoadFromIntfImage(Image3);
  result:=Qbmp;
 finally
  Bmp2.free;
  Bmp3.free;
  Image2.Free;
  Image3.Free;
 end;
end;

function FlipVertical(QBmp: TBitmap):TBitmap;
var bmp2,bmp3      : TBitmap;
    Image2,Image3  : TLazIntfImage;
    varR,varG,varB : byte;
    x,y,w          : integer;
    P              :TPoint;

begin
 try


  bmp2     := TBitmap.Create;
  bmp2.SetSize(QBmp.Width,QBmp.Height);
  bmp2.Assign(QBmp);
  Image2:= bmp2.CreateIntfImage;
  bmp3     := TBitmap.Create;
  bmp3.SetSize(QBmp.Width,QBmp.Height);
  Image3:= bmp3.CreateIntfImage;

  w:= Image2.height;
  for y := 0 to  Image2.height- 1 do
   begin
    dec(w);
    for x := 0 to Image2.width - 1 do
     begin
      {$IFDEF LINUX}
      P.X:=x;P.Y:=y;
      varR:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed;
      varG:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen;
      varB:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue;
      P.X:=x;P.Y:=w;
      PRGBQuad(Image3.GetDataLineStart(P.Y))[P.X].rgbRed    := varR;
      PRGBQuad(Image3.GetDataLineStart(P.Y))[P.X].rgbGreen  := varG;
      PRGBQuad(Image3.GetDataLineStart(P.Y))[P.X].rgbBlue   := varB;
      {$ENDIF}
      {$IFDEF WINDOWS}
      P.X:=x;P.Y:=y;
      varR:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtRed;
      varG:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtGreen;
      varB:= PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtBlue;
      P.X:=x;P.Y:=w;
      PRGBTriple(Image3.GetDataLineStart(P.Y))[P.X].rgbtRed    := varR;
      PRGBTriple(Image3.GetDataLineStart(P.Y))[P.X].rgbtGreen  := varG;
      PRGBTriple(Image3.GetDataLineStart(P.Y))[P.X].rgbtBlue   := varB;
      {$ENDIF}
     end;
    end;
  Qbmp.LoadFromIntfImage(Image3);
  result:=Qbmp;
 finally
 end;
  Bmp2.free;
  Bmp3.free;
  Image2.Free;
  Image3.Free;
end;

procedure BmpToTransparentPNG(var aPNG:TPortableNetworkGraphic ;QBmp :TBitmap;aTransparentColor:TColor);
var Image1 : TLazIntfImage;
    Image2 : TLazIntfImage;
    Bmp    : TBitmap;
    png    : TPortableNetworkGraphic;
    valR,valG,valB : byte;
    x,y            : integer;
    P              : TPoint;
    tmpColor       : TColor;
    DestColor      : TFPColor;
begin
 try
  png       := TPortableNetworkGraphic.Create;
  Bmp       := TBitmap.Create;
  Bmp.Assign(QBmp);
  Image1:= Bmp.CreateIntfImage;
  Image2:= TLazIntfImage.Create(0,0, [riqfRGB, riqfAlpha]);
  Image2.SetSize(Bmp.Width,Bmp.Height);
  {$IFDEF LINUX}
   for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue;

       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed    := valB;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen  := valG;
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue   := valR;
       tmpColor:=rgb(valR,valG,valB);
       if tmpColor = aTransparentColor then
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=0
       else
       PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved:=255;
      end;//for x
    end;//for y
    png.LoadFromIntfImage(Image2);
    apng.Assign(png);
   {$ENDIF}
   {$IFDEF WINDOWS}
   for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       valR:= PRGBTriple(Image1.GetDataLineStart(P.Y))[P.X].rgbtRed;
       valG:= PRGBTriple(Image1.GetDataLineStart(P.Y))[P.X].rgbtGreen;
       valB:= PRGBTriple(Image1.GetDataLineStart(P.Y))[P.X].rgbtBlue;
       tmpColor:=rgb(valR,valG,valB);
       DestColor := Image1.Colors[x,y];
       if tmpColor = aTransparentColor then
       DestColor.Alpha:= alphaTransparent else DestColor.Alpha:=alphaOpaque;
       Image2.Colors[x,y]:=DestColor;
      end;//for x
    end;//for y
   png.LoadFromIntfImage(Image2);
   apng.Assign(png);
   {$ENDIF}
 finally
  Image1.Free;
  Image2.Free;
  png.Free;
  Bmp.Free;
 end;
end;

procedure FormFreeCopy(var aBmp:TBitmap;OutLine:array of TPoint);
var x,y           : integer;
    TmpBmp1       : TBitmap;
    TmpBmp2       : TBitmap;
    Image1        : TLazIntfImage;
    Image2        : TLazIntfImage;
    valR,valG,valB: byte;
    P             : TPoint;
begin
 try
  TmpBmp1        := TBitmap.Create;
  TmpBmp1.Assign(aBmp);
  Image1 := TmpBmp1.CreateIntfImage;
  TmpBmp2        := TBitmap.Create;
  TmpBmp2.SetSize(aBmp.Width,aBmp.Height);
  TmpBmp2.Canvas.Brush.Color:= rgb(253,253,253);
  TmpBmp2.Canvas.Pen.Color:=rgb(253,253,253);
  TmpBmp2.Canvas.Rectangle(0,0,aBmp.Width,aBmp.Height);
  Image2 := TmpBmp2.CreateIntfImage;

  for y := 0 to  aBmp.height - 1 do
    begin
     for x := 0 to aBmp.width - 1 do
      begin
       P := Point(x,y);
       if PtInPoly(OutLine,P) then
        begin
         {$IFDEF LINUX}
         valR:= PRGBQuad(Image1.GetDataLineStart(y))[X].rgbRed;      //scannen
         valG:= PRGBQuad(Image1.GetDataLineStart(y))[X].rgbGreen;
         valB:= PRGBQuad(Image1.GetDataLineStart(y))[X].rgbBlue;

         PRGBQuad(Image2.GetDataLineStart(y))[X].rgbRed    := valR;  //zeichnen
         PRGBQuad(Image2.GetDataLineStart(y))[X].rgbGreen  := valG;
         PRGBQuad(Image2.GetDataLineStart(y))[X].rgbBlue   := ValB;
         {$ENDIF}
         {$IFDEF WINDOWS}
         valR:= PRGBTriple(Image1.GetDataLineStart(P.Y))[P.X].rgbtRed;
         valG:= PRGBTriple(Image1.GetDataLineStart(P.Y))[P.X].rgbtGreen;
         valB:= PRGBTriple(Image1.GetDataLineStart(P.Y))[P.X].rgbtBlue;

         PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtRed    := valR;
         PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtGreen  := valG;
         PRGBTriple(Image2.GetDataLineStart(P.Y))[P.X].rgbtBlue   := valB;
         {$ENDIF}
        end;
      end;//x
    end;//y
  TmpBmp2.LoadFromIntfImage(Image2);
  aBmp.Assign(TmpBmp2);
  aBmp.Transparent:=true;
  aBmp.TransparentColor:=rgb(253,253,253);
 finally
  Image1.Free;
  Image2.Free;
  TmpBmp1.Free;
  TmpBmp2.Free;
 end;

end;

procedure TransPngToTransBmp(aPng:TPortableNetworkGraphic;var aBmp:TBitmap;var aTransparentColor:TColor);
var TmpBmp1 : TBitmap;
    TmpBmp2 : TBitmap;
    Image1  : TLazIntfImage;
    Image2  : TLazIntfImage;
    P       : TPoint;
    valR,valG,valB: byte;
    y,x     : integer;
    DestColor      : TFPColor;
begin
 try
  TmpBmp1 := TBitmap.Create;
  TmpBmp1.SetSize(aPng.Width,aPng.Height);
  Image1 := TmpBmp1.CreateIntfImage;
  TmpBmp2 := TBitmap.Create;
  TmpBmp2.SetSize(aPng.Width,aPng.Height);
  Image2 := aPng.CreateIntfImage;

  for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       {$IFDEF LINUX}
       valR:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbRed;
       valG:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbGreen;
       valB:= PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbBlue;
       if PRGBQuad(Image2.GetDataLineStart(P.Y))[P.X].rgbReserved=0 then
       aTransparentColor := rgb(valB,valG,valR);
       PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbRed    := valB;
       PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen  := valG;
       PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue   := valR;
       {$ENDIF}
       {$IFDEF WINDOWS}
       DestColor := Image2.Colors[x,y];
       Image1.Colors[x,y]:=DestColor;
       if DestColor.Alpha = alphaTransparent then
        begin
         aTransparentColor:=FPColorToTColorRef(DestColor);
        end;
       {$ENDIF}
      end;//for x
    end;//for y
    TmpBmp2.LoadFromIntfImage(Image1);
    TmpBmp2.Transparent:=true;
    TmpBmp2.TransparentColor:=aTransparentColor;
    aBmp.Assign(TmpBmp2);

 finally
  TmpBmp1.Free;
  TmpBmp2.Free;
  Image1.Free;
  Image2.Free;
 end;
end;

function IfPngTransparent(aPng:TPortableNetworkGraphic):boolean;
var Image1    : TLazIntfImage;
    x,y       : integer;
    t         : boolean;
    P         : TPoint;
    DestColor : TFPColor;
begin
try
 Image1 := aPng.CreateIntfImage;
 t:=false;

 for y := 0 to  Image1.height - 1 do
    begin
     for x := 0 to Image1.width - 1 do
      begin
       P.X:=x;P.Y:=y;
       {$IFDEF LINUX}
       if PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbReserved=255 then
       {$ENDIF}
       {$IFDEF WINDOWS}
       DestColor := Image1.Colors[x,y];
       if DestColor.Alpha = alphaTransparent then
       {$ENDIF}
        begin
         //DebugLn(inttostr(PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbRed));
         //debugln(inttostr(PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbGreen));
         //debugln(inttostr(PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbBlue));
         //debugln(inttostr(PRGBQuad(Image1.GetDataLineStart(P.Y))[P.X].rgbReserved));
         result:= true;
         t := true;
         break;
        end;//if 0
      end;//x
      if t then break;
    end;//y
 if not t then result:= false;
 finally
  Image1.Free;
 end;
end;

end.

