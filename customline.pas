{ <Makes a custom line>

  Copyright (C) <Version 1.0.1.1 09.03.2021> <Bernd Hübner>

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option)
  any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with
  independent modules to produce an executable, regardless of the license terms of these independent modules,and to
  copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each
  linked independent module, the terms and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this library, you may extend this exception to
  your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
  details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to
  the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}


unit CustomLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, LCLType, LCLIntf, Dialogs, Controls;

type
 TCustomPenStyle = (cpsSolid,cpsDash,cpsDot,cpsDashDot,cpsDashDotDot,cpsNull);

type
 TPenValues = record
  Color    : TColor;                  //Linienfarbe
  PenStyle : TCustomPenStyle;         //benutzerdefinierter Stil
  PenWidth : integer;                 //Liniendicke
  EndCap   : integer;                 //Style Linienende
  Join     : integer;                 //Ecken Style
  LB       : TLogBrush;               //LogBrush
  Style    : LongWord;                //PenStyle
  Dashes   : array [0..5] of DWord;   //aray für Striche
  Length   : DWord;                   //array Länge
  LL       : integer;                 //Länge Linien
  LS       : integer;                 //Länge Lücken
 end;

type
 { TCustomPen }
 TCustomPen = class (TObject)
  private
   FParent   : TWinControl;
   procedure SetColor(AValue: TColor);
   procedure SetParent(AValue: TWinControl);
   procedure SetPenStyle(AValue: TCustomPenStyle);
   procedure SetPenWidth(AValue: integer);
  public
   PenValues : TPenValues;
   AktValues : TPenValues;
   constructor Create;overload;
   function  CreatePen: HPen;
   property Style : TCustomPenStyle read PenValues.PenStyle  write SetPenStyle  default cpsSolid;
   property PenWidth : integer      read PenValues.PenWidth  write SetPenWidth  default 1;
   property Color : TColor          read PenValues.Color     write SetColor     default clBlack;
   property Parent: TWinControl     read FParent   write SetParent;
 end;
implementation

{ TCustomPen }

constructor TCustomPen.Create;
begin
  inherited;

  PenValues.Color    := clBlack;
  PenValues.EndCap   := PS_ENDCAP_FLAT;
  PenValues.Join     := PS_JOIN_MITER;
  PenValues.PenStyle := cpsSolid;
  PenValues.PenWidth := 1;
  PenValues.Length   := 4;
  PenValues.LL       := 10;
  PenValues.LS       :=  3;
  PenValues.LB.lbStyle:= BS_SOLID;
  PenValues.LB.lbHatch:= 0;
  AktValues := PenValues;
end;

function TCustomPen.CreatePen: HPen;
begin
 if PenValues.PenStyle = cpsSolid then
  begin
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_Solid + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,0, nil);
  end;

 if PenValues.PenStyle = cpsDash then
  begin
   PenValues.Length      :=  4;
   PenValues.Dashes[0]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsDot then
  begin
   PenValues.Length      :=  4;
   PenValues.Dashes[0]   := round(PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsDashDot then
  begin
   PenValues.Length      :=  4;
   PenValues.Dashes[0]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsDashDotDot then
  begin
   PenValues.Length      :=  6;
   PenValues.Dashes[0]   := round(PenValues.LL*PenValues.PenWidth);
   PenValues.Dashes[1]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[2]   := round(PenValues.PenWidth);
   PenValues.Dashes[3]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.Dashes[4]   := round(PenValues.PenWidth);
   PenValues.Dashes[5]   := round(PenValues.LS*PenValues.PenWidth);
   PenValues.LB.lbColor  := ColorToRGB(PenValues.Color);
   PenValues.Style := PS_USERSTYLE + PS_GEOMETRIC + PenValues.EndCap  + PenValues.Join;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,PenValues.Length,@PenValues.Dashes);
  end;

 if PenValues.PenStyle = cpsNull  then
  begin
   PenValues.Style := PS_GEOMETRIC or PS_Null;
   Result := ExtCreatePen(PenValues.Style,PenValues.PenWidth,PenValues.LB,0, nil);
  end;

 if Result = 0 then showmessage('Fehler beim Erzeugen des Stiftes');
end;


procedure TCustomPen.SetPenWidth(AValue: integer);
begin
  if aValue < 1 then Exit;
  if aValue > 25 then Exit;
  if PenValues.PenWidth = aValue then Exit;
  PenValues.PenWidth:=AValue;
  AktValues := PenValues;
end;

procedure TCustomPen.SetColor(AValue: TColor);
begin
  if PenValues.Color = aValue then Exit;
  PenValues.Color :=AValue;
  AktValues := PenValues;
end;

procedure TCustomPen.SetParent(AValue: TWinControl);
begin
  if FParent=AValue then Exit;
  FParent:=AValue;
end;

procedure TCustomPen.SetPenStyle(AValue: TCustomPenStyle);
begin
  if PenValues.PenStyle=AValue then Exit;
  PenValues.PenStyle:=AValue;
  PenValues.LL :=10;
  PenValues.LS := 3;
  AktValues := PenValues;
end;

end.

