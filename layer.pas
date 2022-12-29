{ <This component provides a page that can also be made invisible at design time.>

  Copyright (C) <Version 1.0.0.1 29.12.2022> <Bernd Hübner>

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



unit Layer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLProc;

type

  { TLayer }

  TLayer = class(TCustomControl)
  private

  protected
   procedure SetVisible(Value: Boolean);override;
  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;

  published
   property Width;
   property Height;
   property Visible;
   property Color;
   property Align;
   property Anchors;
   property BorderSpacing;
   property Constraints;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I layer_icon.lrs}
  RegisterComponents('Others',[TLayer]);
end;

{ TLayer }

procedure TLayer.SetVisible(Value: Boolean);
begin

  if Value then ControlStyle := ControlStyle + [csAcceptsControls]-[csNoDesignVisible] else
   ControlStyle := ControlStyle + [csAcceptsControls,csNoDesignVisible];
  inherited SetVisible(Value);
end;

constructor TLayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 30;
  Height:= 30;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Align := alClient;
end;

destructor TLayer.Destroy;
begin
  inherited Destroy;
end;




end.
