{%LEDBar}

{ TRectangle }

constructor TRectangle.Create(ACollection: TCollection);
begin
 inherited Create(ACollection);
 FRectangles             := ACollection;
 FDisplayName            := 'Rectangle';
 FEnabled                := true;
 FVisible                := true;
 FColor                  := clSilver;
 FBorderColor            := clBlack;
 FBorderWidth            := 1;
 FActiveColor            := clLime;
 FAlertColor             := clYellow;
 FFaultColor             := clRed;

end;




procedure TRectangle.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
end;

procedure TRectangle.SetEnabled(AValue: boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
end;

function TRectangle.GetDisplayName: string;
begin
  Result:=inherited GetDisplayName;
end;

procedure TRectangle.SetDisplayName(const Value: string);
begin
  //inherited SetDisplayName(Value);
 FDisplayName := Value;
end;

function TRectangle.GetOwner: TPersistent;
begin
  //Result:=inherited GetOwner;
 Result := FRectangles;
end;

