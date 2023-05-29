//this is a part of LEDBar

{%LEDBar}

{ TRectCollection }


constructor TRectCollection.Create(aCollection: TLEDBar;
  aItemClass: TCollectionItemClass);
begin
 inherited Create( aItemClass );
 FDrawRect := aCollection;
end;

function TRectCollection.GetRect(Index: Integer): TRectangle;
begin
 result := TRectangle( inherited Items[Index] );
end;

function TRectCollection.GetEnabled: Boolean;
begin
 result := VisibleCount > 0;
end;

function TRectCollection.GetVisibleCount: Integer;
{$ifNdef newcols}
var
  i: Integer;
{$endif}
begin
  {$ifdef newcols}
  result := Count;
  {$else}
  result := 0;
  for i:=0 to Count-1 do
    if Items[i].Visible then
      inc(result);

  {$endif}
end;

procedure TRectCollection.SetRect(Index: Integer; aRect: TRectangle);
begin
 Items[Index].Assign(aRect);
end;

function TRectCollection.GetOwner: TPersistent;
begin
  Result:= FDrawRect;
end;


