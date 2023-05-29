unit LEDBar;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf;

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
   FBorderColor: TColor;
   FBorderWidth: integer;
   FIncrementHeight: integer;
   FRectCollection     : TRectCollection;
   FFaultBorder: integer;
   //FRedCount : integer;
   FValue: integer;
   FAlertBorder: integer;
   FStep : double;
   FCount: integer;
   //FYellowCount : integer;

   procedure SetBorderColor(AValue: TColor);
   procedure SetBorderWidth(AValue: integer);
   procedure SetIncrementHeight(AValue: integer);
   procedure SetFaultBorder(AValue: integer);
   procedure SetValue(AValue: integer);
   procedure SetAlertBorder(AValue: integer);
  protected
   function CreateRectangles: TRectCollection;
   function GetRect: TRectCollection;
   function IsRectStored: Boolean;
   procedure SetRect(ARectCollection: TRectCollection);
  public
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

  FRectCollection := CreateRectangles;  //TCollection
  FRectCollection.Add;
end;

procedure TLEDBar.SetBorderColor(AValue: TColor);
begin
  if FBorderColor=AValue then Exit;
  FBorderColor:=AValue;
  Invalidate;
end;

procedure TLEDBar.SetBorderWidth(AValue: integer);
begin
  if FBorderWidth=AValue then Exit;
  FBorderWidth:=AValue;
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


destructor TLEDBar.Destroy;
begin
  inherited Destroy;
  FRectCollection.Free;
end;

procedure TLEDBar.Paint;
var lv : integer;
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
    Canvas.Rectangle(0+FBorderWidth,Height-25-FIncrementHeight-(lv*FIncrementHeight),
                     width-FBorderWidth,Height-25-(lv*FIncrementHeight));
   end;
end;


{$Include rectcollection.inc}
{$Include rectitem.inc}
end.




























