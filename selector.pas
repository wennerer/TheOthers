{ <This component creates a selection frame>
  <This component is a part of the package the others>

  Copyright (C) <Version 1.0.2.1 26.02.2021> <Bernd Hübner>

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


unit Selector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, types,
  Menus, LMessages, LCLType, LCLIntf, extctrls, SimpleGraphics, FPCanvas,
  clipbrd, StdCtrls, IntfGraphics, GraphType, LCLTranslator(*, LCLProc*),
  selectorstrconsts, CustomLine;


const
 nothing  =  0;  //Rahmenmodus
 active   =  1;
 marked   =  2;

 LT       = 10;   //LeftTop
 TC       = 11;   //TopCenter
 RT       = 12;   //RightTop
 RC       = 13;   //RightCenter
 RB       = 14;   //RightBottom
 BC       = 15;   //BottomCenter
 LB       = 16;   //LeftBottom
 LC       = 17;   //LeftCenter

type TSelOpt = (Rectangle,Quad,Formfree);

type
  TOverlay = record
    bkgrdColor : TColor;    // FCuttingColor
    bkgrdRect  : TRect;     // FBackgrd
    DrawArea   : TRect;     // FSelArea
    RtrnBmp    : TBitmap;   // ReturnBmp
    Cutting    : boolean    // Ausschneiden aktiv?
  end;
OverlayList = array of TOverlay;

type
  TSelection = record
    BkgrdColor : TColor;    // FCuttingColor
    BkgrdArea  : TRect;     // FBackgrd
    DrawArea   : TRect;     // FSelArea
    RtrnBmp    : TBitmap;   // ReturnBmp
    Cutting    : boolean;   // Ausschneiden aktiv?
    Ident      : string;    // Kennung
  end;

type
  TChangeEvent = procedure(aRect: TRect) of object;

type
  TFinishedEvent = procedure(aSelection: TSelection) of object;

type
  TPop = (pmShow,pmHide);

type

  { TSelector }

  TSelector = class(TCustomControl)
  private
   Label1     : TLabel;               //Für DialoWindow
   WidthEdit  : TLabeledEdit;
   HeightEdit : TLabeledEdit;
   Label2     : TLabel;
   CheckBox   : TCheckBox;
   RadioGroup : TRadioGroup;
   OkayButton : TButton;
   AbortButton: TButton;
   FSelArea           : TRect;        //Ausgewählter Bereich
   FBackgrd           : TRect;        //Koordinaten Hintergrund Ausschnitt
   FCuttingOut        : boolean;      //Hintergrund ausschneiden?
   FCuttingColor      : TColor;       //Farbe des Hintergrunds
   FPrimaryColor      : TColor;       //Untere Farbe des Auswahlrahmens
   FActiveColor       : TColor;       //Obere Farbe beim Ziehen des Rahmens
   FMarkerColor       : TColor;       //Obere Farbe des Auswahlrahmens wenn markiert
   FSquareColor       : TColor;       //Farbe der StretchQuadrate
   FEnable            : boolean;      //Auswahlrahmen ein/aus
   FOnChange          : TChangeEvent; //Größe hat sich geändert
   FOnFinished        : TFinishedEvent;//Selection abgeschlossen
   FTransparent       : boolean;      //Transparenz ein/aus
   FTransparentColor  : TColor;       //Farbe die Transparent gezeichnet wird
   FDemarcationColor  : TColor;       //Abgrenzungsfarbe für freies Kopieren

   //FLanguage          : TLanguage;    //Menü Sprache;
   FMenu              : TPopupMenu;   //PopupMenü
   FPop               : TPop;         //Auswahl zum Zeigen/Verstecken des Popup
   FItem0             : TMenuItem;    //kopieren
   FItem1             : TMenuItem;    //ausschneiden
   FItem2             : TMenuItem;    //einfügen
   FItem21            : TMenuItem;    //einpassen
   FItem3             : TMenuItem;    //löschen
   FItem4             : TMenuItem;    //Transparent
   FItem5             : TMenuItem;    //Transparentefarbe setzen
   FItem51            : TMenuItem;    //Transparentefarbe auswählen
   FItem52            : TMenuItem;    //Transparentefarbe scannen
   FItem6             : TMenuItem;    //Hintergrund löschen
   FItem7             : TMenuItem;    //Auswahloptionen
   FItem71            : TMenuItem;    //Alles Auswählen
   FItem72            : TMenuItem;    //Rechteckige Auswahl
   FItem73            : TMenuItem;    //Quadratische Auswahl
   FItem74            : TMenuItem;    //Formfreie Auswahl
   FItem8             : TMenuItem;    //Drehen
   FItem81            : TMenuItem;    //Rechts Drehen
   FItem82            : TMenuItem;    //Links Drehen
   FItem83            : TMenuItem;    //180° Drehen
   FItem84            : TMenuItem;    //Vertikal Spiegeln
   FItem85            : TMenuItem;    //Horizontal Spiegeln
   FItem9             : TMenuItem;    //Größe Ändern
   FItem10            : TMenuItem;    //als transparente PNG speichern
   FItem11            : TMenuItem;    //Abbrechen
   FItem12            : TMenuItem;    //Beenden


   SourceBmp          : TBitmap;      //Hintergrund für"Transparenz"
   ReturnBmp          : TBitmap;      //Rückgabe Bitmap
   LastBmp            : TBitmap;      //letztes Rückgabe Bitmap vor enable false

   Overlays           : OverlayList;  //Array von Ausgabe Bitmaps
   SelRec             : TSelection;   //Record der Auswahl
   idx                : integer;      //Anzahl Ausgabe Bitmaps
   idxx               : integer;      //Formfree beim letzten Mal einmal mehr ausgeben
   strg               : boolean;      //kopieren Tastatur ein
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
   modus              : byte;         //Modus markiert,aktiv
   square             : byte;         //Welches Quadrat angewählt
   moveX              : integer;      //Startpunkt beim Bewegen
   moveY              : integer;
   moveStart          : boolean;      //Bewegen gestartet
   moveRect           : TRect;        //Anfangskoordinaten
   StretchBmp         : TBitmap;      //von diesem Bitmap aus wird gestretcht
   stretchStart       : boolean;      //Größe ziehen
   escape             : boolean;      //Abbrechen
   ShowSelector       : boolean;      //Sichtbarkeit Auswahlrahmen ein
   ImageList1         : TImageList;   //Bilder für PopupMenü
   SelOpt             : TSelOpt;      //AuswahlOptionen Recht,Quadrat,Frei
   OldOption          :  TSelOpt;     //alte Auswahloption
   FormFreeAktiv      : boolean;      //Formfrei aktiv?
   DemarcationLine    : array of TPoint; //Grenzlinie
   Num                : integer;      //Anzahl der Punkte der Grenzlinie
   LastSelArea        : TRect;        //letzter Bereich vor enable false
   scanning           : boolean;      //Transparentefarbe scannen aktiv
   CustomLine         : array [0..3] of TCustomPen;   //für benuter definierte Linien


   procedure SetSelArea(ARect:TRect);
   procedure SetPrimaryColor(const aColor:TColor);
   procedure SetActiveColor(const aColor:TColor);
   procedure SetMarkerColor(const aColor:TColor);
   procedure SetSquareColor(const aColor:TColor);
   procedure SetEnable(const Value:boolean);
   procedure SetPop(const aValue:TPop);
   procedure SetCuttingOut(const Value:boolean);
   procedure SetCuttingColor(const aColor:TColor);
   procedure SetTransparent(const Value:boolean);
   procedure SetTransparentColor(const aColor:TColor);
   //procedure SetLanguage(aValue:TLanguage);
   procedure SetDemarcationColor(const aColor:TColor);

   procedure WidthEditChange(Sender: TObject);
   procedure HeightEditChange(Sender: TObject);
   procedure WHCklicked(Sender: TObject);
   procedure CheckBoxChanged(Sender: TObject);
   procedure RadioGroupChanged(Sender: TObject);
   procedure DialogWindowResult(Sender: TObject);
   procedure ChangeSize(const X,Y:integer;const {%H-}aSquare:byte);
   procedure CalculateSquares;
   procedure ActivePaint;
   procedure MarkedPaint;

  protected

   procedure KeyDown(var Key: Word; Shift: TShiftState);  override;
   procedure CNKeyDown    (var Message: TLMKeyDown);    message CN_KEYDOWN;
   procedure DoExit;  override;
   procedure DoEnter; override;
   procedure Loaded; override;
   procedure MouseMove({%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseDown(Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;
   procedure MouseUp({%H-}Button: TMouseButton;{%H-}Shift: TShiftState; X, Y: Integer);override;

  public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Paint; override;
   procedure LastBitmap(var aBmp:TBitmap);
   procedure Drawing(aCanvas:TCanvas);
   function SelAreaToString: string;
   function SelAreaLeftToString:string;
   function SelAreaRightToString:string;
   function SelAreaTopToString:string;
   function SelAreaBottomToString:string;
   function SelAreaWidthToString:string;
   function SelAreaHeightToString:string;
   procedure UpdateLocale;
//Eigenes PopupMenü
   procedure PopMCopy(Sender: TObject);
   procedure PopMCut(Sender: TObject);
   procedure PopMPaste(Sender: TObject);
   procedure PopMFitIn(Sender: TObject);
   procedure PopMDelete(Sender: TObject);
   procedure PopMTransparent(Sender: TObject);
   procedure PopMChooseTransCol(Sender: TObject);
   procedure PopMScanTransCol(Sender: TObject);
   procedure PopMSelectedAll(Sender: TObject);
   procedure PopMSelectedRect(Sender: TObject);
   procedure PopMSelectedSquare(Sender: TObject);
   procedure PopMSelectedFormFree(Sender: TObject);
   procedure PopMRotateRight(Sender: TObject);
   procedure PopMRotateLeft(Sender: TObject);
   procedure PopMRotate180(Sender: TObject);
   procedure PopMFlipHoriz(Sender: TObject);
   procedure PopMFlipVert(Sender: TObject);
   procedure PopMDeleteBackgrd(Sender: TObject);
   procedure PopMSetSize(Sender: TObject);
   procedure PopMSavePNG(Sender: TObject);
   procedure PopMAbort(Sender: TObject);
   procedure PopMFinished(Sender: TObject);


//Rechteckkoordinaten des markierten Bereichs
   property SelArea         : TRect   read FSelArea           write SetSelArea;
//Ein und ausschalten des Selectors
   property Enable          : boolean read FEnable            write SetEnable default false;
//Koordinaten Hintergrund Ausschnitt
   property Background      : TRect   read FBackgrd           write FBackgrd;
//diese Farbe wird transparent gezeichnet
   property TransparentColor: TColor  read FTransparentColor  write SetTransparentColor default clWhite;
//Farbe für Stift Formfrei
   property DemarcationColor: TColor  read FDemarcationColor  write SetDemarcationColor;
//Rückgabewerte für eine TObjectList (ähnlich assign)
   property RtrnBmp: TBitmap      read ReturnBmp;
   property DrawArea   : TRect    read  FSelArea     write FSelArea;
   property BkgrdColor : TColor   read FCuttingColor write FCuttingColor;
   property BkgrdArea  : TRect    read FBackgrd      write FBackgrd;
   property Cutting    : boolean  read FCuttingOut   write FCuttingOut;





  published
   //Ereignis wenn Rahmen verändert wird
   //Event when Selector is changed
   property OnChange        : TChangeEvent read FOnChange     write FOnChange;
   //Ereignis bei Fertigstellung der Auswahl
   //Event when Selector is finished
   property OnFinished : TFinishedEvent read FOnFinished write FOnFinished;
   //Untere Farbe des Auswahlrahmens
   //Lower color of the selection frame
   property PrimaryColor    : TColor  read FPrimaryColor      write SetPrimaryColor default clwhite;
   //Obere Farbe beim Ziehen des Rahmens
   //Upper color when pulling the frame
   property ActiveColor     : TColor  read FActiveColor       write SetActiveColor  default clred;
   //Obere Farbe des Auswahlrahmens wenn markiert
   //Upper color of the selection frame if selected
   property MarkerColor     : TColor  read FMarkerColor       write SetMarkerColor  default clnavy;
   //Farbe der Quadrate
   //Color of the squares
   property SquareColor     : TColor  read FSquareColor       write SetSquareColor  default clblue;
   //der Hintergrund wird mit der CuttingColor gezeichnet
   //the background is drawn with the CuttingColor
   property CuttingOut : boolean      read FCuttingOut      write SetCuttingOut default true;
   //Farbe des Ausschnittes
   //Color of the Cutout
   property CuttingColor : TColor     read FCuttingColor    write SetCuttingColor default clDefault;
   //Schaltet die Transparenz ein/aus
   //Turns transparency on / off
   property Transparent : boolean     read FTransparent     write SetTransparent default false;
   //Vorbelegtes PopupMenü
   //PrecastPopup
   property PrecastPopup    : TPop    read FPop               write SetPop          default pmHide;
   //fügt ein benutzerdefiniertes PopupMenü ein
   property PopupMenu;
   property Left default 30;
   property Top default 30;
   property Width default 35;
   property Height default 35;

  end;

procedure Register;

var DialogWindow : TForm;


implementation

//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx---TSELECTOR---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

constructor TSelector.Create(AOwner: TComponent);
var Cur    : TCursorImage;
    TmpBmp :TBitmap;
    lv     : integer;
begin
 inherited Create(AOwner);

 for lv:=0 to 3 do CustomLine[lv]  := TCustomPen.Create;


 Cur := TCursorImage.Create;
 Screen.Cursors[1] := LoadCursorFromLazarusResource('stift');
 Screen.Cursors[2] := LoadCursorFromLazarusResource('fadenkreuz');
 Cur.Free;
//Bilder für Popup
 ImageList1         := TImageList.Create(self);
 Imagelist1.AddLazarusResource('menu_view_units',clNone); //0
 Imagelist1.AddLazarusResource('laz_cut',clNone); //1
 Imagelist1.AddLazarusResource('laz_add',clNone); //2
 Imagelist1.AddLazarusResource('evaluate_up',clNone); //3 - 2.1
 Imagelist1.AddLazarusResource('laz_delete',clNone); //4 -3
 TmpBmp:=TBitmap.Create;
 TmpBmp.SetSize(16,16);
 TmpBmp.Canvas.Brush.Color:=clForm;
 TmpBmp.Canvas.Rectangle(0,0,16,16);
 Imagelist1.Add(TmpBmp,nil);  //Bitmap für Transparent 5 -5
 Imagelist1.AddLazarusResource('tcolordialog',clNone); //6 -5.1
 Imagelist1.AddLazarusResource('debugger_inspect',clNone); //7 -5.2
 Imagelist1.AddLazarusResource('pkg_design_overlay',clNone); //8 -6
 Imagelist1.AddLazarusResource('align',clNone); //9 -7.1
 Imagelist1.AddLazarusResource('laz_edit',clNone); //10 -7.4
 Imagelist1.AddLazarusResource('restore_default',clNone); //11 -8
 Imagelist1.AddLazarusResource('drehen_rechts',clNone); //12 -8.1
 Imagelist1.AddLazarusResource('drehen_links',clNone); //13 -8.2
 Imagelist1.AddLazarusResource('drehen_180',clNone); //14 -8.3
 Imagelist1.AddLazarusResource('horiz',clNone); //15 -8.4
 Imagelist1.AddLazarusResource('vert',clNone); //16 -8.5
 Imagelist1.AddLazarusResource('scale',clNone); //17 -9
 Imagelist1.AddLazarusResource('menu_project_save',clNone); //18 -10
 Imagelist1.AddLazarusResource('laz_cancel',clNone); //19 -11
 Imagelist1.AddLazarusResource('laz_tick',clNone); //20 -12

 width := 0;
 height:= 0;
 FSelArea.Left     := 0;
 FSelArea.Top      := 0;
 FSelArea.Right    := 0;
 FSelArea.Bottom   := 0;
 FPrimaryColor     := clwhite;
 FActiveColor      := clred;
 FMarkerColor      := clnavy;
 FSquareColor      := clblue;
 FEnable           := false;
 FPop              := pmHide;
 FCuttingOut       := true;
 FCuttingColor     := clForm;
 FTransparent      := false;
 //FLanguage         := de;
 FDemarcationColor := rgb(253,20,254);

 FMenu:=TPopupMenu.Create(Self);
 FMenu.Parent := self;
 FMenu.SetSubComponent(true);
 FMenu.Images:= ImageList1;

//Kopieren
 FItem0:=TMenuItem.Create(Self);
 FItem0.Caption := TransText0;
 FItem0.ShortCut:= Menus.ShortCut(vk_C,[ssCtrl]);
 FItem0.OnClick  :=@PopMCopy;
 FItem0.ImageIndex:=0;
 FMenu.Items.Add(FItem0);

//Ausschneiden
 FItem1:=TMenuItem.Create(Self);
 FItem1.Caption := TransText1;
 FItem1.ShortCut:= Menus.ShortCut(vk_X,[ssCtrl]);
 FItem1.OnClick  :=@PopMCut;
 FItem1.ImageIndex:=1;
 FMenu.Items.Add(FItem1);

//Einfügen
  FItem2:=TMenuItem.Create(Self);
  FItem2.Caption := TransText2;
  FItem2.ShortCut:= Menus.ShortCut(vk_V,[ssCtrl]);
  FItem2.OnClick  :=@PopMPaste;
  FItem2.ImageIndex:=2;
  FMenu.Items.Add(FItem2);

//Einpassen
  FItem21:=TMenuItem.Create(Self);
  FItem21.Caption := TransText21;
  FItem21.ShortCut:= Menus.ShortCut(vk_I,[ssCtrl]);
  FItem21.OnClick  :=@PopMFitIn;
  FItem21.ImageIndex:=3;
  FMenu.Items.Add(FItem21);

//Löschen
  FItem3:=TMenuItem.Create(Self);
  FItem3.Caption := TransText3;
  FItem3.ShortCut:= Menus.ShortCut(vk_Delete,[ssCtrl]);
  FItem3.OnClick  :=@PopMDelete;
  FItem3.ImageIndex:=4;
  FMenu.Items.Add(FItem3);

//Transparent
  FItem4:=TMenuItem.Create(Self);
  FItem4.Caption := TransText4;
  FItem4.ShortCut:= Menus.ShortCut(vk_T,[ssCtrl]);
  FItem4.OnClick  :=@PopMTransparent;
  FMenu.Items.Add(FItem4);

//Transparentefarbe setzen
  FItem5:=TMenuItem.Create(Self);
  FItem5.Caption := TransText5;
  FItem5.SetSubComponent(true);
  FMenu.Items.Add(FItem5);
  FItem5.ImageIndex:=5;
  TmpBmp.Free;

//Transparentefarbe Auswählen
  FItem51:=TMenuItem.Create(Self);
  FItem51.Caption := TransText51;
  FItem51.OnClick  :=@PopMChooseTransCol;
  FItem51.ImageIndex:=6;
  FItem5.Add(FItem51);

//Transparentefarbe scannen
  FItem52:=TMenuItem.Create(Self);
  FItem52.Caption := TransText52;
  FItem52.OnClick  :=@PopMScanTransCol;
  FItem52.ImageIndex:=7;
  FItem5.Add(FItem52);

//Hintergrund löschen
  FItem6:=TMenuItem.Create(Self);
  FItem6.Caption := TransText6;
  FItem6.Checked:=true;
  FItem6.OnClick  :=@PopMDeleteBackgrd;
  FMenu.Items.Add(FItem6);


//Auswahloptionen
  FItem7:=TMenuItem.Create(Self);
  FItem7.Caption := TransText7;
  FItem7.SetSubComponent(true);
  FItem7.ImageIndex:=8;
  FMenu.Items.Add(FItem7);

//Alles Auswählen
  FItem71:=TMenuItem.Create(Self);
  FItem71.Caption := TransText71;
  FItem71.ShortCut:= Menus.ShortCut(vk_A,[ssCtrl]);
  FItem71.OnClick  :=@PopMSelectedAll;
  FItem71.ImageIndex:=9;
  FItem7.Add(FItem71);

//Rechteckige Auswahl
  FItem72:=TMenuItem.Create(Self);
  FItem72.Caption := TransText72;
  FItem72.OnClick  :=@PopMSelectedRect;
  FItem72.Checked:=true;
  FItem7.Add(FItem72);

//Quadratische Auswahl
  FItem73:=TMenuItem.Create(Self);
  FItem73.Caption := TransText73;
  FItem73.OnClick  :=@PopMSelectedSquare;
  FItem73.SetSubComponent(true);
  FItem7.Add(FItem73);

//Formfreie Auswahl
  FItem74:=TMenuItem.Create(Self);
  FItem74.Caption := TransText74;
  FItem74.OnClick  :=@PopMSelectedFormFree;
  FItem74.ImageIndex:=10;
  FItem7.Add(FItem74);

//Drehen
  FItem8:=TMenuItem.Create(Self);
  FItem8.Caption := TransText8;
  FItem8.SetSubComponent(true);
  FItem8.ImageIndex:=11;
  FMenu.Items.Add(FItem8);

//Rechts Drehen
  FItem81:=TMenuItem.Create(Self);
  FItem81.Caption := TransText81;
  FItem81.OnClick  :=@PopMRotateRight;
  FItem81.ImageIndex:=12;
  FItem8.Add(FItem81);

//Links Drehen
  FItem82:=TMenuItem.Create(Self);
  FItem82.Caption := TransText82;
  FItem82.OnClick  :=@PopMRotateLeft;
  FItem82.ImageIndex:=13;
  FItem8.Add(FItem82);

//180° Drehen
  FItem83:=TMenuItem.Create(Self);
  FItem83.Caption := TransText83;
  FItem83.OnClick  :=@PopMRotate180;
  FItem83.ImageIndex:=14;
  FItem8.Add(FItem83);

//Horizontal Spiegeln
  FItem84:=TMenuItem.Create(Self);
  FItem84.Caption := TransText84;
  FItem84.OnClick  :=@PopMFlipHoriz;
  FItem84.ImageIndex:=16;
  FItem8.Add(FItem84);

//Vertikal Spiegeln
  FItem85:=TMenuItem.Create(Self);
  FItem85.Caption := TransText85;
  FItem85.OnClick  :=@PopMFlipVert;
  FItem85.ImageIndex:=15;
  FItem8.Add(FItem85);

//Größe ändern
  FItem9:=TMenuItem.Create(Self);
  FItem9.Caption := TransText9;
  FItem9.OnClick  :=@PopMSetSize;
  FItem9.ImageIndex:=17;
  FMenu.Items.Add(FItem9);

//Transparent PNG
  FItem10:=TMenuItem.Create(Self);
  FItem10.Caption := TransText10;
  FItem10.ShortCut:= Menus.ShortCut(vk_S,[ssCtrl]);
  FItem10.OnClick  :=@PopMSavePNG;
  FItem10.ImageIndex:=18;
  FMenu.Items.Add(FItem10);

//Abbrechen
  FItem11:=TMenuItem.Create(Self);
  FItem11.Caption := TransText11;
  FItem11.ShortCut:= Menus.ShortCut(vk_ESCAPE,[]);
  FItem11.OnClick  :=@PopMAbort;
  FItem11.ImageIndex:=19;
  FMenu.Items.Add(FItem11);

//Fertigstellen
  FItem12:=TMenuItem.Create(Self);
  FItem12.Caption := TransText12;
  FItem12.ShortCut:= Menus.ShortCut(vk_Return,[]);
  FItem12.OnClick  :=@PopMFinished;
  FItem12.ImageIndex:=20;
  FMenu.Items.Add(FItem12);

 ShowSelector  := false;
 idx           := -1;
 SelOpt        := Rectangle;

 SourceBmp                  := TBitmap.Create;
 ReturnBmp                  := TBitmap.Create;
 ReturnBmp.Transparent      := false;
 StretchBmp                 := TBitmap.Create;
 LastBmp                    := TBitmap.Create;
 LastBmp.Assign(nil);
end;

destructor TSelector.Destroy;
var lv : integer;
begin
 SourceBmp.Free;
 ImageList1.Free;
 ReturnBmp.Free;
 StretchBmp.Free;
 LastBmp.Free;
 for lv:= 0 to idx do Overlays[lv].RtrnBmp.Free;
 for lv:= 0 to 3 do CustomLine[lv].Free;
 inherited Destroy;
end;
//XXXXXXXXXXXXXXXXXXXXXXXXXXX---AB HIER TASTEN---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TSelector.KeyDown(var Key: Word; Shift: TShiftState);
var   SMessage : TLMKey;
begin

 if enable then
  if (GetKeyState(VK_CONTROL) < 0) then  //mit kopieren
      begin
       CuttingOut:=false;
       strg:=true; //showmessage('in');
      end;
  begin
   if Key = VK_LEFT then    //bewegen links
    begin
     FSelArea.Left := FSelArea.Left-1;
     FSelArea.Right:= FSelArea.Right-1;
     Invalidate; //showmessage('');
    end;
   if Key = VK_RIGHT then    //bewegen rechts
    begin
     FSelArea.Left := FSelArea.Left+1;
     FSelArea.Right:= FSelArea.Right+1;
     Invalidate;
    end;
   if Key = VK_UP then     //bewegen oben
    begin
     FSelArea.Top   := FSelArea.Top-1;
     FSelArea.Bottom:= FSelArea.Bottom-1;
     Invalidate;
    end;
   if Key = VK_DOWN then    //bewegen unten
    begin
     FSelArea.Top   := FSelArea.Top+1;
     FSelArea.Bottom:= FSelArea.Bottom+1;
     Invalidate;
    end;
   if Key = VK_ESCAPE then    //Abbrechen
    begin
     Escape:=true;
     Enable:=false;
    end;
   if Key = VK_RETURN then   //Übernehmen
    begin
     Enable:=false;
    end;

   if FPop = pmShow then
    begin
     SMessage.CharCode:=key;     //Shortcuts abfragen
     FMenu.IsShortcut(SMessage);
    end;//pmShow

 end;//enable
 inherited;
end;

procedure TSelector.CNKeyDown(var Message: TLMKeyDown);
begin
  with Message do begin
    Result := 1;
    case CharCode of  //Tasten abfangen damit der Fokus nicht weitergegeben wird!
        VK_LEFT  : ;
        VK_RIGHT : ;
        VK_UP    : ;
        VK_DOWN  : ;
    else
      begin
        Result := 0;
      end; //else
    end //case
  end; //with
  inherited;
end;

procedure TSelector.DoEnter;
begin
  invalidate;
  inherited;
end;

procedure TSelector.DoExit;
begin
  invalidate;
  inherited;
end;

procedure TSelector.Loaded;
var TmpBmp : TBitmap;
begin
 inherited Loaded;
//setzt erstmal die Formfarbe des Parent als TransparentColor
 if parent.Color = cldefault then FTransparentColor:= clForm
 else FTransparentColor:=parent.Color;
 ReturnBmp.TransparentColor := FTransparentColor;

 self.Hide;

 TmpBmp:=TBitmap.Create;
 TmpBmp.SetSize(16,16);
 TmpBmp.Canvas.Brush.Color:=FTransparentColor;
 TmpBmp.Canvas.Rectangle(0,0,16,16);
 Imagelist1.Replace(5,TmpBmp,nil);  //Bitmap für Transparent 5 -5
 TmpBmp.Free;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXX---AB HIER SETTER---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TSelector.SetSelArea(ARect:TRect);
begin
 if FSelArea = ARect then exit;
 FSelArea := ARect;
 width    := FSelArea.Width+4;
 height   := FSelArea.Height+4;
 Invalidate;
end;

procedure TSelector.SetPrimaryColor(const aColor:TColor);
begin
 if FPrimaryColor = aColor then exit;
 FPrimaryColor   := aColor;
 Invalidate;
end;

procedure TSelector.SetActiveColor(const aColor:TColor);
begin
 if FActiveColor = aColor then exit;
 FActiveColor   := aColor;
 Invalidate;
end;

procedure TSelector.SetMarkerColor(const aColor:TColor);
begin
 if FMarkerColor = aColor then exit;
 FMarkerColor   := aColor;
 Invalidate;
end;

procedure TSelector.SetSquareColor(const aColor:TColor);
begin
 if FSquareColor = aColor then exit;
 FSquareColor   := aColor;
 Invalidate;
end;

procedure TSelector.SetEnable(const Value:boolean);
begin
 if FEnable = Value then exit;
 FEnable   := Value;
 if FEnable then
  begin
   FSelArea.Left   := -100; //Rahmen ausblenden
   FSelArea.Top    := -100;
   FSelArea.Right  := -100;
   FSelArea.Bottom := -100;
   FBackgrd:=FSelArea;
   self.Show;
   left  :=0;
   top   :=0;
   width :=Parent.Width;
   height:=Parent.Height;
   SourceBmp.SetSize(Width,Height);

   {$IFDEF LINUX}
    parent.PaintTo(SourceBmp.Canvas.Handle,0,0);
   {$ENDIF}
   {$IFDEF WINDOWS}
    try
     SourceBmp.Canvas.CopyRect(Rect(0,0,width,height),(Parent as TCustomControl).Canvas,Rect(0,0,width,height));
    except
     showmessage('Please choose a parent with the ancestor TCustomControl');
    end;
   {$ENDIF}
   self.SendToBack;
   ReturnBmp.Assign(nil);
   escape:=false;
  end else
  begin
   if not escape then
    begin
     //array füllen
     inc(idx);
     SetLength(Overlays,idx+1);
     Overlays[idx].RtrnBmp := TBitmap.Create;
     Overlays[idx].DrawArea  := FSelArea;
     Overlays[idx].RtrnBmp.Assign(ReturnBmp);
     Overlays[idx].bkgrdColor:= FCuttingColor;
     Overlays[idx].bkgrdRect := FBackgrd;

     if FCuttingOut then Overlays[idx].Cutting:=true
     else Overlays[idx].Cutting:=false;

     SelRec.RtrnBmp    := ReturnBmp;
     SelRec.DrawArea   := FSelArea;
     SelRec.BkgrdColor := FCuttingColor;
     SelRec.BkgrdArea  := FBackgrd;
     SelRec.Cutting    := Overlays[idx].Cutting;
     SelRec.Ident      := 'Selection';
     if Assigned(OnFinished) then OnFinished(SelRec);
    end;//not escape
//Hintergrund kopieren mit strg wieder aus
   if strg then
    begin
     CuttingOut:=true;
     strg:=false;
    end;
   LastSelArea:=FSelArea;
   LastBmp.Assign(ReturnBmp);
//alles ausblenden
   self.Hide;

  end;
 Invalidate;
end;

procedure TSelector.SetPop(const aValue:TPop);
begin
 if FPop = aValue then exit;
 FPop   := aValue;
end;

procedure TSelector.SetCuttingOut(const Value:boolean);
begin
 if FCuttingOut = Value then exit;
 FCuttingOut   := Value;
 if FCuttingOut then FItem6.Checked:=true else FItem6.Checked:=false;
 Invalidate;
end;

procedure TSelector.SetCuttingColor(const aColor:TColor);
begin
 if FCuttingColor = aColor then exit;
 FCuttingColor   := aColor;
 Invalidate;
end;

procedure TSelector.SetTransparent(const Value: boolean);
begin
 if FTransparent = Value then exit;
 FTransparent   := Value;
 ReturnBmp.Transparent:=FTransparent;
 if FTransparent then FItem4.Checked:=true else FItem4.Checked:=false;
 Invalidate;
end;

procedure TSelector.SetTransparentColor(const aColor: TColor);
var TmpBmp : TBitmap;
begin
 if FTransparentColor = aColor then exit;
 FTransparentColor   := aColor;
 ReturnBmp.TransparentColor:= FTransparentColor;
 TmpBmp:=TBitmap.Create;   //für Menü
 TmpBmp.SetSize(16,16);
 TmpBmp.Canvas.Brush.Color:=FTransparentColor;
 TmpBmp.Canvas.Rectangle(0,0,16,16);
 Imagelist1.Replace(5,TmpBmp,nil);  //Bitmap für Transparent 5 -5
 TmpBmp.Free;
 Invalidate;
end;

procedure TSelector.SetDemarcationColor(const aColor: TColor);
begin
 if FDemarcationColor = aColor then exit;
 FDemarcationColor   := aColor;
 Invalidate;
end;

procedure TSelector.WidthEditChange(Sender: TObject);
var fak : double;
    i   : integer;
begin
 if CheckBox.Checked then
  begin
   try
    fak:=FSelArea.Width/FSelArea.Height;
    i:=strtoint(WidthEdit.Caption);
    HeightEdit.Caption:= inttostr(round(i/fak));
   except
    WidthEdit.Caption:='';
    HeightEdit.Caption:='';
   end;
  end;
end;

procedure TSelector.HeightEditChange(Sender: TObject);
var fak : double;
    i   : integer;
begin
 if CheckBox.Checked then
  begin
   try
    fak:=FSelArea.Width/FSelArea.Height;
    i:=strtoint(HeightEdit.Caption);
    WidthEdit.Caption:= inttostr(round(i*fak));
   except
    WidthEdit.Caption:='';
    HeightEdit.Caption:='';
   end;
  end;
end;

procedure TSelector.WHCklicked(Sender: TObject);
begin
 RadioGroup.ItemIndex:=-1;
end;

procedure TSelector.CheckBoxChanged(Sender: TObject);
begin
 WidthEdit.Caption:='';
 HeightEdit.Caption:='';
end;

procedure TSelector.RadioGroupChanged(Sender: TObject);
begin
 WidthEdit.Caption:='';
 HeightEdit.Caption:='';
end;

procedure TSelector.DialogWindowResult(Sender: TObject);
var w,h    : integer;
    tmpBmp : TBitmap;
begin
//Abbrechen
 if sender = AbortButton then
  begin
   DialogWindow.Close;
  end;
//OKAY
 if sender= OkayButton then
  begin
   if RadioGroup.ItemIndex = 0 then
    begin
     parent.Width :=FSelArea.Width;
     parent.Height:=FSelArea.Height;
     FSelArea.Left:=0;
     FSelArea.Top :=0;
     FSelArea.Width :=parent.Width;
     FSelArea.Height:=parent.Height;
     DialogWindow.Close;
     enable:= false;
    end;//Zuschneiden
   if RadioGroup.ItemIndex = 1 then
    begin
     FSelArea.Width:=16;
     FSelArea.Height:=16;
     tmpbmp := TBitmap.Create;
     tmpbmp.Assign(ReturnBmp);
     ReturnBmp.SetSize(16,16);
     ReturnBmp.Canvas.StretchDraw(rect(0,0,16,16),tmpBmp);
     ReturnBmp.Transparent:=FTransparent;
     ReturnBmp.TransparentColor:=FTransparentColor;
     tmpbmp.Free;
     DialogWindow.Close;
     exit;
    end;//16x16
   if RadioGroup.ItemIndex = 2 then
    begin
     FSelArea.Width:=24;
     FSelArea.Height:=24;
     tmpbmp := TBitmap.Create;
     tmpbmp.Assign(ReturnBmp);
     ReturnBmp.SetSize(24,24);
     ReturnBmp.Canvas.StretchDraw(rect(0,0,24,24),tmpBmp);
     ReturnBmp.Transparent:=FTransparent;
     ReturnBmp.TransparentColor:=FTransparentColor;
     tmpbmp.Free;
     DialogWindow.Close;
     exit;
    end;//24x24
   if RadioGroup.ItemIndex = 3 then
    begin
     FSelArea.Width:=32;
     FSelArea.Height:=32;
     tmpbmp := TBitmap.Create;
     tmpbmp.Assign(ReturnBmp);
     ReturnBmp.SetSize(32,32);
     ReturnBmp.Canvas.StretchDraw(rect(0,0,32,32),tmpBmp);
     ReturnBmp.Transparent:=FTransparent;
     ReturnBmp.TransparentColor:=FTransparentColor;
     tmpbmp.Free;
     DialogWindow.Close;
     exit;
    end;//32x32

   if WidthEdit.Caption <> '' then
    begin
     try
      w:=strtoint(WidthEdit.Caption);
     except
      WidthEdit.Caption:='';
      exit;
     end;
    end;//WidthEdit
   if HeightEdit.Caption <> '' then
    begin
     try
      h:=strtoint(HeightEdit.Caption);
      if WidthEdit.Caption <> ''then
       begin
        FSelArea.Width:=w;
        FSelArea.Height:=h;
        tmpbmp := TBitmap.Create;
        tmpbmp.Assign(ReturnBmp);
        ReturnBmp.SetSize(w,h);
        ReturnBmp.Canvas.StretchDraw(rect(0,0,FSelArea.Width,FSelArea.Height),tmpBmp);
        ReturnBmp.Transparent:=FTransparent;
        ReturnBmp.TransparentColor:=FTransparentColor;
        tmpbmp.Free;
        DialogWindow.Close;
        exit;
       end;
     except
      HeightEdit.Caption:='';
      exit;
     end;
    end;//HeightEdit
   if (HeightEdit.Caption='') or (WidthEdit.Caption='') then exit;
  end;//okay
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---ENDE SET PROCEDUREN---XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---AB HIER POPUPMENÜ---XXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TSelector.PopMCopy(Sender: TObject); //kopieren
var PNG    :TPortableNetworkGraphic;
    PNGMs     :TMemoryStream;
    BMPMs     :TMemoryStream;
begin
  try
    Png := TPortableNetworkGraphic.Create;
    Png.Assign(ReturnBmp);
    if self.Transparent then
     begin
      BmpToTransparentPNG(png,ReturnBmp,FTransparentColor);
     end;
    BmpMs := TMemoryStream.Create;
    ReturnBmp.SaveToStream(BMPMs);
    Clipboard.AddFormat(ClipboardRegisterFormat(ReturnBmp.MimeType), BMPMs);
    PNGMs := TMemoryStream.Create;
    Png.SaveToStream(PNGMs);
    Clipboard.AddFormat(ClipboardRegisterFormat(Png.MimeType), PNGMs);
    Enable:=false;
  finally
    Png.Free;
    BMPMs.Free;
    PNGMs.Free;
  end;

end;

procedure TSelector.PopMCut(Sender: TObject); //ausschneiden
var PNG    :TPortableNetworkGraphic;
    PNGMs     :TMemoryStream;
    BMPMs     :TMemoryStream;
begin
 (*if not self.Transparent then
  begin
   try
    PNG:=TPortableNetworkGraphic.Create;
    Png.Assign(ReturnBmp);
    ClipBoard.Assign(png);//ReturnBmp);
    ReturnBmp.Canvas.Brush.Color := FCuttingColor;
    ReturnBmp.Canvas.Pen.Color   := FCuttingColor;
    ReturnBmp.Canvas.Rectangle(0,0,ReturnBmp.Width,ReturnBmp.Height);
    invalidate;
    Enable:=false;
   finally
       PNG.Free;
   end;
  end else
  begin
   try
    PNG:=TPortableNetworkGraphic.Create;
    BmpToTransparentPNG(png,ReturnBmp,FTransparentColor);
    ClipBoard.Assign(png);
    ReturnBmp.Canvas.Brush.Color := FCuttingColor;
    ReturnBmp.Canvas.Pen.Color   := FCuttingColor;
    ReturnBmp.Canvas.Rectangle(0,0,ReturnBmp.Width,ReturnBmp.Height);
    invalidate;
    Enable:=false;
   finally
    png.Free;
   end;
  end; *)
  try
    Png := TPortableNetworkGraphic.Create;
    Png.Assign(ReturnBmp);
    if self.Transparent then
     begin
      BmpToTransparentPNG(png,ReturnBmp,FTransparentColor);
     end;
    BmpMs := TMemoryStream.Create;
    ReturnBmp.SaveToStream(BMPMs);
    Clipboard.AddFormat(ClipboardRegisterFormat(ReturnBmp.MimeType), BMPMs);
    PNGMs := TMemoryStream.Create;
    Png.SaveToStream(PNGMs);
    Clipboard.AddFormat(ClipboardRegisterFormat(Png.MimeType), PNGMs);
    ReturnBmp.Canvas.Brush.Color := FCuttingColor;
    ReturnBmp.Canvas.Pen.Color   := FCuttingColor;
    ReturnBmp.Canvas.Rectangle(0,0,ReturnBmp.Width,ReturnBmp.Height);
    invalidate;
    Enable:=false;
  finally
    Png.Free;
    BMPMs.Free;
    PNGMs.Free;
  end;
end;

procedure TSelector.PopMPaste(Sender: TObject); //einfügen
var PNG    : TPortableNetworkGraphic;
    TmpBmp : TBitmap;
    aColor : TColor;
begin
 if clipboard.HasFormat(PredefinedClipboardFormat(pcfPicture))
  then begin
   try
    PNG:=TPortableNetworkGraphic.Create;
    PNG.Assign(ClipBoard);
    if IfPngTransparent(Png) then
     begin
      TransPngToTransBmp(Png,ReturnBmp,aColor{%H-});
      self.TransparentColor:=aColor;
      self.Transparent:=true;
      self.CuttingOut:= false;
     end
     else
      begin
       ReturnBmp.Assign(PNG);
       self.Transparent:=false;
      end;
     FSelArea.Width := ReturnBmp.Width;     //die Größe anpassen
     FSelArea.Height:= ReturnBmp.Height;
     FSelArea:=rect(FSelArea.Left,FSelArea.Top,FSelArea.Right,FSelArea.Bottom);
     Invalidate;
     exit;
   finally
    PNG.Free;
   end;
  end;

  if clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
   begin
    try
     TmpBmp := TBitmap.Create;
     TmpBmp.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
     ReturnBmp.SetSize(TmpBmp.Width,TmpBmp.Height);
     ReturnBmp.Canvas.Draw(0,0,TmpBmp);
     if FTransparent then
      begin
       ReturnBmp.Transparent:=false;
       ReturnBmp.Transparent:=true;
       ReturnBmp.TransparentColor:=FTransparentColor;
       Transparent:= false;
       Transparent:= true;
      end;
     FSelArea.Width := ReturnBmp.Width;     //die Größe anpassen
     FSelArea.Height:= ReturnBmp.Height;
     FSelArea:=rect(FSelArea.Left,FSelArea.Top,FSelArea.Right,FSelArea.Bottom);
    finally
     TmpBmp.Free;
    end;
    Invalidate;
    exit;
   end;
end;

procedure TSelector.PopMFitIn(Sender: TObject);//einpassen
var PNG      : TPortableNetworkGraphic;
    TmpBmp   : TBitmap;
    aColor   : TColor;

begin
 if clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
   begin
    try
     TmpBmp := TBitmap.Create;
     StretchBmp.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
     StretchBmp.Transparent:=false;
     TmpBmp.SetSize(StretchBmp.Width,StretchBmp.Height);
     ReturnBmp.TransparentColor  :=rgb(33,44,55);
     TmpBmp.Assign(StretchBmp);
     ReturnBmp.Canvas.StretchDraw(Rect(0,0,ReturnBmp.Width,ReturnBmp.Height),TmpBmp);
     Invalidate;
     if FTransparent then
      begin
       ReturnBmp.Transparent:=false;
       ReturnBmp.Transparent:=true;
       ReturnBmp.TransparentColor:=FTransparentColor;
       Transparent:= false;
       Transparent:= true;
      end;
    finally
     TmpBmp.Free;
    end;
    Invalidate;
    exit;
   end;

 if clipboard.HasFormat(PredefinedClipboardFormat(pcfPicture))
  then begin
   try
    PNG:=TPortableNetworkGraphic.Create;
    PNG.Assign(ClipBoard);
    TmpBmp := TBitmap.Create;
    TmpBmp.SetSize(PNG.Width,PNG.Height);
    ReturnBmp.TransparentColor  :=rgb(33,44,55);
    if IfPngTransparent(PNG) then
     begin
      TransPngToTransBmp(PNG,TmpBmp,aColor{%H-});
      ReturnBmp.Canvas.StretchDraw(Rect(0,0,ReturnBmp.Width,ReturnBmp.Height),TmpBmp);
      self.Transparent:=false;
      self.TransparentColor:=aColor;
      self.Transparent:=true;
      ReturnBmp.TransparentColor:=aColor;
     end
     else
      begin
       TmpBmp.Assign(PNG);
       ReturnBmp.Canvas.StretchDraw(Rect(0,0,ReturnBmp.Width,ReturnBmp.Height),TmpBmp);
       self.Transparent:=false;
      end;
     Invalidate;
   finally
    PNG.Free;
    TmpBmp.Free;
   end;
  end;
end;

procedure TSelector.PopMDelete(Sender: TObject); //löschen
var t : boolean;
begin
 t:=false;
 if Transparent then t:=true;
 Transparent:=false;
 ReturnBmp.Canvas.Brush.Color := FCuttingColor;
 ReturnBmp.Canvas.Pen.Color   := FCuttingColor;
 ReturnBmp.Canvas.Rectangle(0,0,ReturnBmp.Width,ReturnBmp.Height);
 Enable:=false;
 if t then Transparent:=true;
end;

procedure TSelector.PopMTransparent(Sender: TObject); //Transparent
begin
 if Transparent then Transparent:=false else Transparent:=true;
 if Transparent then FItem4.Checked:=true else FItem4.Checked:=false;
end;

procedure TSelector.PopMChooseTransCol(Sender: TObject);//Transparentefarbe auswählen
var ColorDialog1 : TColorDialog;
begin
  try
   ColorDialog1 := TColorDialog.Create(self);
   if ColorDialog1.Execute then
   TransparentColor := ColorDialog1.Color;
   Transparent:=false;
   Transparent:= true;
  finally
   ColorDialog1.Free;
  end;
end;

procedure TSelector.PopMScanTransCol(Sender: TObject);//Transparentefarbe scannen
begin
 Cursor:=2;
 scanning:=true;
end;

procedure TSelector.PopMSelectedAll(Sender: TObject); //Alles Auswählen
begin
 FSelArea.SetLocation(0,0);
 FSelArea.width:=parent.Width;
 FSelArea.height:=parent.Height;
 ReturnBmp.Assign(SourceBmp);
 StretchBmp.Assign(ReturnBmp);
 StretchBmp.Transparent:=false;
 Invalidate;
end;

procedure TSelector.PopMSelectedRect(Sender: TObject);  //Rechteckige Auswahl
begin
 SelOpt := Rectangle;
 FItem72.Checked:=true;
 FItem73.Checked:=false;
 FItem74.Checked:=false;
 Escape:=true;
 Enable:=false;
end;

procedure TSelector.PopMSelectedSquare(Sender: TObject); //Quadratische Auswahl
begin
 SelOpt := Quad;
 FItem72.Checked:=false;
 FItem73.Checked:=true;
 FItem74.Checked:=false;
 Escape:=true;
 Enable:=false;
end;

procedure TSelector.PopMSelectedFormFree(Sender: TObject);  //Freie Auswahl
var MP : TPoint;
begin
 OldOption:= SelOpt;
 SelOpt := FormFree;
 FItem72.Checked:=false;
 FItem73.Checked:=false;
 FItem74.Checked:=true;
 MP.x:= FSelArea.Left+round(FSelArea.Width/2);
 MP.y:= FSelArea.Top+round(FSelArea.Height/2);
 mouse.CursorPos:= ClientToScreen(MP);
 idxx:=0;
end;

procedure TSelector.PopMRotateRight(Sender: TObject);  //rechts drehen
var swap : integer;
begin
 ReturnBmp.Assign(RotateRight(ReturnBmp));
 ReturnBmp.Transparent:=FTransparent;
 ReturnBmp.TransparentColor:=FTransparentColor;
 swap            := FSelArea.Width;
 FSelArea.Width  := FSelArea.Height;
 FSelArea.Height := swap;
 invalidate;
end;

procedure TSelector.PopMRotateLeft(Sender: TObject); //links drehen
var swap : integer;
begin
 ReturnBmp.Assign(RotateLeft(ReturnBmp));
 ReturnBmp.Transparent:=FTransparent;
 ReturnBmp.TransparentColor:=FTransparentColor;
 swap            := FSelArea.Width;
 FSelArea.Width  := FSelArea.Height;
 FSelArea.Height := swap;
 invalidate;
end;

procedure TSelector.PopMRotate180(Sender: TObject);  //180° drehen
begin
 ReturnBmp.Assign(Rotate180(ReturnBmp));
 ReturnBmp.Transparent:=FTransparent;
 ReturnBmp.TransparentColor:=FTransparentColor;
 invalidate;
end;


procedure TSelector.PopMFlipHoriz(Sender: TObject);  //Horizontal Spiegeln
begin
 ReturnBmp.Assign(FlipHorizontal(ReturnBmp));
 ReturnBmp.Transparent:=FTransparent;
 ReturnBmp.TransparentColor:=FTransparentColor;
 invalidate;
end;

procedure TSelector.PopMFlipVert(Sender: TObject); //Vertikal Spiegeln
begin
 ReturnBmp.Assign(FlipVertical(ReturnBmp));
 ReturnBmp.Transparent:=FTransparent;
 ReturnBmp.TransparentColor:=FTransparentColor;
 invalidate;
end;

procedure TSelector.PopMDeleteBackgrd(Sender: TObject);
begin
  if FCuttingOut then CuttingOut:=false else CuttingOut:=true;
end;

procedure TSelector.PopMSetSize(Sender: TObject); //Größe ändern
begin
 try
   DialogWindow           := TForm.Create(self);
   DialogWindow.SetBounds(parent.Left+50,parent.Top+50,320,320);
   DialogWindow.Caption   := TransText91+
   self.SelAreaWidthToString+'/'+self.SelAreaHeightToString;
   DialogWindow.Color     := clForm;

   Label1              := TLabel.Create(self);
   Label1.Parent       := DialogWindow;
   Label1.Left         := 10;
   Label1.Top          := 10;
   Label1.AutoSize     := true;
   Label1.Color        := clForm;
   Label1.Font.Color   := clMenuText;
   Label1.Caption      := TransText92;

   WidthEdit           := TLabeledEdit.Create(self);
   WidthEdit.Parent    := DialogWindow;
   WidthEdit.AutoSize  := true;
   WidthEdit.Left      := 10;
   WidthEdit.Top       := 55;
   WidthEdit.Alignment := taRightJustify;
   WidthEdit.LabelPosition        := lpAbove;
   WidthEdit.EditLabel.Font.Color := clMenuText;
   WidthEdit.EditLabel.Caption    := TransText93;
   WidthEdit.OnChange  := @WidthEditChange;
   WidthEdit.OnClick   := @WHCklicked;

   HeightEdit           := TLabeledEdit.Create(self);
   HeightEdit.Parent    := DialogWindow;
   HeightEdit.AutoSize  := true;
   HeightEdit.Left      := WidthEdit.Left+WidthEdit.Width+40;
   HeightEdit.Top       := 55;
   HeightEdit.Alignment := taRightJustify;
   HeightEdit.LabelPosition        := lpAbove;
   HeightEdit.EditLabel.Font.Color := clMenuText;
   HeightEdit.EditLabel.Caption    := TransText94;
   HeightEdit.OnChange  := @HeightEditChange;
   HeightEdit.OnClick   := @WHCklicked;

   CheckBox            := TCheckBox.Create(self);
   CheckBox.Parent     := DialogWindow;
   CheckBox.Left       := WidthEdit.Left;
   CheckBox.Top        := 100;
   CheckBox.Checked    := true;
   CheckBox.OnChange   := @CheckBoxChanged;

   Label2              := TLabel.Create(self);
   Label2.Parent       := DialogWindow;
   Label2.Left         := CheckBox.Left+30;
   Label2.AutoSize     := true;
   Label2.Color        := clForm;
   Label2.Font.Color   := clMenuText;
   Label2.Caption      := TransText95;
   Label2.Top          := 105;

   RadioGroup          := TRadioGroup.Create(self);
   RadioGroup.Parent   := DialogWindow;
   RadioGroup.SetBounds(10,140,200,120);
   RadioGroup.Caption:=TransText96;
   RadioGroup.Items.Add(TransText97);
   RadioGroup.Items.Add('16x16');
   RadioGroup.Items.Add('24x24');
   RadioGroup.Items.Add('32x32');
   RadioGroup.OnSelectionChanged:= @RadioGroupChanged;

   OkayButton          := TButton.Create(self);
   OkayButton.Parent   := DialogWindow;
   OkayButton.SetBounds(10,280,125,25);
   OkayButton.Caption  := 'Okay';
   OkayButton.OnClick  := @DialogWindowResult;

   AbortButton          := TButton.Create(self);
   AbortButton.Parent   := DialogWindow;
   AbortButton.SetBounds(185,280,125,25);
   AbortButton.Caption  := TransText11;
   AbortButton.OnClick  := @DialogWindowResult;

   DialogWindow.ShowModal;
 finally
   CheckBox.Free;
   Label2.Free;
   WidthEdit.Free;
   HeightEdit.Free;
   Label1.Free;
   RadioGroup.Free;
   OkayButton.Free;
   AbortButton.Free;
   DialogWindow.Free;
 end;
end;

procedure TSelector.PopMSavePNG(Sender: TObject); //als transparent PNG
var SaveDialog     : TSaveDialog;
    png            : TPortableNetworkGraphic;
begin
 png       := TPortableNetworkGraphic.Create;
 BmpToTransparentPNG(png,ReturnBmp,FTransparentColor);
 try
  SaveDialog := TSaveDialog.Create(self);
  SaveDialog.Filename := 'Picture1.png';
  SaveDialog.Filter := '*.png';
  SaveDialog.DefaultExt := 'png';
  SaveDialog.Options := [ofOverwritePrompt];

  if SaveDialog.Execute then
  png.SaveToFile(SaveDialog.FileName);
 finally
  SaveDialog.Free;
  png.Free;
 end;//finally
 enable:=false;
end;

procedure TSelector.PopMAbort(Sender: TObject);  //Abbrechen
begin
 Escape:=true;
 Enable:=false;
end;

procedure TSelector.PopMFinished(Sender: TObject); //Fertigstellen
begin
 Enable:=false;
end;



//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---ENDE POPUPMENÜ---XXXXXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TSelector.MouseDown(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var P1 : TPoint;
    TmpBmp : TBitmap;
begin
 setfocus;
 if Button=mbRight then
  begin
   if FPop = pmShow then FMenu.PopUp else
   if not assigned(PopupMenu) then Enable:=false;

  end;//mbRight
 if Button = mbLeft then
  begin
   if not ShowSelector then ShowSelector := true; //zeigt den Rahmen in Paint
   if scanning then
    begin
     Cursor:=2;
     try
      TmpBmp := TBitmap.Create;  //je nach Parent ändert sich Canvas.x,y deshalb so
      TmpBmp.SetSize(width,height);
      TmpBmp.Canvas.CopyRect(Rect(0,0,width,height),self.Canvas,Rect(0,0,width,height));
      TransparentColor:=TmpBmp.canvas.Pixels[x,y];

      if FTransparent then
       begin
        ReturnBmp.Transparent:=true;
        ReturnBmp.TransparentColor:=FTransparentColor;
        Transparent:=false;
        Transparent:= true;

       end;
     finally
      TmpBmp.Free;
     end;
     exit;
    end;
   if SelOpt = FormFree then  //Formfreie Auswahl
    begin
     Cursor:=1;
     FormFreeAktiv := true;
     Num:=0;
     setLength(DemarcationLine,1);
     DemarcationLine[Num].x:=X;
     DemarcationLine[Num].y:=Y;
     inc(Num);
     Invalidate;
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
       StretchBmp.Assign(ReturnBmp);
       StretchBmp.Transparent:=false;
       exit;
      end;
      if ptinrect(FSelArea,P1) then  //bewegen starten
        begin
         Cursor:= crSize;
         moveX:=x;moveY:=y;
         moveStart:=true;
         moveRect:=FSelArea;
         if strg then FCuttingOut:=false;
         if ptinrect(FSelArea,P1) then Exit;
        end;
    end;//marked
   ReturnBmp.Assign(nil);
   FSelArea.Left   := X;
   FSelArea.Top    := Y;
   FSelArea.Right  := X;   //Schaut besser aus
   FSelArea.Bottom := Y;
   if Assigned(OnChange) then OnChange(FSelArea);
   modus:=active;
   invalidate;
 end;//mbLeft
end;

procedure TSelector.MouseMove(Shift: TShiftState; X, Y: Integer);
var P1 : TPoint;
    deltaX,deltaY : integer;
begin
 if scanning then
    begin
     Cursor:=2;
     exit;
    end;
 if SelOpt = FormFree then  //Formfreie Auswahl
  begin
   Cursor:=1;
   P1.x:=X;P1.y:=Y;
   if P1.x < FSelArea.Left  then mouse.CursorPos:= ClientToScreen(Point(FSelArea.Left,P1.y));
   if P1.x > FSelArea.Right then mouse.CursorPos:= ClientToScreen(Point(FSelArea.Right,P1.y));
   if P1.y < FSelArea.Top then mouse.CursorPos:= ClientToScreen(Point(P1.x,FSelArea.Top));
   if P1.y > FSelArea.Bottom then mouse.CursorPos:= ClientToScreen(Point(P1.x,FSelArea.Bottom));

   if FormFreeAktiv then
    begin
     setLength(DemarcationLine,Num+1);
     DemarcationLine[Num].x:=X;
     DemarcationLine[Num].y:=Y;
     Invalidate;
     inc(Num);
   end;
   exit;
  end;//Formfreie Auswahl


 if modus = active then   //roter Rahmen
  begin
   if Assigned(OnChange) then OnChange(FSelArea);
   if SelOpt = Rectangle then
    begin
     FSelArea.Right  := X;
     FSelArea.Bottom := Y;
    end;//Rectangle
   if SelOpt = Quad then
    begin
     FSelArea.Bottom := Y;
     FSelArea.Right  := FSelArea.Left + (Y-FSelArea.Top);
    end;//Quad
  end;//active
 if modus = marked then   //blauer Rahmen
  begin
   if FSelArea.Left<>-100 then if Assigned(OnChange) then OnChange(FSelArea);
   Cursor:=crdefault;
   P1.x:=X;P1.y:=Y;

   if ptinrect(LTBigSq,P1) then Cursor:= crSizeNW;
   if ptinrect(TCBigSq,P1) and (SelOpt = Rectangle) then Cursor:= crSizeN;
   if ptinrect(RTBigSq,P1) then Cursor:= crSizeNE;
   if ptinrect(RCBigSq,P1) and (SelOpt = Rectangle) then Cursor:= crSizeE;
   if ptinrect(RBBigSq,P1) then Cursor:= crSizeSE;
   if ptinrect(BCBigSq,P1) and (SelOpt = Rectangle) then Cursor:= crSizeS;
   if ptinrect(LBBigSq,P1) then Cursor:= crSizeSW;
   if ptinrect(LCBigSq,P1) and (SelOpt = Rectangle) then Cursor:= crSizeW;
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

procedure TSelector.MouseUp(Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
var OldColor : TColor;
    lv       : integer;
begin
 if Assigned(OnChange) then OnChange(FSelArea);
 if scanning then
    begin
     Cursor:=crDefault;
     scanning:=false;
     exit;
    end;
 if FormFreeAktiv then
    begin
     setLength(DemarcationLine,Num+1);
     DemarcationLine[Num].x:=DemarcationLine[0].x;
     DemarcationLine[Num].y:=DemarcationLine[0].y;
     idxx:=1;
     Paint;
     FormFreeAktiv:=false;
     StretchBmp.Assign(ReturnBmp);
     SelOpt:= OldOption;
     OldColor := ReturnBmp.canvas.Pen.Color;
     for lv:=0 to Num do DemarcationLine[Lv].x:= DemarcationLine[Lv].x - FSelArea.Left;
     for lv:=0 to Num do DemarcationLine[Lv].y:= DemarcationLine[Lv].y - FSelArea.Top;
     ReturnBmp.Canvas.Pen.Color:=FDemarcationColor;
     ReturnBmp.canvas.MoveTo(DemarcationLine[0]);
     for lv:=1 to Num do ReturnBmp.canvas.LineTo(DemarcationLine[Lv]);
     ReturnBmp.canvas.Pen.Color:= OldColor;
     if SelOpt= Rectangle then
      begin
       FItem72.Checked:=true;
       FItem73.Checked:=false;
       FItem74.Checked:=false;
      end;
     if SelOpt= Quad      then
      begin
       FItem72.Checked:=false;
       FItem73.Checked:=true;
       FItem74.Checked:=false;
      end;
     FormFreeCopy(StretchBmp,DemarcationLine);
     ReturnBmp.Assign(StretchBmp);

     Cursor := crDefault;
     Invalidate;
     exit;
   end;
 if modus = active then
  begin
   if SelOpt = Rectangle then
    begin
     FSelArea.Right  := X;
     FSelArea.Bottom := Y;
    end;//Rectangle
   if SelOpt = Quad then
    begin
     FSelArea.Bottom := Y;
     FSelArea.Right  := FSelArea.Left + (Y-FSelArea.Top);
    end;//Quad
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
   if stretchStart then
      begin
       stretchStart:=false;
       exit;
      end;
  end;

 ReturnBmp.SetSize(FSelArea.Width,FSelArea.Height);
 ReturnBmp.Canvas.CopyRect(rect(0,0,ReturnBmp.Width,ReturnBmp.Height),SourceBmp.Canvas,FSelArea);
 FBackgrd:=FSelArea;
 invalidate;
end;

procedure TSelector.ChangeSize(const X,Y:integer;const aSquare:byte);
var delta  : integer;
    tmpbmp : TBitmap;
begin
 tmpbmp := TBitmap.Create;
 tmpbmp.SetSize(FSelArea.Width,FSelArea.Height);
 if square=LT then   //LinksOben ziehen
   begin
     delta:= Y - FSelArea.Top;
     FSelArea.Left   := FSelArea.Left + delta;
     FSelArea.Top    := FSelArea.Top  + delta;
    end;
 if (square=TC) and (SelOpt = Rectangle)  then   //ObenMitte ziehen
   begin
     FSelArea.Top    := Y;
    end;
 if square=RT then   //RechtsOben ziehen
   begin
     delta:= Y - FSelArea.Top;
     FSelArea.width   := FSelArea.width - delta;
     FSelArea.Top    := FSelArea.Top  + delta;
    end;
 if (square=RC) and (SelOpt = Rectangle) then   //RechtsMitte ziehen
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
 if (square=BC) and (SelOpt = Rectangle)  then   //UntenMitte ziehen
   begin
     FSelArea.Bottom    := Y;
    end;
 if square=LB then   //LinksUnten ziehen
   begin
     delta:= FSelArea.Left -x;
     FSelArea.Left   := FSelArea.Left - delta;
     FSelArea.Bottom    := FSelArea.Bottom  + delta;
    end;
 if (square=LC) and (SelOpt = Rectangle) then   //LinksMitte ziehen
   begin
     FSelArea.Left    := X;
    end;
 tmpbmp.SetSize(FSelArea.Width,FSelArea.Height);
 tmpbmp.Canvas.StretchDraw(rect(0,0,FSelArea.Width,FSelArea.Height),StretchBmp);
 ReturnBmp.Assign(tmpbmp);
 ReturnBmp.Transparent:=FTransparent;
 ReturnBmp.TransparentColor:=FTransparentColor;
 if FTransparent then   //braucht es unter win?
  begin
   Transparent:=false;
   Transparent:=true;
  end;
 tmpbmp.Free;
 invalidate;
 exit;
end;

procedure TSelector.CalculateSquares;
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


function TSelector.SelAreaToString: string;
var aLeft,aRight,aTop,aBottom : string;
    Area : TRect;
begin
  if not Enable then Area:= LastSelArea else Area:= FSelArea;
  aLeft   := inttostr(Area.Left);
  aTop    := inttostr(Area.Top);
  aRight  := inttostr(Area.Right);
  aBottom := inttostr(Area.Bottom);
  result:= aLeft+','+ATop+','+aRight+','+aBottom;
end;

function TSelector.SelAreaLeftToString: string;
var Area : TRect;
begin
 if not Enable then Area:= LastSelArea else Area:= FSelArea;
 result := inttostr(Area.Left);
end;

function TSelector.SelAreaRightToString: string;
var Area : TRect;
begin
 if not Enable then Area:= LastSelArea else Area:= FSelArea;
 result  := inttostr(Area.Right);
end;

function TSelector.SelAreaTopToString: string;
var Area : TRect;
begin
 if not Enable then Area:= LastSelArea else Area:= FSelArea;
 result    := inttostr(Area.Top);
end;

function TSelector.SelAreaBottomToString: string;
var Area : TRect;
begin
 if not Enable then Area:= LastSelArea else Area:= FSelArea;
 result := inttostr(Area.Bottom);
end;

function TSelector.SelAreaWidthToString: string;
var Area : TRect;
begin
 if not Enable then Area:= LastSelArea else Area:= FSelArea;
 result  := inttostr(Area.Width);
end;

function TSelector.SelAreaHeightToString: string;
var Area : TRect;
begin
 if not Enable then Area:= LastSelArea else Area:= FSelArea;
 result := inttostr(Area.Height);
end;

procedure TSelector.UpdateLocale;
begin
  FItem0.Caption  := TransText0;
  FItem1.Caption  := TransText1;
  FItem2.Caption  := TransText2;
  FItem21.Caption := TransText21;
  FItem3.Caption  := TransText3;
  FItem4.Caption  := TransText4;
  FItem5.Caption  := TransText5;
  FItem51.Caption := TransText51;
  FItem52.Caption := TransText52;
  FItem6.Caption  := TransText6;
  FItem7.Caption  := TransText7;
  FItem71.Caption := TransText71;
  FItem72.Caption := TransText72;
  FItem73.Caption := TransText73;
  FItem74.Caption := TransText74;
  FItem8.Caption  := TransText8;
  FItem81.Caption := TransText81;
  FItem82.Caption := TransText82;
  FItem83.Caption := TransText83;
  FItem84.Caption := TransText84;
  FItem85.Caption := TransText85;
  FItem9.Caption  := TransText9;
  FItem10.Caption := TransText10;
  FItem11.Caption := TransText11;
  FItem12.Caption := TransText12;
end;

//XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---AB HIER PAINT PROCEDUREN---XXXXXXXXXXXXXXXXXXXXXXXXXX

procedure TSelector.LastBitmap(var aBmp: TBitmap);
begin
 aBmp.Assign(LastBmp);
end;


procedure TSelector.Drawing(aCanvas: TCanvas);   //Diese Methode kann vom Parent in seiner
var OldStyle : TFPPenStyle;                      //Paintmethode aufgerufen werden!
    lv       : integer;
begin
 for lv:= 0 to idx do
  begin
   if Overlays[lv].Cutting then
    begin
     aCanvas.Brush.Color:= Overlays[lv].bkgrdColor;//FCuttingColor;
     OldStyle := aCanvas.Pen.Style;
     aCanvas.Pen.Style:=psClear;
     aCanvas.Rectangle(Overlays[lv].bkgrdRect);//  FBackgrd);
     aCanvas.Pen.Style:= OldStyle;
    end; //cuttingout
  aCanvas.Draw(Overlays[lv].DrawArea.Left,Overlays[lv].DrawArea.Top,Overlays[lv].RtrnBmp);// ReturnBmp);
 end;//for
end;

procedure TSelector.ActivePaint;
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

procedure TSelector.MarkedPaint;
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

procedure TSelector.Paint;
var OldStyle : TFPPenStyle;
    OldColor : TColor;
    lv       : integer;
begin
 inherited;
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
//zeichnet den Parent
   canvas.Draw(0,0,SourceBmp);
//zeichnet den Ausschnitt falls aktiv
     if FCuttingOut then
      begin
       Canvas.Brush.Color:=FCuttingColor;
       OldStyle := Canvas.Pen.Style;
       Canvas.Pen.Style:=psClear;
       Canvas.Rectangle(FBackgrd);
       Canvas.Pen.Style:= OldStyle;
      end;
//zeichnet den Ausschnitt
      canvas.Draw(FSelArea.Left,FSelArea.Top,ReturnBmp);
   if ShowSelector then
    begin
     canvas.Pen.Width  := 1;
     if modus = active then ActivePaint else MarkedPaint;
     if FormFreeAktiv then
      begin
        OldColor := canvas.Pen.Color;
        canvas.Pen.Color:=FDemarcationColor;
        canvas.MoveTo(DemarcationLine[0]);
        for lv:=1 to Num-1+idxx do canvas.LineTo(DemarcationLine[Lv]);
        canvas.Pen.Color:= OldColor;
      end;
    end;
  end;
end;



procedure Register;
begin
  {$I selector_icon.lrs}
  RegisterComponents('Others',[TSelector]);
end;

initialization
  {$I image.lrs}
  {$I cursors.lrs}
end.

