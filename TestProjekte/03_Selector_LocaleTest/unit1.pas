unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLTranslator, Translations, Selector;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Selector1: TSelector;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    fn   : string;
    lang : string;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  Selector1.Enable:= true;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
 lang:= 'en';
 SetDefaultLang(lang);

 fn := Application.Location + 'locale/selectorstrconsts.%s.po';
 Translations.TranslateUnitResourceStrings('selectorstrconsts', Format(fn, [lang]));
 //Aktualisiert die PopUp-Menüeinträge
 Selector1.UpdateLocale;
end;

end.

