{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit TheOthers;

{$warn 5023 off : no warning about unused units}
interface

uses
  Selector, CustomLine, selectorstrconsts, SimpleGraphics, Sizer, Layer, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Selector', @Selector.Register);
  RegisterUnit('Sizer', @Sizer.Register);
  RegisterUnit('Layer', @Layer.Register);
end;

initialization
  RegisterPackage('TheOthers', @Register);
end.
