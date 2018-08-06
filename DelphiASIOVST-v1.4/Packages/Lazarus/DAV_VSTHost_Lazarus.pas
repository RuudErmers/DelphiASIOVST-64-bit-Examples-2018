{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DAV_VSTHost_Lazarus; 

interface

uses
DAV_VSTHostRegister, DAV_VSTHost, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_VSTHostRegister', @DAV_VSTHostRegister.Register); 
end; 

initialization
  RegisterPackage('DAV_VSTHost_Lazarus', @Register); 
end.
