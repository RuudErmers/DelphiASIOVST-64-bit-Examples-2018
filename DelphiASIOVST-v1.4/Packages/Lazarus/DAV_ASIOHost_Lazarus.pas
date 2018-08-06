{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DAV_ASIOHost_Lazarus; 

interface

uses
    DAV_ASIO, DAV_OpenASIO, DAV_ASIOConvert, DAV_ASIOGenerator, DAV_ASIOHost, 
  DAV_ASIOMixer, DAV_ASIORegister, DAV_AsioInterface, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_ASIORegister', @DAV_ASIORegister.Register); 
end; 

initialization
  RegisterPackage('DAV_ASIOHost_Lazarus', @Register); 
end.
