{ Diese Datei wurde automatisch von Lazarus erzeugt. Sie darf nicht bearbeitet 
  werden!
  Dieser Quelltext dient nur dem Übersetzen und Installieren des Packages.
 }

unit DAV_VSTPlugin_Lazarus; 

interface

uses
  DAV_VSTBasicModule, DAV_VSTChannels, DAV_VSTCustomModule, DAV_VSTEffect,
  DAV_VSTModule, DAV_VSTModuleWithDsp, DAV_VSTModuleWithMidi, 
  DAV_VSTModuleWithPrograms, DAV_VSTOfflineTask, DAV_VSTParameters, 
  DAV_VSTPrograms, DAV_VSTShellPlugins, DAV_BufferMathPascal, 
  DAV_VSTModuleLazIDE, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('DAV_VSTModuleLazIDE', @DAV_VSTModuleLazIDE.Register); 
end; 

initialization
  RegisterPackage('DAV_VSTPlugin_Lazarus', @Register); 
end.
