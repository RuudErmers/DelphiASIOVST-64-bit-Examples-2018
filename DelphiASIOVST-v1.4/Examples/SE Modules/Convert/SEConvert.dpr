library SEConvert;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEConvertModule in 'SEConvertModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
   0: TSEFloatToIntModule.GetModuleProperties(Properties);
   1: TSEDoubleToIntModule.GetModuleProperties(Properties);
   2: TSEIntToFloatModule.GetModuleProperties(Properties);
   3: TSEIntToDoubleModule.GetModuleProperties(Properties);
   4: TSEFloatToTextModule.GetModuleProperties(Properties);
   5: TSEDoubleToTextModule.GetModuleProperties(Properties);
   6: TSEIntToTextModule.GetModuleProperties(Properties);
   7: TSETextToGuiTextModule.GetModuleProperties(Properties);
   8: TSEIntToGuiIntModule.GetModuleProperties(Properties);
   9: TSEFloatToGuiFloatModule.GetModuleProperties(Properties);
  10: TSEDoubleToGuiDoubleModule.GetModuleProperties(Properties);
  11: TSEBooleanToGuiBooleanModule.GetModuleProperties(Properties);
  12: TSESampleToFloatModule.GetModuleProperties(Properties);
  13: TSESampleToDoubleModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;

 case Index of // !!TODO!! list your in / out plugs
   0: if (ProcessType = 1) then SEModuleBase := TSEFloatToIntModule.Create(SEAudioMaster, Reserved);
   1: if (ProcessType = 1) then SEModuleBase := TSEDoubleToIntModule.Create(SEAudioMaster, Reserved);
   2: if (ProcessType = 1) then SEModuleBase := TSEIntToFloatModule.Create(SEAudioMaster, Reserved);
   3: if (ProcessType = 1) then SEModuleBase := TSEIntToDoubleModule.Create(SEAudioMaster, Reserved);
   4: if (ProcessType = 1) then SEModuleBase := TSEFloatToTextModule.Create(SEAudioMaster, Reserved);
   5: if (ProcessType = 1) then SEModuleBase := TSEDoubleToTextModule.Create(SEAudioMaster, Reserved);
   6: if (ProcessType = 1) then SEModuleBase := TSEIntToTextModule.Create(SEAudioMaster, Reserved);
   7: if (ProcessType = 1) then SEModuleBase := TSETextToGuiTextModule.Create(SEAudioMaster, Reserved);
   8: if (ProcessType = 1) then SEModuleBase := TSEIntToGuiIntModule.Create(SEAudioMaster, Reserved);
   9: if (ProcessType = 1) then SEModuleBase := TSEFloatToGuiFloatModule.Create(SEAudioMaster, Reserved);
  10: if (ProcessType = 1) then SEModuleBase := TSEDoubleToGuiDoubleModule.Create(SEAudioMaster, Reserved);
  11: if (ProcessType = 1) then SEModuleBase := TSEBooleanToGuiBooleanModule.Create(SEAudioMaster, Reserved);
  12: if (ProcessType = 1) then SEModuleBase := TSESampleToFloatModule.Create(SEAudioMaster, Reserved);
  13: if (ProcessType = 1) then SEModuleBase := TSESampleToDoubleModule.Create(SEAudioMaster, Reserved);
 end;

 if Assigned(SEModuleBase)
  then Result := SEModuleBase.Effect
  else Result := nil;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.