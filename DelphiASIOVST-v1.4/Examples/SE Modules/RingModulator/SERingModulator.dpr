library SERingModulator;

uses
  FastMove,
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SERingModulatorModule in 'SERingModulatorModule.pas',
  SEAnalogRingModulatorModule in 'SEAnalogRingModulatorModule.pas',
  SELightweightAnalogRingModulatorModule in 'SELightweightAnalogRingModulatorModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSERingModulatorStaticModule.GetModuleProperties(Properties);
  1: TSERingModulatorControllableModule.GetModuleProperties(Properties);
  2: TSEAnalogRingModulatorStaticModule.GetModuleProperties(Properties);
  3: TSEAnalogRingModulatorControllableModule.GetModuleProperties(Properties);
  4: TSELightweightAnalogRingModulatorStaticModule.GetModuleProperties(Properties);
  5: TSELightweightAnalogRingModulatorControllableModule.GetModuleProperties(Properties);
  else Result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 Result := nil;
 if (ProcessType = 1) then
  case Index of
   0: Result := TSERingModulatorStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: Result := TSERingModulatorControllableModule.Create(SEAudioMaster, Reserved).Effect;
   2: Result := TSEAnalogRingModulatorStaticModule.Create(SEAudioMaster, Reserved).Effect;
   3: Result := TSEAnalogRingModulatorControllableModule.Create(SEAudioMaster, Reserved).Effect;
   4: Result := TSELightweightAnalogRingModulatorStaticModule.Create(SEAudioMaster, Reserved).Effect;
   5: Result := TSELightweightAnalogRingModulatorControllableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
