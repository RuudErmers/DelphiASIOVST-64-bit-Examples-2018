library SELightweightDynamics;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SELightweightDynamicsModule in 'SELightweightDynamicsModule.pas';

{$E sem}
{$R *.res}

const
  ModuleClasses : array [0..11] of TCustomLightweightDynamicsSEModuleClass =
    (TLightweightCompressorStaticSEModule,
     TLightweightCompressorParamStaticSEModule,
     TLightweightCompressorAutomatableSEModule,
     TLightweightFeedbackCompressorStaticSEModule,
     TLightweightFeedbackCompressorParamStaticSEModule,
     TLightweightFeedbackCompressorAutomatableSEModule,
     TLightweightLimiterStaticSEModule,
     TLightweightLimiterParamStaticSEModule,
     TLightweightLimiterAutomatableSEModule,
     TLightweightGateStaticSEModule,
     TLightweightGateParamStaticSEModule,
     TLightweightGateAutomatableSEModule);

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 if (Index >= 0) and (Index < Length(ModuleClasses))
  then ModuleClasses[Index].GetModuleProperties(Properties)
  else Result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(ModuleClasses))
  then Result := (ModuleClasses[Index].Create(SEAudioMaster, Reserved)).Effect
  else Result := nil;
end;

exports
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
