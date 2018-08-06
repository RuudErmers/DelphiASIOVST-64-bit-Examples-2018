library SEDynamics;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEDynamicsModule in 'SEDynamicsModule.pas';

{$E sem}
{$R *.res}

const
  ModuleClasses : array [0..41] of TCustomDynamicsSEModuleClass =
    (TSimpleDirectGateStaticSEModule, TSimpleDirectGateParamStaticSEModule,
     TSimpleDirectGateAutomatableSEModule, TSoftDirectGateStaticSEModule,
     TSoftDirectGateParamStaticSEModule, TSoftDirectGateAutomatableSEModule,
     TBrickwallLimiterStaticSEModule, TBrickwallLimiterParamStaticSEModule,
     TBrickwallLimiterAutomatableSEModule, TBrickwallSoftLimiterStaticSEModule,
     TBrickwallSoftLimiterParamStaticSEModule,
     TBrickwallSoftLimiterAutomatableSEModule,
     TBrickwallSimpleSoftLimiterStaticSEModule,
     TBrickwallSimpleSoftLimiterParamStaticSEModule,
     TBrickwallSoftLimiterAutomatableSEModule, TClassicGateStaticSEModule,
     TClassicGateParamStaticSEModule, TClassicGateAutomatableSEModule,
     TSoftClassicGateStaticSEModule, TSoftClassicGateParamStaticSEModule,
     TSoftClassicGateAutomatableSEModule, TLimiterStaticSEModule,
     TLimiterParamStaticSEModule, TLimiterAutomatableSEModule,
     TSoftLimiterStaticSEModule, TSoftLimiterParamStaticSEModule,
     TSoftLimiterAutomatableSEModule, TSimpleSoftLimiterStaticSEModule,
     TSimpleSoftLimiterParamStaticSEModule,
     TSimpleSoftLimiterAutomatableSEModule, TRangeGateStaticSEModule,
     TRangeGateParamStaticSEModule, TRangeGateAutomatableSEModule,
     TSimpleCompressorStaticSEModule, TSimpleCompressorParamStaticSEModule,
     TSimpleCompressorAutomatableSEModule, TSoftKneeCompressorStaticSEModule,
     TSoftKneeCompressorParamStaticSEModule,
     TSoftKneeCompressorAutomatableSEModule, TRMSCompressorStaticSEModule,
     TRMSCompressorParamStaticSEModule, TRMSCompressorAutomatableSEModule);

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
