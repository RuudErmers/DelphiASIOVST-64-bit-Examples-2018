library SELookaheadLimiter;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SELookaheadLimiterModule in 'SELookaheadLimiterModule.pas';

{$E sem}
{$R *.res}

const
  ModuleClasses : array [0..2] of TCustomLookaheadLimiterSEModuleClass =
    (TLookaheadLimiterStaticSEModule, TLookaheadLimiterParamStaticSEModule,
     TLookaheadLimiterAutomatableSEModule);

function GetModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 if (Index >= 0) and (Index < Length(ModuleClasses))
  then ModuleClasses[Index].GetModuleProperties(Properties)
  else Result := False;
end;

function MakeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 if (Index >= 0) and (Index < Length(ModuleClasses))
  then Result := (ModuleClasses[Index].Create(SEAudioMaster, Reserved)).Effect
  else Result := nil;
end;

exports 
  MakeModule name 'makeModule', 
  GetModuleProperties name 'getModuleProperties';

end.
