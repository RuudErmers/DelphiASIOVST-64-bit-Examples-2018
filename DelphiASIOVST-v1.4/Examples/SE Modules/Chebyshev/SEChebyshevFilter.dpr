library SEChebyshevFilter;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEChebyshev1FilterModule in 'SEChebyshev1FilterModule.pas',
  SEChebyshev2FilterModule in 'SEChebyshev2FilterModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses : array [0..15] of TSEModuleBaseClass = (
    TSEStaticChebyshev1FilterLPModule, TSEStaticChebyshev1FilterHPModule,
    TSEControlableChebyshev1FilterLPModule,
    TSEControlableChebyshev1FilterHPModule,
    TSEAutomatebleChebyshev1FilterLPModule,
    TSEAutomatebleChebyshev1FilterHPModule,
    TSEAutomatebleXChebyshev1FilterLPModule,
    TSEAutomatebleXChebyshev1FilterHPModule,
    TSEStaticChebyshev2FilterLPModule, TSEStaticChebyshev2FilterHPModule,
    TSEControlableChebyshev2FilterLPModule,
    TSEControlableChebyshev2FilterHPModule,
    TSEAutomatebleChebyshev2FilterLPModule,
    TSEAutomatebleChebyshev2FilterHPModule,
    TSEAutomatebleXChebyshev2FilterLPModule,
    TSEAutomatebleXChebyshev2FilterHPModule);

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 if Index in [0..Length(CModuleClasses) - 1] then
  begin
   CModuleClasses[Index].GetModuleProperties(Properties);
   Result := True;
  end
 else Result := False;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 if Index in [0..Length(CModuleClasses) - 1]
  then Result := CModuleClasses[Index].Create(SEAudioMaster, Reserved).Effect
  else Result := nil;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.