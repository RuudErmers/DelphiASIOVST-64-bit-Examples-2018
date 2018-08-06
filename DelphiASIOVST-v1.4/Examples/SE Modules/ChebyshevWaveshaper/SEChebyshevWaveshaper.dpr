library SEChebyshevWaveshaper;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEChebyshevWaveshaperModule in 'SEChebyshevWaveshaperModule.pas';

{$E sem}
{$R *.res}

const
  CModuleClasses : array [0..1] of TSEModuleBaseClass = (
    TSEStaticChebyshevWaveshaperModule,
    TSEAutomatableChebyshevWaveshaperModule);


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