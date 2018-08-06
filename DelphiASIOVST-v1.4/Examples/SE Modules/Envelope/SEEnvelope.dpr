library SEEnvelope;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEEnvelopeModule in 'SEEnvelopeModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEEnvelopeModule.GetModuleProperties(Properties);
  1: TSEHilbertModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 Result := nil;
 if (ProcessType = 1) then
  case Index of // !!TODO!! list your in / out plugs
   0: Result := TSEEnvelopeModule.Create(SEAudioMaster, Reserved).Effect;
   1: Result := TSEHilbertModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.