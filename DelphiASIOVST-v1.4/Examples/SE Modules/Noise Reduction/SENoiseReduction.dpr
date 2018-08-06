library SENoiseReduction;

uses
  FastMove, // either download the library or comment if there is an error here
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SENoiseReductionModule in 'SENoiseReductionModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSENoiseReductionStaticModule.GetModuleProperties(Properties);
  1: TSENoiseReductionControllableModule.GetModuleProperties(Properties);
  else Result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 Result := nil;
 if (ProcessType = 1) then
  case Index of
   0: Result := TSENoiseReductionStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: Result := TSENoiseReductionControllableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
