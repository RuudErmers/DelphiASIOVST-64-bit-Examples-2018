library SEWhiteNoise;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEWhiteNoiseModule in 'SEWhiteNoiseModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEWhiteNoiseModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 Result := nil;
 case Index of
  0: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEWhiteNoiseModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.