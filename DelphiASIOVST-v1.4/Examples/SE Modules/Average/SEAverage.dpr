library SEAverage;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEAverageModule in 'SEAverageModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEAverageModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  Effect: TSEModuleBase;
begin
 Result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        Effect := TSEAverageModule.Create(SEAudioMaster, Reserved);
        if Assigned(Effect)
         then Result := Effect.Effect;
       end;
     end;
 end;
end;

exports
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.