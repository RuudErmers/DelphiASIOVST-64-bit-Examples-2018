library SEAudioRGB;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEAudioRGBModule in 'SEAudioRGBModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEAudioRGBModule.GetModuleProperties(Properties);
  1: TSERGBToHSLModule.GetModuleProperties(Properties);
  2: TSEHSLToRGBModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 Result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: if (ProcessType = 1) then// Audio Processing Object
      begin
       SEModuleBase := TSEAudioRGBModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  1: if (ProcessType = 1) then// Audio Processing Object
      begin
       SEModuleBase := TSERGBToHSLModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  2: if (ProcessType = 1) then// Audio Processing Object
      begin
       SEModuleBase := TSEHSLToRGBModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.