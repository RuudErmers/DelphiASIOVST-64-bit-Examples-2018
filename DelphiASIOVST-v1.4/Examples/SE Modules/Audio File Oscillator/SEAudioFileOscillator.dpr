library SEAudioFileOscillator;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SESimpleAudioFileOscillatorModule in 'SESimpleAudioFileOscillatorModule.pas',
  SEAudioFileOscillatorModule in 'SEAudioFileOscillatorModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSESimpleAudioFileOscillatorModule.GetModuleProperties(Properties);
  1: TSEAudioFileOscillatorModule.GetModuleProperties(Properties);
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
       SEModuleBase := TSESimpleAudioFileOscillatorModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  1: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEAudioFileOscillatorModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.