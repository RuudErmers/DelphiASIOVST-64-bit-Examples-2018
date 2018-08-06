library SEAudioFilePlayer;

uses
  FastMM4,
  FastMove,
  DAV_SECommon,
  DAV_SEModule,
  SESimpleAudioFilePlayerModule in 'SESimpleAudioFilePlayerModule.pas',
  SEAudioFilePlayerModule in 'SEAudioFilePlayerModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSESimpleAudioFilePlayerModule.GetModuleProperties(Properties);
  1: TSEAudioFilePlayerModule.GetModuleProperties(Properties);
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
       SEModuleBase := TSESimpleAudioFilePlayerModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  1: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEAudioFilePlayerModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.
