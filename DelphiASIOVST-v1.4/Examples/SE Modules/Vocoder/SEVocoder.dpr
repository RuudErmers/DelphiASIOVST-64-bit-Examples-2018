library SEVocoder;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEVocoderModule in 'SEVocoderModule.pas',
  SESimpleVocoderModule in 'SESimpleVocoderModule.pas',
  SEBarkVocoderModule in 'SEBarkVocoderModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEVocoderStaticModule.GetModuleProperties(Properties);
  1: TSEVocoderControllableModule.GetModuleProperties(Properties);
  2: TSEVocoderAutomatableModule.GetModuleProperties(Properties);
  3: TSESimpleVocoderStaticModule.GetModuleProperties(Properties);
  4: TSESimpleVocoderControllableModule.GetModuleProperties(Properties);
  5: TSESimpleVocoderAutomatableModule.GetModuleProperties(Properties);
  6: TSEBarkVocoderStaticModule.GetModuleProperties(Properties);
  7: TSEBarkVocoderControllableModule.GetModuleProperties(Properties);
  8: TSEBarkVocoderAutomatableModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 Result := nil;
 if (ProcessType = 1) then
  case Index of
   0: Result := TSEVocoderStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: Result := TSEVocoderControllableModule.Create(SEAudioMaster, Reserved).Effect;
   2: Result := TSEVocoderAutomatableModule.Create(SEAudioMaster, Reserved).Effect;
   3: Result := TSESimpleVocoderStaticModule.Create(SEAudioMaster, Reserved).Effect;
   4: Result := TSESimpleVocoderControllableModule.Create(SEAudioMaster, Reserved).Effect;
   5: Result := TSESimpleVocoderAutomatableModule.Create(SEAudioMaster, Reserved).Effect;
   6: Result := TSEBarkVocoderStaticModule.Create(SEAudioMaster, Reserved).Effect;
   7: Result := TSEBarkVocoderControllableModule.Create(SEAudioMaster, Reserved).Effect;
   8: Result := TSEBarkVocoderAutomatableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.