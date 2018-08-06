library SEDitherNoiseshaper;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEDitherNoiseshaperModule in 'SEDitherNoiseshaperModule.pas',
  SEDitherHighshelfNoiseshaperModule in 'SEDitherHighshelfNoiseshaperModule.pas',
  SEDitherSharpNoiseshaperModule in 'SEDitherSharpNoiseshaperModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEDitherNoiseshaperStaticModule.GetModuleProperties(Properties);
  1: TSEDitherNoiseshaperControllableModule.GetModuleProperties(Properties);
  2: TSEDitherHighshelfNoiseshaperStaticModule.GetModuleProperties(Properties);
  3: TSEDitherHighshelfNoiseshaperControllableModule.GetModuleProperties(Properties);
  4: TSEDitherSharpNoiseshaperStaticModule.GetModuleProperties(Properties);
  5: TSEDitherSharpNoiseshaperControllableModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
begin
 Result := nil;
 if (ProcessType = 1) then
  case Index of
   0: Result := TSEDitherNoiseshaperStaticModule.Create(SEAudioMaster, Reserved).Effect;
   1: Result := TSEDitherNoiseshaperControllableModule.Create(SEAudioMaster, Reserved).Effect;
   2: Result := TSEDitherHighshelfNoiseshaperStaticModule.Create(SEAudioMaster, Reserved).Effect;
   3: Result := TSEDitherHighshelfNoiseshaperControllableModule.Create(SEAudioMaster, Reserved).Effect;
   4: Result := TSEDitherSharpNoiseshaperStaticModule.Create(SEAudioMaster, Reserved).Effect;
   5: Result := TSEDitherSharpNoiseshaperControllableModule.Create(SEAudioMaster, Reserved).Effect;
  end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.