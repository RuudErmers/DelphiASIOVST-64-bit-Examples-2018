library SEMp3Player;

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  DAV_SECommon,
  DAV_SEModule,
  SESimpleMp3PlayerModule in 'SESimpleMp3PlayerModule.pas',
  SEMp3PlayerModule in 'SEMp3PlayerModule.pas',
  SEBufferedMp3PlayerModule in 'SEBufferedMp3PlayerModule.pas';

{$E sem}
{$R *.res}

function GetModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSESimpleMp3PlayerModule.GetModuleProperties(Properties);
  1: TSEMp3PlayerModule.GetModuleProperties(Properties);
  2: TSEBufferedMp3PlayerModule.GetModuleProperties(Properties);
  else Result := False;
 end;;
end;

function MakeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 Result := nil;
 case Index of
  0: if (ProcessType = 1) then
      begin
       SEModuleBase := TSESimpleMp3PlayerModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  1: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEMp3PlayerModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  2: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEBufferedMp3PlayerModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.