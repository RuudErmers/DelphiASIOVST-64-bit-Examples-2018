library SEVST2SEM;

{-$R 'Test.res' 'Test.rc'}

{$I DAV_Compiler.inc}

uses
  {$IFDEF UseFastMove}
  FastMove, // either download the library or comment if there is an error here
  {$ENDIF}
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SEVstWrapper in 'SEVstWrapper.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TStaticVST2SEModule.GetModuleProperties(Properties);
  1: TAutomatableVST2SEModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 Result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: if (ProcessType = 1) then// Audio Processing Object
      begin
       SEModuleBase := TStaticVST2SEModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
(*
      end else
     if (ProcessType = 2) then // GUI Object
      begin
       GUI := TSEWaveshaperGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
       if Assigned(GUI)
        then Result := GUI.SEGUIStructBase;
*)
      end;
  1: if (ProcessType = 1) then// Audio Processing Object
      begin
       SEModuleBase := TAutomatableVST2SEModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
(*
      end else
     if (ProcessType = 2) then // GUI Object
      begin
       GUI := TSEWaveshaperGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
       if Assigned(GUI)
        then Result := GUI.SEGUIStructBase;
*)
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.