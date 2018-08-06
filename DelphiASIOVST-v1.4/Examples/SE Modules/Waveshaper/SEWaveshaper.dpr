library SEWaveshaper;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEWaveshaperModule in 'SEWaveshaperModule.pas',
  SEWaveshaperGUI in 'SEWaveshaperGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSEWaveshaperModule.GetModuleProperties(Properties);
  else Result := False;
 end;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
begin
 Result := nil;
 case Index of
  0: if (ProcessType = 1) then// Audio Processing Object
      begin
       SEModuleBase := TSEWaveshaperModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end else
     if (ProcessType = 2) then // GUI Object
      begin
       GUI := TSEWaveshaperGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
       if Assigned(GUI)
        then Result := GUI.SEGUIStructBase;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.