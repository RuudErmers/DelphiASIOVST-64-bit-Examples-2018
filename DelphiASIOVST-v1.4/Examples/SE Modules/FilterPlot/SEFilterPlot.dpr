library SEFilterPlot;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEFilterPlotModule in 'SEFilterPlotModule.pas',
  SEFilterPlotGUI in 'SEFilterPlotGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSEFilterPlotModule.GetModuleProperties(Properties);
  else Result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
begin
 Result := nil;
 case Index of
  0: if (ProcessType = 1) then
      begin
       SEModuleBase := TSEFilterPlotModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end else
     if (ProcessType = 2) then
      begin
       GUI := TSEFilterPlotGui.Create(TSEGuiCallback(SEAudioMaster), Reserved);
       if Assigned(GUI)
        then Result := GUI.SEGUIStructBase;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.