library SECorrelationMeter;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SECorrelationMeterModule in 'SECorrelationMeterModule.pas',
  SECorrelationMeterGUI in 'SECorrelationMeterGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSECorrelationMeterModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase : TSEModuleBase;
  GUI          : TSEGUIBase;
begin
 Result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSECorrelationMeterModule.Create(SEAudioMaster, Reserved);
        if Assigned(SEModuleBase)
         then Result := SEModuleBase.Effect;
       end else
      if (ProcessType = 2) then // GUI Object
       begin
        GUI := TSECorrelationMeterGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
        if Assigned(GUI)
         then Result := GUI.SEGUIStructBase;
       end;
     end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.