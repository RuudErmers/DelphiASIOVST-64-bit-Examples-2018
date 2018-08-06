library SEIntToList;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  DAV_SEGUI,
  SEIntToListModule in 'SEIntToListModule.pas',
  SEIntToListGUI in 'SEIntToListGUI.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSEIntToListModule.GetModuleProperties(Properties)
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: Pointer; Reserved: Pointer): Pointer; cdecl; export;
var
  SEIntToListModule : TSEIntToListModule;
  GUI               : TSEIntToListGui;
begin
 Result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEIntToListModule := TSEIntToListModule.Create(SEAudioMaster, Reserved);
        if Assigned(SEIntToListModule)
         then Result := SEIntToListModule.Effect;
       end else
      if (ProcessType = 1) then // GUI Object
       begin
        GUI := TSEIntToListGui.Create(TSEGuiCallback(SEAudioMaster), Reserved); //nasty!
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