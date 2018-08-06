library SETanhAproximations;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SETanhAproximationsModule in 'SETanhAproximationsModule.pas';

{$E sem}

{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of // !!TODO!! list your in / out plugs
  0: TSETanhAproximationsModule.GetModuleProperties(Properties);
  1: TSETanhAproxModule.GetModuleProperties(Properties);
  2: TSETanhModule.GetModuleProperties(Properties);
  else Result := False; // host will ask for module 0,1,2,3 etc. return false to signal when done
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 Result := nil;
 case Index of // !!TODO!! list your in / out plugs
  0: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSETanhAproximationsModule.Create(SEAudioMaster, Reserved);
        if Assigned(SEModuleBase)
         then Result := SEModuleBase.Effect;
       end;
     end;
  1: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSETanhAproxModule.Create(SEAudioMaster, Reserved);
        if Assigned(SEModuleBase)
         then Result := SEModuleBase.Effect;
       end;
     end;
  2: begin
      if (ProcessType = 1) then// Audio Processing Object
       begin
        SEModuleBase := TSETanhModule.Create(SEAudioMaster, Reserved);
        if Assigned(SEModuleBase)
         then Result := SEModuleBase.Effect;
       end;
     end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.