library SELinkwitzRiley;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  DAV_SEModule,
  SELinkwitzRileyModule in 'SELinkwitzRileyModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 Result := True;
 case Index of
  0: TSELinkwitzRileyStaticModule.GetModuleProperties(Properties);
  1: TSELinkwitzRileyControlableModule.GetModuleProperties(Properties);
  2: TSELinkwitzRileyAutomatableModule.GetModuleProperties(Properties);
  else Result := False;
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
       SEModuleBase := TSELinkwitzRileyStaticModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  1: if (ProcessType = 1) then
      begin
       SEModuleBase := TSELinkwitzRileyControlableModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
  2: if (ProcessType = 1) then
      begin
       SEModuleBase := TSELinkwitzRileyAutomatableModule.Create(SEAudioMaster, Reserved);
       if Assigned(SEModuleBase)
        then Result := SEModuleBase.Effect;
      end;
 end;
end;

exports 
  makeModule name 'makeModule',
  getModuleProperties name 'getModuleProperties';

end.