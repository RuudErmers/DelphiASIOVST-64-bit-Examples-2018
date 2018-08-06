library NoGUIFilter;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Classes,
  DAV_VSTEffect,
  DAV_VSTModule,
  DAV_VSTParameters,
  FilterDSP in 'FilterDSP.pas';

function VSTPluginMain(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VSTFilterModule : TVSTFilter;
begin
  VSTFilterModule := TVSTFilter.Create(nil);
  // check if Effect has been assigned
  Result := nil;
  if (not Assigned(VSTFilterModule)) or (not Assigned(VSTFilterModule.Effect))
   then Exit;

  {$IFDEF UseAudioEffectPtr}
  VSTFilterModule.Effect^.AudioEffectPtr := VSTFilterModule;
  {$ELSE}
  VSTFilterModule.Effect^.User := VSTFilterModule;
  {$ENDIF}

  VSTFilterModule.AudioMaster := audioMaster;
  Result := VSTFilterModule.Effect;

  with VSTFilterModule do
  try
    Flags := [effFlagsCanReplacing, effFlagsCanDoubleReplacing];
    Version := '1.0';
    EffectName := 'Delphi VST Filter';
    ProductName := 'Delphi VST Filter';
    VendorName := 'Delphi VST';
    PlugCategory := vpcEffect;
    TailSize := 0;
    CanDos := [vcdplugAsChannelInsert, vcdplugAsSend, vcd1in1out, vcd1in2out,
               vcd2in1out, vcd2in2out];
    SampleRate := 44100.0;
    KeysRequired := False;
    UniqueID := 'Filt';
    OnProcess := VSTModuleProcess;
    OnProcess32Replacing := VSTModuleProcess;
    OnProcess64Replacing := VSTModuleProcessDoubleReplacing;
    OnOpen := VSTModuleOpen;

    with (Programs.Add) do
     begin
      DisplayName := 'Preset 1';
      VSTModule:=VSTFilterModule;
     end;
    with (Programs.Add) do
     begin
      DisplayName := 'Preset 2';
      VSTModule:=VSTFilterModule;
     end;
    with (Programs.Add) do
     begin
      DisplayName := 'Preset 3';
      VSTModule:=VSTFilterModule;
     end;

    with (ParameterProperties.Add) do
     begin
      VSTModule := VSTFilterModule;
      Min := 20.0;
      Max := 20000.0;
      Curve := ctLinear;
      DisplayName := 'Cutoff Frequency';
      Units := 'Hz';
      CurveFactor := 1000.0;
      SmoothingFactor := 0;
      CanBeAutomated := True;
      ReportVST2Properties := False;
      StepFloat := 100.0;
      SmallStepFloat := 100.0;
      LargeStepFloat := 1000.0;
      Flags := [];
      MinInteger := 20;
      MaxInteger := 20000;
      StepInteger := 100;
      LargeStepInteger := 1000;
      ShortLabel := 'Cutoff';
      OnParameterChange := VSTFilterParameterChange;
    end;
    with (ParameterProperties.Add) do
     begin
      Min := 0.01;
      Max := 20.0;
      Curve := ctLinear;
      DisplayName := 'Resonance';
      CurveFactor := 1.0;
      SmoothingFactor := 0;
      CanBeAutomated := True;
      ReportVST2Properties := False;
      StepFloat := 0.1;
      SmallStepFloat := 0.1;
      LargeStepFloat := 1.0;
      Flags := [];
      MinInteger := 0;
      MaxInteger := 20;
      StepInteger := 1;
      LargeStepInteger := 1;
      ShortLabel := 'Res';
      VSTModule := VSTFilterModule;
     end;
    CurrentProgram := 0;
    if Assigned(OnCreate) then OnCreate(VSTFilterModule);
    if Assigned(OnInitialize) then OnInitialize(VSTFilterModule);
  except
  end;
end;

exports
{$IFDEF DARWIN}  {OS X entry points}
  VSTPluginMain name '_main',
  VSTPluginMain name '_main_macho',
  VSTPluginMain name '_VSTPluginMain';
{$ELSE}
  VSTPluginMain name 'main',
  VSTPluginMain name 'main_plugin',
  VSTPluginMain name 'VSTPluginMain';
{$ENDIF}

end.
