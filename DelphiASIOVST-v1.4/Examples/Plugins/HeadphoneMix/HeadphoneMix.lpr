{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library HeadphoneMix;

{$I DAV_Compiler.inc}

uses
  Interfaces,
  Forms,
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  HMDM in 'HMDM.pas' {HMModule: TVSTModule};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  HMModule: THMModule;
begin
 try
  HMModule := THMModule.Create(nil);
  HMModule.AudioMaster := audioMaster;
  Result := HMModule.Effect;
  {$IFNDEF UseDelphi}
  with HMModule do
   begin
    Effect^.user := HMModule;
    Effect^.AudioEffectPtr := HMModule;
    Name := 'HMModule';
    Flags := [effFlagsCanReplacing];
    Version := '1.0';
    EffectName := 'Headphone Mix';
    ProductName := 'DAV Effect Examples';
    VendorName := 'Delphi ASIO & VST Project';
    VersionMajor := 1;
    VersionMinor := 0;
    VersionRelease := 0;
    PlugCategory := vpcEffect;
    TailSize := 0;
    CanDos := [vcdOffline, vcdPlugAsChannelInsert, vcd2in2out];
    SampleRate := 44100.0;
    numCategories := 1;
    CurrentProgram := -1;
    KeysRequired := False;
    UniqueID := 'HMAc';
    OnClose := VSTModuleClose;
    OnOpen := VSTModuleOpen;
    OnProcess := VSTModuleProcess;
    OnProcessReplacing := VSTModuleProcess;
    OnSampleRateChange := VSTModuleSampleRateChange;
    with ParameterProperties.Add do begin
      Max := 100.0;
      DisplayName := 'Effect';
      Units := '%';
      SmoothingFactor := 1.0;
      CanBeAutomated := True;
      ReportVST2Properties := False;
      StepFloat := 5.0;
      SmallStepFloat := 1.0;
      LargeStepFloat := 10.0;
      Flags := [];
      MinInteger := 0;
      MaxInteger := 100;
      StepInteger := 5;
      LargeStepInteger := 10;
      ShortLabel := 'Effect';
      VSTModule := HMModule;
      OnParameterChange := HMMEffectChange;
    end;
    with ParameterProperties.Add do begin
      Max := 2.0;
      DisplayName := 'Model';
      SmoothingFactor := 1.0;
      CanBeAutomated := True;
      ReportVST2Properties := False;
      StepFloat := 1.0;
      SmallStepFloat := 1.0;
      LargeStepFloat := 1.0;
      Flags := [];
      MinInteger := 0;
      MaxInteger := 2;
      StepInteger := 1;
      LargeStepInteger := 1;
      ShortLabel := 'Model';
      VSTModule := HMModule;
      OnParameterChange := HMMModelChange;
      OnCustomParameterDisplay := HMMModelDisplay;
    end;
    with ParameterProperties.Add do begin
      Max := 1.0;
      DisplayName := 'Polarity';
      SmoothingFactor := 1.0;
      CanBeAutomated := True;
      ReportVST2Properties := False;
      StepFloat := 1.0;
      SmallStepFloat := 1.0;
      LargeStepFloat := 1.0;
      Flags := [];
      MinInteger := 0;
      MaxInteger := 1;
      StepInteger := 1;
      LargeStepInteger := 1;
      ShortLabel := 'Pol';
      VSTModule := HMModule;
      OnParameterChange := HMMPolarityChange;
      OnCustomParameterDisplay := HMMPolarityDisplay;
    end;
  end;
  {$ENDIF}
 except
   Result := nil;
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
  VSTPluginMain name 'VSTPluginMain',
{$ENDIF}

begin
  Application.Initialize;
end.