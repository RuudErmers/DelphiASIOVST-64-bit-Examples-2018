unit ExperimentalFilterDSP;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs,
  DAV_Types, DAV_VSTModule, DAV_DspFilterBasics;

type
  TExperimentalFilter = class(TBasicPeakFilter)
  public
    function ProcessSample32(Input: Single): Single; override;
  end;

  TExperimentalFilterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FFilters         : array [0..1] of TExperimentalFilter;
  public
  end;

implementation

{$R *.DFM}

uses
  ExperimentalFilterGUI, DAV_Approximations;

{ TExperimentalFilter }

function TExperimentalFilter.ProcessSample32(Input: Single): Single;
begin
 Result    := FNominator[0] * Input + FState[0];
 FState[0] := 5 * FastTanhContinousError5(0.2 * (FNominator[1] * Input - FDenominator[1] * Result + FState[1]));
 FState[1] := 5 * FastTanhContinousError5(0.2 * (FNominator[2] * Input - FDenominator[2] * Result));
end;


{ TExperimentalFilterModule }

procedure TExperimentalFilterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TExperimentalFilterModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TExperimentalFilterModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilters) - 1 do
  begin
   FFilters[ChannelIndex] := TExperimentalFilter.Create;
   if Abs(SampleRate) > 0
    then FFilters[ChannelIndex].SampleRate := Abs(SampleRate);
  end;

 // set editor form class
 EditorFormClass := TFmExperimentalFilter;

 // set initial parameters
 Parameter[0] := 1000;
 Parameter[1] := 0;
 Parameter[2] := 1;
end;

procedure TExperimentalFilterModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FFilters) - 1
   do FFilters[ChannelIndex].Frequency := Value;
 finally
  FCriticalSection.Leave;
 end;

 // eventually update GUI
 if EditorForm is TFmExperimentalFilter
  then TFmExperimentalFilter(EditorForm).UpdateFrequency;
end;

procedure TExperimentalFilterModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FFilters) - 1
   do FFilters[ChannelIndex].Gain := Value;
 finally
  FCriticalSection.Leave;
 end;

  // eventually update GUI
 if EditorForm is TFmExperimentalFilter
  then TFmExperimentalFilter(EditorForm).UpdateGain;
end;

procedure TExperimentalFilterModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FFilters) - 1
   do FFilters[ChannelIndex].Bandwidth := Value;
 finally
  FCriticalSection.Leave;
 end;

  // eventually update GUI
 if EditorForm is TFmExperimentalFilter
  then TFmExperimentalFilter(EditorForm).UpdateBandwidth;
end;

procedure TExperimentalFilterModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilters) - 1
  do FreeAndNil(FFilters[ChannelIndex]);
end;

procedure TExperimentalFilterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for ChannelIndex := 0 to Length(FFilters) - 1
    do FFilters[ChannelIndex].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TExperimentalFilterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   for ChannelIndex := 0 to Length(FFilters) - 1
    do Outputs[ChannelIndex, SampleIndex] := FFilters[ChannelIndex].ProcessSample32(Inputs[ChannelIndex, SampleIndex]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
