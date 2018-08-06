unit PhaseRotatorDSP;

interface

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_DspFilterBasics, DAV_VSTModule, DAV_VSTEffect;

type
  { TPhaseRotatorModule }
  TPhaseRotatorModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure DataModuleResume(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMidi(Sender: TObject; const MidiEvent: TVstMidiEvent);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FAllpass         : array of array of TBasicAllpassFilter;
    FOrder           : Integer;
    FCriticalSection : TCriticalSection;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF}
  {$IFDEF MSWINDOWS} Registry, {$ENDIF} DAV_Common, PhaseRotatorGUI;

{$IFDEF MSWINDOWS}
const
  CRegKeyRoot = 'Software\Delphi ASIO & VST Project\Phase Rotator';
{$ENDIF}

procedure TPhaseRotatorModule.VSTModuleCreate(Sender: TObject);
var
  MaxStages : Integer;
begin
 Assert(numInputs = numOutputs);
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF MSWINDOWS}
 with TRegistry.Create do
  try
   RootKey := HKEY_CURRENT_USER;
   if OpenKey(CRegKeyRoot, False) then
    try
     if ValueExists('Maximum Stages') then
      begin
       MaxStages := StrToInt(ReadString('Maximum Stages'));
       if MaxStages > ParameterProperties[1].Min
        then ParameterProperties[1].Max := MaxStages;
      end;
    except
    end;
  finally
   Free;
  end;
 {$ENDIF}

end;

procedure TPhaseRotatorModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TPhaseRotatorModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 SetLength(FAllpass, numInputs);

 // create allpass filters
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  begin
   SetLength(FAllpass[ChannelIndex], Round(ParameterProperties[1].Max));
   for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
    begin
     FAllpass[ChannelIndex, BandIndex] := TBasicAllpassFilter.Create;
     with FAllpass[ChannelIndex, BandIndex] do
      begin
       SampleRate := Self.SampleRate;
       Frequency  := 200;
       Bandwidth  := 1.4;
      end;
    end;
  end;

 // initialize default parameters
 Parameter[0] := 200;
 Parameter[1] := 2;
 Parameter[2] := 1.4;

 // set editor form class
 EditorFormClass := TFmPhaseRotator;
end;

procedure TPhaseRotatorModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 for ChannelIndex := 0 to Length(FAllpass) - 1 do
  for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1
   do FreeAndNil(FAllpass[ChannelIndex, BandIndex]);
end;

procedure TPhaseRotatorModule.DataModuleResume(Sender: TObject);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FAllpass) - 1 do
   for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
    if Assigned(FAllpass[ChannelIndex, BandIndex])
      then FAllpass[ChannelIndex, BandIndex].ResetStates;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TPhaseRotatorModule.ParameterFrequencyDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := AnsiString(FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 3, 3));
end;

procedure TPhaseRotatorModule.ParameterFrequencyLabel(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TPhaseRotatorModule.ParameterOrderDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(Parameter[Index])));
end;

procedure TPhaseRotatorModule.ParameterBandwidthDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 2, 2));
end;

procedure TPhaseRotatorModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FAllpass) - 1 do
   for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
    if Assigned(FAllpass[ChannelIndex, BandIndex])
     then FAllpass[ChannelIndex, BandIndex].Bandwidth := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPhaseRotator
  then TFmPhaseRotator(EditorForm).UpdateBandwidth;
end;

procedure TPhaseRotatorModule.ParameterFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FAllpass) - 1 do
   for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
    if Assigned(FAllpass[ChannelIndex, BandIndex])
     then FAllpass[ChannelIndex, BandIndex].Frequency := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPhaseRotator
  then TFmPhaseRotator(EditorForm).UpdateFrequency;
end;

procedure TPhaseRotatorModule.ParameterOrderChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FOrder := Round(Value);
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmPhaseRotator
  then TFmPhaseRotator(EditorForm).UpdateStages;
end;

procedure TPhaseRotatorModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
  BandIndex    : Integer;
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   for ChannelIndex := 0 to Length(FAllpass) - 1 do
    for BandIndex := 0 to Length(FAllpass[ChannelIndex]) - 1 do
     if Assigned(FAllpass[ChannelIndex, BandIndex])
      then FAllpass[ChannelIndex, BandIndex].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TPhaseRotatorModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  BandIndex    : Integer;
  Data         : Double;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FAllpass) - 1 do
   for SampleIndex := 0 to SampleFrames - 1 do
    begin
     Data := Inputs[ChannelIndex, SampleIndex];
     for BandIndex := 0 to FOrder - 1
      do Data := FAllpass[ChannelIndex, BandIndex].ProcessSample64(Data);
     Outputs[ChannelIndex, SampleIndex] := Data;
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TPhaseRotatorModule.VSTModuleProcessMidi(Sender: TObject;
  const MidiEvent: TVstMidiEvent);
var
  Status : Integer;
  CCData : Single;
begin
 Status := MidiEvent.MidiData[0] and $F0; // channel information is removed

 if (Status = $B0) then // midi CC ?
  begin
   CCData := MidiEvent.MidiData[2] / 127; // CC data
   case MidiEvent.MidiData[1] of // midi CC#
    70: Parameter[0] := FreqLinearToLog(CCData);
    71: Parameter[1] := 4 * CCData;
    72: Parameter[2] := 0.1 * Power(10, 2 * CCData);
   end;
  end;
end;

end.
