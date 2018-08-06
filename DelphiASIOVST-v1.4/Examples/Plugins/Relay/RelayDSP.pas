unit RelayDSP;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspRelay;

type
  TRelayModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure VSTModuleClose(Sender: TObject);
  private
    FScaleFactor : array [0..1] of Single;
    FRelays      : array of TDspRelay32;
  public
  end;

implementation

{$R *.DFM}

uses
  DAV_Common, RelayGUI;

procedure TRelayModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = numOutputs);
end;

procedure TRelayModule.VSTModuleOpen(Sender: TObject);
var
  RelayIndex : Integer;
begin
 SetLength(FRelays, numInputs);

 for RelayIndex := 0 to numInputs - 1 do
  begin
   FRelays[RelayIndex] := TDspRelay32.Create;
   FRelays[RelayIndex].Upper := 0.5;
   FRelays[RelayIndex].Lower := -0.5;
  end;

 // set editor form class
 EditorFormClass := TFmRelay;
end;

procedure TRelayModule.VSTModuleClose(Sender: TObject);
var
  RelayIndex : Integer;
begin
 for RelayIndex := 0 to numInputs - 1
  do FreeAndNil(FRelays[RelayIndex]);
end;

procedure TRelayModule.ParameterInputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FScaleFactor[0] := dB_to_Amp(Value);
end;

procedure TRelayModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FScaleFactor[1] := dB_to_Amp(Value);
end;

procedure TRelayModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 for ChannelIndex := 0 to Length(FRelays) - 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    Outputs[ChannelIndex, SampleIndex] := FScaleFactor[1] *
      FRelays[ChannelIndex].ProcessSample32(FScaleFactor[0] *
      Inputs[ChannelIndex, SampleIndex]);
   end;
end;

end.
