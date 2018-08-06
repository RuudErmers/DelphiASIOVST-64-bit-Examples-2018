unit HysteresisDSP;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_DspPreisach;

type
  THysteresisModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject;
      const Index: Integer; var Value: Single);
  private
    FScaleFactor : array [0..1] of Single;
    FHysteresis  : array of TDspPreisach32;
  public
  end;

implementation

{$R *.DFM}

uses
  DAV_Common, HysteresisGUI;

procedure THysteresisModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = numOutputs);
end;

procedure THysteresisModule.VSTModuleOpen(Sender: TObject);
var
  HysteresisIndex : Integer;
begin
 SetLength(FHysteresis, numInputs);

 for HysteresisIndex := 0 to numInputs - 1 do
  begin
   FHysteresis[HysteresisIndex] := TDspPreisach32.Create;
   FHysteresis[HysteresisIndex].HysteronResolution := 128;
  end;

 // set editor form class
 EditorFormClass := TFmHysteresis;
end;

procedure THysteresisModule.VSTModuleClose(Sender: TObject);
var
  HysteresisIndex : Integer;
begin
 for HysteresisIndex := 0 to numInputs - 1
  do FreeAndNil(FHysteresis[HysteresisIndex]);
end;

procedure THysteresisModule.ParameterInputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FScaleFactor[0] := dB_to_Amp(Value);
end;

procedure THysteresisModule.ParameterOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FScaleFactor[1] := dB_to_Amp(Value);
end;

procedure THysteresisModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 for ChannelIndex := 0 to Length(FHysteresis) - 1 do
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    Outputs[ChannelIndex, SampleIndex] := FScaleFactor[1] *
      FHysteresis[ChannelIndex].ProcessSample32(FScaleFactor[0] *
      Inputs[ChannelIndex, SampleIndex]);
   end;
end;

end.
