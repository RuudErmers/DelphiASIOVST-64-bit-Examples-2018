unit PulsingDSP;

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_DspPulsing, DAV_VSTModule;

type
  TPulsingDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPeriodChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSlewrateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMaximumChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMinimumChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPulsing : array of TSymetricPulsing;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  PulsingGUI;

procedure TPulsingDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 SetLength(FPulsing, numInputs);

 for Channel := 0 to Length(FPulsing) - 1 do
  begin
   FPulsing[Channel] := TSymetricPulsing.Create;
  end;

 // initialize parameters
 Parameter[0] := 1000;
 Parameter[1] := 0;
 Parameter[2] := -40;
 Parameter[3] := 400;

 // set editor form class
 EditorFormClass := TFmPulsing;
end;

procedure TPulsingDataModule.ParameterPeriodChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FPulsing) - 1 do
  if Assigned(FPulsing[Channel]) then
   with FPulsing[Channel] do
    begin
     Period_s := 1E-3 * Value;
//     Length_s := 0.5 * 1E-3 * Value;
    end;
end;

procedure TPulsingDataModule.ParameterSlewrateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FPulsing) - 1 do
  if Assigned(FPulsing[Channel])
   then FPulsing[Channel].SlewRate_dB_s := Value;
end;

procedure TPulsingDataModule.ParameterMinimumChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FPulsing) - 1 do
  if Assigned(FPulsing[Channel])
   then FPulsing[Channel].Minimum_dB := Value;
end;

procedure TPulsingDataModule.ParameterMaximumChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FPulsing) - 1 do
  if Assigned(FPulsing[Channel])
   then FPulsing[Channel].Maximum_dB := Value;
end;

procedure TPulsingDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FPulsing) - 1
  do FreeAndNil(FPulsing[Channel]);
end;

procedure TPulsingDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FPulsing) - 1
   do FPulsing[Channel].SampleRate := Abs(SampleRate);
end;

procedure TPulsingDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 for Channel := 0 to Length(FPulsing) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FPulsing[Channel].ProcessSample32(Inputs[Channel, Sample]);
end;

end.
