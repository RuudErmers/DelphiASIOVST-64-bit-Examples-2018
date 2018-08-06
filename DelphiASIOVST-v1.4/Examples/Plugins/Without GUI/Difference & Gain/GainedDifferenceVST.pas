unit GainedDifferenceVST;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

type
  TGainedDifferenceModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray;
      const SampleFrames: Cardinal);
    procedure ParameterGainChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FGain : Double;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} DAV_Common;

procedure TGainedDifferenceModule.VSTModuleOpen(Sender: TObject);
begin
  // initialize parameter
  Parameter[0] := 0;
end;

procedure TGainedDifferenceModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
  FGain := dB_to_Amp(Value);
end;

procedure TGainedDifferenceModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Outputs[0, SampleIndex] := FGain * (Inputs[0, SampleIndex] -
      Inputs[1, SampleIndex]);
    Outputs[1, SampleIndex] := Outputs[0, SampleIndex];
  end;
end;

end.
