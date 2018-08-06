unit RpeqDSP;

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms,  DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics;

type
  TRequantizedPeakFilter = class(TBasicPeakFilter)
  private
    FBitMul   : Double;
    FBitDiv   : Double;
    FBitDepth : Single;
    procedure SetBitDepth(const Value: Single);
    procedure BitDepthChanged;
  public
    function ProcessSample64(Input: Double): Double; override;
    constructor Create; override;

    property BitDepth: Single read FBitDepth write SetBitDepth;
  end;

  TRpeqModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray;
      const SampleFrames: Cardinal);
    procedure ParameterBitDepthChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure ParameterBWChange(Sender: TObject;
      const Index: Integer; var Value: Single);
  private
    FFilter : array [0..1] of TRequantizedPeakFilter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, RpeqGUI;

{ TRequantizedPeakFilter }

constructor TRequantizedPeakFilter.Create;
begin
 inherited;
 FBitDepth := 24;
 BitDepthChanged;
end;

function TRequantizedPeakFilter.ProcessSample64(Input: Double): Double;
begin
 Result    := FNominator[0] * Input + FState[0];
 FState[0] := Round(FBitMul * (FNominator[1] * Input - FDenominator[1] * Result + FState[1])) * FBitDiv;
 FState[1] := Round(FBitMul * (FNominator[2] * Input - FDenominator[2] * Result)) * FBitDiv;
end;

procedure TRequantizedPeakFilter.SetBitDepth(const Value: Single);
begin
 if FBitDepth <> Value then
  begin
   FBitDepth := Value;
   BitDepthChanged;
  end;
end;

procedure TRequantizedPeakFilter.BitDepthChanged;
begin
 FBitMul := Power(2, FBitDepth + 1) - 1;
 FBitDiv := 1 / FBitMul;
end;


{ TRpeqModule }

procedure TRpeqModule.VSTModuleOpen(Sender: TObject);
begin
 EditorFormClass := TFmRpeq;

 FFilter[0] := TRequantizedPeakFilter.Create;
 FFilter[1] := TRequantizedPeakFilter.Create;

 Parameter[0] := 1000;
 Parameter[1] := 0;
 Parameter[2] := 1;
 Parameter[3] := 24;
end;

procedure TRpeqModule.ParameterBitDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1
  do FFilter[ChannelIndex].BitDepth := Value;
end;

procedure TRpeqModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1
  do FFilter[ChannelIndex].Frequency := Value;
end;

procedure TRpeqModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1
  do FFilter[ChannelIndex].Gain := Value;
end;

procedure TRpeqModule.ParameterBWChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to Length(FFilter) - 1
  do FFilter[ChannelIndex].Bandwidth := Value;
end;

procedure TRpeqModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  for ChannelIndex := 0 to Length(FFilter) - 1
   do Outputs[ChannelIndex, SampleIndex] :=  FFilter[ChannelIndex].ProcessSample64(Inputs[ChannelIndex, SampleIndex]);
end;

end.
