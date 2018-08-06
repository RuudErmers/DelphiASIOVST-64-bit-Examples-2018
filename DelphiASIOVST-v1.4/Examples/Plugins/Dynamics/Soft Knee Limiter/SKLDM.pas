unit SKLDM;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TSoftKneeLimiterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLSoftKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    FSoftKneeLimiters : array [0..1] of TSimpleSoftKneeLimiter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, EditorFrm;

procedure TSoftKneeLimiterDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSoftKneeLimiters[0] := TSimpleSoftKneeLimiter.Create;
 FSoftKneeLimiters[1] := TSimpleSoftKneeLimiter.Create;

 // Initial Parameters
 Parameter[0] := 0;
 Parameter[1] := 1;
 Parameter[2] := 5;
 Parameter[3] := 40;
 Parameter[4] := 0;

 // set editor form class
 EditorFormClass := TEditorForm;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSoftKneeLimiters[0]);
 FreeAndNil(FSoftKneeLimiters[1]);
end;

procedure TSoftKneeLimiterDataModule.SKLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeLimiters[0]) then FSoftKneeLimiters[0].Threshold_dB := Value;
 if Assigned(FSoftKneeLimiters[1]) then FSoftKneeLimiters[1].Threshold_dB := Value;
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateThreshold;
end;

procedure TSoftKneeLimiterDataModule.SKLMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeLimiters[0]) then FSoftKneeLimiters[0].MakeUpGain_dB := Value;
 if Assigned(FSoftKneeLimiters[1]) then FSoftKneeLimiters[1].MakeUpGain_dB := Value;
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateMakeUp;
end;

procedure TSoftKneeLimiterDataModule.SKLSoftKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeLimiters[0]) then FSoftKneeLimiters[0].Knee_dB := Value;
 if Assigned(FSoftKneeLimiters[1]) then FSoftKneeLimiters[1].Knee_dB := Value;
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateSoftKnee;
end;

procedure TSoftKneeLimiterDataModule.SKLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeLimiters[0]) then FSoftKneeLimiters[0].Release := Value;
 if Assigned(FSoftKneeLimiters[1]) then FSoftKneeLimiters[1].Release := Value;
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateRelease;
end;

procedure TSoftKneeLimiterDataModule.SKLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeLimiters[0]) then FSoftKneeLimiters[0].Attack := Value;
 if Assigned(FSoftKneeLimiters[1]) then FSoftKneeLimiters[1].Attack := Value;
 if EditorForm is TEditorForm
  then TEditorForm(EditorForm).UpdateAttack;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleProcess(const Inputs, Outputs:
  TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSoftKneeLimiters[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSoftKneeLimiters[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSoftKneeLimiters[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSoftKneeLimiters[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FSoftKneeLimiters[0])
  then FSoftKneeLimiters[0].SampleRate := SampleRate;
 if Assigned(FSoftKneeLimiters[1])
  then FSoftKneeLimiters[1].SampleRate := SampleRate;
end;

end.
