unit CCPmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs, GR32_Image, GR32_Chart, StdCtrls, MFControlsEdit, MFControlsSpinEdit,
  DAV_DspDynamics, DAV_DspLevelingAmplifier, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiGraphXY;

type
  TFmDynamicCurvePlot = class(TForm)
    LbKnee: TLabel;
    LbRatio: TLabel;
    LbThreshold: TLabel;
    DialThreshold: TGuiDial;
    DialRatio: TGuiDial;
    DialKnee: TGuiDial;
    LbThresholdValue: TLabel;
    LbRatioValue: TLabel;
    LbKneeValue: TLabel;
    DynamicsChart: TGuiGraphXY;
    CBType: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function FunctionEvaluation(Sender: TObject; X: Double): Double;
    procedure SEThresholdChange(Sender: TObject);
    procedure SERatioChange(Sender: TObject);
    procedure SEKneeChange(Sender: TObject);
    procedure CBTypeChange(Sender: TObject);
  private
    FDynamics: TObjectList;
  end;

var
  FmDynamicCurvePlot: TFmDynamicCurvePlot;

implementation

{$R *.dfm}

uses
  PngImage, DAV_Common;

procedure TFmDynamicCurvePlot.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;

begin
 PngBmp := TPngObject.Create;
 try
(*
  RS := TResourceStream.Create(hInstance, 'CCPMakeUp', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialKnee.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
*)

  RS := TResourceStream.Create(hInstance, 'CCPRatio', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialRatio.DialBitmap.Assign(PngBmp);
   DialKnee.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;

  RS := TResourceStream.Create(hInstance, 'CCPThreshold', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialThreshold.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;

 FDynamics := TObjectList.Create;
 FDynamics.Add(TSimpleSoftKneeLimiter.Create);

 with (DynamicsChart.SeriesCollection.Add) do
  begin
   SeriesClassName := 'TGuiGraphXYFunctionSeries';
   TGuiGraphXYFunctionSeries(Series).OnEvaluate := FunctionEvaluation;
  end;
end;

procedure TFmDynamicCurvePlot.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FDynamics);
end;

procedure TFmDynamicCurvePlot.FormShow(Sender: TObject);
begin
 SEThresholdChange(Sender);
end;

procedure TFmDynamicCurvePlot.CBTypeChange(Sender: TObject);
var
  OldObject : TObject;
begin
 OldObject := FDynamics[0];
 FDynamics[0] := FindClass(CBType.Text).Create;
 SERatioChange(nil);
 SEThresholdChange(nil);
 SEKneeChange(nil);
 FreeAndNil(OldObject);
 DynamicsChart.UpdateGraph;
end;

function TFmDynamicCurvePlot.FunctionEvaluation(Sender: TObject;
  X: Double): Double;
begin
 if Sender is TCustomGuiGraphXYSeries then
  with TCustomGuiGraphXYSeries(Sender) do
   begin
    if FDynamics[Tag] is TCustomDynamicProcessor then
     with TCustomDynamicProcessor(FDynamics[Tag])
      do result := CharacteristicCurve_dB(x)
    else
    if FDynamics[Tag] is TCustomLevelingAmplifier then
     with TCustomLevelingAmplifier(FDynamics[Tag])
      do result := CharacteristicCurve_dB(x)
    else result := 0;
   end
 else result := 0;  
end;

procedure TFmDynamicCurvePlot.SERatioChange(Sender: TObject);
var
  i : Integer;
begin
 // update all dynamics in the list
 for i := 0 to FDynamics.Count - 1 do
  if FDynamics[i] is TCustomTimeConstantRatioDynamics then
   with TCustomTimeConstantRatioDynamics(FDynamics[i])
    do Ratio := 1 / DialRatio.Position
   else
  if FDynamics[i] is TCustomLevelingAmplifier then
   with TCustomLevelingAmplifier(FDynamics[i])
    do Ratio := 1 / DialRatio.Position;

 LbRatioValue.Caption := '1 : ' + FloatToStrF(DialRatio.Position, ffGeneral, 3, 3);
 DynamicsChart.UpdateGraph;
end;

procedure TFmDynamicCurvePlot.SEKneeChange(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to FDynamics.Count - 1 do
  if FDynamics[i] is TClassicSoftKneeGate then
   with TClassicSoftKneeGate(FDynamics[i])
    do Knee_dB := DialKnee.Position
   else
  if FDynamics[i] is TSoftDirectGate then
   with TSoftDirectGate(FDynamics[i])
    do Knee_dB := DialKnee.Position
   else
  if FDynamics[i] is TAdvancedGate then
   with TAdvancedGate(FDynamics[i])
    do Knee_dB := DialKnee.Position
   else
  if FDynamics[i] is TCustomKneeCompressor then
   with TCustomKneeCompressor(FDynamics[i])
    do Knee_dB := DialKnee.Position
   else
  if FDynamics[i] is TSimpleSoftKneeLimiter then
   with TSimpleSoftKneeLimiter(FDynamics[i])
    do Knee_dB := DialKnee.Position else
  if FDynamics[i] is TCustomLevelingAmplifier then
   with TCustomLevelingAmplifier(FDynamics[i])
    do Knee := DialKnee.Position;

 LbKneeValue.Caption := FloatToStrF(DialKnee.Position, ffGeneral, 3, 3) + ' dB';
 DynamicsChart.UpdateGraph;
end;

procedure TFmDynamicCurvePlot.SEThresholdChange(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to FDynamics.Count - 1 do
  if FDynamics[i] is TCustomDynamicProcessor then
   with TCustomDynamicProcessor(FDynamics[i])
    do Threshold_dB := DialThreshold.Position
   else
  if FDynamics[i] is TCustomLevelingAmplifier then
   with TCustomLevelingAmplifier(FDynamics[i])
    do Threshold := DialThreshold.Position;

 LbThresholdValue.Caption := FloatToStrF(DialThreshold.Position, ffGeneral, 3, 3) + ' dB';
 DynamicsChart.UpdateGraph;
end;

initialization
  RegisterClass(TSimpleDirectGate);
  RegisterClass(TSoftDirectGate);
  RegisterClass(TBrickwallLimiter);
  RegisterClass(TSoftBrickwallLimiter);
  RegisterClass(TSimpleSoftBrickwallLimiter);
  RegisterClass(TLimiter);
  RegisterClass(TSoftKneeLimiter);
  RegisterClass(TSimpleSoftKneeLimiter);
  RegisterClass(TClassicGate);
  RegisterClass(TClassicSoftRangeGate);
  RegisterClass(TClassicSoftKneeGate);
  RegisterClass(TAdvancedGate);
  RegisterClass(TSimpleCompressor);
  RegisterClass(TSoftKneeCompressor);
  RegisterClass(TSimpleRMSCompressor);
  RegisterClass(TCompressor);

end.
