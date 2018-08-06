unit AnalyserForm;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, Spin, Math, TeEngine, 
  Series, TeeProcs, Chart, DAV_ASIOHost, DAV_Types, DAV_DspFilterChebyshevType1, 
  GLScene, GLObjects, GLWin32Viewer, GLCoordinates, GLCrossPlatform, 
  BaseClasses;

const
  CNumHistory = 5;
  CNumFrequencies = 32;
  CThirdOctaveFrequencies : Array [0..cNumFrequencies - 1] of Single =
      (16,20,25,31,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
       1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000);
  CDS = 8;
  CBW = 0.4;

type
  TDownsampleFilterRecord = record
    Lowpass      : TChebyshev1LowpassFilter;
    Highpass     : TChebyshev1HighpassFilter;
    Downsampling : Integer;
    RMS          : Double;
  end;

  TFmAnalyser = class(TForm)
    ASIOHost: TASIOHost;
    BarGraphScene: TGLScene;
    BtAnalyse: TButton;
    BtControlPanel: TButton;
    CbChannel: TComboBox;
    CbDriver: TComboBox;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLLight: TGLLightSource;
    GLSceneViewer: TGLSceneViewer;
    LbChannels: TLabel;
    LbFullScaleUnit: TLabel;
    LbDrivername: TLabel;
    LbFullscale: TLabel;
    LbSpeed: TLabel;
    RbFast: TRadioButton;
    RbMedium: TRadioButton;
    RbSlow: TRadioButton;
    SeFullscaleGain: TSpinEdit;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AnalyserChartDblClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BSDownSampled(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BtAnalyseClick(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure CbDriverChange(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure LbDrivernameClick(Sender: TObject);
    procedure RbFastClick(Sender: TObject);
    procedure RbMediumClick(Sender: TObject);
    procedure RbSlowClick(Sender: TObject);
    procedure SeFullscaleGainChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FUseDownsampling      : Boolean;
    FOldMousePoint        : TPoint;
    FSampleRateReciprocal : Double;
    function GetSamplerate: Single;
    function GetBandReserve: Single;
    procedure Zoom(Value: Single);
    procedure SetBandReserve(const Value: Single);
    procedure SetUseDownsampling(const Value: Boolean);
    procedure CalculateSmoothingFactor;
  protected
    FMaxDSStages     : Integer;
    FDownSampleCount : Integer;
    FDownSampleMax   : Integer;
    FBandReserve     : Double;
    FHistoryCount    : Integer;

    FFilterArray     : Array [0..cNumFrequencies - 1] of TDownsampleFilterRecord;
    FCubeArray       : Array [0..CNumHistory - 1, 0..cNumFrequencies - 1] of TGLCube;

    FChannelNr       : Integer;
    FSpeedConst      : Array [0..1] of Single;
    FFSGain          : Single;
    procedure UpdateBarGraph; virtual;
    procedure UpdateFilters; virtual;
    procedure DownsamplingChanged; virtual;
  public
    property BandReserve: Single read GetBandReserve write SetBandReserve;
    property UseDownsampling: Boolean read FUseDownsampling write SetUseDownsampling default True;
    property SampleRate: Single read GetSamplerate;
  end;

var
  FmAnalyser: TFmAnalyser;

implementation

{$R *.DFM}

uses
  Inifiles, Registry, VectorGeometry, DAV_Common;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var
  Band    : Integer;
  History : Integer;
begin
 FChannelNr := 0;
 FSpeedConst[0] := 0.999;
 FSpeedConst[1] := 1 - FSpeedConst[0];
 FHistoryCount := 0;
 FFSGain := SEFullscaleGain.Value;
 CbDriver.Items := ASIOHost.DriverList;

 // make sure any ASIO driver is present
 if CbDriver.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 FBandReserve := 0.25;
 UpdateFilters;

 // add bands
 for History := 0 to CNumHistory - 1 do
  for Band := 0 to CNumFrequencies - 1 do
   begin
    FCubeArray[History, Band] := TGLCube.Create(BarGraphScene);
    with FCubeArray[History, Band] do
     begin
      CubeWidth := 0.06;
      CubeHeight := 1;
      CubeDepth := 0.1;
      Position.X := 0.08 * (Band - 16);
      Position.Z := -0.20 * History;
      BarGraphScene.Objects.AddChild(FCubeArray[History, Band]);
     end;
   end;

 GLCamera.TargetObject := FCubeArray[0, 16];
// GLLight.Position.Style :=

 UseDownsampling := True;
 DownsamplingChanged;

 if FDownSampleCount = -1
  then ASIOHost.OnBufferSwitch32 := BSNormal
  else ASIOHost.OnBufferSwitch32 := BSDownSampled;
end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 ASIOHost.Active := False;

 // free filters
 for i := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FFilterArray[i].Lowpass);
   FreeAndNil(FFilterArray[i].Highpass);
  end;
end;

procedure TFmAnalyser.FormShow(Sender: TObject);
begin
 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   CbDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if CbDriver.ItemIndex >= 0 then CbDriverChange(CbDriver);
   CbChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   SEFullscaleGain.Value := ReadInteger('Audio', 'Fullscale Gain', 0);
  finally
   Free;
  end;
end;

procedure TFmAnalyser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // save settings
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', CbDriver.ItemIndex);
   WriteInteger('Audio', 'Channels', CbChannel.ItemIndex);
   WriteInteger('Audio', 'Fullscale Gain', SEFullscaleGain.Value);
  finally
   Free;
  end;
end;

procedure TFmAnalyser.UpdateFilters;
var
  Band         : Integer;
  Downsampling : Integer;
  DesiredFreq  : Double;
const
  HalfThirdMulFak : Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
begin
 Downsampling := 0;

 for Band := 0 to Length(FFilterArray) - 1 do
  begin
   // Lowpass
   DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] * HalfThirdMulFak;
   if DesiredFreq > 0.499 * SampleRate then DesiredFreq := 0.499 * SampleRate;   

   if UseDownsampling then
    while ((2 * DesiredFreq * FSampleRateReciprocal) * (1 shl Downsampling)) < FBandReserve
     do Inc(Downsampling);

   // eventually create filter
   if not assigned(FFilterArray[Band].Lowpass)
    then FFilterArray[Band].Lowpass := TChebyshev1LowpassFilter.Create(10);

   with FFilterArray[Band].Lowpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
   FFilterArray[Band].Downsampling := (1 shl Downsampling);

   // Highpass
   DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] / HalfThirdMulFak;

   // eventually create filter
   if not assigned(FFilterArray[Band].Highpass)
    then FFilterArray[Band].Highpass := TChebyshev1HighpassFilter.Create(12);
    
   with FFilterArray[Band].Highpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
  end;
 FDownSampleMax := 1 shl Downsampling;
end;

procedure TFmAnalyser.Zoom(Value: Single);
var
  vect : TVector;
begin
 if GLSceneViewer.Camera = GLCamera then
  with GLCamera do
   if Assigned(TargetObject) then
    begin
     vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
     if ((VectorLength(vect) > 1.2) or (Value > 1)) and
        ((VectorLength(vect) < 10)  or (Value < 1)) then
      begin
       ScaleVector(vect, Value - 1);
       AddVector(vect, AbsolutePosition);
       if Assigned(Parent)
        then vect := Parent.AbsoluteToLocal(vect);
       Position.AsVector := vect;
      end;
    end
end;

function TFmAnalyser.GetBandReserve: Single;
begin
 result := 100 * FBandReserve;
end;

function TFmAnalyser.GetSamplerate: Single;
begin
 result := ASIOHost.SampleRate;
end;

procedure TFmAnalyser.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FOldMousePoint.X := X;
 FOldMousePoint.Y := Y;
end;

procedure TFmAnalyser.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  Scale = 1/40;
var
   originalT2C, normalT2C, normalCameraRight, newPos : TVector;
   pitchNow, dist: Single;
begin
 if ssLeft in Shift then
  begin
   with GLSceneViewer.Camera do
    begin
     originalT2C := VectorSubtract(AbsolutePosition, GLDummyCube.AbsolutePosition);
     SetVector(normalT2C, originalT2C);
     dist := VectorLength(normalT2C);
     NormalizeVector(normalT2C);
     normalCameraRight := VectorCrossProduct(AbsoluteUp, normalT2C);
     if VectorLength(normalCameraRight) < 0.001
      then SetVector(normalCameraRight, XVector) // arbitrary vector
      else NormalizeVector(normalCameraRight);
     pitchNow := Math.ArcCos(VectorDotProduct(AbsoluteUp, normalT2C));
     if not (ssAlt in Shift)
      then pitchNow := ClampValue(pitchNow + Math.DegToRad(FOldMousePoint.Y - Y), 0.002, PI - 0.77);
     SetVector(normalT2C, AbsoluteUp);
     RotateVector(normalT2C, normalCameraRight, -pitchNow);
     if not (ssShift in Shift)
      then RotateVector(normalT2C, AbsoluteUp, -Math.DegToRad(FOldMousePoint.X - X));
     ScaleVector(normalT2C, dist);
     newPos := VectorAdd(AbsolutePosition, VectorSubtract(normalT2C, originalT2C));
     if Assigned(Parent) then newPos := Parent.AbsoluteToLocal(newPos);
     Position.AsVector := newPos;
     case GLLight.Position.Style of
      csPoint  : GLLight.Position.SetPoint(newPos);
      csVector : GLLight.Position.SetVector(newPos);
     end;
    end;
   FOldMousePoint.X := X;
   FOldMousePoint.Y := Y;
  end else
 if ssRight in Shift then
  begin
   Zoom(Power(0.995, (FOldMousePoint.Y - Y)));
   FOldMousePoint.X := X;
   FOldMousePoint.Y := Y;
  end;
end;

procedure TFmAnalyser.GLSceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
const
  CScale = 1/120;
begin
 Zoom(Power(0.9, WheelDelta * CScale));
 Handled := true
end;

procedure TFmAnalyser.RbFastClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.99;
 CalculateSmoothingFactor;
end;

procedure TFmAnalyser.RbMediumClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.999;
 CalculateSmoothingFactor;
end;

procedure TFmAnalyser.RbSlowClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.9999;
 CalculateSmoothingFactor;
end;

procedure TFmAnalyser.CalculateSmoothingFactor;
begin
 FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TFmAnalyser.SeFullscaleGainChange(Sender: TObject);
begin
 FFSGain := SEFullscaleGain.Value;
// AnalyserChart.LeftAxis.Maximum := FFSGain+20;
end;

procedure TFmAnalyser.SetBandReserve(const Value: Single);
begin
 FBandReserve := 0.01 * Value;
end;

procedure TFmAnalyser.SetUseDownsampling(const Value: Boolean);
begin
 if FUseDownsampling <> Value then
  begin
   FUseDownsampling := Value;
   DownsamplingChanged;
  end;
end;

procedure TFmAnalyser.DownsamplingChanged;
begin
 if FUseDownsampling
  then FDownSampleCount := 0
  else FDownSampleCount := -1
end;

procedure TFmAnalyser.CbDriverChange(Sender: TObject);
var
  i : Integer;
begin
 BtControlPanel.Enabled := False;
 BtAnalyse.Enabled := False;
 CbDriver.ItemIndex := CbDriver.Items.IndexOf(CbDriver.Text);
 if CbDriver.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := CbDriver.ItemIndex;
   CbChannel.Clear;
   for i := 0 to ASIOHost.InputChannelCount - 1
    do CbChannel.Items.Add(string(ASIOHost.InputChannelInfos[i].Name));
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', CbDriver.ItemIndex);
    finally
     Free;
    end;
   BtControlPanel.Enabled := True;
   BtAnalyse.Enabled := True;
   CbChannel.ItemIndex := 0;
  end;
end;

procedure TFmAnalyser.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmAnalyser.BtAnalyseClick(Sender: TObject);
begin
 if BtAnalyse.Caption = 'Analyse' then
  begin
   ASIOHost.Active := True; // Start Audio
   BtAnalyse.Caption := 'Stop';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   BtAnalyse.Caption := 'Analyse';
  end;
 Timer.Enabled := ASIOHost.Active;
end;

procedure TFmAnalyser.UpdateBarGraph;
var
  Band    : Integer;
  History : Integer;
  Value   : Double;
begin
 for Band := 0 to cNumFrequencies - 1 do
  begin
   if FHistoryCount = 0 then
    for History := 1 to CNumHistory - 1 do
     begin
      FCubeArray[History, Band].CubeHeight := FCubeArray[History - 1, Band].CubeHeight;
      FCubeArray[History, Band].StructureChanged;
     end;

   Value := (FFilterArray[cNumFrequencies - Band - 1].RMS + FFSGain) / FFSGain;
   if Value < 0 then Value := 0;

   FCubeArray[0, Band].CubeHeight := Value;
   FCubeArray[0, Band].StructureChanged;
  end;

(*
 if FHistoryCount < 1
  then inc(FHistoryCount)
  else FHistoryCount := 0;
*)
end;

procedure TFmAnalyser.AnalyserChartDblClick(Sender: TObject);
begin
 if GLSceneViewer.Align <> alClient
  then GLSceneViewer.Align := alClient
  else
   begin
    GLSceneViewer.Align := alBottom;
    GLSceneViewer.Top := 88;
    GLSceneViewer.Height := ClientHeight - 88;
   end;
end;

procedure TFmAnalyser.TimerTimer(Sender: TObject);
begin
 UpdateBarGraph;
end;

procedure TFmAnalyser.LbDrivernameClick(Sender: TObject);
begin
 if FDownSampleCount > 0
  then FDownSampleCount := -1
  else FDownSampleCount := 0;

 UpdateFilters;
end;

procedure TFmAnalyser.BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  i,j : Integer;
  d,z : Double;
const
  cDenorm = 1E-32;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   d := InBuffer[FChannelNr,i];
   for j := 0 to CNumFrequencies - 1 do
    begin
     d := FFilterArray[j].Lowpass.ProcessSample64(d + cDenorm);
     z := FFilterArray[j].Highpass.ProcessSample64(d + cDenorm);
     FFilterArray[j].RMS := FSpeedConst[0] * FFilterArray[j].RMS + FSpeedConst[1] * Amp_to_dB(abs(z));
    end;
  end;
end;

procedure TFmAnalyser.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if SampleRate = 0
  then raise Exception.Create('Samplerate may not be zero!');
 FSampleRateReciprocal := 1 / SampleRate;
 UpdateFilters;
end;

procedure TFmAnalyser.BSDownSampled(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  i, j : Integer;
  d, z, s : Double;
const
  cDenorm = 1E-32;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   d := InBuffer[FChannelNr, i];
   for j := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[j].Downsampling) <> 0
      then Break;

     d := FFilterArray[j].Lowpass.ProcessSample64(d + cDenorm);
     z := FFilterArray[j].Highpass.ProcessSample64(d + cDenorm);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[j].Downsampling + 1);
     FFilterArray[j].RMS := s * FFilterArray[j].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
   inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
  end;
end;

initialization
  Set8087CW(Default8087CW or $3F);

end.
