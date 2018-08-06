unit LinearPhaseLinkwitzRileyDM;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

{$DEFINE UseLowLatencyConvolution}
{$DEFINE UseThread}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, SyncObjs, DAV_Types, DAV_DspFilterButterworth, DAV_DspConvolution,
  DAV_DspPolyphaseDownsampler, DAV_VSTModule;

type
  {$IFDEF UseThread}
  TFilterCalculationThread = class(TThread)
  private
    FPluginModule : TVSTModule;
  public
    constructor Create(PluginModule: TVSTModule); overload;
    procedure Execute; override;
  end;
  {$ENDIF}

  TLinearPhaseLinkwitzRileyDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleResume(Sender: TObject);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterOversamplingChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection    : TCriticalSection;
    FFilterChanged      : Boolean;
    FCurrentKernelSize  : Integer;
    FFilterKernel       : array [0..3] of PDAVSingleFixedArray;
    {$IFDEF UseLowLatencyConvolution}
    FConvolution        : array [0..3] of TLowLatencyConvolution32;
    {$ELSE}
    FConvolution        : array [0..3] of TConvolution32;
    {$ENDIF}
    FLowpassFilter      : array [0..2] of TButterworthLowPassFilter;
    FHighpassFilter     : array [0..2] of TButterworthHighpassFilter;
    {$IFDEF UseThread}
    FFilterUpdateThread : TFilterCalculationThread;
    {$ENDIF}
    FUpdateCounter      : Integer;
    FDownsampler        : TPolyphaseDownsampler64;
    FOversampled        : Boolean;
    procedure FilterKernelChanged;
    procedure UpdateFilterKernel;
    procedure CalculateFilterKernel;
    procedure CalculateLowpassKernel;
    procedure CalculateBandpassKernels;
    procedure CalculateHighpassKernel;
    procedure BeginFilterUpdate;
    procedure EndFilterUpdate;
  public
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Math, DAV_VSTParameters, LinearPhaseLinkwitzRileyGUI;

const
  CMaxIRSize : Integer = 1 shl 14;
  CStepSize : Integer = 1 shl 8;

{ TFilterCalculationThread }

{$IFDEF UseThread}
constructor TFilterCalculationThread.Create(PluginModule: TVSTModule);
begin
  inherited Create(True);
  FPluginModule := PluginModule;
end;

procedure TFilterCalculationThread.Execute;
begin
  inherited;

  with TLinearPhaseLinkwitzRileyDataModule(FPluginModule) do
   begin
    while FFilterChanged do
     begin
      // wait for further changes
      Sleep(2);

      //eventually update filter kernel
      if FFilterChanged
       then CalculateFilterKernel;

      // wait for further changes
      Sleep(2);
     end;
   end;
  Terminate;
end;
{$ENDIF}


{ TLinearPhaseLinkwitzRileyDataModule }

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;

 {$IFDEF Debug}
 numInputs := 2;
 numOutputs := 2;
 {$ENDIF}
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleOpen(Sender: TObject);
var
  BandIndex : Integer;
const
  CFrequencies : array [0..2] of Single = (400, 1000, 6000);
begin
 // create convolver
 for BandIndex := 0 to Length(FConvolution) - 1 do
  begin
   {$IFDEF UseLowLatencyConvolution}
   FConvolution[BandIndex] := TLowLatencyConvolution32.Create;
   FConvolution[BandIndex].MinimumIRBlockOrder := Max(7, CeilLog2(InitialDelay));
   FConvolution[BandIndex].MaximumIRBlockOrder := 18;
   {$ELSE}
   FConvolution[BandIndex] := TConvolution32.Create;
   {$ENDIF}
  end;

 // create lowpass filters
 for BandIndex := 0 to Length(FLowpassFilter) - 1 do
  begin
   FLowpassFilter[BandIndex] := TButterworthLowPassFilter.Create(2);
   FLowpassFilter[BandIndex].Frequency := CFrequencies[BandIndex];
   FLowpassFilter[BandIndex].SampleRate := SampleRate;
  end;

 // create highpass filters
 for BandIndex := 0 to Length(FHighpassFilter) - 1 do
  begin
   FHighpassFilter[BandIndex] := TButterworthHighPassFilter.Create(2);
   FHighpassFilter[BandIndex].Frequency := CFrequencies[BandIndex];
   FHighpassFilter[BandIndex].SampleRate := SampleRate;
  end;

 // create downsampler
 FDownsampler := TPolyphaseDownsampler64.Create;
 FOversampled := False;

 // allocate filter kernel
 for BandIndex := 0 to Length(FFilterKernel) - 1 do
  begin
   GetMem(FFilterKernel[BandIndex], CMaxIRSize * SizeOf(Single));
   FillChar(FFilterKernel[BandIndex]^, CMaxIRSize * SizeOf(Single), 0);
  end;

 BeginFilterUpdate;

 // initialize parameters
 Parameter[0] := 400;
 Parameter[1] := 4;
 Parameter[2] := 1000;
 Parameter[3] := 4;
 Parameter[4] := 6000;
 Parameter[5] := 4;
 Parameter[6] := 0;

 // end filter update and calculate filter kernel
 EndFilterUpdate;

 // set editor form class
 EditorFormClass := TFmLinearPhaseLinkwitzRiley;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleClose(Sender: TObject);
var
  BandIndex : Integer;
begin
{$IFDEF UseThread}
 if Assigned(FFilterUpdateThread) then
  begin
   FFilterUpdateThread.Terminate;
   FFilterUpdateThread.WaitFor;
   FreeAndNil(FFilterUpdateThread);
  end;
{$ENDIF}

 for BandIndex := 0 to Length(FConvolution) - 1
  do FreeAndNil(FConvolution[BandIndex]);
 for BandIndex := 0 to Length(FLowpassFilter) - 1
  do FreeAndNil(FLowpassFilter[BandIndex]);
 for BandIndex := 0 to Length(FFilterKernel) - 1
  do Dispose(FFilterKernel[BandIndex]);
 FreeAndNil(FDownsampler);
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParameterFilterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(0.5 * Parameter[Index])));
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParameterOversamplingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  BandIndex : Integer;
begin
 FOversampled := Value > 0.5;

 for BandIndex := 0 to Length(FLowpassFilter) - 1 do
 if Assigned(FLowpassFilter[BandIndex]) then
  begin
   FCriticalSection.Enter;
   try
    if FOversampled
     then FLowpassFilter[BandIndex].Frequency := 0.5 * Parameter[BandIndex * 2]
     else FLowpassFilter[BandIndex].Frequency := Parameter[BandIndex * 2];
    if Assigned(FHighpassFilter[BandIndex])
     then FHighpassFilter[BandIndex].Frequency := FLowpassFilter[BandIndex].Frequency;
   finally
    FCriticalSection.Leave;
   end;
  end;

 FilterKernelChanged;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.FilterKernelChanged;
begin
 FFilterChanged := True;
 if FUpdateCounter = 0
  then UpdateFilterKernel;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.UpdateFilterKernel;
begin
 {$IFDEF UseThread}
 if not Assigned(FFilterUpdateThread) then
  begin
   FFilterUpdateThread := TFilterCalculationThread.Create(Self);
   FFilterUpdateThread.Start;
  end else
 if FFilterUpdateThread.Terminated then
  begin
   FreeAndNil(FFilterUpdateThread);
   FFilterUpdateThread := TFilterCalculationThread.Create(Self);
   FFilterUpdateThread.Start;
  end;
 {$ELSE}
 CalculateFilterKernel;
 {$ENDIF}
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParameterFilterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  BandIndex : Integer;
begin
 BandIndex := Index div 2;
 Assert(BandIndex in [0..2]);
 if Assigned(FLowpassFilter[BandIndex]) then
  begin
   FCriticalSection.Enter;
   try
    FLowpassFilter[BandIndex].Order := Round(0.5 * Parameter[Index]);
    FHighpassFilter[BandIndex].Order := Round(0.5 * Parameter[Index]);
   finally
    FCriticalSection.Leave;
   end;
   FilterKernelChanged;
  end;

 // update GUI
 if EditorForm is TFmLinearPhaseLinkwitzRiley then
  with TFmLinearPhaseLinkwitzRiley(EditorForm)
   do UpdateOrder;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  BandIndex : Integer;
begin
 BandIndex := Index div 2;
 Assert(BandIndex in [0..2]);
 if Assigned(FLowpassFilter[BandIndex]) then
  begin
   FCriticalSection.Enter;
   try
    if FOversampled
     then FLowpassFilter[BandIndex].Frequency := 0.5 * Value
     else FLowpassFilter[BandIndex].Frequency := Value;
    if Assigned(FHighpassFilter[BandIndex])
     then FHighpassFilter[BandIndex].Frequency := FLowpassFilter[BandIndex].Frequency;
   finally
    FCriticalSection.Leave;
   end;
   FilterKernelChanged;
  end;

 // update GUI
 if EditorForm is TFmLinearPhaseLinkwitzRiley then
  with TFmLinearPhaseLinkwitzRiley(EditorForm)
   do UpdateFrequency;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.CalculateFilterKernel;
var
  SampleIndex : Integer;
  BandIndex   : Integer;
  Data        : TDAV2DoubleArray;
begin
 FFilterChanged := False;
 CalculateLowpassKernel;
 Assert(FCurrentKernelSize <= CMaxIRSize);
 CalculateBandpassKernels;
 CalculateHighpassKernel;

 if FOversampled then
  for SampleIndex := 0 to (FCurrentKernelSize div 2) - 1 do
   for BandIndex := 0 to Length(FFilterKernel) - 1 do
    begin
     Data[0] := FFilterKernel[BandIndex]^[2 * SampleIndex];
     Data[1] := FFilterKernel[BandIndex]^[2 * SampleIndex + 1];
     FFilterKernel[BandIndex]^[SampleIndex] := 4 * FDownsampler.ProcessSample64(Data);
    end;

 FCriticalSection.Enter;
 try
  if FOversampled then
   begin
    FConvolution[0].LoadImpulseResponse(FFilterKernel[0], FCurrentKernelSize div 2);
    FConvolution[1].LoadImpulseResponse(FFilterKernel[1], FCurrentKernelSize div 2);
    FConvolution[2].LoadImpulseResponse(FFilterKernel[2], FCurrentKernelSize div 2);
    FConvolution[3].LoadImpulseResponse(FFilterKernel[3], FCurrentKernelSize div 2);
    {$IFDEF UseLowLatencyConvolution}
    InitialDelay := FConvolution[0].Latency + FCurrentKernelSize div 4;
    {$ENDIF}
   end
  else
   begin
    FConvolution[0].LoadImpulseResponse(FFilterKernel[0], FCurrentKernelSize);
    FConvolution[1].LoadImpulseResponse(FFilterKernel[1], FCurrentKernelSize);
    FConvolution[2].LoadImpulseResponse(FFilterKernel[2], FCurrentKernelSize);
    FConvolution[3].LoadImpulseResponse(FFilterKernel[3], FCurrentKernelSize);
    {$IFDEF UseLowLatencyConvolution}
    InitialDelay := FConvolution[0].Latency + FCurrentKernelSize div 2;
    {$ENDIF}
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.CalculateLowpassKernel;
var
  SampleIndex : Integer;
  KernelSize  : Integer;
  Peak        : Double;
begin
 KernelSize := CStepSize;

 // forward processing
 FLowpassFilter[0].ResetStates;
 FFilterKernel[0]^[0] := FLowpassFilter[0].ProcessSample64(1.0);

 Peak := Abs(FFilterKernel[0]^[0]);
 for SampleIndex := 1 to KernelSize - 1 do
  begin
   FFilterKernel[0]^[SampleIndex] := FLowpassFilter[0].ProcessSample64(0.0);
   Peak := 0.9 * Peak;
   if Abs(FFilterKernel[0]^[SampleIndex]) > Peak
    then Peak := Abs(FFilterKernel[0]^[SampleIndex]);
  end;

 // detect kernel size
 while (Peak > 1E-9) and (KernelSize <= CMaxIRSize shr 1) do
  begin
   for SampleIndex := KernelSize to KernelSize + CStepSize - 1 do
    begin
     FFilterKernel[0]^[SampleIndex] := FLowpassFilter[0].ProcessSample64(0.0);
     Peak := 0.9 * Peak;
     if Abs(FFilterKernel[0]^[SampleIndex]) > Peak
      then Peak := Abs(FFilterKernel[0]^[SampleIndex]);
    end;
   Inc(KernelSize, CStepSize);
  end;
 FCurrentKernelSize := 2 * KernelSize;

 // backward processing
 FLowpassFilter[0].ResetStates;
 Move(FFilterKernel[0]^[0], FFilterKernel[0]^[KernelSize], KernelSize * SizeOf(Single));
 for SampleIndex := 0 to KernelSize - 1
  do FFilterKernel[0]^[SampleIndex] := FLowpassFilter[0].ProcessSample64(FFilterKernel[0]^[FCurrentKernelSize - 1 - SampleIndex]);
 for SampleIndex := KernelSize to FCurrentKernelSize - 1
  do FFilterKernel[0]^[SampleIndex] := FLowpassFilter[0].ProcessSample64(0.0);
end;

procedure TLinearPhaseLinkwitzRileyDataModule.BeginFilterUpdate;
begin
 Inc(FUpdateCounter);
end;

procedure TLinearPhaseLinkwitzRileyDataModule.EndFilterUpdate;
begin
 Dec(FUpdateCounter);
 if FUpdateCounter = 0
  then UpdateFilterKernel;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.CalculateBandpassKernels;
var
  SampleIndex    : Integer;
  HalfKernelSize : Integer;
begin
 HalfKernelSize := FCurrentKernelSize div 2;

 // backward processing
 FHighpassFilter[0].ResetStates;
 FHighpassFilter[1].ResetStates;
 FLowpassFilter[1].ResetStates;
 FLowpassFilter[2].ResetStates;
 FFilterKernel[1]^[HalfKernelSize - 1] := FHighpassFilter[0].ProcessSample64(
   FLowpassFilter[1].ProcessSample64(1.0));
 FFilterKernel[2]^[HalfKernelSize - 1] := FHighpassFilter[1].ProcessSample64(
   FLowpassFilter[2].ProcessSample64(1.0));
 for SampleIndex := 1 to HalfKernelSize - 1 do
  begin
   FFilterKernel[1]^[HalfKernelSize - 1 - SampleIndex] :=
     FHighpassFilter[0].ProcessSample64(FLowpassFilter[1].ProcessSample64(0.0));
   FFilterKernel[2]^[HalfKernelSize - 1 - SampleIndex] :=
     FHighpassFilter[1].ProcessSample64(FLowpassFilter[2].ProcessSample64(0.0));
  end;

 // forward processing
 FHighpassFilter[0].ResetStates;
 FHighpassFilter[1].ResetStates;
 FLowpassFilter[1].ResetStates;
 FLowpassFilter[2].ResetStates;
 for SampleIndex := 0 to HalfKernelSize - 1 do
  begin
   FFilterKernel[1]^[SampleIndex] := FHighpassFilter[0].ProcessSample64(
     FLowpassFilter[1].ProcessSample64(FFilterKernel[1]^[SampleIndex]));
   FFilterKernel[2]^[SampleIndex] := FHighpassFilter[1].ProcessSample64(
     FLowpassFilter[2].ProcessSample64(FFilterKernel[2]^[SampleIndex]));
  end;
 for SampleIndex := HalfKernelSize to FCurrentKernelSize - 1 do
  begin
   FFilterKernel[1]^[SampleIndex] := FHighpassFilter[0].ProcessSample64(
     FLowpassFilter[1].ProcessSample64(0.0));
   FFilterKernel[2]^[SampleIndex] := FHighpassFilter[1].ProcessSample64(
     FLowpassFilter[2].ProcessSample64(0.0));
  end;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.CalculateHighpassKernel;
var
  SampleIndex    : Integer;
  HalfKernelSize : Integer;
begin
 HalfKernelSize := FCurrentKernelSize div 2;

 // backward processing
 FHighpassFilter[2].ResetStates;
 FFilterKernel[3]^[HalfKernelSize - 1] := FHighpassFilter[2].ProcessSample64(1.0);
 for SampleIndex := 1 to HalfKernelSize - 1
  do FFilterKernel[3]^[HalfKernelSize - 1 - SampleIndex] := FHighpassFilter[2].ProcessSample64(0.0);

 // forward processing
 FHighpassFilter[2].ResetStates;
 for SampleIndex := 0 to HalfKernelSize - 1
  do FFilterKernel[3]^[SampleIndex] := FHighpassFilter[2].ProcessSample64(FFilterKernel[3]^[SampleIndex]);
 for SampleIndex := HalfKernelSize to FCurrentKernelSize - 1
  do FFilterKernel[3]^[SampleIndex] := FHighpassFilter[2].ProcessSample64(0.0);
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  BandIndex : Integer;
begin
 if Abs(SampleRate) > 0 then
  begin
   // update filters
   for BandIndex := 0 to Length(FLowpassFilter) - 1 do
    if Assigned(FLowpassFilter[BandIndex])
     then FLowpassFilter[BandIndex].SampleRate := SampleRate;
   for BandIndex := 0 to Length(FHighpassFilter) - 1 do
    if Assigned(FHighpassFilter[BandIndex])
     then FHighpassFilter[BandIndex].SampleRate := SampleRate;

   FilterKernelChanged;
  end;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 FCriticalSection.Enter;
 try
  {$IFDEF UseLowLatencyConvolution}
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[0, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
  {$IFNDEF Debug}
  Move(Inputs[0, 0], Outputs[2, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[0, 0], Outputs[3, 0], SampleFrames * SizeOf(Single));
  {$ENDIF}
  FConvolution[0].ProcessBlock(@Outputs[0, 0], SampleFrames);
  FConvolution[1].ProcessBlock(@Outputs[1, 0], SampleFrames);
  {$IFNDEF Debug}
  FConvolution[2].ProcessBlock(@Outputs[2, 0], SampleFrames);
  FConvolution[3].ProcessBlock(@Outputs[3, 0], SampleFrames);
  {$ENDIF}
  {$ELSE}
  FConvolution[0].ProcessBlock(@Inputs[0, 0], @Outputs[0, 0], SampleFrames);
  FConvolution[1].ProcessBlock(@Inputs[0, 0], @Outputs[1, 0], SampleFrames);
  {$IFNDEF Debug}
  FConvolution[2].ProcessBlock(@Inputs[0, 0], @Outputs[2, 0], SampleFrames);
  FConvolution[3].ProcessBlock(@Inputs[0, 0], @Outputs[3, 0], SampleFrames);
  {$ENDIF}
  {$ENDIF}
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TLinearPhaseLinkwitzRileyDataModule.VSTModuleResume(Sender: TObject);
begin
 FCriticalSection.Enter;
 try
  FConvolution[0].Clear;
  FConvolution[1].Clear;
  FConvolution[2].Clear;
  FConvolution[3].Clear;
  FDownsampler.ClearBuffers;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
