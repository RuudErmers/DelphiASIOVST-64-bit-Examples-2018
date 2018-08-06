unit LinearPhaseFromIIRDM;

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

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_DspFilterButterworth, DAV_DspConvolution, 
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, DAV_DspWindowFunctionsAdvanced,
  {$ENDIF} {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF}
  DAV_DspFftReal2Complex, DAV_DspDelayLines, DAV_VSTModule;

type
  TLinearPhaseFromIIRDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection   : TCriticalSection;
    FConvolution       : TLowLatencyConvolution32;
    FButterworthFilter : TButterworthLowPassFilter;
    FDelay             : TDelayLineSamples32;
    procedure CalculateFilterKernel;
    procedure MakeMinimumPhase(FilterKernel: PDAVSingleFixedArray;
      KernelSize: Cardinal);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_VSTParameters, DAV_Common, DAV_Math, DAV_Complex,
  DAV_BlockProcessing, LinearPhaseFromIIRGUI;

procedure TLinearPhaseFromIIRDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TLinearPhaseFromIIRDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TLinearPhaseFromIIRDataModule.VSTModuleOpen(Sender: TObject);
begin
 FConvolution := TLowLatencyConvolution32.Create;
 with FConvolution do
  begin
   MinimumIRBlockOrder := 6;
   MaximumIRBlockOrder := 18;
  end;
 FButterworthFilter := TButterworthLowPassFilter.Create(1);
 FDelay := TDelayLineSamples32.Create(FConvolution.Latency);

 // initialize parameters
 Parameter[0] := 1000;
 Parameter[1] := 1;
 Parameter[2] := 0;

 CalculateFilterKernel;

 // set editor GUI
 EditorFormClass := TFmLinearPhaseFromIIR;
end;

procedure TLinearPhaseFromIIRDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FDelay);
 FreeAndNil(FButterworthFilter);
 FreeAndNil(FConvolution);
end;

procedure TLinearPhaseFromIIRDataModule.ParameterFilterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(2 * Round(0.5 * Parameter[Index])));
end;

procedure TLinearPhaseFromIIRDataModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FButterworthFilter) then
  begin
   FButterworthFilter.Frequency := Value;
   CalculateFilterKernel;
  end;

 // update GUI
 if EditorForm is TFmLinearPhaseFromIIR then
  with TFmLinearPhaseFromIIR(EditorForm)
   do UpdateFrequency;
end;

procedure TLinearPhaseFromIIRDataModule.ParameterFilterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  NewOrder: Integer;
begin
 if Assigned(FButterworthFilter) then
  begin
   NewOrder := Round(Parameter[Index]);

   if FButterworthFilter.Order <> NewOrder then
    begin
     FButterworthFilter.Order := NewOrder;
     CalculateFilterKernel;
    end;
  end;

 // update GUI
 if EditorForm is TFmLinearPhaseFromIIR then
  with TFmLinearPhaseFromIIR(EditorForm)
   do UpdateOrder;
end;

procedure TLinearPhaseFromIIRDataModule.MakeMinimumPhase(
  FilterKernel: PDAVSingleFixedArray; KernelSize: Cardinal);
var
  {$IFDEF Use_IPPS}
  Fft         : TFftReal2ComplexIPPSFloat32;
  {$ELSE} {$IFDEF Use_CUDA}
  Fft         : TFftReal2ComplexCUDA32;
  {$ELSE}
  Fft         : TFftReal2ComplexNativeFloat32;
  {$ENDIF}{$ENDIF}
  TimeDomain  : PDAVSingleFixedArray;
  FreqDomain  : PDAVComplexSingleFixedArray;
  Order       : Cardinal;
  TempSize    : Cardinal;
  BinIndex    : Cardinal;
begin
 Order := CeilLog2(KernelSize - 1);
 TempSize := 2 shl Order;

 Assert(KernelSize < TempSize);

 {$IFDEF Use_IPPS}
 Fft := TFftReal2ComplexIPPSFloat32.Create(Order + 1);

 GetMem(TimeDomain, TempSize * SizeOf(Single));
 GetMem(FreqDomain, 2 * ((1 shl Order) + 1) * SizeOf(TComplex32));
 FillChar(TimeDomain^[0], TempSize * SizeOf(Single), 0);
 FillChar(FreqDomain^[0], (1 shl Order + 1) * SizeOf(TComplex32), 0);
 {$ELSE} {$IFDEF Use_CUDA}
 Fft := TFftReal2ComplexCUDA32.Create(Order + 1);

 GetMem(TimeDomain, TempSize * SizeOf(Single));
 GetMem(FreqDomain, TempSize * SizeOf(Single));
 FillChar(TimeDomain^[0], TempSize * SizeOf(Single), 0);
 FillChar(FreqDomain^[0], TempSize * SizeOf(Single), 0);
 {$ELSE}
 Fft := TFftReal2ComplexNativeFloat32.Create(Order + 1);
 Fft.DataOrder := doPackedComplex;

 GetMem(TimeDomain, TempSize * SizeOf(Single));
 GetMem(FreqDomain, TempSize * SizeOf(Single));
 FillChar(TimeDomain^[0], TempSize * SizeOf(Single), 0);
 FillChar(FreqDomain^[0], TempSize * SizeOf(Single), 0);
 {$ENDIF}{$ENDIF}

 with FFT do
  try
   AutoScaleType := astDivideFwdByN;
   Move(FilterKernel^[0], TimeDomain^[0], KernelSize * SizeOf(Single));

   // fill extended part with zeroes
   FillChar(TimeDomain^[KernelSize], (TempSize - KernelSize) * SizeOf(Single),
     0);

   // transform time signal to frequency domain
   PerformFFT(@FreqDomain[0], @TimeDomain[0]);

   // make linear phase and copy magnitude to real part
   for BinIndex := 1 to (1 shl Order) - 2 do
    begin
     FreqDomain[BinIndex].Re := Hypot(FreqDomain[BinIndex].Re, FreqDomain[BinIndex].Im);
     FreqDomain^[BinIndex].Im := 0;
    end;

   PerformiFFT(@FreqDomain[0], @TimeDomain[0]);

   // fit impulse response to old length
   if KernelSize < (2 shl Order) then
    begin
     Move(TimeDomain[0], FilterKernel[(KernelSize div 2)], (KernelSize div 2) * SizeOf(Single));
     Move(TimeDomain[FFTSize - (KernelSize div 2)], FilterKernel[0], (KernelSize div 2) * SizeOf(Single));
    end
   else
     Move(TimeDomain[0], FilterKernel[0], KernelSize * SizeOf(Single));
  finally
   FFT.Free;
  end;

 FreeMem(TimeDomain);
 FreeMem(FreqDomain);
end;

procedure TLinearPhaseFromIIRDataModule.CalculateFilterKernel;
var
  Sample       : Integer;
  FilterKernel : PDAVSingleFixedArray;
  KernelSize   : Integer;
  Peak         : Double;
const
  CStepSize : Integer = 1 shl 8;
begin
 KernelSize := CStepSize;
 GetMem(FilterKernel, KernelSize * SizeOf(Single));
 try
  // forward processing
  FButterworthFilter.PushStates;
  FButterworthFilter.ResetStates;
  FilterKernel[0] := FButterworthFilter.ProcessSample64(1.0);
  Peak := Abs(FilterKernel[0]);
  for Sample := 1 to KernelSize - 1 do
   begin
    FilterKernel[Sample] := FButterworthFilter.ProcessSample64(0.0);
    Peak := 0.9 * Peak;
    if Abs(FilterKernel[Sample]) > Peak
     then Peak := abs(FilterKernel[Sample]);
   end;

  while (Peak > 1E-7) and (KernelSize < 1 shl 16) do
   begin
    ReallocMem(FilterKernel, (KernelSize + CStepSize) * SizeOf(Single));
    for Sample := KernelSize to KernelSize + CStepSize - 1 do
     begin
      FilterKernel[Sample] := FButterworthFilter.ProcessSample64(0.0);
      Peak := 0.9 * Peak;
      if Abs(FilterKernel[Sample]) > Peak
       then Peak := Abs(FilterKernel[Sample]);
     end;
    Inc(KernelSize, CStepSize);
   end;
  FButterworthFilter.PopStates;

  MakeMinimumPhase(FilterKernel, KernelSize);

  FCriticalSection.Enter;
  try
   FConvolution.LoadImpulseResponse(FilterKernel, KernelSize);
   FDelay.BufferSize := FConvolution.Latency + KernelSize div 2;
   FDelay.ClearBuffer;
   FConvolution.Clear;
   InitialDelay := FConvolution.Latency;// + KernelSize;
  finally
   FCriticalSection.Leave;
  end;
 finally
  Dispose(FilterKernel);
 end;
end;

procedure TLinearPhaseFromIIRDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  if Assigned(FButterworthFilter) then
   begin
    FButterworthFilter.SampleRate := SampleRate;
    CalculateFilterKernel;
   end;
end;

procedure TLinearPhaseFromIIRDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
 FCriticalSection.Enter;
 try
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
  FConvolution.ProcessBlock(@Outputs[0, 0], SampleFrames);
  FButterworthFilter.ProcessBlock32(@Outputs[1, 0], SampleFrames);
  FDelay.ProcessBlock32(@Outputs[1, 0], SampleFrames);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
