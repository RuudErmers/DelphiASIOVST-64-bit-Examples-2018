unit LinearPhaseDM;

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
  Forms, SyncObjs, DAV_Types, DAV_Complex, DAV_DspWindowFunctions,
  DAV_DspFftReal2Complex, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS,
  DAV_DspWindowFunctionsAdvanced, {$ENDIF}
{$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_VSTModule;

type
  TLinearPhaseDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer;
      var Value: Single);
    procedure ParameterWindowFunctionsDisplay(Sender: TObject;
      const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterWindowFunctionsChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure StringToWindowParameter(Sender: TObject; const Index: Integer;
      const ParameterString: AnsiString; var Value: Single);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FFilterKernel: PDAVSingleFixedArray;
    FSignalPadded: PDAVSingleFixedArray;
    FFilterFreq: PDAVComplexSingleFixedArray;
    FSignalFreq: PDAVComplexSingleFixedArray;
    FCriticalSection: TCriticalSection;
    FWindowFunction: TCustomWindowFunction;
    FWinFuncIndex: Integer;

{$IFDEF Use_IPPS}
    FFft: TFftReal2ComplexIPPSFloat32;
{$ELSE} {$IFDEF Use_CUDA}
    FFft: TFftReal2ComplexCUDA32;
{$ELSE}
    FFft: TFftReal2ComplexNativeFloat32;
{$ENDIF}{$ENDIF}
    procedure CalculateFilterKernel;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
{$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} Math, DAV_VSTParameters,
  LinearPhaseGUI;

procedure TLinearPhaseDataModule.VSTModuleCreate(Sender: TObject);
begin
  FCriticalSection := TCriticalSection.Create;
  FFilterKernel := nil;
  FSignalPadded := nil;
  FFilterFreq := nil;
  FSignalFreq := nil;
  FWinFuncIndex := -1;
  BlockModeOverlap := BlockModeSize div 2;

  with ParameterProperties[1] do
  begin
    Min := 0;
    MinInteger := 0;
    Max := Length(GWindowFunctions) - 1;
    MaxInteger := Length(GWindowFunctions) - 1;
  end;
end;

procedure TLinearPhaseDataModule.VSTModuleDestroy(Sender: TObject);
begin
  FCriticalSection.Free;
end;

procedure TLinearPhaseDataModule.VSTModuleOpen(Sender: TObject);
begin
{$IFDEF Use_IPPS}
  FFft := TFftReal2ComplexIPPSFloat32.Create(Round(Log2(BlockModeSize)));

  ReallocMem(FFilterFreq, (BlockModeSize div 2 + 1) * SizeOf(TComplex32));
  ReallocMem(FSignalFreq, (BlockModeSize div 2 + 1) * SizeOf(TComplex32));
  FillChar(FFilterFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplex32), 0);
  FillChar(FSignalFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplex32), 0);
{$ELSE} {$IFDEF Use_CUDA}
  FFft := TFftReal2ComplexCUDA32.Create(Round(Log2(BlockModeSize)));

  ReallocMem(FFilterFreq, BlockModeSize * SizeOf(Single));
  ReallocMem(FSignalFreq, BlockModeSize * SizeOf(Single));
  FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
  FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);
{$ELSE}
  FFft := TFftReal2ComplexNativeFloat32.Create(Round(Log2(BlockModeSize)));

  ReallocMem(FFilterFreq, BlockModeSize * SizeOf(Single));
  ReallocMem(FSignalFreq, BlockModeSize * SizeOf(Single));
  FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
  FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);
{$ENDIF}{$ENDIF}
  ReallocMem(FFilterKernel, BlockModeSize * SizeOf(Single));
  ReallocMem(FSignalPadded, BlockModeSize * SizeOf(Single));
  FillChar(FFilterKernel^[0], BlockModeSize * SizeOf(Single), 0);
  FillChar(FSignalPadded^[0], BlockModeSize * SizeOf(Single), 0);

  FFft.AutoScaleType := astDivideInvByN;
  FFft.DataOrder := doPackedComplex;

  // initialize parameters
  Parameter[0] := 20;
  Parameter[1] := 4;

  CalculateFilterKernel;

  // set editor form class
  EditorFormClass := TFmLinearPhase;
end;

procedure TLinearPhaseDataModule.VSTModuleClose(Sender: TObject);
begin
  Dispose(FFilterKernel);
  Dispose(FSignalPadded);
  Dispose(FFilterFreq);
  Dispose(FSignalFreq);

  FreeAndNil(FWindowFunction);
  FreeAndNil(FFft);
end;

procedure TLinearPhaseDataModule.ParameterWindowFunctionsDisplay
  (Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
  PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TLinearPhaseDataModule.StringToWindowParameter(Sender: TObject;
  const Index: Integer; const ParameterString: AnsiString; var Value: Single);
var
  WindowIndex: Integer;
  Text: AnsiString;
begin
  Text := Trim(ParameterString);
  for WindowIndex := 0 to Length(GWindowFunctions) - 1 do
    if Text = GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName
    then
    begin
      Value := WindowIndex;
      Exit;
    end;
end;

procedure TLinearPhaseDataModule.ParameterWindowFunctionsChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  OldFunction: TCustomWindowFunction;
begin
  if FWinFuncIndex <> Round(Value) then
  begin
    FWinFuncIndex := Round(Value);
    OldFunction := FWindowFunction;
    FWindowFunction := GWindowFunctions[FWinFuncIndex].Create;
    FWindowFunction.Length := FFft.FFTSize div 2;

    if Assigned(OldFunction) then
      FreeAndNil(OldFunction);

    CalculateFilterKernel;
  end;
end;

procedure TLinearPhaseDataModule.ParamFrequencyChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  CalculateFilterKernel;
  if EditorForm is TFmLinearPhase then
    with TFmLinearPhase(EditorForm) do
      UpdateFrequency;
end;

procedure TLinearPhaseDataModule.CalculateFilterKernel;
var
  i, h, q: Integer;
  n: Double;
  CutOff: Double;
begin
  if Assigned(FFilterKernel) and Assigned(FFilterFreq) and Assigned(FFft) then
  begin
    FCriticalSection.Enter;
    try
      CutOff := Parameter[0] / SampleRate;
      h := BlockModeSize div 2;
      q := BlockModeSize div 4;

      // Generate sinc delayed by (N-1)/2
      for i := 0 to h - 1 do
        if (i = q) then
          FFilterKernel^[i] := 1 - 2.0 * CutOff
        else
        begin
          n := PI * (i - q);
          FFilterKernel^[i] := -sin(2.0 * CutOff * n) / n;
        end;

      if Assigned(FWindowFunction) then
        FWindowFunction.ProcessBlock32(FFilterKernel, h);
      FillChar(FFilterKernel^[h], h * SizeOf(Single), 0);

      // calculate frequency
{$IFDEF Use_IPPS}
      FFft.PerformFFTCCS(FFilterFreq, FFilterKernel);
{$ELSE}{$IFDEF Use_CUDA}
      FFft.PerformFFTCCS(FFilterFreq, FFilterKernel);
{$ELSE}
      FFft.PerformFFTPackedComplex(FFilterFreq, FFilterKernel);
{$ENDIF}{$ENDIF}
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

procedure TLinearPhaseDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel: Integer;
  Bin: Integer;
  Half: Integer;
begin
  Half := BlockModeSize div 2;
  FCriticalSection.Enter;
  try
    for Channel := 0 to numOutputs - 1 do
    begin
{$IFDEF Use_IPPS}
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FSignalFreq),
        @Inputs[Channel, 0]);

      // DC & Nyquist
      FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
      FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

      for Bin := 1 to Half - 1 do
        ComplexMultiplyInplace32(FSignalFreq^[Bin], FFilterFreq^[Bin]);

      FFft.PerformIFFTCCS(PDAVComplexSingleFixedArray(FSignalFreq),
        @Outputs[Channel, 0]);

{$ELSE}{$IFDEF Use_CUDA}
      FFft.PerformFFT(FSignalFreq, @Inputs[Channel, 0]);

      // DC & Nyquist
      FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
      FSignalFreq^[0].Im := FFilterFreq^[0].Im * FSignalFreq^[0].Im;
      FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

      for Bin := 1 to Half - 1 do
        ComplexMultiplyInplace(FSignalFreq^[Bin], FFilterFreq^[Bin]);

      FFft.PerformIFFT(FSignalFreq, @Outputs[Channel, 0]);
{$ELSE}
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FSignalFreq),
        @Inputs[Channel, 0]);

      // DC & Nyquist
      FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
      FSignalFreq^[0].Im := FFilterFreq^[0].Im * FSignalFreq^[0].Im;
      FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

      for Bin := 1 to Half - 1 do
        ComplexMultiplyInplace32(FSignalFreq^[Bin], FFilterFreq^[Bin]);

      FFft.PerformIFFTPackedComplex(PDAVComplexSingleFixedArray(FSignalFreq),
        @Outputs[Channel, 0]);
{$ENDIF}{$ENDIF}
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TLinearPhaseDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
  CalculateFilterKernel;
end;

end.
