unit WeightingFiltersDM;

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
  Forms, SyncObjs, Dialogs, DAV_Types, DAV_Complex, 
  DAV_DspAnalogueFilterPrototypes, DAV_DspWindowFunctions, DAV_DspConvolution, 
  DAV_DspFftReal2Complex,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, DAV_DspWindowFunctionsAdvanced,
  {$ENDIF} {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_VSTModule;

type
  TWeightingFiltersDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure StringToWindowParameter(Sender: TObject; const Index: Integer; const ParameterString: string; var Value: Single);
    procedure ParameterWindowFunctionsDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterWindowFunctionsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FFilterKernel      : PDAVSingleFixedArray;
    FSignalPadded      : PDAVSingleFixedArray;
    FFilterFreq        : PDAVComplexSingleFixedArray;
    FSignalFreq        : PDAVComplexSingleFixedArray;
    FCriticalSection   : TCriticalSection;
    FConvolution       : TLowLatencyConvolutionStereo32;
    FWindowFunction    : TCustomWindowFunction;
    FWinFuncIndex      : Integer;
    FIRSize            : Integer;
    FAnaloguePrototype : TCustomAnalogueWeightingFilterPrototype;

    {$IFDEF Use_IPPS}
    FFft             : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft             : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft             : TFftReal2ComplexNativeFloat32;
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
  Math, DAV_Common, DAV_VSTModuleWithPrograms;

procedure TWeightingFiltersDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;;

 FFilterKernel := nil;
 FSignalPadded := nil;
 FFilterFreq   := nil;
 FSignalFreq   := nil;
 FWinFuncIndex := 0;
 FIRSize       := 1 shl 12;

 with ParameterProperties[1] do
  begin
   Min := 0;
   MinInteger := 0;
   Max := Length(GWindowFunctions) - 1;
   MaxInteger := Length(GWindowFunctions) - 1;
  end;
end;

procedure TWeightingFiltersDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TWeightingFiltersDataModule.VSTModuleOpen(Sender: TObject);
var
  Band : Integer;
begin
 FConvolution := TLowLatencyConvolutionStereo32.Create;
 with FConvolution do
  begin
   MinimumIRBlockOrder := 6;
   InitialDelay := Latency;
  end;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(Round(Log2(FIRSize)));

 ReallocMem(FFilterFreq, (FIRSize div 2 + 1) * SizeOf(TComplex32));
 ReallocMem(FSignalFreq, (FIRSize div 2 + 1) * SizeOf(TComplex32));
 FillChar(FFilterFreq^[0], (FIRSize div 2 + 1) * SizeOf(TComplex32), 0);
 FillChar(FSignalFreq^[0], (FIRSize div 2 + 1) * SizeOf(TComplex32), 0);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(Round(Log2(FIRSize)));

 ReallocMem(FFilterFreq, FIRSize * SizeOf(Single));
 ReallocMem(FSignalFreq, FIRSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], FIRSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], FIRSize * SizeOf(Single), 0);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(Round(Log2(FIRSize)));

 ReallocMem(FFilterFreq, FIRSize * SizeOf(Single));
 ReallocMem(FSignalFreq, FIRSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], FIRSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], FIRSize * SizeOf(Single), 0);
 {$ENDIF}{$ENDIF}

 ReallocMem(FFilterKernel, FIRSize * SizeOf(Single));
 ReallocMem(FSignalPadded, FIRSize * SizeOf(Single));
 FillChar(FFilterKernel^[0], FIRSize * SizeOf(Single), 0);
 FillChar(FSignalPadded^[0], FIRSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;
 FFft.DataOrder := doPackedComplex;

 FAnaloguePrototype := TAnalogueAWeightingFilterPrototype.Create;

 Parameter[0] := 0;
 Parameter[1] := 0;

 CalculateFilterKernel;
end;

procedure TWeightingFiltersDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FAnaloguePrototype);

 Dispose(FFilterKernel);
 Dispose(FSignalPadded);
 Dispose(FFilterFreq);
 Dispose(FSignalFreq);

 FreeAndNil(FConvolution);
 FreeAndNil(FWindowFunction);
 FreeAndNil(FFft);
end;

procedure TWeightingFiltersDataModule.ParameterWindowFunctionsDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TWeightingFiltersDataModule.StringToWindowParameter(
  Sender: TObject; const Index: Integer; const ParameterString: string;
  var Value: Single);
var
  WindowIndex : Integer;
  Text        : string;
begin
 Text := Trim(ParameterString);
 for WindowIndex := 0 to Length(GWindowFunctions) - 1 do
  if Text = GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName then
   begin
    Value := WindowIndex;
    Exit;
   end;
end;

procedure TWeightingFiltersDataModule.ParameterWindowFunctionsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  OldFunction : TCustomWindowFunction;
begin
 if FWinFuncIndex <> Round(Value) then
  begin
   FWinFuncIndex := Round(Value);
   OldFunction := FWindowFunction;
   FWindowFunction := GWindowFunctions[FWinFuncIndex].Create;
   FWindowFunction.Length := FFft.FFTSize div 2;

   if Assigned(OldFunction)
    then FreeAndNil(OldFunction);

   CalculateFilterKernel;
  end;
end;

procedure TWeightingFiltersDataModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
const
  CTypeNames : array [0..3] of string = ('A', 'B', 'C', 'D');
begin
 PreDefined := CTypeNames[Round(Parameter[Index])];
end;

procedure TWeightingFiltersDataModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  OldFilter : TCustomAnalogueWeightingFilterPrototype;
const
  CFilterClasses : array [0..3] of TCustomAnalogueWeightingFilterPrototypeClass =
    (TAnalogueAWeightingFilterPrototype, TAnalogueBWeightingFilterPrototype,
     TAnalogueCWeightingFilterPrototype, TAnalogueDWeightingFilterPrototype);
begin
 if not Assigned(FAnaloguePrototype) or
  (CFilterClasses[Round(Value)] <> FAnaloguePrototype.ClassType) then
  begin
   OldFilter := FAnaloguePrototype;
   FAnaloguePrototype := CFilterClasses[Round(Value)].Create;

   // eventually free old filter
   if Assigned(OldFilter)
    then FreeAndNil(OldFilter);

   CalculateFilterKernel;
  end;
end;

procedure TWeightingFiltersDataModule.CalculateFilterKernel;
var
  i, h, q : Integer;
  Band    : Integer;
  n       : Double;
  Freq    : Double;
  Scale   : Double;
  Cmplx   : array [0..1] of TComplex32;
begin
 // checks whether everything is set correctly
 if not Assigned(FAnaloguePrototype) then Exit;

 if (not Assigned(FFilterKernel)) or (not Assigned(FFilterFreq)) or
   (not Assigned(FFft)) then Exit;

 FCriticalSection.Enter;
 try
  h := FIRSize div 2;
  q := FIRSize div 4;

  // calculate filter frequency response
  FFilterFreq[0] := FAnaloguePrototype.Complex32(0);
  FFilterFreq[h] := FAnaloguePrototype.Complex32(0.5 * FSampleRate);

  Cmplx[0] := ComplexPolar(1, ArcTan2(-FFilterFreq[h].Im, FFilterFreq[h].Re) / h);
  Cmplx[1].Re := 1;
  Cmplx[1].Im := 0;
  Scale := 1 / h * 0.5 * FSampleRate;

  for i := 1 to h - 1 do
   begin
    Freq := i * Scale;
    FFilterFreq[i] := FAnaloguePrototype.Complex32(Freq);

    ComplexMultiplyInplace(FFilterFreq[i], Cmplx[1]);
    ComplexMultiplyInplace(Cmplx[1], Cmplx[0]);
   end;

  ComplexMultiplyInplace(FFilterFreq[h], Cmplx[1]);

  // calculate frequency
  {$IFDEF Use_IPPS}
  FFft.PerformIFFTCCS(FFilterFreq, FFilterKernel);
  {$ELSE}{$IFDEF Use_CUDA}
  FFft.PerformIFFTCCS(FFilterFreq, FFilterKernel);
  {$ELSE}
  FFft.PerformIFFTPackedComplex(FFilterFreq, FFilterKernel);
  {$ENDIF}{$ENDIF}

  if Assigned(FWindowFunction) then
   with FWindowFunction do
    begin
     Length := h;
     Slope := wsRight;
     ProcessBlock32(FFilterKernel, h);
    end;

  FConvolution.LoadImpulseResponse(FFilterKernel, h);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TWeightingFiltersDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
  FConvolution.ProcessBlock(@Outputs[0, 0], @Outputs[1, 0], SampleFrames);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TWeightingFiltersDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 CalculateFilterKernel;
end;

end.
