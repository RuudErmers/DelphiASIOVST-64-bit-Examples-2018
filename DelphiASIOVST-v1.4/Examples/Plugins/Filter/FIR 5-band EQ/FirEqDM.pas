unit FirEQDM;

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
  Forms, SyncObjs, DAV_Types, DAV_Complex, DAV_DspAnalogueFilterPrototypes,
  DAV_DspWindowFunctions, DAV_DspConvolution, DAV_DspFftReal2Complex,
  {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, DAV_DspWindowFunctionsAdvanced,
  {$ENDIF} {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_VSTModule;

type
  TFirEQDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure StringToWindowParameter(Sender: TObject; const Index: Integer; const ParameterString: string; var Value: Single);
    procedure ParamFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowFunctionsDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterWindowFunctionsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterInputGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FFilterKernel    : PDAVSingleFixedArray;
    FSignalPadded    : PDAVSingleFixedArray;
    FFilterFreq      : PDAVComplexSingleFixedArray;
    FSignalFreq      : PDAVComplexSingleFixedArray;
    FCriticalSection : TCriticalSection;
    FConvolution     : TLowLatencyConvolutionStereo32;
    FWindowFunction  : TCustomWindowFunction;
    FWinFuncIndex    : Integer;
    FIRSize          : Integer;
    FInputGain       : Double;
    FFilterProtos    : array [0..4] of TCustomBiquadAnalogueFilterPrototype;

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
  Math, DAV_Common, DAV_Math, DAV_VSTModuleWithPrograms;

procedure TFirEQDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;;

 FFilterKernel := nil;
 FSignalPadded := nil;
 FFilterFreq   := nil;
 FSignalFreq   := nil;
 FWinFuncIndex := -1;
 FIRSize       := 1 shl 11;

 with ParameterProperties[0] do
  begin
   Min := 0;
   MinInteger := 0;
   Max := Length(GWindowFunctions) - 1;
   MaxInteger := Length(GWindowFunctions) - 1;
  end;
end;

procedure TFirEQDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TFirEQDataModule.VSTModuleOpen(Sender: TObject);
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

 for Band := 0 to Length(FFilterProtos) - 1
  do FFilterProtos[Band] := TAnaloguePeakFilterPrototype.Create;;

 Parameter[ 0] := 4;
 Parameter[ 1] := 0;
 Parameter[ 2] := 100;
 Parameter[ 3] := 0;
 Parameter[ 4] := 1;
 Parameter[ 5] := 4;
 Parameter[ 6] := 800;
 Parameter[ 7] := 0;
 Parameter[ 8] := 1;
 Parameter[ 9] := 4;
 Parameter[10] := 1600;
 Parameter[11] := 0;
 Parameter[12] := 1;
 Parameter[13] := 4;
 Parameter[14] := 5000;
 Parameter[15] := 0;
 Parameter[16] := 1;
 Parameter[17] := 4;
 Parameter[18] := 12500;
 Parameter[19] := 0;
 Parameter[20] := 1;
 Parameter[21] := 4;

// CalculateFilterKernel;
end;

procedure TFirEQDataModule.VSTModuleClose(Sender: TObject);
var
  Band : Integer;
begin
 for Band := 0 to Length(FFilterProtos) - 1
  do FreeAndNil(FFilterProtos[Band]);

 Dispose(FFilterKernel);
 Dispose(FSignalPadded);
 Dispose(FFilterFreq);
 Dispose(FSignalFreq);

 FreeAndNil(FConvolution);
 FreeAndNil(FWindowFunction);
 FreeAndNil(FFft);
end;

procedure TFirEQDataModule.ParameterWindowFunctionsDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := GWindowFunctions[Round(Parameter[Index])].GetWindowFunctionName;
end;

procedure TFirEQDataModule.StringToWindowParameter(
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

procedure TFirEQDataModule.ParameterWindowFunctionsChange(
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

procedure TFirEQDataModule.ParamFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 2) div 4;
 FFilterProtos[Band].Frequency := Value;

 CalculateFilterKernel;
end;

procedure TFirEQDataModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 2) div 4;
 FFilterProtos[Band].Gain := Value;

 CalculateFilterKernel;
end;

procedure TFirEQDataModule.ParameterTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
const
  CFilterClasses : array [0..11] of string =
    ('Lowcut', 'Lowshelf', 'Lowshelf A', 'Lowshelf B', 'Peak', 'Notch',
    'Allpass', 'Bandpass', 'Highshelf', 'Highshelf A', 'Highshelf B',
    'Highcut');
begin
 PreDefined := CFilterClasses[Round(Parameter[Index])];
end;

procedure TFirEQDataModule.ParameterInputGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FInputGain := dB_to_Amp(Value);
end;

procedure TFirEQDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 4, 4);
end;

procedure TFirEQDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TFirEQDataModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 2) div 4;
 FFilterProtos[Band].Bandwidth := Value;

 CalculateFilterKernel;
end;

procedure TFirEQDataModule.ParameterTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band      : Integer;
  OldFilter : TCustomAnalogueFilterPrototype;
const
  CFilterClasses : array [0..11] of TCustomBiquadAnalogueFilterPrototypeClass =
    (TAnalogueHighpassFilterPrototype, TAnalogueLowshelfFilterPrototype,
     TAnalogueLowshelfAFilterPrototype, TAnalogueLowshelfBFilterPrototype,
     TAnaloguePeakFilterPrototype, TAnalogueNotchFilterPrototype,
     TAnalogueAllpassFilterPrototype, TAnalogueBandpassFilterPrototype,
     TAnalogueHighshelfFilterPrototype, TAnalogueHighshelfAFilterPrototype,
     TAnalogueHighshelfBFilterPrototype, TAnalogueLowpassFilterPrototype);
begin
 Band := (Index - 2) div 4;

 if not Assigned(FFilterProtos[Band]) or
  (CFilterClasses[Round(Value)] <> FFilterProtos[Band].ClassType) then
  begin
   OldFilter := FFilterProtos[Band];
   FFilterProtos[Band] := CFilterClasses[Round(Value)].Create;

   with FFilterProtos[Band] do
    begin
     Frequency := Parameter[Index - 3];
     Gain := Parameter[Index - 2];
     Bandwidth := Parameter[Index - 1];
    end;
  end;

 CalculateFilterKernel;
end;

procedure TFirEQDataModule.CalculateFilterKernel;
var
  i, h, q : Integer;
  Band    : Integer;
  n       : Double;
  Freq    : Double;
  Scale   : Double;
  Cmplx   : array [0..2] of TComplex64;
begin
 // checks whether everything is set correctly
 for Band := 0 to Length(FFilterProtos) - 1 do
  if not Assigned(FFilterProtos[Band]) then Exit;

 if (not Assigned(FFilterKernel)) or (not Assigned(FFilterFreq)) or
   (not Assigned(FFft)) then Exit;

 FCriticalSection.Enter;
 try
  h := FIRSize div 2;
  q := FIRSize div 4;

  FFilterFreq[0] := FFilterProtos[0].Complex32(0);
  for Band := 1 to Length(FFilterProtos) - 1
   do FFilterFreq[0].Re := FFilterFreq[0].Re * FFilterProtos[Band].Complex32(0).Re;

  Cmplx[2] := FFilterProtos[0].Complex64(0.5 * FSampleRate);
  for Band := 1 to Length(FFilterProtos) - 1
   do ComplexMultiplyInplace(Cmplx[2], FFilterProtos[Band].Complex64(0.5 * FSampleRate));

  // compensate nyquist phase offset
  GetSinCos(ArcTan2(-Cmplx[2].Im, Cmplx[2].Re) / h, Cmplx[0].Im, Cmplx[0].Re);
  Cmplx[1].Re := 1;
  Cmplx[1].Im := 0;

  Scale := 1 / h * 0.5 * FSampleRate;

  for i := 1 to h - 1 do
   begin
    Freq := i * Scale;

    // calculate complex frequency response
    Cmplx[2] := FFilterProtos[0].Complex64(Freq);
    for Band := 1 to Length(FFilterProtos) - 1
     do ComplexMultiplyInplace(Cmplx[2], FFilterProtos[Band].Complex64(Freq));

    // compensate nyquist phase offset
    ComplexMultiplyInplace(Cmplx[2], Cmplx[1]);
    ComplexMultiplyInplace(Cmplx[1], Cmplx[0]);

    FFilterFreq[i].Re := Cmplx[2].Re;
    FFilterFreq[i].Im := Cmplx[2].Im;
   end;

  // compensate nyquist phase offset
  Cmplx[2] := FFilterProtos[0].Complex64(0.5 * FSampleRate);
  for Band := 1 to Length(FFilterProtos) - 1
   do ComplexMultiplyInplace(Cmplx[2], FFilterProtos[Band].Complex64(0.5 * FSampleRate));
  ComplexMultiplyInplace(Cmplx[2], Cmplx[1]);

  FFilterFreq[h].Re := Cmplx[2].Re;
  FFilterFreq[h].Im := Cmplx[2].Im;

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

procedure TFirEQDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FInputGain * Inputs[0, Sample];
    Outputs[1, Sample] := FInputGain * Inputs[1, Sample];
   end;
  FConvolution.ProcessBlock(@Outputs[0, 0], @Outputs[1, 0], SampleFrames);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TFirEQDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 CalculateFilterKernel;
end;

end.
