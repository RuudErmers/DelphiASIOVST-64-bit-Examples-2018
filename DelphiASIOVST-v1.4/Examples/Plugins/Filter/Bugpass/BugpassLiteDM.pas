unit BugpassLiteDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, SyncObjs, DAV_Types, DAV_Complex, DAV_MemoryUtils,
  DAV_DspFftReal2Complex, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_VSTModule;

type
  TBugpassLiteDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamFreqLowChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamFreqHighChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FFilterKernel    : PDAVSingleFixedArray;
    FSignalPadded    : PDAVSingleFixedArray;
    FFilterFreq      : PDAVComplexSingleFixedArray;
    FSignalFreq      : PDAVComplexSingleFixedArray;
    FCriticalSection : TCriticalSection;
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
  Math, DAV_Common, DAV_DspWindowing, BugpassLiteGUI;

procedure TBugpassLiteDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 FFilterKernel    := nil;
 FSignalPadded    := nil;
 FFilterFreq      := nil;
 FSignalFreq      := nil;
 BlockModeOverlap := BlockModeSize div 2;
 InitialDelay     := (BlockModeOverlap * 3) div 4
end;

procedure TBugpassLiteDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TBugpassLiteDataModule.VSTModuleOpen(Sender: TObject);
begin
 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(Round(Log2(BlockModeSize)));
 FFft.DataOrder := doComplex;

 ReallocateAlignedMemory(Pointer(FFilterFreq), (BlockModeSize div 2 + 1) * SizeOf(TComplex32));
 ReallocateAlignedMemory(FSignalFreq, (BlockModeSize div 2 + 1) * SizeOf(TComplex32));
 FillChar(FFilterFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplex32), 0);
 FillChar(FSignalFreq^[0], (BlockModeSize div 2 + 1) * SizeOf(TComplex32), 0);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(Round(Log2(BlockModeSize)));
 FFft.DataOrder := doPackedComplex;

 ReallocateAlignedMemory(FFilterFreq, BlockModeSize * SizeOf(Single));
 ReallocateAlignedMemory(FSignalFreq, BlockModeSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(Round(Log2(BlockModeSize)));
 FFft.DataOrder := doPackedComplex;

 ReallocateAlignedMemory(FFilterFreq, BlockModeSize * SizeOf(Single));
 ReallocateAlignedMemory(FSignalFreq, BlockModeSize * SizeOf(Single));
 FillChar(FFilterFreq^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalFreq^[0], BlockModeSize * SizeOf(Single), 0);
 {$ENDIF}{$ENDIF}

 ReallocateAlignedMemory(FFilterKernel, BlockModeSize * SizeOf(Single));
 ReallocateAlignedMemory(FSignalPadded, BlockModeSize * SizeOf(Single));
 FillChar(FFilterKernel^[0], BlockModeSize * SizeOf(Single), 0);
 FillChar(FSignalPadded^[0], BlockModeSize * SizeOf(Single), 0);

 FFft.AutoScaleType := astDivideInvByN;

 // set editor form class
 EditorFormClass := TFmBugpassLite;

 // Parameters and Programs
 Parameter[0] := 100;
 Parameter[1] := 16000;
 Programs[1].CopyParameters(0);
 Programs[2].CopyParameters(0);

 // finally calculate the first filter kernel
 CalculateFilterKernel;
end;

procedure TBugpassLiteDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAlignedMemory(FFilterKernel);
 FreeAlignedMemory(FSignalPadded);
 FreeAlignedMemory(FFilterFreq);
 FreeAlignedMemory(FSignalFreq);
 FreeAndNil(FFft);
end;

procedure TBugpassLiteDataModule.ParamFreqLowChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateFilterKernel;

 // update GUI
 if EditorForm is TFmBugpassLite then
  with TFmBugpassLite(EditorForm)
   do UpdateFrequencyBar;
end;

procedure TBugpassLiteDataModule.ParamFreqHighChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 CalculateFilterKernel;

 // update GUI
 if EditorForm is TFmBugpassLite then
  with TFmBugpassLite(EditorForm)
   do UpdateFrequencyBar;
end;

procedure TBugpassLiteDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := FloatToAnsiString(1E-3 * Parameter[Index], 3)
  else PreDefined := FloatToAnsiString(Parameter[Index], 3);
end;

procedure TBugpassLiteDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz'
  else PreDefined := 'Hz';
end;

procedure TBugpassLiteDataModule.CalculateFilterKernel;
var
  Index   : Integer;
  Half    : Integer;
  Quarter : Integer;
  n       : Double;
  CutOff  : array [0..1] of Double;
begin
 if Assigned(FFilterKernel) and Assigned(FFilterFreq) and Assigned(FFft) then
  begin
   FCriticalSection.Enter;
   try
    CutOff[0] := Parameter[0] / SampleRate;
    CutOff[1] := Parameter[1] / SampleRate;
    Half := BlockModeSize div 2;
    Quarter := BlockModeSize div 4;

    // Generate sinc delayed by (N-1)/2
    for Index := 0 to Half - 1 do
     if (Index = Quarter)
      then FFilterKernel^[Index] := 2.0 * (CutOff[0] - CutOff[1])
      else
       begin
        n := PI * (Index - Quarter);
        FFilterKernel^[Index] := (sin(2.0 * CutOff[0] * n) - sin(2.0 * CutOff[1] * n)) / n;
       end;
    ApplyBlackmanWindow(FFilterKernel, Half);
    FillChar(FFilterKernel^[Half], Half * SizeOf(Single), 0);

    // calculate frequency
    {$IFDEF Use_IPPS}
    FFft.PerformFFTCCS(FFilterFreq, FFilterKernel);
    {$ELSE}{$IFDEF Use_CUDA}
    FFft.PerformFFT(FFilterFreq, FFilterKernel);
    {$ELSE}
    case FFft.DataOrder of
     doPackedRealImaginary : FFft.PerformFFTPackedReIm(PDAVSingleFixedArray(FFilterFreq), FFilterKernel);
           doPackedComplex : FFft.PerformFFTPackedComplex(FFilterFreq, FFilterKernel);
     else raise Exception.Create('not supported');
    end;
    {$ENDIF}{$ENDIF}
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

procedure TBugpassLiteDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Bin     : Integer;
  Half    : Integer;
begin
 FCriticalSection.Enter;
 try
  Half := BlockModeSize div 2;
  for Channel := 0 to numOutputs - 1 do
   begin
    {$IFDEF Use_IPPS}
    FFft.PerformFFT(PDAVComplexSingleFixedArray(FSignalFreq), @Inputs[Channel, 0]);

    // DC & Nyquist
    FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
    FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

    for Bin := 1 to Half - 1
     do ComplexMultiplyInplace32(FSignalFreq^[Bin], FFilterFreq^[Bin]);

    FFft.PerformIFFT(PDAVComplexSingleFixedArray(FSignalFreq), @Outputs[Channel, 0]);

    {$ELSE}{$IFDEF Use_CUDA}
    FFft.PerformFFT(FSignalFreq, @Inputs[Channel, 0]);

    // DC & Nyquist
    FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
    FSignalFreq^[0].Im := FFilterFreq^[0].Im * FSignalFreq^[0].Im;
    FSignalFreq^[Half].Re := FFilterFreq^[Half].Re * FSignalFreq^[Half].Re;

    for Bin := 1 to Half - 1
     do ComplexMultiplyInplace(FSignalFreq^[Bin], FFilterFreq^[Bin]);

    FFft.PerformIFFT(FSignalFreq, @Outputs[Channel, 0]);
    {$ELSE}
    case FFft.DataOrder of
     doPackedRealImaginary :
      begin
       FFft.PerformFFTPackedReIm(PDAVSingleFixedArray(FSignalFreq), @Inputs[Channel, 0]);

       // DC
       Bin := 0;
       PDAVSingleFixedArray(FSignalFreq)^[Bin] :=
         PDAVSingleFixedArray(FFilterFreq)^[Bin] * PDAVSingleFixedArray(FSignalFreq)^[Bin];
       Inc(Bin);

       // inbetween...
       while Bin < Half do
        begin
         ComplexMultiplyInplace32(
           PDAVSingleFixedArray(FSignalFreq)^[Bin],
           PDAVSingleFixedArray(FSignalFreq)^[Bin + Half],
           PDAVSingleFixedArray(FFilterFreq)^[Bin],
           PDAVSingleFixedArray(FFilterFreq)^[Bin + Half]);
         Inc(Bin);
        end;

       // Nyquist
       PDAVSingleFixedArray(FSignalFreq)^[Bin] :=
         PDAVSingleFixedArray(FFilterFreq)^[Bin] * PDAVSingleFixedArray(FSignalFreq)^[Bin];

       FFft.PerformIFFTPackedReIm(PDAVSingleFixedArray(FSignalFreq), @Outputs[Channel, 0]);
      end;
      doPackedComplex :
       begin
        FFft.PerformFFTPackedComplex(FSignalFreq, @Inputs[Channel, 0]);

        // DC & Nyquist
        FSignalFreq^[0].Re := FFilterFreq^[0].Re * FSignalFreq^[0].Re;
        FSignalFreq^[0].Im := FFilterFreq^[Half].Im * FSignalFreq^[Half].Im;

        for Bin := 1 to Half - 1
         do ComplexMultiplyInplace32(FSignalFreq^[Bin], FFilterFreq^[Bin]);

        FFft.PerformIFFTPackedComplex(FSignalFreq, @Outputs[Channel, 0]);
       end
     else raise Exception.Create('not supported');
    end;
    {$ENDIF}{$ENDIF}
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TBugpassLiteDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0
  then CalculateFilterKernel;
end;

end.
