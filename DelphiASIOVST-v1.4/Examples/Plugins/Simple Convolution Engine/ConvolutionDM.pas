unit ConvolutionDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}
{-$DEFINE Use_IPPS}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  SyncObjs, Forms, DAV_Types, DAV_Complex, DAV_DspFftReal2Complex,
  DAV_VSTModule, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU, DAV_AudioData;

type
  TConvolutionDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
  private
    FFilterKernel   : PDAVSingleFixedArray;
    FFilterFreq     : PDAVComplexSingleFixedArray;
    FSignalFreq     : PDAVComplexSingleFixedArray;
    FSignalTime     : array of PDAVSingleFixedArray;
    FBuffer         : PDAVSingleFixedArray;
    FCriticalSection: TCriticalSection;
    {$IFDEF Use_IPPS}
    FFft            : TFftReal2ComplexIPPSFloat32;
    {$ELSE}
    FFft            : TFftReal2ComplexNativeFloat32;
    {$ENDIF}
    FIRSize         : Integer;
    FFFTSize        : Integer;
    FFFTSizeHalf    : Integer;
    FFFTSizeQuarter : Integer;
    procedure PerformConvolution(Signal: PDAVSingleFixedArray);
    procedure SetIRSize(const Value: Integer);
    procedure IRSizeChanged;
  public
    procedure LoadIR(FileName: TFileName);
    property IRSize: Integer read FIRSize write SetIRSize; 
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Math, DAV_DspWindowing, DAV_BlockProcessing, ConvolutionGUI;


{ TConvolutionDataModule }

procedure TConvolutionDataModule.VSTModuleOpen(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 FFilterKernel := nil;
 FFilterFreq   := nil;
 FSignalFreq   := nil;
 FBuffer       := nil;
 SetLength(FSignalTime, max(numInputs, numOutputs));
 FFFTSize       := 0;

 IRSize := 64;

 // set editor form class
 EditorFormClass := TFmConvolution;
end;

procedure TConvolutionDataModule.VSTModuleClose(Sender: TObject);
var
  i : Integer;
begin
 Dispose(FFilterKernel);
 Dispose(FFilterFreq);
 Dispose(FSignalFreq);
 for i := 0 to Length(FSignalTime) - 1
  do Dispose(FSignalTime[i]);
 FreeAndNil(FFft);
end;

procedure TConvolutionDataModule.SetIRSize(const Value: Integer);
begin
 if FIRSize <> Value then
  begin
   FIRSize := Value;
   IRSizeChanged;
  end;
end;

procedure TConvolutionDataModule.IRSizeChanged;
var
  i : Integer;
begin
 i := 1 + CeilLog2(FIRSize);
 if not Assigned(FFft)
 {$IFDEF Use_IPPS}
  then FFft := TFftReal2ComplexIPPSFloat32.Create(i)
 {$ELSE}
  then
   begin
    FFft := TFftReal2ComplexNativeFloat32.Create(i);
    FFft.DataOrder := doPackedComplex;
   end
 {$ENDIF}
  else FFft.Order := i;

 FFFTSize        := FFft.FFTSize;
 FFFTSizeHalf    := FFFTSize div 2;
 FFFTSizeQuarter := FFFTSize div 4;
 ReallocMem(FFilterKernel, FFFTSize * SizeOf(Single));
 ReallocMem(FFilterFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
 ReallocMem(FBuffer, FFFTSize * SizeOf(Single));

 FillChar(FFilterKernel^[0], FFFTSize * SizeOf(Single), 0);
 FillChar(FFilterFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);
 FillChar(FBuffer^[0], FFFTSize * SizeOf(Single), 0);

 for i := 0 to Length(FSignalTime) - 1 do
  begin
   GetMem(FSignalTime[i], FFFTSize * SizeOf(Single));
   FillChar(FSignalTime[i]^[0], FFFTSize * SizeOf(Single), 0);
  end;

 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TConvolutionDataModule.LoadIR(FileName: TFileName);
var
  ADC : TAudioDataCollection32;
begin
 if Assigned(FFilterKernel) and Assigned(FFilterFreq) and Assigned(FFft) then
  begin
   FCriticalSection.Enter;
   try
    ADC := TAudioDataCollection32.Create(Self);
    with ADC do
     try
      LoadFromFile(FileName);

      IRSize := ADC.SampleFrames;
      Move(ADC[0].ChannelDataPointer^[0], FFilterKernel^[0], ADC.SampleFrames * SizeOf(Single));
      FillChar(FFilterKernel^[ADC.SampleFrames], (FFFTSize - ADC.SampleFrames) * SizeOf(Single), 0);
     finally
      FreeAndNil(ADC);
     end;

    // calculate frequency
    FFft.PerformFFT(FFilterFreq, FFilterKernel);
   finally
    FCriticalSection.Leave;
   end;
  end;
end;

procedure TConvolutionDataModule.PerformConvolution(Signal: PDAVSingleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, Signal);
 ComplexMultiplyBlock32(@FSignalFreq^[0], @FFilterFreq^[0], FFFTSizeHalf);
 FFft.PerformIFFT(FSignalFreq, Signal);
end;

procedure TConvolutionDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel     : Integer;
  SamplesPos  : Integer;
  SampleCount : Integer;
begin
 FCriticalSection.Enter;
 try
  if SampleFrames <= 0 then exit;
  SamplesPos := 0;

  repeat
   // calculate samples left to process
   SampleCount := SampleFrames - SamplesPos;

   // limit to 50% overlap if necessary
   if SampleCount > FFFTSizeHalf then SampleCount := FFFTSizeHalf;

   for Channel := 0 to numOutputs - 1 do
    begin
     Move(FSignalTime[Channel, SampleCount], FSignalTime[Channel, 0], (FFFTSize - SampleCount) * SizeOf(Single));
     Move(Inputs[Channel, SamplesPos], FSignalTime[Channel, (FFFTSize - SampleCount)], SampleCount * SizeOf(Single));
     Move(FSignalTime[Channel, 0], FBuffer[0], FFFTSize * SizeOf(Single));
     PerformConvolution(@FBuffer[0]);
     Move(FBuffer[(FFFTSize - SampleCount)], Outputs[Channel, SamplesPos], SampleCount * SizeOf(Single));
    end;
   Inc(SamplesPos, SampleCount);
  until SamplesPos >= SampleCount;

(*
  // complete block processing
  while SamplesPos + FFFTSizeHalf < SampleFrames do
   begin
    for Channel := 0 to numOutputs - 1 do
     begin
      Move(FSignalTime[Channel, FFFTSizeHalf], FSignalTime[Channel, 0], FFFTSizeHalf * SizeOf(Single));
      Move(Inputs[Channel, SamplesPos], FSignalTime[Channel, FFFTSizeHalf], FFFTSizeHalf * SizeOf(Single));
      Move(FSignalTime[Channel, 0], FBuffer[0], FFFTSize * SizeOf(Single));
      PerformConvolution(@FBuffer[0]);
      Move(FBuffer[FFFTSizeHalf], Outputs[Channel, SamplesPos], FFFTSizeHalf * SizeOf(Single));
     end;
    inc(SamplesPos, FFFTSizeHalf);
   end
*)

 finally
  FCriticalSection.Leave;
 end;
end;

end.
