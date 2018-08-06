unit QuadropolisDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  SyncObjs, Forms, DAV_Types, DAV_Complex, DAV_DspFftReal2Complex,
  DAV_VSTModule, {$IFDEF Use_IPPS}DAV_DspFftReal2ComplexIPPS, {$ENDIF}
  {$IFDEF Use_CUDA}DAV_DspFftReal2ComplexCUDA, {$ENDIF} DAV_DspHrtf;

type
  TQuadropolisDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs,
      Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FIR: array [0 .. 3, 0 .. 1] of PDAVSingleFixedArray;
    FLength: Integer;
    FHRTFs: THRTFs;

    FFilterFreq: array [0 .. 3, 0 .. 1] of PDAVComplexSingleFixedArray;
    FSignalFreq: array [0 .. 3] of PDAVComplexSingleFixedArray;
    FOutputFreq: array [0 .. 1] of PDAVComplexSingleFixedArray;
    FCriticalSection: TCriticalSection;
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
  Math, DAV_DspWindowing, QuadropolisGUI;

procedure TQuadropolisDataModule.VSTModuleCreate(Sender: TObject);
begin
  FCriticalSection := TCriticalSection.Create;
  BlockModeOverlap := BlockModeSize div 2;
end;

procedure TQuadropolisDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
  MemSize: Integer;
  RS: TResourceStream;
begin
  FLength := BlockModeSize;

  FHRTFs := THRTFs.Create;

  RS := TResourceStream.Create(hInstance, 'Default', 'HRTF');
  try
    FHRTFs.LoadFromStream(RS);
  finally
    RS.Free;
  end;

  {$IFDEF Use_IPPS}
  FFft := TFftReal2ComplexIPPSFloat32.Create(Round(Log2(FLength)));
  {$ELSE} {$IFDEF Use_CUDA}
  FFft := TFftReal2ComplexCUDA32.Create(Round(Log2(FLength)));
  {$ELSE}
  FFft := TFftReal2ComplexNativeFloat32.Create(Round(Log2(FLength)));
  {$ENDIF}{$ENDIF}
  FFft.AutoScaleType := astDivideInvByN;
  FFft.DataOrder := doPackedComplex;

  for Channel := 0 to Length(FIR) - 1 do
  begin
    {$IFDEF Use_IPPS}
    MemSize := (FLength div 2 + 1) * SizeOf(TComplex32);
    {$ELSE} {$IFDEF Use_CUDA}
    MemSize := FLength * SizeOf(Single);
    {$ELSE}
    MemSize := FLength * SizeOf(Single);
    {$ENDIF}{$ENDIF}
    GetMem(FIR[Channel, 0], FLength * SizeOf(Single));
    FillChar(FIR[Channel, 0]^, FLength * SizeOf(Single), 0);

    GetMem(FIR[Channel, 1], FLength * SizeOf(Single));
    FillChar(FIR[Channel, 1]^, FLength * SizeOf(Single), 0);

    GetMem(FFilterFreq[Channel, 0], MemSize);
    FillChar(FFilterFreq[Channel, 0]^[0], MemSize, 0);

    GetMem(FFilterFreq[Channel, 1], MemSize);
    FillChar(FFilterFreq[Channel, 1]^[0], MemSize, 0);

    GetMem(FSignalFreq[Channel], MemSize);
    FillChar(FSignalFreq[Channel]^[0], MemSize, 0);
  end;

  for Channel := 0 to Length(FOutputFreq) - 1 do
  begin
    GetMem(FOutputFreq[Channel], MemSize);
    FillChar(FOutputFreq[Channel]^[0], MemSize, 0);
  end;

  CalculateFilterKernel;

  // set editor form class
  EditorFormClass := TQuadropolisGUI;
end;

procedure TQuadropolisDataModule.VSTModuleClose(Sender: TObject);
var
  Channel: Integer;
begin
  for Channel := 0 to Length(FIR) - 1 do
  begin
    Dispose(FFilterFreq[Channel, 0]);
    Dispose(FFilterFreq[Channel, 1]);
    Dispose(FSignalFreq[Channel]);
    Dispose(FIR[Channel, 0]);
    Dispose(FIR[Channel, 1]);
  end;

  FreeAndNil(FFft);
  FreeAndNil(FHRTFs);
end;

procedure TQuadropolisDataModule.CalculateFilterKernel;
begin
  if Assigned(FHRTFs) then
    with FHRTFs do
    begin
      InterpolateHrir(-45, 0, FLength div 2, FIR[0, 0], FIR[0, 1]);
      InterpolateHrir(+45, 0, FLength div 2, FIR[1, 0], FIR[1, 1]);
      InterpolateHrir(+135, 0, FLength div 2, FIR[2, 0], FIR[2, 1]);
      InterpolateHrir(-135, 0, FLength div 2, FIR[3, 0], FIR[3, 1]);

      {$IFDEF Use_IPPS}
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[0, 0]),
        FIR[0, 0]);
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[0, 1]),
        FIR[0, 1]);
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[1, 0]),
        FIR[1, 0]);
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[1, 1]),
        FIR[1, 1]);
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[2, 0]),
        FIR[2, 0]);
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[2, 1]),
        FIR[2, 1]);
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[3, 0]),
        FIR[3, 0]);
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FFilterFreq[3, 1]),
        FIR[3, 1]);
      {$ELSE}{$IFDEF Use_CUDA}
      FFft.PerformFFT(FFilterFreq[0, 0], FIR[0, 0]);
      FFft.PerformFFT(FFilterFreq[0, 1], FIR[0, 1]);
      FFft.PerformFFT(FFilterFreq[1, 0], FIR[1, 0]);
      FFft.PerformFFT(FFilterFreq[1, 1], FIR[1, 1]);
      FFft.PerformFFT(FFilterFreq[2, 0], FIR[2, 0]);
      FFft.PerformFFT(FFilterFreq[2, 1], FIR[2, 1]);
      FFft.PerformFFT(FFilterFreq[3, 0], FIR[3, 0]);
      FFft.PerformFFT(FFilterFreq[3, 1], FIR[3, 1]);
      {$ELSE}
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[0, 0]
        ), FIR[0, 0]);
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[0, 1]
        ), FIR[0, 1]);
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[1, 0]
        ), FIR[1, 0]);
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[1, 1]
        ), FIR[1, 1]);
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[2, 0]
        ), FIR[2, 0]);
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[2, 1]
        ), FIR[2, 1]);
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[3, 0]
        ), FIR[3, 0]);
      FFft.PerformFFTPackedComplex(PDAVComplexSingleFixedArray(FFilterFreq[3, 1]
        ), FIR[3, 1]);
      {$ENDIF}{$ENDIF}
    end;
end;

procedure TQuadropolisDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel: Integer;
  Bin: Integer;
  Half: Integer;
begin
  FCriticalSection.Enter;
  Half := BlockModeSize div 2;
  try
    {$IFDEF Use_IPPS}
    // transform input channels to frequency domain
    for Channel := 0 to numInputs - 1 do
    begin
      FFft.PerformFFTCCS(PDAVComplexSingleFixedArray(FSignalFreq[Channel]),
        @Inputs[Channel, 0]);

      // DC & Nyquist
      FOutputFreq[0]^[0].Re := FOutputFreq[0]^[0].Re + FFilterFreq[Channel, 0]^
        [0].Re * FSignalFreq[Channel]^[0].Re;
      FOutputFreq[0]^[0].Im := FOutputFreq[0]^[0].Im + FFilterFreq[Channel, 0]^
        [0].Im * FSignalFreq[Channel]^[0].Im;
      FOutputFreq[0]^[Half].Re := FOutputFreq[0]^[Half].Re + FFilterFreq
        [Channel, 0]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

      for Bin := 1 to Half - 1 do
        FOutputFreq[0]^[Bin] := ComplexAdd32(FOutputFreq[0]^[Bin],
          ComplexMultiply32(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel,
          0]^[Bin]));

      // DC & Nyquist
      FOutputFreq[1]^[0].Re := FOutputFreq[1]^[0].Re + FFilterFreq[Channel, 1]^
        [0].Re * FSignalFreq[Channel]^[0].Re;
      FOutputFreq[1]^[0].Im := FOutputFreq[1]^[0].Im + FFilterFreq[Channel, 1]^
        [0].Im * FSignalFreq[Channel]^[0].Im;
      FOutputFreq[1]^[Half].Re := FOutputFreq[1]^[Half].Re + FFilterFreq
        [Channel, 1]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

      for Bin := 1 to Half - 1 do
        FOutputFreq[1]^[Bin] := ComplexAdd32(FOutputFreq[1]^[Bin],
          ComplexMultiply32(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel,
          1]^[Bin]));
    end;

    for Channel := 0 to numOutputs - 1 do
    begin
      FFft.PerformIFFTCCS(PDAVComplexSingleFixedArray(FOutputFreq[Channel]),
        @Outputs[Channel, 0]);
      FillChar(FOutputFreq[Channel]^[0], (BlockModeSize div 2 + 1) *
        SizeOf(TComplex32), 0);
    end;

    {$ELSE}{$IFDEF Use_CUDA}

    // transform input channels to frequency domain
    for Channel := 0 to numInputs - 1 do
    begin
      FFft.PerformFFT(FSignalFreq[Channel], @Inputs[Channel, 0]);

      // DC & Nyquist
      FOutputFreq[0]^[0].Re := FOutputFreq[0]^[0].Re + FFilterFreq[Channel, 0]^
        [0].Re * FSignalFreq[Channel]^[0].Re;
      FOutputFreq[0]^[0].Im := FOutputFreq[0]^[0].Im + FFilterFreq[Channel, 0]^
        [0].Im * FSignalFreq[Channel]^[0].Im;
      FOutputFreq[0]^[Half].Re := FOutputFreq[0]^[Half].Re + FFilterFreq
        [Channel, 0]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

      for Bin := 1 to Half - 1 do
        FOutputFreq[0]^[Bin] := ComplexAdd32(FOutputFreq[0]^[Bin],
          ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel,
          0]^[Bin]));

      // DC & Nyquist
      FOutputFreq[1]^[0].Re := FOutputFreq[1]^[0].Re + FFilterFreq[Channel, 1]^
        [0].Re * FSignalFreq[Channel]^[0].Re;
      FOutputFreq[1]^[0].Im := FOutputFreq[1]^[0].Im + FFilterFreq[Channel, 1]^
        [0].Im * FSignalFreq[Channel]^[0].Im;
      FOutputFreq[1]^[Half].Re := FOutputFreq[1]^[Half].Re + FFilterFreq
        [Channel, 1]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

      for Bin := 1 to Half - 1 do
        FOutputFreq[1]^[Bin] := ComplexAdd(FOutputFreq[1]^[Bin],
          ComplexMultiply(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel,
          1]^[Bin]));
    end;

    for Channel := 0 to numOutputs - 1 do
    begin
      FFft.PerformIFFT(FOutputFreq[Channel], @Outputs[Channel, 0]);
      FillChar(FOutputFreq[Channel]^[0], BlockModeSize * SizeOf(Single), 0);
    end;
    {$ELSE}
    // transform input channels to frequency domain
    for Channel := 0 to numInputs - 1 do
    begin
      FFft.PerformFFTPackedComplex
        (PDAVComplexSingleFixedArray(FSignalFreq[Channel]),
        @Inputs[Channel, 0]);

      // DC & Nyquist
      FOutputFreq[0]^[0].Re := FOutputFreq[0]^[0].Re + FFilterFreq[Channel, 0]^
        [0].Re * FSignalFreq[Channel]^[0].Re;
      FOutputFreq[0]^[0].Im := FOutputFreq[0]^[0].Im + FFilterFreq[Channel, 0]^
        [0].Im * FSignalFreq[Channel]^[0].Im;
      FOutputFreq[0]^[Half].Re := FOutputFreq[0]^[Half].Re + FFilterFreq
        [Channel, 0]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

      for Bin := 1 to Half - 1 do
        FOutputFreq[0]^[Bin] := ComplexAdd32(FOutputFreq[0]^[Bin],
          ComplexMultiply32(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel,
          0]^[Bin]));

      // DC & Nyquist
      FOutputFreq[1]^[0].Re := FOutputFreq[1]^[0].Re + FFilterFreq[Channel, 1]^
        [0].Re * FSignalFreq[Channel]^[0].Re;
      FOutputFreq[1]^[0].Im := FOutputFreq[1]^[0].Im + FFilterFreq[Channel, 1]^
        [0].Im * FSignalFreq[Channel]^[0].Im;
      FOutputFreq[1]^[Half].Re := FOutputFreq[1]^[Half].Re + FFilterFreq
        [Channel, 1]^[Half].Re * FSignalFreq[Channel]^[Half].Re;

      for Bin := 1 to Half - 1 do
        FOutputFreq[1]^[Bin] := ComplexAdd32(FOutputFreq[1]^[Bin],
          ComplexMultiply32(FSignalFreq[Channel]^[Bin], FFilterFreq[Channel,
          1]^[Bin]));
    end;

    for Channel := 0 to numOutputs - 1 do
    begin
      FFft.PerformIFFTPackedComplex
        (PDAVComplexSingleFixedArray(FOutputFreq[Channel]),
        @Outputs[Channel, 0]);
      FillChar(FOutputFreq[Channel]^[0], BlockModeSize * SizeOf(Single), 0);
    end;
    {$ENDIF}{$ENDIF}
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TQuadropolisDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
  FCriticalSection.Enter;
  try
    CalculateFilterKernel;
  finally
    FCriticalSection.Leave;
  end;
end;

end.
