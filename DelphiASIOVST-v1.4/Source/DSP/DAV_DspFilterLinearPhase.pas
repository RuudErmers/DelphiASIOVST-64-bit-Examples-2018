unit DAV_DspFilterLinearPhase;

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

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspConvolution, DAV_DspDelayLines;

type
  TCustomLinearPhaseBandSplitter = class(TDspSampleRatePersistent)
  private
    FFFTOrder   : Integer;
    FKernelSize : Integer;
    FLatency    : Integer;
    procedure SetFFTOrder(const Value: Integer);
  protected
    FConvolution : TLowLatencyConvolution32;
    FDelay       : TDelayLineSamples32;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateFilterKernel; virtual; abstract;
    procedure FFTOrderChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create(const FFTOrder: Integer = 8); reintroduce; virtual;
    destructor Destroy; override;

    procedure ProcessSample(const Input: Single; out Low, High: Single); overload;
    procedure ProcessSample(const Input: Double; out Low, High: Double); overload;
    procedure ProcessBlock(const Input: PDAVSingleFixedArray; out Low, High: PDAVSingleFixedArray; SampleFrames: Integer); overload;
    procedure ProcessBlock(const Input: PDAVDoubleFixedArray; out Low, High: PDAVDoubleFixedArray; SampleFrames: Integer); overload;

    property FFTOrder: Integer read FFFTOrder write SetFFTOrder;
    property Latency: Integer read FLatency;
    property SampleRate;
  end;

  TArbitraryLinearPhaseBandSplitter = class(TCustomLinearPhaseBandSplitter)
  private
    FFrequency  : Single;
    procedure SetFrequency(const Value: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateFilterKernel; override;
    procedure FrequencyChanged; virtual;
  public
    constructor Create(const FFTOrder: Integer = 7); override;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property FFTOrder;
    property SampleRate;
  end;

  THalfbandLinearPhaseBandSplitter = class(TCustomLinearPhaseBandSplitter)
  protected
    procedure CalculateFilterKernel; override;
  published
    property FFTOrder;
    property SampleRate;
  end;

implementation

uses
  Math, SysUtils, DAV_DspWindowing;

{ TCustomLinearPhaseBandSplitter }

constructor TCustomLinearPhaseBandSplitter.Create(const FFTOrder: Integer = 8);
begin
 inherited Create;
 FFFTOrder := FFTOrder;

 FConvolution := TLowLatencyConvolution32.Create;
 with FConvolution do
  begin
   MinimumIRBlockOrder := 5;
   MaximumIRBlockOrder := 17;
  end;

 FKernelSize := 1 shl FFFTOrder;
 FLatency := (FKernelSize div 2) + FConvolution.Latency;
 FDelay := TDelayLineSamples32.Create(FLatency);

 CalculateFilterKernel;
end;

destructor TCustomLinearPhaseBandSplitter.Destroy;
begin
 FreeAndNil(FConvolution);
 FreeAndNil(FDelay);
 inherited;
end;

procedure TCustomLinearPhaseBandSplitter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomLinearPhaseBandSplitter then
  with TCustomLinearPhaseBandSplitter(Dest) do
   begin
    inherited;
    FConvolution.Assign(Self.FConvolution);
    FDelay.Assign(Self.FDelay);

    FKernelSize := Self.FKernelSize;
    FFFTOrder   := Self.FFFTOrder;
   end
 else inherited;
end;

procedure TCustomLinearPhaseBandSplitter.SetFFTOrder(const Value: Integer);
begin
 if FFFTOrder <> Value then
  begin
   FFFTOrder := Value;
   FFTOrderChanged;
  end;
end;

procedure TCustomLinearPhaseBandSplitter.FFTOrderChanged;
begin
 FKernelSize := 1 shl FFFTOrder;
 FLatency := (FKernelSize div 2) + FConvolution.Latency;
 FDelay.BufferSize := FLatency;
 CalculateFilterKernel;
end;

procedure TCustomLinearPhaseBandSplitter.SampleRateChanged;
begin
 CalculateFilterKernel;
end;

procedure TCustomLinearPhaseBandSplitter.ProcessSample(const Input: Single; out Low,
  High: Single);
begin
 Low := FConvolution.ProcessSample32(Input);
 High := FDelay.ProcessSample32(Input) - Low;
end;

procedure TCustomLinearPhaseBandSplitter.ProcessSample(const Input: Double; out Low,
  High: Double);
begin
 Low := FConvolution.ProcessSample32(Input);
 High := FDelay.ProcessSample32(Input) - Low;
end;

procedure TCustomLinearPhaseBandSplitter.ProcessBlock(const Input: PDAVSingleFixedArray; out Low,
  High: PDAVSingleFixedArray; SampleFrames: Integer);
begin
 FConvolution.ProcessBlock(Input, Low, SampleFrames);
end;

procedure TCustomLinearPhaseBandSplitter.ProcessBlock(const Input: PDAVDoubleFixedArray; out Low,
  High: PDAVDoubleFixedArray; SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Low^[Sample]  := FConvolution.ProcessSample32(Input^[Sample]);
   High^[Sample] := FDelay.ProcessSample32(Input^[Sample]) - Low^[Sample];
  end;
end;


{ TArbitraryLinearPhaseBandSplitter }

constructor TArbitraryLinearPhaseBandSplitter.Create(const FFTOrder: Integer);
begin
 inherited;
 FFrequency := 1000;
end;

procedure TArbitraryLinearPhaseBandSplitter.AssignTo(Dest: TPersistent);
begin
 if Dest is TArbitraryLinearPhaseBandSplitter then
  with TArbitraryLinearPhaseBandSplitter(Dest) do
   begin
    inherited;
    FFrequency  := Self.FFrequency;
   end
 else inherited;
end;

procedure TArbitraryLinearPhaseBandSplitter.CalculateFilterKernel;
var
  i            : Integer;
  Cutoff, Temp : Double;
  FilterKernel : PDAVSingleFixedArray;
begin
 GetMem(FilterKernel, FKernelSize * SizeOf(Single));
 try
  Cutoff := FFrequency / SampleRate;

  // Generate sinc delayed by (N-1)/2
  for i := 0 to FKernelSize - 1 do
   if (2 * i = FKernelSize)
    then FilterKernel^[i] := 2.0 * Cutoff
    else
     begin
      Temp := PI * (i - FKernelSize * 0.5);
      FilterKernel^[i] := sin(2.0 * Cutoff * Temp) / Temp;
     end;
//  ApplyHanningWindow(FilterKernel, FKernelSize);

  FConvolution.LoadImpulseResponse(FilterKernel, FKernelSize);
 finally
  Dispose(FilterKernel);
 end;
end;

procedure TArbitraryLinearPhaseBandSplitter.FrequencyChanged;
begin
 CalculateFilterKernel;
end;

procedure TArbitraryLinearPhaseBandSplitter.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;


{ THalfbandLinearPhaseBandSplitter }

procedure THalfbandLinearPhaseBandSplitter.CalculateFilterKernel;
var
  i            : Integer;
  Temp, Scale  : Double;
  FilterKernel : PDAVSingleFixedArray;
begin
 GetMem(FilterKernel, FKernelSize * SizeOf(Single));
 try
  Scale := (FKernelSize - 1) * 0.5;
  // Generate sinc delayed by (N-1)/2
  for i := 0 to (FKernelSize div 2) - 1 do
   begin
    Temp := PI * (i - Scale);
    FilterKernel^[i] := Sin(Temp) / Temp;
    FilterKernel^[FKernelSize - i] := -FilterKernel^[i];
   end;
  ApplyHanningWindow(FilterKernel, FKernelSize);

  FConvolution.LoadImpulseResponse(FilterKernel, FKernelSize);
 finally
  Dispose(FilterKernel);
 end;
end;

end.
