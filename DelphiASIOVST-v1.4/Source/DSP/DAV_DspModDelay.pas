unit DAV_DspModDelay;

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

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspLFO, DAV_DspFilterButterworth;

type
  TCustomModDelay = class(TDspSampleRatePersistent)
  private
    FDelay          : Double;
    FDepth          : Double;
    FFeedback       : Double;
    FFeedbackFactor : Double;
    FMix            : Double;
    FMixFactors     : array [0..1] of Single;
    FRate           : Double;
    FLpfFreq        : Double;
    FRealBufSize    : Integer;
    FBufferSize     : Integer;
    FBufferPos      : Integer;
    FLFO            : TLFOSine;
    FLowpassFilter  : TButterworthLowPassFilter;
    procedure SetDepth(const Value: Double);
    procedure SetRate(const Value: Double);
    procedure SetDelay(const Value: Double);
    procedure SetFeedback(const Value: Double);
    procedure SetMix(const Value: Double);
    procedure SetLpfFreq(const Value: Double);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DelayChanged; virtual;
    procedure DepthChanged; virtual;
    procedure FeedbackChanged; virtual;
    procedure LowpassFrequencyChanged; virtual;
    procedure MixChanged; virtual;
    procedure RateChanged; virtual;
    procedure SampleRateChanged; override;
    procedure UpdateBuffer; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; virtual;

    property Mix: Double read FMix write SetMix;
    property Delay: Double read FDelay write SetDelay;
    property Feedback: Double read FFeedback write SetFeedback;
    property LowpassFrequency: Double read FLpfFreq write SetLpfFreq;
    property Rate: Double read FRate write SetRate;
    property Depth: Double read FDepth write SetDepth;
  end;

  TCustomModDelay32 = class(TCustomModDelay, IDspProcessor32)
  private
    FBuffer32 : PDAVSingleFixedArray;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

    procedure Reset; override;
  end;

  TModDelay32 = class(TCustomModDelay32)
  published
    property Mix;              // [%]
    property Delay;            // [ms]
    property Depth;            // [%]
    property Feedback;         // [%]
    property LowpassFrequency; // [Hz]
    property Rate;             // [Hz]
    property SampleRate;
  end;

  TCustomModDelay64 = class(TCustomModDelay, IDspProcessor64)
  private
    FBuffer64 : PDAVDoubleFixedArray;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

    procedure Reset; override;
  end;

  TModDelay64 = class(TCustomModDelay64)
  published
    property Mix;              // [%]
    property Delay;            // [ms]
    property Depth;            // [%]
    property Feedback;         // [%]
    property LowpassFrequency; // [Hz]
    property Rate;             // [Hz]
    property SampleRate;
  end;

implementation

uses
  SysUtils, DAV_DspInterpolation;

{ TCustomModDelay }

constructor TCustomModDelay.Create;
begin
 inherited;
 FRate          := 2;
 FDepth         := 0.5;
 FBufferPos     := 0;

 // create and setup sine LFO
 FLFO := TLFOSine.Create;
 FLFO.SampleRate := SampleRate;

 // create and setup lowpass filter
 FLowpassFilter := TButterworthLowPassFilter.Create(1);
 FLowpassFilter.SampleRate := SampleRate;
end;

destructor TCustomModDelay.Destroy;
begin
 FreeAndNil(FLFO);
 FreeAndNil(FLowpassFilter);
 inherited;
end;

procedure TCustomModDelay.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomModDelay then
  with TCustomModDelay(Dest) do
   begin
    inherited;
    FDelay          := Self.FDelay;
    FDepth          := Self.FDepth;
    FFeedback       := Self.FFeedback;
    FFeedbackFactor := Self.FFeedbackFactor;
    FMix            := Self.FMix;
    FMixFactors     := Self.FMixFactors;
    FRate           := Self.FRate;
    FLpfFreq        := Self.FLpfFreq;
    FRealBufSize    := Self.FRealBufSize;
    FBufferSize     := Self.FBufferSize;
    FBufferPos      := Self.FBufferPos;

    UpdateBuffer;

    FLFO.Assign(Self.FLFO);
    FLowpassFilter.Assign(Self.FLowpassFilter);
   end
 else inherited;
end;

procedure TCustomModDelay.Reset;
begin
 FLFO.Reset;
end;

procedure TCustomModDelay.UpdateBuffer;
begin
 // calculate buffer size in samples
 FBufferSize  := round(0.001 * FDelay * abs(SampleRate));
 FRealBufSize := FBufferSize + 8;

 // check and reset buffer position
 if FBufferPos >= FRealBufSize
  then FBufferPos := 4;
end;

procedure TCustomModDelay.DelayChanged;
begin
 UpdateBuffer;
 Changed;
end;

procedure TCustomModDelay.DepthChanged;
begin
 FLFO.Amplitude := 0.01 * FDepth;
 Changed;
end;

procedure TCustomModDelay.FeedbackChanged;
begin
 FFeedbackFactor := 0.01 * FFeedback;
 Changed;
end;

procedure TCustomModDelay.LowpassFrequencyChanged;
begin
 FLowpassFilter.Frequency := FLpfFreq;
 Changed;
end;

procedure TCustomModDelay.MixChanged;
begin
 FMixFactors[1] := 0.01 * FMix;
 FMixFactors[0] := 1 - FMixFactors[1];
 Changed;
end;

procedure TCustomModDelay.RateChanged;
begin
 FLFO.Frequency := Rate;
 Changed;
end;

procedure TCustomModDelay.SampleRateChanged;
begin
 FLFO.SampleRate := SampleRate;
 FLowpassFilter.SampleRate := SampleRate;
 UpdateBuffer;
 Changed;
end;

procedure TCustomModDelay.SetDelay(const Value: Double);
begin
 if FDelay <> Value then
  begin
   FDelay := Value;
   DelayChanged;
  end;
end;

procedure TCustomModDelay.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomModDelay.SetFeedback(const Value: Double);
begin
 if FFeedback <> Value then
  begin
   FFeedback := Value;
   FeedbackChanged;
  end;
end;

procedure TCustomModDelay.SetLpfFreq(const Value: Double);
begin
 if FLpfFreq <> Value then
  begin
   FLpfFreq := Value;
   LowpassFrequencyChanged;
  end;
end;

procedure TCustomModDelay.SetMix(const Value: Double);
begin
 if FMix <> Value then
  begin
   FMix := Value;
   MixChanged;
  end;
end;

procedure TCustomModDelay.SetRate(const Value: Double);
begin
 if FRate <> Value then
  begin
   FRate := Value;
   RateChanged;
  end;
end;


{ TCustomModDelay32 }

procedure TCustomModDelay32.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TCustomModDelay32 then
  with TCustomModDelay32(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    Move(Self.FBuffer32^, FBuffer32^, FRealBufSize * SizeOf(Single));
   end else
 if Dest is TCustomModDelay64 then
  with TCustomModDelay64(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    for Sample := 0 to FRealBufSize - 1
     do FBuffer64^[Sample] := Self.FBuffer32^[Sample];
   end
 else inherited;
end;

constructor TCustomModDelay32.Create;
begin
 inherited;
 FBuffer32 := nil;
end;

destructor TCustomModDelay32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TCustomModDelay32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TCustomModDelay32.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FRealBufSize * SizeOf(Single));
end;

procedure TCustomModDelay32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TCustomModDelay32.ProcessSample32(Input: Single): Single;
var
  p : Integer;
  d : Double;
begin
 inherited;

 // calculate next LFO position
 FLFO.CalculateNextSample;
 d := 4 + FBufferSize * 0.5 * (1 - FLFO.Sine);

 // calculate absolute sample position
 p := round(d - 0.5);
 d := d - p;
 p := FBufferPos + p;
 if p >= FRealBufSize
  then p := p - (FRealBufSize - 4) else
 if p < 4 then p := p + (FRealBufSize - 4);

 // calculate pure result
 result := FLowpassFilter.ProcessSample64(Hermite32_asm(d, @FBuffer32[p - 4]));

 // store new data
 FBuffer32[FBufferPos] := Input + FFeedbackFactor * result;

 // apply mix
 result := FMixFactors[0] * Input + FMixFactors[1] * result;

 // advance buffer position
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferPos := 4;
  end;
end;

{ TCustomModDelay64 }

procedure TCustomModDelay64.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TCustomModDelay32 then
  with TCustomModDelay32(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    for Sample := 0 to FRealBufSize - 1
     do FBuffer32^[Sample] := Self.FBuffer64^[Sample];
   end else
 if Dest is TCustomModDelay64 then
  with TCustomModDelay64(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    Move(Self.FBuffer64^, FBuffer64^, FRealBufSize * SizeOf(Double));
   end
 else inherited;
end;

constructor TCustomModDelay64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TCustomModDelay64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TCustomModDelay64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TCustomModDelay64.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FRealBufSize * SizeOf(Double));
end;

procedure TCustomModDelay64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomModDelay64.ProcessSample64(Input: Double): Double;
var
  p : Integer;
  d : Double;
begin
 inherited;

 // calculate next LFO position
 FLFO.CalculateNextSample;
 d := 4 + FBufferSize * 0.5 * (1 - FLFO.Sine);

 // calculate absolute sample position
 p := round(d - 0.5);
 d := d - p;
 p := FBufferPos + p;
 if p >= FRealBufSize
  then p := p - (FRealBufSize - 4) else
 if p < 4 then p := p + (FRealBufSize - 4);

 // calculate pure result
 result := FLowpassFilter.ProcessSample64(Hermite64_asm(d, @FBuffer64[p - 4]));

 // store new data
 FBuffer64[FBufferPos] := Input + FFeedbackFactor * result;

 // apply mix
 result := FMixFactors[0] * Input + FMixFactors[1] * result;

 // advance buffer position
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer64[FRealBufSize - 4], FBuffer64[0], 4 * SizeOf(Double));
   FBufferPos := 4;
  end;
end;

initialization
  RegisterDspProcessor32(TModDelay32);
  RegisterDspProcessor64(TModDelay64);

end.
