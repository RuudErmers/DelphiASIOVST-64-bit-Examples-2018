unit DAV_DspBandlimitedImpulseTrain;

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
  DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterAllpasses;

type
  TCustomSimpleBandlimitedImpulseTrain = class(TDspSampleRatePersistent)
  private
    procedure SetFrequency(const Value: Single);
    procedure SetBufferSize(const Value: Integer);
  protected
    FBufferPos  : Integer;
    FBufferSize : Integer;
    FFrequency  : Single;
    procedure FrequencyChanged; virtual; abstract;
    procedure BufferSizeChanged; virtual; abstract;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
  public
    constructor Create; override;
    procedure Reset; virtual;

    property Frequency: Single read FFrequency write SetFrequency;
  end;

  TCustomSimpleBandlimitedImpulseTrain32 = class(TCustomSimpleBandlimitedImpulseTrain)
  private
    procedure SetFractional(const Value: Single);
  protected
    FBuffer         : PDAVSingleFixedArray;
    FRealBufferSize : Integer;
    FFractional     : Single;
    FAllpass        : TThiranAllpass2ndOrder;
    procedure BufferSizeChanged; override;
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; override;
    procedure FractionalChanged; virtual;
    procedure CalculateBufferSize; virtual;

    property Fractional: Single read FFractional write SetFractional;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; override;
  end;

  TSimpleBandlimitedImpulseTrain32 = class(TCustomSimpleBandlimitedImpulseTrain32,
    IDspGenerator32)
  private
    FLastSample : Single;
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32: Single;
    procedure Reset; override;
  published
    property Samplerate;
    property Frequency;
  end;

  TSimpleBandlimitedBipolarImpulseTrain32 = class(TCustomSimpleBandlimitedImpulseTrain32,
    IDspGenerator32)
  private
    FLastSample : Single;
  public
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32: Single; virtual;
    procedure Reset; override;
  published
    property Samplerate;
    property Frequency;
  end;

  TSynchronizedImpulseTrain32 = class(TDspSampleRatePersistent,
    IDspGenerator32)
  private
    FFrequency : Single;
    procedure SetFrequency(const Value: Single);
  protected
    FImpulseTrains : array [0..1] of TSimpleBandlimitedImpulseTrain32;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample32: Single; virtual;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray;
      SampleCount: Integer);

    property Frequency: Single read FFrequency write SetFrequency;
  end;

implementation

uses
  Math, SysUtils, DAV_Complex, DAV_DspInterpolation;

{ TCustomSimpleBandlimitedImpulseTrain }

constructor TCustomSimpleBandlimitedImpulseTrain.Create;
begin
 inherited Create;
 FFrequency := 440;
end;

procedure TCustomSimpleBandlimitedImpulseTrain.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TCustomSimpleBandlimitedImpulseTrain.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomSimpleBandlimitedImpulseTrain.Reset;
begin
 FBufferPos := 0;
end;


{ TCustomSimpleBandlimitedImpulseTrain32 }

constructor TCustomSimpleBandlimitedImpulseTrain32.Create;
begin
 inherited;
 FBuffer := nil;
 FAllpass := TThiranAllpass2ndOrder.Create; 
 CalculateBufferSize;
end;

destructor TCustomSimpleBandlimitedImpulseTrain32.Destroy;
begin
 Dispose(FBuffer);
 FreeAndNil(FAllpass);
 inherited;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 Reset;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
 FBufferPos := 0;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.CalculateBufferSize;
var
  Samples : Double;
begin
 Samples := SampleRate / FFrequency;
 BufferSize := Round(Samples - 0.5) - 2;
 Fractional := BufferSize + 3 - Samples;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.FractionalChanged;
begin
 assert(FFractional >= 0);
 assert(FFractional <= 1);
 FAllpass.PhaseDelay := 1 + FFractional;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.SampleRateChanged;
begin
 inherited;
 CalculateBufferSize;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.SetFractional(const Value: Single);
begin
 if FFractional <> Value then
  begin
   FFractional := Value;
   FractionalChanged;
  end;
end;

procedure TCustomSimpleBandlimitedImpulseTrain32.FrequencyChanged;
begin
 inherited;
 CalculateBufferSize;
end;


{ TSimpleBandlimitedImpulseTrain32 }

procedure TSimpleBandlimitedImpulseTrain32.Reset;
begin
 inherited;
 FLastSample := 1;
end;

procedure TSimpleBandlimitedImpulseTrain32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32;
end;

function TSimpleBandlimitedImpulseTrain32.ProcessSample32: Single;
begin
 FBuffer^[FBufferPos] := FLastSample;

 // decrease buffer position
 if FBufferPos = 0
  then FBufferPos := BufferSize - 1
  else Dec(FBufferPos);

 FLastSample := FAllpass.ProcessSample64(FBuffer^[FBufferPos]);
 Result := FLastSample;
end;

{ TSimpleBandlimitedBipolarImpulseTrain32 }

procedure TSimpleBandlimitedBipolarImpulseTrain32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32;
end;

function TSimpleBandlimitedBipolarImpulseTrain32.ProcessSample32: Single;
begin
 FBuffer^[FBufferPos] := -FLastSample;

 // decrease buffer position
 if FBufferPos = 0
  then FBufferPos := BufferSize - 1
  else Dec(FBufferPos);

 FLastSample := FAllpass.ProcessSample64(FBuffer^[FBufferPos]);
 Result := FLastSample;
end;

procedure TSimpleBandlimitedBipolarImpulseTrain32.Reset;
begin
 inherited;
 FLastSample := 1;
end;

{ TSynchronizedImpulseTrain32 }

constructor TSynchronizedImpulseTrain32.Create;
begin
 inherited;
 FFrequency := 1000;
 FImpulseTrains[0] := TSimpleBandlimitedImpulseTrain32.Create;
 FImpulseTrains[1] := TSimpleBandlimitedImpulseTrain32.Create;
end;

destructor TSynchronizedImpulseTrain32.Destroy;
begin
 FreeAndNil(FImpulseTrains[0]);
 FreeAndNil(FImpulseTrains[1]);
 inherited;
end;

procedure TSynchronizedImpulseTrain32.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleCount - 1
  do Data[SampleIndex] := ProcessSample32;
end;

function TSynchronizedImpulseTrain32.ProcessSample32: Single;
begin
 Result := FImpulseTrains[0].ProcessSample32;
// FImpulseTrains[1].ProcessSample;
end;

procedure TSynchronizedImpulseTrain32.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FImpulseTrains[0].Frequency := FFrequency;
   FImpulseTrains[1].Frequency := FFrequency;
  end;
end;

procedure TSynchronizedImpulseTrain32.SampleRateChanged;
begin
 FImpulseTrains[0].SampleRate := SampleRate;
 FImpulseTrains[1].SampleRate := SampleRate;
end;

end.
