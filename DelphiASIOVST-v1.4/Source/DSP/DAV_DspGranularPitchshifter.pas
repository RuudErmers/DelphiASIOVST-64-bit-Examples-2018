unit DAV_DspGranularPitchShifter;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter;

type
  TFractionalDelayAllpass = class(TDspPersistent, IDspProcessor32, IDspProcessor64)
  protected
    FState      : Double;
    FFractional : Single;
  public
    constructor Create; virtual;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double; virtual;
    function ProcessSample32(Input: Single): Single; virtual;

    property Fractional: Single read FFractional write FFractional;
  end;

  TGranularPitchShifterStage = class(TDspPersistent)
  private
    FAllpass        : TFractionalDelayAllpass;
    FBufferOffset   : Integer;
    FEnvelopePos    : Double;
    procedure SetFractional(const Value: Double);
    function GetFractional: Double;
  public
    constructor Create; virtual;
    property Allpass: TFractionalDelayAllpass read FAllpass;
    property BufferOffset: Integer read FBufferOffset;
    property Fractional: Double read GetFractional write SetFractional;
  end;

  TCustomDspGranularPitchShifter = class(TDspSampleRatePersistent)
  private
    FSampleRateInv     : Double;
    FSampleOffset      : Double;
    FEnvelopeOffset    : Double;
    FBufferSize        : Integer;
    FBufferPos         : Integer;
    FPitchShifterStage : array of TGranularPitchShifterStage;
    FStages            : Byte;
    FStageMix          : Double;
    FSemitones         : Double;
    FGranularity       : Double;
    procedure SetStages(const Value: Byte);
    procedure SetSemitones(const Value: Double);
    procedure SetGranularity(const Value: Double);
    procedure CalculateEnvelopeOffset;
    procedure CalculateSampleRateReciprocal;
  protected
    procedure SampleRateChanged; override;
    procedure StagesChanged; virtual;
    procedure SemitonesChanged; virtual;
    procedure GranularityChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure Reset; virtual; abstract;
  published
    property Semitones: Double read FSemitones write SetSemitones;
    property Granularity: Double read FGranularity write SetGranularity; // in s
    property Stages: Byte read FStages write SetStages default 0;
  end;

  TDspGranularPitchShifter32 = class(TCustomDspGranularPitchShifter,
    IDspProcessor32)
  private
    FBuffer32 : PDAVSingleFixedArray;
  protected
    procedure UpdateBuffer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

    procedure Reset; override;
  published
    property SampleRate;
    property Stages;
  end;

  TDspGranularPitchShifter64 = class(TCustomDspGranularPitchShifter,
    IDspProcessor64)
  private
    FBuffer64 : PDAVDoubleFixedArray;
  protected
    procedure UpdateBuffer; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double;

    procedure Reset; override;
  published
    property SampleRate;
    property Stages;
  end;

implementation

uses
  SysUtils, Math, DAV_DspInterpolation;

{ TFractionalDelayAllpass }

constructor TFractionalDelayAllpass.Create;
begin
 FState := 0;
end;

procedure TFractionalDelayAllpass.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TFractionalDelayAllpass.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TFractionalDelayAllpass.ProcessSample32(Input: Single): Single;
begin
 Result := FState + FFractional * Input;
 FState := Input - FFractional * Result;
end;

function TFractionalDelayAllpass.ProcessSample64(Input: Double): Double;
begin
 Result := FState + FFractional * Input;
 FState := Input - FFractional * Result;
end;

{ TGranularPitchShifterStage }

constructor TGranularPitchShifterStage.Create;
begin
 inherited;
 FAllpass := TFractionalDelayAllpass.Create;
 FBufferOffset := 0;
end;

function TGranularPitchShifterStage.GetFractional: Double;
begin
 Result := FAllpass.Fractional;
end;

procedure TGranularPitchShifterStage.SetFractional(const Value: Double);
begin
 if FAllpass.Fractional <> Value then
  begin
   FAllpass.Fractional := Value;
   while FAllpass.Fractional > 1 do
    begin
     FAllpass.Fractional := FAllpass.Fractional - 1;
     Inc(FBufferOffset);
    end;
   while FAllpass.Fractional < 0 do
    begin
     FAllpass.Fractional := FAllpass.Fractional + 1;
     Dec(FBufferOffset);
    end;
  end;
end;


{ TCustomDspGranularPitchShifter }

constructor TCustomDspGranularPitchShifter.Create;
begin
 inherited;
 FBufferPos     := 0;
 FSemitones     := 0;
 FGranularity   := 0.05;
 Stages         := 2;
 SampleRateChanged;
end;

procedure TCustomDspGranularPitchShifter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspGranularPitchShifter then
  with TCustomDspGranularPitchShifter(Dest) do
   begin
    Semitones    := Self.FSemitones;
    Stages       := Self.Stages;
   end
 else inherited;
end;

procedure TCustomDspGranularPitchShifter.StagesChanged;
var
  i            : Integer;
  BaseStage, d : Double;
  BaseEnv      : Double;
  StageOffset  : Double;
begin
 with FPitchShifterStage[0] do BaseStage := FBufferOffset + Fractional;
 StageOffset := 1 / FStages;
 if FBufferSize > 0 then
  begin
   BaseEnv := BaseStage / FBufferSize;
   FPitchShifterStage[0].FEnvelopePos := BaseEnv;
//   assert(abs(BaseEnv - FPitchShifterStage[0].FEnvelopePos) < 1E-3);
   for i := 1 to FStages - 1 do
    with FPitchShifterStage[i] do
     begin
      FPitchShifterStage[i].FEnvelopePos := i * StageOffset + BaseEnv;
      while FEnvelopePos > 1 do FEnvelopePos := FEnvelopePos - 1;
      d := BaseStage + i * StageOffset * FBufferSize;
      FBufferOffset := round(d + 0.500001) - 1;
      FAllpass.FFractional := d - FBufferOffset;
      while FBufferOffset > FBufferSize
       do FBufferOffset := FBufferOffset - FBufferSize;
     end;
  end;

 FStageMix := StageOffset;
end;

procedure TCustomDspGranularPitchShifter.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := round(FGranularity * SampleRate) + 1; // quarter second

 // check and reset buffer position
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;

 StagesChanged;
 CalculateEnvelopeOffset;
end;

procedure TCustomDspGranularPitchShifter.SampleRateChanged;
begin
 CalculateSampleRateReciprocal;
 UpdateBuffer;
 inherited;
end;

procedure TCustomDspGranularPitchShifter.CalculateSampleRateReciprocal;
begin
 FSampleRateInv := 1 / SampleRate;
end;

procedure TCustomDspGranularPitchShifter.SemitonesChanged;
begin
 FSampleOffset := Power(2, FSemitones / 12) - 1;
 CalculateEnvelopeOffset;
 Changed;
end;

procedure TCustomDspGranularPitchShifter.SetGranularity(const Value: Double);
begin
 if FGranularity <> Value then
  begin
   FGranularity := Value;
   GranularityChanged;
  end;
end;

procedure TCustomDspGranularPitchShifter.CalculateEnvelopeOffset;
begin
 FEnvelopeOffset := abs(FSampleOffset / FBufferSize);
end;

procedure TCustomDspGranularPitchShifter.GranularityChanged;
begin
 UpdateBuffer;
 Changed;
end;

procedure TCustomDspGranularPitchShifter.SetSemitones(const Value: Double);
begin
 if FSemitones <> Value then
  begin
   FSemitones := Value;
   SemitonesChanged;
  end;
end;

procedure TCustomDspGranularPitchShifter.SetStages(const Value: Byte);
var
  i : Integer;
begin
 if FStages <> Value then
  begin
   if FStages > Value then
    begin
     FStages := Value;
     for i := Length(FPitchShifterStage) - 1 downto FStages
      do FreeAndNil(FPitchShifterStage[i]);
     SetLength(FPitchShifterStage, FStages);
    end
   else
    begin
     SetLength(FPitchShifterStage, Value);
     for i := FStages to Length(FPitchShifterStage) - 1
      do FPitchShifterStage[i] := TGranularPitchShifterStage.Create;
     FStages := Value;
    end;
   StagesChanged;
  end;
end;


{ TDspGranularPitchShifter32 }

constructor TDspGranularPitchShifter32.Create;
begin
 FBuffer32 := nil;
 inherited;
end;

destructor TDspGranularPitchShifter32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TDspGranularPitchShifter32.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TDspGranularPitchShifter32 then
  with TDspGranularPitchShifter32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(Self.FBuffer32^, FBuffer32^, FBufferSize * SizeOf(Single));
   end else
 if Dest is TDspGranularPitchShifter64 then
  with TDspGranularPitchShifter64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1
     do Self.FBuffer32^[Sample] := FBuffer64^[Sample];
   end
 else inherited;
end;

procedure TDspGranularPitchShifter32.Reset;
begin
 FillChar(FBuffer32^[0], FBufferSize * SizeOf(Single), 0);
end;

procedure TDspGranularPitchShifter32.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FBufferSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FBufferSize * SizeOf(Single));
 if FBufferSize > OldBufferSize
  then FillChar(FBuffer32^[OldBufferSize], (FBufferSize - OldBufferSize) * SizeOf(Single), 0);
end;

procedure TDspGranularPitchShifter32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspGranularPitchShifter32.ProcessSample32(Input: Single): Single;
var
  i, p : Integer;
  v    : Double;
begin
 inherited;

 // dry signal
 Result := 0;

 // store new data
 FBuffer32[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 0;

 for i := 0 to Stages - 1 do
  with FPitchShifterStage[i] do
   begin
    p := FBufferPos + FBufferOffset;

    // calculate absolute sample position
    while p >= FBufferSize do p := p - FBufferSize;
    while p < 0 do p := p + FBufferSize;

    v := FStageMix * (1 - abs(2 * FEnvelopePos - 1));
    FEnvelopePos := FEnvelopePos + FEnvelopeOffset;
    if FEnvelopePos >= 1 then FEnvelopePos := FEnvelopePos - 1;
    Result := Result + v * Allpass.ProcessSample32(FBuffer32[p]);

    FAllpass.FFractional := FAllpass.Fractional + FSampleOffset;
    if ((PInteger(@FAllpass.FFractional)^ and $FF800000) shr 23) > 126 then
     while FAllpass.Fractional > 1 do
      begin
       inc(FBufferOffset);
       FAllpass.Fractional := FAllpass.Fractional - 1
      end;
     while FPitchShifterStage[i].FAllpass.Fractional < 0 do
      begin
       dec(FBufferOffset);
       FAllpass.Fractional := FAllpass.Fractional + 1
      end;

    if FBufferOffset >= FBufferSize then FBufferOffset := 0 else
    if FBufferOffset < 0 then FBufferOffset := FBufferOffset + FBufferSize;
   end;
end;

{ TDspGranularPitchShifter64 }

procedure TDspGranularPitchShifter64.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TDspGranularPitchShifter32 then
  with TDspGranularPitchShifter32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1
     do Self.FBuffer64^[Sample] := FBuffer32^[Sample];
   end else
 if Dest is TDspGranularPitchShifter64 then
  with TDspGranularPitchShifter64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(Self.FBuffer64^, FBuffer64^, FBufferSize * SizeOf(Single));
   end
 else inherited;
end;

constructor TDspGranularPitchShifter64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TDspGranularPitchShifter64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TDspGranularPitchShifter64.Reset;
begin
 FillChar(FBuffer64^[0], FBufferSize * SizeOf(Double), 0);
end;

procedure TDspGranularPitchShifter64.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FBufferSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FBufferSize * SizeOf(Double));
 if FBufferSize > OldBufferSize
  then FillChar(FBuffer64^[OldBufferSize], (FBufferSize - OldBufferSize) * SizeOf(Double), 0);
end;

procedure TDspGranularPitchShifter64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspGranularPitchShifter64.ProcessSample64(Input: Double): Double;
var
  i, p : Integer;
  v    : Double;
begin
 inherited;

 // dry signal
 Result := 0;

 // store new data
 FBuffer64[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FBufferSize then FBufferPos := 0;

 for i := 0 to Stages - 1 do
  with FPitchShifterStage[i] do
   begin
    p := FBufferPos + FBufferOffset;

    // calculate absolute sample position
    while p >= FBufferSize do p := p - FBufferSize;
    while p < 0 do p := p + FBufferSize;

    v := FStageMix * (1 - abs(2 * FEnvelopePos - 1));
    FEnvelopePos := FEnvelopePos + FEnvelopeOffset;
    if FEnvelopePos >= 1 then FEnvelopePos := FEnvelopePos - 1;
    Result := Result + v * Allpass.ProcessSample64(FBuffer64[p]);

    FAllpass.FFractional := FAllpass.Fractional + FSampleOffset;
    if ((PInteger(@FAllpass.FFractional)^ and $FF800000) shr 23) > 126 then
     while FAllpass.Fractional > 1 do
      begin
       inc(FBufferOffset);
       FAllpass.Fractional := FAllpass.Fractional - 1
      end;
     while FPitchShifterStage[i].FAllpass.Fractional < 0 do
      begin
       dec(FBufferOffset);
       FAllpass.Fractional := FAllpass.Fractional + 1
      end;

    if FBufferOffset >= FBufferSize then FBufferOffset := 0 else
    if FBufferOffset < 0 then FBufferOffset := FBufferOffset + FBufferSize;
   end;
end;

initialization
  RegisterDspProcessors32([TFractionalDelayAllpass, TDspGranularPitchShifter32]);
  RegisterDspProcessors64([TFractionalDelayAllpass, TDspGranularPitchShifter64]);

end.
