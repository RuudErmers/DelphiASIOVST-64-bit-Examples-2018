unit DAV_DspPhaser;

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
  DAV_Types, DAV_Classes, DAV_DspLFO;

type
  TMasterAllPass = class(TDspPersistent, IDspProcessor32)
  private
    FCoefficient : Single;
    FDelay       : Single;
    FStages      : Integer;
    FY           : PDAVSingleFixedArray;
    FSampleRate  : Single;
    procedure SetDelay(const Value: Single);
    procedure SetStages(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;
    property Delay: Single read FDelay write SetDelay;
    property Stages: Integer read FStages write SetStages;
    property SampleRate: Single read FSampleRate write FSampleRate;
  end;

  TCustomPhaser = class(TDspSampleRatePersistent, IDspProcessor32)
  private
    FZM1           : Single;
    FDepth         : Single;
    FLFO           : TLFOSineLike;
    FLFOPhase      : Single;
    FFeedBack      : Single;
    FRate          : Single;
    FMinimum       : Single;
    FMaximum       : Single;
    FMin           : Single;
    FMax           : Single;
    FInvSampleRate : Single;
    FMasterAllPass : TMasterAllPass;
    procedure SetMinimum(const Value: Single);
    procedure SetMaximum(const Value: Single);
    procedure SetRate(const Value: Single);
    procedure SetStages(const Value: Integer);
    function GetStages: Integer;
  protected
    procedure RateChanged; virtual;
    procedure SampleRateChanged; override;
    procedure CalculateFilters; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single;

    property Depth: Single read FDepth write FDepth; //0..1
    property Feedback: Single read FFeedBack write FFeedBack; // 0..<1
    property Minimum: Single read FMin write SetMinimum;
    property Maximum: Single read FMax write SetMaximum;
    property Stages: Integer read GetStages write SetStages;
    property Rate: Single read FRate write SetRate; // Hz
  end;

  TPhaser = class(TCustomPhaser)
  published
    property Depth; //0..1
    property Feedback; // 0..<1
    property Minimum;
    property Maximum;
    property Rate; // Hz
    property SampleRate;
    property Stages;
  end;

implementation

uses
  SysUtils, DAV_Common;

resourcestring
  RCStrStagesLargeZero = 'Stages must be larger than 0!';
  RCStrFrequencyPositive = 'Frequency must be positive!';
  RCStrFrequencyNyquist = 'Frequency must be below the nyquist frequency';

{ TMasterAllpass }

constructor TMasterAllpass.Create;
begin
  inherited;
  FY           := nil;
  FCoefficient := 0;
  Stages       := 1;
end;

destructor TMasterAllpass.Destroy;
begin
  inherited;
end;

{$DEFINE PUREPASCAL}

procedure TMasterAllPass.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TMasterAllpass.ProcessSample32(Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  a    : array[0..2] of Single;
  i, p : Integer;
begin
  a[0] := Input * FCoefficient + FY[0];
  a[2] := a[0] * FCoefficient;
  FY[0] := a[2] - Input;

  p := 1;
  for i := 1 to FStages - 1 do
   begin
    a[p]  := a[2] - FY[i];
    a[2]  := a[p] * FCoefficient;
    p     := 1 - p;
    FY[i] := a[p] - a[2];
   end;
  Result := a[1 - p];
end;
{$ELSE}
asm
    FLD      Self.FCoefficient.Single
    MOV      ECX, Self.FStages.Integer
    ADD      EAX, FY.Single
    FLD      Input.Single
    FMUL     ST(0), ST(1)
    FADD     [EAX].Single
    FLD      ST(0)
    FMUL     ST(0),ST(2)
    FLD      ST(0)
    FSUB     Input.Single
    FSTP     [EAX].Single
    ADD      EAX, 4
    FSUB     [EAX].Single
    FLD      ST(0)
    FMUL     ST(0),ST(3)
    FSUB     ST(2),ST(0)
    FXCH     ST(2)
    FSTP     [EAX].Single
    ADD      EAX, 4
    FLD      [EAX].Single
    FSUBP    ST(2),ST(0)
    FLD      ST(1)
    FMUL     ST(0),ST(3)
    FSUB     ST(1),ST(0)
    FXCH

@StartLoop:
    FSTP     [EAX].Single
    ADD      EAX, 4
    FSUB     [EAX].Single
    FLD      ST(0)
    FMUL     ST(0), ST(3)
    FSUB     ST(2), ST(0)
    FXCH     ST(2)
    FSTP     [EAX].Single
    ADD      EAX, 4
    FLD      [EAX].Single
    FSUBP    ST(2), ST(0)
    FLD      ST(1)
    FMUL     ST(0), ST(3)
    FSUB     ST(1), ST(0)
    LOOP     @StartLoop

    FXCH
    FSTP     [EAX].Single
    ADD      EAX, 4
    FADD     [EAX].Single
    FXCH
    FLD      ST(1)
    FMULP    ST(3), ST(0)
    FSUBP    ST(2), ST(0)
    FXCH
    FSTP     [EAX].Single
end;
{$ENDIF}

procedure TMasterAllpass.SetDelay(const Value: Single);
begin
 if FDelay <> Value then
  begin
   FDelay := Value;
   FCoefficient := (1 - Value) / (1 + Value);
  end;
end;

procedure TMasterAllPass.SetStages(const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create(RCStrStagesLargeZero);
 if FStages <> Value then
  begin
   if Value > FStages then
    begin
     ReallocMem(FY, Value * SizeOf(Single));
     FillChar(FY^[FStages], (Value - FStages) * SizeOf(Single), 0);
     FStages := Value;
    end
   else
    begin
     FStages := Value;
     ReallocMem(FY, FStages * SizeOf(Single));
    end;
  end;
end;


{ TCustomPhaser }

constructor TCustomPhaser.Create;
begin
  inherited;
  FLFO           := TLFOSineLike.Create;
  FMasterAllPass := TMasterAllPass.Create;
  FMinimum       := 440;
  FMaximum       := 1600;
  FFeedBack      := 0.7;
  FLFOPhase      := 0;
  FDepth         := 1;
  FZM1           := 0;
  Rate           := 1;
  Stages         := 5;
  SampleRateChanged;
  CalculateFilters;
end;
(*
 Parameter[0] := 30;
 Parameter[1] := 30;
 Parameter[2] := 300;
 Parameter[3] := 1000;
 Parameter[4] := 0.1;
 Parameter[5] := 5;
 *)

destructor TCustomPhaser.Destroy;
begin
  FreeAndNil(FLFO);
  FreeAndNil(FMasterAllPass);
  inherited;
end;

procedure TCustomPhaser.RateChanged;
begin
 FLFO.Speed := 2 * SampleRate / FRate;
end;

procedure TCustomPhaser.SetRate(const Value: Single);
begin
 if Value <> FRate then
  begin
   FRate := Value;
   RateChanged;
  end;
end;

procedure TCustomPhaser.CalculateFilters;
begin
 FMin := 2 * FMinimum * FInvSampleRate;
 FMax := 2 * FMaximum * FInvSampleRate;
end;

procedure TCustomPhaser.SetMinimum(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create(RCStrFrequencyPositive);
 if Value > Samplerate * 0.5
  then raise Exception.Create(RCStrFrequencyNyquist);
 if FMinimum <> Value then
  begin
   FMinimum := Value;
   CalculateFilters;
  end;
end;

procedure TCustomPhaser.SetMaximum(const Value: Single);
begin
 if Value < 0
  then raise Exception.Create(RCStrFrequencyPositive);
 if Value > Samplerate * 0.5
  then raise Exception.Create(RCStrFrequencyNyquist);
 if FMaximum <> Value then
  begin
   FMaximum := Value;
   CalculateFilters;
  end;
end;

procedure TCustomPhaser.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TCustomPhaser.ProcessSample32(Input: Single): Single;
begin
  FMasterAllPass.Delay := FMin + (FMax - FMin) * FLFO.Value;
  FZM1 := FMasterAllPass.ProcessSample32(CDenorm32 + Input + FZM1 * FFeedBack);
  Result := Input + FZM1 * FDepth;
end;

procedure TCustomPhaser.SampleRateChanged;
begin
 FMasterAllPass.SampleRate := SampleRate;
 FInvSampleRate := 1 / SampleRate;
 CalculateFilters;
 RateChanged;
end;

procedure TCustomPhaser.SetStages(const Value: Integer);
begin
 FMasterAllPass.Stages := Value;
end;

function TCustomPhaser.GetStages: Integer;
begin
 Result := FMasterAllPass.FStages;
end;

initialization
  RegisterDspProcessors32([TMasterAllPass, TPhaser]);

end.
