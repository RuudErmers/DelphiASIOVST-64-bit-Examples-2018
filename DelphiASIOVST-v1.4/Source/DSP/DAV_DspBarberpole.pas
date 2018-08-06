unit DAV_DspBarberpole;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspLFO;

type
  TBarberpoleDirection = (sdUp, sdDown, sdUpInv, sdDownInv);
  TCustomDspBarberpole = class(TDspSampleRatePersistent)
  private
    FSampleRateInv : Double;
    FSpeed         : Double;
    FDepth         : Double;
    FMix           : Double;
    FOffset        : Double;
    FStageMix      : Double;
    FRealBufSize   : Integer;
    FBufferSize    : Integer;
    FBufferPos     : Integer;
    FDirection     : TBarberpoleDirection;
    FStagePosition : PDAVDoubleFixedArray;
    FStages        : Byte;
    procedure SetDepth(const Value: Double);
    procedure SetMix(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetStages(const Value: Byte);
    procedure SetDirection(const Value: TBarberpoleDirection);
    procedure CalculateReciprocalSampleRate;
  protected
    procedure CalculateStageMix; virtual;
    procedure DirectionChanged; virtual;
    procedure DepthChanged; virtual;
    procedure MixChanged; virtual;
    procedure SampleRateChanged; override;
    procedure SpeedChanged; virtual;
    procedure StagesChanged; virtual;
    procedure UpdateBuffer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure Reset; virtual; abstract;
  published
    property Depth: Double read FDepth write SetDepth;
    property Direction: TBarberpoleDirection read FDirection write SetDirection;
    property Speed: Double read FSpeed write SetSpeed;
    property Stages: Byte read FStages write SetStages default 0;
    property Mix: Double read FMix write SetMix;
  end;

  TDspBarberpole32 = class(TCustomDspBarberpole, IDspProcessor32)
  private
    FBuffer32 : PDAVSingleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer); virtual;
    function ProcessSample32(Input: Single): Single; virtual;
    procedure Reset; override;
  published
    property Depth;
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

  TDspBarberpole64 = class(TCustomDspBarberpole, IDspProcessor64)
  private
    FBuffer64 : PDAVDoubleFixedArray;
  protected
    procedure UpdateBuffer; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample64(Input: Double): Double; virtual;

    procedure Reset; override;
  published
    property Depth;
    property Mix;
    property SampleRate;
    property Speed;
    property Stages;
  end;

implementation

uses
  SysUtils, Math, DAV_DspInterpolation;

{ TCustomDspBarberpole }

constructor TCustomDspBarberpole.Create;
begin
 inherited;
 CalculateReciprocalSampleRate;
 
 FSpeed         := 2;
 FDepth         := 0.5;
 FMix           := 0.5;
 FBufferPos     := 4;
 FStagePosition := nil;
 FDirection     := sdUp;
 Stages         := 2;
end;

procedure TCustomDspBarberpole.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspBarberpole then
  with TCustomDspBarberpole(Dest) do
   begin
    inherited;
    Speed        := Self.Speed;
    Depth        := Self.Depth;
    Mix          := Self.Mix;
    Stages       := Self.Stages;
   end
 else inherited;
end;

procedure TCustomDspBarberpole.CalculateStageMix;
begin
 FStageMix := sqrt(2) * 0.5 * (FMix + Power(FMix, Stages));
end;

procedure TCustomDspBarberpole.StagesChanged;
var
  i           : Integer;
  BaseStage   : Double;
  StageOffset : Double;
begin
 BaseStage := FStagePosition^[0];
 StageOffset := 1 / FStages;
 for i := 1 to FStages - 1 do
  begin
   FStagePosition^[i] := BaseStage + i * StageOffset;
   if FStagePosition^[i] >= 1
    then FStagePosition^[i] := FStagePosition^[i] - 1;
  end;
 SpeedChanged;
 CalculateStageMix;
 Changed; 
end;

procedure TCustomDspBarberpole.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := Round(sqr(Depth) * 0.25 * SampleRate); // quarter second
 FRealBufSize := FBufferSize + 4;

 // check and reset buffer position
 if FBufferPos >= FRealBufSize
  then FBufferPos := 4;
end;

procedure TCustomDspBarberpole.SpeedChanged;
begin
 FOffset := FSampleRateInv * Speed;
 Changed; 
end;

procedure TCustomDspBarberpole.DepthChanged;
begin
 UpdateBuffer;
 Changed; 
end;

procedure TCustomDspBarberpole.MixChanged;
begin
 CalculateStageMix;
 Changed; 
end;

procedure TCustomDspBarberpole.SampleRateChanged;
begin
 inherited;
 CalculateReciprocalSampleRate;
 UpdateBuffer;
end;

procedure TCustomDspBarberpole.CalculateReciprocalSampleRate;
begin
 FSampleRateInv := 1 / SampleRate;
end;

procedure TCustomDspBarberpole.DirectionChanged;
begin
 Changed; 
end;

procedure TCustomDspBarberpole.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomDspBarberpole.SetDirection(const Value: TBarberpoleDirection);
begin
 if FDirection <> Value then
  begin
   FDirection := Value;
   DirectionChanged;
  end;
end;

procedure TCustomDspBarberpole.SetMix(const Value: Double);
begin
 if FMix <> Value then
  begin
   FMix := Value;
   MixChanged;
  end;
end;

procedure TCustomDspBarberpole.SetSpeed(const Value: Double);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedChanged;
  end;
end;

procedure TCustomDspBarberpole.SetStages(const Value: Byte);
begin
 if FStages <> Value then
  begin
   if FStages > Value then
    begin
     FStages := Value;
     ReallocMem(FStagePosition, Value * SizeOf(Double));
    end
   else
    begin
     ReallocMem(FStagePosition, Value * SizeOf(Double));
     FillChar(FStagePosition^[FStages], (Value - FStages) * SizeOf(Double), 0);
     FStages := Value;
    end;
   StagesChanged;
  end;
end;

{ TDspBarberpole32 }

constructor TDspBarberpole32.Create;
begin
 inherited;
 FBuffer32 := nil;
end;

destructor TDspBarberpole32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TDspBarberpole32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TDspBarberpole32.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FRealBufSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FRealBufSize * SizeOf(Single));
 if FRealBufSize > OldBufferSize
  then FillChar(FBuffer32^[OldBufferSize], (FRealBufSize - OldBufferSize) * SizeOf(Single), 0);
end;

procedure TDspBarberpole32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TDspBarberpole32.ProcessSample32(Input: Single): Single;
var
  i, p : Integer;
  d, m : Double;
  v    : Double;
begin
 inherited;

 // dry signal
 Result := (1 - FMix) * Input;

 m := FStageMix;
 case FDirection of
  sdUp :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d - CHalf32);
     d := d - p;

     p := FBufferPos + p - 2;
     if p >= FBufferSize then p := p - FBufferSize;

     v := sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdDown :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d + CHalf32);
     d := p - d;

     p := FBufferPos - p - 2;

     if p < 0 then p := p + FBufferSize else
     if p >= FBufferSize then p := p - FBufferSize;

     v := sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdUpInv :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d - CHalf32);
     d := d - p;

     p := FBufferPos + p - 2;
     if p >= FBufferSize then p := p - FBufferSize;

     v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdDownInv :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d + CHalf32);
     d := p - d;

     p := FBufferPos - p - 2;

     if p < 0 then p := p + FBufferSize;

     v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite32_asm(d, @FBuffer32[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
 end;

 // store new data
 FBuffer32[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferPos := 4;
  end;
end;

{ TDspBarberpole64 }

constructor TDspBarberpole64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TDspBarberpole64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TDspBarberpole64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TDspBarberpole64.UpdateBuffer;
var
  OldBufferSize : Integer;
begin
 OldBufferSize := FRealBufSize;
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FRealBufSize * SizeOf(Double));
 if FRealBufSize > OldBufferSize
  then FillChar(FBuffer64^[OldBufferSize], (FRealBufSize - OldBufferSize) * SizeOf(Double), 0);
end;

procedure TDspBarberpole64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspBarberpole64.ProcessSample64(Input: Double): Double;
var
  i, p : Integer;
  d, m : Double;
  v    : Double;
begin
 inherited;

 // dry signal
 Result := (1 - FMix) * Input;

 m := FStageMix;
 case FDirection of
  sdUp :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d - CHalf64);
     d := d - p;

     p := FBufferPos + p - 2;
     if p >= FBufferSize then p := p - FBufferSize;

     v := sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite64_asm(d, @FBuffer64[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdDown :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d + CHalf64);
     d := p - d;

     p := FBufferPos - p - 2;

     if p < 0 then p := p + FBufferSize else
     if p >= FBufferSize then p := p - FBufferSize;

     v := sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite64_asm(d, @FBuffer64[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdUpInv :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d - CHalf64);
     d := d - p;

     p := FBufferPos + p - 2;
     if p >= FBufferSize then p := p - FBufferSize;

     v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite64_asm(d, @FBuffer64[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
  sdDownInv :
   for i := 0 to Stages - 1 do
    begin
     d := FBufferSize * FStagePosition^[i];

     // calculate absolute sample position
     p := Round(d + CHalf64);
     d := p - d;

     p := FBufferPos - p - 2;

     if p < 0 then p := p + FBufferSize;

     v := (2 * (i mod 2) - 1) * sqr((1 - abs(1 - 2 * FStagePosition^[i])));
     result := result + v * m * Hermite64_asm(d, @FBuffer64[p]);

     FStagePosition^[i] := FStagePosition^[i] + FOffset;
     if FStagePosition^[i] >= 1
      then FStagePosition^[i] := 0;
    end;
 end;

 // store new data
 FBuffer64[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer64[FRealBufSize - 4], FBuffer64[0], 4 * SizeOf(Double));
   FBufferPos := 4;
  end;
end;

initialization
  RegisterDspProcessor32(TDspBarberpole32);
  RegisterDspProcessor64(TDspBarberpole64);

end.
