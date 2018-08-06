unit DAV_DspVibrato;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspLFO;

type
  TCustomDspVibrato = class(TDspSampleRatePersistent)
  private
    FSpeed        : Double;
    FDepth        : Double;
    FRealBufSize  : Integer;
    FBufferSize   : Integer;
    FBufferPos    : Integer;
    FLFO          : TLFOSine;
    procedure SetDepth(const Value: Double);
    procedure SetSpeed(const Value: Double);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DepthChanged; virtual;
    procedure SampleRateChanged; override;
    procedure SpeedChanged; virtual;
    procedure UpdateBuffer; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset; virtual;
  published
    property Speed: Double read FSpeed write SetSpeed;
    property Depth: Double read FDepth write SetDepth;
  end;

  TCustomDspVibrato32 = class(TCustomDspVibrato, IDspProcessor32)
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

  TDspVibrato32 = class(TCustomDspVibrato32)
  published
    property Depth;
    property SampleRate;
    property Speed;
  end;

  TCustomDspVibrato64 = class(TCustomDspVibrato, IDspProcessor64)
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

  TDspVibrato64 = class(TCustomDspVibrato64)
  published
    property Depth;
    property SampleRate;
    property Speed;
  end;

implementation

uses
  SysUtils, DAV_DspInterpolation;

{ TCustomDspVibrato }

constructor TCustomDspVibrato.Create;
begin
 inherited;
 FSpeed     := 2;
 FDepth     := 0.5;
 FBufferPos := 0;
 FLFO       := TLFOSine.Create;
end;

destructor TCustomDspVibrato.Destroy;
begin
 FreeAndNil(FLFO);
 inherited;
end;

procedure TCustomDspVibrato.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspVibrato then
  with TCustomDspVibrato(Dest) do
   begin
    inherited;
    FSpeed       := Self.FSpeed;
    FDepth       := Self.FDepth;
    FRealBufSize := Self.FRealBufSize;
    FBufferSize  := Self.FBufferSize;
    FBufferPos   := Self.FBufferPos;
    FLFO.Assign(Self.FLFO);
   end
 else inherited;
end;

procedure TCustomDspVibrato.Reset;
begin
 FLFO.Reset;
end;

procedure TCustomDspVibrato.UpdateBuffer;
begin
 // determine buffer size
 FBufferSize  := round(sqr(Depth) * 0.25 * SampleRate); // quarter second
 FRealBufSize := FBufferSize + 8;

 // check and reset buffer position
 if FBufferPos >= FRealBufSize
  then FBufferPos := 4;
end;

procedure TCustomDspVibrato.SpeedChanged;
begin
 FLFO.Frequency := Speed;
 UpdateBuffer;
 Changed;
end;

procedure TCustomDspVibrato.DepthChanged;
begin
 UpdateBuffer;
 Changed;
end;

procedure TCustomDspVibrato.SampleRateChanged;
begin
 FLFO.SampleRate := SampleRate;
 UpdateBuffer;
 Changed;
end;

procedure TCustomDspVibrato.SetDepth(const Value: Double);
begin
 if FDepth <> Value then
  begin
   FDepth := Value;
   DepthChanged;
  end;
end;

procedure TCustomDspVibrato.SetSpeed(const Value: Double);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   SpeedChanged;
  end;
end;

{ TCustomDspVibrato32 }

constructor TCustomDspVibrato32.Create;
begin
 inherited;
 FBuffer32 := nil;
end;

destructor TCustomDspVibrato32.Destroy;
begin
 Dispose(FBuffer32);
 inherited;
end;

procedure TCustomDspVibrato32.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspVibrato32 then
  with TCustomDspVibrato32(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    Move(Self.FBuffer32^, FBuffer32^, FRealBufSize * SizeOf(Single));
   end
 else inherited;
end;

procedure TCustomDspVibrato32.Reset;
begin
 FillChar(FBuffer32^[0], FRealBufSize * SizeOf(Single), 0);
end;

procedure TCustomDspVibrato32.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer32, FRealBufSize * SizeOf(Single));
end;

procedure TCustomDspVibrato32.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

function TCustomDspVibrato32.ProcessSample32(Input: Single): Single;
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
 result := Hermite32_asm(d, @FBuffer32[p - 4]);

 // store new data
 FBuffer32[FBufferPos] := Input;
 inc(FBufferPos);
 if FBufferPos >= FRealBufSize then
  begin
   Move(FBuffer32[FRealBufSize - 4], FBuffer32[0], 4 * SizeOf(Single));
   FBufferPos := 4;
  end;
end;

{ TCustomDspVibrato64 }

constructor TCustomDspVibrato64.Create;
begin
 inherited;
 FBuffer64 := nil;
end;

destructor TCustomDspVibrato64.Destroy;
begin
 Dispose(FBuffer64);
 inherited;
end;

procedure TCustomDspVibrato64.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDspVibrato64 then
  with TCustomDspVibrato64(Dest) do
   begin
    inherited;
    assert(FRealBufSize = Self.FRealBufSize);
    Move(Self.FBuffer64^, FBuffer64^, FRealBufSize * SizeOf(Double));
   end
 else inherited;
end;

procedure TCustomDspVibrato64.Reset;
begin
 FillChar(FBuffer64^[0], FRealBufSize * SizeOf(Double), 0);
end;

procedure TCustomDspVibrato64.UpdateBuffer;
begin
 inherited;

 // allocate memory
 ReallocMem(FBuffer64, FRealBufSize * SizeOf(Double));
end;

procedure TCustomDspVibrato64.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomDspVibrato64.ProcessSample64(Input: Double): Double;
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
 result := Hermite64_asm(d, @FBuffer64[p - 4]);

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
  RegisterDspProcessor32(TDspVibrato32);
  RegisterDspProcessor64(TDspVibrato64);

end.
