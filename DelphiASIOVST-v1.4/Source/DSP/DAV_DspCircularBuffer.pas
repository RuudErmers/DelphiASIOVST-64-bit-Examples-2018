unit DAV_DspCircularBuffer;

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
  Classes, DAV_Types, DAV_Complex, DAV_Classes;

type
  TCustomCircularBuffer = class(TDspPersistent)
  private
    procedure SetBufferSize(const Value: Cardinal);
    procedure ResetBufferPositions;
  protected
    FReadBufferPos   : Cardinal;
    FWriteBufferPos  : Cardinal;
    FSamplesInBuffer : Cardinal;
    FBufferSize      : Cardinal;
    function GetFreeSampleCount: Cardinal; virtual;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; virtual; abstract;

    property BufferSize: Cardinal read FBufferSize write SetBufferSize;
  public
    constructor Create(const BufferSize: Integer = 0); virtual;
    procedure Reset; virtual;

    property SamplesInBuffer: Cardinal read FSamplesInBuffer;
    property FreeSampleCount: Cardinal read GetFreeSampleCount;
  end;

  TCustomCircularBuffer32 = class(TCustomCircularBuffer)
  protected
    FBuffer : PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Data: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Data: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;

  TCustomCircularBuffer64 = class(TCustomCircularBuffer)
  protected
    FBuffer : PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Data: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Data: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;

  TCustomCircularStereoBuffer32 = class(TCustomCircularBuffer)
  protected
    FBuffer : array [0..1] of PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Left, Right: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Left, Right: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;

  TCustomCircularStereoBuffer64 = class(TCustomCircularBuffer)
  protected
    FBuffer : array [0..1] of PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;
    procedure Reset; override;
    function ReadBuffer(const Left, Right: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Left, Right: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;

  TCustomCircularMultiBuffer = class(TCustomCircularBuffer)
  private
    FChannelCount          : Integer;
    FOnChannelCountChanged : TNotifyEvent;
    procedure SetChannelCount(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure ChannelCountChanged; virtual;
  public
    constructor Create(const BufferSize: Integer = 0); override;

    property ChannelCount: Integer read FChannelCount write SetChannelCount default 1;
    property OnChannelCountChanged: TNotifyEvent read FOnChannelCountChanged write FOnChannelCountChanged;
  end;

  TCustomCircularMultiBuffer32 = class(TCustomCircularMultiBuffer)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;

  TCustomCircularMultiBuffer64 = class(TCustomCircularMultiBuffer)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;


  TCustomCircularReserveMultiBuffer = class(TCustomCircularMultiBuffer)
  private
    FLatency: Cardinal;
    procedure SetLatency(const Value: Cardinal);
  protected
    function GetFreeSampleCount: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure LatencyChanged; virtual;
  public
    constructor Create(const BufferSize: Integer = 0); override;

    property Latency: Cardinal read FLatency write SetLatency;
  end;

  TCustomCircularReserveMultiBuffer32 = class(TCustomCircularReserveMultiBuffer)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVSingleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function ReadReserveBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteReserveBuffer(const Data: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;

  TCustomCircularReserveMultiBuffer64 = class(TCustomCircularReserveMultiBuffer)
  private
    procedure AllocateChannelData(Channel: Integer);
  protected
    FBuffer : array of PDAVDoubleFixedArray;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BufferSizeChanged; override;
    procedure ChannelCountChanged; override;
  public
    constructor Create(const BufferSize: Integer = 0); override;
    destructor Destroy; override;

    procedure Reset; override;

    function ReadBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function ReadReserveBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
    function WriteReserveBuffer(const Data: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
  end;


  TCircularBuffer32 = class(TCustomCircularBuffer32)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularBuffer64 = class(TCustomCircularBuffer64)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularStereoBuffer32 = class(TCustomCircularStereoBuffer32)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularStereoBuffer64 = class(TCustomCircularStereoBuffer64)
  published
    property BufferSize;
    property SamplesInBuffer;
  end;

  TCircularMultiBuffer32 = class(TCustomCircularMultiBuffer32)
  published
    property BufferSize;
    property ChannelCount;
    property SamplesInBuffer;
  end;

  TCircularMultiBuffer64 = class(TCustomCircularMultiBuffer64)
  published
    property BufferSize;
    property ChannelCount;
    property SamplesInBuffer;
  end;

  TCircularReserveMultiBuffer32 = class(TCustomCircularReserveMultiBuffer32)
  published
    property BufferSize;
    property ChannelCount;
    property SamplesInBuffer;
  end;

  TCircularReserveMultiBuffer64 = class(TCustomCircularReserveMultiBuffer64)
  published
    property BufferSize;
    property ChannelCount;
    property SamplesInBuffer;
  end;

implementation

uses
  SysUtils, Math;

resourcestring
  RcStrLatencyTooHigh = 'Latency must be <= Buffersize!';

{ TCustomCircularBuffer }

constructor TCustomCircularBuffer.Create(const BufferSize: Integer = 0);
begin
 inherited Create;
 FBufferSize := BufferSize;
 if BufferSize > 0
  then BufferSizeChanged;
 ResetBufferPositions;
end;

procedure TCustomCircularBuffer.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCircularBuffer then
  with TCustomCircularBuffer(Dest) do
   begin
    FReadBufferPos   := Self.FReadBufferPos;
    FWriteBufferPos  := Self.FWriteBufferPos;
    FSamplesInBuffer := Self.FSamplesInBuffer;
    FBufferSize      := Self.FBufferSize;
   end
  else inherited;
end;

function TCustomCircularBuffer.GetFreeSampleCount: Cardinal;
begin
 Result := FBufferSize - FSamplesInBuffer;
end;

procedure TCustomCircularBuffer.Reset;
begin
 ResetBufferPositions;
end;

procedure TCustomCircularBuffer.ResetBufferPositions;
begin
 FReadBufferPos := 0;
 FWriteBufferPos := 0;
 FSamplesInBuffer := 0;
end;

procedure TCustomCircularBuffer.SetBufferSize(const Value: Cardinal);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;


{ TCustomCircularBuffer32 }

constructor TCustomCircularBuffer32.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(Buffersize);
end;

destructor TCustomCircularBuffer32.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TCustomCircularBuffer32.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1
     do Self.FBuffer^[Sample] := FBuffer^[Sample];
   end
  else inherited;
end;

procedure TCustomCircularBuffer32.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularBuffer32.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Single), 0);
end;

function TCustomCircularBuffer32.ReadBuffer(const Data: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer^[FReadBufferPos], Data^[0], PartialSamples * SizeOf(Single));
   Move(FBuffer^[0], Data^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer^[FReadBufferPos], Data^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularBuffer32.WriteBuffer(
  const Data: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move(Data^[0], FBuffer^[FWriteBufferPos], PartialSamples * SizeOf(Single));
   Move(Data^[PartialSamples], FBuffer^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move(Data^[0], FBuffer^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;


{ TCustomCircularBuffer64 }

constructor TCustomCircularBuffer64.Create(const BufferSize: Integer = 0);
begin
 FBuffer := nil;
 inherited Create(BufferSize);
end;

destructor TCustomCircularBuffer64.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TCustomCircularBuffer64.AssignTo(Dest: TPersistent);
var
  Sample : Integer; 
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1
     do Self.FBuffer^[Sample] := FBuffer^[Sample];
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer^, Self.FBufferSize * SizeOf(Double));
   end
  else inherited;
end;

procedure TCustomCircularBuffer64.BufferSizeChanged;
begin
 ReallocMem(FBuffer, FBufferSize * SizeOf(Double));
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;

procedure TCustomCircularBuffer64.Reset;
begin
 inherited;
 FillChar(FBuffer^, FBufferSize * SizeOf(Double), 0);
end;

function TCustomCircularBuffer64.ReadBuffer(
  const Data: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer^[FReadBufferPos], Data^[0], PartialSamples * SizeOf(Double));
   Move(FBuffer^[0], Data^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FReadBufferPos := PartialSamples;

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer^[FReadBufferPos], Data^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularBuffer64.WriteBuffer(
  const Data: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move(Data^[0], FBuffer^[FWriteBufferPos], PartialSamples * SizeOf(Double));
   Move(Data^[PartialSamples], FBuffer^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FWriteBufferPos := PartialSamples;

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move(Data^[0], FBuffer^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;


{ TCustomCircularStereoBuffer32 }

constructor TCustomCircularStereoBuffer32.Create(const BufferSize: Integer);
begin
 FBuffer[0] := nil;
 FBuffer[1] := nil;
 inherited Create(Buffersize);
end;

destructor TCustomCircularStereoBuffer32.Destroy;
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
 inherited;
end;

procedure TCustomCircularStereoBuffer32.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Single));
    Move(FBuffer^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularStereoBuffer32 then
  with TCustomCircularStereoBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer[0]^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Single));
    Move(FBuffer[1]^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1 do
     begin
      Self.FBuffer[0]^[Sample] := FBuffer^[Sample];
      Self.FBuffer[1]^[Sample] := FBuffer^[Sample];
     end;
   end else
 if Dest is TCustomCircularStereoBuffer64 then
  with TCustomCircularStereoBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1 do
     begin
      Self.FBuffer[0]^[Sample] := FBuffer[0]^[Sample];
      Self.FBuffer[1]^[Sample] := FBuffer[1]^[Sample];
     end;
   end
  else inherited;
end;

procedure TCustomCircularStereoBuffer32.BufferSizeChanged;
begin
 ReallocMem(FBuffer[0], FBufferSize * SizeOf(Single));
 ReallocMem(FBuffer[1], FBufferSize * SizeOf(Single));
 FillChar(FBuffer[0]^, FBufferSize * SizeOf(Single), 0);
 FillChar(FBuffer[1]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularStereoBuffer32.Reset;
begin
 inherited;
 FillChar(FBuffer[0]^, FBufferSize * SizeOf(Single), 0);
 FillChar(FBuffer[1]^, FBufferSize * SizeOf(Single), 0);
end;

function TCustomCircularStereoBuffer32.ReadBuffer(const Left,
  Right: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer[0]^[FReadBufferPos],  Left^[0], PartialSamples * SizeOf(Single));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], PartialSamples * SizeOf(Single));
   Move(FBuffer[0]^[0],  Left^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   Move(FBuffer[1]^[0], Right^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer[0]^[FReadBufferPos], Left^[0], Result * SizeOf(Single));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularStereoBuffer32.WriteBuffer(const Left,
  Right: PDAVSingleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
   Move( Left^[PartialSamples], FBuffer[0]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   Move(Right^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Single));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;


{ TCustomCircularStereoBuffer64 }

constructor TCustomCircularStereoBuffer64.Create(const BufferSize: Integer);
begin
 FBuffer[0] := nil;
 FBuffer[1] := nil;
 inherited Create(BufferSize);
end;

destructor TCustomCircularStereoBuffer64.Destroy;
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
 inherited;
end;

procedure TCustomCircularStereoBuffer64.AssignTo(Dest: TPersistent);
var
  Sample : Integer;
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1 do
     begin
      Self.FBuffer[0]^[Sample] := FBuffer^[Sample];
      Self.FBuffer[1]^[Sample] := FBuffer^[Sample];
     end;
   end else
 if Dest is TCustomCircularStereoBuffer32 then
  with TCustomCircularStereoBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Sample := 0 to FBufferSize - 1 do
     begin
      Self.FBuffer[0]^[Sample] := FBuffer[0]^[Sample];
      Self.FBuffer[1]^[Sample] := FBuffer[1]^[Sample];
     end;
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Double));
    Move(FBuffer^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Double));
   end else
 if Dest is TCustomCircularStereoBuffer64 then
  with TCustomCircularStereoBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    Move(FBuffer[0]^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Double));
    Move(FBuffer[1]^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Double));
   end
  else inherited;
end;

procedure TCustomCircularStereoBuffer64.BufferSizeChanged;
begin
 ReallocMem(FBuffer[0], FBufferSize * SizeOf(Double));
 ReallocMem(FBuffer[1], FBufferSize * SizeOf(Double));
 FillChar(FBuffer[0]^, FBufferSize * SizeOf(Single), 0);
 FillChar(FBuffer[1]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularStereoBuffer64.Reset;
begin
 inherited;
 FillChar(FBuffer[0]^, FBufferSize * SizeOf(Double), 0);
 FillChar(FBuffer[1]^, FBufferSize * SizeOf(Double), 0);
end;

function TCustomCircularStereoBuffer64.ReadBuffer(const Left,
  Right: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   Move(FBuffer[0]^[FReadBufferPos],  Left^[0], PartialSamples * SizeOf(Double));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], PartialSamples * SizeOf(Double));
   Move(FBuffer[0]^[0],  Left^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   Move(FBuffer[1]^[0], Right^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FReadBufferPos := PartialSamples;

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   Move(FBuffer[0]^[FReadBufferPos],  Left^[0], Result * SizeOf(Double));
   Move(FBuffer[1]^[FReadBufferPos], Right^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularStereoBuffer64.WriteBuffer(const Left,
  Right: PDAVDoubleFixedArray; const SampleFrames: Cardinal): Cardinal;
var
  PartialSamples : Cardinal;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
   Move( Left^[PartialSamples], FBuffer[0]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   Move(Right^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
   FWriteBufferPos := PartialSamples;

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   Move( Left^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Double));
   Move(Right^[0], FBuffer[1]^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;


{ TCustomCircularMultiBuffer }

constructor TCustomCircularMultiBuffer.Create(const BufferSize: Integer);
begin
 inherited Create(Buffersize);
 FChannelCount := 1;
 ChannelCountChanged;
end;

procedure TCustomCircularMultiBuffer.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCircularMultiBuffer then
  with TCustomCircularMultiBuffer(Dest) do
   begin
    inherited;
    ChannelCount           := Self.ChannelCount;
    FOnChannelCountChanged := Self.FOnChannelCountChanged;
   end
 else inherited;
end;

procedure TCustomCircularMultiBuffer.ChannelCountChanged;
begin
 if assigned(FOnChannelCountChanged)
  then FOnChannelCountChanged(Self);
end;

procedure TCustomCircularMultiBuffer.SetChannelCount(const Value: Integer);
begin
 if FChannelCount <> Value then
  begin
   FChannelCount := Value;
   ChannelCountChanged;
  end;
end;


{ TCustomCircularMultiBuffer32 }

constructor TCustomCircularMultiBuffer32.Create(const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularMultiBuffer32.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularMultiBuffer32.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularMultiBuffer32.AssignTo(Dest: TPersistent);
var
  Sample  : Integer;
  Channel : Integer;
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Single));
    Move(FBuffer^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularStereoBuffer32 then
  with TCustomCircularStereoBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel mod 2]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularMultiBuffer32 then
  with TCustomCircularMultiBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer^[Sample];
   end else
 if Dest is TCustomCircularStereoBuffer64 then
  with TCustomCircularStereoBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel mod 2]^[Sample];
   end else
 if Dest is TCustomCircularMultiBuffer64 then
  with TCustomCircularMultiBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel]^[Sample];
   end
  else inherited;
end;

procedure TCustomCircularMultiBuffer32.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularMultiBuffer32.AllocateChannelData(Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Single));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularMultiBuffer32.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
 inherited;
end;

function TCustomCircularMultiBuffer32.ReadBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 // Assert(Length(Data) >= ChannelCount); // replaced by Min(Length(Data), ChannelCount)

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to Min(Length(Data), ChannelCount) - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Single));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to Min(Length(Data), ChannelCount) - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularMultiBuffer32.WriteBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 // Assert(Length(Data) >= ChannelCount); // replaced by Min(Length(Data), ChannelCount)

 if SampleFrames <= FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to Min(Length(Data), ChannelCount) - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[Channel]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
     Move(Data[Channel]^[PartialSamples], FBuffer[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to Min(Length(Data), ChannelCount) - 1
    do Move(Data[Channel]^[0], FBuffer[Channel]^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;


{ TCustomCircularMultiBuffer64 }

constructor TCustomCircularMultiBuffer64.Create(const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularMultiBuffer64.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularMultiBuffer64.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularMultiBuffer64.AssignTo(Dest: TPersistent);
var
  Sample  : Integer;
  Channel : Integer;
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer^[Sample];
   end else
 if Dest is TCustomCircularStereoBuffer32 then
  with TCustomCircularStereoBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel mod 2]^[Sample];
   end else
 if Dest is TCustomCircularMultiBuffer32 then
  with TCustomCircularMultiBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel]^[Sample];
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Double));
    Move(FBuffer^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Double));
   end else
 if Dest is TCustomCircularStereoBuffer64 then
  with TCustomCircularStereoBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel mod 2]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Double));
   end else
 if Dest is TCustomCircularMultiBuffer64 then
  with TCustomCircularMultiBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Double));
   end
  else inherited;
end;

procedure TCustomCircularMultiBuffer64.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularMultiBuffer64.AllocateChannelData(Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Double));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
end;

procedure TCustomCircularMultiBuffer64.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
 inherited;
end;

function TCustomCircularMultiBuffer64.ReadBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], PartialSamples * SizeOf(Double));
     Move(FBuffer[Channel]^[0], Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularMultiBuffer64.WriteBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[Channel]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
     Move(Data[Channel]^[PartialSamples], FBuffer[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(Data[Channel]^[0], FBuffer[Channel]^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;


{ TCustomCircularReserveMultiBuffer }

procedure TCustomCircularReserveMultiBuffer.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCircularReserveMultiBuffer then
  with TCustomCircularReserveMultiBuffer(Dest) do
   begin
    inherited;
    FLatency := Self.FLatency;
   end
 else inherited;
end;

procedure TCustomCircularReserveMultiBuffer.SetLatency(const Value: Cardinal);
begin
 if Value >= Cardinal(FBufferSize)
  then raise Exception.Create(RcStrLatencyTooHigh);
 
 if FLatency <> Value then
  begin
   FLatency := Value;
   LatencyChanged;
  end;
end;

function TCustomCircularReserveMultiBuffer.GetFreeSampleCount: Cardinal;
begin
 Result := inherited GetFreeSampleCount;
 Assert(Result >= FLatency);
 Result := Result - FLatency;
end;

procedure TCustomCircularReserveMultiBuffer.LatencyChanged;
begin
 if FBufferSize - SamplesInBuffer < FLatency then
  begin
   if FReadBufferPos < FLatency
    then FWriteBufferPos := FWriteBufferPos + FBufferSize - FLatency
    else FWriteBufferPos := FReadBufferPos - FLatency;
  end; 
end;

{ TCustomCircularReserveMultiBuffer32 }

constructor TCustomCircularReserveMultiBuffer32.Create(
  const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularReserveMultiBuffer32.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer32.AssignTo(Dest: TPersistent);
var
  Sample  : Integer;
  Channel : Integer;
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Single));
    Move(FBuffer^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularStereoBuffer32 then
  with TCustomCircularStereoBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel mod 2]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Single));
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer^[Sample];
   end else
 if Dest is TCustomCircularStereoBuffer64 then
  with TCustomCircularStereoBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel mod 2]^[Sample];
   end
  else inherited;
end;

constructor TCustomCircularReserveMultiBuffer.Create(
  const BufferSize: Integer);
begin
 FLatency := 0;
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer32.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularReserveMultiBuffer32.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer32.AllocateChannelData(
  Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Single));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
end;

procedure TCustomCircularReserveMultiBuffer32.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Single), 0);
 inherited;
end;

function TCustomCircularReserveMultiBuffer32.ReadBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Single));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularReserveMultiBuffer32.WriteBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Single));
     Move(Data[Channel]^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Single));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;

function TCustomCircularReserveMultiBuffer32.ReadReserveBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
(*
var
  Channel        : Integer;
  PartialSamples : Cardinal;
*)
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < FLatency
  then Result := SampleFrames
  else Result := FLatency;

(*
 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Single));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Single));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Single));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 FSamplesInBuffer := FSamplesInBuffer - Result;
 Assert(FSamplesInBuffer >= 0);
*)
end;

function TCustomCircularReserveMultiBuffer32.WriteReserveBuffer(
  const Data: TDAVArrayOfSingleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
begin

end;


{ TCustomCircularReserveMultiBuffer64 }

constructor TCustomCircularReserveMultiBuffer64.Create(
  const BufferSize: Integer);
begin
 inherited Create(Buffersize);
end;

destructor TCustomCircularReserveMultiBuffer64.Destroy;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do Dispose(FBuffer[Channel]);
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer64.AssignTo(Dest: TPersistent);
var
  Sample  : Integer;
  Channel : Integer;
begin
 if Dest is TCustomCircularBuffer32 then
  with TCustomCircularBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer^[Sample];
   end else
 if Dest is TCustomCircularStereoBuffer32 then
  with TCustomCircularStereoBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel mod 2]^[Sample];
   end else
 if Dest is TCustomCircularMultiBuffer32 then
  with TCustomCircularMultiBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel]^[Sample];
   end else
 if Dest is TCustomCircularReserveMultiBuffer32 then
  with TCustomCircularReserveMultiBuffer32(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1 do
     for Sample := 0 to FBufferSize - 1
      do Self.FBuffer[Channel]^[Sample] := FBuffer[Channel]^[Sample];
   end else
 if Dest is TCustomCircularBuffer64 then
  with TCustomCircularBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer^, Self.FBuffer[0]^, Self.FBufferSize * SizeOf(Double));
    Move(FBuffer^, Self.FBuffer[1]^, Self.FBufferSize * SizeOf(Double));
   end else
 if Dest is TCustomCircularStereoBuffer64 then
  with TCustomCircularStereoBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel mod 2]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Double));
   end else
 if Dest is TCustomCircularMultiBuffer64 then
  with TCustomCircularMultiBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Double));
   end else
 if Dest is TCustomCircularReserveMultiBuffer64 then
  with TCustomCircularReserveMultiBuffer64(Dest) do
   begin
    inherited;
    Assert(FBufferSize = Self.FBufferSize);
    for Channel := 0 to FChannelCount - 1
     do Move(FBuffer[Channel]^, Self.FBuffer[Channel]^, Self.FBufferSize * SizeOf(Double));
   end
  else inherited;
end;

procedure TCustomCircularReserveMultiBuffer64.BufferSizeChanged;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do AllocateChannelData(Channel);
end;

procedure TCustomCircularReserveMultiBuffer64.ChannelCountChanged;
var
  Channel         : Integer;
  OldChannelCount : Integer;
begin
 if ChannelCount < Length(FBuffer) then
  begin
   for Channel := Length(FBuffer) - 1 downto ChannelCount
    do Dispose(FBuffer[Channel]);
   SetLength(FBuffer, ChannelCount);
  end else
 if ChannelCount > Length(FBuffer) then
  begin
   OldChannelCount := Length(FBuffer);
   SetLength(FBuffer, ChannelCount);
   for Channel := OldChannelCount to ChannelCount - 1
    do AllocateChannelData(Channel);
  end;
 inherited;
end;

procedure TCustomCircularReserveMultiBuffer64.AllocateChannelData(
  Channel: Integer);
begin
 ReallocMem(FBuffer[Channel], FBufferSize * SizeOf(Double));
 FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
end;

procedure TCustomCircularReserveMultiBuffer64.Reset;
var
  Channel : Integer;
begin
 for Channel := 0 to FChannelCount - 1
  do FillChar(FBuffer[Channel]^, FBufferSize * SizeOf(Double), 0);
 inherited;
end;

function TCustomCircularReserveMultiBuffer64.ReadBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 Assert(Length(Data) >= ChannelCount);

 if SampleFrames < SamplesInBuffer
  then Result := SampleFrames
  else Result := SamplesInBuffer;

 if FReadBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FReadBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(FBuffer[Channel]^[FReadBufferPos],  Data[Channel]^[0], PartialSamples * SizeOf(Double));
     Move(FBuffer[Channel]^[0],  Data[Channel]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;

   FReadBufferPos := (Result - PartialSamples);

   if FReadBufferPos >= FBufferSize
    then FReadBufferPos := FReadBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(FBuffer[Channel]^[FReadBufferPos], Data[Channel]^[0], Result * SizeOf(Double));
   FReadBufferPos := FReadBufferPos + Result;
  end;
 Assert(FSamplesInBuffer >= Result);
 FSamplesInBuffer := FSamplesInBuffer - Result;
end;

function TCustomCircularReserveMultiBuffer64.WriteBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
var
  Channel        : Integer;
  PartialSamples : Cardinal;
begin
 if SampleFrames < FreeSampleCount
  then Result := SampleFrames
  else Result := FreeSampleCount;

 if FWriteBufferPos + Result >= FBufferSize then
  begin
   PartialSamples := FBufferSize - FWriteBufferPos;
   for Channel := 0 to ChannelCount - 1 do
    begin
     Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], PartialSamples * SizeOf(Double));
     Move(Data[Channel]^[PartialSamples], FBuffer[1]^[PartialSamples], (Result - PartialSamples) * SizeOf(Double));
    end;
   FWriteBufferPos := (Result - PartialSamples);

   if FWriteBufferPos >= FBufferSize
    then FWriteBufferPos := FWriteBufferPos - FBufferSize;
  end
 else
  begin
   for Channel := 0 to ChannelCount - 1
    do Move(Data[Channel]^[0], FBuffer[0]^[FWriteBufferPos], Result * SizeOf(Double));
   FWriteBufferPos := FWriteBufferPos + Result;
  end;

 // keep track of samples in buffer
 FSamplesInBuffer := FSamplesInBuffer + Result;
 Assert(FSamplesInBuffer <= FBufferSize);
end;

function TCustomCircularReserveMultiBuffer64.ReadReserveBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
begin

end;

function TCustomCircularReserveMultiBuffer64.WriteReserveBuffer(
  const Data: TDAVArrayOfDoubleFixedArray;
  const SampleFrames: Cardinal): Cardinal;
begin

end;

end.
