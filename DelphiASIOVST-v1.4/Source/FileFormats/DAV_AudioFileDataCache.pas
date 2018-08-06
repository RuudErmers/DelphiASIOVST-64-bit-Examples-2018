unit DAV_AudioFileDataCache;

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
//                                                                            //
//  Start:                                                                    //
//                                                        <---Latency--->     //
//    +------------------------------------------------------------------+    //
//    |                                                                  |    //
//    +------------------------------------------------------------------+    //
//    ^                                                  ^                    //
//  Position                                     Position-Latency             //
//                                                                            //
//                                                                            //
//  Buffer Filled by thread:                                                  //
//                                                        <---Latency--->     //
//    +------------------------------------------------------------------+    //
//    |OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO                |    //
//    +------------------------------------------------------------------+    //
//    ^                                                  ^                    //
//  Position                                     Position-Latency             //
//                                                                            //
//                                                                            //
//  Buffer Filled by ASIO:                                                    //
//     ------->                                                  <-------     //
//    +------------------------------------------------------------------+    //
//    |xxxxxxxxOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO        IIIIIIII|    //
//    +------------------------------------------------------------------+    //
//             ^                                                 ^            //
//          Position                                     Position-Latency     //
//                                                                            //
//                                                                            //
//  Buffer Filled by ASIO:                                                    //
//     <---Latency--->                                                        //
//    +------------------------------------------------------------------+    //
//    |xxxxxxxxxxxxxxxOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOIIIIIIIIIIIIIIII|    //
//    +------------------------------------------------------------------+    //
//     ^             ^                                                        //
//  Pos-Lat       Position                                                    //
//                                                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, SyncObjs, DAV_Types, DAV_Classes, DAV_AudioData,
  DAV_AudioFile, DAV_ChannelDataCoder;

type
  TAudioFileCache = class;
  TDriver = class(TThread)
  private
    FAudioFileCache : TAudioFileCache;
  protected
    procedure Execute; override;
  public
    constructor Create(AudioFileCache: TAudioFileCache);
  end;

  TAudioFileCache = class(TDspPersistent)
  private
    FAudioFile         : TCustomAudioFile;

    FPosition          : Cardinal;
    FLatency           : Cardinal;
    FReadAheadOffset   : Cardinal;
    FWriteAheadOffset  : Cardinal;
    FTotalBufferSize   : Cardinal;

    FBuffer            : TDAVArrayOfSingleFixedArray;
    FBufferPos         : Cardinal;
    FBlockSize         : Cardinal;
    FBufferSize        : Cardinal;
    FDriver            : TDriver;
    FCriticalSection   : TCriticalSection;
    procedure SetPosition(const Value: Cardinal);
    procedure SetLatency(const Value: Cardinal);
    function GetChannelCount: Cardinal;
    procedure SetChannelCount(const Value: Cardinal);
    procedure SetBlockSize(const Value: Cardinal);
    procedure SetBufferSize(const Value: Cardinal);
    function GetFreeSamplesInBuffer: Cardinal;
    procedure FillBufferZero(SampleFrames: Cardinal);
  protected
    constructor Create; overload; virtual;
    procedure AllocateBuffer; virtual;
    procedure BlockSizeChanged; virtual;
    procedure ChannelCountChanged; virtual;
    procedure BufferSizeChanged; virtual;
    procedure PositionChanged; virtual;
    procedure LatencyChanged; virtual;
    procedure BackgroundProcess; virtual;
    procedure BackgroundRead; virtual;
    procedure BackgroundWrite; virtual;

(*
    procedure ReadChannelBlock(const Channel, Position: Integer; Data: PDAVSingleFixedArray);
    procedure WriteChannelBlock(const Channel, Position: Integer; Data: PDAVSingleFixedArray);
*)

    procedure ReadData(const Channel, Position: Integer; Data: PDAVSingleFixedArray; SampleFrames: Cardinal);
    procedure WriteData(const Channel, Position: Integer; Data: PDAVSingleFixedArray; SampleFrames: Cardinal);
    procedure WriteZero(const Channel, Position: Integer; SampleFrames: Cardinal);

    procedure DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure EncodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);

    property TotalBufferSize: Cardinal read FTotalBufferSize;
    property FreeSamplesInBuffer: Cardinal read GetFreeSamplesInBuffer;
  public
    constructor Create(const FileName: TFileName); overload; virtual;
    constructor Create(const Stream: TStream; AudioFileClass: TAudioFileClass = nil); overload; virtual;
    destructor Destroy; override;

    procedure ReadChannelData(const Channel: Integer; Data: PDAVSingleFixedArray);
    procedure WriteChannelData(const Channel: Integer; Data: PDAVSingleFixedArray);
    procedure Advance;
    procedure ReadAdvance;
    procedure WriteAdvance;

    property ChannelCount: Cardinal read GetChannelCount write SetChannelCount;
    property Position: Cardinal read FPosition write SetPosition;
    property BlockSize: Cardinal read FBlockSize write SetBlockSize;
    property Latency: Cardinal read FLatency write SetLatency;
    property BufferSize: Cardinal read FBufferSize write SetBufferSize;
  end;

implementation

uses
  Math;

resourcestring
  RCStrUnableToOpenAudio = 'Unable to open audio file';
  RCStrChannelOutOfBounds = 'Channel out of bounds (%d)';

{ TDriver }

constructor TDriver.Create(AudioFileCache: TAudioFileCache);
begin
 inherited Create(False);
 FAudioFileCache := AudioFileCache;
end;

procedure TDriver.Execute;
begin
 while not Terminated do
  begin
   FAudioFileCache.BackgroundProcess;
   Sleep(1);
  end;
end;

{ TAudioFileCache }

constructor TAudioFileCache.Create;
begin
 inherited;
 FBufferSize := 1 shl 16;
 FBlockSize  := 1 shl 8;
 FLatency    := FBlockSize;

 FBufferPos         := 0;
 FPosition          := 0;
 FReadAheadOffset   := 0;
 FWriteAheadOffset  := FLatency;

 FAudioFile.BlockSize := 0;
 FAudioFile.OnDecode  := DecodeHandler;
 FAudioFile.OnEncode  := EncodeHandler;

 SetLength(FBuffer, FAudioFile.ChannelCount);
 AllocateBuffer;
 FDriver := TDriver.Create(Self);
end;

constructor TAudioFileCache.Create(const FileName: TFileName);
var
  AudioFileClass: TAudioFileClass;
begin
 FCriticalSection := TCriticalSection.Create;

 if FileExists(FileName)
  then AudioFileClass := FileNameToFormat(FileName)
  else AudioFileClass := ExtensionToFileFormat(ExtractFileExt(FileName));

 if AudioFileClass = nil
  then raise Exception.Create(RCStrUnableToOpenAudio);

 FAudioFile := AudioFileClass.Create(FileName);

 Create;
end;

constructor TAudioFileCache.Create(const Stream: TStream; AudioFileClass: TAudioFileClass = nil);
begin
 FCriticalSection := TCriticalSection.Create;

 if (AudioFileClass = nil) and (Stream.Size > 0)
  then AudioFileClass := StreamToFormat(Stream);

 if AudioFileClass = nil
  then raise Exception.Create(RCStrUnableToOpenAudio);

 FAudioFile := AudioFileClass.Create(Stream);

 Create;
end;

destructor TAudioFileCache.Destroy;
var
  Channel : Cardinal;
begin
 with FDriver do
  begin
   if Suspended then Resume;
   Terminate;
   WaitFor;
   Free;
  end;

 FreeAndNil(FAudioFile);
 for Channel := 0 to Length(FBuffer) - 1 do
  if assigned(FBuffer[Channel]) then Dispose(FBuffer[Channel]);

 FreeAndNil(FCriticalSection); 
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TAudioFileCache.GetChannelCount: Cardinal;
begin
 Assert(assigned(FAudioFile));
 Result := FAudioFile.ChannelCount;
end;

function TAudioFileCache.GetFreeSamplesInBuffer: Cardinal;
begin
 Assert(FReadAheadOffset + FWriteAheadOffset <= FTotalBufferSize);
 Result := FTotalBufferSize - FReadAheadOffset - FWriteAheadOffset;
 Assert(Result <= FTotalBufferSize);
end;

procedure TAudioFileCache.DecodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel : Integer;
  AbsPos  : Cardinal;
begin
 Assert(Coder.ChannelCount = FAudioFile.ChannelCount);

 if FBufferPos < FReadAheadOffset
  then AbsPos := FTotalBufferSize + FBufferPos - FReadAheadOffset
  else AbsPos := FBufferPos - FReadAheadOffset;

 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to Coder.ChannelCount - 1
   do WriteData(Channel, AbsPos, ChannelPointer[Channel], SampleFrames);

 FReadAheadOffset := FReadAheadOffset + Coder.SampleFrames;
 Assert(FReadAheadOffset < FTotalBufferSize);
end;

procedure TAudioFileCache.FillBufferZero(SampleFrames: Cardinal);
var
  Channel : Integer;
  AbsPos  : Cardinal;
begin
 if FBufferPos < FReadAheadOffset
  then AbsPos := FTotalBufferSize + FBufferPos - FReadAheadOffset
  else AbsPos := FBufferPos - FReadAheadOffset;

 Assert(AbsPos < FTotalBufferSize);

 for Channel := 0 to FAudioFile.ChannelCount - 1
  do WriteZero(Channel, AbsPos, SampleFrames);

 FReadAheadOffset := FReadAheadOffset + SampleFrames;
 Assert(FReadAheadOffset < FTotalBufferSize);
end;

procedure TAudioFileCache.EncodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  Channel : Integer;
  AbsPos  : Cardinal;
begin
 Assert(Coder.ChannelCount = FAudioFile.ChannelCount);

 AbsPos := FBufferPos + FWriteAheadOffset;
 if AbsPos >= FTotalBufferSize
  then AbsPos := AbsPos - FTotalBufferSize;

 with TCustomChannel32DataCoder(Coder) do
  for Channel := 0 to Coder.ChannelCount - 1
   do ReadData(Channel, AbsPos, ChannelPointer[Channel], SampleFrames);

 Assert(FWriteAheadOffset >= FLatency + Coder.SampleFrames);
 FWriteAheadOffset := FWriteAheadOffset - Coder.SampleFrames;
end;

procedure TAudioFileCache.ReadData(const Channel, Position: Integer;
  Data: PDAVSingleFixedArray; SampleFrames: Cardinal);
var
  PartialSamples : Cardinal;
begin
 Assert(SampleFrames > 0);
 if Cardinal(Position) >= FTotalBufferSize
  then Assert(Cardinal(Position) < FTotalBufferSize);

 if Cardinal(Position) + SampleFrames <= FTotalBufferSize
  then Move(FBuffer[Channel]^[Position], Data^[0], SampleFrames * SizeOf(Single))
  else
   begin
    PartialSamples := (FTotalBufferSize - Cardinal(Position));
    Move(FBuffer[Channel]^[Position], Data^[0], PartialSamples * SizeOf(Single));
    Move(FBuffer[Channel]^[0], Data^[PartialSamples], (SampleFrames - PartialSamples) * SizeOf(Single));
   end;
end;

procedure TAudioFileCache.WriteData(const Channel, Position: Integer;
  Data: PDAVSingleFixedArray; SampleFrames: Cardinal);
var
  PartialSamples : Cardinal;
begin
 Assert(Cardinal(Position) < FTotalBufferSize);
 Assert(SampleFrames > 0);

(*
 for PartialSamples := 0 to SampleFrames - 1
  do FBuffer[Channel]^[(Position + PartialSamples) mod FTotalBufferSize] := 1 / FPosition;
*)
 if Cardinal(Position) + SampleFrames < FTotalBufferSize
  then Move(Data^[0], FBuffer[Channel]^[Position], SampleFrames * SizeOf(Single))
  else
   begin
    PartialSamples := (FTotalBufferSize - Cardinal(Position));
    if PartialSamples > 0
     then Move(Data^[0], FBuffer[Channel]^[Position], PartialSamples * SizeOf(Single));
    Move(Data^[PartialSamples], FBuffer[Channel]^[0], (SampleFrames - PartialSamples) * SizeOf(Single));
   end;
end;

procedure TAudioFileCache.WriteZero(const Channel, Position: Integer;
  SampleFrames: Cardinal);
var
  PartialSamples : Cardinal;
begin
 Assert(Cardinal(Position) < FTotalBufferSize);
 Assert(SampleFrames > 0);

 if Cardinal(Position) + SampleFrames < FTotalBufferSize
  then FillChar(FBuffer[Channel]^[Position], SampleFrames * SizeOf(Single), 0)
  else
   begin
    PartialSamples := (FTotalBufferSize - Cardinal(Position));
    FillChar(FBuffer[Channel]^[Position], PartialSamples * SizeOf(Single), 0);
    FillChar(FBuffer[Channel]^[0], (SampleFrames - PartialSamples) * SizeOf(Single), 0);
   end;
end;


procedure TAudioFileCache.ReadChannelData(const Channel: Integer;
  Data: PDAVSingleFixedArray);
begin
 // check channel is valid
 if (Channel < 0) or (Cardinal(Channel) >= ChannelCount)
  then raise Exception.CreateFmt(RCStrChannelOutOfBounds, [Channel]);

 ReadData(Channel, FBufferPos, Data, FBlockSize);
end;

procedure TAudioFileCache.WriteChannelData(const Channel: Integer;
  Data: PDAVSingleFixedArray);
begin
 // check channel is valid
 if (Channel < 0) or (Cardinal(Channel) >= ChannelCount)
  then raise Exception.CreateFmt('Channel out of bounds (%d)', [Channel]);

 if FBufferPos < FLatency
  then WriteData(Channel, FTotalBufferSize + FBufferPos - FLatency, Data, FBlockSize)
  else WriteData(Channel, FBufferPos - FLatency, Data, FBlockSize);
end;

procedure TAudioFileCache.BackgroundProcess;
begin
 FCriticalSection.Enter;
 try
  BackgroundWrite;
  BackgroundRead;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.BackgroundWrite;
const
  CBlockSize : Cardinal = 1 shl 14;
begin
 // eventuall write samples
 if ChannelCount * (FWriteAheadOffset - FLatency) > CBlockSize
  then FAudioFile.Encode(FPosition - FWriteAheadOffset, CBlockSize div ChannelCount);
end;

procedure TAudioFileCache.BackgroundRead;
const
  CBlockSize       : Cardinal = 1 shl 14;
var
  SampleFrames     : Cardinal;
  PartitialSamples : Cardinal;
begin
 // eventuall read samples
 if ChannelCount * FreeSamplesInBuffer > CBlockSize then
  begin
   SampleFrames := CBlockSize div ChannelCount;
   if FPosition + FReadAheadOffset + SampleFrames < FAudioFile.SampleFrames
    then FAudioFile.Decode(FPosition + FReadAheadOffset, SampleFrames) else
   if FPosition >= FAudioFile.SampleFrames
    then FillBufferZero(SampleFrames)
    else
     begin
      PartitialSamples := FAudioFile.SampleFrames - FPosition + FReadAheadOffset;
      FAudioFile.Decode(FPosition + FReadAheadOffset, PartitialSamples);
      FillBufferZero(SampleFrames - PartitialSamples);
     end;
  end;
end;

procedure TAudioFileCache.SetBlockSize(const Value: Cardinal);
begin
 if FBlockSize <> Value then
  begin
   FBlockSize := Value;
   BlockSizeChanged;
  end;
end;

procedure TAudioFileCache.SetChannelCount(const Value: Cardinal);
begin
 if FAudioFile.ChannelCount <> Value then
  begin
   FAudioFile.ChannelCount := Value;
   ChannelCountChanged;
  end;
end;

procedure TAudioFileCache.SetBufferSize(const Value: Cardinal);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   BufferSizeChanged;
  end;
end;

procedure TAudioFileCache.SetPosition(const Value: Cardinal);
begin
 if FPosition <> Value then
  begin
   FPosition := Value;
   PositionChanged;
  end;
end;

procedure TAudioFileCache.SetLatency(const Value: Cardinal);
begin
 if FLatency <> Value then
  begin
   FLatency := Value;
   LatencyChanged;
  end;
end;

procedure TAudioFileCache.BlockSizeChanged;
begin
 FCriticalSection.Enter;
 try
  AllocateBuffer;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.BufferSizeChanged;
begin
 FCriticalSection.Enter;
 try
  AllocateBuffer;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.LatencyChanged;
begin
 FCriticalSection.Enter;
 try
  if FWriteAheadOffset < FLatency
   then FWriteAheadOffset := FLatency;
  AllocateBuffer;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.AllocateBuffer;
var
  Channel : Integer;
begin
 FTotalBufferSize := Max(FBufferSize, FBlockSize + FLatency);
 for Channel := 0 to FAudioFile.ChannelCount - 1 do
  begin
   ReAllocMem(FBuffer[Channel], FTotalBufferSize * SizeOf(Single));
   FillChar(FBuffer[Channel]^, FTotalBufferSize * SizeOf(Single), 0);
  end;
end;

procedure TAudioFileCache.ChannelCountChanged;
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := FAudioFile.ChannelCount to Length(FBuffer) - 1 do
   begin
    Dispose(FBuffer[Channel]);
    FBuffer[Channel] := nil;
   end;
  SetLength(FBuffer, FAudioFile.ChannelCount);
  AllocateBuffer;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.Advance;
begin
 FCriticalSection.Enter;
 try
  Position := Position + FBlockSize;
  FBufferPos := FBufferPos + FBlockSize;
  if FBufferPos >= FTotalBufferSize
   then FBufferPos := FBufferPos - FTotalBufferSize;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.ReadAdvance;
begin
 FCriticalSection.Enter;
 try
  if FReadAheadOffset > FBlockSize
   then FReadAheadOffset := FReadAheadOffset - FBlockSize
   else FReadAheadOffset := 0;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.WriteAdvance;
begin
 FCriticalSection.Enter;
 try
  FWriteAheadOffset := FWriteAheadOffset + FBlockSize;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TAudioFileCache.PositionChanged;
begin
 // yet todo!
end;

end.
