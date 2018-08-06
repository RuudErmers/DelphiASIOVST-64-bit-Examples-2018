unit DAV_DspBufferedAudioFilePlayer;

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

{$I DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Types, DAV_Classes, DAV_DspCircularBuffer,
  DAV_AudioFile, DAV_ChannelDataCoder;

type
  TCustomBufferedAudioPlayer = class;

  TReloadThread = class(TThread)
  protected
    FBufferedAudioPlayer: TCustomBufferedAudioPlayer;
    procedure Execute; override;
  public
    constructor Create(BufferedAudioPlayer: TCustomBufferedAudioPlayer);
  end;

  TBufferInterpolation = (biNone, biLinear, biHermite, biBSpline6Point5thOrder);

  TCustomBufferedAudioPlayer = class(TDspSampleRatePersistent)
  private
    FRatio               : Single;
    FAllowSuspend        : Boolean;
    FFractalPos          : Single;
    FInterpolation       : TBufferInterpolation;
    FPitch               : Single;
    FPitchFactor         : Single;
    FInterpolationBuffer : array of PDAV8SingleArray;

    FAudioFile           : TCustomAudioFile;
    FBuffer              : TCircularMultiBuffer32;
    FBufferSize          : Integer;
    FSampleRate          : Single;
    FStreamBuffer        : TDAVArrayOfSingleFixedArray;
    FStreamBufSize       : Integer;
    FTimeOut             : Integer;

    FCurrentPosition     : Integer;
    FSubBlockPosition    : Integer;
    function GetBufferFill: Single;
    procedure DecodeHandler(Sender: TObject; const Coder: TCustomChannelDataCoder; var Position: Cardinal);
    procedure SetBlockSize(Value: Integer);
    procedure SetBufferSize(const Value: Integer);
    procedure SetInterpolation(const Value: TBufferInterpolation);
    procedure SetPitch(const Value: Single);
    function CheckReload: Boolean;
    function GetChannelCount: Integer;
  protected
    FBufferThread : TReloadThread;
    procedure AllocateStreamBuffer;
    procedure AllocateInterpolationBuffer;
    procedure CalculatePitchFactor;
    procedure CalculateSampleRateRatio;
    procedure CalculateTimeOut;
    procedure GetInternalSamples(Data: TDAVArrayOfSingleFixedArray; SampleFrames: Integer); overload;

    procedure AudioFileChanged; virtual;
    procedure BlockSizeChanged; virtual;
    procedure BufferSizeChanged; virtual;
    procedure InterpolationChanged; virtual;
    procedure PitchChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GetSamples(Data: TDAVArrayOfSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);

    property AllowSuspend: Boolean read FAllowSuspend write FAllowSuspend;
    property AudioFile: TCustomAudioFile read FAudioFile;
    property BlockSize: Integer read FStreamBufSize write SetBlockSize;
    property BufferFill: Single read GetBufferFill;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property ChannelCount: Integer read GetChannelCount;
    property Interpolation: TBufferInterpolation read FInterpolation write SetInterpolation;
    property Pitch: Single read FPitch write SetPitch;
  end;

  TBufferedAudioFilePlayer = class(TCustomBufferedAudioPlayer)
  private
    FFileName : TFileName;
    procedure SetFileName(const Value: TFileName);
  protected
    procedure FileNameChanged; virtual;
  public
    constructor Create; override;
  published
    property Filename: TFileName read FFileName write SetFileName;
    property BufferSize;
    property BlockSize;
    property SampleRate;
    property BufferFill;
    property Interpolation;
    property Pitch;
  end;

  TBufferedAudioStreamPlayer = class(TCustomBufferedAudioPlayer)
  private
    FStream : TStream;
    procedure SetStream(const Value: TStream);
  protected
    procedure StreamChanged; virtual;
  public
    constructor Create; override;
  published
    property Stream: TStream read FStream write SetStream;
    property BufferSize;
    property BlockSize;
    property SampleRate;
    property BufferFill;
    property Interpolation;
    property Pitch;
  end;

implementation

uses
  Math, DAV_DspInterpolation;

{ TReloadThread }

constructor TReloadThread.Create(BufferedAudioPlayer: TCustomBufferedAudioPlayer);
begin
 FBufferedAudioPlayer := BufferedAudioPlayer;
 inherited Create(True);
end;

procedure TReloadThread.Execute;
var
  IdleLoops: Integer;
begin
 IdleLoops := 20;
 with FBufferedAudioPlayer do
  while not Terminated do
   begin
    if CheckReload
     then IdleLoops := 20;
    Dec(IdleLoops);
    if FAllowSuspend and (IdleLoops <= 0)
     then Suspended := True
     else Sleep(FTimeOut);
   end;
end;

{ TCustomBufferedAudioPlayer }

constructor TCustomBufferedAudioPlayer.Create;
begin
 inherited;
 FPitchFactor  := 1;
 FAllowSuspend := False;

 FBufferSize := 16384;
 FSampleRate := 44100;
 FStreamBufSize := 4096;
 FAllowSuspend := False;
 CalculateTimeOut;

 AllocateStreamBuffer;
 AllocateInterpolationBuffer;

 FBuffer := TCircularMultiBuffer32.Create(FBufferSize);

 // initialize reload thread
 FBufferThread := TReloadThread.Create(Self);
 {$IFDEF MSWindows}
 FBufferThread.Priority := tpNormal;
 {$ENDIF}

 CalculateSampleRateRatio;
 InterpolationChanged;
end;

destructor TCustomBufferedAudioPlayer.Destroy;
var
  ChannelIndex : Integer;
begin
 with FBufferThread do
  begin
   if Suspended
    then Suspended := False;
   Terminate;
   WaitFor;
  end;
 FreeAndNil(FBufferThread);

 FreeAndNil(FBuffer);
 FreeAndNil(FAudioFile);

 for ChannelIndex := 0 to Length(FStreamBuffer) - 1
  do Dispose(FStreamBuffer[ChannelIndex]);

 for ChannelIndex := 0 to Length(FInterpolationBuffer) - 1 do
  if Assigned(FInterpolationBuffer[ChannelIndex])
   then Dispose(FInterpolationBuffer[ChannelIndex]);

 inherited;
end;

procedure TCustomBufferedAudioPlayer.AllocateStreamBuffer;
var
  ChannelIndex : Integer;
begin
 SetLength(FStreamBuffer, ChannelCount);
 for ChannelIndex := 0 to Length(FStreamBuffer) - 1
  do ReallocMem(FStreamBuffer[ChannelIndex], FStreamBufSize * SizeOf(Single));
end;

function TCustomBufferedAudioPlayer.GetBufferFill: Single;
begin
 Result := 100 * (FBuffer.SamplesInBuffer / FBuffer.BufferSize);
end;

function TCustomBufferedAudioPlayer.GetChannelCount: Integer;
begin
 if Assigned(FAudioFile)
  then Result := FAudioFile.ChannelCount
  else Result := 0;
end;

function TCustomBufferedAudioPlayer.CheckReload: Boolean;
begin
 Result := False;
 while (FBuffer.BufferSize - FBuffer.SamplesInBuffer) > FStreamBufSize do
  begin
   Result := True;

   if Assigned(FAudioFile) then
    begin
     FAudioFile.OnDecode := DecodeHandler;
     FSubBlockPosition := 0;
     if FCurrentPosition + FStreamBufSize < FAudioFile.SampleFrames
      then FAudioFile.Decode(FCurrentPosition * ChannelCount, FStreamBufSize)
      else
       begin
        FAudioFile.Decode(FCurrentPosition * ChannelCount, FAudioFile.SampleFrames - FCurrentPosition);
        FAudioFile.Decode(0, FStreamBufSize - (FAudioFile.SampleFrames - FCurrentPosition));
        FCurrentPosition := 0;
       end;
     Inc(FCurrentPosition, FStreamBufSize);
    end
   else
    begin
     FillChar(FStreamBuffer[0]^, FStreamBufSize * SizeOf(Single), 0);
     FillChar(FStreamBuffer[1]^, FStreamBufSize * SizeOf(Single), 0);
    end;
   FBuffer.WriteBuffer(FStreamBuffer, FStreamBufSize);
  end;
end;

procedure TCustomBufferedAudioPlayer.LoadFromFile(FileName: TFileName);
var
  AudioFileClass : TAudioFileClass;
begin
 AudioFileClass := FileNameToFormat(FileName);
 if Assigned(AudioFileClass) then
  begin
   if Assigned(FAudioFile)
    then FreeAndNil(FAudioFile);

   FAudioFile := AudioFileClass.Create(FileName);
   FCurrentPosition := 0;
  end;

 AudioFileChanged;
end;

procedure TCustomBufferedAudioPlayer.LoadFromStream(Stream: TStream);
var
  AudioFileClass : TAudioFileClass;
begin
 AudioFileClass := StreamToFormat(Stream);
 if Assigned(AudioFileClass) then
  begin
   if Assigned(FAudioFile)
    then FreeAndNil(FAudioFile);

   FAudioFile := AudioFileClass.Create(Stream);
   FCurrentPosition := 0;
  end;

 AudioFileChanged;
end;

procedure TCustomBufferedAudioPlayer.DecodeHandler(Sender: TObject;
  const Coder: TCustomChannelDataCoder; var Position: Cardinal);
var
  ChannelIndex : Integer;
begin
 assert(Coder is TCustomChannel32DataCoder);
 with TCustomChannel32DataCoder(Coder) do
  begin
   case ChannelCount of
    0 : ;
    1 : for ChannelIndex := 0 to Length(FStreamBuffer) - 1
         do Move(ChannelPointer[0]^[0], FStreamBuffer[ChannelIndex]^[FSubBlockPosition], SampleFrames * SizeOf(Single));
   else for ChannelIndex := 0 to Length(FStreamBuffer) - 1
         do Move(ChannelPointer[ChannelIndex mod ChannelCount]^[0], FStreamBuffer[ChannelIndex]^[FSubBlockPosition], SampleFrames * SizeOf(Single));
   end;
   FSubBlockPosition := FSubBlockPosition + SampleFrames;
  end;
end;

procedure TCustomBufferedAudioPlayer.AudioFileChanged;
begin
 if Assigned(FAudioFile) then
  begin
   FBuffer.ChannelCount := ChannelCount;
   FSampleRate := FAudioFile.SampleRate;
   SampleRateChanged;
   CalculateSampleRateRatio;
   AllocateStreamBuffer;
   AllocateInterpolationBuffer;
   FBufferThread.Resume;
  end;
end;

procedure TCustomBufferedAudioPlayer.Reset;
begin
 FCurrentPosition := 0;
end;

procedure TCustomBufferedAudioPlayer.BufferSizeChanged;
begin
 FBuffer.BufferSize := FBufferSize;
end;

procedure TCustomBufferedAudioPlayer.BlockSizeChanged;
begin
 ReallocMem(FStreamBuffer[0], FStreamBufSize * SizeOf(Single));
 ReallocMem(FStreamBuffer[1], FStreamBufSize * SizeOf(Single));
end;

procedure TCustomBufferedAudioPlayer.CalculateTimeOut;
begin
 FTimeOut := Round(1000 * FStreamBufSize / FSampleRate);
end;

procedure TCustomBufferedAudioPlayer.SetBlockSize(Value: Integer);
begin
 if Value > FBufferSize div 2
  then Value := FBufferSize div 2;
 
 if FStreamBufSize <> Value then
  begin
   FStreamBufSize := Value;
   FAudioFile.BlockSize := ChannelCount * FStreamBufSize;
   BlockSizeChanged;
  end;
end;

procedure TCustomBufferedAudioPlayer.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   if BlockSize > FBufferSize div 2
    then BlockSize := FBufferSize div 2;

   BufferSizeChanged;
  end;
end;

procedure TCustomBufferedAudioPlayer.SetInterpolation(
  const Value: TBufferInterpolation);
begin
 if FInterpolation <> Value then
  begin
   FInterpolation := Value;
   InterpolationChanged;
  end;
end;

procedure TCustomBufferedAudioPlayer.SetPitch(const Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

procedure TCustomBufferedAudioPlayer.SampleRateChanged;
begin
 CalculateTimeOut;
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedAudioPlayer.InterpolationChanged;
begin
 AllocateInterpolationBuffer;
end;

procedure TCustomBufferedAudioPlayer.AllocateInterpolationBuffer;
var
  ChannelIndex : Integer;
begin
 SetLength(FInterpolationBuffer, ChannelCount);
 case FInterpolation of
  biNone:
   for ChannelIndex := 0 to Length(FInterpolationBuffer) - 1 do
    begin
     ReallocMem(FInterpolationBuffer[ChannelIndex], 1 * SizeOf(Single));
     FillChar(FInterpolationBuffer[ChannelIndex]^, 1 * SizeOf(Single), 0);
    end;
  biLinear:
   for ChannelIndex := 0 to Length(FInterpolationBuffer) - 1 do
    begin
     ReallocMem(FInterpolationBuffer[ChannelIndex], 2 * SizeOf(Single));
     FillChar(FInterpolationBuffer[ChannelIndex]^, 2 * SizeOf(Single), 0);
    end;
  biHermite:
   for ChannelIndex := 0 to Length(FInterpolationBuffer) - 1 do
    begin
     ReallocMem(FInterpolationBuffer[ChannelIndex], 4 * SizeOf(Single));
     FillChar(FInterpolationBuffer[ChannelIndex]^, 4 * SizeOf(Single), 0);
    end;
  biBSpline6Point5thOrder:
   for ChannelIndex := 0 to Length(FInterpolationBuffer) - 1 do
    begin
     ReallocMem(FInterpolationBuffer[ChannelIndex], 6 * SizeOf(Single));
     FillChar(FInterpolationBuffer[ChannelIndex]^, 6 * SizeOf(Single), 0);
    end;
 end;
end;

procedure TCustomBufferedAudioPlayer.CalculatePitchFactor;
begin
 FPitchFactor := Power(2, FPitch / 12);
end;

procedure TCustomBufferedAudioPlayer.PitchChanged;
begin
 CalculatePitchFactor;
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedAudioPlayer.CalculateSampleRateRatio;
begin
 if Assigned(FAudioFile)
  then FRatio := FPitchFactor * FAudioFile.SampleRate / SampleRate
  else FRatio := FPitchFactor;
end;

procedure TCustomBufferedAudioPlayer.GetInternalSamples(Data: TDAVArrayOfSingleFixedArray;
  SampleFrames: Integer);
var
  Channel        : Integer;
  SampleInBuffer : Integer;
begin
 SampleInBuffer := FBuffer.SamplesInBuffer;
 if SampleFrames < SampleInBuffer
  then FBuffer.ReadBuffer(Data, SampleFrames)
  else
   begin
    if SampleInBuffer > 0
     then FBuffer.ReadBuffer(Data, SampleInBuffer);

    for Channel := 0 to Length(Data) - 1
     do FillChar(Data[Channel]^, (SampleFrames - SampleInBuffer) * SizeOf(Single), 0);
   end;
end;

procedure TCustomBufferedAudioPlayer.GetSamples(Data: TDAVArrayOfSingleFixedArray;
  SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 // eventually reactivate thread
 if FAllowSuspend and FBufferThread.Suspended then FBufferThread.Resume;
 if FRatio = 1
  then GetInternalSamples(Data, SampleFrames)
  else
   case FInterpolation of
    biNone:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1
          do Data[Channel] := PDAVSingleFixedArray(FInterpolationBuffer[Channel]);
         GetInternalSamples(TDAVArrayOfSingleFixedArray(FInterpolationBuffer), 1);
         FFractalPos := FFractalPos - 1;
        end;

       for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1
        do Data[Channel]^[Sample]  := FInterpolationBuffer[Channel]^[0];
      end;
    biLinear:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1 do
          begin
           FInterpolationBuffer[Channel]^[1] := FInterpolationBuffer[Channel]^[0];
           Data[Channel] := PDAVSingleFixedArray(FInterpolationBuffer[Channel]);
          end;
         GetInternalSamples(TDAVArrayOfSingleFixedArray(FInterpolationBuffer), 1);
         FFractalPos := FFractalPos - 1;
        end;
       for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1
        do Data[Channel]^[Sample]  := LinearInterpolation(1 - FFractalPos, PDAV2SingleArray(FInterpolationBuffer[Channel]));
      end;
    biHermite:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1 do
          begin
           Move(FInterpolationBuffer[Channel]^[0], FInterpolationBuffer[Channel]^[1], 3 * SizeOf(Single));
           Data[Channel] := PDAVSingleFixedArray(FInterpolationBuffer[Channel]);
          end;
         GetInternalSamples(TDAVArrayOfSingleFixedArray(FInterpolationBuffer), 1);
         FFractalPos := FFractalPos - 1;
        end;
       for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1
        do Data[Channel]^[Sample]  := Hermite32_asm(1 - FFractalPos, PDAV4SingleArray(FInterpolationBuffer[Channel]));
      end;
    biBSpline6Point5thOrder:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1 do
          begin
           Move(FInterpolationBuffer[Channel]^[0], FInterpolationBuffer[Channel]^[1], 5 * SizeOf(Single));
           Data[Channel] := PDAVSingleFixedArray(FInterpolationBuffer[Channel]);
          end;
         GetInternalSamples(TDAVArrayOfSingleFixedArray(FInterpolationBuffer), 1);
         FFractalPos := FFractalPos - 1;
        end;
       for Channel := 0 to Min(Length(Data), Length(FInterpolationBuffer)) - 1
        do Data[Channel]^[Sample]  := BSplineInterpolation6Point5thOrder(1 - FFractalPos, PDAV6SingleArray(FInterpolationBuffer[Channel])^);
      end;
   end;
end;


{ TBufferedAudioFilePlayer }

constructor TBufferedAudioFilePlayer.Create;
begin
 inherited;
 FFileName := '';
end;

procedure TBufferedAudioFilePlayer.SetFileName(const Value: TFileName);
begin
 if FFileName <> Value then
  begin
   FFileName := Value;
   FileNameChanged;
  end;
end;

procedure TBufferedAudioFilePlayer.FileNameChanged;
begin
 LoadFromFile(FFileName);
end;

{ TBufferedAudioStreamPlayer }

constructor TBufferedAudioStreamPlayer.Create;
begin
 inherited;
 FStream := nil;
end;

procedure TBufferedAudioStreamPlayer.SetStream(const Value: TStream);
begin
 if FStream <> Value then
  begin
   FStream := Value;
   StreamChanged;
  end;
end;

procedure TBufferedAudioStreamPlayer.StreamChanged;
begin
 LoadFromStream(FStream);
end;

end.
