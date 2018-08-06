unit DAV_DspBufferedMp3Player;

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
  DAV_MpegAudio;

type
  TBufferThread = class(TThread)
  private
    FMpegAudio     : TMpegAudio;
    FBufferSize    : Integer;
    FBuffer        : TCircularStereoBuffer32;
    FSampleRate    : Single;
    FStreamBuffer  : array [0..1] of PDAVSingleFixedArray;
    FStreamBufSize : Integer;
    FTimeOut       : Integer;
    FAllowSuspend  : Boolean;
    FLoop          : Boolean;
    procedure SetBufferSize(const Value: Integer);
    procedure SetBlockSize(Value: Integer);
    function GetBufferFill: Single;
    procedure EndOfFileHandler(Sender: TObject);
  protected
    procedure Execute; override;
    procedure BlockSizeChanged; virtual;
    procedure BufferSizeChanged; virtual;
    procedure CalculateTimeOut; virtual;
    procedure SampleRateChanged; virtual;
    procedure MpegAudioChanged; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);

    property AllowSuspend: Boolean read FAllowSuspend write FAllowSuspend;
    property BufferSize: Integer read FBufferSize write SetBufferSize;
    property BlockSize: Integer read FStreamBufSize write SetBlockSize;
    property Loop: Boolean read FLoop write FLoop;
    property SampleRate: Single read FSampleRate;
    property BufferFill: Single read GetBufferFill;

    property MpegAudio: TMPEGAudio read FMpegAudio;
  end;

  TBufferInterpolation = (biNone, biLinear, biHermite, biBSpline6Point5thOrder);

  TCustomBufferedMP3Player = class(TDspSampleRatePersistent)
  private
    FRatio               : Single;
    FAllowSuspend        : Boolean;
    FFractalPos          : Single;
    FInterpolation       : TBufferInterpolation;
    FPitch               : Single;
    FPitchFactor         : Single;
    FInterpolationBuffer : array [0..1] of PDAV8SingleArray;
    function GetBlockSize: Integer;
    function GetBufferSize: Integer;
    function GetBufferFill: Single;
    function GetMpegAudio: TMpegAudio;
    procedure SetBlockSize(const Value: Integer);
    procedure SetBufferSize(const Value: Integer);
    procedure SetAllowSuspend(const Value: Boolean);
    procedure SetInterpolation(const Value: TBufferInterpolation);
    procedure SetPitch(const Value: Single);
    function GetLoop: Boolean;
    procedure SetLoop(const Value: Boolean);
  protected
    FBufferThread : TBufferThread;
    procedure CalculatePitchFactor; virtual;
    procedure CalculateSampleRateRatio; virtual;
    procedure SampleRateChanged; override;
    procedure InterpolationChanged; virtual;
    procedure PitchChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GetSamples(Left, Right: PDAVSingleFixedArray; SampleFrames: Integer);
    procedure Reset;

    property AllowSuspend: Boolean read FAllowSuspend write SetAllowSuspend;
    property BlockSize: Integer read GetBlockSize write SetBlockSize;
    property BufferFill: Single read GetBufferFill;
    property BufferSize: Integer read GetBufferSize write SetBufferSize;
    property Loop: Boolean read GetLoop write SetLoop;
    property Interpolation: TBufferInterpolation read FInterpolation write SetInterpolation;
    property MpegAudio: TMpegAudio read GetMpegAudio;
    property Pitch: Single read FPitch write SetPitch;
  end;

  TBufferedMP3FilePlayer = class(TCustomBufferedMP3Player)
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

  TBufferedMP3StreamPlayer = class(TCustomBufferedMP3Player)
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

{ TBufferThread }

constructor TBufferThread.Create;
begin
 inherited Create(True);
 FBufferSize := 16384;
 FSampleRate := 44100;
 FStreamBufSize := 4096;
 FAllowSuspend := False;
 CalculateTimeOut;

 GetMem(FStreamBuffer[0], FStreamBufSize * SizeOf(Single));
 GetMem(FStreamBuffer[1], FStreamBufSize * SizeOf(Single));

 FBuffer := TCircularStereoBuffer32.Create(FBufferSize);
end;

destructor TBufferThread.Destroy;
begin
 Dispose(FStreamBuffer[0]);
 Dispose(FStreamBuffer[1]);
 FreeAndNil(FBuffer);
 FreeAndNil(FMpegAudio);
 inherited;
end;

procedure TBufferThread.Execute;
var
  IdleLoops: Integer;
begin
 IdleLoops := 20;
 while not Terminated do
  begin
   while (FBuffer.BufferSize - FBuffer.SamplesInBuffer) > FStreamBufSize do
    begin
     IdleLoops := 20;

     if Assigned(FMpegAudio)
      then FMpegAudio.ReadBuffer(FStreamBuffer[0], FStreamBuffer[1], FStreamBufSize)
      else
       begin
        FillChar(FStreamBuffer[0]^, FStreamBufSize * SizeOf(Single), 0);
        FillChar(FStreamBuffer[1]^, FStreamBufSize * SizeOf(Single), 0);
       end;
     FBuffer.WriteBuffer(FStreamBuffer[0], FStreamBuffer[1], FStreamBufSize);
    end;

   Dec(IdleLoops);
   if FAllowSuspend and (IdleLoops <= 0)
    then Suspended := True
    else Sleep(FTimeOut);
  end;
end;

function TBufferThread.GetBufferFill: Single;
begin
 Result := 100 * (FBuffer.SamplesInBuffer / FBuffer.BufferSize); 
end;

procedure TBufferThread.GetSamples(Left, Right: PDAVSingleFixedArray;
  SampleFrames: Integer);
var
  SampleInBuffer: Integer;  
begin
 SampleInBuffer := FBuffer.SamplesInBuffer;
 if SampleFrames < SampleInBuffer
  then FBuffer.ReadBuffer(Left, Right, SampleFrames)
  else
   begin
    if SampleInBuffer > 0
     then FBuffer.ReadBuffer(Left, Right, SampleInBuffer);

    FillChar( Left^, (SampleFrames - SampleInBuffer) * SizeOf(Single), 0);
    FillChar(Right^, (SampleFrames - SampleInBuffer) * SizeOf(Single), 0);
   end;
end;

procedure TBufferThread.LoadFromFile(FileName: TFileName);
begin
 if FileExists(FileName) then
  begin
   if Assigned(FMpegAudio)
    then FreeAndNil(FMpegAudio);
   FMpegAudio := TMPEGAudio.Create(FileName);
   FMpegAudio.OnEndOfFile := EndOfFileHandler;
   MpegAudioChanged;
  end;
end;

procedure TBufferThread.EndOfFileHandler(Sender: TObject);
begin
 if FLoop
  then Reset;
end;

procedure TBufferThread.LoadFromStream(Stream: TStream);
begin
 if Stream <> nil then
  begin
   if Assigned(FMpegAudio)
    then FreeAndNil(FMpegAudio);
   FMpegAudio := TMPEGAudio.Create(Stream);
   MpegAudioChanged;
  end;
end;

procedure TBufferThread.MpegAudioChanged;
begin
 if Assigned(FMpegAudio) then
  begin
   FSampleRate := FMpegAudio.SampleRate;
   SampleRateChanged;
  end;
end;

procedure TBufferThread.Reset;
begin
 if Assigned(FMpegAudio)
  then FMpegAudio.Reset;
end;

procedure TBufferThread.BufferSizeChanged;
begin
 FBuffer.BufferSize := FBufferSize;
end;

procedure TBufferThread.BlockSizeChanged;
begin
 ReallocMem(FStreamBuffer[0], FStreamBufSize * SizeOf(Single));
 ReallocMem(FStreamBuffer[1], FStreamBufSize * SizeOf(Single));
end;

procedure TBufferThread.CalculateTimeOut;
begin
 FTimeOut := Round(1000 * FStreamBufSize / FSampleRate);
end;

procedure TBufferThread.SampleRateChanged;
begin
 CalculateTimeOut;
end;

procedure TBufferThread.SetBlockSize(Value: Integer);
begin
 if Value > FBufferSize div 2
  then Value := FBufferSize div 2;
 
 if FStreamBufSize <> Value then
  begin
   FStreamBufSize := Value;
   BlockSizeChanged;
  end;
end;

procedure TBufferThread.SetBufferSize(const Value: Integer);
begin
 if FBufferSize <> Value then
  begin
   FBufferSize := Value;
   if BlockSize > FBufferSize div 2
    then BlockSize := FBufferSize div 2;

   BufferSizeChanged;
  end;
end;


{ TCustomBufferedMP3Player }

constructor TCustomBufferedMP3Player.Create;
begin
 inherited;
 FAllowSuspend := False;
 FPitch := 0;
 FRatio := 1;
 CalculatePitchFactor;
 FBufferThread := TBufferThread.Create;
 FBufferThread.Priority := tpNormal;
 FBufferThread.AllowSuspend := FAllowSuspend;

 FInterpolation := biNone;
 GetMem(FInterpolationBuffer[0], 1 * SizeOf(Single));
 GetMem(FInterpolationBuffer[1], 1 * SizeOf(Single));
end;

destructor TCustomBufferedMP3Player.Destroy;
begin
 with FBufferThread do
  begin
   if Suspended
    then Suspended := False;
   Terminate;
   WaitFor;
  end;
 FreeAndNil(FBufferThread);

 Dispose(FInterpolationBuffer[0]);
 Dispose(FInterpolationBuffer[1]);
 inherited;
end;

function TCustomBufferedMP3Player.GetBlockSize: Integer;
begin
 Result := FBufferThread.BlockSize;
end;

function TCustomBufferedMP3Player.GetBufferFill: Single;
begin
 Result := FBufferThread.BufferFill;
end;

function TCustomBufferedMP3Player.GetBufferSize: Integer;
begin
 Result := FBufferThread.BufferSize;
end;

function TCustomBufferedMP3Player.GetLoop: Boolean;
begin
 Result := FBufferThread.Loop;
end;

function TCustomBufferedMP3Player.GetMpegAudio: TMpegAudio;
begin
 Result := FBufferThread.MpegAudio;
end;

procedure TCustomBufferedMP3Player.SetAllowSuspend(const Value: Boolean);
begin
 if FAllowSuspend <> Value then
  begin
   FAllowSuspend := Value;
   FBufferThread.AllowSuspend := True;
  end;
end;

procedure TCustomBufferedMP3Player.SetBlockSize(const Value: Integer);
begin
 FBufferThread.BlockSize := Value;
end;

procedure TCustomBufferedMP3Player.SetInterpolation(
  const Value: TBufferInterpolation);
begin
 if FInterpolation <> Value then
  begin
   FInterpolation := Value;
   InterpolationChanged;
  end;
end;

procedure TCustomBufferedMP3Player.SetLoop(const Value: Boolean);
begin
 FBufferThread.Loop := Value;
end;

procedure TCustomBufferedMP3Player.SetPitch(const Value: Single);
begin
 if FPitch <> Value then
  begin
   FPitch := Value;
   PitchChanged;
  end;
end;

procedure TCustomBufferedMP3Player.InterpolationChanged;
begin
 case FInterpolation of
  biNone:
   begin
    ReallocMem(FInterpolationBuffer[0], 1 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 1 * SizeOf(Single));
    FillChar(FInterpolationBuffer[0]^, SizeOf(Single), 0);
    FillChar(FInterpolationBuffer[1]^, SizeOf(Single), 0);
   end;
  biLinear:
   begin
    ReallocMem(FInterpolationBuffer[0], 2 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 2 * SizeOf(Single));
    FillChar(FInterpolationBuffer[0]^, 2 * SizeOf(Single), 0);
    FillChar(FInterpolationBuffer[1]^, 2 * SizeOf(Single), 0);
   end;
  biHermite:
   begin
    ReallocMem(FInterpolationBuffer[0], 4 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 4 * SizeOf(Single));
    FillChar(FInterpolationBuffer[0]^, 4 * SizeOf(Single), 0);
    FillChar(FInterpolationBuffer[1]^, 4 * SizeOf(Single), 0);
   end;
  biBSpline6Point5thOrder:
   begin
    ReallocMem(FInterpolationBuffer[0], 6 * SizeOf(Single));
    ReallocMem(FInterpolationBuffer[1], 6 * SizeOf(Single));
    FillChar(FInterpolationBuffer[0]^, 6 * SizeOf(Single), 0);
    FillChar(FInterpolationBuffer[1]^, 6 * SizeOf(Single), 0);
   end;
 end;
end;

procedure TCustomBufferedMP3Player.PitchChanged;
begin
 CalculatePitchFactor;
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedMP3Player.CalculatePitchFactor;
begin
 FPitchFactor := Power(2, FPitch / 12);
end;

procedure TCustomBufferedMP3Player.SetBufferSize(const Value: Integer);
begin
 FBufferThread.BufferSize := Value;
end;

procedure TCustomBufferedMP3Player.SampleRateChanged;
begin
 CalculateSampleRateRatio;
end;

procedure TCustomBufferedMP3Player.CalculateSampleRateRatio;
begin
 FRatio := FPitchFactor * FBufferThread.SampleRate / SampleRate;
end;

procedure TCustomBufferedMP3Player.GetSamples(Left, Right: PDAVSingleFixedArray;
  SampleFrames: Integer);
var
  Sample : Integer;
begin
 // eventually reactivate thread
 if FAllowSuspend and FBufferThread.Suspended
  then FBufferThread.Suspended := False;

 if FRatio = 1
  then FBufferThread.GetSamples(Left, Right, SampleFrames)
  else
   case FInterpolation of
    biNone:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := FInterpolationBuffer[0]^[0];
       Right^[Sample] := FInterpolationBuffer[1]^[0];
      end;
    biLinear:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         FInterpolationBuffer[0]^[1] := FInterpolationBuffer[0]^[0];
         FInterpolationBuffer[1]^[1] := FInterpolationBuffer[1]^[0];
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := LinearInterpolation(1 - FFractalPos, PDAV2SingleArray(FInterpolationBuffer[0]));
       Right^[Sample] := LinearInterpolation(1 - FFractalPos, PDAV2SingleArray(FInterpolationBuffer[1]));
      end;
    biHermite:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         Move(FInterpolationBuffer[0]^[0], FInterpolationBuffer[0]^[1], 3 * SizeOf(Single));
         Move(FInterpolationBuffer[1]^[0], FInterpolationBuffer[1]^[1], 3 * SizeOf(Single));
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := Hermite32_asm(1 - FFractalPos, PDAV4SingleArray(FInterpolationBuffer[0]));
       Right^[Sample] := Hermite32_asm(1 - FFractalPos, PDAV4SingleArray(FInterpolationBuffer[1]));
      end;
    biBSpline6Point5thOrder:
     for Sample := 0 to SampleFrames - 1 do
      begin
       FFractalPos := FFractalPos + FRatio;
       while FFractalPos > 1 do
        begin
         Move(FInterpolationBuffer[0]^[0], FInterpolationBuffer[0]^[1], 5 * SizeOf(Single));
         Move(FInterpolationBuffer[1]^[0], FInterpolationBuffer[1]^[1], 5 * SizeOf(Single));
         FBufferThread.GetSamples(PDAVSingleFixedArray(FInterpolationBuffer[0]),
           PDAVSingleFixedArray(FInterpolationBuffer[1]), 1);
         FFractalPos := FFractalPos - 1;
        end;
       Left^[Sample]  := BSplineInterpolation6Point5thOrder(1 - FFractalPos, PDAV6SingleArray(FInterpolationBuffer[0])^);
       Right^[Sample] := BSplineInterpolation6Point5thOrder(1 - FFractalPos, PDAV6SingleArray(FInterpolationBuffer[1])^);
      end;
   end;
end;

procedure TCustomBufferedMP3Player.Reset;
begin
 FBufferThread.Reset;
end;

{ TBufferedMP3FilePlayer }

procedure TBufferedMP3FilePlayer.SetFileName(const Value: TFileName);
begin
 if FFileName <> Value then
  begin
   FFileName := Value;
   FileNameChanged;
  end;
end;

constructor TBufferedMP3FilePlayer.Create;
begin
 inherited;
 FFileName := '';
end;

procedure TBufferedMP3FilePlayer.FileNameChanged;
begin
 with FBufferThread do
  begin
   LoadFromFile(FFileName);
   CalculateSampleRateRatio;
   Suspended := False;
  end;
end;

{ TBufferedMP3StreamPlayer }

constructor TBufferedMP3StreamPlayer.Create;
begin
 inherited;
 FStream := nil;
end;

procedure TBufferedMP3StreamPlayer.SetStream(const Value: TStream);
begin
 if FStream <> Value then
  begin
   FStream := Value;
   StreamChanged;
  end;
end;

procedure TBufferedMP3StreamPlayer.StreamChanged;
begin
 with FBufferThread do
  begin
   LoadFromStream(FStream);
   CalculateSampleRateRatio;
   Suspended := False;
  end;
end;

end.
