unit DAV_DspAudioToMidiTrigger;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterBasics;

type
  TAudio2MidiTriggerFlag = (amFilterBypass, amFilterOutput);
  TAudio2MidiTriggerFlags = set of TAudio2MidiTriggerFlag;

  TTriggerNotifyEvent = procedure(Sender: TObject; const Level: Single) of object;

  TCustomAudio2MidiTrigger = class(TDspSampleRatePersistent, IDspProcessor32,
    IDspProcessor64)
  private
    function GetFilterCount: Integer;
    function GetFilter(Index: Integer): TCustomFilter;
    procedure CalculateReciprocalSamplerate;
    procedure CalculateThresholdFactor;
    procedure SetFlags(const Value: TAudio2MidiTriggerFlags);
    procedure SetThreshold(const Value: Double);
    procedure SetInterval(const Value: Double);
  protected
    FInterval         : Double;
    FFilter           : array of TCustomFilter;
    FFlags            : TAudio2MidiTriggerFlags;
    FOnTrigger        : TTriggerNotifyEvent;
    FSampleCount      : Integer;
    FSampleInterval   : Integer;
    FSRR              : Double;
    FThreshold        : Double;
    FThresholdFactor  : Double;
    procedure IntervalChanged; virtual;
    procedure FlagsChanged; virtual;
    procedure SampleRateChanged; override;
    procedure ThresholdChanged; virtual;

    property SampleRateReciprocal: Double read FSRR;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
    function ProcessSample32(Input: Single): Single; virtual;
    function ProcessSample64(Input: Double): Double; virtual;

    procedure AddFilter(const Filter: TCustomFilter); virtual;
    procedure DeleteFilter(const Filter: TCustomFilter); overload; virtual;
    procedure DeleteFilter(const Index: Integer); overload; virtual;

    property Flags: TAudio2MidiTriggerFlags read FFlags write SetFlags default [];
    property FilterCount: Integer read GetFilterCount;
    property Filter[Index: Integer]: TCustomFilter read GetFilter;
    property Interval: Double read FInterval write SetInterval;
    property Threshold: Double read FThreshold write SetThreshold;
    property OnTrigger: TTriggerNotifyEvent read FOnTrigger write FOnTrigger;
  end;

  TAudio2MidiTrigger = class(TCustomAudio2MidiTrigger, IDspProcessor32,
    IDspProcessor64)
  published
    property Flags;       
    property Interval;    // [s]
    property SampleRate;  // [Hz]
    property Threshold;   // [dB]
  end;

implementation

uses
  SysUtils, DAV_Common;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds [%d]';

{ TCustomAudio2MidiTrigger }


constructor TCustomAudio2MidiTrigger.Create;
begin
 inherited;
 FInterval := 20;
 FFlags    := [];
 FThreshold := -20;
 CalculateReciprocalSamplerate;
 CalculateThresholdFactor;
end;

destructor TCustomAudio2MidiTrigger.Destroy;
var
  Band : Integer;
begin
 for Band := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Band])
   then FreeAndNil(FFilter[Band]);
  inherited;
end;

procedure TCustomAudio2MidiTrigger.AddFilter(const Filter: TCustomFilter);
begin
 // make sure a filter is passed
 assert(Filter <> nil);

 // increase size of filter array
 SetLength(FFilter, Length(FFilter) + 1);

 // actually add filter
 FFilter[Length(FFilter) - 1] := Filter;
end;

procedure TCustomAudio2MidiTrigger.DeleteFilter(const Filter: TCustomFilter);
var
  Index : Integer;
begin
 // make sure a filter is passed
 assert(Filter <> nil);

 Index := 0;
 while Index < Length(FFilter) do
  begin
   if Index < Length(FFilter) - 1
    then Move(FFilter[Index + 1], FFilter[Index], (Length(FFilter) - Index - 1) * SizeOf(Pointer));

   // decrease size of filter array
   SetLength(FFilter, Length(FFilter) - 1);
  end;
end;

procedure TCustomAudio2MidiTrigger.DeleteFilter(const Index: Integer);
begin
 if (Index >= 0) and (Index < Length(FFilter)) then
  begin
   if Index < Length(FFilter) - 1
    then Move(FFilter[Index + 1], FFilter[Index], (Length(FFilter) - Index - 1) * SizeOf(Pointer));

   // decrease size of filter array
   SetLength(FFilter, Length(FFilter) - 1);
  end else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TCustomAudio2MidiTrigger.CalculateReciprocalSamplerate;
begin
 FSRR := 1 / SampleRate;
end;

procedure TCustomAudio2MidiTrigger.SetFlags(const Value: TAudio2MidiTriggerFlags);
begin
 if FFlags <> Value then
  begin
   FFlags := Value;
   FlagsChanged;
  end;
end;

procedure TCustomAudio2MidiTrigger.SetInterval(const Value: Double);
begin
 if FInterval <> Value then
  begin
   FInterval := Value;
   IntervalChanged;
  end;
end;

procedure TCustomAudio2MidiTrigger.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   ThresholdChanged;
  end;
end;

procedure TCustomAudio2MidiTrigger.FlagsChanged;
begin
 // eventually change function pointer here!
 Changed; 
end;

function TCustomAudio2MidiTrigger.GetFilter(Index: Integer): TCustomFilter;
begin
 if (Index >= 0) and (Index < Length(FFilter))
  then Result := Filter[Index] 
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TCustomAudio2MidiTrigger.GetFilterCount: Integer;
begin
 Result := Length(FFilter);
end;

procedure TCustomAudio2MidiTrigger.IntervalChanged;
begin
 FSampleInterval := round(Interval * SampleRate);
 Changed; 
end;

procedure TCustomAudio2MidiTrigger.SampleRateChanged;
var
  Band : Integer;
begin
 CalculateReciprocalSamplerate;
 for Band := 0 to Length(FFilter) - 1 do
  if assigned(FFilter[Band]) then FFilter[Band].SampleRate := SampleRate;
 inherited;
end;

procedure TCustomAudio2MidiTrigger.ThresholdChanged;
begin
 CalculateThresholdFactor;
 Changed;
end;

procedure TCustomAudio2MidiTrigger.CalculateThresholdFactor;
begin
 FThresholdFactor := dB_to_Amp(FThreshold);
end;

procedure TCustomAudio2MidiTrigger.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TCustomAudio2MidiTrigger.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TCustomAudio2MidiTrigger.ProcessSample32(Input: Single): Single;
var
  Band : Integer;
begin
 Result := Input;

 // eventually filter audio data
 if not (amFilterBypass in FFlags) then
  for Band := 0 to Length(FFilter) - 1 do
   if assigned(FFilter[Band])
    then Result := FFilter[Band].ProcessSample64(Result);

 // check if interval is over
 if (FSampleCount >= 0) then
  if (abs(Result) > FThresholdFactor) then
   begin
    if assigned(FOnTrigger)
     then FOnTrigger(Self, Amp_to_dB(abs(Result)));

    // reset sample count
    FSampleCount := FSampleInterval;
   end else
  else Dec(FSampleCount);

 // eventually restore original signal
 if not (amFilterOutput in FFlags)
  then Result := Input;
end;

function TCustomAudio2MidiTrigger.ProcessSample64(Input: Double): Double;
var
  Band : Integer;
begin
 Result := Input;

 // eventually filter audio data
 if not (amFilterBypass in FFlags) then
  for Band := 0 to Length(FFilter) - 1 do
   if assigned(FFilter[Band])
    then Result := FFilter[Band].ProcessSample64(Result);

 // check if interval is over
 if (FSampleCount >= 0) then
  if (abs(Result) > FThresholdFactor) then
   begin
    if assigned(FOnTrigger)
     then FOnTrigger(Self, Amp_to_dB(abs(Result)));

    // reset sample count
    FSampleCount := FSampleInterval;
   end else
  else Dec(FSampleCount);

 // eventually restore original signal
 if not (amFilterOutput in FFlags)
  then Result := Input;
end;

initialization
  RegisterDspProcessor32(TAudio2MidiTrigger);
  RegisterDspProcessor64(TAudio2MidiTrigger);

end.
