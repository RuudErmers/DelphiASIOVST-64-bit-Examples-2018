{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_MidiFile;

//  This code was originally written by F.Bouwmans and has been adapted
//  for this project by Tobias Fleischer and Christian-W. Budde.
//  see comments below for more information

interface

{$I DAV_Compiler.inc}

{
  Load a MidiFile and get access to Tracks and Events
  I did build this component to convert MidiFiles to wave files
  or play the files on a software synthesizer which I'm currenly
  building.

  version 1.0 first release

  version 1.1
    added some function
    function KeyToStr(Key: Integer): string;
    function MyTimeToStr(Value: Integer): string;
    Bpm can be set to change speed

  version 1.2
    added some functions
    function GetTrackLength: Integer;
    function Ready: Boolean;

  version 1.3
    update by Chulwoong,
    He knows how to use the MM timer, the timing is much better now, thank you

  version 1.4
    update by Tobybear,
    Rewrote component to allow usage in DLLs,
    timing improvements, some additional functions...

  for comments/bugs
  F.Bouwmans
  fbouwmans@spiditel.nl

  If you think this component is nice and you use it, sent me a short email.
  I've seen that other of my components have been downloaded a lot, but I've
  got no clue wether they are actually used.
  Don't worry because you are free to use these components

  Timing has improved, however because the messages are handled by the normal
  windows message loop (of the main window) it is still influenced by actions
  done on the window (minimize/maximize ..).
  Use of a second thread with higher priority which only handles the
  timer message should increase performance. If somebody knows such a component
  which is freeware please let me know.

  interface description:

  procedure ReadFile:
    actually read the file which is set in Filename

  function GetTrack(index: Integer): TMidiTrack;

  property Filename
    set/read filename of FMidiFile

  property NumberOfTracks
    read number of FTracks in current file

  property TicksPerQuarter: Integer
    ticks per quarter, tells how to interpret the Time value in midi FEvents

  property FileFormat: TFileFormat
    tells the format of the current FMidiFile

  property Bpm:Integer
    tells Beats per minut

  property OnMidiEvent:TOnMidiEvent
    called while playing for each midi Event

  procedure StartPlaying;
    start playing the current loaded midi file from the beginning

  procedure StopPlaying;
    stop playing the current midi file

  procedure PlayToTime(Time : Integer);
    if playing yourSelf then FEvents from last Time to this Time are produced


  function KeyToStr(Key: Integer): string;
      give note string on key value:  e.g. C4

  function MyTimeToStr(Value: Integer): string;
      give Time string from msec Time

  function  GetTrackLength:Integer;
      gives the track lenght in msec (assuming the bpm at the start oof the file)

  function  Ready: Boolean;
      now you can check wether the playback is finished

}

uses
  {$IFDEF FPC}LCLIntf, LMessages, {$ELSE}Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Stdctrls, ExtCtrls;

type
  TChunkType = (ctIllegal, ctHeader, ctTrack);
  TFileFormat = (ffSingleSynch, ffMultiSynch, ffMultiAsynch);
  PByte = ^Byte;

  TMidiEvent = record
    Event   : Byte;
    Data1   : Byte;
    Data2   : Byte;
    Str     : String;
    DTicks  : Integer;
    Time    : Integer;
    MTime   : Integer;
    Len     : Integer;
  end;
  PMidiEvent = ^TMidiEvent;

  TOnMidiEvent = procedure(Event: PMidiEvent) of object;
  TEvent = procedure of object;

  TMidiTrack = class(TObject)
  protected
    FEvents       : TList;
    FName         : string;
    FInstrument   : string;
    FCurrentTime  : Integer;
    FCurrentPos   : Integer;
    FReady        : Boolean;
    FTrackLenght  : Integer;
    FOnMidiEvent  : TOnMidiEvent;
    FOnTrackReady : TEvent;
    procedure CheckReady;
    function GetEventCount: Integer;
    function GetTrackLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Rewind(const pos: Integer);
    procedure PlayUntil(const pos: Integer);
    procedure GoUntil(const pos: Integer);

    procedure PutEvent(Event: PMidiEvent);
    function GetEvent(const index: Integer): PMidiEvent;
  public
    property Name: string read FName;
    property Instrument: string read FInstrument;
    property EventCount: Integer read GetEventCount;
    property CurrentTime: Integer read FCurrentTime;
    property TrackLength: Integer read GetTrackLength;
    property IsReady: Boolean read FReady;
    property OnMidiEvent: TOnMidiEvent read FOnMidiEvent write FOnMidiEvent;
    property OnTrackReady: TEvent read FOnTrackReady write FOnTrackReady;
  end;

  TMidiFile = class(TComponent)
  private
    FManual: Boolean;
    procedure SetManual(const Value: Boolean);
  protected
    FMidiTimer       : TTimer;
    FMidiFileHandle  : THandle;

    FMidiFile        : File of Byte;
    FChunkType       : TChunkType;
    FChunkLength     : Integer;
    FChunkData       : PByte;
    FChunkIndex      : PByte;
    FChunkEnd        : PByte;
    FPriority        : DWORD;

    FBpmOld          : Integer;

    // midi file attributes
    FFileFormat      : TFileFormat;
    FNumberTracks    : Integer;
    FDeltaTicks      : Integer;
    FBPM             : Integer;
    FBeatsPerMeasure : Integer;
    FFusPerTick      : Double;
    FFilename        : string;

    FTracks          : TList;
    FCurrentTrack    : TMidiTrack;
    FOnMidiEvent     : TOnMidiEvent;
    FOnUpdateEvent   : TNotifyEvent;

    // playing attributes
    FPlaying         : Boolean;
    FPlayStartTime   : Integer;
    FCurrentTime     : Integer; // Current playtime in msec
    FCurrentPos      : Double;  // Current Position in ticks

    procedure OnTrackReady;
    procedure SetFileName(Value: string);
    procedure ReadChunkHeader;
    procedure ReadChunkContent;
    procedure ReadChunk;
    procedure ProcessHeaderChunk;
    procedure ProcessTrackChunk;
    function ReadVarLength: Integer;
    function ReadString(Index: Integer): string;
    procedure SetOnMidiEvent(handler: TOnMidiEvent);
    procedure SetBpm(Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MidiTimer(sender : TObject);

    procedure ReadFile;
    function GetTrack(index: Integer): TMidiTrack;
    function GetTrackLength2: Integer;

    procedure StartPlaying;
    procedure StopPlaying;
    procedure ContinuePlaying;

    procedure PlayToTime(Time: Integer);
    procedure GoToTime(Time: Integer);
    function GetCurrentTime: Integer;
    function GetCurrentPos: Double;
    function GetFusPerTick : Double;
    function GetTrackLength: Integer;
    function Ready: Boolean;
  published
    property ManualCall: Boolean read FManual write SetManual;
    property Filename: string read FFilename write SetFileName;
    property NumberOfTracks: Integer read FNumberTracks;
    property TicksPerQuarter: Integer read FDeltaTicks;
    property FileFormat: TFileFormat read FFileFormat;
    property Playing: Boolean read FPlaying;
    property Bpm: Integer read FBPM write SetBpm;

    property OnMidiEvent: TOnMidiEvent read FOnMidiEvent write SetOnMidiEvent;
    property OnUpdateEvent: TNotifyEvent read FOnUpdateEvent write FOnUpdateEvent;
  end;

function KeyToStr(const Key: Integer): string;
function MyTimeToStr(const Value: Integer): string;

implementation

uses
  MMSystem;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

{ TMidiTrack }

constructor TMidiTrack.Create;
begin
 inherited Create;
 FEvents := TList.Create;
 FCurrentTime := 0;
 FCurrentPos := 0;
end;

destructor TMidiTrack.Destroy;
var
  i: Integer;
begin
  for i := 0 to FEvents.Count - 1 do
    Dispose(PMidiEvent(FEvents.items[i]));
  FEvents.Free;
  inherited Destroy;
end;

procedure TMidiTRack.PutEvent(Event: PMidiEvent);
var
  command : Integer;
  i       : Integer;
  pevent  : PMidiEvent;
begin
  if (Event.Event = $FF) then
   begin
    if (Event.Data1 = 3) then FName := Event.Str;
    if (Event.Data1 = 4) then FInstrument := Event.Str;
   end;
  FCurrentTime := FCurrentTime + Event.DTicks;
  Event.Time := FCurrentTime; // for the moment just add DTicks
  Event.Len := 0;
  FEvents.Add(TObject(Event));
  command := Event.Event and $F0;

  if ((command = $80) // note off
    or ((command = $90) and (Event.Data2 = 0))) //note on with speed 0
  then
  begin
    // this is a note off, try to find the accompanion note on
    command := Event.Event or $90;
    i := FEvents.Count - 2;
    while i >= 0 do
    begin
      pevent := PMidiEvent(FEvents[i]);
      if (pevent.Event = command) and
        (pevent.Data1 = Event.Data1)
        then
      begin
        pevent.Len := FCurrentTime - pevent.Time;
        i := 0;
        Event.Len := -1;
      end;
      dec(i);
    end;
  end;
end;

function TMidiTrack.GetEventCount: Integer;
begin
  result := FEvents.Count;
end;

function TMidiTrack.GetEvent(const index: Integer): PMidiEvent;
begin
  if ((index < FEvents.Count) and (index >= 0))
   then result := FEvents[index]
   else result := nil;
end;

procedure TMidiTrack.Rewind(const Pos: Integer);
begin
 if FCurrentPos = FEvents.Count then dec(FCurrentPos);
 while ((FCurrentPos > 0) and (PMidiEvent(FEvents[FCurrentPos]).Time > Pos))
  do dec(FCurrentPos);
 CheckReady;
end;

procedure TMidiTrack.PlayUntil(const pos: Integer);
begin
 if assigned(OnMidiEvent) then
  begin
   while ((FCurrentPos < FEvents.Count) and (PMidiEvent(FEvents[FCurrentPos]).Time < pos)) do
    begin
     OnMidiEvent(PMidiEvent(FEvents[FCurrentPos]));
     Inc(FCurrentPos);
    end;
  end;
 CheckReady;
end;

procedure TMidiTrack.GoUntil(const pos: Integer);
begin
 while ((FCurrentPos < FEvents.Count) and (PMidiEvent(FEvents[FCurrentPos]).Time < pos))
  do Inc(FCurrentPos);
 CheckReady;
end;

procedure TMidiTrack.CheckReady;
begin
 if FCurrentPos >= FEvents.Count then
  begin
   FReady := True;
   if assigned(OnTrackReady)
    then OnTrackReady;
  end
 else FReady := False;
end;

function TMidiTrack.GetTrackLength: Integer;
begin
  result := PMidiEvent(FEvents[FEvents.Count-1]).Time
end;

constructor TMidiFile.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FManual := False;
 FChunkData := nil;
 FChunkType := ctIllegal;
 FTracks := TList.Create;

 FMidiTimer := TTimer.Create(nil);
 FMidiTimer.Interval := 2;
 FMidiTimer.OnTimer := MidiTimer;
 FMidiTimer.Enabled := True;
end;

destructor TMidiFile.Destroy;
var
  Track: Integer;
begin
 FreeAndNil(FMidiTimer);

 if assigned(FChunkData)
  then Dispose(FChunkData);

 for Track := 0 to FTracks.Count - 1
  do TMidiTrack(FTracks.Items[Track]).Free;

 FreeAndNil(FTracks);

 inherited;
end;

function TMidiFile.GetTrack(Index: Integer): TMidiTrack;
begin
 if (Index >= 0) and (Index < FTracks.Count)
  then result := FTracks.Items[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TMidiFile.SetFileName(Value: string);
begin
  FFilename := Value;
//  ReadFile;
end;

procedure TMidiFile.SetOnMidiEvent(Handler: TOnMidiEvent);
var
  i: Integer;
begin
//  if not (FOnMidiEvent = Handler) then
//  begin
  FOnMidiEvent := Handler;
  for i := 0 to FTracks.Count - 1
   do TMidiTrack(FTracks.items[i]).OnMidiEvent := Handler;
//  end;
end;

procedure TMidiFile.MidiTimer(Sender: TObject);
begin
 if FPlaying then
  begin
   PlayToTime(GetTickCount - Cardinal(FPlayStartTime));
   if assigned(FOnUpdateEvent) then FOnUpdateEvent(Self);
  end;
end;

procedure TMidiFile.StartPlaying;
var
  i: Integer;
begin
  for i := 0 to FTracks.Count - 1 do TMidiTrack(FTracks[i]).Rewind(0);
  FPlayStartTime := getTickCount;
  FPlaying := True;
  if not FManual then FMidiTimer.Enabled := True;
  FCurrentPos := 0.0;
  FCurrentTime := 0;
end;

procedure TMidiFile.ContinuePlaying;
begin
 FPlayStartTime := GetTickCount - Cardinal(FCurrentTime);
 FPlaying := True;
 if not FManual then FMidiTimer.Enabled := True;
end;

procedure TMidiFile.StopPlaying;
var
  i: Integer;
begin
 for i := 0 to FTracks.Count - 1
  do TMidiTrack(FTracks.items[i]).Rewind(0);

 FPlaying := False;
 FMidiTimer.Enabled := False;
//  KillMIDITimer;
//  SetPriorityClass(FMidiFileHandle, FPriority);
end;

function TMidiFile.GetCurrentTime: Integer;
begin
  Result := FCurrentTime;
end;

procedure TMidiFile.PlayToTime(Time: Integer);
var
  i         : Integer;
  pos       : Integer;
  deltaTime : Integer;
begin
 // calculate the pos in the file.
 // pos is actually tick
 // Current FFusPerTick is uses to determine the actual pos
 deltaTime := Time - FCurrentTime;
 FCurrentPos := FCurrentPos + (deltaTime * 1000) / FFusPerTick;
 pos := round(FCurrentPos);
 for i := 0 to FTracks.Count - 1
  do TMidiTrack(FTracks.items[i]).PlayUntil(pos);
 FCurrentTime := Time;
end;

procedure TMidiFile.GoToTime(Time: Integer);
var
  i   : Integer;
  pos : Integer;
begin
 // this function should be changed because FFusPerTick might not be constant
 pos := round((Time * 1000) / FFusPerTick);
 for i := 0 to FTracks.Count - 1 do
  with TMidiTrack(FTracks.items[i]) do
   begin
    Rewind(0);
    GoUntil(pos);
   end;
 FCurrentTime := Time;
end;

procedure TMidiFile.SetBpm(Value: Integer);
var
  us_per_quarter: Integer;
begin
 if not (Value = FBPM) then
  begin
   us_per_quarter := 60000000 div Value;
   FBPM := 60000000 div us_per_quarter;
   FFusPerTick := us_per_quarter / FDeltaTicks;
  end;
end;

procedure TMidiFile.ReadChunkHeader;
var
  theByte: array[0..7] of Byte;
begin
 BlockRead(FMidiFile, theByte, 8);
 if (theByte[0] = $4D) and (theByte[1] = $54) then
  begin
   if (theByte[2] = $68) and (theByte[3] = $64)
    then FChunkType := ctHeader else
   if (theByte[2] = $72) and (theByte[3] = $6B)
    then FChunkType := ctTrack
    else FChunkType := ctIllegal;
  end
 else FChunkType := ctIllegal;
 FChunkLength := theByte[7] + theByte[6] * $100 + theByte[5] * $10000 + theByte[4] * $1000000;
end;

procedure TMidiFile.ReadChunkContent;
begin
 if not (FChunkData = nil) then FreeMem(FChunkData);
 GetMem(FChunkData, FChunkLength + 10);
 BlockRead(FMidiFile, FChunkData^, FChunkLength);
 FChunkIndex := FChunkData;
 FChunkEnd := PByte(Integer(FChunkIndex) + Integer(FChunkLength) - 1);
end;

procedure TMidiFile.ReadChunk;
begin
 ReadChunkHeader;
 ReadChunkContent;
 case FChunkType of
  ctHeader : ProcessHeaderChunk;
   ctTrack : ProcessTrackChunk;
 end;
end;

procedure TMidiFile.ProcessHeaderChunk;
begin
 FChunkIndex := FChunkData;
 Inc(FChunkIndex);
 if FChunkType = ctHeader then
  begin
   case FChunkIndex^ of
    0: FFileFormat := ffSingleSynch;
    1: FFileFormat := ffMultiSynch;
    2: FFileFormat := ffMultiAsynch;
   end;
   Inc(FChunkIndex);
   FNumberTracks := FChunkIndex^ * $100;
   Inc(FChunkIndex);
   FNumberTracks := FNumberTracks + FChunkIndex^;
   Inc(FChunkIndex);
   FDeltaTicks := FChunkIndex^ * $100;
   Inc(FChunkIndex);
   FDeltaTicks := FDeltaTicks + FChunkIndex^;
  end;
end;

procedure TMidiFile.ProcessTrackChunk;
var
  dTime          : Integer;
  Event          : Integer;
  Len            : Integer;
  midiEvent      : PMidiEvent;
  us_per_quarter : Integer;
begin
  FChunkIndex := FChunkData;
//  Inc(FChunkIndex);
  Event := 0;
  if FChunkType = ctTrack then
   begin
    FCurrentTrack := TMidiTrack.Create;
    FCurrentTrack.OnMidiEvent := FOnMidiEvent;
    FTracks.add(FCurrentTrack);
    while Integer(FChunkIndex) < Integer(FChunkEnd) do
     begin
      // each Event starts with var length delta Time
      dTime := ReadVarLength;
      if FChunkIndex^ >= $80 then
       begin
        Event := FChunkIndex^;
        Inc(FChunkIndex);
       end;
      // else it is a running status Event (just the same Event as before)

      if Event = $FF then
       begin
{        case FChunkIndex^ of
        $00: // sequence number, not implemented jet
            begin
              Inc(FChunkIndex); // $02
              Inc(FChunkIndex);
            end;
        $01 .. $0f: // text FEvents  FF ty Len text
            begin
              New(midiEvent);
              midiEvent.Event := $FF;
              midiEvent.Data1 := FChunkIndex^;     // type is stored in Data1
              midiEvent.DTicks := dtime;

              Inc(FChunkIndex);
              Len := ReadVarLength;
              midiEvent.Str    := ReadString(Len);

              FCurrentTrack.PutEvent(midiEvent);
            end;
        $20: // Midi channel prefix  FF 20 01 cc
             begin
               Inc(FChunkIndex); // $01
               Inc(FChunkIndex); // channel
               Inc(FChunkIndex);
             end;
        $2F: // End of ctTrack FF 2F 00
             begin
               Inc(FChunkIndex); // $00
               Inc(FChunkIndex);
             end;
        $51: // Set Tempo  FF 51 03 tttttt
             begin
               Inc(FChunkIndex); // $03
               Inc(FChunkIndex); // tt
               Inc(FChunkIndex); // tt
               Inc(FChunkIndex); // tt
               Inc(FChunkIndex);
             end;
        $54: // SMPTE offset  FF 54 05 hr mn se fr ff
             begin
               Inc(FChunkIndex); // $05
               Inc(FChunkIndex); // hr
               Inc(FChunkIndex); // mn
               Inc(FChunkIndex); // se
               Inc(FChunkIndex); // fr
               Inc(FChunkIndex); // ff
               Inc(FChunkIndex);
             end;
        $58: // Time signature FF 58 04 nn dd cc bb
             begin
               Inc(FChunkIndex); // $04
               Inc(FChunkIndex); // nn
               Inc(FChunkIndex); // dd
               Inc(FChunkIndex); // cc
               Inc(FChunkIndex); // bb
               Inc(FChunkIndex);
             end;
        $59: // Key signature FF 59 02 df mi
             begin
               Inc(FChunkIndex); // $02
               Inc(FChunkIndex); // df
               Inc(FChunkIndex); // mi
               Inc(FChunkIndex);
             end;
        $7F: // Sequence specific Meta-event
            begin
              Inc(FChunkIndex);
              Len := ReadVarLength;
              Str := ReadString(Len);
            end;
        else // unknown meta Event
        }
         begin
          New(midiEvent);
          midiEvent.Event := $FF;
          midiEvent.Data1 := FChunkIndex^; // type is stored in Data1
          midiEvent.DTicks := dtime;

          Inc(FChunkIndex);
          Len := ReadVarLength;
          midiEvent.Str := ReadString(Len);
          FCurrentTrack.PutEvent(midiEvent);

          case midiEvent.Data1 of
            $51:
              begin
                us_per_quarter :=
                  (Integer(Byte(midiEvent.Str[1])) shl 16 +
                  Integer(Byte(midiEvent.Str[2])) shl 8 +
                  Integer(Byte(midiEvent.Str[3])));
                FBPM := 60000000 div us_per_quarter;
                FBpmOld := FBPM;
                FFusPerTick := us_per_quarter / FDeltaTicks;
              end;
          end;
        end;
//        end;
       end
      else
       begin
      // these are all midi FEvents
        New(midiEvent);
        midiEvent.Event := Event;
        midiEvent.DTicks := dtime;
//         Inc(FChunkIndex);
        case Event of
          $80..$8F, // note off
          $90..$9F, // note on
          $A0..$AF, // key aftertouch
          $B0..$BF, // control change
          $E0..$EF: // pitch wheel change
            begin
              midiEvent.Data1 := FChunkIndex^; Inc(FChunkIndex);
              midiEvent.Data2 := FChunkIndex^; Inc(FChunkIndex);
            end;
          $C0..$CF, // program change
          $D0..$DF: // channel aftertouch
            begin
              midiEvent.Data1 := FChunkIndex^; Inc(FChunkIndex);
            end;
        else
           // error
        end;
        FCurrentTrack.PutEvent(midiEvent);
       end;
     end;
   end;
end;


function TMidiFile.ReadVarLength: Integer;
var
  i: Integer;
  b: Byte;
begin
 b := 128;
 i := 0;
 while b > 127 do
  begin
   i := i shl 7;
   b := FChunkIndex^;
   i := i + b and $7F;
   Inc(FChunkIndex);
  end;
 result := i;
end;

function TMidiFile.ReadString(Index: Integer): string;
var
  s: PChar;
  i: Integer;
begin
 GetMem(s, Index + 1); ;
 s[Index] := chr(0);
 for i := 0 to Index - 1 do
  begin
   s[i] := Chr(FChunkIndex^);
   Inc(FChunkIndex);
  end;
 result := string(s);
end;

procedure TMidiFile.ReadFile;
var
  Track: Integer;
begin
  for Track := 0 to FTracks.Count - 1
   do TMidiTrack(FTracks.Items[Track]).Free;
  FTracks.Clear;
  FChunkType := ctIllegal;

  AssignFile(FMidiFile, FFilename);
  FileMode := 0;
  Reset(FMidiFile);
  while not EoF(FMidiFile) do ReadChunk;
  CloseFile(FMidiFile);
  FNumberTracks := FTracks.Count;
end;

function KeyToStr(const Key: Integer): string;
var
  n   : Integer;
  Str : string;
begin
  n := key mod 12;
  case n of
    0: Str := 'C';
    1: Str := 'C#';
    2: Str := 'D';
    3: Str := 'D#';
    4: Str := 'E';
    5: Str := 'F';
    6: Str := 'F#';
    7: Str := 'G';
    8: Str := 'G#';
    9: Str := 'A';
   10: Str := 'A#';
   11: Str := 'B';
  end;
  Result := Str + IntToStr(key div 12);
end;

function IntToLenStr(val: Integer; Len: Integer): string;
var
  Str: string;
begin
  Str := IntToStr(val);
  while Length(Str) < Len do Str := '0' + Str;
  Result := Str;
end;

function MyTimeToStr(const Value: Integer): string;
var
  hour : Integer;
  min  : Integer;
  sec  : Integer;
  msec : Integer;
begin
  msec := Value mod 1000;
  sec  := Value div 1000;
  min  := sec div 60;
  sec  := sec mod 60;
  hour := min div 60;
  min  := min mod 60;
  Result := IntToStr(hour) + ':' + IntToLenStr(min, 2) + ':' +
            IntToLenStr(sec, 2) + '.' + IntToLenStr(msec, 3);
end;

function TMidiFile.GetFusPerTick: Double;
begin
 Result := FFusPerTick;
end;

function  TMidiFile.GetTrackLength: Integer;
var
  i, length : Integer;
  Time      : Extended;
begin
 length := 0;
 for i := 0 to FTracks.Count - 1 do
  if TMidiTrack(FTracks.Items[i]).GetTrackLength > length
   then length := TMidiTrack(FTracks.Items[i]).GetTrackLength;
 Time := length * FFusPerTick;
 Time := Time / 1000.0;
 result := round(Time);
end;

function TMidiFile.GetTrackLength2: Integer;
var
  i, length: Integer;
begin
 length := 0;
 for i := 0 to FTracks.Count - 1 do
  if TMidiTrack(FTracks.Items[i]).GetTrackLength > length
   then length := TMidiTrack(FTracks.Items[i]).GetTrackLength;
 result := length;
end;

function TMidiFile.Ready: Boolean;
var
  i : Integer;
begin
 result := True;
 for i := 0 to FTracks.Count - 1 do
  if not TMidiTrack(FTracks.Items[i]).isready then
    result := False;
end;

procedure TMidiFile.OnTrackReady;
begin
 if Ready then
  if assigned(FOnUpdateEvent) then FOnUpdateEvent(Self);
end;

function TMidiFile.GetCurrentPos: Double;
begin
 result := FCurrentPos;
end;

procedure TMidiFile.SetManual(const Value: Boolean);
begin
 FManual := Value;
 FMidiTimer.Enabled := not Value;
end;

end.
