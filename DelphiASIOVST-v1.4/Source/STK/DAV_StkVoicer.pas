unit DAV_StkVoicer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK voice manager class.

  This class can be used to manage a group of STK instrument classes.
  Individual voices can be controlled via unique note tags. Instrument groups
  can be controlled by channel number.

  A previously constructed STK instrument class is linked with a voice manager
  using the addInstrument function. An optional channel number argument can be
  specified to the addInstrument function as well (default channel = 0).
  The voice manager does not delete any instrument instances ... it is the
  responsibility of the user to allocate and deallocate all instruments.

  The tick function returns the mix of all sounding voices. Each noteOn
  returns a unique Tag (credits to the NeXT MusicKit), so you can send
  control changes to specific voices within an ensemble. Alternately, control
  changes can be sent to all voices on a given Channel.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Types, DAV_StkCommon, DAV_StkInstrument, Math;

type
  TStkVoice = class
    Instrument : TStkControlableInstrument;
    Tag        : Integer;
    NoteNumber : Single;
    Frequency  : Single;
    Sounding   : Integer;
    Channel    : Integer;
  end;

  TStkVoiceManager = class(TStk)
  private
    function GetVoiceCount: Integer;
  protected
    FMaxVoiceCount : Integer;
    FVoices        : array of TStkVoice;
    FMutetime      : Integer;
    FTags          : Integer;
    FLastOutput    : Single;
    FScale         : Single;
    procedure SampleRateChanged; override;
  public
    // Class constructor taking the maximum number of instruments to control and an optional note decay time (in seconds).
    constructor Create(const SampleRate: Single; const MaxInstruments: Integer;
      const DecayTime: Single = 0.2); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Add an Instrument with an optional channel number to the voice manager.
  {
    A set of instruments can be grouped by channel number and
    controlled via the functions which take a channel number argument.
  }
    procedure AddInstrument(const Instrument: TStkControlableInstrument; const Channel: Integer = 0);

    // Remove the given Instrument pointer from the voice manager's control.
  {
    It is important that any instruments which are to be deleted by
    the user while the voice manager is running be first removed from
    the manager's control via this function!!
   }
    procedure RemoveInstrument(const Instrument: TStkControlableInstrument);

    // Initiate a noteOn event with the given note number and amplitude and result:= a unique note Tag.
  {
    Send the noteOn message to the first available unused voice.
    If all voices are Sounding, the oldest voice is interrupted and
    sent the noteOn message.  If the optional channel argument is
    non-zero, only voices on that channel are used.  If no voices are
    found for a specified non-zero channel value, the function returns
    -1.  The amplitude value should be in the range 0.0 - 1.0.
  }
    function NoteOn(const NoteNumber, Amplitude: Single; const Channel: Integer = 0): Integer;

    // Send a noteOff to all voices having the given NoteNumber and optional channel (default Channel:=0).
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure NoteOff(const NoteNumber, Amplitude: Single; const Channel: Integer = 0); overload;

    // Send a noteOff to the TStkVoice with the given note Tag.
  {
    The amplitude value should be in the range 0.0 - 1.0.
  }
    procedure NoteOff(const Tag: Integer; const Amplitude: Single); overload;

    // Send a Frequency update message to all voices assigned to the optional channel argument (default Channel:=0).
  {
    The \e NoteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure SetFrequency(const NoteNumber: Single; const Channel: Integer = 0); overload;

    // Send a Frequency update message to the TStkVoice with the given note Tag.
  {
    The \e NoteNumber argument corresponds to a MIDI note number, though it is a floating-point value and can range beyond the normal 0-127 range.
  }
    procedure SetFrequency(const Tag: Integer; const NoteNumber: Single); overload;

    // Send a pitchBend message to all voices assigned to the optional channel argument (default channel = 0).
    procedure PitchBend(const Value: Single; const Channel: Integer = 0); overload;

    // Send a pitchBend message to the voice with the given note Tag.
    procedure PitchBend(const Tag: Integer; const Value: Single); overload;

    // Send a controlChange to all instruments assigned to the optional channel argument (default channel = 0).
    procedure ControlChange(const Number: Integer; const Value: Single;
      const Channel: Integer = 0); overload;

    // Send a controlChange to the voice with the given note tag.
    procedure ControlChange(const Tag, Number: Integer; const Value: Single); overload;

    // Send a noteOff message to all existing voices.
    procedure Silence;

    // Mix the output for all sounding voices.
    function Tick: Single; overload;

    // Processes 'SampleFrames' samples in-place
    procedure Tick(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;

    property LastOutput: Single read FLastOutput;
    property VoiceCount: Integer read GetVoiceCount;
    property MaxVoiceCount: Integer read FMaxVoiceCount; 
  end;

implementation

uses
  SysUtils;

{ TStkVoiceManager }

constructor TStkVoiceManager.Create(const SampleRate: Single; const MaxInstruments: Integer;
  const DecayTime: Single);
begin
  inherited Create(SampleRate);
  SetLength(FVoices, 0);
  FMaxVoiceCount := MaxInstruments;
  FTags := 0;
  FMutetime := Round(DecayTime * SampleRate);
end;

destructor TStkVoiceManager.Destroy;
begin
  inherited Destroy;
end;

function TStkVoiceManager.GetVoiceCount: Integer;
begin
 result := Length(FVoices);
end;

procedure TStkVoiceManager.AddInstrument(const Instrument: TStkControlableInstrument; const Channel: Integer);
var
  Voice: Integer;
begin
  if (Length(FVoices) >= FMaxVoiceCount) then exit;

  Voice := Length(FVoices);
  SetLength(FVoices, Length(FVoices) + 1);
  FVoices[Voice] := TStkVoice.Create;
  FVoices[Voice].Instrument := Instrument;
  FVoices[Voice].Tag := 0;
  FVoices[Voice].Channel := Channel;
  FVoices[Voice].NoteNumber := -1;
  FVoices[Voice].Frequency := 0.0;
  FVoices[Voice].Sounding := 0;

  FScale := 1 / Length(FVoices);
end;

procedure TStkVoiceManager.RemoveInstrument(const Instrument: TStkControlableInstrument);
var
  Voice : Integer;
begin
 for Voice := 0 to Length(FVoices) - 1 do
  if (FVoices[Voice].Instrument = Instrument) then
   begin
    FreeAndNil(FVoices[Voice]);
    if (Voice + 1 < Length(FVoices))
     then Move(FVoices[Voice + 1], FVoices[Voice], (Length(FVoices) - Voice - 1) * SizeOf(Pointer));
    SetLength(FVoices, Length(FVoices) - 1);
    break;
   end;
end;

procedure TStkVoiceManager.ControlChange(const Number: Integer; const Value: Single;
  const Channel: Integer = 0);
var
  Voice: Integer;
begin
 for Voice := 0 to Length(FVoices) - 1 do
  if (FVoices[Voice].Channel = Channel)
   then FVoices[Voice].Instrument.ControlChange(Number, Value);
end;

procedure TStkVoiceManager.ControlChange(const Tag, Number: Integer; const Value: Single);
var
  Voice: Integer;
begin
 for Voice := 0 to Length(FVoices) - 1 do
  if (FVoices[Voice].Tag = Tag) then
   begin
    FVoices[Voice].Instrument.ControlChange(Number, Value);
    Break;
   end;
end;

procedure TStkVoiceManager.NoteOff(const Tag: Integer; const Amplitude: Single);
var
  Voice: Integer;
begin
 for Voice := 0 to Length(FVoices) - 1 do
  if (FVoices[Voice].Tag = Tag) then
   begin
    FVoices[Voice].Instrument.NoteOff(Amplitude);
    FVoices[Voice].Sounding := -FMutetime;
    Break;
   end;
end;

procedure TStkVoiceManager.NoteOff(const NoteNumber, Amplitude: Single; const Channel: Integer);
var
  Voice: Integer;
begin
 for Voice := 0 to Length(FVoices) - 1 do
  if (FVoices[Voice].NoteNumber = NoteNumber) and (FVoices[Voice].Channel = Channel) then
   begin
    FVoices[Voice].Instrument.NoteOff(Amplitude);
    FVoices[Voice].Sounding := -FMuteTime;
   end;
end;

function TStkVoiceManager.NoteOn(const NoteNumber, Amplitude: Single;
  const Channel: Integer): Integer;
var
  OldVoice  : Integer;
  Voice     : Integer;
  Frequency : Single;
begin
  // calculate frequency
  Frequency := 220.0 * Power(2.0, (NoteNumber - 57.0) * COneTwelfth32);

  for Voice := 0 to Length(FVoices) - 1 do
   if (FVoices[Voice].NoteNumber < 0) and (FVoices[Voice].Channel = Channel) then
    begin
     FVoices[Voice].Tag := FTags;
     Inc(FTags);
     FVoices[Voice].Channel := Channel;
     FVoices[Voice].NoteNumber := NoteNumber;
     FVoices[Voice].Frequency := Frequency;
     FVoices[Voice].Instrument.NoteOn(Frequency, Amplitude);
     FVoices[Voice].Sounding := 1;
     Result := FVoices[Voice].Tag;
     exit;
    end;

  // All voices are sounding, so interrupt the oldest voice.
  OldVoice := -1;
  for Voice := 0 to Length(FVoices) - 1 do
   if (FVoices[Voice].Channel = Channel) then
    if (OldVoice = -1) then OldVoice := Voice else
     if (FVoices[Voice].Tag < FVoices[OldVoice].Tag)
      then OldVoice := Voice;

  if (OldVoice >= 0) then
   begin
    FVoices[OldVoice].Tag := FTags;
    Inc(FTags);
    FVoices[OldVoice].Channel := Channel;
    FVoices[OldVoice].NoteNumber := NoteNumber;
    FVoices[OldVoice].Frequency := Frequency;
    FVoices[OldVoice].Instrument.NoteOn(Frequency, Amplitude);
    FVoices[OldVoice].Sounding := 1;
    Result := FVoices[OldVoice].Tag;
    exit;
   end;
  Result := -1;
end;

procedure TStkVoiceManager.PitchBend(const Tag: Integer; const Value: Single);
var
  PitchScaler : Single;
  Voice       : Integer;
begin
 PitchScaler := Power(2.0, Value * 2 - 1);
 for Voice := 0 to Length(FVoices) - 1 do
  if (FVoices[Voice].Tag = Tag) then
   begin
    FVoices[Voice].Instrument.Frequency := FVoices[Voice].Frequency * PitchScaler;
    Break;
   end;
end;

procedure TStkVoiceManager.PitchBend(const Value: Single; const Channel: Integer);
var
  PitchScaler : Single;
  Voice       : Integer;
begin
 PitchScaler := Power(2.0, Value * 2 - 1);
 for Voice := 0 to Length(FVoices) - 1 do
  if (FVoices[Voice].Channel = Channel)
   then FVoices[Voice].Instrument.Frequency := FVoices[Voice].Frequency * PitchScaler;
end;

procedure TStkVoiceManager.SampleRateChanged;
var
  Voice : Integer;
begin
  for Voice := 0 to Length(FVoices) - 1
   do FVoices[Voice].Instrument.SampleRate := SampleRate;
end;

procedure TStkVoiceManager.SetFrequency(const Tag: Integer; const NoteNumber: Single);
var
  Frequency : Single;
  Voice     : Integer;
begin
  Frequency := 220.0 * Power(2.0, (NoteNumber - 57.0) * COneTwelfth32);
  for Voice := 0 to Length(FVoices) - 1 do
    if (FVoices[Voice].Tag = Tag) then
     begin
      FVoices[Voice].NoteNumber := NoteNumber;
      FVoices[Voice].Frequency := Frequency;
      FVoices[Voice].Instrument.Frequency := Frequency;
      Break;
     end;
end;

procedure TStkVoiceManager.SetFrequency(const NoteNumber: Single; const Channel: Integer);
var
  Frequency: Single;
  i: Integer;
begin
  Frequency := 220.0 * Power(2.0, (NoteNumber - 57.0) * COneTwelfth32);
  for i := 0 to Length(FVoices) - 1 do
    if (FVoices[i].Channel = Channel) then
     begin
      FVoices[i].NoteNumber := NoteNumber;
      FVoices[i].Frequency := Frequency;
      FVoices[i].Instrument.Frequency := Frequency;
     end;
end;

procedure TStkVoiceManager.Silence;
var
  i: Integer;
begin
  for i := 0 to Length(FVoices) - 1 do
    if (FVoices[i].Sounding > 0) then
      FVoices[i].Instrument.NoteOff(0.5);
end;

procedure TStkVoiceManager.Tick(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleFrames - 1 do Data^[Sample] := Tick;
end;

function TStkVoiceManager.Tick: Single;
var
  i: Integer;
begin
  FLastOutput := 0.0;
  for i := 0 to Length(FVoices) - 1 do
   begin
    if (FVoices[i].Sounding <> 0)
     then FLastOutput := FLastOutput + FVoices[i].Instrument.Tick;
    if (FVoices[i].Sounding < 0) then
     begin
      FVoices[i].Sounding := FVoices[i].Sounding + 1;
      if (FVoices[i].Sounding = 0)
       then FVoices[i].NoteNumber := -1;
     end;
   end;
  Result := FLastOutput * FScale;
end;

end.
