unit DAV_ModularVoiceList;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_ModularVoice;

type
  TDspVoiceList = class(TList)
  protected
    function Get(Index: Integer): TDspVoice;
    procedure Put(Index: Integer; Item: TDspVoice);
    function GetPlayingVoiceCount: Integer;
  public
    function Add(Item: TDspVoice): Integer;
    function Extract(Item: TDspVoice): TDspVoice;
    function First: TDspVoice;
    function IndexOf(Item: TDspVoice): Integer;
    function Last: TDspVoice;
    function Remove(Item: TDspVoice): Integer;
    procedure Insert(Index: Integer; Item: TDspVoice);

    function GetOldestVoice(OnlyNoteOnVoices: Boolean = true): TDspVoice;
    function GetNearestVoice(KeyNr: Byte; OnlyNoteOnVoices: Boolean = true): TDspVoice;
    function GetVoiceByKey(KeyNr: Byte; OnlyNoteOnVoices: Boolean = true): TDspVoice;

    procedure SetSampleRate(Value: Single);
    procedure SetChannels(Value: Integer);

    procedure ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);

    property Items[Index: Integer]: TDspVoice read Get write Put;
    property PlayingVoiceCount: Integer read GetPlayingVoiceCount;
  end;

implementation

{ TDspVoiceList }

procedure TDspVoiceList.Put(Index: Integer; Item: TDspVoice);
begin
   inherited Put(Index, Item);
end;

function TDspVoiceList.Get(Index: Integer): TDspVoice;
begin
  Result := TDspVoice(inherited Get(Index));
end;

function TDspVoiceList.Add(Item: TDspVoice): Integer;
begin
  Result := inherited Add(Item);
end;

procedure TDspVoiceList.Insert(Index: Integer; Item: TDspVoice);
begin
  inherited Insert(Index, Item);
end;

function TDspVoiceList.Extract(Item: TDspVoice): TDspVoice;
begin
  Result := TDspVoice(inherited Extract(Item));
end;

function TDspVoiceList.First: TDspVoice;
begin
  Result := TDspVoice(inherited First);
end;

function TDspVoiceList.IndexOf(Item: TDspVoice): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TDspVoiceList.Last: TDspVoice;
begin
  Result := TDspVoice(inherited Last);
end;

function TDspVoiceList.Remove(Item: TDspVoice): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TDspVoiceList.SetSampleRate(Value: Single);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].SampleRate := Value;
end;

procedure TDspVoiceList.SetChannels(Value: Integer);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].Channels := Value;
end;

function TDspVoiceList.GetOldestVoice(OnlyNoteOnVoices: Boolean): TDspVoice;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (not OnlyNoteOnVoices or Items[i].IsVoiceNoteOn) then
    begin
      Result := Items[i];
      exit;
    end;
end;

function TDspVoiceList.GetNearestVoice(KeyNr: Byte; OnlyNoteOnVoices: Boolean): TDspVoice;
var
  i: Integer;
begin
  Result := nil;
  for i := Count - 1 downto 0 do
    if (not OnlyNoteOnVoices or Items[i].IsVoiceNoteOn) then
    begin
      if Result=nil then Result := Items[i]
      else if abs(Result.VoiceInfo.NoteNr-KeyNr)>abs(Items[i].VoiceInfo.NoteNr-KeyNr) then Result := Items[i];
    end;
end;

function TDspVoiceList.GetVoiceByKey(KeyNr: Byte; OnlyNoteOnVoices: Boolean): TDspVoice;
var
  i: Integer;
begin
  Result := nil;
  for i := Count - 1 downto 0 do
    if (Items[i].VoiceInfo.NoteNr = KeyNr) and (not OnlyNoteOnVoices or Items[i].IsVoiceNoteOn) then
    begin
      Result := Items[i];
      exit;
    end;
end;

function TDspVoiceList.GetPlayingVoiceCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := Count - 1 downto 0 do
    if Items[i].IsVoiceNoteOn then inc(Result);
end;

procedure TDspVoiceList.ProcessMidiEvent(MidiEvent: TDAVMidiEvent; var FilterEvent: Boolean);
var
  i      : Integer;
  filter : Boolean;
begin
  FilterEvent := false;
  for i := Count - 1 downto 0 do
   begin
    filter := false;
    Items[i].ProcessMidiEvent(MidiEvent, filter);
    FilterEvent := FilterEvent or filter;
   end;
end;

end.
