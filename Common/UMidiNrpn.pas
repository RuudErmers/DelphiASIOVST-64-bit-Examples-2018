unit UMidiNrpn;

interface

uses UMidiEvent;

type TNrpnStatus = (nrpnIdle,nrpnFirstCC,nrpnSecondCC);
type TNrpnProc = reference to procedure(const midievent:TMidiEvent);
type TNrpnIn = class
               MidiInStatus:array[0..15] of TNrpnStatus;
               MidiInLoCC,MidiInHiCC:array[0..15] of integer;
               constructor Create;
               procedure MidiIn(midievent:TmidiEvent;proc:TNrpnProc);
             end;
type TNrpnOut = class
               procedure MidiOut(midievent:TmidiEvent;proc:TNrpnProc);
             end;

implementation

{ TNrpn }

const   MIDI_CC = $B0;
const   NRPN_HICC = 99;
const   NRPN_LOCC = 98;

constructor TNrpnIn.Create;
VAR i:integer;
begin
  for i:=0 to 15 do
    MidiInStatus[i]:=nrpnIdle;
end;

procedure TNrpnIn.MidiIn(midievent: TmidiEvent; proc: TNrpnProc);
VAR ok:boolean;
    channel:integer;
begin
  repeat
    ok:=true;
    channel:=midievent.Status and $F;
    with midievent do if Status<$F0 then
    case MidiInStatus[channel] of
      nrpnIdle: if (Status = MIDI_CC) and (Data1 = NRPN_HICC) then
               begin
                 MidiInHiCC[channel]:=Data2;
                 MidiInStatus[channel]:=nrpnFirstCC;
                 exit;
               end;
      nrpnFirstCC:
               if (Status = MIDI_CC) and (Data1 = NRPN_LOCC) then
               begin
                 MidiInLoCC[channel]:=Data2;
                 MidiInStatus[channel]:=nrpnSecondCC;
                 exit;
               end
               else
                 MidiInStatus[channel]:=nrpnIdle;
      nrpnSecondCC:
               if (Status = MIDI_CC) and (Data1 = 6) then
               begin
                 Data1:=$80 * MidiInHiCC[channel]+ MidiInLoCC[channel];
               end
               else // retry
               begin
                 MidiInStatus[channel]:=nrpnIdle;
                 ok:=false;
               end;
    end;
  until ok;

  if (midievent.status = MIDI_CC) // See spec...
  then
  begin
    inc(midievent.midichannel,16*(midievent.data1 DIV 1024));
    midievent.data1:=midievent.data1 MOD 1024;
  end;
  proc(midievent);
end;

procedure TNrpnOut.MidiOut(midievent: TmidiEvent; proc: TNrpnProc);
begin
  with midievent do
  begin
    if status = MIDI_CC then
    begin
      inc(data1, (midichannel DIV 16) * 1024);  // See spec...  I don't think this is still used...
      midichannel:=midichannel MOD 16;
      if data1>=128 then
      begin
        // send as NRPN
        proc(UMidiEvent.MidiEvent(midichannel,MIDI_CC,NRPN_HICC,data1 DIV $80));
        proc(UMidiEvent.MidiEvent(midichannel,MIDI_CC,NRPN_LOCC,data1 MOD $80));
        proc(UMidiEvent.MidiEvent(midichannel,MIDI_CC,  6,data2));
        exit;
      end;
    end;
    proc(UMidiEvent.MidiEvent(midichannel,status,data1,data2));
  end;

end;


end.
