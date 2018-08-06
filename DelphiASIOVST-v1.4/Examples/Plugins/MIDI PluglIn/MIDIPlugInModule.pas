unit MIDIPlugInModule;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTEffect, DAV_VSTModule;

type
  TMIDIModule = class(TVSTModule)
    procedure ParamTransposeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  MIDIPlugInGUI;

procedure TMIDIModule.ParamTransposeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
  with (EditorForm As TVSTGUI) do
   begin
    LbTranspose.Caption := 'transpose: ' + IntToStr(Round(Value));

   // Update Scrollbar, if necessary
    if par0.Position <> Round(Value)
     then par0.Position := Round(Value);
   end;
end;

procedure TMIDIModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TVSTGUI.Create(Self);
end;

procedure TMIDIModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
var
  newnote, time, data1, data2, status, channel: integer;
begin
  channel := MidiEvent.midiData[0] and $0F;
  status := MidiEvent.midiData[0] and $F0;
  data1 := MidiEvent.midiData[1] and $7F;
  data2 := MidiEvent.midiData[2] and $7F;
  time := MidiEvent.deltaFrames;
 // example MIDI code:
  if (status = $90) and (data2 > 0) then // "Note On" ?
   begin
  // data1 contains note number
  // data2 contains note velocity
    newnote := Round(Limit(data1 + Parameter[0], 0, 120));
    MIDI_NoteOn(channel, newnote, data2, time);
   end
  else if ((status = $90) and (data2 = 0)) or (status = $80) then
 // "Note Off" ?
   begin
  // data1 contains note number
  // data2 contains note off velocity
  // send "Note Off" back to host (MIDI thru)
    newnote := Round(Limit(data1 + Parameter[0], 0, 120));
    MIDI_NoteOff(channel, newnote, data2, time);
   end
  else if (status = $A0) then // "Polyphonic Aftertouch" ?
 // data1 contains note number
// data2 contains aftertouch value

  else if (status = $B0) then // "MIDI Controller" ?
 // data1 contains CC number
// data2 contains data value

  else if (status = $C0) then // "Program Change" ?
 // data1 contains program number

  else if (status = $D0) then // "Channel Aftertouch" ?
 // data1 contains channel aftertouch value

  else if (status = $E0) then // "Pitchbend" ?
 // data1 and data2 make up the 12 bit pitchbend value
  ;
end;

end.
