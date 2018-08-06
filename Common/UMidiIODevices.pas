unit UMidiIODevices;

interface

const midiIO_Keyboards = 3;
type  TMidiIODeviceID = (midiIO_Keyboard__1,midiIO_Keyboard__2,midiIO_Keyboard__3,midiIO_Keyboard__4,midiIO_Keyboard__5,
                         midiIO_BCR2000_Left,midiIO_BCR2000_DKLeft,midiIO_BCF2000_DKRight,midiIO_BCR2000_Right,
                         midiIO_CrumarDS2,midiIO_MidiMix_Left,midiIO_MidiMix_Right,midiIO_GraphiteCtrl);
function midiIO_Keyboard(keyb:integer):TMidiIODeviceID;
const midiIODeviceName: array[TMidiIODeviceID] of string =
                        ( 'KeyboardBottom','KeyboardMid','KeyboardTop','KeyboardLeft','KeyboardRight','BCR2000-Left','BCR2000-DKLeft','BCF2000-DKRight','BCR2000-Right','Crumar DS2','Akai Midimix Left','Akai Midimix Right','Graphite Control');
const midiIODevicesFilename = 'C:\MIDI\Programs\Super52\Data\MidiHWDevices.txt';

implementation

function midiIO_Keyboard(keyb:integer):TMidiIODeviceID;
begin
  result:=TMidiIODeviceID(ord(midiIO_Keyboard__1)+keyb);
end;

end.
