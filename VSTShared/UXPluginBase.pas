unit UXPluginBase;
{ partial class which implements a Plugin Synth }

interface

uses Generics.Collections,Controls,Forms,URMCVSTView,DAV_VSTModule,DAV_Types,Classes,UIXPlugin,DAV_VSTEffect,UMidiEvent,USampleValue,XEffectsBase;

const VOICECOUNT = 16;

type TXPluginBase = class;

     TXVoice = class
private
    FSampleRate : Single;
    FSampleReci : Single;
    FPitch  : Integer;
    FReleased,FHighest : Boolean;

    { property } IsPlaying:boolean;
    { property } NoteCounter: integer;
protected
    FXPlugin: TXPluginBase;

    { property } IsQuickReleasing:boolean;
    { property } Sustain:boolean;

    procedure SetSampleRate(const Value: Single); virtual;
    procedure NoteOn(pitch,startpitch:integer; Amplitude: Single);virtual;
    procedure ReleaseQuick;virtual;
    procedure NoteOff;
    procedure SampleRateChanged; virtual;
    function CanGlide:boolean;
public
    constructor Create(xplugin:TXPluginBase);

    function Process: TSampleValue; virtual;
    property SampleRate: Single read FSampleRate write SetSampleRate;
    property SampleReci: single read FSampleReci;
    property Pitch: Integer read FPitch write FPitch;
    property Released: Boolean read FReleased;

end;
     TVoiceList = array[0..VOICECOUNT-1] of TXVoice;
   TXPluginBase = class (TComponent,IXPlugin)
strict private
    FEffects: TEffectsBase;
    FvstModule:TvstModule;
    FSampleReci:single;
    Voices : TVoiceList;
    FguiForm: TRMCVSTView;
    FLastVelocity,FLastPitch:integer;
    FSustain:boolean;
    EditorForm: TForm;
    FNotes:array[0..127] of integer;
private
    FNoteCounter:integer;
    procedure CheckHighest;
    function OnEffectsChanged(index, value: integer): boolean;
protected
    FEffectsSettings:TEffectsSettings;
    FModWheel,FPitchBend,FSampleRate:single;
    FPoly:boolean;
    procedure OnVoiceDone(Sender: TObject);
    procedure ProcessThis(const (*Inputs, *) Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);virtual;
    procedure PitchBend(l,m:integer);
    procedure NoteOff(pitch: integer);
    procedure NoteOn(pitch, velo: integer);
    procedure AllNotesOff;
    function paramtopcc(param: integer): integer;virtual;abstract;
    function pcctoparam(pcc: integer): integer;virtual;abstract;
    procedure Sustain(_on: boolean);
    procedure OnUIPCCChanged(Sender:TObject;pcc,value:integer);
    procedure RegisterNote(pitch:integer);
    procedure UnregisterNote(pitch:integer);
    procedure OnNoteEvent(pitch:integer;_on:boolean);
    function CreateGUIForm(form:TForm):TRMCVSTView;virtual;abstract;
    procedure OnUINoteOff(Sender:TObject;pitch:byte);
    procedure OnUINoteOn(Sender:TObject;pitch:byte;velo: single);
    function PreviousNote:integer;
    procedure EffectSettings(value:integer);
    function CreateVoice:TXVoice;virtual;abstract;
    procedure AddParameter(id:integer;displayname:string;min,max:integer;def:single;units:string;customparameters:string='');
    function DoMidiKeys:boolean;virtual;
    function CreateEffectsSettings(paramStart: integer; createdefault:boolean=true):TEffectsSettings;virtual;
public
    procedure SetEffect(index,value:integer);

    procedure AddParameters;virtual;
    procedure SetEditor(form:TForm);
    procedure SetSlider(index,value:integer); // for quick debugging
    procedure ShowEffects;
    procedure Process(const Inputs,Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure SetSampleRate(rate:single);virtual;
    procedure OnParameterChanged(Sender: TObject; const Index,Value: Integer);virtual;
    function ParamCount:integer;virtual;
    procedure Close;
    procedure SetTempo(tempo:single);
    procedure ProcessMidi(Sender: TObject; MidiEvent: TMidiEvent);virtual;
    procedure LoadFromStream(stream: TMemoryStream; index: integer; isPreset: boolean);
    procedure SaveToStream(stream: TMemoryStream; index: integer;   isPreset: boolean);
    function GetFormClass:TFormClass;virtual;
    procedure MidiCC(cc,value:integer);virtual;
    constructor create(vstModule:TVSTModule;SampleRate:single);virtual;
    function getSample: TSampleValue;virtual;

end;

implementation

uses Math,DAV_Common,DAV_Approximations,XSynthModule,SysUtils,Windows,XSynthMainFrame,UVirtCC,CodeSiteLogging;
const XRECDS2EFFPARAMS = 11;
procedure TXPluginBase.Process(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
VAR i,count:integer;
begin
  // Processing van meerdere effects, Input en Output buffers…
  // Een VST kan bestaan uit meerdere sub-vst’s die achtereenvolgens doorlopen worden.
  // De uber-VST zal zijn input van input naar output moeten processen, maar daartussen moet het slim gebeuren, waarbij er geen buffer copieen gebeuren.
  // Stel een uber-VST bestaat uit 1 vst dan is processing simpel: input -> output.
  // Anders bestaat de uber-VST uit twee chains V1, V2. Van ieder van die Vi’s moet er een property zijn die
  // aangeeft hoeveel vst’s er in de chain zitten: ProcessCount: PC1, PC2
  // is de som even moeten we processen Chain 1: output-> input
  //                                    Chain 2: input -> output indien beide oneven     i
  //                                             output-> input indien beide even        ii
  // is de som oneven dan               Chain 1: input -> output
  //                                    Chain 2: input -> output PC1 even                iii
  //                                             output-> input indien PC2 even          iv
  // in dit geval zijn er twee chains, met PC1 = 1
  count:=fEffects.ProcessCount;
  if count MOD 2 = 1 then  // Som Even, beide PCi oneven => i
    begin
      ProcessThis((*Outputs,*)Inputs,SampleFrames);
      fEffects.Process(Inputs,Outputs,SampleFrames);
    end
  else                     // Som oneven, PC1 = 1, PC2 even-> iv
    begin
      ProcessThis((*Inputs,*)Outputs,SampleFrames);
      fEffects.Process(Outputs,Inputs,SampleFrames);
    end;
end;


const MIDI_NOTE_ON = $90;
  MIDI_NOTE_OFF = $80;
  MIDI_POLYPRESSURE = $A0;
  MIDI_CC = $B0;
  MIDI_PRG = $C0;
  MIDI_CHNLPRESSURE = $D0;
  MIDI_PITCHWHEEL = $E0;

procedure TXPluginBase.ProcessMidi(Sender: TObject; MidiEvent: TMidiEvent);
VAR
  prg:integer;
begin
  with MidiEvent do
  begin
    if (Status = MIDI_CC) then
      if data1 = $7E then
        AllNotesOff
      else
        MidiCC(data1,data2);
    if (Status = MIDI_PRG) then
      begin
        prg:=data1;
        if prg < FvstModule.numPrograms then
          FvstModule.CurrentProgram:=prg;
      end;
    if (Status = MIDI_PITCHWHEEL) then
      PitchBend(data1,data2);
    if isNoteOn then NoteOn(data1,data2);
    if isNoteOff then NoteOff(data1);
  end;
end;

procedure TXPluginBase.SetSampleRate(rate: single);
VAR i, lfo:integer;
begin
  FSampleRate:=Rate;
  FSampleReci:=1/FSampleRate;
  for i:=0 to VOICECOUNT-1 do
     Voices[i].SampleRate:=FSampleRate;
end;

procedure TXPluginBase.SetSlider(index, value: integer);
begin
//
end;

procedure TXPluginBase.SetTempo(tempo: single);
begin
  FEffects.SetTempo(tempo);
end;

procedure TXPluginBase.Close;
begin
  FEffects.Close;
end;

constructor TXPluginBase.create(vstModule: TVSTModule; SampleRate: single);
VAR i:integer;
begin
  FvstModule:=VSTModule;
  FEffectsSettings:=CreateEffectsSettings(-1);
  FEffects:=FEffectsSettings.factory.Create(FEffectsSettings);
  FSampleRate:=SampleRate;
  FSampleReci:=1/SampleRate;
  for i:=0 to VOICECOUNT -1 do
    Voices[i] := CreateVoice;
  FModWheel:=0;
  FPitchBend:=0.5;
  FLastPitch:=0;
  FNoteCounter:=0;
end;

function TXPluginBase.CreateEffectsSettings(paramStart: integer; createdefault:boolean=true): TEffectsSettings;
begin
  result:=TEffectsSettings.Create(paramStart,createdefault);
  result.factory:=TEffectsBase;
end;


function TXPluginBase.DoMidiKeys: boolean;
begin
  result:=true;
end;

procedure TXPluginBase.LoadFromStream(stream: TMemoryStream; index: integer;  isPreset: boolean);
begin
//
end;

procedure TXPluginBase.OnParameterChanged(Sender: TObject; const Index, Value: Integer);
begin
  OnEffectsChanged(index,value);
  if (FguiForm<>NIL) and (FGuiForm<>Sender) and (FGuiForm.Parent<>NIL) then
    FguiForm.ChangePCC(paramtopcc(index),Value);
end;

procedure TXPluginBase.ProcessThis(const (*Inputs,*) Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
VAR i,count:integer;
    sample:TSampleValue;
begin
  for i:=0 to SampleFrames-1 do
  begin
     sample:=getSample;
     Outputs[0][i]:=sample[0];
     Outputs[1][i]:=sample[1];
  end;
end;

procedure TXPluginBase.MidiCC(cc, value: integer);
VAR index:integer;
begin
  case cc of
       1: begin FModWheel:= value / 127; exit; end;
      64: Sustain(value>64);
    else OnUIPCCChanged(NIL,cc,value);
  end;
end;

procedure TXPluginBase.PitchBend(l, m: integer);
begin
  FPitchBend:=(m * 128 + l) / (128*128);
end;

procedure TXPluginBase.CheckHighest;
VAR i,phighest:integer;
begin
  phighest:=-1;
  for i:=0 to VOICECOUNT-1 do
    if (Voices[i].IsPlaying) and not Voices[i].IsQuickReleasing then
    begin
      if phighest=-1 then phighest:=i
      else if Voices[phighest].FPitch < Voices[i].FPitch then phighest:=i;
    end;
  for i:=0 to VOICECOUNT-1 do
    Voices[i].Fhighest:=i=phighest;
end;

procedure TXPluginBase.NoteOn(pitch,velo:integer);
  function CheckVoicePlaying:boolean;
  VAR i:integer;
  begin
    result:=false;
    for i:=0 to VOICECOUNT-1 do
     if (Voices[i].Pitch = pitch) and (Voices[i].IsPlaying) then
     begin
       Voices[i].ReleaseQuick;
       result:=true;
     end;
  end;
  function EarliestPlayingIndex:integer;
  VAR i,lowNoteCounter:integer;
  begin
    result:=-1;
    lowNoteCounter := FNoteCounter+10;
    for i:=0 to VOICECOUNT-1 do
      if (Voices[i].IsPlaying) and not Voices[i].IsQuickReleasing and (Voices[i].NoteCounter < lowNoteCounter) then
      begin
        result:=i;
        lowNoteCounter:=Voices[i].NoteCounter;
      end;
  end;
  procedure RemoveVoice;
  VAR index:integer;
  begin
    index:=EarliestPlayingIndex;
    if index>=0 then
//      Voices[index].IsPlaying:=false;
      Voices[index].ReleaseQuick;
  end;
  function ActiveVoicesCount:integer;
  VAR i:integer;
  begin
    result:=0;
    for i:=0 to VOICECOUNT-1 do
     if (Voices[i].IsPlaying) and not Voices[i].IsQuickReleasing then inc(result);
  end;
  function GetFreeIndex:integer;
  VAR i:integer;
  begin
    for i:=0 to VOICECOUNT-1 do
      if (not Voices[i].IsPlaying) then begin result:=i; exit; end;
    result:=0;
  end;
var av,index,VoiceMax:integer;
const  CVeloDiv : Single = 1 / 128;
begin
  CheckVoicePlaying;
  av:=ActiveVoicesCount;
  if FPoly then VoiceMax:=8 else VoiceMax:=1;
  if av >= VoiceMax then RemoveVoice;
  RegisterNote(pitch);
  index:=GetFreeIndex;
  with Voices[index] do
    begin
     NoteCounter:=FNoteCounter;
     inc(FNoteCounter);
     IsPlaying:=true;
    end;
  // next statement can not be called from with do construction due to membername pitch
  Voices[index].NoteOn(pitch,FLastPitch, Velo * CVeloDiv);
  CheckHighest;
  OnNoteEvent(pitch,true);
  FLastVelocity:=velo;
  FLastPitch:=pitch;
end;

procedure TXPluginBase.NoteOff(pitch:integer);
VAR prev:integer;
  procedure ReleaseNote;
  VAR i:integer;
  begin
    for i:=0 to VOICECOUNT-1 do
      if (Voices[i].IsPlaying) and (Voices[i].Pitch = pitch) then
      begin
   			if (FSustain) then voices[i].sustain := true
                 			else Voices[i].NoteOff;
      end;
  end;
begin
  UnregisterNote(pitch);
  OnNoteEvent(pitch,false);
  if not FPoly then
  begin
    prev:=PreviousNote;
    if (prev<>0) and (pitch=FLastPitch)  then NoteOn(prev,FLastVelocity);
    ReleaseNote;
  end
  else ReleaseNote;
end;


procedure TXPluginBase.OnVoiceDone(Sender:TObject);
VAR i:integer;
begin
  for i:=0 to VOICECOUNT-1 do
    if (Voices[i].IsPlaying) and (Voices[i] = Sender) then
     begin
      Voices[i].IsPlaying:=false;
      CheckHighest;
      Exit;
     end;
end;



procedure TXPluginBase.SaveToStream(stream: TMemoryStream; index: integer;  isPreset: boolean);
begin
//
end;

procedure TXPluginBase.SetEditor(form: TForm);
VAR i:integer;
    v:single;
    s:string;
begin
  if form<>NIL then
  begin
    s:=Form.Name;
    CodeSite.Send('Set Editor : '+s);

    if FguiForm = NIL then FguiForm:=CreateGUIForm(form)
    else
      form.InsertComponent(FguiForm);
    s:='Formpie'+inttostr(GetTickCount);
    form.Name:=s;
    FGuiForm.Name:='crumarview1';
    FguiForm.parent:=form;
    FguiForm.Visible:=true;
    with form as TSynthMainFrame do
    begin
      MidiKeys.visible:=DoMidiKeys;
      ClientWidth:=FGuiForm.Width;
      ClientHeight:=FGuiForm.Height+MidiKeys.Height*ord(DoMidiKeys);
      FguiForm.OnPCCChanged:=OnUIPCCChanged;
      MidiKeys.OnNoteOn:=OnUINoteOn;
      MidiKeys.onNoteOff:=OnUINoteOff;
    end;
    if FVstModule<>NIl then
      TVSTSSModule(FVstModule).ResendParameters;
  end
  else
  begin
    FguiForm.Visible:=false;
    CodeSite.Send('Set Editor : NIL');

    if FguiForm.Parent = EditorForm then
    begin
      CodeSite.Send('Set Editor : NIL PARENT UNLINK');
      FguiForm.Parent:=NIL;
      FGuiForm.Name:='crumarview1NoParent';

      EditorForm.RemoveComponent(FguiForm);
    end
    else
      CodeSite.Send('Set Editor : NIL NO MATCH');
  //  FguiForm:=NIL;
  end;
  EditorForm:=form;
end;

procedure TXPluginBase.Sustain(_on:boolean);
VAR i:integer;
begin
  FSustain:=_on;
  if FSustain then
    for i:=0 to VOICECOUNT-1 do
  			if (voices[i].sustain) AND (voices[i].isPlaying) then
  				voices[i].NoteOff;
end;

procedure TXPluginBase.OnUIPCCChanged(Sender:TObject;pcc,value:integer);
const VirtCC_SHOWEFFECTS = 1026;
begin
  if pcc = VirtCC_SHOWEFFECTS then
  begin
    ShowEffects;
    exit;
  end;
  if FVstModule<>NIL then
    TVSTSSModule(FVstModule).ModelSetParameter(pcctoparam(pcc),value)
  else { VST is coupled to UI }
    OnParameterChanged(Sender,pcctoparam(pcc),value);
end;

procedure TXPluginBase.RegisterNote(pitch:integer);
begin
  FNotes[pitch]:=FNoteCounter;
end;


procedure TXPluginBase.UnregisterNote(pitch:integer);
begin
  FNotes[pitch]:=0;
end;

procedure TXPluginBase.OnNoteEvent(pitch: integer; _on: boolean);
begin
  if EditorForm<>NIL then TSynthMainFrame(EditorForm).OnNoteEvent(pitch,_on);
end;

procedure TXPluginBase.AddParameters;
VAR fxStart,delta:integer;
begin
  fxStart:=FEffectsSettings.ParamStart;
  if fxStart=-1 then exit;
  delta:=fxStart-XRECDS2PARM_DELAYAMOUNT;
  AddParameter(XRECDS2PARM_DELAYAMOUNT + delta,'Delay Amount',0,100,10,'%');
  AddParameter(XRECDS2PARM_DELAYTIME + delta, 'Delay Time',0,127,50,'beat/24');
  AddParameter(XRECDS2PARM_REVERBAMOUNT + delta,'Reverb Amount',0,100,30,'%');
  AddParameter(XRECDS2PARM_REVERBTIME + delta, 'Reverb Time',0,4000,0,'msec');
  AddParameter(XRECDS2PARM_CHORUSDEPTH + delta,'Chorus Amount',0,100,0,'%');
  AddParameter(XRECDS2PARM_CHORUSRATE + delta,'Chorus Rate',0,100,50,'%');
  AddParameter(XRECDS2PARM_PHASERDEPTH + delta,'Phaser Amount',0,100,0,'%');
  AddParameter(XRECDS2PARM_PHASERRATE  + delta,'Phaser Rate',0,100,50,'%');
  AddParameter(XRECDS2PARM_DELAYFEEDBACK + delta,'Delay Feedback',0,100,50,'%');
  AddParameter(XRECDS2PARM_REVERBFEEDBACK + delta,'Reverb Feedback',0,100,50,'%');
  AddParameter(XRECDS2PARM_EFFECTSETTINGS + delta,'Effect Settings',0,127,4,'(Internal)');
  Assert(XRECDS2EFFPARAMS=11);
end;

function TXPluginBase.OnEffectsChanged(index,value:integer):boolean;
VAR fxStart:integer;
begin
  result:=false;
  fxStart:=FEffectsSettings.ParamStart;
  if fxStart=-1 then exit;
  dec(index,fxStart);
  if (index>=0) and (index<=XRECDS2PARM_REVERBFEEDBACK-XRECDS2PARM_DELAYAMOUNT) then
    SetEffect(index,value)
  else if index = XRECDS2PARM_EFFECTSETTINGS-XRECDS2PARM_DELAYAMOUNT then
    EffectSettings(value)
  else
    exit; // not found
  result:=true;
end;

procedure TXPluginBase.AllNotesOff;
VAR i:integer;
begin
  for i:=0 to VOICECOUNT-1 do
    Voices[i].IsPlaying:=false;
end;

procedure TXPluginBase.ShowEffects;
begin
  FEffects.ShowEffects;
end;

procedure TXPluginBase.OnUINoteOff(Sender:TObject;pitch:byte);
begin
  NoteOff(pitch);
end;

procedure TXPluginBase.OnUINoteOn(Sender:TObject;pitch:byte;velo: single);
begin
  NoteOn(pitch,round(127*velo));
end;

function TXPluginBase.PreviousNote:integer;
VAR i,max,pmax:integer;
begin
  max:=0;
  pmax:=0;
  for i:=0 to 127 do if FNotes[i]>max then begin pmax:=i; max:=FNotes[i]; end;
  result:=pmax;
end;

function TXPluginBase.getSample: TSampleValue;
var
  voice  : Integer;
begin
  result.zero;
  try
    for voice:=0 to VOICECOUNT-1 do
      if Voices[voice].IsPlaying then
        result.add(Voices[voice].Process);
  except end;
  result.bound;
end;

procedure TXPluginBase.AddParameter(id: integer; displayname: string; min,
  max: integer; def: single; units, customparameters: string);
VAR v:integer;
begin
  if FVstModule<>NIL then
    TVSTSSModule(FVstModule).AddParameter(id,displayname,min,  max,def,units, customparameters)
  else
  begin
    v:=round(127*(def-min)/(max-min));
    OnParameterChanged(self,id,v);
  end;
end;

procedure TXPluginBase.SetEffect(index, value: integer);
begin
  FEffects.SetEffect(index,value);
end;

procedure TXPluginBase.EffectSettings(value: integer);
begin
// bit 0..1: which effect should be visible
// bit 2: enable effects
   FEffects.ShowEffect(value and 3);
   FEffects.SetEffectsEnable(value and 4 = 4);
end;

function TXPluginBase.GetFormClass: TFormClass;
begin
  result:=TSynthMainFrame;
end;

function TXPluginBase.ParamCount: integer;
begin
  result:=0;
end;

{ TXVoice }

function TXVoice.CanGlide: boolean;
begin
  result:=(NoteCounter = Fxplugin.FNoteCounter-1) AND (NoteCounter<>0);
end;

constructor TXVoice.Create(xplugin: TXPluginBase);
begin
  FXPlugin:=xplugin;
  SampleRate:=xplugin.FSampleRate;
end;

procedure TXVoice.NoteOff;
begin
  FReleased := True;
end;

procedure TXVoice.NoteOn(pitch, startpitch: integer; Amplitude: Single);
begin
  FReleased := False;
  self.pitch := pitch;
  IsQuickReleasing:=false;
end;

function TXVoice.Process: TSampleValue;
begin
  result.zero;
end;

procedure TXVoice.ReleaseQuick;
begin
  IsQuickReleasing:=true;
  NoteOff;
end;

procedure TXVoice.SampleRateChanged;
begin

end;

procedure TXVoice.SetSampleRate(const Value: Single);
begin
 if Value <= 0
 then
  begin
   FSampleRate := 44100;
   FSampleReci := 1 / FSampleRate;
   exit;

  end;
 if Value <> FSampleRate then
  begin
   FSampleRate := Value;
   FSampleReci := 1 / FSampleRate;
   SampleRateChanged;
  end;
end;


end.

