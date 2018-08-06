unit XPluginREWavePlayer;

interface

uses DAV_VSTModule,DAV_Types,DAV_VSTEffect,UIXPlugin,Classes,Forms,UWavePlayer,UXPluginBase,URMCVSTView,URMCWavePlayerView;

type TXPluginREWavePlayer = class(TXPluginBase)
  private
    procedure LoadWave(wavefile:string);
    procedure changeParameter(index, value: integer);
    procedure checkParameters;
    procedure UpdateView;
    procedure OnLoadClick(Sender: TObject);
    procedure OnPauseClick(Sender: TObject);
    procedure OnPlayClick(Sender: TObject);
protected
    fWavePlayer:TWavePlayer;
    FWaveFilename:string;
    FValChanger:integer;
    FlastPosition,FlastLength:integer;
    FwavePlayerView:TRMCWavePlayerView;
    function paramtopcc(param: integer): integer;override;
    function pcctoparam(pcc: integer): integer;override;
    function DoMidiKeys:boolean;override;
public
    procedure ProcessThis(const (*Inputs, *) Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);override;
    procedure MidiCC(cc,value:integer);override;
    function ParamCount:integer;override;
    function CreateGUIForm(form:TForm):TRMCVSTView;override;
    function CreateVoice:TXVoice;override;
    procedure AddParameters;override;
    constructor create(vstModule:TVSTModule;Samplerate:single);override;
end;

implementation

{ TXSynthModel }

uses Math,DAV_Common,UMidiEvent,Dialogs,SysUtils,Windows,UVirtCC;

function TXPluginREWavePlayer.ParamCount: integer;
begin
  result:=ParamCountXREWavePlayer;
end;

procedure TXPluginREWavePlayer.AddParameters;
begin
  AddParameter(XREWAVEPLAYERPARM_POSITION0,'Position0',0,127,0,'');
  AddParameter(XREWAVEPLAYERPARM_POSITION1,'Position1',0,127,0,'');
  AddParameter(XREWAVEPLAYERPARM_POSITION2,'Position2',0,127,0,'');
  AddParameter(XREWAVEPLAYERPARM_LENGTH0,'Length0',0,127,0,'');
  AddParameter(XREWAVEPLAYERPARM_LENGTH1,'Length1',0,127,0,'');
  AddParameter(XREWAVEPLAYERPARM_LENGTH2,'Length2',0,127,0,'');
  inherited;
end;

constructor TXPluginREWavePlayer.create(vstModule:TVSTModule;SampleRate:single);
begin
  inherited;
  fWavePlayer:=TWavePlayer.Create;
end;

function TXPluginREWavePlayer.CreateGUIForm(form: TForm): TRMCVSTView;
begin
  FwavePlayerView:=TRMCWavePlayerView.Create(form);
  FwavePlayerView.SetOnLoadClick(OnLoadClick);
  FwavePlayerView.SetOnPlayClick(OnPlayClick);
  FwavePlayerView.SetOnPauseClick(OnPauseClick);
  result:=FwavePlayerView;
end;

function TXPluginREWavePlayer.CreateVoice: TXVoice;
begin
  result:=TXVoice.Create(self);
end;

function TXPluginREWavePlayer.DoMidiKeys: boolean;
begin
  result:=false;
end;

procedure TXPluginREWavePlayer.ProcessThis(const (*Inputs, *) Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
  if fWavePlayer<>NIL then
    fWavePlayer.Process(Outputs,SampleFrames);
  checkParameters
end;

procedure TXPluginREWavePlayer.changeParameter(index,value:integer);
VAR i,val:integer;
begin
  // value can be 15 bits,
  // we use 2 lower bits for making sure the value changes
  for i:=0 to 2 do
  begin
    val:=value MOD 32;
    value:=value DIV 32;
    val:=4*val + FValChanger;
    OnUIPCCChanged(NIL,XREWAVEPLAYERCC_POSITION0+3*index+i,val);
  end;
end;

procedure TXPluginREWavePlayer.checkParameters;
VAR newPosition, newLength:integer;
begin
  newLength := fWavePlayer.Length;
  newPosition := fWavePlayer.Position;
  if  (newPosition<>FlastPosition) or (newLength<>FlastLength) then
  begin
    FlastPosition:=newPosition;
    FlastLength:= newLength;
    UpdateView;
    changeParameter(0,FlastPosition);
    changeParameter(1, FlastLength);
    FValChanger:=(1+FValChanger) MOD 4;
  end;
end;

procedure TXPluginREWavePlayer.MidiCC(cc,value:integer);
const CC_WAVENAME0 = 80;
const CC_WAVEVOLUME0 = 81;
const CC_PLAYSTATUS0 = 82;
  procedure editWavefilename(c:char);
  begin
    if c = #0 then FwaveFileName:=''
              else FwaveFileName:=FwaveFileName+c;
  end;

begin
  case cc of
    CC_WAVENAME0: editWavefilename(chr(value));
    CC_PLAYSTATUS0:
    begin
      case value of
        0: fWavePlayer.Stop;
        1: fWavePlayer.SetPause(true);
        2: begin fWavePlayer.StreamName:=FwaveFileName;fWavePlayer.Play; end;
        3: fWavePlayer.setPause(false);
      end;
      UpdateView;
    end;
    else inherited;
  end;
end;

procedure TXPluginREWavePlayer.UpdateView;
  function SecToStr(n:integer):string;
  function TwoStr(n: integer): string;
  begin
    result := inttostr(n);
    if n < 10 then
      result := '0' + result;
  end;
  begin
    result:=IntToStr(n DIV 60)+':'+TwoStr(n MOD 60);
  end;
begin
  if FwavePlayerView<>NIL then
  begin
    FwavePlayerView.SetInfo(ExtractFileName(FwaveFileName),SecToStr(FlastPosition)+' ('+SecToStr(FLastLength)+')');
  end;
end;

procedure TXPluginREWavePlayer.LoadWave(wavefile:string);
begin
  FwaveFileName:=wavefile;
  fWavePlayer.StreamName:=wavefile;
end;

procedure TXPluginREWavePlayer.OnLoadClick(Sender:TObject);
begin
  with TopenDialog.Create(NIL) do
  begin
    Filter :='WaveFiles|*.mp3;*.wav';
    InitialDir:='C:\Midi\Data\Music';
    Options := [ofFileMustExist];
    if Execute then LoadWave(Filename);
    free;
  end;
end;

procedure TXPluginREWavePlayer.OnPlayClick(Sender:TObject);
begin
  fWavePlayer.Play;
end;

procedure TXPluginREWavePlayer.OnPauseClick(Sender:TObject);
begin
  fWavePlayer.TogglePause;
end;

function TXPluginREWavePlayer.paramtopcc(param: integer): integer;
begin
  result:=ParamToPhysXREWavePlayer[param];
end;

function TXPluginREWavePlayer.pcctoparam(pcc: integer): integer;
begin
  result:=PhysToParamXREWavePlayer[pcc];
end;



end.

