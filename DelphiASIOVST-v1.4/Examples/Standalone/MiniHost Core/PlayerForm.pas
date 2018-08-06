unit PlayerForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Messages, Classes, Controls, Forms, StdCtrls, ComCtrls;

type
  TPlayer = class(TForm)
    GbWavFilePlayer: TGroupBox;
    GbMidiFilePlayer: TGroupBox;
    GbWavRecorder: TGroupBox;
    MidiBox: TListBox;
    WavBox: TListBox;
    BtMidiAdd: TButton;
    BtMidiRemove: TButton;
    BtMidiStop: TButton;
    BtMidiPlay: TButton;
    BtWavAdd: TButton;
    BtWavRemove: TButton;
    BtWavStop: TButton;
    BtWavPlay: TButton;
    BtWavPause: TButton;
    BtWavStopRec: TButton;
    BtWavRecord: TButton;
    LbMidiCurrentFile: TLabel;
    LbMidiFile: TLabel;
    LbWavCurrentFile: TLabel;
    LbWaveFile: TLabel;
    LbMidiPosition: TLabel;
    LbWavPitch: TLabel;
    LbWavPosition: TLabel;
    LbCurrentRecordFile: TLabel;
    LbRecordFile: TLabel;
    LbStatus: TLabel;
    LbMidiPlayMode: TLabel;
    LbWavPlayMode: TLabel;
    CbOnlyChannel1: TCheckBox;
    CbRecInMono: TCheckBox;
    CbRecordFormat: TComboBox;
    CBMidiPlayMode: TComboBox;
    CBWavPlayMode: TComboBox;
    SbTempo: TScrollBar;
    SbMidiPosition: TScrollBar;
    SbPitch: TScrollBar;
    SbWavPosition: TScrollBar;
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;
    procedure FormCreate(Sender: TObject);
    procedure MidiBoxDblClick(Sender: TObject);
    procedure BtMidiAddClick(Sender: TObject);
    procedure BtMidiRemoveClick(Sender: TObject);
    procedure BtMidiPlayClick(Sender: TObject);
    procedure BtMidiStopClick(Sender: TObject);
    procedure WavBoxDblClick(Sender: TObject);
    procedure BtWavAddClick(Sender: TObject);
    procedure BtWavRemoveClick(Sender: TObject);
    procedure BtWavPlayClick(Sender: TObject);
    procedure BtWavStopClick(Sender: TObject);
    procedure BtWavRecordClick(Sender: TObject);
    procedure BtWavPauseClick(Sender: TObject);
    procedure BtWavStopRecClick(Sender: TObject);
    procedure LbRecordFileClick(Sender: TObject);
    procedure CBWavPlayModeChange(Sender: TObject);
    procedure SbTempoChange(Sender: TObject);
    procedure SbMidiPositionChange(Sender: TObject);
    procedure SbPitchChange(Sender: TObject);
    procedure SbWavPositionChange(Sender: TObject);
  private
    LbMidiTempo: TLabel;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var Player: TPlayer;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses SysUtils, MiniHostForm, OptionsForm, ShellAPI;

var FmMiniHost: TFmMiniHost;

procedure TPlayer.WMDropfiles(var msg: TMessage);
var
  Size      : Integer;
  Name      : pchar;
  s         : string;
  i, nCount : Integer;
begin
 inherited;
 nCount := DragQueryFile(msg.WParam, $FFFFFFFF, nil, 0);
 with TStringList.Create do
  try
   for i := 0 to nCount - 1 do
    begin
     Size := DragQueryFile(msg.WParam, i, nil, 0) + 1;
     Name := StrAlloc(size);
     DragQueryFile(msg.WParam, i, Name, Size);
     s := StrPas(Name);
     Add(s);
     StrDispose(Name);
    end;
   DragFinish(msg.WParam);

   if Count = 0 then exit;
   s := UpperCase(ExtractFileExt(Strings[0]));
   if (s = '.MID')
    then FmMiniHost.AddMID(Strings[0]) else
   if (s = '.WAV')
    then FmMiniHost.AddWAV(Strings[0]);
  finally
   Free;
  end;
end;

procedure TPlayer.FormCreate(Sender: TObject);
begin
 DragAcceptFiles(Self.Handle, True);
end;

procedure TPlayer.MidiBoxDblClick(Sender: TObject);
begin
 BtMidiPlayClick(Sender);
end;

procedure TPlayer.BtMidiAddClick(Sender: TObject);
begin
 FmMiniHost.LoadMIDIFile1Click(Sender);
end;

procedure TPlayer.BtMidiRemoveClick(Sender: TObject);
var
  i: Integer;
begin
 if MidiBox.ItemIndex >= 0 then
  begin
   for i := 0 to MidiBox.Items.Count - 1 do
    if MidiBox.Selected[i] then
     FreeMem(pshortstr(MidiBox.Items.Objects[i]));
{$IFNDEF FPC}
   MidiBox.DeleteSelected;
{$ENDIF}
  end;
end;

procedure TPlayer.BtMidiPlayClick(Sender: TObject);
begin
 with FmMiniHost do
  begin
   MidiFile.StopPlaying;
   MidiPlaying := False;
   MIPanicClick(nil);
   if (MidiBox.ItemIndex >= 0) and (MidiBox.Items.Count > 0) then
    begin
     MidiFile.Filename := pshortstr(MidiBox.Items.Objects[MidiBox.ItemIndex])^;
     LbMidiFile.Caption := MidiBox.Items[MidiBox.ItemIndex];
     MidiFile.ReadFile;
     SbTempo.position := MidiFile.Bpm;
     MidiFile.StartPlaying;
     MidiPlaying := True;
    end;
  end;
end;

procedure TPlayer.BtMidiStopClick(Sender: TObject);
begin
 with FmMiniHost do
  begin
   MidiFile.StopPlaying;
   MidiPlaying := False;
   MIPanicClick(nil);
  end;
end;

procedure TPlayer.WavBoxDblClick(Sender: TObject);
begin
 BtWavPlayClick(Sender);
end;

procedure TPlayer.BtWavAddClick(Sender: TObject);
begin
 FmMiniHost.LoadWAVFile;
end;

procedure TPlayer.BtWavRemoveClick(Sender: TObject);
var i: Integer;
begin
 if WavBox.ItemIndex >= 0 then
  begin
   for i := 0 to WavBox.Items.Count - 1 do
    if WavBox.Selected[i] then
     FreeMem(pshortstr(WavBox.Items.Objects[i]));
   {$IFNDEF FPC}
   WavBox.DeleteSelected;
   {$ENDIF}
  end;
end;

procedure TPlayer.BtWavPlayClick(Sender: TObject);
begin
 if (WavBox.ItemIndex >= 0) and (WavBox.Items.Count > 0) then
  with FmMiniHost do
   begin
    LoadWAV(pshortstr(WavBox.Items.Objects[WavBox.ItemIndex])^);
    LbWaveFile.Caption := WavBox.Items[WavBox.ItemIndex];
    StartPlayback2Click(nil);
   end;
end;

procedure TPlayer.BtWavStopClick(Sender: TObject);
begin
 FmMiniHost.StopPlayback2Click(nil)
end;

procedure TPlayer.BtWavRecordClick(Sender: TObject);
begin
 with FmMiniHost do
  if RecordState = rsPause
   then RecordState := rsRecord
   else if RecordState = rsStop
    then MIStartRecordingClick(nil);
end;

procedure TPlayer.BtWavPauseClick(Sender: TObject);
begin
 with FmMiniHost do
  if RecordState = rsRecord
   then RecordState := rsPause;
end;

procedure TPlayer.BtWavStopRecClick(Sender: TObject);
begin
 FmMiniHost.MIStopRecordingClick(nil);
end;

procedure TPlayer.LbRecordFileClick(Sender: TObject);
begin
 FmMiniHost.RecordWAVFileSelect;
end;

constructor TPlayer.Create(AOwner: TComponent);
begin
 inherited;
 FmMiniHost := AOwner as TFmMiniHost; 
end;

procedure TPlayer.CBWavPlayModeChange(Sender: TObject);
begin
 FmMiniHost.WaveFile.Looped := CBWavPlayMode.ItemIndex = 1;
end;

procedure TPlayer.SbTempoChange(Sender: TObject);
begin
 FmMiniHost.MidiFile.Bpm := SbTempo.Position;
 LbMidiTempo.Caption := 'tempo: ' + IntToStr(SbTempo.Position) + ' bpm';
end;

procedure TPlayer.SbMidiPositionChange(Sender: TObject);
begin
 with FmMiniHost do
  if (MIDIPlaying) then
   begin
    WaveTimer.Enabled := False;
    MidiFile.StopPlaying;
    MIPanicClick(Sender);
    MidiFile.PlayToTime(round(MidiFile.GetTrackLength * SbMidiPosition.Position * 0.01));
    MidiFile.ContinuePlaying;
    WaveTimer.Enabled := True;
   end;
 LbMidiPosition.caption := 'position: ' + IntToStr(SbMidiPosition.Position) + ' %';
end;

procedure TPlayer.SbPitchChange(Sender: TObject);
begin
 FmMiniHost.Wavefile.Speed := 2 * SbPitch.Position / 341;
 LbWavPitch.Caption := 'pitch: ' + IntToStr(round(200 * SbPitch.Position / 341)) + ' %';
end;

procedure TPlayer.SbWavPositionChange(Sender: TObject);
begin
 FmMiniHost.Wavefile.SetPos(round((FmMiniHost.Wavefile.Size - 1) * SbWavPosition.Position * 0.01));
 LbWavPosition.Caption := 'position: ' + inttostr(SbWavPosition.Position) + ' %';
end;

{$IFDEF FPC}
initialization
  {$i PlayerForm.lrs}
{$ENDIF}

end.


