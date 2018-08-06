unit MiniHostForm;

{$I DAV_Compiler.inc}

interface

{$IFNDEF FPC}
//  {$DEFINE LoadPluginFromStream}
{$ENDIF}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Types, Messages,
  Forms, SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Menus, SyncObjs,
  DAV_Types, DAV_VstEffect, DAV_MidiFile, DAV_MidiIO, DAV_ASIOHost,
  DAV_VSTHost, DAV_GuiCommon,
  WaveIOX, AboutForm;

type
  ShortStr = string[255];
  PShortStr = ^ShortStr;

const
  AppVersion = '1.0';
  AppName = 'MiniHost Core';

type
  TWavPlayerMode = (wpmPause, wpmPlay);
  TWavPlayer = class
  private
    FBuffer     : PDAVSingleFixedArray;
    FBufferPntr : PSingle;
    FLooped     : Boolean;
    FInterpol   : Boolean;
    FCnt2       : Integer;
    FSize       : Integer;
    FSR, FCh    : Integer;
    FSpeed      : Single; 
    FVol, FPan  : Single;
    FSamplerate : Single;
    FCnt        : Double;
    FPMode      : TWavPlayerMode;
    FFilename   : TFileName;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetPos(const Value: Integer);
    procedure Process(var o1, o2: single);
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Load(const Filename: TFileName);
    procedure Unload;

    property SampleRate: Single read FSamplerate write FSamplerate;
    property Speed: Single read FSpeed write FSpeed;
    property Size: Integer read FSize;
    property Volume: Single read FVol write FVol;
    property Looped: Boolean read FLooped write FLooped;
    property Interpolate: Boolean read FInterpol write FInterpol;
    property Filename: TFileName read FFilename write FFilename;
  end;

  MIDIData = record
    d1, d2, d3: byte;
    pos: Integer;
  end;

  { TFmMiniHost }

  TRecordState = (rsStop, rsRecord, rsPause);

  TFmMiniHost = class(TForm)
    VSTHost: TVSTHost;
    ASIOHost: TASIOHost;
    WaveTimer: TTimer;
    IdleTimer: TTimer;
    MainMenu: TMainMenu;

    MIVST: TMenuItem;
    MIASIO: TMenuItem;
    MIMIDI: TMenuItem;
    MIHelp: TMenuItem;
    MIVSTLoadPlugin: TMenuItem;
    MIAbout: TMenuItem;
    MIPanic: TMenuItem;
    MIMIDIIn: TMenuItem;
    MIMIDIOut: TMenuItem;
    MIPreset: TMenuItem;
    MILoadPreset: TMenuItem;
    MILoadBank: TMenuItem;
    MISavePreset: TMenuItem;
    MISaveBank: TMenuItem;
    MIAsioDriver: TMenuItem;
    MIASIOControlPanel: TMenuItem;
    MIASIOOutputChannel: TMenuItem;
    MIRenamePreset: TMenuItem;
    MISettings: TMenuItem;
    MIASIOInputChannel: TMenuItem;
    MIVSTClosePlugin: TMenuItem;
    MIDownMixToStereo: TMenuItem;
    MIMidiThru: TMenuItem;
    MIShowPreset: TMenuItem;
    MIAlwaysOnTop: TMenuItem;
    MIUseMouseWheel: TMenuItem;
    MIMain: TMenuItem;
    MIExit: TMenuItem;
    MIShowMIDIWAVWindow: TMenuItem;
    N1: TMenuItem;   
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    PnStatus: TPanel;
    PresetBox: TComboBox;
    ToolBarBackground: TImage;
    IBtLeftRight: TImage;
    IBtDropDown: TImage;
    IQuickSettings: TImage;
    IQuickMidPlay: TImage;
    IQuickWavPlay: TImage;
    IQuickWavRec: TImage;
    BorderPlayMIDI: TImage;
    BorderPlayWave: TImage;
    BorderRecordWave: TImage;
    BorderOnOff: TImage;
    BorderOptions: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure VSTHostAudioMasterIdle(Sender: TVSTPlugin);
    procedure VSTHostAudioMasterNeedIdle(Sender: TVSTPlugin);
    procedure ASIOHostLatencyChanged(Sender: TObject);
    procedure ASIOHostUpdateSamplePos(Sender: TObject; SamplePosition: Int64);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure ASIOHostReset(Sender: TObject);
    procedure ASIOHostDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure MIPanicClick(Sender: TObject);
    procedure MILoadPresetClick(Sender: TObject);
    procedure MISavePresetClick(Sender: TObject);
    procedure MILoadBankClick(Sender: TObject);
    procedure MISaveBankClick(Sender: TObject);
    procedure MIVSTClosePluginClick(Sender: TObject);
    procedure MIVSTLoadPluginClick(Sender: TObject);
    procedure MIASIOControlPanelClick(Sender: TObject);
    procedure MIRenamePresetClick(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MISettingsClick(Sender: TObject);
    procedure MIShowPresetClick(Sender: TObject);
    procedure MIStartRecordingClick(Sender: TObject);
    procedure MIStopRecordingClick(Sender: TObject);
    procedure MIAlwaysOnTopClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIShowMIDIWAVWindowClick(Sender: TObject);
    procedure MIDownMixToStereoClick(Sender: TObject);
    procedure MIMidiThruClick(Sender: TObject);
    procedure MIUseMouseWheelClick(Sender: TObject);
    procedure StartPlayback2Click(Sender: TObject);
    procedure StartPlayback1Click(Sender: TObject);
    procedure StopPlayback1Click(Sender: TObject);
    procedure StopPlayback2Click(Sender: TObject);
    procedure WaveTimerTimer(Sender: TObject);
    procedure LoadMIDIFile1Click(Sender: TObject);
    procedure RenameF1Click(Sender: TObject);
    procedure F3PlayStopMIDI1Click(Sender: TObject);
    procedure F4PlayStopWAV1Click(Sender: TObject);
    procedure F5RecStopWAV1Click(Sender: TObject);
    procedure F11MIDIPanic1Click(Sender: TObject);
    procedure PresetBoxClick(Sender: TObject);
    procedure PresetBoxChange(Sender: TObject);
    procedure PresetBoxKeyPress(Sender: TObject; var Key: Char);
    procedure PresetBoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure BorderPlayMIDIMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IdleTimerTimer(Sender: TObject);
    procedure IBtLeftRightMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IBtDropDownMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IOnOffMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickSettingsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickMidPlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickWavPlayMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure IQuickWavRecMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FDataSection    : TCriticalSection;
    FRecordState    : TRecordState;
    FDownMix        : Boolean;
    FTotalFrames    : Integer;
    FPanel          : TPanel;
    FTitle          : AnsiString;
    FWaveFile       : TWavPlayer;
    FProcessing     : Boolean;
    FAllowed        : Boolean;
    FLoadProg       : Integer;
    FDirPlugin,
    FDirPreset,
    FDirWave,
    FDirMidi        : string;
    FPluginLoaded   : Boolean;
    FWavBufL,
    FWavBufR,
    FInBufL,
    FInBufR         : TDAVSingleDynArray;
    FVSTBufIn,
    FVSTBufOut      : TDAVArrayOfSingleDynArray;
    FMIDIPlaying    : Boolean;
    FMyEvents       : TVstEvents;
    FOverallVol     : Single;
    FVSTVol         : Single;
    FInputVol       : Single;
    FCurProg        : Integer;
    FCurProgName    : AnsiString;
    FVSTPinProps    : array of TVstPinProperties;
    FNumIn,
    FNumOut         : Integer;
    FColBack        : Boolean;
    FAboutForm      : TFmAbout;

    FMidiFile       : TMidiFile;
    FMIDIInput      : TMidiInput;
    FMIDIOutput     : TMidiOutput;
    FWavWriter      : TWavWriter;

    FCurrentASIO    : Integer;
    FCurrentMIDIIn  : Integer;
    FCurrentMIDIOut : Integer;
    FCurrentOutputChannel,
    FCurrentInputChannel: Integer;

    FMDataCnt       : Integer;
    function FindBackgroundColor: TColor;
    procedure ASIOChange(Sender: TObject);
    procedure BuildChannelBuffers;
    procedure ClosePlugin;
    procedure MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: byte);
    procedure MIDIInChange(Sender: TObject);
    procedure MIDIOutChange(Sender: TObject);
    procedure MyMidiEvent(event: PMidiEvent);
    procedure ProcessEvents(Sender: TObject; ev: PVstEvents);
    procedure ProcessNoteOnOff(ch, n, v: byte);
    procedure SetChannel(Sender: TObject);
    procedure SetChannelI(Sender: TObject);
    procedure SetPreset(Sender: TObject);
    procedure StopProcessingAndClosePlugin;
    procedure SysExData(const aDeviceIndex: integer; const aStream: TMemoryStream);
  protected
    procedure ShowVSTPlugin(const DefaultProgram: Integer = 0);
  public
    procedure AddMID(const FileName: string);
    procedure AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
    procedure AddWAV(const FileName: string);
    procedure BuildPresetList;
    procedure LoadPlugin(const VSTDll: TFileName; const DefaultProgram: Integer = 0); overload;
    {$IFNDEF FPC}
    procedure LoadPlugin(const Stream: TStream; const DefaultProgram: Integer = 0); overload;
    {$ENDIF}
    procedure LoadPresets(Files: TStrings);
    procedure LoadWAV(const FileName: string);
    procedure LoadWAVFile;
    procedure NoteOff(ch, note: Byte);
    procedure NoteOn(ch, note, v: Byte);
    procedure RecordWAVFileSelect;
    procedure StartAudio;
    procedure StopAudio;
    procedure WMDropFiles(var msg: TMessage); message WM_DROPFILES;

    property MidiFile: TMidiFile read FMidiFile;
  published
    property CurrentProgram: Integer read FCurProg;
    property CurrentProgramName: AnsiString read FCurProgName;
    property InputVol: Single read FInputVol write FInputVol;
    property MidiPlaying: Boolean read FMIDIPlaying write FMIDIPlaying;
    property OverallVolume: Single read FOverallVol write FOverallVol;
    property RecordState: TRecordState read FRecordState write FRecordState;
    property VSTVol: Single read FVSTVol write FVSTVol;
    property WaveFile: TWavPlayer read FWaveFile;
  end;

var
  FmMiniHost : TFmMiniHost;
  ININame    : string;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Math, Inifiles, Dialogs, ShellAPI, DAV_Common, DAV_AudioData,
  DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU,
  OptionsForm, PlayerForm;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;


{ TWavPlayer }

constructor TWavPlayer.Create;
begin
 FBuffer := nil;
 FBufferPntr := nil;
 FCnt := 0;
 FCnt2 := 0;
 FPMode := wpmPause;
 FVol := 1;
 FPan := 0.5;
 FSpeed := 1;
 FInterpol := False;
end;

destructor TWavPlayer.Destroy;
begin
 if Assigned(FBuffer) then Dispose(FBuffer);
 inherited;
end;

procedure TWavPlayer.Load(const Filename: TFileName);
begin
 FPMode := wpmPause;
 if Assigned(FBuffer) then
  begin
   Dispose(FBuffer);
   FBuffer := nil;
  end;
 Filemode := 0;
 if FileName <> '' then
  begin
 //  FBuffer := LoadWAVFile(s,sr,ch,size);
  end;
 FBufferPntr := @FBuffer^[0];
 FCnt := 0;
 FCnt2 := 0;
 FFilename := FileName;
end;

procedure TWavPlayer.Pause;
begin
 FPMode := wpmPause;
end;

procedure TWavPlayer.Play;
begin
 FPMode := wpmPlay;
end;

procedure TWavPlayer.Process(var o1, o2: single);
var
  next, next2, pp: PSingle;
begin
 if (not Assigned(FBufferPntr))   // if buffer is empty (no file loaded)
  or (FPMode = wpmPause) // or "play" not activated
 then begin              // then output silence
  o1 := 0;
  o2 := 0;
 end else
 begin
  o1 := FBufferPntr^;
  if FCh = 2 then // stereo?
   begin
    pp := PSingle(LongInt(FBufferPntr) + 4);
    o2 := pp^;
    next := PSingle(LongInt(FBufferPntr) + 8);
    next2 := PSingle(LongInt(FBufferPntr) + 12);
    o2 := o2 * (1 - FCnt) + FCnt * next2^;
   end
  else
   begin
    next := PSingle(longint(FBufferPntr) + 4);
    o2 := o1;
   end;
  if (FCnt <1 ) and (FInterpol) then // interpolation?
   o1 := o1 * (1 - FCnt) + FCnt * next^; // get next sample

  FCnt := FCnt + speed * (FSR / samplerate);
  while (FCnt >= 1) do
   begin
    Inc(FBufferPntr, FCh);
    FCnt := FCnt - 1;
    Inc(FCnt2, FCh);
   end;
  if (FCnt2 >= FSize - 1) then
   begin
    if not looped then
     begin
      FPMode := wpmPause;
      Player.SbWavPosition.position := 0;
      if (Player.CBWavPlayMode.ItemIndex = 2) and (Player.WavBox.Items.Count > 0) then
       begin
        Player.WavBox.ItemIndex := (Player.WavBox.ItemIndex + 1) mod Player.WavBox.Items.Count;
        Player.BtWavPlayClick(nil);
       end else
      if (Player.CBWavPlayMode.ItemIndex = 3) and (Player.WavBox.Items.Count > 0) then
       begin
        Player.WavBox.itemindex := random(Player.WavBox.Items.Count);
        Player.BtWavPlayClick(nil);
       end;
     end;
    FCnt2 := 0;
    FCnt := 0;
    FBufferPntr := @FBuffer^[0];
   end;
 end;

 if FCh = 2 then // stereo output
  begin
   o1 := FVol * o1;
   o2 := FVol * o2;
  end
 else
  begin // mono output
   o1 := FVol * o1 * 2 * (1 - FPan);
   o2 := FVol * o1 * 2 * FPan;
  end;
end;

procedure TWavPlayer.Stop;
begin
 FPMode := wpmPause;
 FCnt2 := 0;
 FCnt := 0;
 FBufferPntr := @FBuffer^[FCnt2];
end;

procedure TWavPlayer.SetPos(const Value: Integer);
begin
 FCnt2 := Value;
 FCnt := 0;
 FBufferPntr := @FBuffer^[FCnt2];
end;

procedure TWavPlayer.Unload;
begin
 FPMode := wpmPause;
 if Assigned(FBuffer) then
  begin
   Dispose(FBuffer);
   FBuffer := nil;
  end;
 FBufferPntr := @FBuffer^[0];
 FCnt := 0;
 FCnt2 := 0;
end;


{ TFmMiniHost }

procedure TFmMiniHost.FormCreate(Sender: TObject);
var
  i, mi               : Integer;
  MenuItem            : TMenuItem;
  PlayList            : TStringList;
  AsioDriverList      : TStrings;
  str                 : string;
  Settings            : TIniFile;
  ContainedVSTPlugins : TStringList;
  RS                  : TResourceStream;
(*
  b        : Byte;
  flt      : array [0..1] of Single;
  x, y     : Integer;
  Line24   : PRGB24Array;
  Line32   : PRGB32Array;
*)
begin
 FDataSection := TCriticalSection.Create;

 FAllowed := False;
 with ToolBarBackground.Picture do
  begin
   Bitmap.TransparentColor := $A8A8A8;
(*
   // not working yet!!!
   case Bitmap.PixelFormat of
    pf24bit:
     for y := 0 to Bitmap.Height - 1 do
      begin
       Line24 := Bitmap.Scanline[y];
       for x := 0 to Bitmap.Width - 1 do
        begin
         flt[1] := 0.9 * flt[0] + 0.1 * (2 * random - 1);
         b := round($F * flt[1]);
         flt[0] := flt[1];
         Line24[x].B := $A8 + b;
         Line24[x].G := $A8 + b;
         Line24[x].R := $A8 + b;
        end;
      end;
    pf32bit:
     for y := 0 to Bitmap.Height - 1 do
      begin
       Line32 := Bitmap.Scanline[y];
       for x := 0 to Bitmap.Width - 1 do
        begin
         flt[1] := 0.9 * flt[0] + 0.1 * (2 * random - 1);
         b := round($F * flt[1]);
         flt[0] := flt[1];
         Line32[x].B := $A8 + b;
         Line32[x].G := $A8 + b;
         Line32[x].R := $A8 + b;
         Line32[x].A := $FF;
        end;
      end;
   end;
( *
*)
  end;
 IBtDropDown.picture.Bitmap.TransparentColor := $A8A8A8;
 IBtDropDown.picture.Bitmap.Transparent := True;
 BorderOnOff.picture.Bitmap.TransparentColor := clBlack;
 BorderOnOff.picture.Bitmap.Transparent := False;
 BorderOptions.Picture.Assign(BorderOnOff.Picture);
 BorderPlayMIDI.Picture.Assign(BorderOnOff.Picture);
 BorderPlayWave.Picture.Assign(BorderOnOff.Picture);
 BorderRecordWave.Picture.Assign(BorderOnOff.Picture);

 Assert(SizeOf(TVSTMidiEvent) = SizeOf(TVstMidiSysexEvent));

 for i := 0 to 2047 do
  begin
   GetMem(FMyEvents.Events[i], SizeOf(TVSTMidiEvent));
   FillChar(FMyEvents.Events[i]^, SizeOf(TVSTMidiEvent), 0);
   with PVstMidiEvent(FMyEvents.Events[i])^ do
    begin
     EventType := etMidi;
     ByteSize := 24;
    end;
  end;

 FMIDIInput  := TMidiInput.Create;
 FMIDIOutput := TMidiOutput.Create;

 FWaveFile := TWavPlayer.Create;
 FPluginLoaded := False;

 Player := TPlayer.Create(Self);

 FPanel := TPanel.Create(Self);
 with FPanel do
  begin
   Parent := Self;
   Top := PnStatus.Height;
  end;

 ClientHeight := PnStatus.Height;
 FmOptions := TFmOptions.Create(Self);
 FmOptions.Host := Self;

 DragAcceptFiles(Self.Handle, True);

{$IFNDEF FPC}
 ININame := GetApplicationDirectory + '\' + ChangeFileExt(GetApplicationFilename, '.ini');
{$ENDIF}

 FMidiFile := TMidiFile.Create(nil);
 FMidiFile.OnMidiEvent := MyMidiEvent;
 FMidiFile.ManualCall := True;
 FRecordState := rsStop;

 try
  MenuItem := TMenuItem.Create(Self);
  MenuItem.RadioItem := True;
  MenuItem.Tag := 0;
  MenuItem.Caption := 'None';
  MenuItem.OnClick := MIDIInChange;
  MIMIDIIn.Add(MenuItem);
  for mi := 0 to FMidiInput.Devices.Count - 1 do
   begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.RadioItem := True;
    MenuItem.Tag := mi + 1;
    MenuItem.Caption := FMidiInput.Devices[mi];
    MenuItem.OnClick := MIDIInChange;
    MIMIDIIn.Add(MenuItem);
   end;
 except
  MessageDlg('ERROR: A serious problem occured with MIDI-In drivers!', mtError, [mbOK], 0);
 end;

 try
  MenuItem := TMenuItem.Create(Self);
  MenuItem.RadioItem := True;
  MenuItem.Tag := 0;
  MenuItem.Caption := 'None';
  MenuItem.OnClick := MIDIOutChange;
  MIMIDIOut.Add(MenuItem);
  for mi := 0 to FMidiOutput.Devices.Count - 1 do
   begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.RadioItem := True;
    MenuItem.Tag := mi + 1;
    MenuItem.Caption := FMidiOutput.Devices[mi];
    MenuItem.OnClick := MIDIOutChange;
    MIMIDIOut.Add(MenuItem);
   end;
 except
  MessageDlg('ERROR: A serious problem occured with MIDI-Out drivers', mtError, [mbOK], 0);
 end;

 try
  AsioDriverList := ASIOHost.DriverList;
 except
  AsioDriverList := nil;
  MessageDlg('ERROR: ASIO driver list could not be received! Application Terminated!', mtError, [mbOK], 0);
  Application.Terminate;
 end;

 if AsioDriverList <> nil then
  for i := 0 to AsioDriverList.Count - 1 do
   begin
    MenuItem := TMenuItem.Create(Self);
    MenuItem.RadioItem := True;
    MenuItem.Tag := i;
    MenuItem.Caption := AsioDriverList.Strings[i];
    MenuItem.OnClick := ASIOChange;
    MIAsioDriver.Add(MenuItem);
   end;

 if AsioDriverList.Count = 0 then
 begin
  MessageDlg('ERROR: No ASIO Driver present! Application Terminated!', mtError, [mbOK], 0);
  Application.Terminate;
 end;

 FMidiInput.OnMidiData := MidiData;
 FMidiInput.OnSysExData := SysExData;

 Settings := TIniFile.Create(ININame);
 try
  i := Settings.ReadInteger('Audio', 'ASIO Driver', -1);
  if i = -1 then i := AsioDriverList.IndexOf('ASIO4ALL v2');
  if i = -1 then i := AsioDriverList.IndexOf('ASIO4ALL');
  if (i < 0) or (i >= AsioDriverList.count) then i := 0;
  MIAsioDriver.Items[i].Checked := True;
  try
   ASIOChange(MIAsioDriver.Items[i]);
  except
  end;
  i := Settings.ReadInteger('Audio', 'Output Channel', 0);
  if (i >= 0) and (i < MIASIOOutputChannel.Count) and (MIASIOOutputChannel.Count <> 0)
   then
    try
     MIASIOOutputChannel.Items[i].Checked := True;
     MIASIOOutputChannel.Items[i].Click;
    except
    end;

  i := Settings.ReadInteger('Audio', 'Input Channel', 0);
  if (i >= 0) and (i < MIASIOInputChannel.Count) and (MIASIOInputChannel.Count <> 0)
   then
    try
     MIASIOInputChannel.Items[i].Checked := True;
     MIASIOInputChannel.Items[i].Click;
    except
    end;

  WaveFile.FFilename := Settings.ReadString('Audio', 'File', '');

  i := Settings.ReadInteger('Audio', 'Record Bits', 16);
  case i of
   16 : Player.CbRecordFormat.ItemIndex := 0;
   else Player.CbRecordFormat.ItemIndex := 1;
  end;

  MIShowPreset.Checked := Settings.ReadBool('Layout', 'ShowPresetInTitleBar', True);
  FDirPlugin := Settings.ReadString('General', 'Plugin Directory', '');
  FDirPreset := Settings.ReadString('General', 'Preset Directory', '');
  FDirWave   := Settings.ReadString('General', 'Wave Directory', '');
  FDirMidi   := Settings.ReadString('General', 'Midi Directory', '');

  // clear playlists
  Player.MidiBox.Clear;
  Player.WavBox.Clear;

  // load playlists
  PlayList := TStringList.Create;
  try
   Settings.ReadSection('Playlist MIDI', PlayList);
   for i := 0 to PlayList.Count - 1 do AddMID(PlayList[i]);
   Settings.ReadSection('Playlist WAV', PlayList);
   for i := 0 to PlayList.Count - 1 do AddWAV(PlayList[i]);
  finally
   PlayList.Free;
  end;

  Player.CBMidiPlayMode.ItemIndex := Settings.ReadInteger('MIDI', 'LoopMode', 1);
  Player.CBWavPlayMode.ItemIndex := Settings.ReadInteger('Audio', 'LoopMode', 1);
  WaveFile.looped := Player.CBWavPlayMode.itemindex = 1;
  FMidiFile.Filename := Settings.ReadString('MIDI', 'LastFile', '');
  if (FMidiFile.filename <> '') and fileexists(FMidiFile.filename)
   and (uppercase(extractfileext(FMidiFile.filename))='.MID') then FMidiFile.ReadFile;
  MidiPlaying := False;

  MIDownMixToStereo.Checked := Settings.ReadBool('VST', 'DownmixStereo', False);
  MIMidiThru.Checked := Settings.ReadBool('VST', 'MIDIThru', False);

  str := Settings.ReadString('VST', 'LastPlugin', '');
  i := Settings.ReadInteger('VST', 'LastProgram', 0);
 finally
  Settings.Free;
 end;

 LoadWAV(WaveFile.Filename);

 {$IFNDEF FPC}
 ContainedVSTPlugins := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'DLL', @EnumNamesFunc, LongWord(ContainedVSTPlugins));

  if (ParamCount > 0) and (FileExists(Paramstr(1))) then
   begin
    LoadPlugin(Paramstr(1));
    VSTHost[0].OnProcessEvents := ProcessEvents;
   end
  else if ContainedVSTPlugins.Count > 0 then
   begin
    RS := TResourceStream.Create(HInstance, ContainedVSTPlugins[0], 'DLL');
    try
     LoadPlugin(RS);
    finally
     FreeAndNil(RS);
    end;
   end
  else if FileExists(str) then
   begin
    FLoadProg := i;
    LoadPlugin(str, i);
    VSTHost[0].OnProcessEvents := ProcessEvents;
   end;

 finally
  FreeAndNil(ContainedVSTPlugins);
 end;
 {$ENDIF}
end;

procedure TFmMiniHost.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 FProcessing := False;
 FAllowed := False;
 try
  StopAudio;
 except
 end;

 // free about form if necessary
 if Assigned(FAboutForm)
  then FreeAndNil(FAboutForm);

 with TIniFile.Create(ININame) do
  try
   EraseSection('Playlist MIDI');
   EraseSection('Playlist WAV');
   for i := 0 to Player.MidiBox.Items.Count - 1
    do WriteString('Playlist MIDI', PShortStr(Player.MidiBox.Items.Objects[i])^, '');
   for i := 0 to Player.WavBox.Items.Count - 1
    do WriteString('Playlist WAV', PShortStr(Player.WavBox.Items.Objects[i])^, '');
   WriteInteger('General', 'Timer', WaveTimer.Interval);
   WriteInteger('Layout', 'MainWindow X', Left);
   WriteInteger('Layout', 'MainWindow Y', Top);
   WriteInteger('Layout', 'SettingsWindow X', FmOptions.Left);
   WriteInteger('Layout', 'SettingsWindow Y', FmOptions.Top);
   WriteBool('Layout', 'SettingsWindow Visible', FmOptions.Showing);
   WriteString('General', 'Plugin Directory', FDirPlugin);
   WriteString('General', 'Preset Directory', FDirPreset);
   WriteString('General', 'Wave Directory', FDirWave);
   WriteString('General', 'Midi Directory', FDirMidi);
   WriteInteger('Layout', 'PlayerWindow X', Player.Left);
   WriteInteger('Layout', 'PlayerWindow Y', Player.Top);
   WriteBool('Layout', 'PlayerWindow Visible', Player.Showing);
   WriteBool('VST', 'DownmixStereo', MIDownMixToStereo.Checked);
   WriteBool('VST', 'MIDIThru', MIMidiThru.Checked);
   WriteBool('VST', 'UseMouseWheel', MIUseMouseWheel.Checked);
   WriteBool('MIDI', 'MidiFileOnlyChannel1', Player.CbOnlyChannel1.Checked);
   WriteInteger('Audio', 'ASIO Driver', FCurrentASIO);
   WriteInteger('Audio', 'Output Channel', FCurrentOutputChannel);
   WriteInteger('Audio', 'Input Channel', FCurrentInputChannel);
   if Player.WavBox.Items.Count = 0 then WAVEFile.Filename := '';//c
   WriteString('Audio', 'File', WAVEFile.Filename);
   WriteInteger('Audio', 'VST Volume', FmOptions.SbVSTVolume.position);
   WriteInteger('Audio', 'Overall Volume', FmOptions.SbOverallVolume.position);
   WriteInteger('Audio', 'Input Volume', FmOptions.SbInputVolume.position);
   WriteInteger('Audio', 'WAV Volume', FmOptions.SbWavVolume.position);
   WriteInteger('VST', 'Tempo', FmOptions.SbTempo.position);
   WriteString('VST', 'LastPlugin', VSTHost[0].DLLFilename);
   WriteInteger('VST', 'LastProgram', FCurProg);
   if Player.MidiBox.Items.Count = 0 then FMidiFile.Filename := '';//c
   WriteString('MIDI', 'LastFile', FMidiFile.Filename);
   WriteInteger('MIDI', 'LoopMode', Player.CBMidiPlayMode.itemindex);
   WriteInteger('Audio', 'LoopMode', Player.CBWavPlayMode.itemindex);
   WriteBool('Layout', 'ShowPresetInTitleBar', MIShowPreset.Checked);
   WriteInteger('MIDI', 'MIDI-In Driver', FCurrentMIDIIn);
   WriteInteger('MIDI', 'MIDI-Out Driver', FCurrentMIDIOut);
   case Player.CbRecordFormat.ItemIndex of
     0: i := 16;
    else i := 32;
   end;
   WriteInteger('Audio', 'Record Bits', i);
 finally
  Free;
 end;

 if FPluginLoaded then ClosePlugin;

 if Assigned(FWavWriter)
  then FreeAndNil(FWavWriter);

 try
  FMidiInput.CloseAll;
  FMidiOutput.CloseAll;
 except
 end;

 FreeAndNil(FMidiInput);
 FreeAndNil(FMidiOutput);
 FreeAndNil(FMidiFile);
 FreeAndNil(FWaveFile);
 FNumIn := 0;
 FNumOut := 0;
 for i := 0 to Length(FVSTBufOut) - 1 do SetLength(FVSTBufOut[i], 0);
 for i := 0 to Length(FVSTBufIn) - 1 do SetLength(FVSTBufIn[i], 0);
 SetLength(FVSTBufOut, 0);
 SetLength(FVSTBufIn, 0);
 SetLength(FVSTPinProps, 0);
 for i := 0 to 2047
  do FreeMem(FMyEvents.Events[i]);

 FreeAndNil(FDataSection);
end;

procedure TFmMiniHost.FormShow(Sender: TObject);
var
  i : Integer;
begin
 with TIniFile.Create(ININame) do
  try
   MIUseMouseWheel.Checked := ReadBool('VST', 'UseMouseWheel', True);
   Player.CbOnlyChannel1.Checked := ReadBool('MIDI', 'MidiFileOnlyChannel1', False);
   FmOptions.SbOverallVolume.position := ReadInteger('Audio', 'Overall Volume', 100);
   FmOptions.SbVSTVolume.position := ReadInteger('Audio', 'VST Volume', 100);
   FmOptions.SbInputVolume.position := ReadInteger('Audio', 'Input Volume', 100);
   FmOptions.SbWavVolume.position := ReadInteger('Audio', 'WAV Volume', 100);
   FmOptions.SbTempo.Position := ReadInteger('VST', 'Tempo', 120);

   FmOptions.Left := ReadInteger('Layout', 'SettingsWindow X', Left - 100);
   FmOptions.Top := ReadInteger('Layout', 'SettingsWindow Y', Top);
   if FmOptions.Left < 0 then FmOptions.Left := 0;
   if FmOptions.Top < 0 then FmOptions.Top := 0;
   if FmOptions.Left > Screen.Width - 20 then FmOptions.Left := Screen.Width - 20;
   if FmOptions.Top > Screen.Height - 20 then FmOptions.Top := Screen.Height - 20;

   Player.Left := ReadInteger('Layout', 'PlayerWindow X', Left - 100);
   Player.Top := ReadInteger('Layout', 'PlayerWindow Y', Top);
   if Player.Left < 0 then Player.Left := 0;
   if Player.Top  < 0 then Player.Top := 0;
   if Player.Left > Screen.Width  - 20 then Player.Left := Screen.Width  - 20;
   if Player.Top  > Screen.Height - 20 then Player.Top  := Screen.Height - 20;
   FmOptions.SbTempoChange(nil);
   if ReadBool('Layout', 'SettingsWindow Visible', False) then
    begin
     FmOptions.Show;
     FmOptions.setfocus;
    end;
   if ReadBool('Layout', 'PlayerWindow Visible', False) then
    begin
     Player.Show;
     Player.SetFocus;
    end;

   MIAlwaysOnTop.Checked := not ReadBool('Layout', 'AlwaysOnTop', False);
   MIAlwaysOnTopClick(Sender);
   i := ReadInteger('MIDI', 'MIDI-In Driver', 0);
   if (i < 0) or (i > FMidiInput.Devices.Count) then i := 0;
   FCurrentMIDIIn := i;
   MIMIDIIn.Items[i].Click;
   i := ReadInteger('MIDI', 'MIDI-Out Driver', 0);
   if (i < 0) or (i > FMidiOutput.Devices.Count) then i := 0;
   FCurrentMIDIOut := i;
   MIMIDIOut.Items[i].Click;
  finally
   Free;
  end;
 WaveTimer.Enabled := True;

 if FLoadProg >= 0 then
  begin
   VSTHost[0].CurrentProgram := FLoadProg;
   FLoadProg := -1;
  end;

 if PnStatus.Visible then PnStatus.SetFocus;

end;

procedure TFmMiniHost.StartAudio;
var
  i : Integer;
begin
 if ASIOHost.Active then Exit;

 // deactivate ASIO host
 ASIOHost.Active := False;

 // update plugins
 VSTHost.BlockSize := ASIOHost.BufferSize;
 for i := 0 to VSTHost.VSTPlugIns.Count - 1 do
  with VSTHost[i] do
   if Active then
    begin
     VstCanDo('sendVstTimeInfo');
     VstCanDo('receiveVstTimeInfo');
    end;

 // activate ASIO host
 with VSTHost.VstTimeInfo
  do Flags := Flags + [vtiTransportPlaying];
 ASIOHost.Active := True;
end;

procedure TFmMiniHost.StopAudio;
begin
 // exclude transport playing flag
 if Assigned(VSTHost) then
  with VSTHost.VstTimeInfo
   do Flags := Flags - [vtiTransportPlaying];

 // deactivate ASIO host
 if not ASIOHost.Active then Exit;
 ASIOHost.Active := False;
end;

procedure TFmMiniHost.LoadWAV(const FileName: string);
begin
 WaveFile.Unload;

 if FileExists(FileName) then
  begin
   WaveFile.Load(FileName);
   WaveFile.SampleRate := ASIOHost.SampleRate;
  end;
end;

procedure TFmMiniHost.ClosePlugin;
var
  i : Integer;
begin
 FPanel.Height := 0;
 FMidiFile.StopPlaying;
 MidiPlaying := False;
 WaveTimer.Enabled := False;
 FProcessing := False;

 StopAudio;
 MIPanicClick(nil);
 FRecordState := rsStop;

 if Assigned(FWavWriter)
  then FreeAndNil(FWavWriter);

 if (VSTHost[0].DLLFileName <> '') and VSTHost[0].Active then
  with VSTHost[0] do
   begin
    CloseEdit;
    Close;
    Unload;
   end;

 FNumIn := 0;
 FNumOut := 0;
 for i := 0 to Length(FVSTBufOut) - 1 do SetLength(FVSTBufOut[i], 0);
 for i := 0 to Length(FVSTBufIn) - 1 do SetLength(FVSTBufIn[i], 0);
 SetLength(FVSTBufOut, 0);
 SetLength(FVSTBufIn, 0);
 SetLength(FVSTPinProps, 0);

 MILoadPreset.Enabled := False;
 MISavePreset.Enabled := False;
 MILoadBank.Enabled := False;
 MISaveBank.Enabled := False;
 FPluginLoaded := False;
 PresetBox.clear;
 WaveTimer.Enabled := True;
end;

procedure TFmMiniHost.BuildPresetList;
var
  m    : TMenuItem;
  n, i : Integer;
  s    : AnsiString;
begin
 PresetBox.clear;
 n := VSTHost[0].numPrograms;

 for i := 0 to n - 1 do
  begin
   VSTHost[0].GetProgramNameIndexed(-1, i, s);
   m := TMenuItem.Create(Self);
   m.Caption := string(s);
   m.OnClick := SetPreset;
   m.Tag := i;
  {$IFNDEF FPC}
   if (i > 0) and (i mod 256 <> 0) and (i mod 32 = 0)
    then m.break := mbBarBreak;
  {$ENDIF}
   s := AnsiString(IntToStr(i));
   if i < 10 then s := '00' + s else
   if i < 100 then s := '0' + s;
   PresetBox.AddItem(string(s) + ': ' + M.Caption, nil);
  end;

 if n >= 0 then PresetBox.ItemIndex := FCurProg;
end;

procedure TFmMiniHost.StopProcessingAndClosePlugin;
begin
 WaveTimer.Enabled := False;
 FProcessing := False;
 StopAudio;
 Sleep(2);
 ClosePlugin;
 Sleep(2);
end;

procedure TFmMiniHost.LoadPlugin(const VSTDll: TFileName; const DefaultProgram: Integer = 0);
var
  PresetFileName : TFileName;
  {$IFDEF LoadPluginFromStream}
  FileStream     : TFileStream;
  {$ENDIF}
begin
 if not FileExists(VSTDll) then Exit;

 StopProcessingAndClosePlugin;

 VSTHost.BlockSize := ASIOHost.BufferSize;
 {$IFDEF LoadPluginFromStream}
 FileStream := TFileStream.Create(VSTDll, fmOpenRead);
 try
  VSTHost[0].LoadFromStream(FileStream);
 finally
  FreeAndNil(FileStream);
 end;
 {$ELSE}
 VSTHost[0].LoadFromFile(VSTDll);
 {$ENDIF}

 try
  VSTHost[0].Active := True;
 except
  MessageDlg('ERROR: ' + VSTDll + ' is not a valid VST plugin!', mtError, [mbOK], 0);
  VSTHost[0].Active := False;
  VSTHost[0].DLLFilename := '';
  FPanel.Height := 0;
  Exit;
 end;

 // try loading possible default bank
 PresetFileName := ChangeFileExt(VSTDll, '.fxb');
 if FileExists(PresetFileName) then
  try
   VSTHost[0].LoadBank(PresetFileName);
  except
  end;

 // try loading possible default program
 PresetFileName := ChangeFileExt(VSTDll, '.fxp');
 if FileExists(PresetFileName) then
  try
   VSTHost[0].LoadPreset(PresetFileName);
  except
  end;

 BuildChannelBuffers;

 ShowVSTPlugin(DefaultProgram);
end;

{$IFNDEF FPC}
procedure TFmMiniHost.LoadPlugin(const Stream: TStream; const DefaultProgram: Integer = 0);
begin
 StopProcessingAndClosePlugin;

 VSTHost.BlockSize := ASIOHost.BufferSize;
 VSTHost[0].LoadFromStream(Stream);

 try
  VSTHost[0].Active := True;
 except
  VSTHost[0].Active := False;
  VSTHost[0].DLLFilename := '';
  FPanel.Height := 0;
  Exit;
 end;

 BuildChannelBuffers;

 ShowVSTPlugin(DefaultProgram);
end;
{$ENDIF}

procedure TFmMiniHost.BuildChannelBuffers;
var
  i : Integer;
begin
 SetLength(FVSTBufIn,  max(VSTHost[0].numInputs,  2), ASIOHost.BufferSize);
 SetLength(FVSTBufOut, max(VSTHost[0].numOutputs, 2), ASIOHost.BufferSize);
 FNumIn := VSTHost[0].numInputs;
 FNumOut := VSTHost[0].numOutputs;
 SetLength(FVSTPinProps, FNumOut);
 for i := 0 to FNumOut - 1
  do FVSTPinProps[i] := VSTHost[0].GetOutputProperties(i);
end;

procedure TFmMiniHost.ShowVSTPlugin(const DefaultProgram: Integer = 0);
var
  rct : ERect;
begin
 with VSTHost[0] do
  try
   ShowEdit(FPanel);

   FTitle := GetVendorString + ' ' +  GetEffectName;
   BuildPresetList;

   if (effFlagsHasEditor in VSTHost[0].EffectOptions) then
    begin
     rct := EditGetRect;
     FPanel.Width  := Rct.Right - Rct.Left;
     FPanel.Height := Rct.Bottom - Rct.Top;
     FPanel.Top    := PnStatus.Height;

     // set client width
     if FPanel.Width < 560 then
      begin
       ClientWidth := 560;
       FPanel.Left := (560 - FPanel.Width) div 2;
      end
     else
      begin
       ClientWidth := FPanel.Width;
       FPanel.left := 0;
      end;
     ClientHeight := FPanel.Height + PnStatus.Height;

     // find background color ASAP
     FColBack := False;
    end
   else
    begin
     FPanel.Width  := 560;
     FPanel.Height := 480;
    end;
  except
   raise;
  end;

 MILoadPreset.Enabled := True;
 MILoadBank.Enabled   := True;
 MISavePreset.Enabled := True;
 MISaveBank.Enabled   := True;
 FProcessing          := True;
 StartAudio;
 WaveTimer.Enabled    := True;
 MIRenamePreset.Enabled := VSTHost[0].numPrograms >= 1;

 FPluginLoaded := True;
 FAllowed := True;
 Caption := string(FTitle);
 Left := Screen.Width div 2 - Width div 2;
 Top := Screen.Height div 2 - Height div 2;
 VSTHost[0].CurrentProgram := DefaultProgram;
 FmOptions.SbTempoChange(nil);
end;

function TFmMiniHost.FindBackgroundColor: TColor;
var
  BMP     : TBitmap;
  SCL     : PRGB24Array;
  R, G, B : Integer;
  x       : Integer;
begin
 // fill background
 Application.ProcessMessages;

 {$IFNDEF FPC}
 BMP := TBitmap.Create;
 BMP.PixelFormat := pf24bit;
 with BMP do
  try
   VSTHost[0].RenderEditorToBitmap(BMP);
   SCL := BMP.ScanLine[0];
   R := 0; G := 0; B := 0;

   for x := 0 to BMP.Width - 1 do
    begin
     R := R + SCL[x].R;
     G := G + SCL[x].G;
     B := B + SCL[x].B;
    end;

   Result := RGB(R div BMP.Width, G div BMP.Width, B div BMP.Width);
  finally
   FreeAndNil(BMP);
  end;
 {$ELSE}
 Result := $808080;
 {$ENDIF}
end;

procedure TFmMiniHost.VSTHostAudioMasterIdle(Sender: TVSTPlugin);
begin
 Sender.Idle;
end;

procedure TFmMiniHost.VSTHostAudioMasterNeedIdle(Sender: TVSTPlugin);
begin
 Sender.EditIdle;
end;

procedure TFmMiniHost.MIDIInChange(Sender: TObject);
begin
 if FMidiInput.Devices.Count = 0
  then Exit;
 FMidiInput.OnMidiData := MidiData;
 (Sender as TMenuItem).Checked := True;
 try
  FMidiInput.Close(FCurrentMIDIIn);
 except
 end;
 FCurrentMIDIIn := (sender as TMenuItem).Tag;
 MIMIDIIn.Items[FCurrentMIDIIn].Checked := True;
 try
  if FCurrentMIDIIn > 0
   then FMidiInput.Open(FCurrentMIDIIn - 1);
 except
 end;
end;

// By Daniel:  Note that Dav_MidiIO midiInProc midiInCallback is called
// concurrently by different service threads
procedure TFmMiniHost.MidiData(const aDeviceIndex: Integer; const aStatus, aData1, aData2: Byte);
begin
 if aStatus = $FE then Exit; // ignore active sensing
 if (not Player.CbOnlyChannel1.Checked) or ((aStatus and $0F) = 0) then
  begin
   if (aStatus and $F0) = $90
    then NoteOn(aStatus, aData1, aData2) //ok
    else
   if (aStatus and $F0) = $80
    then NoteOff(aStatus, aData1)
    else AddMidiData(aStatus, aData1, aData2);
  end;
end;

procedure TFmMiniHost.SysExData(const aDeviceIndex: integer; const aStream: TMemoryStream);
begin
 FDataSection.Acquire;
 try
  if FMDataCnt > 2046 
   then Exit;

  Inc(FMDataCnt);
  with PVstMidiSysexEvent(FMyEvents.events[FMDataCnt - 1])^ do
   begin
    EventType := etSysEx;
    ByteSize := 24;
    DeltaFrames := 0;
    Flags := [];
    dumpBytes := aStream.Size;
    sysexDump := aStream.Memory;
    Reserved1 := 0;
    Reserved2 := 0;
   end;
 finally
  FDataSection.Release;
 end; 
end;

procedure TFmMiniHost.ASIOHostLatencyChanged(Sender: TObject);
begin
 VSTHost.LatencyInput := ASIOHost.InputLatency;
 VSTHost.LatencyOutput := ASIOHost.OutputLatency;
end;

procedure TFmMiniHost.ASIOHostUpdateSamplePos(Sender: TObject;
  SamplePosition: Int64);
begin
 VSTHost.VstTimeInfo.SamplePos := SamplePosition;
end;

procedure TFmMiniHost.ASIOHostSampleRateChanged(Sender: TObject);
begin
 MIStopRecordingClick(nil);
 if VSTHost[0].Active
  then VSTHost[0].SetSampleRate(ASIOHost.SampleRate);
 VSTHost.VstTimeInfo.SampleRate := ASIOHost.SampleRate;
 WaveFile.samplerate := ASIOHost.SampleRate;
end;

procedure TFmMiniHost.SetChannel(Sender: TObject);
begin
 (Sender as TMenuItem).Checked := True;
 FCurrentOutputChannel := (Sender as TMenuItem).Tag;
 if ASIOHost.Active then
 begin
  StopAudio;
  StartAudio;
 end else FProcessing := False;
 FmOptions.LbOutputs.Caption := 'Outputs: ' + MIASIOOutputChannel.Items[FCurrentOutputChannel].Caption;
end;

procedure TFmMiniHost.ASIOChange(Sender: TObject);
var
  Channel, j: Integer;
  m: TMenuItem;
begin
 // make sure the sender was a TMenuItem, then check this item!
 Assert(Sender is TMenuItem);
 TMenuItem(Sender).Checked := True;

 // set flag to disable internal processing
 FProcessing := False;

 // do panic!
 MIPanicClick(nil);

 // stop MIDI playback
 FMidiFile.StopPlaying;
 MidiPlaying := False;

 // stop audio
 StopPlayback2Click(nil);
 StopAudio;

 // get new driver index
 FCurrentASIO := (Sender as TMenuItem).Tag;
 if FCurrentASIO >= 0 then
  begin
   // ensure the driver has some time to stop actually!
   Sleep(10);

   // change driver index
   ASIOHost.DriverIndex := FCurrentASIO;

   // delete all channels
   for Channel := 0 to MIASIOOutputChannel.Count - 1 do MIASIOOutputChannel.Delete(0);
   for Channel := 0 to MIASIOInputChannel.Count - 1 do MIASIOInputChannel.Delete(0);

   // add new output channel pairs
   j := 0;
   for Channel := 0 to ASIOHost.OutputChannelCount - 1 do
    if not Odd(Channel) then
     begin
      m := TMenuItem.Create(Self);
      m.RadioItem := True;
      m.Tag := j;
      Inc(j);
      m.OnClick := SetChannel;
      if Channel < ASIOHost.OutputChannelCount - 1
       then m.Caption := string(ASIOHost.OutputChannelInfos[Channel].Name +
         AnsiString(' / ') + ASIOHost.OutputChannelInfos[Channel + 1].Name)
       else m.Caption := string(ASIOHost.OutputChannelInfos[Channel].Name);
      MIASIOOutputChannel.Add(m);
     end;

   // add new input channel pairs
   m := TMenuItem.Create(Self);
   m.RadioItem := True;
   m.Tag := 0;
   m.OnClick := SetChannelI;
   m.Caption := 'None';
   MIASIOInputChannel.Add(m);
   j := 1;
   for Channel := 0 to ASIOHost.InputChannelCount - 1 do
    if not Odd(Channel) then
     begin
      m := TMenuItem.Create(Self);
      m.RadioItem := True;
      m.Tag := j;
      inc(j);
      m.OnClick := SetChannelI;
      if Channel < ASIOHost.InputChannelCount - 1
       then m.Caption := string(ASIOHost.InputChannelInfos[Channel].Name +
         AnsiString(' / ') + ASIOHost.InputChannelInfos[Channel + 1].Name)
       else m.Caption := string(ASIOHost.InputChannelInfos[Channel].Name);
      MIASIOInputChannel.Add(m);
     end;

   MIASIOInputChannel.Items[0].Click;
   if ASIOHost.OutputChannelCount > 0
    then MIASIOOutputChannel.Items[0].Click;
  end;

 // update options form 
 with FmOptions do
  begin
   LbASIODriver.Caption := 'ASIO Driver: ' + ASIOHost.DriverName;
   if MIASIOOutputChannel.Count > 0
    then LbOutputs.Caption := 'Outputs: ' + MIASIOOutputChannel.Items[0].Caption
    else LbOutputs.Caption := 'Outputs: None';
   if MIASIOInputChannel.Count > 0
    then LbInputs.Caption := 'Inputs: ' + MIASIOInputChannel.Items[0].Caption
    else LbInputs.Caption := 'Inputs: None';
   if ASIOHost.OutputChannelCount > 0
    then LbFormat.Caption := 'Format: ' + IntToStr(ASIOHost.OutputChannelInfos[0].SampleType) + ' ' + ChannelTypeToString(ASIOHost.OutputChannelInfos[0].SampleType)
    else LbFormat.Caption := 'Format: None';
   LbBufferSize.Caption := 'Buffersize: ' + IntToStr(ASIOHost.BufferSize);
   LbSampleRate.Caption := 'Samplerate: ' + IntToStr(round(ASIOHost.SampleRate));
  end;

 // reset ASIO driver and start processing...
 ASIOHostReset(Sender);
 StartAudio;
 FProcessing := True;
end;

procedure TFmMiniHost.MIPanicClick(Sender: TObject);
var
  Ch, Note: word;
begin
 FDataSection.Acquire;
 try
  FMDataCnt := 0;
  for Note := 0 to 127 do AddMidiData($80, Note, 0);
  for Ch := 0 to 15 do AddMidiData($B0 + Ch, 123, 0);
 finally 
  FDataSection.Release;
 end; 
end;

procedure TFmMiniHost.MILoadPresetClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   Filename := '*.fxp';
   InitialDir := FDirPreset;
   DefaultExt := '.fxp';
   Options := [ofAllowMultiSelect, ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   Filter := 'preset files (*.fxp)|*.fxp';
   Title := 'Select a preset';
   if Execute then
    begin
     FDirPreset := extractfiledir(filename);
     LoadPresets(Files);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.LoadPresets(Files: TStrings);
var
  i, j, k: Integer;
  s: string;
begin
 MIPanicClick(nil);
 WaveTimer.Enabled := False;
 j := FCurProg;
 for i := 0 to Files.Count - 1 do
  begin
   if i > 0 then VSTHost[0].CurrentProgram := j + i;
   try
    VSTHost[0].LoadPreset(Files[i]);
   except
    MessageDlg('ERROR: Preset file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
    WaveTimer.Enabled := True;
    Exit;
   end;
   k := VSTHost[0].CurrentProgram;
   s := IntToStr(k);
   if k < 10 then s := '00' + s else
   if k < 100 then s := '0' + s;
  end;
 WaveTimer.Enabled := True;
end;

procedure TFmMiniHost.MISavePresetClick(Sender: TObject);
var
  s2: string;
begin
 MIPanicClick(nil);
 Sleep(2);
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.fxp';
   filename := '*.fxp';
   Filter := 'preset files (*.fxp)|*.fxp';
   Title := 'Select a preset';
   InitialDir := FDirPreset;
   Options := [ofForceShowHidden];
{$IFNDEF FPC}
   Ctl3D := False;
{$ENDIF}

   s2 := PresetBox.Items[PresetBox.ItemIndex];
   s2 := Copy(s2, 6, Length(s2) - 5);
{$IFNDEF FPC}
   Filename := MakeGoodFileName(s2) + '.fxp';
{$ENDIF}

   if Execute then
    begin
     VSTHost[0].SavePreset(FileName);
     FDirPreset := extractfiledir(filename);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MILoadBankClick(Sender: TObject);
begin
 WaveTimer.Enabled := False;
 Sleep(2);
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.fxb';
   filename := '*.fxb';
   Filter := 'bank files (*.fxb)|*.fxb';
   Title := 'Select a bank';
   InitialDir := FDirPreset;

   Options := [ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}

   if Execute then
    begin
     FDirPreset := ExtractFileDir(filename);
     try
      VSTHost[0].LoadBank(Filename);
     except
      MessageDlg('ERROR: Bank file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
      WaveTimer.Enabled := True;
     end;
     BuildPresetList;
    end;
  finally
   Free;
   FCurProg := 0;
   VSTHost[0].CurrentProgram := 0;
   PresetBox.ItemIndex := 0;
   WaveTimer.Enabled := True;
  end;
end;

procedure TFmMiniHost.MISaveBankClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   FileName := '*.fxb';
   DefaultExt := '.fxb';
   Filter := 'bank files (*.fxb)|*.fxb';
   Title := 'Select a bank';
   InitialDir := FDirPreset;
   Options := [ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   if Execute then
    begin
     FDirPreset := ExtractFileDir(filename);
     VSTHost[0].SaveBank(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MIVSTClosePluginClick(Sender: TObject);
begin
 WaveFile.Stop;
 ClosePlugin;
end;

procedure TFmMiniHost.MIVSTLoadPluginClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   filename := '*.dll';
   Filter := 'VST Plugins (*.dll)|*.dll';
   Options := [ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   Title := 'Select a VST plugin';
   InitialDir := FDirPlugin;
   if Execute then
    begin
     FDirPlugin := ExtractFileDir(Filename);
     LoadPlugin(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.LoadWAVFile;
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   Filename := '*.wav;*.wpl';
   Filter := 'WAV files and playlists (*.wav;*.wpl)|*.wav;*.wpl|WAV files (*.wav)|*.wav|WAV playlists (*.wpl)|*.wpl';
   FilterIndex := 0;
   InitialDir := FDirWave;
   Options := [ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   Title := 'Select a WAV file';
   if Execute then
    begin
     FDirWave := ExtractFileDir(Filename);
     AddWAV(FileName);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.StartPlayback2Click(Sender: TObject);
begin
 if not FileExists(Wavefile.filename) then Exit;
 WaveFile.play;
end;

procedure TFmMiniHost.SetPreset(Sender: TObject);
begin
 MIPanicClick(nil);
 VSTHost[0].CurrentProgram := (Sender as TMenuItem).Tag;
end;

procedure TFmMiniHost.MyMidiEvent(event: PMidiEvent);
begin
 with Event^ do
  if (Event and $F0) = $90 then NoteOn(event, data1, data2) else
  if (Event and $F0) = $80 then NoteOff(event, data1)
   else AddMidiData(event, data1, data2);
end;

procedure TFmMiniHost.StartPlayback1Click(Sender: TObject);
begin
 MIPanicClick(nil);
 FMidiFile.StartPlaying;
 MidiPlaying := True;
end;

procedure TFmMiniHost.RecordWAVFileSelect;
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   InitialDir := FDirWave;
   filename := '*.wav';
   Filter := 'WAV files (*.wav)|*.wav';
   Title := 'Select a WAV file';
   Options := [ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}
   if Execute then
    begin
     FDirWave := extractfiledir(filename);
     Player.LbRecordFile.Caption := filename;
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MIStartRecordingClick(Sender: TObject);
var
  s : string;
  i : Integer;
begin
 if Assigned(FWavWriter)
  then FreeAndNil(FWavWriter);

 case Player.CbRecordFormat.ItemIndex of
    0 : i := 16;
   else i := 32;
 end;
 s := Player.LbRecordFile.Caption;
 if s = '<none>' then
 begin
  RecordWavFileSelect;
  s := Player.LbRecordFile.Caption;
  if (s = '<none>') or (s = '') then Exit;
 end;
 FTotalFrames := 0;
 if Player.CbRecInMono.Checked
  then FWavWriter := TWavWriter.Create(s, round(ASIOHost.Samplerate), 1, i)
  else FWavWriter := TWavWriter.Create(s, round(ASIOHost.Samplerate), 2, i);
 FRecordState := rsRecord;
end;

procedure TFmMiniHost.MIDIOutChange(Sender: TObject);
begin
 (Sender as TMenuItem).Checked := True;
 try
  FMidiOutput.Close(FCurrentMIDIOut);
 except
 end;
 FCurrentMIDIOut := (sender as TMenuItem).Tag;
 MIMidiOut.Items[FCurrentMIDIOut].Checked := True;
 try
  if FCurrentMIDIOut > 0
   then FMidiOutput.Open(FCurrentMIDIOut - 1);
 except
 end;
end;

procedure TFmMiniHost.WaveTimerTimer(Sender: TObject);
var
  s2, s : AnsiString;
  i     : Integer;
  e     : single;
begin
 if WaveFile.FPMode > wpmPause then
  begin
   i := round(100 * WaveFile.FCnt2 / (WaveFile.Size - 2));
   Player.LbWavPosition.caption := 'position: ' + IntToStr(i) +' %';
   Player.SbWavPosition.position := i;
  end;

 BorderOnOff.Visible := FProcessing;
 BorderOptions.Visible := FmOptions.Showing;
 BorderPlayMIDI.Visible := midiplaying;
 BorderPlayWave.Visible := not (WaveFile.FPMode = wpmPause);
 BorderRecordWave.Visible := (FRecordState = rsRecord);

 case FRecordState of
  rsRecord: Player.LbStatus.caption := 'status: recording';
   rsPause: Player.LbStatus.caption := 'status: paused';
       else Player.LbStatus.caption := 'status: stopped';
 end;

 if FRecordState > rsStop then
  begin
   e := FTotalFrames / ASIOHost.SampleRate;
   Player.LbStatus.Caption :=
    Player.LbStatus.Caption + ' (time: '
    + FloatToStrF(e, ffFixed, 4, 2) + ' sec, size: ' + IntToStr(
     round(e * FWavWriter.Format.nAvgBytesPerSec / 1000))
    + ' kbytes)';
  end;

 FDownMix := MIDownMixToStereo.Checked;

 if (MIDIPlaying) then
  begin
   i := round(100 * FMidiFile.GetCurrentPos / FMidiFile.GetTrackLength2);
   if i > 100 then i := 100 else if i < 0 then i := 0;
   Player.SbMidiPosition.position := i;

   if (FMidiFile.Ready) then
   begin
    Player.SbMidiPosition.Position := 0;
    if Player.CBMidiPlayMode.ItemIndex = 1 then
     begin
      MIPanicClick(nil);
      FMidiFile.StartPlaying;
     end else
    if (Player.CBMidiPlayMode.ItemIndex = 2) and (Player.MidiBox.Items.Count > 0) then
     begin
      Player.MidiBox.itemindex := (Player.MidiBox.itemindex + 1) mod Player.MidiBox.Items.Count;
      Player.BtMidiPlayClick(nil);
     end else
    if (Player.CBMidiPlayMode.ItemIndex = 3) and (Player.MidiBox.Items.Count > 0) then
     begin
      Player.MidiBox.itemindex := random(Player.MidiBox.Items.Count);
      Player.BtMidiPlayClick(nil);
     end
    else MIDIPlaying := False;
   end;
  end;

 if PresetBox.Items.Count = 0 then
  begin
   Caption := 'Delphi ASIO & VST Project -  MiniHost';
   Exit;
  end;

 s := VSTHost[0].GetProgramName;
 i := VSTHost[0].CurrentProgram;
 if (FCurProg <> i) or (FCurProgName <> s) then
  begin
   FCurProg := i;
   FCurProgName := s;
   s := AnsiString(IntToStr(FCurProg));
   if FCurProg < 10 then s := '00' + s else
   if FCurProg < 100 then s := '0' + s;
   if (PresetBox.items.Count > 0) and (FCurProg>=0) then
    begin
     PresetBox.Items[FCurProg] := string(s + ': ' + FCurProgName);
     PresetBox.ItemIndex := i;
    end;
   s2 := FTitle;
   if MIShowPreset.Checked
    then s2 := s2 + AnsiString(' - ') + s + AnsiString(': ') + FCurProgName;
   if Caption <> string(s2) then Caption := string(s2);
  end;
end;

procedure TFmMiniHost.MIASIOControlPanelClick(Sender: TObject);
begin
 StopAudio;
 ASIOHost.ControlPanel;
 ASIOHost.Reset;
 StartAudio;
end;

procedure TFmMiniHost.ProcessEvents(Sender: TObject; ev: PVstEvents);
var
  i: Integer;
  event: PVstMidiEvent;
  Sysex : PVstMidiSysexEvent;
  aStream: TMemoryStream;
begin
 if FCurrentMIDIOut = 0 then Exit;

 FDataSection.Acquire;
 try
  for i := 0 to ev^.numEvents - 1 do
   if (ev.events[i].EventType = etMidi) then
    begin
     event := PVstMidiEvent(ev^.events[i]);
     FMidiOutput.Send(FCurrentMIDIOut - 1, event^.mididata[0],
       event^.mididata[1], event^.mididata[2]);
    end else
   if ev.events[i].EventType = etSysex then
    begin
     Sysex := PVstMidiSysexEvent(ev^.events[i]);
     if Sysex.dumpBytes > 0 then
      begin
       AStream := TMemoryStream.Create;
       try
        aStream.Size := Sysex.dumpBytes;
        aStream.Position := 0;
        Move(Sysex.SysexDump^, pchar(aStream.Memory)[0], Sysex.dumpBytes);
        FMidiOutput.SendSysEx(FCurrentMIDIOut - 1, aStream);
       finally
        FreeAndNil(aStream);
       end;
      end;
    end;
 finally
  FDataSection.Release;
 end; 
end;

procedure TFmMiniHost.WMDropFiles(var msg: TMessage);
var
  size  : Integer;
  name  : PChar;
  fn, s : string;
begin
 inherited;
 size := DragQueryFile(msg.wparam, 0, nil, 0) + 1;
 name := StrAlloc(size);
 DragQueryFile(msg.wparam, 0, name, size);
 s := StrPas(name);
 StrDispose(name);
 DragFinish(msg.wparam);

 fn := UpperCase(ExtractFileExt(s));
 if (fn = '.FXP') then
 begin
  try
   VSTHost[0].LoadPreset(s);
  except
   MessageDlg('ERROR: Preset file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
   Exit;
  end;
 end else
 if (fn = '.FXB') then
 begin
  try
   VSTHost[0].LoadBank(s);
  except
   MessageDlg('ERROR: Bank file not for this plugin (or file is corrupted)!', mtError, [mbOK], 0);
   Exit;
  end;
 end else
  if (fn = '.DLL') then LoadPlugin(s)
 else
  if (fn = '.WAV') then
  begin
   AddWAV(s);
   LoadWAV(s);
  end else
   if (fn = '.MID') then AddMid(s);
end;

procedure TFmMiniHost.MIRenamePresetClick(Sender: TObject);
var
  s2, s: string;
begin
 s := InputBox('Rename Preset', 'New name:', string(VSTHost[0].GetProgramName));
 VSTHost[0].SetProgramName(AnsiString(s));
 VSTHost[0].Idle;
 VSTHost[0].EditIdle;

 s2 := IntToStr(FCurProg);
 if FCurProg < 10 then s2 := '00' + s2 else
 if FCurProg < 100 then s2 := '0' + s2;

 PresetBox.Items[FCurProg] := s2 + ': ' + s;
end;

procedure TFmMiniHost.LoadMIDIFile1Click(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.mid';
   InitialDir := FDirMidi;
   Options := [ofFileMustExist, ofForceShowHidden];
   {$IFNDEF FPC}
   Ctl3D := False;
   {$ENDIF}

   Filename := '*.mid;*.mpl';
   Filter := 'MIDI files and playlists (*.mid;*.mpl)|*.mid;*.mpl|MIDI files (*.mid)|*.mid|MIDI playlists (*.mpl)|*.mpl';
   FilterIndex := 0;
   Title := 'Select a MIDI file';
   if Execute then
    begin
     FDirMidi := ExtractFileDir(Filename);
     AddMID(Filename);
    end;
  finally
   Free;
  end;
end;

procedure TFmMiniHost.MISettingsClick(Sender: TObject);
begin
 FmOptions.Show;
end;

procedure TFmMiniHost.SetChannelI(Sender: TObject);
begin
 (Sender as TMenuItem).Checked := True;
 FCurrentInputChannel := (Sender as TMenuItem).Tag;
 FmOptions.LbInputs.Caption := 'Inputs: ' + MIASIOInputChannel.Items[FCurrentInputChannel].Caption;
end;

procedure TFmMiniHost.ASIOHostReset(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to Length(FVSTBufOut) - 1
  do SetLength(FVSTBufOut[i], ASIOHost.BufferSize);
 for i := 0 to Length(FVSTBufIn) - 1
  do SetLength(FVSTBufIn[i], ASIOHost.BufferSize);
 SetLength(FInBufL, ASIOHost.BufferSize);
 SetLength(FInBufR, ASIOHost.BufferSize);
 SetLength(FWavBufL, ASIOHost.BufferSize);
 SetLength(FWavBufR, ASIOHost.BufferSize);
 ASIOHostSampleRateChanged(Sender);
end;

procedure TFmMiniHost.ASIOHostDestroy(Sender: TObject);
var
  i : Integer;
begin
 FProcessing := False;
 SetLength(FInBufL, 0);
 SetLength(FInBufR, 0);
 SetLength(FWavBufL, 0);
 SetLength(FWavBufR, 0);
 FNumIn := 0;
 FNumOut := 0;
 for i := 0 to Length(FVSTBufOut) - 1 do SetLength(FVSTBufOut[i], 0);
 for i := 0 to Length(FVSTBufIn) - 1 do SetLength(FVSTBufIn[i], 0);
 SetLength(FVSTBufOut, 0);
 SetLength(FVSTBufIn, 0);
 SetLength(FVSTPinProps, 0);
end;


// By Daniel:  Dav_MidiIO midiInProc midiIncallback is called
// concurrently by different service threads
// we need to protect the midi event arrary and the DataCnt
// against concurrent access
procedure TFmMiniHost.AddMIDIData(d1, d2, d3: byte; pos: Integer = 0);
begin
 FDataSection.Acquire; 
 try
  if FMDataCnt > 2046 
   then Exit;
 
  inc(FMDataCnt);
  with PVstMidiEvent(FMyEvents.events[FMDataCnt - 1])^ do
   begin
    EventType := etMidi;
    deltaFrames := pos;
    midiData[0] := d1;
    midiData[1] := d2;
    midiData[2] := d3;
   end;
 finally 
  FDataSection.Release;
 end; 
end;

procedure TFmMiniHost.NoteOn(ch, note, v: byte);
begin
 if v = 0 then
  begin
   ch := ch - $10;
   NoteOff(ch, note);
   Exit;
  end;
 if (note <= 127)
  then ProcessNoteOnOff(ch, note, v);
end;

procedure TFmMiniHost.NoteOff(ch, note: byte);
begin
 if (note <= 127) then ProcessNoteOnOff(ch, note, 0);
end;

procedure TFmMiniHost.ProcessNoteOnOff(ch, n, v: byte);
begin
 if v = 0 then
  begin // Note Off
   if ch >= $90 then ch := ch - $10;
   AddMidiData(ch, n, 0);
  end
 else
  begin // Note On
   if ch < $90 then ch := ch + $10;
   AddMidiData(ch, n, v);
  end;
end;

procedure TFmMiniHost.MIAboutClick(Sender: TObject);
begin
 if not Assigned(FAboutForm)
  then FAboutForm := TFmAbout.Create(Self);
 FAboutForm.ShowModal;
end;

procedure TFmMiniHost.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 MIPanicClick(nil);

 FAllowed := False;
 WaveFile.stop;
 WaveFile.Unload;
 FMidiFile.StopPlaying;
 MidiPlaying := False;

 FRecordState := rsStop;
 if Assigned(FWavWriter)
  then FreeAndNil(FWavWriter);
end;

procedure TFmMiniHost.MIShowPresetClick(Sender: TObject);
var
  s: AnsiString;
begin
 MIShowPreset.Checked := not MIShowPreset.Checked;
 s := AnsiString(IntToStr(FCurProg));
 if FCurProg < 10 then s := '00' + s else
 if FCurProg < 100 then s := '0' + s;
 if MIShowPreset.Checked
  then Caption := string(FTitle + ' - ' + s + ': ' + FCurProgName)
  else Caption := string(FTitle);
end;

procedure TFmMiniHost.StopPlayback1Click(Sender: TObject);
begin
 FMidiFile.StopPlaying;
 MidiPlaying := False;
 MIPanicClick(nil);
end;

procedure TFmMiniHost.StopPlayback2Click(Sender: TObject);
begin
 WaveFile.Stop;
end;

procedure TFmMiniHost.MIStopRecordingClick(Sender: TObject);
begin
 FRecordState := rsStop;
 if Assigned(FWavWriter)
  then FreeAndNil(FWavWriter);
end;

procedure TFmMiniHost.RenameF1Click(Sender: TObject);
begin
 MIRenamePresetClick(nil);
end;

procedure TFmMiniHost.F3PlayStopMIDI1Click(Sender: TObject);
begin
 with Player do
 if MidiBox.ItemIndex >= 0
  then Player.LbMidiFile.Caption := MidiBox.items[MidiBox.itemindex]
  else Exit;
  
 if MidiPlaying then
  begin
   FMidiFile.StopPlaying;
   MidiPlaying := False;
   MIPanicClick(nil);
  end
 else
  begin
   MIPanicClick(nil);
   FMidiFile.StartPlaying;
   MidiPlaying := True;
  end;
end;

procedure TFmMiniHost.F4PlayStopWAV1Click(Sender: TObject);
begin
 with Player do
 if WavBox.Items.Count > 0 then
 if WavBox.ItemIndex >= 0 then
  begin
   Player.LbWaveFile.Caption := WavBox.items[WavBox.itemindex];
   if Wavefile.FPMode = wpmPlay
    then StopPlayback2Click(nil)
    else StartPlayback2Click(nil);
  end;
end;

procedure TFmMiniHost.F5RecStopWAV1Click(Sender: TObject);
begin
 if FRecordState >= rsRecord then MIStopRecordingClick(nil)
 else if FRecordState = rsStop then MIStartRecordingClick(nil);
end;

procedure TFmMiniHost.F11MIDIPanic1Click(Sender: TObject);
begin
 MIPanicClick(nil);
end;

procedure TFmMiniHost.MIAlwaysOnTopClick(Sender: TObject);
begin
 MIAlwaysOnTop.Checked := not MIAlwaysOnTop.Checked;
{$IFNDEF FPC}
 if MIAlwaysOnTop.Checked then
  SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0,
   SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE)
 else
  SetWindowPos(Self.Handle, HWND_NOTOPMOST, 0, 0, 0, 0,
   SWP_NOMOVE or SWP_NOACTIVATE or SWP_NOSIZE);
{$ENDIF}
end;

procedure TFmMiniHost.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
 if not MIUseMouseWheel.Checked then Exit;
 MIPanicClick(nil);
 if FCurProg > 0 then
  VSTHost[0].CurrentProgram := FCurProg - 1;
end;

procedure TFmMiniHost.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
 if not MIUseMouseWheel.Checked then Exit;
 MIPanicClick(nil);
 if FCurProg + 1 < VSTHost[0].numPrograms
  then VSTHost[0].CurrentProgram := FCurProg + 1;
end;

procedure TFmMiniHost.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmMiniHost.IOnOffMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 FProcessing := not FProcessing;
 MIPanicClick(nil);
end;

procedure TFmMiniHost.PresetBoxClick(Sender: TObject);
begin
 WaveTimer.Enabled := False;
 MIPanicClick(nil);
 VSTHost[0].CurrentProgram := PresetBox.ItemIndex;
 FCurProg := PresetBox.ItemIndex;
 WaveTimer.Enabled := True;
end;

procedure TFmMiniHost.PresetBoxKeyPress(Sender: TObject; var Key: Char);
begin
 Key := #0;
end;

procedure TFmMiniHost.PresetBoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
 if Index < 0 then Exit;
 PresetBox.Canvas.FillRect(Rect);
 PresetBox.Canvas.TextOut(rect.Left + 2, rect.top, PresetBox.items[index]);
end;

procedure TFmMiniHost.IBtLeftRightMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 MIPanicClick(nil);
 if x < IBtLeftRight.width shr 1 then
  if FCurProg > 0
   then VSTHost[0].CurrentProgram := FCurProg - 1
   else else
 if FCurProg + 1 < VSTHost[0].numPrograms
  then VSTHost[0].CurrentProgram := FCurProg + 1;
end;

procedure TFmMiniHost.IBtDropDownMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 PresetBox.DroppedDown := not PresetBox.DroppedDown;
end;

procedure TFmMiniHost.PresetBoxChange(Sender: TObject);
begin
 PnStatus.SetFocus;
end;

procedure TFmMiniHost.IQuickSettingsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 with FmOptions do if Showing then Hide else Show;
end;

procedure TFmMiniHost.IQuickMidPlayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then F3PlayStopMIDI1Click(Sender);
end;

procedure TFmMiniHost.IQuickWavPlayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then F4PlayStopWAV1Click(Sender);
end;

procedure TFmMiniHost.IQuickWavRecMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Button = mbLeft then F5RecStopWAV1Click(Sender);
end;

procedure TFmMiniHost.AddMID(const FileName: string);
var
  j, i : Integer;
  ms   : PShortStr;
begin
 if UpperCase(ExtractFileExt(FileName)) = '.MPL' then
  begin
   {$IFDEF FPC}
   for I := 0 to Player.MidiBox.Items.Count - 1
    do Player.MidiBox.Selected[I] := True;
   {$ELSE}
   Player.MidiBox.SelectAll;
   {$ENDIF}
   Player.BtMidiAddClick(nil);
   with TStringList.Create do
    try
     LoadFromFile(FileName);
     for i := 0 to Count - 1 do AddMID(Strings[i]);
     if (Count > 0) and (uppercase(Strings[0]) = 'RANDOM') then
      Player.CBMidiPlayMode.ItemIndex := 3;
    finally
     Free
    end;
  end;
 if UpperCase(ExtractFileExt(FileName)) <> '.MID' then Exit;
 if not FileExists(FileName) then Exit;
 j := -1;
 for i := 0 to Player.MidiBox.Items.Count - 1 do
  if PShortStr(Player.MidiBox.Items.Objects[i])^ = FileName
   then j := 0;
 if j = 0 then Exit;
 GetMem(ms, SizeOf(shortstr));
 ms^ := FileName;
 Player.MidiBox.Items.AddObject(ExtractFilename(FileName), TObject(ms));
 Player.MidiBox.ItemIndex := Player.MidiBox.Items.Count - 1;
end;

procedure TFmMiniHost.AddWAV(const FileName: string);
var
  j, i : Integer;
  ms   : PShortStr;
begin
 if UpperCase(ExtractFileExt(FileName)) = '.WPL' then
  begin
   {$IFDEF FPC}
   for I := 0 to Player.WavBox.Items.Count - 1
    do Player.WavBox.Selected[I] := True;
   {$ELSE}
   Player.WavBox.SelectAll;
   {$ENDIF}
   Player.BtWavAddClick(nil);
   with TStringList.Create do
    try
     LoadFromFile(FileName);
     for i := 0 to Count - 1 do AddWAV(Strings[i]);
     if (Count > 0) and (UpperCase(Strings[0]) = 'RANDOM')
      then Player.CBWavPlayMode.ItemIndex := 3;
    finally
     Free;
    end;
   if UpperCase(ExtractFileExt(FileName)) <> '.WAV' then Exit;
   if not FileExists(FileName) then Exit;
   j := -1;
   for i := 0 to Player.WavBox.Items.Count - 1 do
    if string(PShortStr(Player.WavBox.Items.Objects[i])^) = FileName
     then j := 0;
   if j = 0 then Exit;
   GetMem(ms, SizeOf(ShortStr));
   ms^ := ShortStr(FileName);
   Player.WavBox.Items.AddObject(ExtractFilename(FileName), TObject(ms));
   Player.WavBox.ItemIndex := Player.WavBox.Items.Count - 1;
  end;
end;

procedure TFmMiniHost.BorderPlayMIDIMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Shift = [ssRight] then Player.Show;
end;

procedure TFmMiniHost.MIShowMIDIWAVWindowClick(Sender: TObject);
begin
 Player.Show;
end;

procedure TFmMiniHost.MIDownMixToStereoClick(Sender: TObject);
begin
 MIDownMixToStereo.Checked := not MIDownMixToStereo.Checked;
end;

procedure TFmMiniHost.MIMidiThruClick(Sender: TObject);
begin
 MIMidiThru.Checked := not MIMidiThru.Checked;
end;

procedure TFmMiniHost.MIUseMouseWheelClick(Sender: TObject);
begin
 MIUseMouseWheel.Checked := not MIUseMouseWheel.Checked;
end;

procedure TFmMiniHost.IdleTimerTimer(Sender: TObject);
begin
 VSTHost[0].Idle;
 VSTHost[0].EditIdle;
 if not FColBack then
  begin
   FColBack := True;
   if VSTHost[0].Active
    then Self.Color := FindBackgroundColor;
  end;
end;

procedure TFmMiniHost.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  j, i      : Integer;
  bs, ChOfs : Integer;
begin
 bs := ASIOHost.BufferSize;
 if (bs <= 0) or (not FAllowed) or (VSTHost = nil)
  then Exit;

 VSTHost.UpdateVstTimeInfo(bs);
 FMidiFile.MidiTimer(nil);

 FDataSection.Acquire;
 try
  if FMDataCnt > 0 then
   begin
    FMyEvents.numEvents := FMDataCnt;
 
    // by Daniel :  this is a critical path, better save this cando in a value
    // instead of calling VstDispatch here
 //   if VSTHost[0].VstCanDo('receiveVstMidiEvent') >= 0 then
    VSTHost[0].ProcessEvents(FMyEvents);
 
    if (FCurrentMIDIOut > 0) and MIMidiThru.Checked then
     begin
      for i := 0 to FMDataCnt - 1 do
       FMidiOutput.Send(FCurrentMIDIOut - 1,
                       PVstMidiEvent(FMyEvents.events[i])^.midiData[0],
                       PVstMidiEvent(FMyEvents.events[i])^.midiData[1],
                       PVstMidiEvent(FMyEvents.events[i])^.midiData[2]);
     end;
     FMDataCnt := 0;
   end;
 finally  
  FDataSection.Release;
 end; 

 ChOfs := FCurrentOutputChannel * 2;
 if FCurrentInputChannel = 0 then
  for i := 0 to bs - 1 do
   begin
    FInBufL[i] := 0;
    FInBufR[i] := 0;
   end
 else
  for i := 0 to bs - 1 do
   begin
    FInBufL[i] := InputVol * InBuffer[(FCurrentInputChannel - 1) * 2, i];
    FInBufR[i] := InputVol * InBuffer[(FCurrentInputChannel - 1) * 2 + 1, i];
   end;

 // fill WavBufL and WavBufR
 if Wavefile.FPMode = wpmPlay then
  for i := 0 to bs - 1
   do WaveFile.Process(FWavBufL[i], FWavBufR[i])
 else
  begin
   Assert(Length(FWavBufL) >= bs);
   Assert(Length(FWavBufR) >= bs);
   FillChar(FWavBufL[0], bs * SizeOf(Single), 0);
   FillChar(FWavBufR[0], bs * SizeOf(Single), 0);
  end;

 if FNumOut > 0 then
  begin
   // assign Input to VSTBufIn
   for i := 0 to FNumOut - 1 do FillChar(FVSTBufOut[i][0], bs * SizeOf(Single), 0);
   if effFlagsIsSynth in VSTHost[0].EffectOptions then
    for i := 0 to FNumIn - 1 do FillChar(FVSTBufIn[i][0], bs * SizeOf(Single), 0)
   else
    for i := 0 to bs - 1 do
     begin
      FVSTBufIn[0][i] := (FWavBufL[i] * Wavefile.Volume) + FInBufL[i];
      FVSTBufIn[1][i] := (FWavBufR[i] * Wavefile.Volume) + FInBufR[i];
     end;

   // apply Processing
   if FProcessing then
    begin
     if effFlagsCanReplacing in VSTHost[0].EffectOptions
      then VSTHost[0].Process32Replacing(@FVSTBufIn[0], @FVSTBufOut[0], bs)
      else VSTHost[0].Process(@FVSTBufIn[0], @FVSTBufOut[0], bs);
     if FDownMix then
      for i := 0 to bs - 1 do
       for j := 2 to FNumOut - 1 do
        begin
         if FVSTPinProps[j].ArrangementType = satMono then
          begin
           FVSTBufOut[0][i] := FVSTBufOut[0][i] + FVSTBufOut[j][i];
           FVSTBufOut[1][i] := FVSTBufOut[1][i] + FVSTBufOut[j][i];
          end
         else FVSTBufOut[j mod 2][i] := FVSTBufOut[j mod 2][i] + FVSTBufOut[j][i];
        end;
    end;

   // assign Output from VSTBufOut
   if FNumOut = 1 then j := 0 else j := 1;
   if effFlagsIsSynth in VSTHost[0].EffectOptions then
    for i := 0 to bs - 1 do
     begin
      OutBuffer[ChOfs][i] := (FVSTBufOut[0][i] * VSTVol + FInBufL[i] + FWavBufL[i] * Wavefile.Volume) * FOverallVol;
      OutBuffer[ChOfs + 1][i] := (FVSTBufOut[j][i] * VSTVol + FInBufR[i] + FWavBufR[i] * Wavefile.Volume) * FOverallVol;
     end
   else
    for i := 0 to bs - 1 do
     begin
      OutBuffer[ChOfs][i] := (FVSTBufOut[0][i] * VSTVol + (1 - VSTVol) * FVSTBufIn[0][i]) * FOverallVol;
      OutBuffer[ChOfs + 1][i] := (FVSTBufOut[j][i] * VSTVol + (1 - VSTVol) * FVSTBufIn[j][i]) * FOverallVol;
     end;
  end
 else
  for i := 0 to bs - 1 do
   begin
    OutBuffer[ChOfs][i] := (FInBufL[i] + FWavBufL[i] * Wavefile.Volume) * FOverallVol;
    OutBuffer[ChOfs + 1][i] := (FInBufR[i] + FWavBufR[i] * Wavefile.Volume) * FOverallVol;
   end;

 if FRecordState = rsRecord then
  begin
   FTotalFrames := FTotalFrames + integer(ASIOHost.buffersize);
   with FWavWriter do
    if Format.nChannels = 1
     then WriteFloatData(OutBuffer[ChOfs], bs)
     else WriteFloatDataSeparateStereo(OutBuffer[ChOfs], OutBuffer[ChOfs + 1], bs);
  end;

 // by Daniel: this line messes up, midi data may have changed in the meantime
 // by other thread so this will kill data which was just processed
 // Line has been moved to just after vstPlugin.processEvents has been called
 //  FMDataCnt := 0;
end;

{$IFDEF FPC}
initialization
  {$i MiniHostForm.lrs}
{$ENDIF}

end.
