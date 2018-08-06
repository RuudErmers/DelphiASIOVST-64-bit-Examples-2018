unit UViewASIOForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Complex, DAV_Types,UIXPlugin,UMidiEvent,UMidiPortsCombo,UMidiPorts,
  DAV_AudioData, DAV_ASIOHost, DAV_ASIOHostAudioData, Vcl.ExtCtrls, DAV_DspBufferedMp3Player, DAV_DspBufferedAudioFilePlayer;

type
  TViewASIOForm = class(TForm)                                        
    ASIOHostAudioData: TASIOHostAudioData;
    BtControlPanel: TButton;
    BtStartStop: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    Lb_Copyright: TLabel;
    Lb_Drivername: TLabel;
    XSynthPanel: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Memo1: TMemo;
    ComboBoxMidi: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostAudioDataBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TASIOAudioDataCollection32);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure ASIOHostAudioDataSampleRateChanged(Sender: TObject);
    procedure ScrollBar1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ButtonMP3Click(Sender: TObject);
  private
    FModel : IXPlugin;
    FSamples: integer;
    FVSTBufIn,
    FVSTBufOut      : TDAVArrayOfSingleFixedArray;
    FMidiInPort:TMidiInPortCombo;
    FBufferedPlayer      : TBufferedAudioFilePlayer; //TBufferedMP3FilePlayer;


    procedure CheckBuffers(samples: integer);
    procedure DoMidiData(Sender: TMidiPort; const midievent: TMidiEvent);
  public
     property Model : IXPlugin read FModel;
  end;
var
  ViewASIOForm        : TViewASIOForm;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles,XPluginFactory, Dialogs,DAV_VSTEffect, DAV_MpegAudio;

resourcestring
  RCStrStartAudio = 'Start Audio';
  RCStrStopAudio = 'Stop Audio';
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TViewASIOForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  ASIOHostAudioData.Active := False; // Stop Audio
end;

procedure TViewASIOForm.FormCreate(Sender: TObject);
VAR XSynthFrame:TForm;
begin
  FModel := XPluginFactory.CreateObject(NIL,44100);
  XSynthFrame:=FModel.GetFormClass.Create(XSynthPanel);
  XSynthFrame.Parent:=XSynthPanel;
  FModel.SetEditor(XSynthFrame);
  XSynthFrame.Visible:=true;
  XSynthPanel.ClientWidth:=XSynthFrame.Width;
  XSynthPanel.ClientHeight:=XSynthFrame.Height;
  FModel.AddParameters;
  ClientWidth:=XSynthPanel.Left+XSynthPanel.Width;
  ClientHeight:=XSynthPanel.Top+XSynthPanel.Height;
 DriverCombo.Items := ASIOHostAudioData.DriverList;
 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create(RCStrNoASIODriverPresent);
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   FMidiInPort:=TMidiInPortCombo.Create(ComboBoxMidi,DoMidiData);
   FMidiInport.Open(ReadString('Audio','Midiport',''));
  finally
   Free;
  end;

 FBufferedPlayer := // TBufferedMP3FilePlayer.Create;
 TBufferedAudioFilePlayer.Create;
 with FBufferedPlayer do
  begin
   BufferSize := 65536;
   BlockSize  := 4096
  end;

end;

procedure TViewASIOForm.DoMidiData(Sender: TMidiPort; const midievent:TMidiEvent);
begin
  FModel.ProcessMidi(Sender,MidiEvent);
end;

procedure TViewASIOForm.DriverComboChange(Sender: TObject);
var i : Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHostAudioData.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for i := 0 to (ASIOHostAudioData.OutputChannelCount div 2) - 1 do
   begin
    ChannelBox.Items.Add(
     ASIOHostAudioData.OutputChannelInfos[2 * i].Name + ' / ' +
     ASIOHostAudioData.OutputChannelInfos[2 * i + 1].Name);
   end;
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := True;
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TViewASIOForm.BtControlPanelClick(Sender: TObject);
begin
 ASIOHostAudioData.ControlPanel;
end;

procedure TViewASIOForm.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
   WriteString('Audio', 'Midiport',FMidiInPort.name);
  finally
   Free;
  end;
end;

procedure TViewASIOForm.ScrollBar1Change(Sender: TObject);
VAR sb:TScrollBar;
begin
  sb:=TScrollBar(Sender);
  FModel.SetSlider(sb.Tag,sb.Position);
end;

procedure TViewASIOForm.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = RCStrStartAudio then
  begin
   ASIOHostAudioData.Active := True; // Start Audio
   BtStartStop.Caption := RCStrStopAudio;
  end
 else
  begin
   ASIOHostAudioData.Active := False; // Stop Audio
   BtStartStop.Caption := RCStrStartAudio;
  end;
end;

procedure TViewASIOForm.Button1Click(Sender: TObject);
begin
  FModel.ShowEffects
end;

procedure TViewASIOForm.ButtonMP3Click(Sender: TObject);
begin
//   FBufferedPlayer.Filename := 'C:\Midi\Data\Music\01 Prelude.wav';
   FBufferedPlayer.Filename :='C:\Midi\Data\Syntorchestra\Samples\FSO01-Trombone Poly.4.wav';
//   FBufferedPlayer.Filename :='C:\Midi\Data\Mellotron\Choir\A4.wav';
end;

procedure TViewASIOForm.CheckBuffers(samples:integer);
VAR i:integer;
begin
  if samples<>FSamples then
  begin
    FSamples:=samples;
    SetLength(FVSTBufIn,2);
    SetLength(FVSTBufOut,2);
    for i:=0 to 1 do
    begin
      if FVSTBufIn[i]<>NIL then begin FreeMem(FVSTBufIn[i]);FVSTBufIn[i]:=NIL; end;
      if FVSTBufOut[i]<>NIL then begin FreeMem(FVSTBufOut[i]);FVSTBufOut[i]:=NIL; end;
      GetMem(FVSTBufIn[i],samples*sizeof(single));
      GetMem(FVSTBufOut[i],samples*sizeof(single));
    end;
  end;
end;

procedure TViewASIOForm.ASIOHostAudioDataBufferSwitch32(Sender: TObject;  const InBuffer, OutBuffer: TASIOAudioDataCollection32);
VAR
  k,SampleIndex : Integer;
begin
  CheckBuffers(OutBuffer.SampleFrames);
  FModel.Process(FVSTBufIn, FVSTBufOut, OutBuffer.SampleFrames);
// FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);
// FBufferedPlayer.GetSamples( FVSTBufOut, OutBuffer.SampleFrames);

//procedure TCustomBufferedAudioPlayer.GetSamples(Data: TDAVArrayOfSingleFixedArray;

  for SampleIndex := 0 to OutBuffer.SampleFrames - 1 do
  begin
    if FVSTBufOut[0][SampleIndex] > 0.3 then
      k:=3;
    OutBuffer[0].ChannelDataPointer[SampleIndex]:=FVSTBufOut[0][SampleIndex];
    OutBuffer[1].ChannelDataPointer[SampleIndex]:=FVSTBufOut[1][SampleIndex];
  end;
end;

procedure TViewASIOForm.ASIOHostAudioDataSampleRateChanged(
  Sender: TObject);
begin
  FModel.SetSampleRate(ASIOHostAudioData.SampleRate);
end;

procedure TViewASIOForm.ASIOHostSampleRateChanged(Sender: TObject);
begin
  FModel.SetSampleRate(ASIOHostAudioData.SampleRate);
 if assigned(FBufferedPlayer)
  then FBufferedPlayer.SampleRate := ASIOHostAudioData.SampleRate;

end;

end.

