unit OptionsForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf,  LResources, Buttons, LMessages,{$ELSE} Windows,
  Messages,{$ENDIF} Classes, Controls, Forms, StdCtrls, MiniHostForm;

type
  TFmOptions = class(TForm)
    BtInfo: TButton;
    GbASIO: TGroupBox;
    GbGlobalSetting: TGroupBox;
    LbASIODriver: TLabel;
    LbBufferSize: TLabel;
    LbFormat: TLabel;
    LbInputs: TLabel;
    LbInputVolume: TLabel;
    LbOutputs: TLabel;
    LbOverallVolume: TLabel;
    LbSampleRate: TLabel;
    LbTempo: TLabel;
    LbVSTVolume: TLabel;
    LbWavVolume: TLabel;
    MemoInfo: TMemo;
    SbInputVolume: TScrollBar;
    SbOverallVolume: TScrollBar;
    SbTempo: TScrollBar;
    SbVSTVolume: TScrollBar;
    SbWavVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtInfoClick(Sender: TObject);
    procedure SbTempoChange(Sender: TObject);
  public
    Host: TFmMiniHost;
    procedure FillInfo;
  end;

var
  FmOptions : TFmOptions;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  SysUtils, DAV_VSTEffect, DAV_ASIOHost, DAV_ASIO;

procedure TFmOptions.FormCreate(Sender: TObject);
begin
 MemoInfo.left := 8;
end;

procedure TFmOptions.FillInfo;
var o, i: integer;
    s: TStrings;
begin
 Host.StopAudio;
 with Host.ASIOHost, MemoInfo do
  begin
   Clear;
   o := DriverIndex;
   s := host.ASIOHost.DriverList;
   Lines.Add('number of ASIO drivers: '+IntToStr(s.Count));
   Lines.Add('');
   for i := 0 to s.Count - 1 do
    with Lines do
     begin
      DriverIndex := i;
      Add('driver #' + IntToStr(i) + ': ');
      Add('name: ' + DriverName);
      Add('version: ' + IntToStr(DriverVersion));
      Add('input channels: ' + IntToStr(InputChannelCount));
      Add('output channels: ' + IntToStr(OutputChannelCount));
      Add('input format: ' +
        ChannelTypeToString(InputChannelInfos[0].SampleType) +
        ' (' + IntToStr(InputChannelInfos[0].SampleType) + ')');
      Add('output format: ' +
        ChannelTypeToString(OutputChannelInfos[0].SampleType) +
        ' (' + IntToStr(OutputChannelInfos[0].SampleType) + ')');
      Add('input latency: '  + IntToStr(InputLatency));
      Add('output latency: ' + IntToStr(OutputLatency));
      Add('buffer size: ' + IntToStr(BufferSize));
      Add('min size: '    + IntToStr(BufferMinimum));
      Add('max size: '    + IntToStr(BufferMaximum));
      Add('pref size: '   + IntToStr(BufferPreferredSize));
      Add('granularity: ' + IntToStr(BufferGranularity));
      Add('samplerate: '  + FloatToStr(SampleRate));
      Add('samplerate 8000 Hz possible: '   + BoolToStr(canSamplerate(  8000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('samplerate 16000 Hz possible: '  + BoolToStr(canSamplerate( 16000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('samplerate 22050 Hz possible: '  + BoolToStr(canSamplerate( 22050) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('samplerate 32000 Hz possible: '  + BoolToStr(canSamplerate( 32000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('samplerate 44100 Hz possible: '  + BoolToStr(canSamplerate( 44100) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('samplerate 48000 Hz possible: '  + BoolToStr(canSamplerate( 48000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('samplerate 96000 Hz possible: '  + BoolToStr(canSamplerate( 96000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('samplerate 192000 Hz possible: ' + BoolToStr(canSamplerate(192000) = ASE_OK{$IFNDEF FPC}, True{$ENDIF}));
      Add('');
     end;
   DriverIndex := o;
   Host.StartAudio;
  end;
end;

procedure TFmOptions.BtInfoClick(Sender: TObject);
begin
 MemoInfo.Visible := not MemoInfo.Visible;
 if (MemoInfo.Lines.Count = 0) and (MemoInfo.Visible)
  then FillInfo;
end;

procedure TFmOptions.FormShow(Sender: TObject);
begin
 GbGlobalSetting.SetFocus;
end;

procedure TFmOptions.SbTempoChange(Sender: TObject);
begin
 if not Assigned(Host) then exit;
 Host.OverallVolume := SbOverallVolume.Position * 0.01;
 LbOverallVolume.caption := 'Overall Volume: ' + IntToStr(SbOverallVolume.Position);
 Host.VSTVol := SbVSTVolume.Position * 0.01;
 if effFlagsIsSynth in Host.VSTHost[0].EffectOptions
  then LbVSTVolume.caption := 'VST Volume: ' + IntToStr(SbVSTVolume.Position)
  else LbVSTVolume.caption := 'VST Dry/Wet Mix: ' + IntToStr(SbVSTVolume.Position);
 Host.InputVol := SbInputVolume.Position * 0.01;
 LbInputVolume.caption := 'Input Volume: ' + IntToStr(SbInputVolume.Position);
 Host.Wavefile.Volume := SbWavVolume.Position * 0.01;
 LbWavVolume.caption := 'WAV Volume: ' + IntToStr(SbWavVolume.Position);
 Host.VSTHost.Tempo := SbTempo.Position;
 LbTempo.caption := 'Tempo: ' + IntToStr(SbTempo.Position) + ' bpm';
end;

{$IFDEF FPC}
initialization
  {$i OptionsForm.lrs}
{$ENDIF}

end.
