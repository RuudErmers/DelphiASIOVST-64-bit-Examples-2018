unit AsioDemoForm;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLType, LResources, Buttons, {$ELSE} Windows, {$ENDIF}
  Forms, Classes, Controls, StdCtrls, DAV_Complex, DAV_Types,
  DAV_AudioData, DAV_ASIOHost, DAV_ASIOHostAudioData;

type
  TFmASIO = class(TForm)
    ASIOHostAudioData: TASIOHostAudioData;
    BtControlPanel: TButton;
    BtStartStop: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    Lb_Channels: TLabel;
    Lb_Copyright: TLabel;
    Lb_Drivername: TLabel;
    LbFreq: TLabel;
    LbPanorama: TLabel;
    LbVolume: TLabel;
    SbFreq: TScrollBar;
    SbPan: TScrollBar;
    SbVolume: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostAudioDataBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TASIOAudioDataCollection32);
    procedure ASIOHostAudioDataBufferSwitch64(Sender: TObject; const InBuffer, OutBuffer: TASIOAudioDataCollection64);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure SbFreqChange(Sender: TObject);
    procedure SbPanChange(Sender: TObject);
    procedure SbVolumeChange(Sender: TObject);
  private
    procedure SetFrequency(const CurrentValue: Double);
  public
    FAngle, FPosition : TComplex64;
    FPan, FFreq, FVol : Double;
    FChannelOffset    : Byte;
  published
    property Frequency : Double read FFreq write SetFrequency;
  end;

var
  FmASIO        : TFmASIO;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses
  SysUtils, Inifiles;
procedure GetSinCos(angle:double;VAR _sin,_cos:double);
begin
  _sin:=sin(angle);
  _cos:=cos(angle);
end;

resourcestring
  RCStrPanorama = 'Panorama';
  RCStrVolume = 'Volume';
  RCStrFrequency = 'Frequency';
  RCStrStartAudio = 'Start Audio';
  RCStrStopAudio = 'Stop Audio';
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

procedure TFmASIO.FormCreate(Sender: TObject);
begin
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
  finally
   Free;
  end;

 FPosition.Re   :=    0;
 FPosition.Im   :=   -1;
 FFreq          := 1000;
 FPan           :=    0.5;
 FVol           :=    1;
 FChannelOffset :=    0;
 GetSinCos(2 * Pi * FFreq / ASIOHostAudioData.SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TFmASIO.DriverComboChange(Sender: TObject);
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

procedure TFmASIO.BtControlPanelClick(Sender: TObject);
begin
 ASIOHostAudioData.ControlPanel;
end;

procedure TFmASIO.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
  finally
   Free;
  end; 
end;

procedure TFmASIO.BtStartStopClick(Sender: TObject);
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

procedure TFmASIO.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmASIO.SbFreqChange(Sender: TObject);
begin
 Frequency :=  5000; //FreqLinearToLog(SbFreq.Position * 0.00001);
end;

procedure TFmASIO.SetFrequency(const CurrentValue: Double);
begin
 if FFreq<>CurrentValue then
  begin
   FFreq := CurrentValue;
   LbFreq.Caption := RCStrFrequency + ': ' + FloatTostrF(FFreq, ffGeneral, 5, 5) + ' Hz';
   GetSinCos(2 * Pi * FFreq / ASIOHostAudioData.SampleRate, FAngle.Im, FAngle.Re);
  end;
end;

procedure TFmASIO.ASIOHostAudioDataBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TASIOAudioDataCollection32);
var
  Sample, Channel : Integer;
  CurrentValue      : Double;
begin
 for Sample := 0 to OutBuffer.SampleFrames - 1 do
  begin
   CurrentValue := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
   FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
   FPosition.Re := CurrentValue; CurrentValue := CurrentValue * FVol;
   for Channel := 0 to ASIOHostAudioData.OutputChannelCount - 1
    do OutBuffer[Channel].ChannelDataPointer[Sample] := CurrentValue;
  end;
end;

procedure TFmASIO.ASIOHostAudioDataBufferSwitch64(Sender: TObject;
  const InBuffer, OutBuffer: TASIOAudioDataCollection64);
var
  Sample, Channel : Integer;
  CurrentValue    : Double;
begin
 for Sample := 0 to ASIOHostAudioData.BufferSize - 1 do
  begin
   CurrentValue := FPosition.Re * FAngle.Re - FPosition.Im * FAngle.Im;
   FPosition.Im := FPosition.Im * FAngle.Re + FPosition.Re * FAngle.Im;
   FPosition.Re := CurrentValue; CurrentValue := CurrentValue * FVol;
   for Channel := 0 to ASIOHostAudioData.OutputChannelCount - 1
    do OutBuffer[Channel].ChannelDataPointer[Sample] := CurrentValue;
  end;
end;

procedure TFmASIO.ASIOHostSampleRateChanged(Sender: TObject);
begin
 GetSinCos(2 * Pi * FFreq / ASIOHostAudioData.SampleRate, FAngle.Im, FAngle.Re);
end;

procedure TFmASIO.SbVolumeChange(Sender: TObject);
begin
 FVol := SbVolume.Position * 0.00001;
(*
 if FVol = 0
  then LbVolume.Caption := RCStrVolume + ': 0 equals -oo dB'
  else LbVolume.Caption := RCStrVolume + ': ' +
                           FloattostrF(FVol, ffFixed, 2, 2) + ' equals ' +
                           FloattostrF(Amp_to_dB(FVol), ffGeneral, 2, 2) + ' dB'; *)
end;

procedure TFmASIO.SbPanChange(Sender: TObject);
begin
 FPan := SbPan.Position * 0.01;
 if FPan = 0.5
  then LbPanorama.Caption := RCStrPanorama + ': C'
  else LbPanorama.Caption := RCStrPanorama + ': ' + Inttostr(round(100 * (FPan * 2 - 1)));
end;

{$IFDEF FPC}
initialization
  {$i AsioDemoForm.lrs}
{$ENDIF}

end.

