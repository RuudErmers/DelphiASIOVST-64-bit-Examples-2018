unit ASIOMP3GUI;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, DAV_Types, 
  DAV_DspBufferedMp3Player, DAV_ASIOHost;

type
  TFmASIOMP3 = class(TForm)
    ASIOHost: TASIOHost;
    BtControlPanel: TButton;
    BtSelect: TButton;
    BtStartStop: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    EdFile: TEdit;
    LbBuffer: TLabel;
    LbBufferValue: TLabel;
    LbChannels: TLabel;
    LbDrivername: TLabel;
    LbMp3File: TLabel;
    OpenDialog: TOpenDialog;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtControlPanelClick(Sender: TObject);
    procedure BtSelectClick(Sender: TObject);
    procedure BtStartStopClick(Sender: TObject);
    procedure ChannelBoxChange(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure EdFileChange(Sender: TObject);
    procedure LbBufferClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FIniFile        : TFileName;
    FVolumeFactor   : Single;
    FChannelOffset  : Byte;
    FBufferedPlayer : TBufferedMP3FilePlayer;
  end;

var
  FmASIOMP3: TFmASIOMP3;

implementation

{$R *.dfm}

uses
  IniFiles;

resourcestring
  RCStrNoASIODriverPresent = 'No ASIO Driver present! Application Terminated!';

{ TFmASIOMP3 }

procedure TFmASIOMP3.FormCreate(Sender: TObject);
begin
 FIniFile := ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI';
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  begin
   MessageDlg(RCStrNoASIODriverPresent,
     mtError, [mbOK], 0);
   Application.Terminate;
  end;

 FVolumeFactor := 1;
 FChannelOffset := 0;
 FBufferedPlayer := TBufferedMP3FilePlayer.Create;
 FBufferedPlayer.Pitch := 1;
 FBufferedPlayer.Interpolation := biBSpline6Point5thOrder;
 with FBufferedPlayer do
  begin
   BufferSize := 65536;
   BlockSize  := 4096
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(FIniFile) do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);

   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then
     DriverComboChange(DriverCombo);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
   EdFile.Text := ReadString('Audio', 'MP3 File', EdFile.Text);
   BtStartStop.Enabled := FileExists(EdFile.Text);
  finally
   Free;
  end;

 // enable timer
 Timer.Enabled := True;
end;

procedure TFmASIOMP3.FormDestroy(Sender: TObject);
begin
  ASIOHost.Active := False;
  FreeAndNil(FBufferedPlayer);

  with TIniFile.Create(FIniFile) do
   try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
    WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
    WriteString('Audio', 'MP3 File', EdFile.Text);
   finally
    Free;
   end;
end;

procedure TFmASIOMP3.LbBufferClick(Sender: TObject);
begin
 ASIOHost.SampleRate := 48000;
end;

procedure TFmASIOMP3.TimerTimer(Sender: TObject);
begin
 LbBufferValue.Caption := IntToStr(Round(FBufferedPlayer.BufferFill)) + ' %';
end;

procedure TFmASIOMP3.DriverComboChange(Sender: TObject);
var
  i: Integer;
begin
 BtControlPanel.Enabled := False;
 BtStartStop.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for i := 0 to (ASIOHost.OutputChannelCount div 2) - 1 do
     ChannelBox.Items.Add(
       ASIOHost.OutputChannelInfos[2 * i].Name + ' / ' +
       ASIOHost.OutputChannelInfos[2 * i + 1].Name);

   with TIniFile.Create(FIniFile) do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;

   BtControlPanel.Enabled := True;
   BtStartStop.Enabled := FileExists(EdFile.Text);
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmASIOMP3.ChannelBoxChange(Sender: TObject);
begin
 FChannelOffset := ChannelBox.ItemIndex * 2;
end;

procedure TFmASIOMP3.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if Assigned(FBufferedPlayer)
  then FBufferedPlayer.SampleRate := ASIOHost.SampleRate;
end;

procedure TFmASIOMP3.BtControlPanelClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmASIOMP3.BtStartStopClick(Sender: TObject);
begin
 if BtStartStop.Caption = '&Start Audio' then
  begin
   ASIOHost.Active := True;
   BtStartStop.Caption := '&Stop Audio';
  end
 else
  begin
   ASIOHost.Active := False;
   FBufferedPlayer.Reset;
   BtStartStop.Caption := '&Start Audio';
  end;
end;

procedure TFmASIOMP3.BtSelectClick(Sender: TObject);
begin
 if OpenDialog.Execute then EdFile.Text := OpenDialog.FileName;
end;

procedure TFmASIOMP3.EdFileChange(Sender: TObject);
begin
 FBufferedPlayer.Filename := EdFile.Text;
end;

procedure TFmASIOMP3.ASIOHostBufferSwitch32(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
begin
 FBufferedPlayer.GetSamples(OutBuffer[0], OutBuffer[1], ASIOHost.Buffersize);
end;

end.
