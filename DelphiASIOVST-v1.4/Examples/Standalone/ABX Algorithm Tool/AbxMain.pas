unit AbxMain;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, StdCtrls, DAV_AudioData, 
  DAV_ASIOHost, DAV_AudioFile, DAV_AudioFileWav, DAV_AudioFileAu, 
  DAV_AudioFileAIFF, DAV_Classes;

type
  TFmAbxAlgorithmTest = class(TForm)
    Adc: TAudioDataCollection32;
    ASIOHost: TASIOHost;
    BtASIOSetup: TButton;
    BtGo: TButton;
    LbChooseTest: TLabel;
    TVTestSelect: TTreeView;
    Label1: TLabel;
    EdAudioFile: TEdit;
    BtSelectAudioFile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtASIOSetupClick(Sender: TObject);
    procedure BtGoClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure TVTestSelectChange(Sender: TObject; Node: TTreeNode);
    procedure BtSelectAudioFileClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FIniFileName   : TFileName;
    FChannelOffset : Integer;
    procedure SelectMusic;
    procedure GenerateNoise;
  public
    property IniFileName: TFileName read FIniFileName;
    property ChannelOffset: Integer read FChannelOffset write FChannelOffset;
  end;

var
  FmAbxAlgorithmTest: TFmAbxAlgorithmTest;

implementation

uses
  IniFiles, DAV_DspPinkNoiseGenerator, AbxAudio, AbxTest, AbxTestSetup;

{$R *.dfm}

procedure TFmAbxAlgorithmTest.FormCreate(Sender: TObject);
begin
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'AbxAlgo.ini';
 TVTestSelect.FullExpand;

 with TIniFile.Create(FmAbxAlgorithmTest.IniFileName) do
  try
   Top := ReadInteger('Layout', 'Main Top', Top);
   Left := ReadInteger('Layout', 'Main Left', Left);

   EdAudioFile.Text := ReadString('Setup', 'Audio File', EdAudioFile.Text);
  finally
   Free;
  end;
end;

procedure TFmAbxAlgorithmTest.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create(FmAbxAlgorithmTest.IniFileName) do
  try
   WriteInteger('Layout', 'Main Top', Top);
   WriteInteger('Layout', 'Main Left', Left);

   WriteString('Setup', 'Audio File', EdAudioFile.Text);
  finally
   Free;
  end;
end;

procedure TFmAbxAlgorithmTest.BtASIOSetupClick(Sender: TObject);
begin
 FmAudioSettings.ShowModal;
end;

procedure TFmAbxAlgorithmTest.BtGoClick(Sender: TObject);
begin
 // eventually choose music
 if (TVTestSelect.Selected.Text = 'Music') or
    (TVTestSelect.Selected.Parent.Text = 'Limiter')
  then SelectMusic
  else GenerateNoise;

 with TFmAbxTest.Create(Self) do
  try
   if Pos('Gain', TVTestSelect.Selected.Text) > 0
    then AbxTestSetup := TAbxTestPeakFilterGainSetup.Create
    else
   if Pos('Threshold', TVTestSelect.Selected.Text) > 0
    then AbxTestSetup := TLimiterThresholdAbxTestSetup.Create
    else
   if Pos('Knee', TVTestSelect.Selected.Text) > 0
    then AbxTestSetup := TLimiterKneeAbxTestSetup.Create;

   if assigned(AbxTestSetup) then
    begin
     AbxTestSetup.ChannelCount := Adc.ChannelCount;
     AbxTestSetup.SampleRate := ASIOHost.SampleRate;

     if ShowModal = mrOk then
      begin
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmAbxAlgorithmTest.BtSelectAudioFileClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   Filter := 'Wave File (*.wav)|*.wav';
   if Execute
    then EdAudioFile.Text := FileName;
  finally
   Free;
  end;
end;

procedure TFmAbxAlgorithmTest.SelectMusic;
begin
 if FileExists(EdAudioFile.Text)
  then Adc.LoadFromFile(EdAudioFile.Text)
  else
   with TOpenDialog.Create(Self) do
    try
     DefaultExt := '.wav';
     Filter := 'Wave File (*.wav)|*.wav';
     if Execute
      then Adc.LoadFromFile(FileName);
    finally
     Free;
    end;
end;

procedure TFmAbxAlgorithmTest.GenerateNoise;
var
  Channel, Sample : Integer;
begin
 with Adc do
  begin
   ChannelCount := 2;
   SampleFrames := round(10 * Samplerate);
   with TPinkNoiseGenerator.Create do
    try

     // process some unused initial samples
     for Sample := 0 to 1000 do ProcessSample64;

     for Channel := 0 to ChannelCount - 1 do
      for Sample := 0 to SampleFrames - 1
       do Adc[Channel].ChannelData[Sample] := ProcessSample64;
    finally
     Free;
    end;
  end;
end;

procedure TFmAbxAlgorithmTest.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmAbxAlgorithmTest.TVTestSelectChange(Sender: TObject;
  Node: TTreeNode);
begin
 BtGo.Enabled := Node.Level = 2;
end;

end.
