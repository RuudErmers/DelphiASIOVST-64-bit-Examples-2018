unit AbxTestEQGain;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, AbxTest, ComCtrls, StdCtrls, 
  DAV_DspFilterBasics;

type
  TFmAbxTestEqGain = class(TFmAbxTest)
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    FPeakFilter : array of TBasicPeakFilter;
    FStepSize   : Double;
    function ProcessAudio(Channel: Integer; Data: Double): Double;
    procedure NextTrial(GuessWasCorrect: Boolean); override;
    procedure SampleRateChanged(Sender: TObject); override;
    procedure UpdateStatusBar; override;
  public
  end;

implementation

uses AbxMain, DAV_DspFilter;

{$R *.dfm}

procedure TFmAbxTestEqGain.FormShow(Sender: TObject);
var
  Channel : Integer;
begin
 inherited;
 SetLength(FPeakFilter, FmAbxAlgorithmTest.Adc.ChannelCount);
 for Channel := 0 to Length(FPeakFilter) - 1 do
  begin
   FPeakFilter[Channel] := TBasicPeakFilter.Create;
   with FPeakFilter[Channel] do
    begin
     SampleRate := FmAbxAlgorithmTest.ASIOHost.SampleRate;
     Gain       := 10;
     Frequency  := 1000;
     Bandwidth  := 1;
    end;
  end;
end;

procedure TFmAbxTestEqGain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Channel : Integer;
begin
 inherited;
 for Channel := 0 to Length(FPeakFilter) - 1
  do FreeAndNil(FPeakFilter[Channel]);
end;

procedure TFmAbxTestEqGain.SampleRateChanged(Sender: TObject);
var
  Channel : Integer;
begin
 inherited;
 for Channel := 0 to Length(FPeakFilter) - 1
  do FPeakFilter[Channel].SampleRate := FmAbxAlgorithmTest.ASIOHost.SampleRate;
end;

procedure TFmAbxTestEqGain.UpdateStatusBar;
begin
 if (Length(FPeakFilter) > 0) and assigned(FPeakFilter[0])
  then StatusBar.SimpleText := 'Gain: ' + FloatToStr(FPeakFilter[0].Gain) + ' dB'
  else StatusBar.SimpleText := '';
 inherited;
end;

procedure TFmAbxTestEqGain.NextTrial(GuessWasCorrect: Boolean);
var
  Channel : Integer;
  Step    : Single;
begin
 if GuessWasCorrect then
  for Channel := 0 to Length(FPeakFilter) - 1 do
   with FPeakFilter[Channel] do Gain := Gain * 0.75
 else
  for Channel := 0 to Length(FPeakFilter) - 1 do
   with FPeakFilter[Channel] do Gain := Gain * 3.1;

 inherited;
end;

function TFmAbxTestEqGain.ProcessAudio(Channel: Integer; Data: Double): Double;
begin
 result := FPeakFilter[Channel].ProcessSample(Data);
end;

end.
