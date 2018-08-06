unit AbxTest;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, DAV_Types, 
  DAV_ASIOHost, DAV_AudioData, AbxTestSetup;

type
  TFmAbxTest = class(TForm)
    BtAudioPlay: TButton;
    BtAudioStop: TButton;
    BtXisA: TButton;
    BtXisB: TButton;
    LbA: TLabel;
    LbB: TLabel;
    LbX: TLabel;
    Notes: TMemo;
    NotesBox: TGroupBox;
    StatusBar: TStatusBar;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BtXisBClick(Sender: TObject);
    procedure BtXisAClick(Sender: TObject);
    procedure BtAudioPlayClick(Sender: TObject);
    procedure BtAudioStopClick(Sender: TObject);
    procedure LbAClick(Sender: TObject);
    procedure LbXClick(Sender: TObject);
    procedure LbBClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPosition   : Integer;
    FCurrentX   : TXAssignment;
    FAbxTest    : TCustomAbxTestSetup;
    FLogStrings : TStringList;
    procedure WriteLogText(Sender: TObject; LogText: string);
    procedure TestDone(Sender: TObject);
  protected
    procedure RandomizeAssignment; virtual;
    procedure AssignAudiodataX; virtual;
    procedure StartTest; virtual;
    procedure UpdateStatusBar; virtual;
    procedure NextTrial(GuessWasCorrect: Boolean); virtual;
  public
    property AbxTestSetup: TCustomAbxTestSetup read FAbxTest write FAbxTest;
  end;

implementation

uses
  AbxMain;

resourcestring
  RCStrTrial = 'Trial';
  RCStrRunning = 'running';

{$R *.dfm}

procedure TFmAbxTest.FormShow(Sender: TObject);
begin
 StartTest;

 with FmAbxAlgorithmTest.ASIOHost do
  begin
   OnBufferSwitch32 := ASIOHostBufferSwitch32;
   OnSampleRateChanged := ASIOHostSampleRateChanged;
  end;
end;

procedure TFmAbxTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 BtAudioStopClick(Self);
end;

procedure TFmAbxTest.FormCreate(Sender: TObject);
begin
 FLogStrings := TStringList.Create;
 if FileExists('AbxAlgos.log')
  then FLogStrings.LoadFromFile('AbxAlgos.log');
end;

procedure TFmAbxTest.FormDestroy(Sender: TObject);
begin
 FLogStrings.SaveToFile('AbxAlgos.log');
 FreeAndNil(FLogStrings);
 FreeAndNil(FAbxTest);
end;

procedure TFmAbxTest.WriteLogText(Sender: TObject; LogText: string);
begin
 if Assigned(FLogStrings)
  then FLogStrings.Add(LogText);
end;

procedure TFmAbxTest.LbAClick(Sender: TObject);
begin
 LbA.Font.Color := clRed;
 LbB.Font.Color := clWindowText;
 LbX.Font.Color := clWindowText;

 FAbxTest.Processed := False;
end;

procedure TFmAbxTest.LbBClick(Sender: TObject);
begin
 LbA.Font.Color := clWindowText;
 LbB.Font.Color := clRed;
 LbX.Font.Color := clWindowText;

 FAbxTest.Processed := True;
end;

procedure TFmAbxTest.LbXClick(Sender: TObject);
begin
 LbA.Font.Color := clWindowText;
 LbB.Font.Color := clWindowText;
 LbX.Font.Color := clRed;

 AssignAudiodataX;
end;

procedure TFmAbxTest.NextTrial(GuessWasCorrect: Boolean);
begin
 if Assigned(FAbxTest)
  then FAbxTest.NextTrial(GuessWasCorrect);

 RandomizeAssignment;
 if LbX.Font.Color = clRed
  then AssignAudiodataX;
end;

procedure TFmAbxTest.BtXisAClick(Sender: TObject);
begin
 NextTrial(Integer(FCurrentX) = Integer(gXisA));
end;

procedure TFmAbxTest.BtXisBClick(Sender: TObject);
begin
 NextTrial(Integer(FCurrentX) = Integer(gXisB));
end;

procedure TFmAbxTest.BtAudioPlayClick(Sender: TObject);
begin
 FmAbxAlgorithmTest.ASIOHost.Active := True;
 BtAudioPlay.Enabled := False;
 BtAudioStop.Enabled := True;
 UpdateStatusBar;
end;

procedure TFmAbxTest.BtAudioStopClick(Sender: TObject);
begin
 FmAbxAlgorithmTest.ASIOHost.Active := False;
 BtAudioPlay.Enabled := True;
 BtAudioStop.Enabled := False;
 UpdateStatusBar;
end;

procedure TFmAbxTest.ASIOHostSampleRateChanged(Sender: TObject);
begin
 if Assigned(FAbxTest)
  then FAbxTest.SampleRate := FmAbxAlgorithmTest.ASIOHost.SampleRate;
end;

procedure TFmAbxTest.AssignAudiodataX;
begin
 if Assigned(FAbxTest) then
  case FCurrentX of
   xaXisA : FAbxTest.Processed := False;
   xaXisB : FAbxTest.Processed := True;
  end;
end;

procedure TFmAbxTest.RandomizeAssignment;
begin
 FCurrentX := TXAssignment(1 + random(2));
end;

procedure TFmAbxTest.StartTest;
begin
 Randomize;
 RandomizeAssignment;
 AssignAudiodataX;

 if Assigned(FAbxTest) then
  begin
   FAbxTest.OnLogMessage := WriteLogText;
   FAbxTest.OnTestDone := TestDone;
   FAbxTest.StartTest;
  end;
end;

procedure TFmAbxTest.TestDone(Sender: TObject);
begin
 if Notes.Text <> ''
  then WriteLogText(Self, 'Notes: ' + Notes.Text);
 ModalResult := mrOK;
end;

procedure TFmAbxTest.UpdateStatusBar;
begin
 if Assigned(FAbxTest)
  then StatusBar.SimpleText := FAbxTest.TestInfo;

 if BtAudioStop.Enabled
  then StatusBar.SimpleText := StatusBar.SimpleText + ' (' + RCStrRunning + ')';
end;

procedure TFmAbxTest.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Sample, Channel : Integer;
  AudioData : array [0..1] of Double;
begin
 with FmAbxAlgorithmTest, TASIOHost(Sender) do
  for Sample := 0 to Buffersize - 1 do
   begin
    for Channel := 0 to OutputChannelCount - 1 do
     begin
      AudioData[0] := Adc[Channel mod Adc.ChannelCount].ChannelDataPointer^[FPosition];
      AudioData[1] := FAbxTest.ProcessedAudio(Channel, AudioData[0]);
      AudioData[0] := FAbxTest.UnprocessedAudio(Channel, AudioData[0]);
      if FAbxTest.Processed
       then OutBuffer[Channel, Sample] := AudioData[1]
       else OutBuffer[Channel, Sample] := AudioData[0];
     end;

    // advance position & loop
    inc(FPosition);
    if FPosition >= Adc.SampleFrames
     then FPosition := 0;
   end;
end;

end.
