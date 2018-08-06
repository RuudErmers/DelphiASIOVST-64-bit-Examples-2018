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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, DAV_Types, 
  DAV_ASIOHost, DAV_AudioData;

type
  TXAssignment = (xaXisA = 1, xaXisB = 2);
  TGuess = (gNone = 0, gXisA = 1, gXisB = 2);

  TTrialRecord = record
    XAssignment : TXAssignment;
    Guess       : TGuess;
  end;

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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BtXisBClick(Sender: TObject);
    procedure BtXisAClick(Sender: TObject);
    procedure BtAudioPlayClick(Sender: TObject);
    procedure BtAudioStopClick(Sender: TObject);
    procedure LbAClick(Sender: TObject);
    procedure LbXClick(Sender: TObject);
    procedure LbBClick(Sender: TObject);
  private
    FTrialCount  : Integer;
    FTrialPos    : Integer;
    FTrials      : array of TTrialRecord;
    FCurrentADC  : TAudioDataCollection32;
    FPosition    : Integer;
    FNameID      : string;
    procedure SetTrials(const Value: Integer);
    procedure SetNameID(const Value: string);
  protected
    procedure AdvanceTrial; virtual;
    procedure AssignAudiodataX; virtual;
    procedure RandomizeTrials; virtual;
    procedure TestDone; virtual;
    procedure TrialCountChanged; virtual;
    procedure UpdateStatusBar; virtual;
  public
    property TrialCount: Integer read FTrialCount write SetTrials;
    property NameID: string read FNameID write SetNameID;
  end;

implementation

uses
  AbxMain, AbxProject, AbxTestSetup;

resourcestring
  RCStrTrial = 'Trial';
  RCStrRunning = 'running';

{$R *.dfm}

procedure TFmAbxTest.FormCreate(Sender: TObject);
begin
 FTrialCount := 15;
 TrialCountChanged;
end;

procedure TFmAbxTest.FormShow(Sender: TObject);
begin
 FTrialPos := 0;
 RandomizeTrials;
 AssignAudiodataX;
 UpdateStatusBar;
 FmAbxMain.ASIOHost.OnBufferSwitch32 := ASIOHostBufferSwitch32;
end;

procedure TFmAbxTest.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 BtAudioStopClick(Self);
end;

procedure TFmAbxTest.LbAClick(Sender: TObject);
begin
 LbA.Font.Color := clRed;
 LbB.Font.Color := clWindowText;
 LbX.Font.Color := clWindowText;
 assert(Owner is TFmTestSetup);
 assert(TFmTestSetup(Owner).Owner is TFmProject);
 FCurrentADC := TFmProject(TFmTestSetup(Owner).Owner).AdcA;
end;

procedure TFmAbxTest.LbBClick(Sender: TObject);
begin
 LbA.Font.Color := clWindowText;
 LbB.Font.Color := clRed;
 LbX.Font.Color := clWindowText;
 assert(Owner is TFmTestSetup);
 assert(TFmTestSetup(Owner).Owner is TFmProject);
 FCurrentADC := TFmProject(TFmTestSetup(Owner).Owner).AdcB;
end;

procedure TFmAbxTest.LbXClick(Sender: TObject);
begin
 LbA.Font.Color := clWindowText;
 LbB.Font.Color := clWindowText;
 LbX.Font.Color := clRed;

 AssignAudiodataX;
end;

procedure TFmAbxTest.AssignAudiodataX;
begin
 assert(Owner is TFmTestSetup);
 assert(TFmTestSetup(Owner).Owner is TFmProject);
 with TFmProject(TFmTestSetup(Owner).Owner) do
  case FTrials[FTrialPos].XAssignment of
   xaXisA : FCurrentADC := AdcA;
   xaXisB : FCurrentADC := AdcB;
  end;
end;

procedure TFmAbxTest.RandomizeTrials;
var
  Trial: Integer;
begin
 for Trial := 0 to Length(FTrials) - 1 do
  with FTrials[Trial] do
   begin
    XAssignment := TXAssignment(1 + random(2));
    Guess := gNone;
   end;
end;

procedure TFmAbxTest.BtXisAClick(Sender: TObject);
begin
 FTrials[FTrialPos].Guess := gXisA;
 AdvanceTrial;
end;

procedure TFmAbxTest.BtXisBClick(Sender: TObject);
begin
 FTrials[FTrialPos].Guess := gXisB;
 AdvanceTrial;
end;

procedure TFmAbxTest.BtAudioPlayClick(Sender: TObject);
begin
 FmAbxMain.ASIOHost.Active := True;
 BtAudioPlay.Enabled := False;
 BtAudioStop.Enabled := True;
 UpdateStatusBar;
end;

procedure TFmAbxTest.BtAudioStopClick(Sender: TObject);
begin
 FmAbxMain.ASIOHost.Active := False;
 BtAudioPlay.Enabled := True;
 BtAudioStop.Enabled := False;
 UpdateStatusBar;
end;

procedure TFmAbxTest.AdvanceTrial;
begin
 Inc(FTrialPos);
 if FTrialPos = Length(FTrials)
  then TestDone
  else
   // eventually assign audiodata X
   if LbX.Font.Color = clRed
    then AssignAudiodataX;
 UpdateStatusBar;
end;

procedure TFmAbxTest.TestDone;
var
  i      : Integer;
  Rating : Single;
  Weight : Single;
begin
 assert(Owner is TFmTestSetup);
 assert(TFmTestSetup(Owner).Owner is TFmProject);
 with TFmProject(TFmTestSetup(Owner).Owner) do
  begin
   Database.Append;
   Database.Edit;
   try
    Database.FieldValues['Date'] := Now;
    Database.FieldValues['Name/ID'] := FNameID;
    Database.FieldValues['Trials'] := 15;

    Rating := 0;
    Weight := 1 / Length(FTrials);
    for i := 0 to Length(FTrials) - 1 do
     if Integer(FTrials[i].XAssignment) = Integer(FTrials[i].Guess)
      then Rating := Rating + Weight
      else Rating := Rating - Weight;

    Database.FieldValues['Rating'] := Rating;

    MessageDlg('Test done!', mtInformation, [mbOK], 0);
   finally
    Database.Post;
   end;
  end;
 ModalResult := mrOK;
end;

procedure TFmAbxTest.UpdateStatusBar;
begin
 StatusBar.SimpleText := RCStrTrial + ': ' + IntToStr(FTrialPos + 1) + ' / ' + IntToStr(Length(FTrials));
 if BtAudioStop.Enabled
  then StatusBar.SimpleText := StatusBar.SimpleText + ' (' + RCStrRunning + ')';
end;

procedure TFmAbxTest.SetNameID(const Value: string);
begin
 if FNameID <> Value then
  begin
   FNameID := Value;
  end;
end;

procedure TFmAbxTest.SetTrials(const Value: Integer);
begin
 if FTrialCount <> Value then
  begin
   FTrialCount := Value;
   TrialCountChanged;
  end;
end;

procedure TFmAbxTest.TrialCountChanged;
begin
 SetLength(FTrials, FTrialCount);
end;

procedure TFmAbxTest.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  CurrentPosition : Integer;
  Channel         : Integer;
begin
 // check if ADC is selected at all!
 if (FCurrentADC = nil) or (FCurrentADC.ChannelCount = 0) then
  begin
   for Channel := 0 to TAsioHost(Sender).OutputChannelCount - 1
    do FillChar(OutBuffer[Channel, 0], TAsioHost(Sender).Buffersize * SizeOf(Single), 0);
   Exit;
  end;

 CurrentPosition := 0;

 with TAsioHost(Sender) do
  repeat
   if FPosition + (Buffersize - CurrentPosition) < FCurrentADC.SampleFrames then
    begin
     for Channel := 0 to OutputChannelCount - 1
      do Move(FCurrentADC.ChannelDataPointer[Channel mod FCurrentADC.ChannelCount]^[FPosition], OutBuffer[Channel, CurrentPosition], (Buffersize - CurrentPosition) * Sizeof(Single));

     // increase block position and break
     inc(FPosition, Buffersize - CurrentPosition);
     break;
    end
   else
    begin
     for Channel := 0 to OutputChannelCount - 1
      do Move(FCurrentADC.ChannelDataPointer[Channel mod FCurrentADC.ChannelCount]^[FPosition], OutBuffer[Channel, CurrentPosition], (FCurrentADC.SampleFrames - FPosition) * Sizeof(Single));

     // increase current position and reset block position
     Inc(CurrentPosition, (FCurrentADC.SampleFrames - FPosition));
     FPosition := 0;
    end;
   until CurrentPosition >= Buffersize;
end;

end.
