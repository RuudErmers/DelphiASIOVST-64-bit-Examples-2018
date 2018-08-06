unit AbxStandaloneTest;

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
  Graphics, Controls, Forms, Dialogs, ComCtrls, StdCtrls, DB,
  kbmMemCSVStreamFormat, kbmMemTable, kbmMemBinaryStreamFormat, DAV_Classes,
  DAV_Types, DAV_ASIOHost, DAV_AudioData, DAV_AudioFile, DAV_AudioFileWav,
  AbxChunks;

type
  TXAssignment = (xaXisA = 1, xaXisB = 2);
  TGuess = (gNone = 0, gXisA = 1, gXisB = 2);

  TTrialRecord = record
    XAssignment : TXAssignment;
    Guess       : TGuess;
  end;

  ESampleRateMismatch = class(Exception);
  EAbxResources = class(Exception);

  TFmABXStandaloneTest = class(TForm)
    LbB: TLabel;
    LbX: TLabel;
    LbA: TLabel;
    BtXisA: TButton;
    BtXisB: TButton;
    BtAudioStop: TButton;
    BtAudioPlay: TButton;
    NotesBox: TGroupBox;
    Notes: TMemo;
    StatusBar: TStatusBar;
    ASIOHost: TASIOHost;
    AdcA: TAudioDataCollection32;
    AdcB: TAudioDataCollection32;
    BtAudioSetup: TButton;
    Database: TkbmMemTable;
    DatabaseDate: TDateField;
    DatabaseNameID: TStringField;
    DatabaseFieldRating: TFloatField;
    DatabaseTrials: TIntegerField;
    kbmCSVStreamFormat: TkbmCSVStreamFormat;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ASIOHostBufferSwitch32(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BtAudioPlayClick(Sender: TObject);
    procedure BtAudioStopClick(Sender: TObject);
    procedure BtXisAClick(Sender: TObject);
    procedure BtXisBClick(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure LbAClick(Sender: TObject);
    procedure LbBClick(Sender: TObject);
    procedure LbXClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtAudioSetupClick(Sender: TObject);
  private
    FABXContainer : TAbxContainer;
    FTrialCount   : Integer;
    FTrialPos     : Integer;
    FTrials       : array of TTrialRecord;
    FCurrentADC   : TAudioDataCollection32;
    FPosition     : Integer;
    FIniFileName  : TFileName;
    procedure SetTrials(const Value: Integer);
  protected
    procedure AdvanceTrial; virtual;
    procedure AssignAudiodataX; virtual;
    procedure CheckAudioSetup; virtual;
    procedure RandomizeTrials; virtual;
    procedure TestDone; virtual;
    procedure TrialCountChanged; virtual;
    procedure UpdateStatusBar; virtual;
  public
    property TrialCount: Integer read FTrialCount write SetTrials;
  published
    property IniFileName: TFileName read FIniFileName;
  end;

var
  FmABXStandaloneTest: TFmABXStandaloneTest;

implementation

uses
  AbxStandaloneAudioSetup;

resourcestring
  RCStrTrial = 'Trial';
  RCStrRunning = 'running';
  RCStrNoABXTestFound = 'No ABX test found';
  RCStrTooMuchTestsFound = 'More than one ABX test found';
  RCStrReferenceANotPresent = 'Reference A not present!';
  RCStrReferenceBNotPresent = 'Reference B not present!';
  RCStrSamplerateMismatch = 'Samplerate mismatch!';
  RCCheckAudioConfiguration = 'Please check audio configuration';
  RCStrChannelMismatch = 'Too few channels!';

{$R *.dfm}

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

procedure TFmABXStandaloneTest.FormCreate(Sender: TObject);
var
  AbxTestNames : TStringList;
  RS           : TResourceStream;
begin
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'Abx.ini';

 FABXContainer := TAbxContainer.Create;

 FTrialCount := 15;
 TrialCountChanged;

 AbxTestNames := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'ABX', @EnumNamesFunc, LongWord(AbxTestNames));

  try
   if AbxTestNames.Count = 0
    then raise EAbxResources.Create(RCStrNoABXTestFound);

   if AbxTestNames.Count > 1
    then raise EAbxResources.Create(RCStrTooMuchTestsFound);
  except
   on E: EAbxResources do
    begin
     MessageDlg('Error: ' + E.Message, mtError, [mbOK], 0);
     Application.Terminate;
     Exit;
    end;
  end;

  RS := TResourceStream.Create(HInstance, AbxTestNames[0], 'ABX');
  try
   FABXContainer.LoadFromStream(RS);

   // assign title from ABX container
   Caption := 'ABX Test - ' + FAbxContainer.Title;

   // assign notes from ABX container
   if FAbxContainer.Notes <> ''
    then MessageDlg(FAbxContainer.Notes, mtInformation, [mbOK], 0);

   // eventually load audio stream
   if Assigned(FAbxContainer.AudioStreamA)
    then AdcA.LoadFromStream(FAbxContainer.AudioStreamA)
    else raise Exception.Create(RCStrReferenceANotPresent);

   // eventually load audio stream
   if assigned(FAbxContainer.AudioStreamB)
    then AdcB.LoadFromStream(FAbxContainer.AudioStreamB)
    else raise Exception.Create(RCStrReferenceBNotPresent);

  finally
   FreeAndNil(RS);
  end;

 finally
  FreeAndNil(AbxTestNames);
 end;

 // load result database
 Database.PersistentFile := ParamStr(0) + '.csv';
 Database.Persistent := True;
 if FileExists(Database.PersistentFile)
  then Database.LoadPersistent;
end;

procedure TFmABXStandaloneTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FABXContainer);
end;

procedure TFmABXStandaloneTest.FormShow(Sender: TObject);
begin
 CheckAudioSetup;
 RandomizeTrials;
 AssignAudiodataX;
 ASIOHost.OnBufferSwitch32 := ASIOHostBufferSwitch32;
end;

procedure TFmABXStandaloneTest.CBDriversChange(Sender: TObject);
begin
 if assigned(FmAudioSettings)
  then FmAudioSettings.CBDriversChange(Sender);
end;

procedure TFmABXStandaloneTest.LbAClick(Sender: TObject);
begin
 LbA.Font.Color := clRed;
 LbB.Font.Color := clWindowText;
 LbX.Font.Color := clWindowText;
 FCurrentADC := AdcA;
end;

procedure TFmABXStandaloneTest.LbBClick(Sender: TObject);
begin
 LbA.Font.Color := clWindowText;
 LbB.Font.Color := clRed;
 LbX.Font.Color := clWindowText;
 FCurrentADC := AdcB;
end;

procedure TFmABXStandaloneTest.LbXClick(Sender: TObject);
begin
 LbA.Font.Color := clWindowText;
 LbB.Font.Color := clWindowText;
 LbX.Font.Color := clRed;

 AssignAudiodataX;
end;

procedure TFmABXStandaloneTest.AssignAudiodataX;
begin
 case FTrials[FTrialPos].XAssignment of
  xaXisA : FCurrentADC := AdcA;
  xaXisB : FCurrentADC := AdcB;
 end;
end;

procedure TFmABXStandaloneTest.RandomizeTrials;
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

procedure TFmABXStandaloneTest.BtXisAClick(Sender: TObject);
begin
 FTrials[FTrialPos].Guess := gXisA;
 AdvanceTrial;
end;

procedure TFmABXStandaloneTest.BtXisBClick(Sender: TObject);
begin
 FTrials[FTrialPos].Guess := gXisB;
 AdvanceTrial;
end;

procedure TFmABXStandaloneTest.BtAudioPlayClick(Sender: TObject);
begin
 ASIOHost.Active := True;
 BtAudioPlay.Enabled := False;
 BtAudioStop.Enabled := True;
 UpdateStatusBar;
end;

procedure TFmABXStandaloneTest.BtAudioSetupClick(Sender: TObject);
begin
 FmAudioSettings.ShowModal;
end;

procedure TFmABXStandaloneTest.CheckAudioSetup;
begin
 try
  if (ASIOHost.SampleRate <> AdcA.SampleRate) or
     (ASIOHost.SampleRate <> AdcB.SampleRate)
   then raise ESampleRateMismatch.Create(RCStrSamplerateMismatch + #10#13 + RCCheckAudioConfiguration);

  if (ASIOHost.OutputChannelCount < AdcA.ChannelCount) or
     (ASIOHost.OutputChannelCount < AdcB.ChannelCount)
   then raise ESampleRateMismatch.Create(RCStrChannelMismatch + #10#13 + RCCheckAudioConfiguration);
  BtAudioSetup.Visible := False;
 except
  on E: ESampleRateMismatch do MessageDlg(E.Message, mtError, [mbOK], 0);
 end;
end;

procedure TFmABXStandaloneTest.BtAudioStopClick(Sender: TObject);
begin
 ASIOHost.Active := False;
 BtAudioPlay.Enabled := True;
 BtAudioStop.Enabled := False;
 UpdateStatusBar;
end;

procedure TFmABXStandaloneTest.AdvanceTrial;
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

function GetUserName: string;
var
  UserName  : array [0..255] of Char;
  dw        : DWord;
begin
 dw := SizeOf(UserName);
 Windows.GetUserName(@UserName, dw);
 result := Username;
end;

procedure TFmABXStandaloneTest.TestDone;
var
  i      : Integer;
  Rating : Single;
  Weight : Single;
begin
 MessageDlg('Thank you for your contribution!', mtInformation, [mbOK], 0);

 Database.Append;
 Database.Edit;
 try
  Database.FieldValues['Date'] := Now;
  Database.FieldValues['Name/ID'] := GetUserName;
  Database.FieldValues['Trials'] := 15;

  Rating := 0;
  Weight := 1 / Length(FTrials);
  for i := 0 to Length(FTrials) - 1 do
   if Integer(FTrials[i].XAssignment) = Integer(FTrials[i].Guess)
    then Rating := Rating + Weight
    else Rating := Rating - Weight;

  Database.FieldValues['Rating'] := Rating;
 finally
  Database.Post;
 end;

 BtAudioStopClick(Self);
 Database.SavePersistent;
 Close;
end;

procedure TFmABXStandaloneTest.SetTrials(const Value: Integer);
begin
 if FTrialCount <> Value then
  begin
   FTrialCount := Value;
   TrialCountChanged;
  end;
end;

procedure TFmABXStandaloneTest.TrialCountChanged;
begin
 SetLength(FTrials, FTrialCount);
end;

procedure TFmABXStandaloneTest.UpdateStatusBar;
begin
 StatusBar.SimpleText := RCStrTrial + ': ' + IntToStr(FTrialPos + 1) + ' / ' + IntToStr(Length(FTrials));
 if BtAudioStop.Enabled
  then StatusBar.SimpleText := StatusBar.SimpleText + ' (' + RCStrRunning + ')';
end;

procedure TFmABXStandaloneTest.ASIOHostBufferSwitch32(Sender: TObject; const InBuffer,
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
