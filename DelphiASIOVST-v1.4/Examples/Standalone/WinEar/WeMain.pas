unit WeMain;

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
//  Portions created by Christian-W. Budde are Copyright (C) 1999-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, Buttons, ExtCtrls,
  DAV_MidiIO;

type
  TTestkind = (tkMelodicIntervals, tkHarmonicIntervals, tkMelody,
    tkThreeNoteChords, tkFourNoteChords);

  TFmWinEar = class(TForm)
    BtDiminished7th: TButton;
    BtDominant7th: TButton;
    BtEnd: TButton;
    BtFourNoteChords: TButton;
    BtHalfDiminished: TButton;
    BtHarmonicIntervals: TButton;
    BtMajor7th: TButton;
    BtMelodicIntervals: TButton;
    BtMelody: TButton;
    BtMidiChannel: TButton;
    BtMidiDevice: TButton;
    BtMidiThrough: TButton;
    BtMinor7th: TButton;
    BtPatch: TButton;
    BtRepeat: TButton;
    BtScore: TButton;
    BtThreeNoteChords: TButton;
    ImMark1: TImage;
    ImMark2: TImage;
    ImMark3: TImage;
    ImMark4: TImage;
    ImMarkRed1: TImage;
    ImMarkRed2: TImage;
    ImMarkRed3: TImage;
    ImMarkRed4: TImage;
    ImNoteA3: TShape;
    ImNoteA4: TShape;
    ImNoteAb3: TShape;
    ImNoteAb4: TShape;
    ImNoteB3: TShape;
    ImNoteB4: TShape;
    ImNoteBb3: TShape;
    ImNoteBb4: TShape;
    ImNoteC4: TShape;
    ImNoteC5: TShape;
    ImNoteD4: TShape;
    ImNoteD5: TShape;
    ImNoteDb4: TShape;
    ImNoteDb5: TShape;
    ImNoteE4: TShape;
    ImNoteE5: TShape;
    ImNoteEb4: TShape;
    ImNoteEb5: TShape;
    ImNoteF3: TShape;
    ImNoteF4: TShape;
    ImNoteF5: TShape;
    ImNoteG3: TShape;
    ImNoteG4: TShape;
    ImNoteGb3: TShape;
    ImNoteGb4: TShape;
    LbTime: TLabel;
    MainMenu: TMainMenu;
    MiCloseScoreLog: TMenuItem;
    MiCopyScore: TMenuItem;
    MiCopyScoreLog: TMenuItem;
    MiEdit: TMenuItem;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiFourNoteChords: TMenuItem;
    MiHarmonicIntervals: TMenuItem;
    MiHelp: TMenuItem;
    MiMelodicIntervals: TMenuItem;
    MiMelody: TMenuItem;
    MiOpenScoreLog: TMenuItem;
    MiPrintScoreLog: TMenuItem;
    MiScore: TMenuItem;
    MiSettings: TMenuItem;
    MiThreeNoteChords: TMenuItem;
    MiUpdateScoreLog: TMenuItem;
    N1: TMenuItem;
    PnChords: TPanel;
    SbVelocity: TScrollBar;
    StopWatch : TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure StopWatchTimer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure BtMelodicIntervalsClick(Sender: TObject);
    procedure BtMidiThroughClick(Sender: TObject);
    procedure BtMelodyClick(Sender: TObject);
    procedure BtHarmonicIntervalsClick(Sender: TObject);
    procedure BtFourNoteChordsClick(Sender: TObject);
    procedure BtThreeNoteChordsClick(Sender: TObject);
    procedure BtEndClick(Sender: TObject);
    procedure BtRepeatClick(Sender: TObject);
    procedure MiHelpClick(Sender: TObject);
    procedure ImNoteF3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImNoteF3MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BtDominant7thClick(Sender: TObject);
    procedure BtMajor7thClick(Sender: TObject);
    procedure BtMinor7thClick(Sender: TObject);
    procedure BtDiminished7thClick(Sender: TObject);
    procedure BtHalfDiminishedClick(Sender: TObject);
  private
    FIfchck, FOlriat : Boolean;
    FSecs, FMins     : Byte;
    FCnt, FDura      : Byte;
    FCType, FOrde    : Byte;
    FDuration        : Byte;
    FTestkind        : TTestkind;
    FNote            : array[1..5] of Byte;
    FNoter           : array[1..5] of Boolean;

    FInstrument      : Integer;
    FChannel         : Integer;
    FMidiOutput      : TMidiOutput;
    procedure Notemark(ntvyl: Byte);
    procedure DeMark;
    procedure ItClick(Nval: Byte);
    procedure NextStep;
    procedure OwlReIt;
    procedure NewButtons;
    procedure NewButtons2;
    procedure OldButtons;
    procedure OldButtons2;
    procedure Notefinder(Moore: Boolean);
    procedure Chordfinder(thre: Boolean);
    procedure MelodicTest;
    procedure HarmonicTest;
    procedure MelodyTest;
    procedure FochoTest(four: Boolean);
    procedure RepeatIt;
    procedure Retry;
    procedure MidiReset;
    procedure MidiNoteOff(Note, Velocity: Byte);
    procedure DelayedMidiNoteOff(Duration, Note, Velocity: Byte);
  end;

var
  FmWinEar: TFmWinEar;

implementation

uses
  WeHelp, WeEndOfTest, WeRetry;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmWinEar.FormCreate(Sender: TObject);
begin
 FIfchck := False;
 Randomize;
 FTestkind := tkMelodicIntervals;

 FMidiOutput := TMidiOutput.Create;
 FMidiOutput.Open(0);
 FInstrument := 0;
 FChannel := 1;
end;

procedure TFmWinEar.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FMidiOutput);
end;

procedure TFmWinEar.Notemark(ntvyl: Byte);
var
  Leftpos: Integer;
begin
  case ntvyl of
    0 : Leftpos := 16;
    1 : Leftpos := 32;
    2 : Leftpos := 48;
    3 : Leftpos := 65;
    4 : Leftpos := 80;
    5 : Leftpos := 101;
    6 : Leftpos := 112;
    7 : Leftpos := 144;
    8 : Leftpos := 162;
    9 : Leftpos := 176;
    10 : Leftpos := 195;
    11 : Leftpos := 208;
    12 : Leftpos := 240;
    13 : Leftpos := 253;
    14 : Leftpos := 272;
    15 : Leftpos := 287;
    16 : Leftpos := 304;
    17 : Leftpos := 323;
    18 : Leftpos := 336;
    19 : Leftpos := 368;
    20 : Leftpos := 384;
    21 : Leftpos := 400;
    22 : Leftpos := 420;
    23 : Leftpos := 432;
    24 : Leftpos := 464;
   end;
  Leftpos := Leftpos + 50;
  case ntvyl of
    1, 3, 5, 8, 10, 13, 15, 17, 20, 22 :
      if ImMarkRed1.Left = -20 then
        ImMarkRed1.Left := Leftpos
      else if ImMarkRed2.Left = -20 then
        ImMarkRed2.Left := Leftpos
      else if ImMarkRed3.Left = -20 then
        ImMarkRed3.Left := Leftpos
      else if ImMarkRed4.Left = -20 then
        ImMarkRed4.Left := Leftpos;
    0, 2, 4, 6, 7, 9, 11, 12, 14, 16, 18, 19, 21, 23, 24 :
      if ImMark1.Left = -20 then
        ImMark1.Left := Leftpos
      else if ImMark2.Left = -20 then
        ImMark2.Left := Leftpos
      else if ImMark3.Left = -20 then
        ImMark3.Left := Leftpos
      else if ImMark4.Left = -20 then
        ImMark4.Left := Leftpos;
   end;
  update;
end;

procedure TFmWinEar.DeMark;
begin
  ImMark1.Left := -20;
  ImMark2.Left := -20;
  ImMark3.Left := -20;
  ImMark4.Left := -20;
  ImMarkRed1.Left := -20;
  ImMarkRed2.Left := -20;
  ImMarkRed3.Left := -20;
  ImMarkRed4.Left := -20;
end;

procedure TFmWinEar.MiExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFmWinEar.MiHelpClick(Sender: TObject);
begin
 with TFmAbout.Create(Self) do
  try
   ShowModal;
  finally
   Free;
  end;
end;

procedure TFmWinEar.NextStep;
begin
  Notemark(FNote[FOrde] - 53);
  if FMins < 2 then
   begin
    sleep(880);
    OwlReIt;
   end
  else
   begin
    FmEndOfTest.Show;
    StopWatch.Enabled := False;
    LbTime.Visible := False;
    DeMark;
   end;
end;

procedure TFmWinEar.OwlReIt;
begin
  if FTestkind = tkMelodicIntervals then
    MelodicTest;
  if FTestkind = tkHarmonicIntervals then
    HarmonicTest;
  if FTestkind = tkMelody then
    if FOrde < 6 then
      FOrde := FOrde + 1
    else
      MelodyTest;
  if FTestkind = tkThreeNoteChords then
   begin
    FNoter[2] := False;
    FNoter[3] := False;
    FochoTest(False);
   end;
  if FTestkind = tkFourNoteChords then
    FochoTest(True);
end;

procedure TFmWinEar.ImNoteF3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Sender is TComponent
  then ItClick(TComponent(Sender).Tag);
end;

procedure TFmWinEar.ImNoteF3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 if Sender is TComponent
  then FMidiOutput.Send(0, CMIDIMsgNoteOff, TComponent(Sender).Tag, SbVelocity.Position);
end;

procedure TFmWinEar.DelayedMidiNoteOff(Duration, Note, Velocity: Byte);
begin
 sleep(4 * Duration);
 MidiNoteOff(Note, Velocity);
end;

procedure TFmWinEar.MidiNoteOff(Note, Velocity: Byte);
begin
 FMidiOutput.Send(0, CMIDIMsgNoteOff, Note, Velocity);
end;

procedure TFmWinEar.ItClick(Nval: Byte);
begin
  if FIfchck then
   begin
    if (FTestkind = tkMelodicIntervals) OR (FTestkind = tkHarmonicIntervals) OR (FTestkind = tkMelody) then
      if (Nval = FNote[FOrde]) then
        NextStep
      else Retry;
    if FTestkind = tkThreeNoteChords then
     begin
      if (nval <> FNote[2]) OR (nval <> FNote[3]) then Retry;
      if (nval = FNote[2]) AND (FNoter[2] = False) then
       begin
        FNoter[2] := True;
        Notemark(FNote[2] - 53);
       end;
      if (nval = FNote[3]) AND (FNoter[3] = False) then
       begin
        FNoter[3] := True;
        Notemark(FNote[3] - 53);
       end;
      if FNoter[2] AND FNoter[3] then
        if FMins < 2 then
         begin
          sleep(880);
          OwlReIt;
         end
        else
         begin
          FmEndOfTest.Show;
          StopWatch.Enabled := False;
          LbTime.Visible := False;
          DeMark;
         end;
     end;
   end
  else FMidiOutput.Send(0, CMIDIMsgNoteOn, Nval, SbVelocity.Position);
end;

procedure TFmWinEar.Retry;
begin
 case FmRetry.ShowModal of
  mrOk    : OwlReIt;
  mrRetry : RepeatIt;
 end;
end;

procedure TFmWinEar.MidiReset;
begin
 FMidiOutput.Send(0, CMIDIMsgControlChange, CMIDICCAllNotesOff, 0);
end;

procedure TFmWinEar.Notefinder(moore: Boolean);
begin
  FNote[1] := random(76 - 53) + 53;
  repeat
    FNote[2] := random(76 - 53) + 53;
  until FNote[2] <> FNote[1];
  if MOORE then
   begin
    repeat
      FNote[3] := random(76 - 53) + 53;
    until FNote[3] <> FNote[2];
    repeat
      FNote[4] := random(76 - 53) + 53;
    until FNote[4] <> FNote[3];
    repeat
      FNote[5] := random(76 - 53) + 53;
    until FNote[5] <> FNote[4];
   end;
end;

procedure TFmWinEar.Chordfinder(thre: Boolean);
begin
  if thre then
   begin
    FCType := random(10) + 1;
    case FCType of
      1 :
       begin // Major
        FNote[1] := random(69 - 53) + 53;
        FNote[2] := FNote[1] + 4;
        FNote[3] := FNote[1] + 7;
       end;
      2 :
       begin // Major 6
        FNote[1] := random(68 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 8;
       end;
      3 :
       begin // Major 4 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 5;
        FNote[3] := FNote[1] + 9;
       end;
      4 :
       begin // Minor
        FNote[1] := random(69 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 7;
       end;
      5 :
       begin // Minor 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 4;
        FNote[3] := FNote[1] + 9;
       end;
      6 :
       begin // Minor 4 6
        FNote[1] := random(68 - 53) + 53;
        FNote[2] := FNote[1] + 5;
        FNote[3] := FNote[1] + 8;
       end;
      7 :
       begin // Dim
        FNote[1] := random(70 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 6;
       end;
      8 :
       begin // Dim 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 9;
       end;
      9 :
       begin // Dim 4 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 6;
        FNote[3] := FNote[1] + 9;
       end;
      10 :
       begin // Augmented
        FNote[1] := random(68 - 53) + 53;
        FNote[2] := FNote[1] + 4;
        FNote[3] := FNote[1] + 8;
       end;
     end;
   end
  else
   begin
    FCType := random(17) + 1;
    case FCType of
      1 :
       begin // Major 7th
        FNote[1] := random(69 - 53) + 53;
        FNote[2] := FNote[1] + 4;
        FNote[3] := FNote[1] + 7;
        FNote[4] := FNote[1] + 10;
       end;
      2 :
       begin // Major 7th 6}
        FNote[1] := random(68 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 8;
        FNote[4] := FNote[1] + 6;
       end;
      3 :
       begin // Major 7th 4 6}
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 5;
        FNote[3] := FNote[1] + 9;
        FNote[4] := FNote[1] + 3;
       end;
      4 :
       begin  // Major 7th 7
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 2;
        FNote[3] := FNote[1] + 6;
        FNote[4] := FNote[1] + 9;
       end;
      5 :
       begin // Major maj7th
        FNote[1] := random(69 - 53) + 53;
        FNote[2] := FNote[1] + 4;
        FNote[3] := FNote[1] + 7;
        FNote[4] := FNote[1] + 11;
       end;
      6 :
       begin // Major maj7th 6
        FNote[1] := random(68 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 8;
        FNote[4] := FNote[1] + 7;
       end;
      7 :
       begin // Major maj7th 4 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 5;
        FNote[3] := FNote[1] + 9;
        FNote[4] := FNote[1] + 4;
       end;
      8 :
       begin  // Major maj7th 7
        FNote[1] := random(68 - 53) + 53;
        FNote[2] := FNote[1] + 1;
        FNote[3] := FNote[1] + 5;
        FNote[4] := FNote[1] + 8;
       end;
      9 :
       begin // Minor 7th
        FNote[1] := random(69 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 7;
        FNote[4] := FNote[1] + 10;
       end;
      10 :
       begin // Minor 7th 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 4;
        FNote[3] := FNote[1] + 9;
        FNote[4] := FNote[1] + 7;
       end;
      11 :
       begin // Minor 7th 4 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 5;
        FNote[3] := FNote[1] + 8;
        FNote[4] := FNote[1] + 3;
       end;
      12 :
       begin // Minor 7th 7
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 2;
        FNote[3] := FNote[1] + 5;
        FNote[4] := FNote[1] + 9;
       end;
      13 :
       begin // Diminished 7th
        FNote[1] := random(69 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 6;
        FNote[4] := FNote[1] + 9;
       end;
      14 :
       begin // Minor 7th
        FNote[1] := random(69 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 6;
        FNote[4] := FNote[1] + 10;
       end;
      15 :
       begin // Minor 7th 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 3;
        FNote[3] := FNote[1] + 9;
        FNote[4] := FNote[1] + 7;
       end;
      16 :
       begin // Minor 7th 4 6
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 6;
        FNote[3] := FNote[1] + 9;
        FNote[4] := FNote[1] + 4;
       end;
      17 :
       begin // Minor 7th 7
        FNote[1] := random(67 - 53) + 53;
        FNote[2] := FNote[1] + 2;
        FNote[3] := FNote[1] + 5;
        FNote[4] := FNote[1] + 8;
       end;
     end;
   end;
end;

procedure TFmWinEar.NewButtons;
begin
  BtEnd.Visible := True;
  BtRepeat.Visible := True;
  BtMelodicIntervals.Visible := False;
  BtMelody.Visible := False;
  BtHarmonicIntervals.Enabled := False;
  BtThreeNoteChords.Enabled := False;
  BtFourNoteChords.Enabled := False;
end;

procedure TFmWinEar.NewButtons2;
begin
 PnChords.Visible := True;
end;

procedure TFmWinEar.OldButtons;
begin
  BtEnd.Visible := False;
  BtRepeat.Visible := False;
  BtMelodicIntervals.Visible := True;
  BtHarmonicIntervals.Enabled := True;
  BtMelody.Visible := True;
  BtThreeNoteChords.Enabled := True;
  BtFourNoteChords.Enabled := True;
  FIfchck := False;
end;

procedure TFmWinEar.OldButtons2;
begin
 PnChords.Visible := False;
end;

procedure TFmWinEar.BtMelodicIntervalsClick(Sender: TObject);
begin
  StopWatch.Enabled := True;
  FSecs := 0;
  FMins := 0;
  LbTime.Visible := True;
  FTestkind := tkMelodicIntervals;
  NewButtons;
  FOrde := 2;
  MidiReset;
  MelodicTest;
end;

procedure TFmWinEar.MelodicTest;
begin
  FIfchck := False;
  DeMark;
  Notefinder(False);
  Notemark(FNote[1] - 53);
  ItClick(FNote[1]);
  DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
  ItClick(FNote[2]);
  DelayedMidiNoteOff(140, FNote[2], SbVelocity.Position);
  FIfchck := True;
end;

procedure TFmWinEar.BtMelodyClick(Sender: TObject);
begin
  NewButtons;
  StopWatch.Enabled := True;
  FSecs := 0;
  FMins := 0;
  LbTime.Visible := True;
  FTestkind := tkMelody;
  FOrde := 2;
  MidiReset;
  MelodyTest;
end;

procedure TFmWinEar.HarmonicTest;
begin
  FIfchck := False;
  DeMark;
  Notefinder(False);
  if FNote[2] < FNote[1] then
   begin
    FNote[3] := FNote[1];
    FNote[1] := FNote[2];
    FNote[2] := FNote[3];
   end;
  Notemark(FNote[1] - 53);
  ItClick(FNote[1]);
  ItClick(FNote[2]);
  DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
  DelayedMidiNoteOff(0, FNote[2], SbVelocity.Position);
  FIfchck := True;
end;

procedure TFmWinEar.BtHarmonicIntervalsClick(Sender: TObject);
begin
  NewButtons;
  StopWatch.Enabled := True;
  FSecs := 0;
  FMins := 0;
  LbTime.Visible := True;
  FTestkind := tkHarmonicIntervals;
  FOrde := 2;
  MidiReset;
  HarmonicTest;
end;

procedure TFmWinEar.MelodyTest;
begin
  FIfchck := False;
  DeMark;
  Notefinder(True);
  Notemark(FNote[1] - 53);
  ItClick(FNote[1]);
  DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
  ItClick(FNote[2]);
  DelayedMidiNoteOff(140, FNote[2], SbVelocity.Position);
  ItClick(FNote[3]);
  DelayedMidiNoteOff(140, FNote[3], SbVelocity.Position);
  ItClick(FNote[4]);
  DelayedMidiNoteOff(140, FNote[4], SbVelocity.Position);
  ItClick(FNote[5]);
  DelayedMidiNoteOff(140, FNote[5], SbVelocity.Position);
  FIfchck := True;
end;

procedure TFmWinEar.BtThreeNoteChordsClick(Sender: TObject);
begin
  NewButtons;
  StopWatch.Enabled := True;
  FSecs := 0;
  FMins := 0;
  LbTime.Visible := True;
  FTestkind := tkThreeNoteChords;
  FOrde := 0;
  MidiReset;
  FochoTest(False);
end;

procedure TFmWinEar.FochoTest(four: Boolean);
begin
  FIfchck := False;
  DeMark;
  if four then
    Chordfinder(False)
  else
    Chordfinder(True);
  Notemark(FNote[1] - 53);
  ItClick(FNote[1]);
  ItClick(FNote[2]);
  ItClick(FNote[3]);
  if four then
    ItClick(FNote[4]);
  DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
  DelayedMidiNoteOff(0, FNote[2], SbVelocity.Position);
  DelayedMidiNoteOff(0, FNote[3], SbVelocity.Position);
  if four then
    DelayedMidiNoteOff(0, FNote[4], SbVelocity.Position);
  FIfchck := True;
end;

procedure TFmWinEar.BtEndClick(Sender: TObject);
begin
  OldButtons;
  if FTestkind = tkFourNoteChords
   then OldButtons2;
  StopWatch.Enabled := False;
  LbTime.Visible := False;
  DeMark;
end;

procedure TFmWinEar.BtFourNoteChordsClick(Sender: TObject);
begin
  NewButtons;
  NewButtons2;
  StopWatch.Enabled := True;
  FSecs := 0;
  FMins := 0;
  LbTime.Visible := True;
  FTestkind := tkFourNoteChords;
  FOrde := 0;
  MidiReset;
  FochoTest(True);
end;

procedure TFmWinEar.RepeatIt;
begin
 FIfchck := False;
 case FTestkind of
  tkMelodicIntervals :
   begin
    ItClick(FNote[1]);
    DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
    ItClick(FNote[2]);
    DelayedMidiNoteOff(140, FNote[2], SbVelocity.Position)
   end;
  tkHarmonicIntervals :
   begin
    ItClick(FNote[1]);
    ItClick(FNote[2]);
    DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
    DelayedMidiNoteOff(0, FNote[2], SbVelocity.Position);
   end;
  tkMelody :
   begin
    ItClick(FNote[1]);
    DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
    ItClick(FNote[2]);
    DelayedMidiNoteOff(140, FNote[2], SbVelocity.Position);
    ItClick(FNote[3]);
    DelayedMidiNoteOff(140, FNote[3], SbVelocity.Position);
    ItClick(FNote[4]);
    DelayedMidiNoteOff(140, FNote[4], SbVelocity.Position);
    ItClick(FNote[5]);
    DelayedMidiNoteOff(140, FNote[5], SbVelocity.Position);
   end;
  tkThreeNoteChords :
   begin
    ItClick(FNote[1]);
    ItClick(FNote[2]);
    ItClick(FNote[3]);
    DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
    DelayedMidiNoteOff(0, FNote[2], SbVelocity.Position);
    DelayedMidiNoteOff(0, FNote[3], SbVelocity.Position);
   end;
  tkFourNoteChords :
   begin
    ItClick(FNote[1]);
    ItClick(FNote[2]);
    ItClick(FNote[3]);
    ItClick(FNote[4]);
    DelayedMidiNoteOff(140, FNote[1], SbVelocity.Position);
    DelayedMidiNoteOff(0, FNote[2], SbVelocity.Position);
    DelayedMidiNoteOff(0, FNote[3], SbVelocity.Position);
    DelayedMidiNoteOff(0, FNote[4], SbVelocity.Position);
   end;
 end;
 FIfchck := True;
end;

procedure TFmWinEar.StopWatchTimer(Sender: TObject);
var
  times    : String[5];
  sec, min : String[2];
begin
  FSecs := FSecs + 1;
  if FSecs >= 60 then
   begin
    FMins := FMins + 1;
    FSecs := 0;
   end;
  Str(FSecs, sec);
  Str(FMins, min);
  if FSecs < 10 then
    sec := '0' + sec;
  times := min + ':' + sec;
  LbTime.Caption := times;
end;

procedure TFmWinEar.Timer2Timer(Sender: TObject);
begin
  FCnt := FCnt + 1;
end;

procedure TFmWinEar.BtMidiThroughClick(Sender: TObject);
begin
 if BtMidiThrough.Caption = 'Midi Thru: OFF'
   then BtMidiThrough.Caption := 'Midi Thru: ON '
   else BtMidiThrough.Caption := 'Midi Thru: OFF';
end;

procedure TFmWinEar.BtRepeatClick(Sender: TObject);
begin
  RepeatIt;
end;

procedure TFmWinEar.BtDominant7thClick(Sender: TObject);
begin
 case FCType of
  1, 2, 3, 4 : FochoTest(True);
  else Retry;
 end;
end;

procedure TFmWinEar.BtMajor7thClick(Sender: TObject);
begin
 case FCType of
  5, 6, 7, 8 : FochoTest(True);
  else Retry;
 end;
end;

procedure TFmWinEar.BtMinor7thClick(Sender: TObject);
begin
 case FCType of
  9, 10, 11, 12 : FochoTest(True);
  else Retry;
 end;
end;

procedure TFmWinEar.BtDiminished7thClick(Sender: TObject);
begin
 case FCType of
  13 : FochoTest(True);
  else Retry;
 end;
end;

procedure TFmWinEar.BtHalfDiminishedClick(Sender: TObject);
begin
 case FCType of
  14, 15, 16, 17 : FochoTest(True);
  else Retry;
 end;
end;

end.
