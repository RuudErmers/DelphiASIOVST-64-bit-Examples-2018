unit MidiGuiU;

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
//  The initial developer of this unit is Maik Menz (the-real-myco)           //
//                                                                            //
//  Code review and slight changes to match the global project rules by       //
//  by Christian-W. Budde                                                     //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, ExtCtrls, 
  DAV_GuiBaseControl, DAV_GuiMidiKeys, DAV_GuiMidiKeyZones;

type
  TKbDemoForm = class(TForm)
    Bevel: TBevel;
    ClipZoneBtn: TBitBtn;
    ColorizeAllBtn: TButton;
    ColorizeBlackBtn: TButton;
    ColorizeCBtn: TButton;
    ColorizeWhiteBtn: TButton;
    DeleteZoneBtn: TButton;
    LbRemoteCOntrol: TLabel;
    LbRemoteControlInfo: TLabel;
    LbZoneOptions: TLabel;
    LogMemo: TMemo;
    MainKb: TGuiMidiKeys;
    PnColorizeKeys: TPanel;
    PnRemoteControl: TPanel;
    PnZoneOptions: TPanel;
    RemoteKeyboard: TGuiMidiKeys;
    ResetColorBtn: TButton;
    ZoneNameBtn: TButton;
    ZoneNameEdit: TEdit;
    procedure RemoteKeyboardNoteOn(Sender: TObject; KeyNr: Byte; Velocity: Single);
    procedure RemoteKeyboardNoteOff(Sender: TObject; KeyNr: Byte);
    procedure ResetColorBtnClick(Sender: TObject);
    procedure ColorizeCBtnClick(Sender: TObject);
    procedure ColorizeWhiteBtnClick(Sender: TObject);
    procedure ColorizeAllBtnClick(Sender: TObject);
    procedure ColorizeBlackBtnClick(Sender: TObject);
    procedure DeleteZoneBtnClick(Sender: TObject);
    procedure MainKbMoveZoneBarDragging(Sender: TObject; KeyNr: Integer;
      var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,
      Y: Integer);
    procedure MainKbZoneSelectionChanged(Sender: TObject;
      Zone: TGuiKeyZoneItem);
    procedure ZoneNameBtnClick(Sender: TObject);
    procedure ClipZoneBtnClick(Sender: TObject);
    procedure MainKbStartZoneBarDragging(Sender: TObject; KeyNr: Integer;
      var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X, Y: Integer);
  end;

var
  KbDemoForm: TKbDemoForm;

implementation

{$R *.dfm}

uses
  Math;

resourcestring
  RCStrNoZoneSelected = 'No zone selected';
  RCStrNewSelectedZone = 'New selected zone';
  RCStrSizeZone = 'Size Zone';
  RCStrMoveZone = 'Move Zone';
  RCStrTestzone = 'Testzone';

function RandomColor(Light: Boolean): TColor;
var
  baseColor: Byte;
begin
  if Light then baseColor := 150 else baseColor := 0;
  result := (Random(100) + baseColor) shl 16
          + (Random(100) + baseColor) shl 8
          + (Random(100) + baseColor);
end;

procedure TKbDemoForm.RemoteKeyboardNoteOn(Sender: TObject; KeyNr: Byte;
  Velocity: Single);
begin
  MainKb.SetKeyPressed(KeyNr);
end;

procedure TKbDemoForm.RemoteKeyboardNoteOff(Sender: TObject; KeyNr: Byte);
begin
  MainKb.ReleaseKey(KeyNr);
end;

procedure TKbDemoForm.ResetColorBtnClick(Sender: TObject);
begin
  MainKb.RemoveKeyColor(0, CKeyboardHighestKey);
end;

procedure TKbDemoForm.ColorizeCBtnClick(Sender: TObject);
var
  i: Byte;
begin
 for i := 0 to CKeyboardMaxOctaves
  do MainKb.SetKeyColor(i * 12, i * 12, RandomColor(True), RandomColor(True), RandomColor(True));
end;

procedure TKbDemoForm.ColorizeWhiteBtnClick(Sender: TObject);
var
  i: Byte;
begin
 for i := 0 to CKeyboardHighestKey do
  if not (kfBlackKey in MainKb.Keys[i].Flags)
   then MainKb.SetKeyColor(i, i, RandomColor(True), RandomColor(True), RandomColor(True));
end;

procedure TKbDemoForm.ColorizeAllBtnClick(Sender: TObject);
var
  i: Byte;
begin
 for i := 0 to CKeyboardHighestKey do
  if kfBlackKey in MainKb.Keys[i].Flags
   then MainKb.SetKeyColor(i, i, RandomColor(False), RandomColor(False), RandomColor(False))
   else MainKb.SetKeyColor(i, i, RandomColor(True), RandomColor(True), RandomColor(True));
end;

procedure TKbDemoForm.ColorizeBlackBtnClick(Sender: TObject);
var
  i: Byte;
begin
 for i := 0 to CKeyboardHighestKey do
  if kfBlackKey in MainKb.Keys[i].Flags then
   MainKb.SetKeyColor(i,i, RandomColor(False), RandomColor(False), RandomColor(False));
end;

procedure TKbDemoForm.DeleteZoneBtnClick(Sender: TObject);
begin
 MainKb.KeyZones.DeleteSelected;
 DeleteZoneBtn.Enabled := False;  
 ZoneNameBtn.Enabled := False;
 ZoneNameEdit.Enabled := False;
end;

procedure TKbDemoForm.MainKbMoveZoneBarDragging(Sender: TObject;
  KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState;
  X, Y: Integer);
var
  tmp: integer;
begin
 if (KeyNr < 0) or (DragInfo.LastKey < 0) then Exit;

 if (DragInfo.Zone <> nil) and (KeyNr <> DragInfo.LastKey) then
  begin
   if mptOnLowestBorder in DragInfo.InZonePos then
    begin
     DragInfo.Zone.SetBorders(KeyNr, DragInfo.StartHighestZoneKey);
     LogMemo.Lines.Add(RCStrSizeZone + ': ' + DragInfo.Zone.DisplayName + ', to: ' + IntToStr(DragInfo.Zone.LowestZoneKey) + ', ' + IntToStr(DragInfo.Zone.HighestZoneKey));
    end else
   if mptOnHighestBorder in DragInfo.InZonePos then
    begin
     if DragInfo.Zone<>nil then DragInfo.Zone.SetBorders(DragInfo.StartLowestZoneKey, KeyNr);
     LogMemo.Lines.Add(RCStrSizeZone + ': ' + DragInfo.Zone.DisplayName + ', to: ' + IntToStr(DragInfo.Zone.LowestZoneKey) + ', ' + IntToStr(DragInfo.Zone.HighestZoneKey));
    end else
   if mptInZone in DragInfo.InZonePos then
    begin
     tmp := KeyNr-DragInfo.LastKey;
     DragInfo.Zone.MoveZone(tmp);
     LogMemo.Lines.Add(RCStrMoveZone + ': ' + DragInfo.Zone.DisplayName + ', to: ' + IntToStr(DragInfo.Zone.LowestZoneKey) + ', ' + IntToStr(DragInfo.Zone.HighestZoneKey));
    end else
   if mptOutside in DragInfo.InZonePos then
    begin
     DragInfo.Zone.SetBorders(KeyNr, DragInfo.StartKey);
     LogMemo.Lines.Add(RCStrSizeZone + ': ' + DragInfo.Zone.DisplayName + ', to: ' + IntToStr(DragInfo.Zone.LowestZoneKey) + ', ' + IntToStr(DragInfo.Zone.HighestZoneKey));
    end;
  end;
end;

procedure TKbDemoForm.MainKbZoneSelectionChanged(Sender: TObject;
  Zone: TGuiKeyZoneItem);
begin
 if Zone <> nil then
  begin
   LogMemo.Lines.Add(RCStrNewSelectedZone + ': ' + Zone.DisplayName);
   Zone.BringToFront;
   DeleteZoneBtn.Enabled := True;
   ZoneNameBtn.Enabled := True;
   ZoneNameEdit.Enabled := True;
   ZoneNameEdit.Text := Zone.DisplayName;
  end
 else
  begin
   LogMemo.Lines.Add(RCStrNoZoneSelected);
   DeleteZoneBtn.Enabled := False;
   ZoneNameBtn.Enabled := False;
   ZoneNameEdit.Enabled := False;
  end;
end;

procedure TKbDemoForm.ZoneNameBtnClick(Sender: TObject);
begin
  MainKb.KeyZones.Selected.DisplayName := ZoneNameEdit.Text; 
end;

procedure TKbDemoForm.ClipZoneBtnClick(Sender: TObject);
begin
  MainKb.KeyZones.ClipZones;
end;

procedure TKbDemoForm.MainKbStartZoneBarDragging(Sender: TObject;
  KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,
  Y: Integer);
begin
  if DragInfo.InZonePos = [mptOutside] then
  begin
    DragInfo.Zone := MainKb.KeyZones.Add;
    with DragInfo.Zone do
    begin
      LowestZoneKey := KeyNr;
      HighestZoneKey := KeyNr;
      Select(False);
      DefaultBrushColor := RandomColor(True);
      HoverBrushColor := RandomColor(True);
      SelectedBrushColor := RandomColor(True);
      DisplayName := RCStrTestzone + IntToStr(Random(100));
    end;
    MainKbZoneSelectionChanged(Self, DragInfo.Zone);
  end;
end;

end.
