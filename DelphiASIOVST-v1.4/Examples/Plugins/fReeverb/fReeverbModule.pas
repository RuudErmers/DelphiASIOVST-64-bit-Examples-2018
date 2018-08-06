unit fReeverbModule;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspFreeverbFilter;

const
  CStereoSpread = 23;

  // These values assume 44.1KHz sample rate they will probably be OK for
  // 48KHz sample rate but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  CAllpassTunings : array [0..3] of Integer = (556, 441, 341, 225);
  CCombTunings : array [0..7] of Integer = (1116, 1188, 1277, 1356, 1422, 1491,
    1557, 1617);

type
  TCombArray    = array [0..1] of TFreeverbCombFilter;
  TAllpassArray = array [0..1] of TFreeverbAllpass;

  TfReeverbVST = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessReplacing(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRoomSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFreezeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterStretchChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNumCombsChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNumAllpassesChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FGain      : Single;
    FRoomSize  : Single;
    FRoomSizeI : Single;
    FDamp      : Single;
    FDampA     : Single;
    FWet       : Single;
    FWet1      : Single;
    FWet2      : Single;
    FDry       : Single;
    FWidth     : Single;
    FMode      : Single;
    FStretch   : Single;

    FAllpass   : array of TAllpassArray; // allpass filters
    FComb      : array of TCombArray;    // comb filters
    function GetRoomSize: Single;
    function GetDamp: Single;
    function GetMode: Single;
    procedure SetDamp(Value: Single);
    procedure SetRoomSize(Value: Single);
    procedure SetWet(Value: Single);
    procedure SetWidth(Value: Single);
    procedure SetMode(Value: Single);
  protected
    procedure UpdateMix;
    procedure Update;
    procedure ShuffleAllPassFeedBack;
    procedure BufferRezize;
  public
    procedure Mute;
    property Mode: Single read GetMode write SetMode;
    property Width: Single read FWidth write SetWidth;
    property Dry: Single read FDry write FDry;
    property Wet: Single read FWet write SetWet;
    property Damp: Single read GetDamp write SetDamp;
    property RoomSize: Single read GetRoomSize write SetRoomSize;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_VSTCustomModule, fReeverbGUI;

procedure TfReeverbVST.VSTModuleOpen(Sender: TObject);
var
  AllpassIndex : Integer;
  CombIndex    : Integer;
begin
 FStretch := 1;

 SetLength(FComb, Length(CCombTunings));
 SetLength(FAllpass, Length(CAllpassTunings));

 // create allpasses & set default values
 for AllpassIndex := 0 to Length(CAllpassTunings) - 1 do
  begin
   FAllpass[AllpassIndex, 0] := TFreeverbAllpass.Create(CAllpassTunings[AllpassIndex]);
   FAllpass[AllpassIndex, 1] := TFreeverbAllpass.Create(CAllpassTunings[AllpassIndex] + CStereoSpread);
   FAllpass[AllpassIndex, 0].Feedback := 0.5;
   FAllpass[AllpassIndex, 1].Feedback := 0.5;
  end;

 // create comb filters
 for CombIndex := 0 to Length(CCombTunings) - 1 do
  begin
   FComb[CombIndex, 0] := TFreeverbCombFilter.Create(CCombTunings[CombIndex]);
   FComb[CombIndex, 1] := TFreeverbCombFilter.Create(CCombTunings[CombIndex] + CStereoSpread);
  end;

 Wet := 1;
 RoomSize := cInitialRoom;
 Dry := 1;
 Damp := cInitialDamp;
 Width := cInitialWidth;
 Mode := cInitialMode;
 Mute;

 // default parameter
 Parameter[0] := 0.5;
 Parameter[1] := 0.5;
 Parameter[2] := 0.5;
 Parameter[3] := 0.5;
 Parameter[4] := 0;
 Parameter[5] := 0;
 Parameter[6] := 0.5;

 // default preset
 with Programs[0] do
  begin
   Parameter[0] := 0.5;
   Parameter[1] := 0.5;
   Parameter[2] := 0.5;
   Parameter[3] := 0.5;
   Parameter[4] := 0;
   Parameter[5] := 0;
   Parameter[6] := 0.5;
  end;

 // preset 1
 with Programs[1] do
  begin
   Parameter[0] := 0.5;
   Parameter[1] := 0.6;
   Parameter[2] := 0.4;
   Parameter[3] := 0.5;
   Parameter[4] := 0;
   Parameter[5] := 0;
   Parameter[6] := 1;
  end;

 // preset 2
 with Programs[2] do
  begin
   Parameter[0] := 0.2;
   Parameter[1] := 0.6;
   Parameter[2] := 0.8;
   Parameter[3] := 1;
   Parameter[4] := 0;
   Parameter[5] := 1;
   Parameter[6] := 1;
  end;

 // preset 3
 with Programs[3] do
  begin
   Parameter[0] := Random;
   Parameter[1] := Random;
   Parameter[2] := Random;
   Parameter[3] := Random;
   Parameter[4] := 0;
   Parameter[5] := Random;
   Parameter[6] := Random;
  end;

 // set editor form class
 EditorFormClass := TFmReverb;
end;

procedure TfReeverbVST.VSTModuleClose(Sender: TObject);
var
  AllpassIndex : Integer;
  CombIndex    : Integer;
begin
 for AllpassIndex := 0 to Length(FAllpass) - 1 do
  begin
   if Assigned(FAllpass[AllpassIndex, 0]) then FreeAndNil(FAllpass[AllpassIndex, 0]);
   if Assigned(FAllpass[AllpassIndex, 1]) then FreeAndNil(FAllpass[AllpassIndex, 1]);
  end;

 for CombIndex := 0 to Length(FComb) - 1 do
  begin
   if Assigned(FComb[CombIndex, 0]) then FreeAndNil(FComb[CombIndex, 0]);
   if Assigned(FComb[CombIndex, 1]) then FreeAndNil(FComb[CombIndex, 1]);
  end;
end;

function TfReeverbVST.GetDamp: Single;
begin
 Result := FDamp / CScaleDamp;
end;

function TfReeverbVST.GetMode: Single;
begin
 if FMode >= CFreezeMode
  then Result := 1
  else Result := 0;
end;

function TfReeverbVST.GetRoomSize: Single;
begin
 Result := (FRoomSize - cOffsetRoom) / CScaleRoom;
end;

procedure TfReeverbVST.Mute;
var
  Index: Integer;
begin
 if FMode >= CFreezeMode then Exit;

 for Index := 0 to Length(FComb) - 1 do
  begin
   if Assigned(FComb[Index, 0]) then FComb[Index, 0].Mute;
   if Assigned(FComb[Index, 1]) then FComb[Index, 1].Mute;
  end;

 for Index := 0 to Length(FAllpass) - 1 do
  begin
   if Assigned(FAllpass[Index, 0]) then FAllpass[Index, 0].Mute;
   if Assigned(FAllpass[Index, 1]) then FAllpass[Index, 1].Mute;
  end;
end;

procedure TfReeverbVST.SetDamp(Value: Single);
begin
 FDamp := Value * CScaleDamp;
 Update;
end;

procedure TfReeverbVST.SetMode(Value: Single);
begin
 FMode := Value;
 Update;
end;

procedure TfReeverbVST.SetRoomSize(Value: Single);
begin
 FRoomSize := (Value * CScaleRoom) + cOffsetRoom;
 Update;
end;

procedure TfReeverbVST.SetWet(Value: Single);
begin
 FWet := Value;
 UpdateMix;
end;

procedure TfReeverbVST.SetWidth(Value: Single);
begin
 FWidth := Value;
 UpdateMix;
end;

procedure TfReeverbVST.UpdateMix;
begin
 // Recalculate internal values after parameter change
 FWet1 := FWet * 0.5 * (1 + FWidth);
 FWet2 := FWet * 0.5 * (1 - FWidth);
end;

procedure TfReeverbVST.Update;
var
  Index: Integer;
begin
 // Recalculate internal values after parameter change
 if FMode >= CFreezeMode then
  begin
   FRoomSizeI := 1;
   FDampA := 0;
   FGain := cMuted;
  end
 else
  begin
   FRoomSizeI := FRoomSize;
   FDampA := FDamp;
   FGain := cFixedGain;
  end;

 for Index := 0 to Length(FComb) - 1 do
  begin
   if Assigned(FComb[Index, 0]) then
    begin
     FComb[Index, 0].Feedback := FRoomSizeI;
     FComb[Index, 0].Damp := FDampA;
    end;

   if Assigned(FComb[Index, 1]) then
    begin
     FComb[Index, 1].Feedback := FRoomSizeI;
     FComb[Index, 1].Damp := FDampA;
    end;
  end;
end;

procedure TfReeverbVST.ShuffleAllPassFeedBack;
var
  Index : Integer;
begin
 for Index := 0 to Length(FAllpass) - 1 do
  begin
   FAllpass[Index, 0].Feedback := 0.5 + 0.4 * Random;
   FAllpass[Index, 1].Feedback := 0.5 + 0.4 * Random;
  end;
end;

procedure TfReeverbVST.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  OutL, OutR : Single;
  Inp        : Single;
  i, j       : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   OutL := Inputs[0, i];
   OutR := Inputs[1, i];
   Inp := (Inputs[0, i] + Inputs[1, i]) * FGain;

   // Accumulate comb filters in parallel
   for j := 0 to Length(FComb) - 1 do
    begin
     OutL := OutL + FComb[j, 0].ProcessSample32(Inp);
     OutR := OutR + FComb[j, 1].ProcessSample32(Inp);
    end;

   // Feed through allpasses in series
   for j := 0 to Length(FAllpass) - 1 do
    begin
     outL := FAllpass[j, 0].ProcessSample32(OutL);
     outR := FAllpass[j, 1].ProcessSample32(OutR);
    end;

   // Calculate output MIXING with anything already there
   Outputs[0,i]  := Outputs[0, i] + OutL * FWet1 + OutR * FWet2 + Inputs[0, i] * FDry;
   Outputs[1,i]  := Outputs[1, i] + OutR * FWet1 + OutL * FWet2 + Inputs[1, i] * FDry;
  end;
end;

procedure TfReeverbVST.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  OutL, OutR  : Single;
  Inp         : Single;
  SampleIndex : Integer;
  Index       : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   OutL := 0;
   OutR := 0;
   Inp := (Inputs[0, SampleIndex] + Inputs[1, SampleIndex]) * FGain;

   // Accumulate comb filters in parallel
   for Index := 0 to Length(FComb) - 1 do
    begin
     OutL := OutL + FComb[Index, 0].ProcessSample32(Inp);
     OutR := OutR + FComb[Index, 1].ProcessSample32(Inp);
    end;

   // Feed through allpasses in series
   for Index := 0 to Length(FAllpass) - 1 do
    begin
     OutL := FAllpass[Index, 0].ProcessSample32(OutL);
     OutR := FAllpass[Index, 1].ProcessSample32(OutR);
    end;

   // Calculate output REPLACING anything already there
   Outputs[0, SampleIndex] := OutL * FWet1 + OutR * FWet2 + Inputs[0, SampleIndex] * FDry;
   Outputs[1, SampleIndex] := OutR * FWet1 + OutL * FWet2 + Inputs[1, SampleIndex] * FDry;
  end;
end;

procedure TfReeverbVST.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Dry := Value;

 // update GUI
 if EditorForm is TFmReverb
  then TFmReverb(EditorForm).UpdateDry;
end;

procedure TfReeverbVST.ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Wet := Value;

 // update GUI
 if EditorForm is TFmReverb
  then TFmReverb(EditorForm).UpdateWet;
end;

procedure TfReeverbVST.ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Width := Value;

 // update GUI
 if EditorForm is TFmReverb
  then TFmReverb(EditorForm).UpdateWidth;
end;

procedure TfReeverbVST.ParameterRoomSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 RoomSize := Value;
 ShuffleAllPassFeedBack;

 // update GUI
 if EditorForm is TFmReverb
  then TFmReverb(EditorForm).UpdateSize;
end;

procedure TfReeverbVST.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 if Abs(SampleRate) > 0
  then BufferRezize;
end;

procedure TfReeverbVST.BufferRezize;
var
  AllpassIndex : Integer;
  CombIndex    : Integer;
  Scale        : Single;
begin
 Scale := Abs(SampleRate) / 44100 * FStretch;

 for AllpassIndex := 0 to Length(FAllpass) - 1 do
  begin
   if Assigned(FAllpass[AllpassIndex, 0]) then FAllpass[AllpassIndex, 0].BufferSize := Round(CAllpassTunings[AllpassIndex] * Scale);
   if Assigned(FAllpass[AllpassIndex, 1]) then FAllpass[AllpassIndex, 1].BufferSize := Round((CAllpassTunings[AllpassIndex] + CStereoSpread) * Scale);
  end;

 for CombIndex := 0 to Length(FComb) - 1 do
  begin
   if Assigned(FComb[CombIndex, 0]) then FComb[CombIndex, 0].BufferSize := Round(CCombTunings[CombIndex] * Scale);
   if Assigned(FComb[CombIndex, 1]) then FComb[CombIndex, 1].BufferSize := Round((CCombTunings[CombIndex] + CStereoSpread) * Scale);
  end;
end;

procedure TfReeverbVST.ParameterFreezeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Mode := Value;
end;

procedure TfReeverbVST.ParameterStretchChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FStretch := 1 + 9 * Value;
 BufferRezize;

 // update GUI
 if EditorForm is TFmReverb then
  with TFmReverb(EditorForm)
   do UpdateStretch;
end;

procedure TfReeverbVST.ParameterDampChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Damp := Value;

 // update GUI
 if EditorForm is TFmReverb then
  with TFmReverb(EditorForm)
   do UpdateDamp;
end;

procedure TfReeverbVST.ParameterNumAllpassesChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength, AllpassIndex : Integer;
begin
 Exit;

 oldLength := Length(FAllpass);
 if (Value < 1) or (oldLength = Round(Value))
  then Exit;

 if (oldLength < Round(Value)) then
  begin
   SetLength(FAllpass, Round(Value));
   for AllpassIndex := oldLength to Length(FAllpass) - 1 do
    begin
     FAllpass[AllpassIndex, 0] := TFreeverbAllpass.Create(1000);
     FAllpass[AllpassIndex, 1] := TFreeverbAllpass.Create(1023);
    end;
  end
 else SetLength(FAllpass, Round(Value));
end;

procedure TfReeverbVST.ParameterNumCombsChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength, CombIndex : Integer;
begin
 Exit;

 oldLength := Length(FComb);
 if (Value < 1) or (oldLength = Round(Value))
  then Exit;

 if (oldLength < Round(Value)) then
  begin
   SetLength(FComb, Round(Value));
   for CombIndex := oldLength to Length(FComb) - 1 do
    begin
     FComb[CombIndex, 0] := TFreeverbCombFilter.Create(1000);
     FComb[CombIndex, 1] := TFreeverbCombFilter.Create(1023);
    end;
  end
 else
  begin
   for CombIndex := oldLength to Round(Value) - 1 do
    begin
     FreeAndNil(FComb[CombIndex, 0]);
     FreeAndNil(FComb[CombIndex, 1]);
    end;
   SetLength(FComb, Round(Value));
  end;
end;

end.
