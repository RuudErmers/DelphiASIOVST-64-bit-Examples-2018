unit fReeverbDSP;

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
  // These values assume 44.1KHz sample rate
  // they will probably be OK for 48KHz sample rate
  // but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  CCombTuningL1 = 1116;
  CCombTuningL2 = 1188;
  CCombTuningL3 = 1277;
  CCombTuningL4 = 1356;
  CCombTuningL5 = 1422;
  CCombTuningL6 = 1491;
  CCombTuningL7 = 1557;
  CCombTuningL8 = 1617;
  CAllpassTuningL1 = 556;
  CAllpassTuningL2 = 441;
  CAllpassTuningL3 = 341;
  CAllpassTuningL4 = 225;

type
  TCombArray    = array [0..1] of TFreeverbCombFilter;
  TAllpassArray = array [0..1] of TFreeverbAllpass;

  TfReeverbVST = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
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
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FCriticalSection : TCriticalSection;
    FGain            : Single;
    FRoomSize        : Single;
    FRoomSizeI       : Single;
    FDamp            : Single;
    FDampA           : Single;
    FWet             : Single;
    FWet1            : Single;
    FWet2            : Single;
    FDry             : Single;
    FWidth           : Single;
    FMode            : Single;
    FStretch         : Single;

    FComb            : array of TCombArray; // Comb filters
    FAllpass         : array of TAllpassArray; // Allpass filters
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
  DAV_VSTCustomModule;

procedure TfReeverbVST.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TfReeverbVST.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TfReeverbVST.VSTModuleOpen(Sender: TObject);
var
  Index : Integer;
begin
 FStretch := 1;
 SetLength(FComb, 8);
 SetLength(FAllpass, 4);

 FComb[0, 0] := TFreeverbCombFilter.Create(CCombTuningL1);
 FComb[0, 1] := TFreeverbCombFilter.Create(CCombTuningL1 + CStereoSpread);
 FComb[1, 0] := TFreeverbCombFilter.Create(CCombTuningL2);
 FComb[1, 1] := TFreeverbCombFilter.Create(CCombTuningL2 + CStereoSpread);
 FComb[2, 0] := TFreeverbCombFilter.Create(CCombTuningL3);
 FComb[2, 1] := TFreeverbCombFilter.Create(CCombTuningL3 + CStereoSpread);
 FComb[3, 0] := TFreeverbCombFilter.Create(CCombTuningL4);
 FComb[3, 1] := TFreeverbCombFilter.Create(CCombTuningL4 + CStereoSpread);
 FComb[4, 0] := TFreeverbCombFilter.Create(CCombTuningL5);
 FComb[4, 1] := TFreeverbCombFilter.Create(CCombTuningL5 + CStereoSpread);
 FComb[5, 0] := TFreeverbCombFilter.Create(CCombTuningL6);
 FComb[5, 1] := TFreeverbCombFilter.Create(CCombTuningL6 + CStereoSpread);
 FComb[6, 0] := TFreeverbCombFilter.Create(CCombTuningL7);
 FComb[6, 1] := TFreeverbCombFilter.Create(CCombTuningL7 + CStereoSpread);
 FComb[7, 0] := TFreeverbCombFilter.Create(CCombTuningL8);
 FComb[7, 1] := TFreeverbCombFilter.Create(CCombTuningL8 + CStereoSpread);
 FAllpass[0, 0] := TFreeverbAllpass.Create(CAllpassTuningL1);
 FAllpass[0, 1] := TFreeverbAllpass.Create(CAllpassTuningL1 + CStereoSpread);
 FAllpass[1, 0] := TFreeverbAllpass.Create(CAllpassTuningL2);
 FAllpass[1, 1] := TFreeverbAllpass.Create(CAllpassTuningL2 + CStereoSpread);
 FAllpass[2, 0] := TFreeverbAllpass.Create(CAllpassTuningL3);
 FAllpass[2, 1] := TFreeverbAllpass.Create(CAllpassTuningL3 + CStereoSpread);
 FAllpass[3, 0] := TFreeverbAllpass.Create(CAllpassTuningL4);
 FAllpass[3, 1] := TFreeverbAllpass.Create(CAllpassTuningL4 + CStereoSpread);

 // Set default values
 for Index := 0 to Length(FAllpass)-1 do
  begin
   FAllpass[Index, 0].Feedback := 0.5;
   FAllpass[Index, 1].Feedback := 0.5;
  end;
 Wet := 1;
 RoomSize := CInitialRoom;
 Dry := 1;
 Damp := CInitialDamp;
 Width := CInitialWidth;
 Mode := CInitialMode;
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
end;

procedure TfReeverbVST.VSTModuleClose(Sender: TObject);
var
  Index: Integer;
begin
 for Index := 0 to 3 do
  begin
   if Assigned(FAllpass[Index, 0]) then FreeAndNil(FAllpass[Index, 0]);
   if Assigned(FAllpass[Index, 1]) then FreeAndNil(FAllpass[Index, 1]);
  end;
 for Index := 0 to 7 do
  begin
   if Assigned(FComb[Index, 0]) then FreeAndNil(FComb[Index, 0]);
   if Assigned(FComb[Index, 1]) then FreeAndNil(FComb[Index, 1]);
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
 Result := (FRoomSize - cOffsetRoom) / cScaleRoom;
end;

procedure TfReeverbVST.Mute;
var
  i: Integer;
begin
 FCriticalSection.Enter;
 try
  if FMode >= CFreezeMode then Exit;
  for i := 0 to Length(FComb) - 1 do
   begin
    FComb[i, 0].Mute;
    FComb[i, 1].Mute;
   end;
  for i := 0 to Length(FAllpass) - 1 do
   begin
    FAllpass[i, 0].Mute;
    FAllpass[i, 1].Mute;
   end;
 finally
  FCriticalSection.Leave;
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
 FRoomSize := (Value * cScaleroom) + cOffsetRoom;
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
 FCriticalSection.Enter;
 try
  // Recalculate internal values after parameter change
  FWet1 := FWet * (FWidth * 0.5 + 0.5);
  FWet2 := FWet * ((1 - FWidth) * 0.5);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TfReeverbVST.Update;
var
  i : Integer;
begin
 FCriticalSection.Enter;
 try
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
  for i := 0 to Length(FComb) - 1 do
   begin
    FComb[i, 0].Feedback := FRoomSizeI;
    FComb[i, 1].Feedback := FRoomSizeI;
    FComb[i, 0].Damp := FDampA;
    FComb[i, 1].Damp := FDampA;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TfReeverbVST.ShuffleAllPassFeedBack;
var
  i : Integer;
begin
 FCriticalSection.Enter;
 try
  for i := 0 to Length(FAllpass) - 1 do
   begin
    FAllpass[i, 0].Feedback := 0.5 + 0.4 * Random;
    FAllpass[i, 1].Feedback := 0.5 + 0.4 * Random;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TfReeverbVST.VSTModuleProcess(const inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  OutL, OutR, Inp    : Single;
  SampleIndex, Index : Integer;
begin
 FCriticalSection.Enter;
 try
  for SampleIndex := 0 to SampleFrames - 1 do
   begin
    OutL := Inputs[0, SampleIndex];
    OutR := Inputs[1, SampleIndex];
    Inp := (Inputs[0, SampleIndex] + Inputs[1, SampleIndex]) * FGain;
    // Accumulate comb filters in parallel
    for Index := 0 to Length(FComb) - 1 do
     begin
      OutL := OutL + FComb[Index, 0].ProcessSample32(inp);
      OutR := OutR + FComb[Index, 1].ProcessSample32(inp);
     end;
    // Feed through allpasses in series
    for Index := 0 to Length(FAllpass) - 1 do
     begin
      outL := FAllpass[Index, 0].ProcessSample32(OutL);
      outR := FAllpass[Index, 1].ProcessSample32(OutR);
     end;
    // Calculate output MIXING with anything already there
    Outputs[0, SampleIndex] := Outputs[0, SampleIndex] + OutL * FWet1 + OutR * FWet2 + Inputs[0, SampleIndex] * FDry;
    Outputs[1, SampleIndex] := Outputs[1, SampleIndex] + OutR * FWet1 + OutL * FWet2 + Inputs[1, SampleIndex] * FDry;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TfReeverbVST.VSTModuleProcessReplacing(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  OutL, OutR, Inp    : Single;
  SampleIndex, Index : Integer;
begin
 FCriticalSection.Enter;
 try
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
      outL := FAllpass[Index, 0].ProcessSample32(OutL);
      outR := FAllpass[Index, 1].ProcessSample32(OutR);
     end;
    // Calculate output REPLACING anything already there
    Outputs[0,SampleIndex] := OutL * FWet1 + OutR * FWet2 + Inputs[0, SampleIndex] * FDry;
    Outputs[1,SampleIndex] := OutR * FWet1 + OutL * FWet2 + Inputs[1, SampleIndex] * FDry;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TfReeverbVST.ParameterDryChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Dry := Value;
end;

procedure TfReeverbVST.ParameterWetChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Wet := Value;
end;

procedure TfReeverbVST.ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Width := Value;
end;

procedure TfReeverbVST.ParameterRoomSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 RoomSize := Value;
 ShuffleAllPassFeedBack;
end;

procedure TfReeverbVST.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 if Abs(SampleRate) > 0
  then BufferRezize;
end;

procedure TfReeverbVST.BufferRezize;
var
  Scale : Single;
begin
 Scale := Abs(SampleRate) / 44100 * FStretch;

 FComb[0, 0].BufferSize := Round(CCombTuningL1 * Scale);
 FComb[0, 1].BufferSize := Round((CCombTuningL1 + CStereoSpread) * Scale);
 FComb[1, 0].BufferSize := Round(CCombTuningL2 * Scale);
 FComb[1, 1].BufferSize := Round((CCombTuningL2 + CStereoSpread) * Scale);
 FComb[2, 0].BufferSize := Round(CCombTuningL3 * Scale);
 FComb[2, 1].BufferSize := Round((CCombTuningL3 + CStereoSpread) * Scale);
 FComb[3, 0].BufferSize := Round(CCombTuningL4 * Scale);
 FComb[3, 1].BufferSize := Round((CCombTuningL4 + CStereoSpread) * Scale);
 FComb[4, 0].BufferSize := Round(CCombTuningL5 * Scale);
 FComb[4, 1].BufferSize := Round((CCombTuningL5 + CStereoSpread) * Scale);
 FComb[5, 0].BufferSize := Round(CCombTuningL6 * Scale);
 FComb[5, 1].BufferSize := Round((CCombTuningL6 + CStereoSpread) * Scale);
 FComb[6, 0].BufferSize := Round(CCombTuningL7 * Scale);
 FComb[6, 1].BufferSize := Round((CCombTuningL7 + CStereoSpread) * Scale);
 FComb[7, 0].BufferSize := Round(CCombTuningL8 * Scale);
 FComb[7, 1].BufferSize := Round((CCombTuningL8 + CStereoSpread) * Scale);
 FAllpass[0, 0].BufferSize := Round(CAllpassTuningL1 * Scale);
 FAllpass[0, 1].BufferSize := Round((CAllpassTuningL1 + CStereoSpread) * Scale);
 FAllpass[1, 0].BufferSize := Round(CAllpassTuningL2 * Scale);
 FAllpass[1, 1].BufferSize := Round((CAllpassTuningL2 + CStereoSpread) * Scale);
 FAllpass[2, 0].BufferSize := Round(CAllpassTuningL3 * Scale);
 FAllpass[2, 1].BufferSize := Round((CAllpassTuningL3 + CStereoSpread) * Scale);
 FAllpass[3, 0].BufferSize := Round(CAllpassTuningL4 * Scale);
 FAllpass[3, 1].BufferSize := Round((CAllpassTuningL4 + CStereoSpread) * Scale);
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
end;

procedure TfReeverbVST.ParameterDampChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 Damp := Value;
end;

procedure TfReeverbVST.ParameterNumAllpassesChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength, i : Integer;
begin
 oldLength := Length(FAllpass);
 if (Value < 1) or (oldLength = Round(Value))
  then exit;

 if (oldLength < Round(Value)) then
  begin
   SetLength(FAllpass, Round(Value));
   for i := oldLength to Length(FAllpass) - 1 do
    begin
     FAllpass[i, 0] := TFreeverbAllpass.Create(1000);
     FAllpass[i, 1] := TFreeverbAllpass.Create(1023);
    end;
  end
 else SetLength(FAllpass, Round(Value));
end;

procedure TfReeverbVST.ParameterNumCombsChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  oldLength, i : Integer;
begin
 oldLength := Length(FComb);
 if (Value < 1) or (oldLength = Round(Value))
  then exit;

 if (oldLength < Round(Value)) then
  begin
   SetLength(FComb, Round(Value));
   for i := oldLength to Length(FComb) - 1 do
    begin
     FComb[i, 0] := TFreeverbCombFilter.Create(1000);
     FComb[i, 1] := TFreeverbCombFilter.Create(1023);
    end;
  end
 else
  begin
   for i := oldLength to Round(Value) - 1 do
    begin
     FreeAndNil(FComb[i, 0]);
     FreeAndNil(FComb[i, 1]);
    end;
   SetLength(FComb, Round(Value));
  end;
end;

end.
