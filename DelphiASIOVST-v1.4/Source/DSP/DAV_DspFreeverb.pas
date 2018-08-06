unit DAV_DspFreeverb;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspFreeverbFilter;

const
  CCombFilterCount = 8;
  CAllpassCount    = 4;

  // These values assume 44.1KHz sample rate
  // they will probably be OK for 48KHz sample rate
  // but would need scaling for 96KHz (or other) sample rates.
  // The values were obtained by listening tests.
  CCombTuningL1    = 1116;
  CCombTuningL2    = 1188;
  CCombTuningL3    = 1277;
  CCombTuningL4    = 1356;
  CCombTuningL5    = 1422;
  CCombTuningL6    = 1491;
  CCombTuningL7    = 1557;
  CCombTuningL8    = 1617;
  CAllpassTuningL1 = 556;
  CAllpassTuningL2 = 441;
  CAllpassTuningL3 = 341;
  CAllpassTuningL4 = 225;

  // Allpass filter class declaration

type
  TFreeverbCombArray = array [0..CCombFilterCount - 1] of TFreeverbCombFilter;
  TFreeverbAllpassArray = array [0..CAllpassCount - 1] of TFreeverbAllpass;

  // Reverb model class declaration
  TFreeverb = class(TDspSampleRatePersistent, IDspProcessor64)
  private
    FGain       : Double;
    FRoomSize   : Double;
    FDamp       : Double;
    FWet        : Double;
    FDry        : Double;
    FComb       : TFreeverbCombArray;    // Comb filters
    FAllpass    : TFreeverbAllpassArray; // Allpass filters
    procedure SetDamp(const Value: Double);
    procedure SetDry(const Value: Double);
    procedure SetRoomSize(const Value: Double);
    procedure SetWet(const Value: Double);
  protected
    procedure DampChanged; virtual;
    procedure DryChanged; virtual;
    procedure RoomsizeChanged; virtual;
    procedure WetChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Reset;

    function ProcessSample32(Input: Single): Single;
    function ProcessSample64(Input: Double): Double;
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);
  published
    property Dry: Double read FDry write SetDry;
    property Wet: Double read FWet write SetWet;
    property Damp: Double read FDamp write SetDamp;
    property RoomSize: Double read FRoomSize write SetRoomSize;
    property SampleRate;
  end;

implementation

uses
  SysUtils;

constructor TFreeverb.Create;
const
  CReci441 : Single = 1 / 44100;
begin
  inherited Create;
  FComb[0]    := TFreeverbCombFilter.Create(Round(CCombTuningL1 * SampleRate * CReci441));
  FComb[1]    := TFreeverbCombFilter.Create(Round(CCombTuningL2 * SampleRate * CReci441));
  FComb[2]    := TFreeverbCombFilter.Create(Round(CCombTuningL3 * SampleRate * CReci441));
  FComb[3]    := TFreeverbCombFilter.Create(Round(CCombTuningL4 * SampleRate * CReci441));
  FComb[4]    := TFreeverbCombFilter.Create(Round(CCombTuningL5 * SampleRate * CReci441));
  FComb[5]    := TFreeverbCombFilter.Create(Round(CCombTuningL6 * SampleRate * CReci441));
  FComb[6]    := TFreeverbCombFilter.Create(Round(CCombTuningL7 * SampleRate * CReci441));
  FComb[7]    := TFreeverbCombFilter.Create(Round(CCombTuningL8 * SampleRate * CReci441));
  FAllpass[0] := TFreeverbAllpass.Create(Round(CAllpassTuningL1 * SampleRate * CReci441));
  FAllpass[1] := TFreeverbAllpass.Create(Round(CAllpassTuningL2 * SampleRate * CReci441));
  FAllpass[2] := TFreeverbAllpass.Create(Round(CAllpassTuningL3 * SampleRate * CReci441));
  FAllpass[3] := TFreeverbAllpass.Create(Round(CAllpassTuningL4 * SampleRate * CReci441));

  Wet      := 1;
  Dry      := 1;
  RoomSize := 0.5;
  Damp     := 0.5;
end;

destructor TFreeverb.Destroy;
var
  Index : Integer;
begin
 for Index := 0 to CAllpassCount - 1 do FreeAndNil(FAllpass[Index]);
 for Index := 0 to CCombFilterCount - 1 do FreeAndNil(FComb[Index]);
 inherited;
end;

procedure TFreeverb.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TFreeverb.ProcessSample32(Input: Single): Single;
var
  Band: Integer;
begin
 Result := FGain * Input;

 // Accumulate comb filters in parallel
 for Band := 0 to CCombFilterCount - 1
  do Result := Result + FComb[Band].ProcessSample32(Input);

 // Feed through allpasses in series
 for Band := 0 to CAllpassCount - 1
  do Result := FAllpass[Band].ProcessSample32(Result);

 Result := Result * FWet + Input * FDry;
end;

function TFreeverb.ProcessSample64(Input: Double): Double;
var
  Band: Integer;
begin
 Result := FGain * Input;

 // Accumulate comb filters in parallel
 for Band := 0 to CCombFilterCount - 1
  do Result := Result + FComb[Band].ProcessSample32(Input);

 // Feed through allpasses in series
 for Band := 0 to CAllpassCount - 1
  do Result := FAllpass[Band].ProcessSample32(Result);

 Result := Result * FWet + Input * FDry;
end;

procedure TFreeverb.Reset;
var
  i : Integer;
begin
 for i := 0 to CCombFilterCount - 1 do FComb[i].Mute;
 for i := 0 to CAllpassCount - 1 do FAllpass[i].Mute;
end;

procedure TFreeverb.SetDamp(const Value: Double);
begin
 if FDamp <> Value then
  begin
   FDamp := Value;
   DampChanged;
  end;
end;

procedure TFreeverb.DampChanged;
var
  i : Integer;
begin
 for i := 0 to CCombFilterCount - 1 do
  begin
   FComb[i].Feedback := FRoomSize;
   FComb[i].Damp     := FDamp;
  end;
 Changed;
end;

procedure TFreeverb.SetRoomSize(const Value: Double);
begin
 if FRoomSize <> Value then
  begin
   FRoomSize := Value;
   RoomsizeChanged;
  end;
end;

procedure TFreeverb.RoomsizeChanged;
var
  Index : Integer;
begin
 for Index := 0 to CCombFilterCount - 1
  do FComb[Index].Feedback := FRoomSize;
 Changed;
end;

procedure TFreeverb.SampleRateChanged;
begin
 inherited;
end;

procedure TFreeverb.SetWet(const Value: Double);
begin
 if FWet <> Value then
  begin
   FWet := Value;
   WetChanged;
  end;
end;

procedure TFreeverb.WetChanged;
begin
 Changed;
end;

procedure TFreeverb.SetDry(const Value: Double);
begin
 if FDry <> Value then
  begin
   FDry := Value;
   DryChanged;
  end;
end;

procedure TFreeverb.DryChanged;
begin
 Changed;
end;

initialization
  RegisterDspProcessor64(TFreeverb);

end.
