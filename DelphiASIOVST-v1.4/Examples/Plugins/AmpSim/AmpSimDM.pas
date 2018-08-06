unit AmpSimDM;

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

{$I DAV_Compiler.inc}
{$DEFINE UseGUI}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics;

type
  TModelType = (mtDI, mtSpeakerSim, mtRadio, mtMesaBoogie1, mtMesaBoogie8,
    mtMarshall4x12, mtScoopedOutMetal);

  TComboDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamBiasChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPFFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHPFResonanceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamModelDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParamNoiseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputChanged(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamProcessChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamProcessDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure StringToParameterModel(Sender: TObject; const Index: Integer; const ParameterString: AnsiString; var Value: Single);
  private
    FBufferSize     : Integer;
    FBufferPosition : Integer;
    FBuffer         : array [0..1] of PDAVSingleFixedArray;
    FMix            : array [0..1] of Double;
    FDelay          : array [0..1] of Integer;
    FHighPass       : array [0..1] of TBasicHighpassFilter;
    FFilterState    : array [0..1, 0..4] of Double;
    FRndAmt         : Double;
    FLPF, FHPF      : Double;
    FTrim           : Double;
    FDrive, FClip   : Double;
    FBias           : Double;
    FStereo         : Boolean;
    FIsSoftClipping : Boolean;
    FModelType      : TModelType;
    function FilterFreq(Frequency: Double): Double;
    function GetModelType: TModelType;
    procedure SetModelType(const Value: TModelType);
  protected
    procedure DriveChanged(const Value: Single);
    procedure BiasChanged;
    procedure ModelTypeChanged;
    procedure TrimChanged;
  public
    property ModelType: TModelType read GetModelType write SetModelType;
    property Stereo: Boolean read FStereo;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  Math, Controls, {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} 
  DAV_VSTEffect, DAV_Common, AmpSimGUI;

procedure TComboDataModule.VSTModuleOpen(Sender: TObject);
begin
 FBufferSize := 1024;
 FBufferPosition := 0;
 GetMem(FBuffer[0], FBufferSize * SizeOf(Single));
 GetMem(FBuffer[1], FBufferSize * SizeOf(Single));
 FHighPass[0] := TBasicHighpassFilter.Create;
 FHighPass[1] := TBasicHighpassFilter.Create;

 {$IFDEF UseGUI}
 Flags := Flags + [effFlagsHasEditor];
 EditorFormClass := TFmCombo;
 {$ENDIF}

 // parameter initialization
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 0;
 Parameter[5] := 100;
 Parameter[6] := 0;
 Parameter[7] := -75;

 // default preset
 with Programs[0] do
  begin
   Parameter[0] := 0;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 0;
   Parameter[4] := 0;
   Parameter[5] := 100;
   Parameter[6] := 0;
   Parameter[7] := -75;
  end;

 // preset 1
 with Programs[1] do
  begin
   Parameter[0] := 3;
   Parameter[1] := 43.5;
   Parameter[2] := 41;
   Parameter[3] := 3.9;
   Parameter[4] := 0;
   Parameter[5] := 565;
   Parameter[6] := 9;
   Parameter[7] := -75;
  end;
end;

procedure TComboDataModule.VSTModuleClose(Sender: TObject);
begin
 Dispose(FBuffer[0]);
 Dispose(FBuffer[1]);
 FreeAndNil(FHighPass[0]);
 FreeAndNil(FHighPass[1]);
end;

procedure TComboDataModule.ParamProcessChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FStereo := Value > 0.5;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateProcess;
 {$ENDIF}
end;

procedure TComboDataModule.ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 TrimChanged;
 DriveChanged(Value);
 BiasChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateDrive;
 {$ENDIF}
end;

procedure TComboDataModule.ParamHPFResonanceChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FHighPass[0]) then
  begin
   FHighPass[0].Bandwidth := 1.1 - 0.01 * Value;
   FHighPass[1].Bandwidth := FHighPass[0].Bandwidth;
  end;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateReso;
 {$ENDIF}
end;

procedure TComboDataModule.ParamBiasChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BiasChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateBias;
 {$ENDIF}
end;

function TComboDataModule.FilterFreq(Frequency: Double): Double;
var
  j, k, r : Double;
begin
 r := 0.999;
 j := sqr(r) - 1;
 k := 2 - 2 * sqr(r) * cos(0.647 * Frequency / SampleRate);
 Result := (sqrt(sqr(k) - 4 * sqr(j)) - k) / (2 * j);
end;

function TComboDataModule.GetModelType: TModelType;
begin
 Result := TModelType(Round(ParameterByName['Model']));
end;

procedure TComboDataModule.SetModelType(const Value: TModelType);
begin
 if Value <> FModelType then
  begin
   FModelType := Value;
   ModelTypeChanged;
  end;
end;

procedure TComboDataModule.ModelTypeChanged;
begin
 case FModelType of
  mtDI:
    begin
     FLPF      := 0;
     FMix[0]   := 0;
     FMix[1]   := 0;
     FDelay[0] := 0;
     FDelay[1] := 0;
     FHPF      := FilterFreq(25);
    end;

  mtSpeakerSim:
    begin
     FLPF      := FilterFreq(2700);
     FMix[0]   := 0;
     FMix[1]   := 0;
     FDelay[0] := 0;
     FDelay[1] := 0;
     FHPF      := FilterFreq(382);
    end;

  mtRadio:
    begin
     FLPF      := FilterFreq(1685);
     FMix[0]   := -1.7;
     FMix[1]   := 0.82;
     FDelay[0] := Round(SampleRate * 1.5276504735716468072105102352582E-4);
     FDelay[1] := Round(SampleRate * 2.3174971031286210892236384704519E-4);
     FHPF      := FilterFreq(25);
    end;

  mtMesaBoogie1:
    begin
     FLPF      := FilterFreq(1385);
     FMix[0]   := -0.53;
     FMix[1]   := 0.21;
     FDelay[0] := Round(SampleRate * 1.361470388019060585432266848196E-4);
     FDelay[1] := Round(SampleRate * 8.382229673093042749371332774518E-4);
     FHPF      := FilterFreq(25);
    end;

  mtMesaBoogie8:
    begin
     FLPF      := FilterFreq(1685);
     FMix[0]   := -0.85;
     FMix[1]   := 0.41;
     FDelay[0] := Round(SampleRate * 1.5276504735716468072105102352582E-4);
     FDelay[1] := Round(SampleRate * 3.0165912518853695324283559577677E-4);
     FHPF      := FilterFreq(25);
    end;

  mtMarshall4x12:
    begin
     FLPF      := FilterFreq(2795);
     FMix[0]   := -0.29;
     FMix[1]   := 0.38;
     FDelay[0] := Round(SampleRate * 1.0183299389002036659877800407332E-3);
     FDelay[1] := Round(SampleRate * 4.1631973355537052456286427976686E-4);
     FHPF      := FilterFreq(459);
    end;

  mtScoopedOutMetal:
    begin
     FLPF      := FilterFreq(1744);
     FMix[0]   := -0.96;
     FMix[1]   := 1.6;
     FDelay[0] := Round(SampleRate * 2.8089887640449438202247191011236E-3);
     FDelay[1] := Round(SampleRate * 7.9176563737133808392715756136184E-4);
     FHPF      := FilterFreq(382);
    end;
 end;
end;

procedure TComboDataModule.ParamModelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 SetModelType(TModelType(Round(Value)));
 TrimChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateModel;
 {$ENDIF}
end;

procedure TComboDataModule.ParamHPFFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FHighPass[0]) then
  begin
   FHighPass[0].Frequency := Value;
   FHighPass[1].Frequency := FHighPass[0].Frequency;
  end;
 DriveChanged(Parameter[1]);
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateFreq;
 {$ENDIF}
end;

procedure TComboDataModule.ParamNoiseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FRndAmt := dB_to_Amp(Value);
end;

procedure TComboDataModule.StringToParameterModel(
  Sender: TObject; const Index: Integer; const ParameterString: AnsiString;
  var Value: Single);
var
  Text : string;
begin
 Text := string(Trim(ParameterString));
 if Text = 'D.I.' then Value := 0 else
 if Text = 'Spkr Sim' then Value := 1 else
 if Text = 'Radio' then Value := 2 else
 if Text = 'MB 1"' then Value := 3 else
 if Text = 'MB 8"' then Value := 4 else
 if Text = '4x12 ^' then Value := 5 else
 if Text = '4x12 >' then Value := 6;
end;

procedure TComboDataModule.DriveChanged(const Value: Single);
begin
 FIsSoftClipping := Value < 0;

 if FIsSoftClipping
  then FDrive := Power(10, -(1 + 0.03 * Value))  // soft clipping
  else                                                  // hard clipping
   begin
    FDrive := 1;
    FClip  := 3.7 - 0.08 * Value;
    if Value > 40 then
     begin
      FDrive := Power(10, 0.035 * Value - 1.4);
      FClip  := 0.5;
     end;
   end;

 if (Parameter[5] > 100)
  then FDrive := FDrive * (1 + 0.1 * FDrive);
end;

procedure TComboDataModule.BiasChanged;
begin
 FBias := 6 * Parameter[2] / (1000 + abs(1.5 * Parameter[1]));
end;

procedure TComboDataModule.TrimChanged;
begin
 case Round(Parameter[0]) of
  0: FTrim := 0.50;   // DI
  1: FTrim := 0.53;   // speaker sim
  2: FTrim := 1.10;   // radio
  3: FTrim := 0.98;   // mesa boogie 1"
  4: FTrim := 0.96;   // mesa boogie 8"
  5: FTrim := 0.59;   // Marshall 4x12" celestion
  6: FTrim := 0.30;   // scooped-out metal
 end;

 if FIsSoftClipping
  then FTrim := FTrim * (0.55 + 150 * IntPower((0.5 + 0.005 * Parameter[1]), 4));

 FTrim := FTrim * Power(10, Parameter[3] * 0.05);
 if FStereo then FTrim := FTrim * 2;
end;

procedure TComboDataModule.ParamModelDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'D.I.';
  1 : PreDefined := 'Spkr Sim';
  2 : PreDefined := 'Radio';
  3 : PreDefined := 'MB 1"';
  4 : PreDefined := 'MB 8"';
  5 : PreDefined := '4x12 ^';
  6 : PreDefined := '4x12 >';
 end;
end;

procedure TComboDataModule.ParamOutputChanged(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 TrimChanged;
 {$IFDEF UseGUI}
 if Assigned(EditorForm) then
  with TFmCombo(EditorForm)
   do UpdateOutput;
 {$ENDIF}
end;

procedure TComboDataModule.ParamProcessDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'STEREO'
  else PreDefined := 'MONO';
end;

procedure TComboDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  InP, OutP    : array [0..1] of Double;
  trm, clp     : Single;
  LPF, bi      : Single;
  HPF, drv     : Single;
  FilterState  : array [0..1, 0..4] of Double;
  d            : array [0..1] of Integer;
  m            : array [0..1] of Single;
  bp, Sample   : Integer;
begin
 m[0] := FMix[0];
 m[1] := FMix[1];
 clp  := FClip;
 LPF  := FLPF;
 HPF  := FHPF;
 bi   := FBias - 0.0001;
 drv  := FDrive;
 FilterState[0, 0] := FFilterState[0, 0];
 FilterState[0, 1] := FFilterState[0, 1];
 FilterState[0, 2] := FFilterState[0, 2];
 FilterState[0, 3] := FFilterState[0, 3];
 FilterState[0, 4] := FFilterState[0, 4];
 FilterState[1, 0] := FFilterState[1, 0];
 FilterState[1, 1] := FFilterState[1, 1];
 FilterState[1, 2] := FFilterState[1, 2];
 FilterState[1, 3] := FFilterState[1, 3];
 FilterState[1, 4] := FFilterState[1, 4];
 d[0] := FDelay[0];
 d[1] := FDelay[1];
 bp   := FBufferPosition;
 trm  := FTrim * sqr(sqr(1 - LPF));

 if FStereo then //stereo
  begin
   for  Sample := 0 to SampleFrames - 1 do
    begin
     InP[0] := FHighPass[0].ProcessSample64(drv * (FRndAmt * random + Inputs[0, Sample] + bi));
     InP[1] := FHighPass[1].ProcessSample64(drv * (FRndAmt * random + Inputs[1, Sample] + bi));

     if FIsSoftClipping then
      begin
       OutP[0] := InP[0] / (1 + abs(InP[0]));
       OutP[1] := InP[1] / (1 + abs(InP[1]));
      end
     else
      begin
       if InP[0] > clp        then OutP[0] := clp
        else if InP[0] < -clp then OutP[0] := -clp
        else OutP[0] := InP[0];
       if InP[1] > clp        then OutP[1] := clp
        else if InP[1] < -clp then OutP[1] := -clp
        else OutP[1] := InP[1];
      end;

     FBuffer[0]^[bp] := OutP[0];
     FBuffer[1]^[bp] := OutP[1];
     OutP[0] := OutP[0] + (m[0] * FBuffer[0]^[(bp + d[0]) mod 1000]) +
                          (m[1] * FBuffer[0]^[(bp + d[1]) mod 1000]);
     OutP[1] := OutP[1] + (m[0] * FBuffer[1]^[(bp + d[0]) mod 1000]) +
                          (m[1] * FBuffer[1]^[(bp + d[1]) mod 1000]);

     FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
     FilterState[1, 0] := LPF * FilterState[1, 0] + trm * OutP[1];
     FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
     FilterState[1, 1] := LPF * FilterState[1, 1] + FilterState[1, 0];
     FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
     FilterState[1, 2] := LPF * FilterState[1, 2] + FilterState[1, 1];
     FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2];
     FilterState[1, 3] := LPF * FilterState[1, 3] + FilterState[1, 2];  //-24dB/oct filter

     FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3];
     FilterState[1, 4] := HPF * (FilterState[1, 4] - FilterState[1, 3]) + FilterState[1, 3];  //high pass

     OutP[0] := FilterState[0, 3] - FilterState[0, 4];
     OutP[1] := FilterState[1, 3] - FilterState[1, 4];

     if (bp = 0)
      then bp := 999
      else bp := bp - 1;

     Outputs[0, Sample] := Outp[0];
     Outputs[1, Sample] := Outp[1];
    end;
  end
 else //mono
  begin
   if FIsSoftClipping then //soft clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := FHighPass[0].ProcessSample64(drv * (FRndAmt * random + Inputs[0, Sample] + Inputs[1, Sample] + bi));

       OutP[0] := InP[0] / (1 + abs(InP[0]));

       FBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * FBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * FBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[0];
      end;
    end
   else //hard clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := FHighPass[0].ProcessSample64(drv * (FRndAmt * random + Inputs[0, Sample] + Inputs[1, Sample] + bi));

       if InP[0] > clp        then OutP[0] :=  clp
        else if InP[0] < -clp then OutP[0] := -clp
        else OutP[0] := InP[0];

       FBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * FBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * FBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass //also want smile curve here...
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[0];
      end;
    end;
  end;
 FBufferPosition := bp;
 if (abs(FilterState[0, 0]) < 1E-10)
  then FillChar(FFilterState[0, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[0, 0], FFilterState[0, 0], 5 * SizeOf(Double));
 if (abs(FilterState[1, 0]) < 1E-10) or (not FStereo)
  then FillChar(FFilterState[1, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[1, 0], FFilterState[1, 0], 5 * SizeOf(Double));
end;

procedure TComboDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  InP, OutP    : array [0..1] of Double;
  trm, clp     : Double;
  LPF, bi      : Double;
  HPF, drv     : Double;
  FilterState  : array [0..1, 0..4] of Double;
  d            : array [0..1] of Integer;
  m            : array [0..1] of Single;
  bp, Sample   : Integer;
begin
 m[0] := FMix[0];
 m[1] := FMix[1];
 clp  := FClip;
 LPF  := FLPF;
 HPF  := FHPF;
 bi   := FBias;
 drv  := FDrive;
 FilterState[0, 0] := FFilterState[0, 0];
 FilterState[0, 1] := FFilterState[0, 1];
 FilterState[0, 2] := FFilterState[0, 2];
 FilterState[0, 3] := FFilterState[0, 3];
 FilterState[0, 4] := FFilterState[0, 4];
 FilterState[1, 0] := FFilterState[1, 0];
 FilterState[1, 1] := FFilterState[1, 1];
 FilterState[1, 2] := FFilterState[1, 2];
 FilterState[1, 3] := FFilterState[1, 3];
 FilterState[1, 4] := FFilterState[1, 4];

 d[0] := FDelay[0];
 d[1] := FDelay[1];
 bp   := FBufferPosition;
 trm  := FTrim * sqr(sqr(1 - LPF));

 if FStereo then //stereo
  begin
   for  Sample := 0 to SampleFrames - 1 do
    begin
     InP[0] := drv * (Inputs[0, Sample] + bi);
     InP[1] := drv * (Inputs[1, Sample] + bi);

      if FIsSoftClipping then
       begin
        OutP[0] := InP[0] / (1 + abs(InP[0]));
        OutP[1] := InP[1] / (1 + abs(InP[1]));
       end
      else
       begin
        if InP[0] > clp        then OutP[0] := clp
         else if InP[0] < -clp then OutP[0] := -clp
         else OutP[0] := InP[0];
        if InP[1] > clp        then OutP[1] := clp
         else if InP[1] < -clp then OutP[1] := -clp
         else OutP[1] := InP[1];
       end;

      FBuffer[0]^[bp] := OutP[0];
      FBuffer[1]^[bp] := OutP[1];
      OutP[0] := OutP[0] + (m[0] * FBuffer[0]^[(bp + d[0]) mod 1000]) +
                           (m[1] * FBuffer[0]^[(bp + d[1]) mod 1000]);
      OutP[1] := OutP[1] + (m[0] * FBuffer[1]^[(bp + d[0]) mod 1000]) +
                           (m[1] * FBuffer[1]^[(bp + d[1]) mod 1000]);

      FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
      FilterState[1, 0] := LPF * FilterState[1, 0] + trm * OutP[1];
      FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
      FilterState[1, 1] := LPF * FilterState[1, 1] + FilterState[1, 0];
      FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
      FilterState[1, 2] := LPF * FilterState[1, 2] + FilterState[1, 1];
      FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2];
      FilterState[1, 3] := LPF * FilterState[1, 3] + FilterState[1, 2];  //-24dB/oct filter

      FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3];
      FilterState[1, 4] := HPF * (FilterState[1, 4] - FilterState[1, 3]) + FilterState[1, 3];  //high pass

      OutP[0] := FilterState[0, 3] - FilterState[0, 4];
      OutP[1] := FilterState[1, 3] - FilterState[0, 4];

      if (bp = 0)
       then bp := 999
       else bp := bp - 1;

      Outputs[0, Sample] := Outp[0];
      Outputs[1, Sample] := Outp[1];
    end;
  end
 else //mono
  begin
   if FIsSoftClipping then //soft clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := FHighPass[0].ProcessSample64(drv * (Inputs[0, Sample] + Inputs[1, Sample] + bi));

       OutP[0] := InP[0] / (1 + abs(InP[0]));

       FBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * FBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * FBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[1];
      end;
    end
   else //hard clip
    begin
     for Sample := 0 to SampleFrames - 1 do
      begin
       InP[0] := FHighPass[0].ProcessSample64(drv * (Inputs[0, Sample] + Inputs[1, Sample] + bi));

       if InP[0] > clp        then OutP[0] :=  clp
        else if InP[0] < -clp then OutP[0] := -clp
        else OutP[0] := InP[0];

       FBuffer[0]^[bp] := OutP[0];
       OutP[0] := OutP[0] + (m[0] * FBuffer[0]^[(bp + d[0]) mod 1000]) +
                            (m[1] * FBuffer[0]^[(bp + d[1]) mod 1000]);

       FilterState[0, 0] := LPF * FilterState[0, 0] + trm * OutP[0];
       FilterState[0, 1] := LPF * FilterState[0, 1] + FilterState[0, 0];
       FilterState[0, 2] := LPF * FilterState[0, 2] + FilterState[0, 1];
       FilterState[0, 3] := LPF * FilterState[0, 3] + FilterState[0, 2]; //-24dB/oct filter

       FilterState[0, 4] := HPF * (FilterState[0, 4] - FilterState[0, 3]) + FilterState[0, 3]; //high pass //also want smile curve here...
       OutP[0] := FilterState[0, 3] - FilterState[0, 4];

       if (bp = 0)
        then bp := 999
        else bp := bp - 1; //buffer position

       Outputs[0, Sample] := Outp[0];
       Outputs[1, Sample] := Outp[1];
      end;
    end;
  end;
 FBufferPosition := bp;
 if (abs(FilterState[0, 0]) < 1E-10)
  then FillChar(FFilterState[0, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[0, 0], FFilterState[0, 0], 5 * SizeOf(Double));
 if (abs(FilterState[1, 0]) < 1E-10) or (not FStereo)
  then FillChar(FFilterState[1, 0], 5 * SizeOf(Double), 0)
  else Move(FilterState[1, 0], FFilterState[1, 0], 5 * SizeOf(Double));
end;

procedure TComboDataModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 if abs(SampleRate) > 0 then
  begin
   if Assigned(FHighPass[0]) then FHighPass[0].SampleRate := abs(SampleRate);
   if Assigned(FHighPass[1]) then FHighPass[1].SampleRate := abs(SampleRate);
   ModelTypeChanged;
  end;
end;

procedure TComboDataModule.VSTModuleSuspend(Sender: TObject);
begin
 if Assigned(FBuffer[0])
  then FillChar(FBuffer[0]^[0], FBufferSize * SizeOf(Single), 0);
 if Assigned(FBuffer[1])
  then FillChar(FBuffer[1]^[0], FBufferSize * SizeOf(Single), 0);
 FillChar(FFilterState[0, 0], 10 * SizeOf(Double), 0);
end;

end.
