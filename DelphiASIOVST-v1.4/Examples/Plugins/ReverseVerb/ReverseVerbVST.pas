unit ReverseVerbVST;

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
{$DEFINE DirectUpdate}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspConvolution,
  DAV_DspFilterSimple, DAV_DspDelayLines, DAV_DspLightweightDynamics;

const
  CNumChannels = 2;
  CImpulsResponseOrder = 16;

type
  {$IFNDEF DirectUpdate}
  TImpulseResponseUpdateThread = class;
  {$ENDIF}

  TReverseVerbDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleBeforeProgramChange(Sender: TObject);
    procedure VSTModuleAfterProgramChange(Sender: TObject);
    procedure VSTModuleProcessForward(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessBackward(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleBlockSizeChange(Sender: TObject; const BlockSize: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterDirectChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampingChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterT60Change(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterTailChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterERGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampingDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterDampingLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterDirectionDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterDirectionChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterERTimeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FConvolution      : array [0..CNumChannels - 1] of TLowLatencyConvolution32;
    FImpulseResponse  : array [0..CNumChannels - 1] of PDAVSingleFixedArray;
    FBackwardIR       : array [0..CNumChannels - 1] of PDAVSingleFixedArray;
    FDelay            : array [0..CNumChannels - 1, 0..1] of TDelayLineSamples32;
    FCompressor       : array [0..CNumChannels - 1] of TLightweightSoftKneeCompressor;
    FTempBuffer       : PDAVSingleFixedArray;
    FTempBufferSize   : Integer;
    FIRLength         : Integer;
    FIsBackward       : Boolean;
    FEarlyReflections : Single;
    FERTime           : Single;
    FTail             : Single;
    FT60              : Single;
    FT60Factor        : Single;
    FDamping          : Single;
    FOutputGain       : Single;
    FDampingFilter    : TFirstOrderHighcutFilter;
    FCriticalSection  : TCriticalSection;
    {$IFNDEF DirectUpdate}
    FIRUpdateThread   : TImpulseResponseUpdateThread;
    FIRChanged        : Boolean;
    {$ENDIF}
    FUpdateCount      : Integer;
    procedure SetT60(const Value: Single);
    procedure CalculateT60FadeFactor;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateImpulseResponse;
    procedure LoadUpdatedIR;
    procedure ImpulseResponseChanged;
    procedure T60Changed;

    property T60: Single read FT60 write SetT60;
  end;

  {$IFNDEF DirectUpdate}
  TImpulseResponseUpdateThread = class(TThread)
  private
    FReverseVerbDataModule : TReverseVerbDataModule;
  protected
    constructor Create(ReverseVerbDataModule: TReverseVerbDataModule); virtual;
    procedure Execute; override;
  end;
  {$ENDIF}

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} DAV_Common,
  DAV_Math, DAV_HalfFloat, DAV_Approximations;
//  ReverseVerbGUI;

{ TImpulseResponseUpdateThread }

{$IFNDEF DirectUpdate}
constructor TImpulseResponseUpdateThread.Create(
  ReverseVerbDataModule: TReverseVerbDataModule);
begin
 FReverseVerbDataModule := ReverseVerbDataModule;
 inherited Create(True);
end;

procedure TImpulseResponseUpdateThread.Execute;
begin
 while not Terminated do
  with FReverseVerbDataModule do
   begin
    if FIRChanged = True then
     begin
      FIRChanged := False;
      UpdateImpulseResponse;
      LoadUpdatedIR;
     end;

    Suspend;
   end;
end;
{$ENDIF}


{ TReverseVerbDataModule }

procedure TReverseVerbDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 FImpulseResponse[0] := nil;
 FImpulseResponse[1] := nil;
 FBackwardIR[0] := nil;
 FBackwardIR[1] := nil;
 FTempBuffer := nil;
end;

procedure TReverseVerbDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TReverseVerbDataModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex: Integer;
begin
 FEarlyReflections := 1;
 FERTime := 0.1;
 FTail := 1;
 FT60 := 0.3;
 FIsBackward := True;
 FIRLength := 1 shl CImpulsResponseOrder;
 CalculateT60FadeFactor;

 for ChannelIndex := 0 to Length(FConvolution) - 1 do
  begin
   FConvolution[ChannelIndex] := TLowLatencyConvolution32.Create;
   FConvolution[ChannelIndex].MinimumIRBlockOrder := 11;
   FConvolution[ChannelIndex].MaximumIRBlockOrder := 18;
  end;
 for ChannelIndex := 0 to Length(FDelay) - 1 do
  begin
   FDelay[ChannelIndex, 0] := TDelayLineSamples32.Create(FIRLength - 1);
   FDelay[ChannelIndex, 1] := TDelayLineSamples32.Create(FConvolution[ChannelIndex].Latency);
  end;
 for ChannelIndex := 0 to Length(FCompressor) - 1 do
  begin
   FCompressor[ChannelIndex] := TLightweightSoftKneeCompressor.Create;
   FCompressor[ChannelIndex].SampleRate := SampleRate;
   FCompressor[ChannelIndex].Ratio := 8;
   FCompressor[ChannelIndex].Knee_dB := 1;
  end;

 GetMem(FImpulseResponse[0], FIRLength * SizeOf(Single));
 GetMem(FImpulseResponse[1], FIRLength * SizeOf(Single));
 GetMem(FBackwardIR[0], FIRLength * SizeOf(Single));
 GetMem(FBackwardIR[1], FIRLength * SizeOf(Single));
 FillChar(FImpulseResponse[0]^, FIRLength * SizeOf(Single), 0);
 FillChar(FImpulseResponse[1]^, FIRLength * SizeOf(Single), 0);
 FillChar(FBackwardIR[0]^, FIRLength * SizeOf(Single), 0);
 FillChar(FBackwardIR[1]^, FIRLength * SizeOf(Single), 0);

 FTempBufferSize := BlockSize;
 GetMem(FTempBuffer, FTempBufferSize * SizeOf(Single));
 FillChar(FTempBuffer^, FTempBufferSize * SizeOf(Single), 0);

 // create damping filter
 FDampingFilter := TFirstOrderHighcutFilter.Create;
 {$IFNDEF DirectUpdate}
 FIRUpdateThread := TImpulseResponseUpdateThread.Create(Self);
 {$ENDIF}

 BeginUpdate;
 Parameter[0] := -3;
 Parameter[1] := -10;
 Parameter[2] := 50;
 Parameter[3] := -16;
 Parameter[4] := 250 ;
 Parameter[5] := 4000;
 Parameter[6] := 0;
 Parameter[7] := 0;
 Parameter[8] := 1;
 Parameter[9] := -8;

 with Programs[0] do
  begin
   Parameter[0] := -3;
   Parameter[1] := -10;
   Parameter[2] := 50;
   Parameter[3] := -16;
   Parameter[4] := 250 ;
   Parameter[5] := 4000;
   Parameter[6] := 0;
   Parameter[7] := 0;
   Parameter[8] := 1;
   Parameter[9] := -4;
  end;

 with Programs[1] do
  begin
   Parameter[0] := -6;
   Parameter[1] := -9;
   Parameter[2] := 1;
   Parameter[3] := -12;
   Parameter[4] := 330;
   Parameter[5] := 5000;
   Parameter[6] := 1;
   Parameter[7] := 0;
   Parameter[8] := 1;
   Parameter[9] := -12.7;
  end;
 with Programs[2] do
  begin
   Parameter[0] := -5;
   Parameter[1] := -5;
   Parameter[2] := 300;
   Parameter[3] := -20;
   Parameter[4] := 630;
   Parameter[5] := 12000;
   Parameter[6] := 1;
   Parameter[7] := 0;
   Parameter[8] := 1;
   Parameter[9] := -2;
  end;
 with Programs[3] do
  begin
   Parameter[0] := -4;
   Parameter[1] := -8;
   Parameter[2] := 150;
   Parameter[3] := -9;
   Parameter[4] := 400;
   Parameter[5] := 6300;
   Parameter[6] := 1;
   Parameter[7] := 0;
   Parameter[8] := 1;
   Parameter[9] := -4.5;
  end;
 with Programs[4] do
  begin
   Parameter[0] := -90;
   Parameter[1] := -7;
   Parameter[2] := 100;
   Parameter[3] := -9;
   Parameter[4] := 700;
   Parameter[5] := 9000;
   Parameter[6] := 1;
   Parameter[7] := 0;
   Parameter[8] := 1;
   Parameter[9] := -11.5;
  end;
 with Programs[5] do
  begin
   Parameter[0] := 5;
   Parameter[1] := -12;
   Parameter[2] := 10;
   Parameter[3] := -16;
   Parameter[4] := 100;
   Parameter[5] := 7000;
   Parameter[6] := 1;
   Parameter[7] := -9;
   Parameter[8] := 8;
   Parameter[9] := -6;
  end;
 with Programs[6] do
  begin
   Parameter[0] := 0;
   Parameter[1] := -90;
   Parameter[2] := 3;
   Parameter[3] := -90;
   Parameter[4] := 20;
   Parameter[5] := 20000;
   Parameter[6] := 0;
   Parameter[7] := 0;
   Parameter[8] := 1;
   Parameter[9] := 0;
  end;
 EndUpdate;

 // set editor class
// EditorFormClass := TFmReverseVerb;
end;

procedure TReverseVerbDataModule.VSTModuleAfterProgramChange(Sender: TObject);
begin
 EndUpdate;
end;

procedure TReverseVerbDataModule.VSTModuleBeforeProgramChange(Sender: TObject);
begin
 BeginUpdate;
end;

procedure TReverseVerbDataModule.VSTModuleBlockSizeChange(Sender: TObject;
  const BlockSize: Integer);
begin
 if FTempBufferSize <> BlockSize then
  begin
   FTempBufferSize := BlockSize;
   ReallocMem(FTempBuffer, FTempBufferSize * SizeOf(Single));
   FillChar(FTempBuffer^, FTempBufferSize * SizeOf(Single), 0);
  end;
end;

procedure TReverseVerbDataModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex: Integer;
begin
 {$IFNDEF DirectUpdate}
 with FIRUpdateThread do
  begin
   Terminate;
   Resume;
   WaitFor;
  end;
 FreeAndNil(FIRUpdateThread);
 {$ENDIF}

 if Assigned(FImpulseResponse[0]) then Dispose(FImpulseResponse[0]);
 if Assigned(FImpulseResponse[1]) then Dispose(FImpulseResponse[1]);
 if Assigned(FBackwardIR[0]) then Dispose(FBackwardIR[0]);
 if Assigned(FBackwardIR[1]) then Dispose(FBackwardIR[1]);
 if Assigned(FTempBuffer) then Dispose(FTempBuffer);

 for ChannelIndex := 0 to Length(FConvolution) - 1
  do FreeAndNil(FConvolution[ChannelIndex]);
 for ChannelIndex := 0 to Length(FDelay) - 1 do
  begin
   FreeAndNil(FDelay[ChannelIndex, 0]);
   FreeAndNil(FDelay[ChannelIndex, 1]);
  end;
 for ChannelIndex := 0 to Length(FCompressor) - 1
  do FreeAndNil(FCompressor[ChannelIndex]);
 FreeAndNil(FDampingFilter);
end;

procedure TReverseVerbDataModule.ParameterDampingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDamping := Value;
 ImpulseResponseChanged;

(*
 // update GUI
 if EditorForm is TFmReverseVerb
  then TFmReverseVerb(EditorForm).UpdateDamping;
*)
end;

procedure TReverseVerbDataModule.UpdateImpulseResponse;
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  Offset       : Integer;
  NextSample   : Integer;
  Current      : Double;
  Weight       : Double;
  TailScale    : Single;
  Scale        : Single;
  T60Factor    : Single;
  ExpGain      : Single;
  ERGain       : Single;
  TailGain     : Single;
  TailStart    : Integer;
  IR, BackIR   : PDAVSingleFixedArray;
const
  CRandomSeed : array [0..7] of Cardinal = ($DEADBEEF, $C0DEAFFE, $31607037,
    $CAFEBABE, $BADCAB1E, $C001D00D, $C0CAC01A, $DD511FE);
begin
 // initialize algorithm
 if FIRLength = 0 then Exit;
 Scale := 1 / FIRLength;
 TailStart := Limit(Round(FERTime * SampleRate), 1, FIRLength - 1);
 TailScale := 2 / TailStart;
 TailGain := FTail;
 ERGain := FEarlyReflections;
 T60Factor := FT60Factor;
 if not Assigned(FDampingFilter) then Exit;
 FDampingFilter.SampleRate := Abs(SampleRate);
 FDampingFilter.Frequency := FDamping;

 for ChannelIndex := 0 to Length(FImpulseResponse) - 1 do
  begin
   // reset damping filter
   FDampingFilter.ResetStates;
   RandSeed := CRandomSeed[ChannelIndex mod 8];
   IR := @FImpulseResponse[ChannelIndex]^[0];
   BackIR := @FBackwardIR[ChannelIndex]^[0];
   ExpGain := T60Factor;
   IR[0] := 0;
   Current := 0;

   // select first ER sample with pre delay
   NextSample := Random(100 + TailStart div 3) div 3;

   // calculate first part of IR (without any tail influence)
   for SampleIndex := 1 to TailStart div 2 - 1 do
    begin
     Assert(SampleIndex < FIRLength);

     // calculate early reflections
     if SampleIndex = NextSample then
      begin
       Current := ERGain * ExpGain * (Random - Random);
       Offset := Random((TailStart - SampleIndex) div 3) div 3;
       NextSample := SampleIndex + 1 + Offset;
      end else IR[SampleIndex] := 0;

     // calculate damping
     Weight := 0.1 * Sqr(Sqr((FIRLength - SampleIndex) * Scale));
     Assert(Weight <= 1);
     IR[SampleIndex] := Weight * Current  +
       (1 - Weight) * FDampingFilter.ProcessSample64(Current);
     BackIR^[FIRLength - 1 - SampleIndex] := IR[SampleIndex];

     // calculate exponential decay
     ExpGain := ExpGain * T60Factor;
    end;

   // calculate second part of IR (with starting tail)
   for SampleIndex := TailStart div 2 to TailStart - 1 do
    begin
     Assert(SampleIndex < FIRLength);

     // calculate tail
     Weight := Sqr((SampleIndex - TailStart div 2) * TailScale);
     Current := 0.5 * (Weight + Sqr(Sqr(Weight))) * TailGain * ExpGain * (Random - Random);

     // calculate early reflections
     if SampleIndex = NextSample then
      begin
       Current := Current + (1 - Sqr(Weight)) * ERGain * ExpGain * (Random - Random);
       Offset := Random((TailStart - SampleIndex) div 3) div 3;
       NextSample := SampleIndex + 1 + Offset;
      end;

     // calculate damping
     Weight := 0.1 * Sqr(Sqr((FIRLength - SampleIndex) * Scale));
     Assert(Weight <= 1);
     IR[SampleIndex] := Weight * Current  +
       (1 - Weight) * FDampingFilter.ProcessSample64(Current);
     BackIR^[FIRLength - 1 - SampleIndex] := IR[SampleIndex];

     // calculate exponential decay
     ExpGain := ExpGain * T60Factor;
    end;

   for SampleIndex := TailStart to FIRLength - 1 do
    begin
     Assert(SampleIndex < FIRLength);

     // calculate tail
     Current := TailGain * ExpGain * (Random - Random);
     Weight := 0.1 * Sqr(Sqr((FIRLength - SampleIndex) * Scale));
     Assert(Weight <= 1);
     IR[SampleIndex] := CDenorm32 + Weight * Current  +
       (1 - Weight) * FDampingFilter.ProcessSample64(Current);
     BackIR^[FIRLength - 1 - SampleIndex] := IR[SampleIndex];

     // calculate exponential decay
     ExpGain := ExpGain * T60Factor;
    end;
  end;
end;

procedure TReverseVerbDataModule.LoadUpdatedIR;
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FConvolution) - 1 do
   if Assigned(FConvolution[ChannelIndex]) then
    if FIsBackward
     then FConvolution[ChannelIndex].LoadImpulseResponse(FBackwardIR[ChannelIndex], FIRLength)
     else FConvolution[ChannelIndex].LoadImpulseResponse(FImpulseResponse[ChannelIndex], FIRLength);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TReverseVerbDataModule.SetT60(const Value: Single);
begin
 if FT60 <> Value then
  begin
   FT60 := Value;
   T60Changed;
  end;
end;

procedure TReverseVerbDataModule.T60Changed;
begin
 CalculateT60FadeFactor;
 ImpulseResponseChanged;
end;

procedure TReverseVerbDataModule.CalculateT60FadeFactor;
var
  Temp : Single;
begin
 Temp := Abs(SampleRate * FT60);
 if Temp <> 0
  then Temp := -3 / (SampleRate * FT60)
  else Temp := -20;
 FT60Factor := FastPower10ContinousError3(Temp);
end;

procedure TReverseVerbDataModule.ParameterDirectChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressor) - 1 do
   if Assigned(FCompressor[ChannelIndex])
    then FCompressor[ChannelIndex].MakeUpGain_dB := Value;
 finally
  FCriticalSection.Leave;
 end;

(*
 // update GUI
 if EditorForm is TFmReverseVerb
  then TFmReverseVerb(EditorForm).UpdateGain;
*)
end;

procedure TReverseVerbDataModule.ParameterERGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FEarlyReflections := dB_to_Amp(Value);
 ImpulseResponseChanged;

(*
 // update GUI
 if EditorForm is TFmReverseVerb
  then TFmReverseVerb(EditorForm).UpdateGain;
*)
end;

procedure TReverseVerbDataModule.ParameterTailChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FTail := dB_to_Amp(Value);
 ImpulseResponseChanged;

(*
 // update GUI
 if EditorForm is TFmReverseVerb
  then TFmReverseVerb(EditorForm).UpdateGain;
*)
end;

procedure TReverseVerbDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressor) - 1 do
   if Assigned(FCompressor[ChannelIndex])
    then FCompressor[ChannelIndex].Threshold_dB := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TReverseVerbDataModule.ParameterCompressorRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressor) - 1 do
   if Assigned(FCompressor[ChannelIndex])
    then FCompressor[ChannelIndex].Ratio := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TReverseVerbDataModule.ParameterOutputGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutputGain := dB_to_Amp(Value);

(*
 // update GUI
 if EditorForm is TFmReverseVerb
  then TFmReverseVerb(EditorForm).UpdateOutputGain;
*)
end;

procedure TReverseVerbDataModule.ParameterERTimeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 FERTime := 0.001 * Value;
 ImpulseResponseChanged;

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressor) - 1 do
   if Assigned(FCompressor[ChannelIndex])
    then FCompressor[ChannelIndex].Attack := 0.1 * Value;
 finally
  FCriticalSection.Leave;
 end;

(*
 // update GUI
 if EditorForm is TFmReverseVerb
  then TFmReverseVerb(EditorForm).UpdateERTime;
*)
end;

procedure TReverseVerbDataModule.ParameterDirectionChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
  FIsBackward := Boolean(Round(Value));

  if FUpdateCount = 0
   then LoadUpdatedIR;

  if FIsBackward
   then OnProcess := VSTModuleProcessBackward
   else OnProcess := VSTModuleProcessForward;
  OnProcess32Replacing := OnProcess;
end;

procedure TReverseVerbDataModule.ParameterDirectionDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
   0 : PreDefined := 'Forward';
   1 : PreDefined := 'Backward';
  else PreDefined := 'Undefined';
 end;
end;

procedure TReverseVerbDataModule.ParameterDampingLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then Predefined := 'kHz';
end;

procedure TReverseVerbDataModule.ParameterDampingDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := AnsiString(FloatToStrF(1E-3 * Parameter[Index], ffGeneral, 4, 4))
  else PreDefined := AnsiString(FloatToStrF(Parameter[Index], ffGeneral, 4, 4));
end;

procedure TReverseVerbDataModule.ParameterT60Change(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 T60 := 0.001 * Value;

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressor) - 1 do
   if Assigned(FCompressor[ChannelIndex])
    then FCompressor[ChannelIndex].Release := Value;
 finally
  FCriticalSection.Leave;
 end;

(*
 // update GUI
 if EditorForm is TFmReverseVerb
  then TFmReverseVerb(EditorForm).UpdateGain;
*)
end;

procedure TReverseVerbDataModule.BeginUpdate;
begin
 Inc(FUpdateCount);
 Assert(FUpdateCount > 0);
end;

procedure TReverseVerbDataModule.EndUpdate;
begin
 Dec(FUpdateCount);
 Assert(FUpdateCount >= 0);
 if FUpdateCount = 0
  then ImpulseResponseChanged;
end;

procedure TReverseVerbDataModule.ImpulseResponseChanged;
begin
 if FUpdateCount > 0 then Exit;
 
 {$IFDEF DirectUpdate}
 UpdateImpulseResponse;
 LoadUpdatedIR;
 {$ELSE}
 FIRChanged := True;
 if Assigned(FIRUpdateThread) and FIRUpdateThread.Suspended
  then FIRUpdateThread.Resume;
 {$ENDIF}
end;

procedure TReverseVerbDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ChannelIndex : Integer;
begin
 if Abs(SampleRate) = 0 then Exit;

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to Length(FCompressor) - 1 do
   if Assigned(FCompressor[ChannelIndex])
    then FCompressor[ChannelIndex].SampleRate := SampleRate;
 finally
  FCriticalSection.Leave;
 end;

 if FUpdateCount = 0
  then ImpulseResponseChanged;
end;

procedure TReverseVerbDataModule.VSTModuleProcessBackward(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 Assert(SampleFrames <= FTempBufferSize);
 Assert(Length(FConvolution) = CNumChannels);
 Assert(Length(FDelay) = CNumChannels);

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to CNumChannels - 1 do
   if Assigned(FDelay[ChannelIndex, 0]) and Assigned(FConvolution[ChannelIndex]) then
    begin
     FConvolution[ChannelIndex].ProcessBlock(@Inputs[ChannelIndex, 0],
       @Outputs[ChannelIndex, 0], SampleFrames);

     // apply output gain
     for SampleIndex := 0 to SampleFrames - 1 do
      begin
       Outputs[ChannelIndex, SampleIndex] :=
         FOutputGain * (Outputs[ChannelIndex, SampleIndex] +
         FDelay[ChannelIndex, 0].ProcessSample32(
           FDelay[ChannelIndex, 1].ProcessSample32(
             FCompressor[ChannelIndex].ProcessSample32(
               Inputs[ChannelIndex, SampleIndex]))));
      end;
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TReverseVerbDataModule.VSTModuleProcessForward(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
begin
 Assert(SampleFrames <= FTempBufferSize);
 Assert(Length(FConvolution) = CNumChannels);
 Assert(Length(FDelay) = CNumChannels);

 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to CNumChannels - 1 do
   if Assigned(FDelay[ChannelIndex, 0]) and
      Assigned(FConvolution[ChannelIndex]) and
      Assigned(FCompressor[ChannelIndex]) then
    begin
     // perform convolution
     FConvolution[ChannelIndex].ProcessBlock(@Inputs[ChannelIndex, 0],
       @Outputs[ChannelIndex, 0], SampleFrames);

     // apply output gain
     for SampleIndex := 0 to SampleFrames - 1 do
      begin
       Outputs[ChannelIndex, SampleIndex] := FOutputGain *
         (FDelay[ChannelIndex, 0].ProcessSample32(
            Outputs[ChannelIndex, SampleIndex] +
            FDelay[ChannelIndex, 1].ProcessSample32(
              FCompressor[ChannelIndex].ProcessSample32(
                Inputs[ChannelIndex, SampleIndex]))));
      end;
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
