unit ConvoFXLiteDM;

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

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspConvolution,
  DAV_DspFilterButterworth;

const
  CNumChannels = 2;

type
  TImpulseResponseUpdateThread = class;
  TConvoFXLiteDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterMaximumIROrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLatencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterIRChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampingChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FConvolution      : array [0..CNumChannels - 1] of TLowLatencyConvolution32;
    FImpulseResponse  : array [0..CNumChannels - 1] of PDAVSingleFixedArray;
    FTempIR           : array [0..CNumChannels - 1] of PDAVSingleFixedArray;
    FTempIRSize       : array [0..CNumChannels - 1] of Integer;
    FImpulseLength    : array [0..CNumChannels - 1] of Integer;
    FGain             : Single;
    FDamping          : Single;
    FDampingFilter    : TButterworthLowpassFilter;
    FImpulseRespIndex : Integer;
    FCriticalSection  : TCriticalSection;
    FIRUpdateThread   : TImpulseResponseUpdateThread;
    FIRChanged        : Boolean;
    FUpdateCount      : Integer;
  protected
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure RenderEnvelopes(const IR: PDAVSingleFixedArray; const Length: Integer);
    procedure UpdateImpulseResponse;
    procedure UpdateIR;
    procedure ImpulseResponseChanged;
  end;

  TImpulseResponseUpdateThread = class(TThread)
  private
    FSampleRateRatio       : Double;
    FConvoFXLiteDataModule : TConvoFXLiteDataModule;
  protected
    constructor Create(ConvoFXLiteDataModule: TConvoFXLiteDataModule); virtual;
    procedure Execute; override;

    property SampleRateRatio: Double read FSampleRateRatio;
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Math, DAV_HalfFloat, DAV_DspInterpolation,
  ConvoFXLiteGUI;

{ TImpulseResponseUpdateThread }

constructor TImpulseResponseUpdateThread.Create(
  ConvoFXLiteDataModule: TConvoFXLiteDataModule);
begin
 FConvoFXLiteDataModule := ConvoFXLiteDataModule;
 inherited Create(True);
end;

procedure TImpulseResponseUpdateThread.Execute;
const
  CDefaultSampleRate: Single = 48000;
begin
 while not Terminated do
  with FConvoFXLiteDataModule do
   begin
    if FIRChanged = True then
     begin
      FIRChanged := False;
      UpdateImpulseResponse;
     end;

    Suspend;
   end;
end;

{ TConvoFXLiteDataModule }

procedure TConvoFXLiteDataModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TConvoFXLiteDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TConvoFXLiteDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FConvolution) - 1 do
  begin
   FConvolution[Channel] := TLowLatencyConvolution32.Create;
   FConvolution[Channel].MinimumIRBlockOrder := Max(7, CeilLog2(InitialDelay));
   FConvolution[Channel].MaximumIRBlockOrder := 18;
  end;
 FGain      := 1;
 FTempIR[0] := nil;
 FTempIR[1] := nil;

 EditorFormClass := TFmConvoFXLite;

 FDampingFilter := TButterworthLowpassFilter.Create(1);
 FIRUpdateThread := TImpulseResponseUpdateThread.Create(Self);

 BeginUpdate;
 Parameter[0] := CeilLog2(InitialDelay);
 Parameter[1] := 18;
 Parameter[2] := 1;
 Parameter[3] := 0;
 Parameter[4] := 16000;
 EndUpdate;
end;

procedure TConvoFXLiteDataModule.VSTModuleClose(Sender: TObject);
var
  Channel: Integer;
begin
 with FIRUpdateThread do
  begin
   Terminate;
   Resume;
   WaitFor;
  end;
 FreeAndNil(FIRUpdateThread);

 Dispose(FTempIR[0]);
 Dispose(FTempIR[1]);

 for Channel := 0 to Length(FConvolution) - 1
  do FreeAndNil(FConvolution[Channel]);
 FreeAndNil(FDampingFilter);
end;

procedure TConvoFXLiteDataModule.ParameterMaximumIROrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FConvolution) - 1 do
   if Assigned(FConvolution[Channel]) then
    if Value >= FConvolution[Channel].MinimumIRBlockOrder
     then FConvolution[Channel].MaximumIRBlockOrder := Round(Limit(Value, 7, 20))
     else Value := FConvolution[Channel].MinimumIRBlockOrder;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TConvoFXLiteDataModule.ParameterDampingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDamping := Value;
 ImpulseResponseChanged;

 if EditorForm is TFmConvoFXLite
  then TFmConvoFXLite(EditorForm).UpdateDamping;
end;

procedure TConvoFXLiteDataModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain := dB_to_Amp(Value);
 ImpulseResponseChanged;

 if EditorForm is TFmConvoFXLite
  then TFmConvoFXLite(EditorForm).UpdateGain;
end;

procedure TConvoFXLiteDataModule.ParameterIRChange(
  Sender: TObject; const Index: Integer; var Value: Single);
const
  CResNames : array [0..9] of string[2] = ('CH', 'LH', 'RC', 'TR', 'SR', 'SP',
    'RP', 'GP', 'GC', 'GN');
var
  i, c, r : Integer;
  ResName : string;
  HFData  : array [0..2047] of THalfFloat;
  Scale   : Single;
begin
 i := Limit(Integer(Round(Value)), 1, 40);
 if FImpulseRespIndex <> i then
  begin
   FImpulseRespIndex := i;
   ResName := CResNames[i - 1];

   while not FIRUpdateThread.Suspended do Sleep(1);

   FCriticalSection.Enter;
   try
    // load left channel
    with TResourceStream.Create(HInstance, ResName + 'L', 'F16') do
     try
      c := 0;
      FImpulseLength[0] := Size div SizeOf(THalfFloat);
      ReallocMem(FImpulseResponse[0], FImpulseLength[0] * SizeOf(Single));
      while c + Length(HFData) < FImpulseLength[0] do
       begin
        Read(HFData[0], Length(HFData) * 2);
        for i := 0 to Length(HFData) - 1
         do FImpulseResponse[0]^[c + i] := HalfFloatToSingle(HFData[i]);
        Inc(c, Length(HFData));
       end;
      if c < FImpulseLength[0] then
       begin
        r := FImpulseLength[0] - c;
        Assert(r > 0);
        Assert(r < Length(HFData));
        Read(HFData[0], r * 2);
        Scale := 1 / r;
        for i := 0 to r - 1
         do FImpulseResponse[0]^[c + i] := (r - i) * Scale * HalfFloatToSingle(HFData[i]);
       end;
     finally
      Free;
     end;

    // load right channel
    with TResourceStream.Create(HInstance, ResName + 'R', 'F16') do
     try
      c := 0;
      FImpulseLength[1] := Size div SizeOf(THalfFloat);
      ReallocMem(FImpulseResponse[1], FImpulseLength[1] * SizeOf(Single));
      while c + Length(HFData) < FImpulseLength[1] do
       begin
        Read(HFData[0], Length(HFData) * 2);
        for i := 0 to Length(HFData) - 1
         do FImpulseResponse[1]^[c + i] := HalfFloatToSingle(HFData[i]);
        Inc(c, Length(HFData));
       end;
      if c < FImpulseLength[1] then
       begin
        r := FImpulseLength[1] - c;
        Assert(r > 0);
        Assert(r < Length(HFData));
        Read(HFData[0], r * 2);
        Scale := 1 / r;
        for i := 0 to r - 1
         do FImpulseResponse[1]^[c + i] := (r - i) * Scale * HalfFloatToSingle(HFData[i]);
       end;
     finally
      Free;
     end;
   finally
    FCriticalSection.Leave;
   end;

   ImpulseResponseChanged;
  end;
 if EditorForm is TFmConvoFXLite
  then TFmConvoFXLite(EditorForm).UpdateIRSelect;
end;

procedure TConvoFXLiteDataModule.UpdateImpulseResponse;
var
  Channel   : Integer;
  Offset    : Double;
  Pos       : Double;
  Sample, r : Integer;
const
  CDefaultSampleRate: Single = 48000;
begin
 for Channel := 0 to Length(FConvolution) - 1 do
  begin
   if Abs(SampleRate - CDefaultSampleRate) < CDenorm32
    then
     begin
      FTempIRSize[Channel] := FImpulseLength[Channel];
      ReallocMem(FTempIR[Channel], FTempIRSize[Channel] * SizeOf(Single));
      Move(FImpulseResponse[Channel]^[0], FTempIR[Channel]^[0], FTempIRSize[Channel] * SizeOf(Single));
     end
    else
     begin
      FTempIRSize[Channel] := Round((FImpulseLength[Channel] - 4) * SampleRate / CDefaultSampleRate);
      Offset := CDefaultSampleRate / SampleRate;
      ReallocMem(FTempIR[Channel], FTempIRSize[Channel] * SizeOf(Single));
      Pos := 0;
      for Sample := 0 to FTempIRSize[Channel] - 1 do
       begin
        r := Round(Pos - CHalf32);
        Assert(Pos - r >= 0);
        Assert(Pos - r <= 1);
        FTempIR[Channel]^[Sample] := Hermite32_asm(Pos - r, @FImpulseResponse[Channel]^[r]);
        Pos := Pos + Offset;
       end;
     end;

   RenderEnvelopes(FTempIR[Channel], FTempIRSize[Channel]);
  end;

 UpdateIR;
end;

procedure TConvoFXLiteDataModule.UpdateIR;
begin
 FCriticalSection.Enter;
 try
  FConvolution[0].LoadImpulseResponse(FTempIR[0], FTempIRSize[0]);
  FConvolution[1].LoadImpulseResponse(FTempIR[1], FTempIRSize[1]);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TConvoFXLiteDataModule.RenderEnvelopes(const IR: PDAVSingleFixedArray;
  const Length: Integer);
var
  Sample   : Integer;
  s, Scale : Single;
begin
 FDampingFilter.ResetStates;
 Scale := 1 / Length;
 for Sample := 0 to Length - 1 do
  begin
   IR[Sample] := FGain * IR[Sample];
   s := 0.1 * sqr(sqr((Length - Sample) * Scale));
   IR[Sample] := s * IR[Sample]  +
     (1 - s) * FDampingFilter.ProcessSample64(IR[Sample]);
  end;
end;

procedure TConvoFXLiteDataModule.ParameterLatencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FConvolution) - 1 do
   if Assigned(FConvolution[Channel]) then
    begin
     if Value > FConvolution[Channel].MaximumIRBlockOrder
      then Value := FConvolution[Channel].MaximumIRBlockOrder;
     FConvolution[Channel].MinimumIRBlockOrder := Round(Value);
    end;
 finally
  FCriticalSection.Leave
 end;
end;

procedure TConvoFXLiteDataModule.BeginUpdate;
begin
 Inc(FUpdateCount);
 Assert(FUpdateCount > 0);
end;

procedure TConvoFXLiteDataModule.EndUpdate;
begin
 Dec(FUpdateCount);
 Assert(FUpdateCount >= 0);
 if FUpdateCount = 0
  then ImpulseResponseChanged;
end;

procedure TConvoFXLiteDataModule.ImpulseResponseChanged;
begin
 if FUpdateCount > 0 then Exit;
 
 FIRChanged := True;
 {$IFDEF DirectUpdate}
 UpdateImpulseResponse;
 {$ELSE}
 if FIRUpdateThread.Suspended
  then FIRUpdateThread.Resume;
 {$ENDIF}
end;

procedure TConvoFXLiteDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 ImpulseResponseChanged;
end;

procedure TConvoFXLiteDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel: Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FConvolution) - 1
   do FConvolution[Channel].ProcessBlock(@Inputs[Channel, 0], @Outputs[Channel, 0], SampleFrames);
 finally
  FCriticalSection.Leave
 end;
end;

end.
