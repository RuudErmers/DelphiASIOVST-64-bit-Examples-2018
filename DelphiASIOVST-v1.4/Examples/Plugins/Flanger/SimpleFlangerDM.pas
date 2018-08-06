unit SimpleFlangerDM;

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
  {$IFDEF FPC}LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms,
  SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspVibrato;

type
  TSimpleFlangerModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FVibrato         : array [0..1] of TDspVibrato32;
    FMix, FMixInv    : Single;
    FCriticalSection : TCriticalSection;
    function GetFlanger(Index: Integer): TDspVibrato32;
  public
    property Flanger[Index: Integer]: TDspVibrato32 read GetFlanger;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, DAV_Approximations, SimpleFlangerGUI;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

procedure TSimpleFlangerModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSimpleFlangerModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSimpleFlangerModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FVibrato) - 1 do
  begin
   FVibrato[Channel] := TDspVibrato32.Create;
   FVibrato[Channel].SampleRate := SampleRate;
  end;

 // initialize parameters
 Parameter[0] :=  5;
 Parameter[1] :=  0.2;
 Parameter[2] := 50;
 with Programs[0] do
  begin
   Parameter[0] :=  5;
   Parameter[1] :=  0.2;
   Parameter[2] := 50;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  2;
   Parameter[1] :=  0.02;
   Parameter[2] := 50;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  4;
   Parameter[1] :=  0.04;
   Parameter[2] := 50;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  4.5;
   Parameter[1] :=  0.62;
   Parameter[2] := 50;
  end;

 // set editor GUI form
 EditorFormClass := TFmSimpleFlanger;
end;

procedure TSimpleFlangerModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FVibrato) - 1
  do FreeAndNil(FVibrato[Channel]);
end;

function TSimpleFlangerModule.GetFlanger(Index: Integer): TDspVibrato32;
begin
 if Index in [0..1]
  then Result := FVibrato[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TSimpleFlangerModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FVibrato[0]) then FVibrato[0].Speed := Value;
  if Assigned(FVibrato[1]) then FVibrato[1].Speed := Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSimpleFlanger then
  with TFmSimpleFlanger(EditorForm)
   do UpdateSpeed;
end;

procedure TSimpleFlangerModule.ParamMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  FMix := 0.01 * Value;
  FMixInv := 2 - FMix;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSimpleFlanger then
  with TFmSimpleFlanger(EditorForm)
   do UpdateMix;
end;

procedure TSimpleFlangerModule.ParamDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FVibrato[0]) then FVibrato[0].Depth := 0.01 * Value;
  if Assigned(FVibrato[1]) then FVibrato[1].Depth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;

 // update GUI
 if EditorForm is TFmSimpleFlanger then
  with TFmSimpleFlanger(EditorForm)
   do UpdateDepth;
end;

procedure TSimpleFlangerModule.VSTModuleResume(Sender: TObject);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FVibrato[0]) then FVibrato[0].Reset;
  if Assigned(FVibrato[1]) then FVibrato[1].Reset;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSimpleFlangerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FCriticalSection.Enter;
 try
  if Abs(SampleRate) > 0 then
   begin
    if Assigned(FVibrato[0]) then FVibrato[0].SampleRate := Abs(SampleRate);
    if Assigned(FVibrato[1]) then FVibrato[1].SampleRate := Abs(SampleRate);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSimpleFlangerModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ChannelIndex : Integer;
  SampleIndex  : Integer;
  Input        : Single;
begin
 FCriticalSection.Enter;
 try
  for ChannelIndex := 0 to 1 do
   for SampleIndex := 0 to Min(FBlockSize, SampleFrames) - 1 do
    begin
     if IsNaN(Inputs[ChannelIndex, SampleIndex])
      then Input := 0
      else Input := Inputs[ChannelIndex, SampleIndex];
     Outputs[ChannelIndex, SampleIndex] := FastTanhContinousError4(FMix * Input +
       FMixInv * FVibrato[ChannelIndex].ProcessSample32(Input));
    end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSimpleFlangerModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   for Sample := 0 to Min(FBlockSize, SampleFrames) - 1
    do Outputs[Channel, Sample] := FastTanhContinousError4(FMix * Inputs[Channel, Sample] + FMixInv * FVibrato[Channel].ProcessSample32(Inputs[Channel, Sample]))
 finally
  FCriticalSection.Leave;
 end;
end;

end.
