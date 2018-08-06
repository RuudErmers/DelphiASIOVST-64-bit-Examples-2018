unit SimpleFeedbackCompressorDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TSimpleFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSimpleFeedbackCompressors : array [0..1] of TCustomFeedbackCompressor;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, EditorFrm;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSimpleFeedbackCompressors) - 1 do
  begin
   FSimpleFeedbackCompressors[Channel] := TSimpleFeedbackCompressor.Create;
   FSimpleFeedbackCompressors[Channel].AutoMakeUp := True;
  end;

 // initial parameters 
 Parameter[0] := 0;
 Parameter[1] := 1;
 Parameter[2] := 5;
 Parameter[3] := 40;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleFeedbackCompressors[0]);
 FreeAndNil(FSimpleFeedbackCompressors[1]);
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleFeedbackCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[0].Threshold_dB := Value;
 if Assigned(FSimpleFeedbackCompressors[1])
  then FSimpleFeedbackCompressors[1].Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if DialThreshold.Value <> Round(Value) then
    begin
     DialThreshold.Value := Round(Value);
     UpdateThreshold;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSimpleFeedbackCompressors[0]) then
  begin
   FSimpleFeedbackCompressors[0].Ratio := 1 / Value;
   FSimpleFeedbackCompressors[1].Ratio := FSimpleFeedbackCompressors[0].Ratio;
  end;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if DialRatio.Value <> Round(100 * Log10(Value)) then
    begin
     DialRatio.Value := Round(100 * Log10(Value));
     UpdateRatio;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[0].Release := Value;
 if Assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[1].Release := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if DialRelease.Value <> Round(Value) then
    begin
     DialRelease.Value := Round(1000 * Log10(Value));
     UpdateRelease;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  SampleDuration_ms : Single;
begin
 SampleDuration_ms := 1000 / SampleRate;
 if Value < 3 * SampleDuration_ms
  then Value := 3 * SampleDuration_ms;

 if Assigned(FSimpleFeedbackCompressors[0])
  then FSimpleFeedbackCompressors[0].Attack := Value;
 if Assigned(FSimpleFeedbackCompressors[1])
  then FSimpleFeedbackCompressors[1].Attack := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSimpleFeedbackCompressors[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSimpleFeedbackCompressors[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
var
  SampleDuration_ms : Single;
begin
 if Abs(SampleRate) <> 0 then
  begin
   if Assigned(FSimpleFeedbackCompressors[0])
    then FSimpleFeedbackCompressors[0].SampleRate := SampleRate;
   if Assigned(FSimpleFeedbackCompressors[1])
    then FSimpleFeedbackCompressors[1].SampleRate := SampleRate;

   SampleDuration_ms := 1000 / SampleRate;
   if Parameter[3] < 3 * SampleDuration_ms
    then Parameter[3] := 3 * SampleDuration_ms;
  end;
end;

end.
