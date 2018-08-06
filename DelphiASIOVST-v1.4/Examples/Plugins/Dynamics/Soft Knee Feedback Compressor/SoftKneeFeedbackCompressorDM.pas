unit SoftKneeFeedbackCompressorDM;

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
  TSoftKneeFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMakeUpGainChange( Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleClose(Sender: TObject);
  private
    FSoftKneeFeedbackCompressors : array [0..1] of TCustomFeedbackCompressor;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, EditorFrm, DAV_VSTModuleWithPrograms;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSoftKneeFeedbackCompressors[0] := TSoftKneeFeedbackCompressor.Create;
 FSoftKneeFeedbackCompressors[1] := TSoftKneeFeedbackCompressor.Create;

 // Initial Parameters
 Parameter[0] := -20;
 Parameter[1] :=   6;
 Parameter[2] :=   8;
 Parameter[3] := 500;
 Parameter[4] :=  10;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSoftKneeFeedbackCompressors[0]);
 FreeAndNil(FSoftKneeFeedbackCompressors[1]);
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleEditOpen(
  Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Threshold_dB := Value;
 if Assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Threshold_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSoftKneeFeedbackCompressorDataModule.ParamMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].MakeUpGain_dB := Value;
 if Assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].MakeUpGain_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateMakeUpGain;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLRatioChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  if Assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Ratio := 1 / Value;
 if Assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Ratio := 1 / Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRatio;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Release := Value;
 if Assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Release := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Attack := Value;
 if Assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Attack := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FSoftKneeFeedbackCompressors[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSoftKneeFeedbackCompressors[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 if Assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].SampleRate := SampleRate;
 if Assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].SampleRate := SampleRate;
end;

end.
