unit SimpleLimiterDM;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_DspDynamics;

type
  TSimpleLimiterDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSimpleLimiters : array [0..1] of TLimiter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  EditorFrm;

procedure TSimpleLimiterDataModule.VSTModuleOpen(Sender: TObject);
begin
 EditorFormClass := TEditorForm;

 FSimpleLimiters[0] := TLimiter.Create;
 FSimpleLimiters[1] := TLimiter.Create;

 Parameter[0] := -10;
end;

procedure TSimpleLimiterDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSimpleLimiters[0]);
 FreeAndNil(FSimpleLimiters[1]);
end;

procedure TSimpleLimiterDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].Threshold_dB := Value;
 if Assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].Threshold_dB := Value;

 // eventually update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSimpleLimiterDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].Release := Value;
 if Assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].Release := Value;

 // eventually update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSimpleLimiterDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].Attack := Value;
 if Assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].Attack := Value;

 // eventually update GUI
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSimpleLimiterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := FSimpleLimiters[0].ProcessSample32(Inputs[0, SampleIndex]);
   Outputs[1, SampleIndex] := FSimpleLimiters[1].ProcessSample32(Inputs[1, SampleIndex]);
  end;
end;

procedure TSimpleLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(FSimpleLimiters[0]) then FSimpleLimiters[0].SampleRate := SampleRate;
 if Assigned(FSimpleLimiters[1]) then FSimpleLimiters[1].SampleRate := SampleRate;
end;

end.
