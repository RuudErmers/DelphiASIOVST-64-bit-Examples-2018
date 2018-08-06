unit FrequencyDomainPitchShifterDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFrequencyDomainPitchshifter;

type
  TFrequencyDomainPitchShifterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterPitchFactorChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPitchShifter : array [0..1] of TFrequencyDomainPitchShifter32;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, Dialogs, FrequencyDomainPitchShifterGUI, DAV_VSTCustomModule;

procedure TFrequencyDomainPitchShifterModule.VSTModuleOpen(Sender: TObject);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FPitchShifter[ch] := TFrequencyDomainPitchShifter32.Create;

 // initialize parameters
 Parameter[0] := 1;

 // set editor form class
 EditorFormClass := TFmFrequencyDomainPitchShifter;
end;

procedure TFrequencyDomainPitchShifterModule.ParameterPitchFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to NumInputs - 1
  do FPitchShifter[ChannelIndex].PitchFactor := Power(2, Value / 12);
 if EditorForm is TFmFrequencyDomainPitchShifter
  then TFmFrequencyDomainPitchShifter(EditorForm).UpdateSemitones;
end;

procedure TFrequencyDomainPitchShifterModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to NumInputs - 1
  do FreeAndNil(FPitchShifter[ChannelIndex]);
end;

procedure TFrequencyDomainPitchShifterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FPitchShifter[ch].ProcessBlock(@Inputs[ch, 0], @Outputs[ch, 0], SampleFrames);
end;

procedure TFrequencyDomainPitchShifterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  ch : Integer;
begin
 for ch := 0 to NumInputs - 1
  do FPitchShifter[ch].SampleRate := SampleRate;
end;

end.
