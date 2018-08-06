unit ButterworthSplitterDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Messages,
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule, DAV_VSTCustomModule,
  DAV_DspFilterButterworth;

type
  TButterworthSplitterModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FButterworthSplitter : array of TButterworthSplitBandFilter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TButterworthSplitterModule }

procedure TButterworthSplitterModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 Assert(numOutputs = 2 * numInputs);
 SetLength(FButterworthSplitter, numInputs);
 for Channel := 0 to Length(FButterworthSplitter) - 1 do
  begin
   FButterworthSplitter[Channel] := TButterworthSplitBandFilter.Create;
   FButterworthSplitter[Channel].SampleRate := SampleRate;
  end;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcessReplacing := VSTModuleProcess;
 {$ENDIF}

 Parameter[0] := 1000;
 Parameter[1] := 2;
end;

procedure TButterworthSplitterModule.VSTModuleClose(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FButterworthSplitter) - 1
  do FreeAndNil(FButterworthSplitter[Channel]);
end;

procedure TButterworthSplitterModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, Channel: Integer;
begin
 {$IFDEF DebugLog} AddLogMessage('VSTModuleProcess'); {$ENDIF}
 // CDenorm32 +
 for Sample := 0 to SampleFrames - 1 do
  for Channel := 0 to Length(FButterworthSplitter) - 1
   do FButterworthSplitter[Channel].ProcessSample32(Inputs[Channel, Sample],
        Outputs[2 * Channel, Sample], Outputs[2 * Channel + 1, Sample])
end;

procedure TButterworthSplitterModule.ParameterOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 Predefined := IntToStr(Round(Parameter[Index]));
end;

procedure TButterworthSplitterModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Freq : Single;
begin
 Freq := Parameter[Index];
 if Freq >= 1000
  then Predefined := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3);
end;

procedure TButterworthSplitterModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[Index] >= 1000
  then PreDefined := 'kHz';
end;

procedure TButterworthSplitterModule.ParameterFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if Assigned(FButterworthSplitter[Channel])
   then FButterworthSplitter[Channel].Frequency := Value;
end;

procedure TButterworthSplitterModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if Assigned(FButterworthSplitter[Channel])
   then FButterworthSplitter[Channel].Order := Round(Value);
end;

procedure TButterworthSplitterModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel: Integer;
begin
 for Channel := 0 to numInputs - 1 do
  if Assigned(FButterworthSplitter[Channel])
   then FButterworthSplitter[Channel].SampleRate := SampleRate;
end;

end.
