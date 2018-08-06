unit PluginDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics;

type
  TPluginDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcessLR(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessMS(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
  private
    FEQs : array [0..1, 0..10] of TBasicPeakFilter;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  EditorFrm;

procedure TPluginDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel, Band : Integer;
const
  CDefaultFrequencies : array [0..10] of Single = (20, 40, 80, 160, 320, 640,
    1250, 2500, 5000, 10000, 20000);   
begin
 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   begin
    if not Assigned(FEQs[Channel, Band])
     then FEQs[Channel, Band] := TBasicPeakFilter.Create;
    with FEQs[Channel, Band] do
     begin
      SampleRate := Self.SampleRate;
      Frequency := CDefaultFrequencies[Band];
      Bandwidth := 1;
     end;
   end;
 EditorFormClass := TEditorForm;
end;

procedure TPluginDataModule.VSTModuleClose(Sender: TObject);
var
  Channel, Band : Integer;
begin
 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   if not Assigned(FEQs[Channel, Band]) then FreeAndNil(FEQs[Channel, Band]);
end;

procedure TPluginDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  PosVal : Integer;
begin
 if Assigned(FEQs[Index div 11, Index mod 11])
  then FEQs[Index div 11, Index mod 11].Gain := -Value;

 // update GUI
 PosVal := Round(Value * 10);
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   case Index of
     0: if SB20L.Position   <> PosVal then SB20L.Position   := PosVal;
     1: if SB40L.Position   <> PosVal then SB40L.Position   := PosVal;
     2: if SB80L.Position   <> PosVal then SB80L.Position   := PosVal;
     3: if SB160L.Position  <> PosVal then SB160L.Position  := PosVal;
     4: if SB320L.Position  <> PosVal then SB320L.Position  := PosVal;
     5: if SB640L.Position  <> PosVal then SB640L.Position  := PosVal;
     6: if SB1200L.Position <> PosVal then SB1200L.Position := PosVal;
     7: if SB2500L.Position <> PosVal then SB2500L.Position := PosVal;
     8: if SB5kL.Position   <> PosVal then SB5kL.Position   := PosVal;
     9: if SB10kL.Position  <> PosVal then SB10kL.Position  := PosVal;
    10: if SB20kL.Position  <> PosVal then SB20kL.Position  := PosVal;
    11: if SB20R.Position   <> PosVal then SB20R.Position   := PosVal;
    12: if SB40R.Position   <> PosVal then SB40R.Position   := PosVal;
    13: if SB80R.Position   <> PosVal then SB80R.Position   := PosVal;
    14: if SB160R.Position  <> PosVal then SB160R.Position  := PosVal;
    15: if SB320R.Position  <> PosVal then SB320R.Position  := PosVal;
    16: if SB640R.Position  <> PosVal then SB640R.Position  := PosVal;
    17: if SB1200R.Position <> PosVal then SB1200R.Position := PosVal;
    18: if SB2500R.Position <> PosVal then SB2500R.Position := PosVal;
    19: if SB5kR.Position   <> PosVal then SB5kR.Position   := PosVal;
    20: if SB10kR.Position  <> PosVal then SB10kR.Position  := PosVal;
    21: if SB20kR.Position  <> PosVal then SB20kR.Position  := PosVal;
   end;
end;

procedure TPluginDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel, Band : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FEQs) - 1 do
   for Band := 0 to Length(FEQs[Channel]) - 1 do
    begin
     if not Assigned(FEQs[Channel, Band])
      then FEQs[Channel, Band] := TBasicPeakFilter.Create;
     FEQs[Channel, Band].SampleRate := SampleRate;
    end;
end;

procedure TPluginDataModule.VSTModuleProcessLR(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FEQs[0, 0].ProcessSample64(FEQs[0, 1].ProcessSample64(
     FEQs[0, 2].ProcessSample64(FEQs[0, 3].ProcessSample64(
     FEQs[0, 4].ProcessSample64(FEQs[0, 5].ProcessSample64(
     FEQs[0, 6].ProcessSample64(FEQs[0, 7].ProcessSample64(
     FEQs[0, 8].ProcessSample64(FEQs[0, 9].ProcessSample64(
     FEQs[0,10].ProcessSample64(Inputs[0, Sample])))))))))));
   Outputs[1, Sample] := FEQs[1, 0].ProcessSample64(FEQs[1, 1].ProcessSample64(
     FEQs[1, 2].ProcessSample64(FEQs[1, 3].ProcessSample64(
     FEQs[1, 4].ProcessSample64(FEQs[1, 5].ProcessSample64(
     FEQs[1, 6].ProcessSample64(FEQs[1, 7].ProcessSample64(
     FEQs[1, 8].ProcessSample64(FEQs[1, 9].ProcessSample64(
     FEQs[1,10].ProcessSample64(Inputs[1, Sample])))))))))));
  end;
end;

procedure TPluginDataModule.VSTModuleProcessMS(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  Temp   : Double;
const
  CQuarter32 : Single = 0.25;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FEQs[0, 0].ProcessSample64(FEQs[0, 1].ProcessSample64(
     FEQs[0, 2].ProcessSample64(FEQs[0, 3].ProcessSample64(
     FEQs[0, 4].ProcessSample64(FEQs[0, 5].ProcessSample64(
     FEQs[0, 6].ProcessSample64(FEQs[0, 7].ProcessSample64(
     FEQs[0, 8].ProcessSample64(FEQs[0, 9].ProcessSample64(
     FEQs[0,10].ProcessSample64(Inputs[0, Sample] + Inputs[1, Sample])))))))))));
   Outputs[1, Sample] := FEQs[1, 0].ProcessSample64(FEQs[1, 1].ProcessSample64(
     FEQs[1, 2].ProcessSample64(FEQs[1, 3].ProcessSample64(
     FEQs[1, 4].ProcessSample64(FEQs[1, 5].ProcessSample64(
     FEQs[1, 6].ProcessSample64(FEQs[1, 7].ProcessSample64(
     FEQs[1, 8].ProcessSample64(FEQs[1, 9].ProcessSample64(
     FEQs[1,10].ProcessSample64(Inputs[0, Sample] - Inputs[1, Sample])))))))))));
   Temp := CQuarter32 * (Outputs[1, Sample] + Outputs[0, Sample]);
   Outputs[1, Sample] := CQuarter32 * (Outputs[1, Sample] - Outputs[0, Sample]);
   Outputs[0, Sample] := Temp;
  end;
end;

end.
