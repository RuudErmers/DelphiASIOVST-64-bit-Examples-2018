unit HumRemovalDSP;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspFilterButterworth,
  DAV_DspFilterChebyshevType1, DAV_DspFilterChebyshevType2, DAV_DspHumRemoval,
  DAV_DspGoertzel;

type
  THumRemovalModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDetect(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterHighpassActiveDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterHighpassTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterHighpassOrderDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterHighpassTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFundamentalFrequencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterBandwidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighpassActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCaptureHumProfileChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttenuationChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FHumRemoval      : array of TDspHumRemoval;
    FGoertzel        : array [0..20] of TDspGoertzel;
    FSampleCount     : Integer;
  public
    function Magnitude_dB(Frequency: Single): Single;  
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  HumRemovalGUI;

procedure THumRemovalModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
 Assert(numInputs = numOutputs);
end;

procedure THumRemovalModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure THumRemovalModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
  Index   : Integer;
begin
 SetLength(FHumRemoval, numInputs);

 // create hum removal class
 for Channel := 0 to Length(FHumRemoval) - 1
  do FHumRemoval[Channel] := TDspHumRemoval.Create;

 for Index := 0 to Length(FGoertzel) - 1 do
  begin
   FGoertzel[Index] := TDspGoertzel.Create;
   FGoertzel[Index].Frequency := 45 + 1 * Index;
  end;

 // set editor form class
 EditorFormClass := TFmHumRemoval;

 // initialize parameters
 Parameter[0] := 1;
 Parameter[1] := 0;
 Parameter[2] := 75;
 Parameter[3] := 2;
 Parameter[4] := 50;
 Parameter[5] := 0.08;
 Parameter[6] := 48;
 Parameter[7] := 0;

 with Programs[0] do
  begin
   // initialize parameters
   Parameter[0] := 1;
   Parameter[1] := 0;
   Parameter[2] := 75;
   Parameter[3] := 2;
   Parameter[4] := 50;
   Parameter[5] := 0.08;
   Parameter[6] := 48;
   Parameter[7] := 0;
  end;

 with Programs[1] do
  begin
   // initialize parameters
   Parameter[0] := 1;
   Parameter[1] := 0;
   Parameter[2] := 75;
   Parameter[3] := 2;
   Parameter[4] := 50;
   Parameter[5] := 0.08;
   Parameter[6] := 48;
   Parameter[7] := 0;
  end;

 with Programs[2] do
  begin
   // initialize parameters
   Parameter[0] := 1;
   Parameter[1] := 0;
   Parameter[2] := 75;
   Parameter[3] := 2;
   Parameter[4] := 60;
   Parameter[5] := 0.08;
   Parameter[6] := 48;
   Parameter[7] := 0;
  end;
end;

procedure THumRemovalModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
  Index   : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1
  do FreeAndNil(FHumRemoval[Channel]);

 for Index := 0 to Length(FGoertzel) - 1
  do FreeAndNil(FGoertzel[Index]);
end;

procedure THumRemovalModule.ParameterHighpassOrderDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 PreDefined := AnsiString(IntToStr(Round(Parameter[Index])));
end;

procedure THumRemovalModule.ParameterHighpassTypeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if Assigned(FHumRemoval[Channel]) then
   case Round(Value) of
    0 : FHumRemoval[Channel].HighpassFilterType := TButterworthLowCutFilter;
    1 : FHumRemoval[Channel].HighpassFilterType := TChebyshev1LowCutFilter;
    2 : FHumRemoval[Channel].HighpassFilterType := TChebyshev2LowCutFilter;
   end;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassType;
end;

procedure THumRemovalModule.ParameterHighpassFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if Assigned(FHumRemoval[Channel]) then
   if Assigned(FHumRemoval[Channel].HighpassFilter)
    then FHumRemoval[Channel].HighpassFilter.Frequency := Value;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassFrequency;
end;

procedure THumRemovalModule.ParameterHighpassOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if Assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].HighpassFilter.Order := Round(Value);

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassOrder;
end;

procedure THumRemovalModule.ParameterHighpassActiveChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if Assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].HighpassFilterActive := Value > 0.5;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateHighpassActive;
end;

procedure THumRemovalModule.ParameterCaptureHumProfileChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Ndx : Integer;
begin
 if Value > 0.5
  then OnProcess := VSTModuleProcessDetect
  else OnProcess := VSTModuleProcess;
 OnProcess32Replacing := OnProcess;

 // reset sample count
 FSampleCount := 0;

 // reset goertzel filters
 for Ndx := 0 to Length(FGoertzel) - 1 do
  if Assigned(FGoertzel[Ndx])
   then FGoertzel[Ndx].Reset;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateCaptureHumProfile;
end;

procedure THumRemovalModule.ParameterAttenuationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if Assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].Attenuation := Value;
end;

function THumRemovalModule.Magnitude_dB(Frequency: Single): Single;
begin
 if Assigned(FHumRemoval[0])
  then Result := FHumRemoval[0].Magnitude_dB(Frequency)
  else Result := 1;
end;

procedure THumRemovalModule.ParameterBandwidthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if Assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].Bandwidth := Value;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateBandwidth;
end;

procedure THumRemovalModule.ParameterFundamentalFrequencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  if Assigned(FHumRemoval[Channel])
   then FHumRemoval[Channel].FundamentalFrequency := Value;

 // eventually update GUI
 if EditorForm is TFmHumRemoval then
  with TFmHumRemoval(EditorForm)
   do UpdateFundamentalFrequency;
end;

procedure THumRemovalModule.ParameterHighpassActiveDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
const
  CActiveStr : array [0..1] of AnsiString = ('On', 'Off');
begin
 PreDefined := CActiveStr[Integer(Parameter[Index] < 0.5)];
end;

procedure THumRemovalModule.ParameterHighpassTypeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Butterworth';
  1 : PreDefined := 'Chebyshev I';
  2 : PreDefined := 'Chebyshev II';
 end;
end;

procedure THumRemovalModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if Abs(SampleRate) > 0 then
  for Channel := 0 to Length(FHumRemoval) - 1 do
   if Assigned(FHumRemoval[Channel])
    then FHumRemoval[Channel].HighpassFilter.SampleRate := abs(SampleRate);
end;

procedure THumRemovalModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 for Channel := 0 to Length(FHumRemoval) - 1 do
  for Sample := 0 to SampleFrames - 1
   do Outputs[Channel, Sample] := FHumRemoval[Channel].ProcessSample32(
     Inputs[Channel, Sample]);
end;

procedure THumRemovalModule.VSTModuleProcessDetect(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Index   : Integer;
  Sample  : Integer;
  Sum     : Single;
  Power   : Single absolute Sum;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Sum := 0;
   for Channel := 0 to Length(FHumRemoval) - 1 do
    begin
     Outputs[Channel, Sample] := FHumRemoval[Channel].ProcessSample32(Inputs[Channel, Sample]);
     Sum := Sum + Inputs[Channel, Sample];
    end;

   for Index := 0 to Length(FGoertzel) - 1
    do FGoertzel[Index].ProcessSample64(Sum);

   Inc(FSampleCount);

   if FSampleCount >= Round(0.5 * SampleRate) then
    begin
     Index := 0;
     Power := FGoertzel[0].Power_dB;
     for Channel := 1 to Length(FGoertzel) - 1 do
      if FGoertzel[Channel].Power_dB > Power then
       begin
        Power := FGoertzel[Channel].Power_dB;
        Index := Channel;
       end;

     // set fundamental frequency  
     Parameter[4] := FGoertzel[Index].Frequency;
     FSampleCount := 0;
    end;
  end;
end;

end.
