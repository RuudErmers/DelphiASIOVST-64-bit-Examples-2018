unit BarberpoleTunerDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_DspBarberpoleTuner,
  DAV_DspFilterButterworth;

type
  TBarberpoleTunerDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterGuitarStringChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNoteDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
  private
    FLowpass          : TButterworthLowPassFilter;
    FBarberpoleFilter : TBarberpoleFilter;
    FDownSamplePos    : Integer;
    FDownSampleCount  : Integer;
    FLinearBuffer     : PDAVSingleFixedArray;
  public
    property BufferPointer: PDAVSingleFixedArray read FLinearBuffer;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, BarberpoleTunerGUI;

procedure TBarberpoleTunerDataModule.VSTModuleOpen(Sender: TObject);
begin
 // create and setup initial lowpass filter
 FLowpass := TButterworthLowPassFilter.Create(2);
 with FLowpass do
  begin
   SampleRate := Self.SampleRate;
   Frequency := 8000;
  end;

 // create and setup barberpole tuner filter
 FBarberpoleFilter := TBarberpoleFilter.Create;
 FBarberpoleFilter.SampleRate := SampleRate;

 // register editor
 EditorFormClass := TFmBarberpoleTuner;

 FDownSamplePos    := 0;
 FDownSampleCount  := 1 shl 8;
 GetMem(FLinearBuffer, 256 * SizeOf(Single));

 Parameter[0] := 2;
end;

procedure TBarberpoleTunerDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FBarberpoleFilter);
 FreeAndNil(FLowpass);
end;

procedure TBarberpoleTunerDataModule.ParameterGuitarStringChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  CenterFrequency : Double;
begin
 try
  case Round(Parameter[Index]) of
   1 : CenterFrequency := 329.62755691286992973584176104656;
   2 : CenterFrequency := 440;
   3 : CenterFrequency := 587.32953583481512052556602772116;
   4 : CenterFrequency := 783.99087196349858817139906091965;
   5 : CenterFrequency := 987.76660251224822366150908371768;
   6 : CenterFrequency := 1318.5102276514797189433670441862;
   else raise Exception.Create('Current frequency doesn''t exist');
  end;
 except
  CenterFrequency := 440;
 end;

 // update GUI
 if Assigned(FBarberpoleFilter)
  then FBarberpoleFilter.Frequency := CenterFrequency;
end;

procedure TBarberpoleTunerDataModule.ParameterNoteDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  1 : PreDefined := 'E';
  2 : PreDefined := 'A';
  3 : PreDefined := 'D';
  4 : PreDefined := 'G';
  5 : PreDefined := 'H';
  6 : PreDefined := 'E''';
 end;
end;

procedure TBarberpoleTunerDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  begin
   if Assigned(FBarberpoleFilter) then FBarberpoleFilter.SampleRate := SampleRate;
   if Assigned(FLowpass) then FLowpass.SampleRate := SampleRate;
  end;
end;

procedure TBarberpoleTunerDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample, c1 : Integer;
  Signal     : Single;
begin
 c1 := 1;
 for Sample := 0 to SampleFrames - 1 do
  begin
   Signal := FLowpass.ProcessSample64(Inputs[0, Sample]);
   Signal := FBarberpoleFilter.ProcessSample32(Signal + 2 * sqr(Signal) - 1);
   if FDownSamplePos = 0 then
    begin
     Move(FLinearBuffer^[0], FLinearBuffer^[c1], 255 * SizeOf(Single));
     FLinearBuffer^[0] := Limit(2 * Signal, -1, 1);
    end;

   // advance downsample position
   inc(FDownSamplePos);
   if FDownSamplePos >= FDownSampleCount
    then FDownSamplePos := 0;

   Outputs[0, Sample] := Signal;
  end;
end;

end.
