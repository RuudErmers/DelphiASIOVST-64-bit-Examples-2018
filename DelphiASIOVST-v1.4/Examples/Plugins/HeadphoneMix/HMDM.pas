unit HMDM;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_VSTModule, DAV_DspCrosstalkSimulator;

type
  THMModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure HMMEffectChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure HMMModelDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure HMMModelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure HMMPolarityDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure HMMPolarityChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCTSimulator : TCustomIIRCrosstalkSimulator;
  public
  end;

implementation

{$IFDEF UseDelphi}
{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}
{$ENDIF}

uses
  Math, SysUtils;

procedure THMModule.VSTModuleOpen(Sender: TObject);
begin
 FCTSimulator := TCustomIIRCrosstalkSimulator.Create;

 Parameter[0] := 50;
 Parameter[1] := 0;
end;

procedure THMModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCTSimulator);
end;

procedure THMModule.HMMEffectChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FCTSimulator)
  then FCTSimulator.Mix := Value;
end;

procedure THMModule.HMMModelDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[index]) of
  0 : PreDefined := 'handcrafted';
  1 : PreDefined := 'matched IRCAM';
  2 : PreDefined := 'matched HDPHX';
 end;
end;

procedure THMModule.HMMModelChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FCTSimulator) then
  case Round(Parameter[index]) of
   0 : begin
        FCTSimulator.Model := csmHandcrafted;
        FCTSimulator.Diameter := 0.12;
       end;
   1 : begin
        FCTSimulator.Model := csmIRCAM;
        FCTSimulator.Diameter := 0.11;
       end;
   2 : begin
        FCTSimulator.Model := csmHDPHX;
        FCTSimulator.Diameter := 0.11;
       end;
  end;
end;

procedure THMModule.HMMPolarityDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Parameter[index] > 0.5
  then PreDefined := '-'
  else PreDefined := '+';
end;

procedure THMModule.HMMPolarityChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FCTSimulator)
  then FCTSimulator.Polarity := (Value < 0.5);
end;

procedure THMModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex  : Integer;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   Outputs[0, SampleIndex] := Inputs[1, SampleIndex];
   Outputs[1, SampleIndex] := Inputs[1, SampleIndex];

   FCTSimulator.ProcessSample(Outputs[0, SampleIndex], Outputs[1, SampleIndex]);
  end;
end;

procedure THMModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 if Abs(SampleRate) > 0 then
  if Assigned(FCTSimulator)
   then FCTSimulator.SampleRate := SampleRate;
end;

end.
