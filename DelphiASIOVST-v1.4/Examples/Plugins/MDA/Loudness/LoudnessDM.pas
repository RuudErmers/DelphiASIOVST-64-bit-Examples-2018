unit LoudnessDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, DAV_Types, DAV_VSTModule;

type
  TLoudnessDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure ParameterLinkDisplay( Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FIsBoost : Boolean;
    FGain    : Single;
    FIGain   : Single;
    FOGain   : Single;
    FCoeffs  : array [0..2] of Single;
    FState   : array [0..1, 0..1] of Single;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math;

const
  cLoudness : array [0..13, 0..2] of Single =
    ( (402,  0.0025,  0.00),  //-60dB
      (334,  0.0121,  0.00),
      (256,  0.0353,  0.00),
      (192,  0.0900,  0.00),
      (150,  0.2116,  0.00),
      (150,  0.5185,  0.00),
      (  1,  0     ,  0.00),  //0dB
      (33.7,    5.5,  1.00),
      (92,      8.7,  0.62),
      (63.7,   18.4,  0.44),
      (42.9,   48.2,  0.30),
      (37.6,  116.2,  0.18),
      (22.9,  428.7,  0.09),  //+60dB
      (   0,      0,  0.00)  );


procedure TLoudnessDataModule.ParameterLinkDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'ON'
  else PreDefined := 'OFF';
end;

procedure TLoudnessDataModule.VSTModuleOpen(Sender: TObject);
begin
 VSTModuleSuspend(Sender);
(*
  Parameter[0] = 0.7;  //loudness
  Parameter[1] = 0.5;  //output
  Parameter[2] = 0.35; //link
*)
end;

procedure TLoudnessDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 VSTModuleResume(Sender);
end;

procedure TLoudnessDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
  z0, z1,
  z2, z3  : Single;
begin
 z0 := 0;
 z1 := 0;
 z2 := 0;
 z3 := 0;
  if (FIsBoost = False) then //cut
   for Sample := 0 to SampleFrames - 1 do
    begin
     z0 := z0 + FCoeffs[0] * (Inputs[0, Sample] - z0 + 0.3 * z1);
     Inputs[0, Sample] := Inputs[0, Sample] - z0;
     z1 := z1 + FCoeffs[0] * (Inputs[0, Sample] - z1);
     Inputs[0, Sample] := Inputs[0, Sample] - z1;
     Inputs[0, Sample] := Inputs[0, Sample] - z0 * FCoeffs[1];

     z2 := z2 + FCoeffs[0] * (Inputs[1, Sample] - z2 + 0.3 * z1);
     Inputs[1, Sample] := Inputs[1, Sample] - z2;
     z3 := z3 + FCoeffs[0] * (Inputs[1, Sample] - z3);
     Inputs[1, Sample] := Inputs[1, Sample] - z3;
     Inputs[1, Sample] := Inputs[1, Sample] - z2 * FCoeffs[1];

     Outputs[0, Sample] := Inputs[0, Sample] * FGain;
     Outputs[1, Sample] := Inputs[1, Sample] * FGain;
    end
  else //boost
   for Sample := 0 to SampleFrames - 1 do
    begin
      z0 := z0 + FCoeffs[0] * (Inputs[0, Sample]  - z0);
      z1 := z1 + FCoeffs[0] * (z0 - z1);
      Inputs[0, Sample]  := Inputs[0, Sample] + FCoeffs[1] * (z1 - FCoeffs[2] * z0);

      z2 := z2 + FCoeffs[0] * (Inputs[1, Sample]  - z2);
      z3 := z3 + FCoeffs[0] * (z2 - z3);
      Inputs[1, Sample] := Inputs[1, Sample] + FCoeffs[1] * (z3 - FCoeffs[2] * z2);

     Outputs[0, Sample] := Inputs[0, Sample] * FGain;
     Outputs[1, Sample] := Inputs[1, Sample] * FGain;
    end;

  if (abs(z1) < 1E-10) or (abs(z1) > 100) then
   begin
    FState[0, 0] := 0;
    FState[0, 1] := 0;
   end
  else
   begin
    FState[0, 0] := z0;
    FState[0, 1] := z1;
   end; //catch denormals
  if (abs(z3) < 1E-10) or (abs(z3) > 100) then
   begin
    FState[1, 0] := 0;
    FState[1, 1] := 0;
   end
  else
   begin
    FState[1, 0] := z2;
    FState[1, 1] := z3;
   end;
end;

procedure TLoudnessDataModule.VSTModuleResume(Sender: TObject);
var
  f, tmp : Single;
  i      : Integer;
begin
 tmp   := sqr(Parameter[0]) - 1;
 FIGain := 60 * sqr(tmp);
 if (tmp < 0) then FIGain := -FIGain ;
 tmp := Parameter[1] + Parameter[1] - 1;
 FOGain := 60 * sqr(tmp);
 if (tmp < 0)
  then FOGain := -FOGain;

 f := 0.1 * FIGain + 6;  //coefficient index + fractional part
 i := Round(f);
 f := f - i;

 tmp := cLoudness[i][0];  FCoeffs[0] := tmp + f * (cLoudness[i + 1][0] - tmp);
 tmp := cLoudness[i][1];  FCoeffs[1] := tmp + f * (cLoudness[i + 1][1] - tmp);
 tmp := cLoudness[i][2];  FCoeffs[2] := tmp + f * (cLoudness[i + 1][2] - tmp);

 FCoeffs[0] := 1 - Exp(-6.283153 * FCoeffs[0] / SampleRate);

 if (FIGain > 0) then
  begin
   //if not FIsBoost then suspend();  //don't click when switching mode
   FIsBoost := True;
  end
 else
  begin
   //if FIsBoost then suspend();
   FIsBoost := False;
  end;

  tmp := FOGain;
  if (Parameter[2] > 0.5) then //linked gain
   begin
    tmp := tmp - FIGain;
    if (tmp > 0)
     then tmp := 0;  //limit max gain
   end
  else FGain := Power(10, 0.05 * tmp);
end;

procedure TLoudnessDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FState[0, 0] := 0;
 FState[0, 1] := 0;
 FState[1, 0] := 0;
 FState[1, 1] := 0;
end;

end.

(*
void mdaLoudness::getParameterDisplay(VstInt32 index, char *text)
{
   char string[16];

  switch(index)
  {
    case  0: sprintf(string, "%.1f", igain); break;
    default: sprintf(string, "%.1f", ogain); break;
  }
  string[8] = 0;
  strcpy(text, (char * )string);
}

*)
