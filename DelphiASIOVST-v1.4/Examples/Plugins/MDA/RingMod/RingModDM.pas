unit RingModDM;

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
  Forms, DAV_Types, DAV_VSTModule;

type
  TRingModDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FPhi      : Single;
    FDeltaPhi : Single;
    FFeedBack : Single;
    FPrev     : Single;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TRingModDataModule.VSTModuleOpen(Sender: TObject);
begin
 FPhi      := 0.0;
 FDeltaPhi := (2 * Pi * 1000) / SampleRate;
 FFeedBack := 0;
 FPrev     := 0;

 // Initial Parameters
 Parameter[0] := 0.0625; //1kHz
 Parameter[1] := 0.0;
 Parameter[2] := 0.0;
end;

procedure TRingModDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FDeltaPhi := (2 * Pi * 100.0 * (Parameter[1] + (160.0 * Parameter[0])) / SampleRate);
end;

procedure TRingModDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  g, fb  : Single;
  p, dp  : Single;
  fp     : array [0..1] of Single;
const
  cTwoPi  : Double = 2 * Pi;
  cTwoPiX : Double = 1 / (2 * Pi);
begin
 p     := FPhi;
 dp    := FDeltaPhi;
 fb    := FFeedBack;
 fp[0] := FPrev;
 fp[1] := FPrev;

 for Sample := 0 to SampleFrames - 1 do
  begin
   g := sin(p);
   p := (p + dp) * cTwoPiX;
   p := (p - Round(p - 0.5)) * cTwoPi; // fmod( p + dp, tp );

   fp[0] := (fb * fp[0] + Inputs[0, Sample]) * g;
   fp[1] := (fb * fp[0] + Inputs[1, Sample]) * g;

   Outputs[0, Sample] := fp[0];
   Outputs[1, Sample] := fp[1];
  end;

 FPhi  := p;
 FPrev := fp[0];
end;

procedure TRingModDataModule.ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFeedBack := 0.95 * 0.01 * Value;
end;

end.
