unit OverdriveDM;

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
  TOverdriveDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMuffleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FDrive  : Single;
    FGain   : Single;
    FFilter : Single;
    FState  : array [0..1] of Double;
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

procedure TOverdriveDataModule.ParameterDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDrive := 0.01 * Value;
end;

procedure TOverdriveDataModule.ParameterMuffleChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
  FFilter := Power(10, -1.6 * 0.01 * Value);
end;

procedure TOverdriveDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain := dB_to_Amp(Value);
end;

procedure TOverdriveDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 0;  // [%]
 Parameter[1] := 0;  // [%]
 Parameter[2] := 0;  // [dB]
end;

procedure TOverdriveDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample : Integer;
  State  : array [0..1] of Double;
  Inp    : array [0..1] of Double;
  Outp   : array [0..1] of Double;
begin
 State[0] := FState[0];
 State[1] := FState[1];

 for Sample := 0 to SampleFrames - 1 do
  begin
   Inp[0] := Inputs[0, Sample];
   if (Inp[0] > 0)
    then Outp[0] :=  sqrt( Inp[0])
    else Outp[0] := -sqrt(-Inp[0]); //overdrive

   Inp[1] := Inputs[1, Sample];
   if (Inputs[1, Sample] > 0)
    then Outp[1] :=  sqrt( Inp[1])
    else Outp[1] := -sqrt(-Inp[1]); //overdrive

   State[0] := State[0] + FFilter * (FDrive * (Outp[0] - Inp[0]) + Inp[0] - State[0]); //filter
   State[1] := State[1] + FFilter * (FDrive * (Outp[1] - Inp[1]) + Inp[1] - State[1]);

   Outputs[0, Sample] := FGain * State[0];
   Outputs[1, Sample] := FGain * State[1];
  end;

  if abs(State[0]) > 1E-10
   then FState[0] := State[0]
   else FState[0] := 0;        //catch denormals
  if abs(State[1]) > 1E-10
   then FState[1] := State[1]
   else FState[1] := 0;
end;

end.
