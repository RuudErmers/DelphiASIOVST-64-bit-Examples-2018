unit TransientDM;

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
  Windows, Messages, Classes, DAV_Types, DAV_VSTModule, DAV_DspTransient;

type
  TTransientDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChangeHold(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FTransientProcessor : TStereoTransientProcessor;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, SysUtils;

procedure TTransientDataModule.VSTModuleOpen(Sender: TObject);
begin
 FTransientProcessor := TStereoTransientProcessor.Create;
 FTransientProcessor.SampleRate := SampleRate;

 // Initial Parameters
 Parameter[0] := 0;   // Attack [%]
 Parameter[1] := 0;   // Release [%]
 Parameter[2] := 0;   // Output Gain [dB]
 Parameter[3] := -1;  // Filter [%]
 Parameter[4] := 35;  // Att-rel [%]
 Parameter[5] := 35;  // Rel-att [%]
end;

procedure TTransientDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FTransientProcessor);
end;

procedure TTransientDataModule.ParameterOutputChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FTransientProcessor)
  then FTransientProcessor.Output := Value;
end;

procedure TTransientDataModule.ParameterAttackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FTransientProcessor)
  then FTransientProcessor.Attack := Value;
end;

procedure TTransientDataModule.ParameterReleaseChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FTransientProcessor)
  then FTransientProcessor.Release := Value;
end;

procedure TTransientDataModule.ParameterAttackChangeHold(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FTransientProcessor)
  then FTransientProcessor.AttackHold := Value;
end;

procedure TTransientDataModule.ParameterReleaseChangeHold(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FTransientProcessor)
  then FTransientProcessor.ReleaseHold := Value;
end;

procedure TTransientDataModule.ParameterFilterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5 then PreDefined := 'Lowpass: ' + PreDefined else
 if Parameter[Index] > 0.5 then PreDefined := 'Highpass: ' + PreDefined;
end;

procedure TTransientDataModule.ParameterFilterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(FTransientProcessor)
  then FTransientProcessor.Filter := Value;
end;

procedure TTransientDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample      : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   FTransientProcessor.Process(Inputs[0, Sample], Inputs[1, Sample],
     Outputs[0, Sample], Outputs[1, Sample]);
  end;

(*
 // catch denormals
 if (FEnv[0] < 1E-10) then
  begin
   FEnv[0] := 0;
   FEnv[1] := 0;
   FEnv[2] := 0;
   FEnv[3] := 0;
   FState[0] := 0;
   FState[1] := 0;
  end;
*)
end;

end.
