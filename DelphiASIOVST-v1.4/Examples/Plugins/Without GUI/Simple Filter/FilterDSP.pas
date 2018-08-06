unit FilterDSP;

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
  DAV_Types, DAV_VSTModule;

type
  TVSTFilter = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTFilterParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCutOffFrequency : Single;
    FOld             : array [0..1, 0..1] of Double;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common;

////////////////////////////////////////////////////////////////////////////////
// OnOpen
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleOpen(Sender: TObject);
begin
 FCutOffFrequency := 0.5;

 {$IFDEF FPC}
 OnProcess := VSTModuleProcess;
 OnProcess32Replacing := VSTModuleProcess;
 OnProcess64Replacing := VSTModuleProcessDoubleReplacing;
 {$ENDIF}

 Parameter[0] := 1000;
 Parameter[1] := 1;
end;

////////////////////////////////////////////////////////////////////////////////
// Parameter 0 Changed (Cutoff Frequency)
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTFilterParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCutOffFrequency := 0.01 + Value * 0.00005;
end;


////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i         : Integer;
  cut, res  : Single;
  fb        : Single;
begin
 cut := FCutOffFrequency;
 res := 0.1 * Parameter[1];
 fb := res + res / (1 - cut * 0.9);
 for i := 0 to SampleFrames - 1 do
  begin
   FOld[0, 0] := FOld[0, 0] + cut * (Inputs[0, i] - FOld[0, 0] + fb * (FOld[0, 0] - FOld[1, 0])) + CDenorm32;
   FOld[0, 1] := FOld[0, 1] + cut * (Inputs[1, i] - FOld[0, 1] + fb * (FOld[0, 1] - FOld[1, 1])) + CDenorm32;
   FOld[1, 0] := FOld[1, 0] + cut * (FOld[0, 0] - FOld[1, 0]);
   FOld[1, 1] := FOld[1, 1] + cut * (FOld[0, 1] - FOld[1, 1]);
   Outputs[0, i] := Limit(FOld[1, 0]);
   Outputs[1, i] := Limit(FOld[1, 1]);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTFilter.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  i         : Integer;
  cut, res  : Double;
  fb        : Double;
begin
 cut := Parameter[0] * 0.8;
 res := Parameter[1];
 fb := res + res / (1 - Cut * 0.9);
 for i := 0 to SampleFrames - 1 do
  begin
   FOld[0, 0] := FOld[0, 0] + cut * (Inputs[0, i] - FOld[0, 0] + fb * (FOld[0, 0] - FOld[1, 0])) + CDenorm32;
   FOld[0, 1] := FOld[0, 1] + cut * (Inputs[1, i] - FOld[0, 1] + fb * (FOld[0, 1] - FOld[1, 1])) + CDenorm32;
   FOld[1, 0] := FOld[1, 0] + cut * (FOld[0, 0] - FOld[1, 0]);
   FOld[1, 1] := FOld[1, 1] + cut * (FOld[0, 1] - FOld[1, 1]);
   Outputs[0, i] := Limit(FOld[1,0]);
   Outputs[1, i] := Limit(FOld[1,1]);
  end;
end;

end.
