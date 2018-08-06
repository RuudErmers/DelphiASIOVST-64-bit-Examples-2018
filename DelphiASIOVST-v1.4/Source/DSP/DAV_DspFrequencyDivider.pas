unit DAV_DspFrequencyDivider;

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

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TOcatveDivider = class(TDspPersistent, IDspProcessor32, IDspProcessor64)
  private
    procedure SetShape(Value: Single);
  protected
    FPhaseSign    : Single;
    FPhaseFactor  : Single;
    FShape        : array [0..1] of Single;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;

    function ProcessSample32(Input: Single): Single; overload;
    function ProcessSample64(Input: Double): Double; overload;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVDoubleFixedArray; SampleCount: Integer);

    procedure Reset;

    property Phase: Single read FPhaseFactor;
  published
    property Shape: Single read FShape[0] write SetShape;
  end;

implementation

uses
  DAV_Common;

{ TOcatveDivider }

constructor TOcatveDivider.Create;
begin
 FPhaseSign   := 1;
 FPhaseFactor := 1;
 FShape[0]    := 0.5;
 FShape[1]    := 0.5;
end;

procedure TOcatveDivider.AssignTo(Dest: TPersistent);
begin
 if Dest is TOcatveDivider then
  with TOcatveDivider(Dest) do
   begin
    inherited;
    FPhaseSign   := Self.FPhaseSign;
    FPhaseFactor := Self.FPhaseFactor;
    FShape       := Self.FShape;
   end
 else inherited;
end;

procedure TOcatveDivider.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TOcatveDivider.ProcessBlock64(const Data: PDAVDoubleFixedArray;
  SampleCount: Integer);
var
  Sample: Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TOcatveDivider.ProcessSample32(Input: Single): Single;
begin
 if Input * FPhaseSign < 0 then     // Octave Divider
  begin
   FPhaseSign := -FPhaseSign;
   if FPhaseSign < 0 then FPhaseFactor := -FPhaseFactor;
  end;
 Result := FPhaseFactor * (FShape[0] + FShape[1] * Input);
end;

function TOcatveDivider.ProcessSample64(Input: Double): Double;
begin
 if Input * FPhaseSign < 0 then
  begin
   FPhaseSign := -FPhaseSign;
   if FPhaseSign < 0 then FPhaseFactor := -FPhaseFactor;
  end;
 Result := FPhaseFactor * (FShape[0] + FShape[1] * Input);
end;

procedure TOcatveDivider.Reset;
begin
 FPhaseSign   := 1;
 FPhaseFactor := 1;
end;

procedure TOcatveDivider.SetShape(Value: Single);
begin
 Value := Limit(Value, 0, 1);
 if FShape[0] <> Value then
  begin
   FShape[0] := Value;
   FShape[1] := 1 - Value;
   Changed;
  end;
end;

initialization
  RegisterDspProcessor32(TOcatveDivider);
  RegisterDspProcessor64(TOcatveDivider);

end.
