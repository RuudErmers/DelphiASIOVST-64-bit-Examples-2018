unit DAV_DspLMS;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes;

type
  TLMS = class(TDspPersistent)
  private
    FCoefficients : PDAVDoubleFixedArray;
    FHistory      : PDAVDoubleFixedArray;
    FStepSize     : Double;
    FOrder        : Integer;
    FBufferPos    : Integer;
    procedure SetStepSize(const Value: Double);
    procedure SetOrder(const Value: Integer);
  protected
    procedure Clear; virtual;
    procedure AllocateBuffers; virtual;
    procedure StepSizeChanged; virtual;
    procedure OrderChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function ProcessSample(Input, Signal : Double): Double;
  published
    property StepSize: Double read FStepSize write SetStepSize;
    property Order: Integer read FOrder write SetOrder;
  end;

implementation

uses
  SysUtils;

{ TLMS }

constructor TLMS.Create;
begin
 inherited;
 FOrder := 8;
 FStepSize := 0;
 FStepSize := 1;
 FCoefficients := nil;
 FHistory := nil;

 AllocateBuffers;
 Clear;
end;

destructor TLMS.Destroy;
begin
 Dispose(FCoefficients);
 Dispose(FHistory);

 inherited;
end;

procedure TLMS.AllocateBuffers;
begin
 ReallocMem(FCoefficients, FOrder * SizeOf(Double));
 ReallocMem(FHistory, FOrder * SizeOf(Double));
end;

procedure TLMS.Clear;
begin
 FillChar(FCoefficients^, FOrder * SizeOf(Double), 0);
 FillChar(FHistory^, 2 * FOrder * SizeOf(Double), 0);
end;

procedure TLMS.SetOrder(const Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TLMS.SetStepSize(const Value: Double);
begin
 if FStepSize <> Value then
  begin
   FStepSize := Value;
   StepSizeChanged;
  end;
end;

procedure TLMS.OrderChanged;
begin
 AllocateBuffers;
 Clear;
 Changed;
end;

procedure TLMS.StepSizeChanged;
begin
 Changed;
end;

function TLMS.ProcessSample(Input, Signal: Double): Double;
var
  RMS      : Double;
  InvRMS   : Double;
  Coef     : Integer;
  Residual : Double;
  CurPos   : Integer;
begin
  CurPos := FBufferPos;
  FHistory[CurPos] := Input;
  Result := Input * FCoefficients[0];
  RMS := 1E-9 + Sqr(FHistory[CurPos]);
  for Coef := 1 to FOrder - 1 do
   begin
    Inc(CurPos);
    if CurPos >= FOrder then CurPos := 0;
    Result := Result + FHistory[CurPos] * FCoefficients[Coef];
    RMS := RMS + Sqr(FHistory[CurPos]);
   end;

  Residual := Signal - Result;
  InvRMS := FStepSize / RMS;

  CurPos := FBufferPos;
  for Coef := 1 to FOrder - 1 do
   begin
    Inc(CurPos);
    if CurPos >= FOrder then CurPos := 0;
    FCoefficients[Coef] := FCoefficients[Coef] + InvRMS * Residual * FHistory[CurPos];
   end;

  Dec(FBufferPos);
  if FBufferPos < 0 then FBufferPos := FOrder - 1;
end;

end.
