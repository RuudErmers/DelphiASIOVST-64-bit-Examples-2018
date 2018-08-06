unit DAV_DspLFO;

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
//  Some parts of this unit are inspired by code by Didier Dambrin            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF FPC}{$DEFINE PUREPASCAL}{$ENDIF}

uses
  Classes, DAV_Classes, DAV_DspSimpleOscillator;

type
  TLFOSine32 = TSimpleOscillator32;
  TLFOSine64 = TSimpleOscillator64;
  TLFOSine = TSimpleOscillator64;

  // TODO: REVIEW SAMPLERATE DEPENDENCY!!!

  TCustomLFOSineLike = class(TCustomOscillator)
  protected
    FIntSpeed  : Integer;
    FSpeed     : Single;
    FMax, FMin : Single;
    FValue     : Single;
    FPos       : Integer;
    FScale     : Single;
    FPosMul    : Single;
    FHalfScale : Single;
    function GetValue: Single; virtual;
    procedure SetMin(const Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetSpeed(const Value: Single);
  public
    constructor Create; override;
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TCustomLFOTriangleLike = class(TCustomOscillator)
  protected
    FIntSpeed  : Integer;
    FSpeed     : Single;
    FMax, FMin : Single;
    FValue     : Single;
    FPos       : Integer;
    FScale     : Single;
    FPosMul    : Single;
    FHalfScale : Single;
    function GetValue: Single; virtual;
    procedure SetMin(const Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetSpeed(const Value: Single);
  public
    constructor Create; override;
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TLFOSineLike = class(TCustomLFOSineLike)
  published
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

  TLFOTriangleLike = class(TCustomLFOTriangleLike)
  published
    property Value: Single read GetValue;
    property Speed: Single read FSpeed write SetSpeed;
    property Min: Single read FMin write SetMin;
    property Max: Single read FMax write SetMax;
  end;

implementation

uses
  Math;

{ TCustomLFOSineLike }

constructor TCustomLFOSineLike.Create;
begin
  inherited;
  FMax   := 1;
  FMin   := 0;
  FValue := 1;
  FPos   := 0;
  Speed  := 100;
  FScale := FMax - ((FMin + FMax) * 0.5);
  FPosMul := Sqrt(FScale * 2) / $80000000;
  FHalfScale := Sqrt(FScale * 2) * 0.5;
end;

procedure TCustomLFOSineLike.SetMin(const Value: Single);
begin
 if FMin <> Value then
  begin
   FMin := Value;
   FScale := FMax - ((FMin + FMax) * 0.5);
  end;
end;

procedure TCustomLFOSineLike.SetMax(const Value: Single);
begin
 if FMax <> Value then
  begin
   FMax := Value;
   FScale := FMax - ((FMin + FMax) * 0.5);
  end;
end;

procedure TCustomLFOSineLike.SetSpeed(const Value: Single);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   FIntSpeed := Round($100000000 / FSpeed);
  end;
end;

function TCustomLFOSineLike.GetValue: Single;
begin
 Result := Abs(FPos * FPosMul) - FHalfScale;
 Result := Result * (FHalfScale * 2 - Abs(Result)) * 2;
 Result := Result + (FMin + FMax) * 0.5;
 FPos := FPos + FIntSpeed;
end;


{ TCustomLFOTriangleLike }

constructor TCustomLFOTriangleLike.Create;
begin
  inherited;
  FMax := 1;
  FMin := 0;
  FValue := 1;
  FPos := 0;
  Speed := 100;
  FScale := FMax - (FMin + FMax) * 0.5;
  FPosMul := FScale / $80000000;
  FHalfScale := Sqrt(FScale * 2) * 0.5;
end;

procedure TCustomLFOTriangleLike.SetMin(const Value: Single);
begin
 if FMin <> Value then
  begin
   FMin := Value;
   FScale := FMax - (FMin + FMax) * 0.5;
  end;
end;

procedure TCustomLFOTriangleLike.SetMax(const Value: Single);
begin
 if FMax <> Value then
  begin
   FMax := Value;
   FScale := FMax - (FMin + FMax) * 0.5;
  end;
end;

procedure TCustomLFOTriangleLike.SetSpeed(const Value: Single);
begin
 if FSpeed <> Value then
  begin
   FSpeed := Value;
   FIntSpeed := Round($100000000 / FSpeed);
  end;
end;

function TCustomLFOTriangleLike.GetValue: Single;
begin
  Result := Abs(FPos * (2 * FPosMul)) + FMin;
  FPos := FPos + FIntSpeed;
end;

end.
