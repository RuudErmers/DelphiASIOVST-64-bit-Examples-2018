unit DAV_DspFilterChebyshev;

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
  Classes, DAV_Types, DAV_Complex, DAV_DspFilter;

type
  TCustomChebyshevFilterClass = class of TCustomChebyshevFilter;
  TCustomChebyshevFilter = class(TCustomOrderFilter)
  protected
    FHypFactors   : TDAV2DoubleArray;
    FFilterGain   : Double;
    FTanW0Half    : Double;
    FOrderInv     : Double;
    FExpOrdPiHalf : TComplex64;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateW0; override;
    procedure CalculateHypFactors; virtual; abstract;
    procedure CalculateExpOrdPiHalf; virtual;
    procedure OrderChanged; override;
  public
    constructor Create(const Order: Integer = 0); override;
    procedure ResetStatesInt64; override;
    function Real(const Frequency: Double): Double; override;
    function Imaginary(const Frequency: Double): Double; override;
  end;

implementation

{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
  Math, DAV_Common, DAV_Math;

{ TCustomChebyshevFilter }

constructor TCustomChebyshevFilter.Create(const Order: Integer = 0);
begin
 FOrder  := Order;
 OrderChanged;
 inherited Create(Order);
end;

procedure TCustomChebyshevFilter.CalculateW0;
begin
 // inherited; FTanW0Half := FExpW0.Im / (1 + FExpW0.Re);
 FW0 := Pi * FSRR * FFrequency;
 FTanW0Half := tan(FW0);
end;

function TCustomChebyshevFilter.Imaginary(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, Temp, result);
end;

procedure TCustomChebyshevFilter.OrderChanged;
begin
 if FOrder > 0 then
  begin
   FOrderInv := 1 / FOrder;
   CalculateExpOrdPiHalf;
   CalculateHypFactors;
   ResetStates;
   inherited;
  end
 else
  begin
   FOrderInv := 1;
   FFilterGain := 1;
  end;
 Changed; 
end;

procedure TCustomChebyshevFilter.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChebyshevFilter then
  with TCustomChebyshevFilter(Dest) do
   begin
    inherited;
    FHypFactors   := Self.FHypFactors;
    FFilterGain   := Self.FFilterGain;
    FTanW0Half    := Self.FTanW0Half;
    FOrderInv     := Self.FOrderInv;
    FExpOrdPiHalf := Self.FExpOrdPiHalf;
   end
 else inherited;
end;

procedure TCustomChebyshevFilter.CalculateExpOrdPiHalf;
begin
 GetSinCos(Pi * CHalf64 * FOrderInv, FExpOrdPiHalf.Im, FExpOrdPiHalf.Re);
end;

function TCustomChebyshevFilter.Real(const Frequency: Double): Double;
var
  Temp : Double;
begin
 Complex(Frequency, result, Temp);
end;

procedure TCustomChebyshevFilter.ResetStatesInt64;
begin
 inherited;
 ResetStates;
end;

end.
