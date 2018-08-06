unit DAV_DspPolyphaseIirDesigner;

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
//  The code is based on the HIIR code by Laurent de Soras, which             //
//  can be found at http://ldesoras.free.fr/prod.html#src_hiir                //
//  It was reviewed and rewritten from scratch by Christian-W. Budde          //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Compute Coeffs for 2-path polyphase IIR filter, half-band filter or       //
//  Pi/2 phaser.                                                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types;

type
  TPolyphaseIirDesigner = class
  private
    class procedure ComputeTransitionParam(out K, Q: Double; const Transition: Double);
    class function ComputeOrder(const Attenuation, Q: Double): Integer;
    class function ComputeAttenuation(const Q: Double; const Order: Integer): Double;
    class function ComputeCoefficient(const Index: Integer; const K, Q: Double; const Order: Integer): Double;
    class function ComputeACCNum(const q: Double; const Order, c: Integer): Double;
    class function ComputeACCDen(const q: Double; const Order, c: Integer): Double;
  public
    class function ComputeNbrOfCoeffsFromPrototype(const Attenuation, Transition: Double): Integer;
    class function ComputeAttenuationFromOrderTBW(const NbrOfCoeffs: Integer; const Transition: Double): Double;
    class function ComputeCoeffs(Coeffs: TDAVDoubleDynArray; const Attenuation, Transition: Double): Integer;
    class procedure ComputeCoeffsSpecOrderTBW(const Coeffs: TDAVDoubleDynArray; const NbrOfCoeffs: Integer; const Transition: Double);
  end;

implementation

uses
  Math;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ComputeNbrOfCoeffsFromPrototype                                     //
//  -------------------------------------                                     //
//                                                                            //
//  Description: Finds the minimum Nbr of Coeffs for a given filter           //
//  specification                                                             //
//                                                                            //
//  Input parameters:                                                         //
//    - Attenuation: stopband attenuation [dB]. > 0.                          //
//    - Transition: normalized transition bandwith. Range: ]0 ; 1/2[          //
//                                                                            //
//  Returns: Nbr of Coeffs > 0                                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TPolyphaseIirDesigner.ComputeNbrOfCoeffsFromPrototype(
  const Attenuation, Transition: Double): Integer;
var
  k, q: Double;
begin
 Assert(Attenuation > 0);
 Assert(Transition > 0);
 Assert(Transition < 0.5);
 ComputeTransitionParam(k, q, Transition);
 Result := (ComputeOrder(Attenuation, q) - 1) div 2;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ComputeAttenuationFromOrderTBW                                      //
//  ------------------------------------                                      //
//                                                                            //
//  Description:                                                              //
//    Compute the attenuation correspounding to a given Nbr of Coeffs         //
//    and the transition bandwith.                                            //
//                                                                            //
//  Input parameters:                                                         //
//    - NbrOfCoeffs: Nbr of desired Coeffs. > 0.                              //
//    - Transition: normalized transition bandwith. Range: ]0 ; 1/2[          //
//                                                                            //
//  Returns: stopband attenuation [dB] > 0.                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TPolyphaseIirDesigner.ComputeAttenuationFromOrderTBW(
  const NbrOfCoeffs: Integer; const Transition: Double): Double;
var
  k, q: Double;
begin
 Assert(NbrOfCoeffs > 0);
 Assert(Transition > 0);
 Assert(Transition < 0.5);
 ComputeTransitionParam(k, q, transition);
 Result := ComputeAttenuation(q, NbrOfCoeffs * 2 + 1);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ComputeCoefs                                                        //
//  ------------------                                                        //
//                                                                            //
//  Description:                                                              //
//    Computes Coeffs for a half-band polyphase IIR filter, function of a     //
//    given stopband gain / transition bandwidth specification.               //
//    Order is automatically calculated.                                      //
//                                                                            //
//  Input parameters:                                                         //
//    - attenuation: stopband attenuation [dB]. > 0.                          //
//    - transition: normalized transition bandwith. Range: ]0..1/2[           //
//                                                                            //
//  Output parameters:                                                        //
//    - Coeffs: Coefficient list, must be large enough to store all the       //
//      Coeffs. Filter Order = NbrOfCoeffs * 2 + 1                            //
//                                                                            //
//  Returns: Nbr of Coeffs                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TPolyphaseIirDesigner.ComputeCoeffs(Coeffs: TDAVDoubleDynArray;
  const Attenuation: Double; const Transition: Double): Integer;
var
  k, q     : Double;
  Order, i : Integer;
begin
 Assert(Length(Coeffs) > 0);
 Assert(Attenuation > 0);
 Assert(Transition > 0);
 Assert(Transition < 0.5);

 ComputeTransitionParam(k, q, transition);

 // Computes Nbr of required Coeffs
 Order := ComputeOrder(Attenuation, q);
 Result := (Order - 1) div 2;

 // Coefficient calculation
 for i := 0 to Result - 1
  do Coeffs[i] := ComputeCoefficient(i, k, q, Order);
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Name: ComputeCoeffsSpecOrderTBW                                           //
//  -------------------------------                                           //
//                                                                            //
//  Description:                                                              //
//    Computes Coeffs for a half-band polyphase IIR filter, function of a     //
//    given transition bandwidth and desired filter Order. Bandstop           //
//    attenuation is set to the maximum value for these constraints.          //
//                                                                            //
//  Input parameters:                                                         //
//    - NbrOfCoeffs: Nbr of desired Coeffs. > 0.                              //
//    - Transition: normalized transition bandwith. Range ]0 ; 1/2[           //
//                                                                            //
//  Output parameters:                                                        //
//    - Coeffs: Coefficient list, must be large enough to store all the       //
//      Coeffs.                                                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class procedure TPolyphaseIirDesigner.ComputeCoeffsSpecOrderTBW(
  const Coeffs: TDAVDoubleDynArray; const NbrOfCoeffs: Integer;
  const Transition: Double);
var
  k, q     : Double;
  Order, i : Integer;
begin
 Assert(Length(Coeffs) > 0);
 Assert(NbrOfCoeffs > 0);
 Assert(Transition > 0);
 Assert(Transition < 0.5);
 ComputeTransitionParam(k, q, Transition);

 Order := NbrOfCoeffs * 2 + 1;

// Coefficient calculation
 for i := 0 to NbrOfCoeffs - 1
  do Coeffs[i] := ComputeCoefficient(i, k, q, Order);
end;

class procedure TPolyphaseIirDesigner.ComputeTransitionParam(
  out K, Q: Double; const Transition: Double);
var
  kksqrt : Double;
  e, e4  : Double;
begin
 Assert(Transition > 0);
 Assert(Transition < 0.5);

// k := (1 - Sin(Pi * Transition)) / (1 + Sin(Pi * Transition));
 k := Sqr(Tan((1 - Transition * 2) * PI * 0.25));
 Assert(k < 1);
 Assert(k > 0);

 kksqrt := Power(1 - Sqr(k), 0.25);
 e := 0.5 * (1 - kksqrt) / (1 + kksqrt);
 e4 := Sqr(Sqr(e));
 q := e * (1 + e4 * (2 + e4 * (15 + 150 * e4)));
 Assert(q > 0);
end;

class function TPolyphaseIirDesigner.ComputeOrder(
  const Attenuation, Q: Double): Integer;
var
  a, attn_p2: Double;
begin
 Assert(Attenuation > 0);
 Assert(Q > 0);
 attn_p2 := Power(-0.1 * Attenuation, 10.0);
 a := attn_p2 / (1 - attn_p2);
 Result := Round(Ln(Sqr(a) / 16) / ln(Q) + 0.5);

 if (Result and 1) = 0
  then Inc(Result);
 if Result = 1
  then Result := 3;
end;

class function TPolyphaseIirDesigner.ComputeAttenuation(const Q: Double;
  const Order: Integer): Double;
begin
 Assert(Q > 0);
 Assert(Order > 0);
 Assert((Order and 1) = 1);

 Result := 4 * Exp(Order * 0.5 * ln(Q));
 Assert(Result <> -1.0);
 Result := -10 * log10(Result / (1 + Result));
 Assert(Result > 0);
end;

class function TPolyphaseIirDesigner.ComputeCoefficient(const Index: Integer;
  const K, Q: Double; const Order: Integer): Double;
var
  c        : Integer;
  Num, Den : Double;
  ww       : Double;
begin
 Assert(Index >= 0);
 Assert(Index * 2 < Order);
 c := Index + 1;
 Num := ComputeACCNum(Q, Order, c) * Power(q, 0.25);
 Den := ComputeACCDen(Q, Order, c) + 0.5;
 ww := Sqr(Num) / Sqr(Den);

 Result := Sqrt((1 - ww * k) * (1 - ww / k)) / (1 + ww);
 Result := (1 - Result) / (1 + Result);
end;

class function TPolyphaseIirDesigner.ComputeACCNum(const Q: Double;
  const Order, C: Integer): Double;
var
  q_i1 : Double;
  i, j : Integer;
begin
 Assert(c >= 1);
 Assert(c < Order * 2);
 i := 0;
 j := 1;
 Result := 0;
 repeat
  q_i1 := IntPower(q, i * (i + 1)) * (sin((i * 2 + 1) * c * PI / Order) * j);
  Result := Result + q_i1;
  j := -j;
  Inc(i);
 until abs(q_i1) <= 1e-100;
end;

class function TPolyphaseIirDesigner.ComputeACCDen(const Q: Double;
  const Order, C: Integer): Double;
var
  q_i2 : Double;
  i, j : Integer;
begin
 Assert(c >= 1);
 Assert(c < Order * 2);
 i := 1;
 j := -1;
 Result := 0;

 repeat
  q_i2 := IntPower(q, i * i) * cos(2 * i * c * PI / Order) * j;
  Result := Result + q_i2;
  j := -j;
  Inc(i);
 until abs(q_i2) <= 1e-100;
end;

end.
