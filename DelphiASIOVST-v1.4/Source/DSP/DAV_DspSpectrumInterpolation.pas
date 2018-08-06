unit DAV_DspSpectrumInterpolation;

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

{$I ..\DAV_Compiler.inc}

uses
  DAV_Types, DAV_Complex;

function QuadraticPeakPosition(const Data: TDAV3SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuadraticPeakPosition(const Data: TDAV3DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BarycentricPeakPosition(const Data: TDAV3SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function BarycentricPeakPosition(const Data: TDAV3DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator1(const Prev, Peak, Next: TComplex32): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator1(const Prev, Peak, Next: TComplex64): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator1(const Data: PDAV3ComplexSingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator1(const Data: PDAV3ComplexDoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator2(const Prev, Peak, Next: TComplex32): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator2(const Prev, Peak, Next: TComplex64): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator2(const Data: PDAV3ComplexSingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function QuinnEstimator2(const Data: PDAV3ComplexDoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function JainEstimator(const Prev, Peak, Next: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function JainEstimator(const Prev, Peak, Next: Double): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
function JainEstimator(const Data: TDAV3SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
function JainEstimator(const Data: TDAV3DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}

implementation

uses
  Math;

function QuadraticPeakPosition(const Data: TDAV3SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
begin
 Result := (abs(Data[2]) - abs(Data[0])) / (2 * (2 * abs(Data[1]) - abs(Data[0]) - abs(Data[2])));
end;

function QuadraticPeakPosition(const Data: TDAV3DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
begin
 Result := (abs(Data[2]) - abs(Data[0])) / (2 * (2 * abs(Data[1]) - abs(Data[0]) - abs(Data[2])));
end;

function BarycentricPeakPosition(const Data: TDAV3SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
begin
 Result := (abs(Data[2]) - abs(Data[0])) / (abs(Data[0]) + abs(Data[1]) + abs(Data[2]))
end;

function BarycentricPeakPosition(const Data: TDAV3DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
begin
 Result := (abs(Data[2]) - abs(Data[0])) / (abs(Data[0]) + abs(Data[1]) + abs(Data[2]))
end;

function QuinnEstimator1(const Prev, Peak, Next: TComplex32): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Single;
  ap, dp, am, dm   : Single;
begin
 InvPeakMagnitude := 1  / (Sqr(Peak.Re) + Sqr(Peak.Im));
 ap := (Next.Re * Peak.Re + Next.Im * Peak.Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Prev.Re * Peak.Re + Prev.Im * Peak.Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 if (dp > 0) and (dm > 0)
  then Result := dp
  else Result := dm;
end;

function QuinnEstimator1(const Prev, Peak, Next: TComplex64): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Double;
  ap, dp, am, dm   : Double;
begin
 InvPeakMagnitude := 1  / (Sqr(Peak.Re) + Sqr(Peak.Im));
 ap := (Next.Re * Peak.Re + Next.Im * Peak.Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Prev.Re * Peak.Re + Prev.Im * Peak.Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 if (dp > 0) and (dm > 0)
  then Result := dp
  else Result := dm;
end;

function QuinnEstimator1(const Data: PDAV3ComplexSingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Single;
  ap, dp, am, dm   : Single;
begin
 InvPeakMagnitude := 1  / (Sqr(Data^[1].Re) + Sqr(Data^[1].Im));
 ap := (Data^[2].Re * Data^[1].Re + Data^[2].Im * Data^[1].Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Data^[0].Re * Data^[1].Re + Data^[0].Im * Data^[1].Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 if (dp > 0) and (dm > 0)
  then Result := dp
  else Result := dm;
end;

function QuinnEstimator1(const Data: PDAV3ComplexDoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Double;
  ap, dp, am, dm   : Double;
begin
 InvPeakMagnitude := 1  / (Sqr(Data^[1].Re) + Sqr(Data^[1].Im));
 ap := (Data^[2].Re * Data^[1].Re + Data^[2].Im * Data^[1].Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Data^[0].Re * Data^[1].Re + Data^[0].Im * Data^[1].Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 if (dp > 0) and (dm > 0)
  then Result := dp
  else Result := dm;
end;


function Tau(x: Double): Double;
const
  CSqrt6Div24 : Double = 0.10206207261596575409155350311271;
  CSqrt2Div3 : Double = 0.81649658092772603273242802490196;
begin
 Result := Sqr(0.25 * Log10(3 * Sqr(x) + 6 * x + 1) -
   CSqrt6Div24 * Log10((x + 1 - CSqrt2Div3)  /  (x + 1 + CSqrt2Div3)));
end;

function QuinnEstimator2(const Prev, Peak, Next: TComplex32): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Single;
  ap, dp, am, dm   : Single;
begin
 InvPeakMagnitude := 1  / (Sqr(Peak.Re) + Sqr(Peak.Im));
 ap := (Next.Re * Peak.Re + Next.Im * Peak.Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Prev.Re * Peak.Re + Prev.Im * Peak.Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 Result := (dp + dm) * 0.5 + tau(dp) - tau(dm);
end;

function QuinnEstimator2(const Prev, Peak, Next: TComplex64): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Single;
  ap, dp, am, dm   : Single;
begin
 InvPeakMagnitude := 1  / (Sqr(Peak.Re) + Sqr(Peak.Im));
 ap := (Next.Re * Peak.Re + Next.Im * Peak.Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Prev.Re * Peak.Re + Prev.Im * Peak.Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 Result := (dp + dm) * 0.5 + tau(dp) - tau(dm);
end;

function QuinnEstimator2(const Data: PDAV3ComplexSingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Double;
  ap, dp, am, dm   : Double;
begin
 InvPeakMagnitude := 1  / (Sqr(Data^[1].Re) + Sqr(Data^[1].Im));
 ap := (Data^[2].Re * Data^[1].Re + Data^[2].Im * Data^[1].Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Data^[0].Re * Data^[1].Re + Data^[0].Im * Data^[1].Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 Result := (dp + dm) * 0.5 + tau(dp) - tau(dm)
end;

function QuinnEstimator2(const Data: PDAV3ComplexDoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  InvPeakMagnitude : Double;
  ap, dp, am, dm   : Double;
begin
 InvPeakMagnitude := 1  / (Sqr(Data^[1].Re) + Sqr(Data^[1].Im));
 ap := (Data^[2].Re * Data^[1].Re + Data^[2].Im * Data^[1].Im) * InvPeakMagnitude;
 dp := -ap  / (1 - ap);
 am := (Data^[0].Re * Data^[1].Re + Data^[0].Im * Data^[1].Im) * InvPeakMagnitude;
 dm := am / (1 - am);
 Result := (dp + dm) * 0.5 + tau(dp) - tau(dm);
end;

function JainEstimator(const Prev, Peak, Next: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  Temp : Single;
begin
 if Abs(Prev) > Abs(Next) then
  begin
   Temp := Abs(Peak) / Abs(Prev);
   Result := Temp / (1 + Temp);
  end
 else
  begin
   Temp := Abs(Next) / Abs(Peak);
   Result := Temp / (1 + Temp);
  end;
end;

function JainEstimator(const Prev, Peak, Next: Double): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  Temp : Double;
begin
 if Abs(Prev) > Abs(Next) then
  begin
   Temp := Abs(Peak) / Abs(Prev);
   Result := Temp / (1 + Temp);
  end
 else
  begin
   Temp := Abs(Next) / Abs(Peak);
   Result := Temp / (1 + Temp);
  end;
end;

function JainEstimator(const Data: TDAV3SingleArray): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  Temp : Single;
begin
 if Abs(Data[0]) > Abs(Data[2]) then
  begin
   Temp := Abs(Data[1]) / Abs(Data[0]);
   Result := Temp / (1 + Temp);
  end
 else
  begin
   Temp := Abs(Data[2]) / Abs(Data[1]);
   Result := Temp / (1 + Temp);
  end;
end;

function JainEstimator(const Data: TDAV3DoubleArray): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
var
  Temp : Double;
begin
 if Abs(Data[0]) > Abs(Data[2]) then
  begin
   Temp := Abs(Data[1]) / Abs(Data[0]);
   Result := Temp / (1 + Temp);
  end
 else
  begin
   Temp := Abs(Data[2]) / Abs(Data[1]);
   Result := Temp / (1 + Temp);
  end;
end;

end.
