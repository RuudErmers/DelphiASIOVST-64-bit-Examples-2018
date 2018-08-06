{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_BesselFunctions;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types;

function ModifiedBesselFirstKindOrderZero(Value: Double): Double;
function ModifiedBesselFirstKindOrderOne(Value: Double): Double;

function BesselFirstKindOrderZero(Value: Double): Double;
function BesselFirstKindOrderOne(Value: Double): Double;

implementation

uses
  DAV_Math, DAV_Complex;

// Modified Bessel function of the first kind of order zero
// minimax rational approximations on intervals, see
// Blair and Edwards, Chalk River Report AECL-4928, 1974

function ModifiedBesselFirstKindOrderZero(Value: Double): Double;
const
  P1: array [0 .. 14] of Double = (-2.2335582639474375249E+15,
    -5.5050369673018427753E+14, -3.2940087627407749166E+13,
    -8.4925101247114157499E+11, -1.1912746104985237192E+10,
    -1.0313066708737980747E+08, -5.9545626019847898221E+05,
    -2.4125195876041896775E+03, -7.0935347449210549190E+00,
    -1.5453977791786851041E-02, -2.5172644670688975051E-05,
    -3.0517226450451067446E-08, -2.6843448573468483278E-11,
    -1.5982226675653184646E-14, -5.2487866627945699800E-18);

  Q1: array [0 .. 5] of Double = (-2.2335582639474375245E+15,
    7.8858692566751002988E+12, -1.2207067397808979846E+10,
    1.0377081058062166144E+07, -4.8527560179962773045E+03, 1.0);

  P2: array [0 .. 6] of Double = (-2.2210262233306573296E-04,
    1.3067392038106924055E-02, -4.4700805721174453923E-01,
    5.5674518371240761397, -2.3517945679239481621E+01,
    3.1611322818701131207E+01, -9.609002196865618);

  Q2: array [0 .. 7] of Double = (-5.5194330231005480228E-04,
    3.2547697594819615062E-02, -1.1151759188741312645,
    1.3982595353892851542E+01, -6.0228002066743340583E+01,
    8.5539563258012929600E+01, -3.1446690275135491500E+01, 1);
var
  Temp: Double;
begin
  // even function
  Value := Abs(Value);

  if Value = 0 then
    Result := 1
  else if Value <= 15 then
    Result := EvaluatePolynomial(P1, Sqr(Value)) / EvaluatePolynomial(Q1,
      Sqr(Value))
  else
  begin
    Temp := 1 / Value - 1 / 15;
    Result := exp(Value) / sqrt(Value) * EvaluatePolynomial(P2, Temp) /
      EvaluatePolynomial(Q2, Temp);
  end;
end;

function ModifiedBesselFirstKindOrderOne(Value: Double): Double;
const
  P1: array [0 .. 14] of Double = (-1.4577180278143463643E+15,
    -1.7732037840791591320E+14, -6.9876779648010090070E+12,
    -1.3357437682275493024E+11, -1.4828267606612366099E+09,
    -1.0588550724769347106E+07, -5.1894091982308017540E+04,
    -1.8225946631657315931E+02, -4.7207090827310162436E-01,
    -9.1746443287817501309E-04, -1.3466829827635152875E-06,
    -1.4831904935994647675E-09, -1.1928788903603238754E-12,
    -6.5245515583151902910E-16, -1.9705291802535139930E-19);

  Q1: array [0 .. 5] of Double = (-2.9154360556286927285E+15,
    9.7887501377547640438E+12, -1.4386907088588283434E+10,
    1.1594225856856884006E+07, -5.1326864679904189920E+03, 1.0);

  P2: array [0 .. 7] of Double = (1.4582087408985668208E-05,
    -8.9359825138577646443E-04, 2.9204895411257790122E-02,
    -3.4198728018058047439E-01, 1.3960118277609544334, -1.9746376087200685843,
    8.5591872901933459000E-01, -6.0437159056137599999E-02);

  Q2: array [0 .. 6] of Double = (3.7510433111922824643E-05,
    -2.2835624489492512649E-03, 7.4212010813186530069E-02,
    -8.5017476463217924408E-01, 3.2593714889036996297,
    -3.880658672155659345, 1);
var
  Temp: Double;
begin
  if Value = 0 then
    Result := 0
  else if Abs(Value) <= 15 then
    Result := Abs(Value) * EvaluatePolynomial(P1, Sqr(Value)) /
      EvaluatePolynomial(Q1, Sqr(Value))
  else
  begin
    Temp := 1 / Abs(Value) - 1 / 15;
    Result := exp(Value) / sqrt(Value) * EvaluatePolynomial(P2, Temp) /
      EvaluatePolynomial(Q2, Temp);
  end;

  // odd function
  if Value < 0 then
    Result := -Result;
end;

// Bessel function of the first kind of order zero
// x <= 8, minimax rational approximations on root-bracketing intervals
// x > 8, Hankel asymptotic expansion in Hart, Computer Approximations, 1968

function BesselFirstKindOrderZero(Value: Double): Double;
const
  P1: array [0 .. 6] of Double = (-4.1298668500990866786E+11,
    2.7282507878605942706E+10, -6.2140700423540120665E+08,
    6.6302997904833794242E+06, -3.6629814655107086448E+04,
    1.0344222815443188943E+02, -1.2117036164593528341E-01);

  Q1: array [0 .. 6] of Double = (2.3883787996332290397E+12,
    2.6328198300859648632E+10, 1.3985097372263433271E+08,
    4.5612696224219938200E+05, 9.3614022392337710626E+02, 1, 0);

  P2: array [0 .. 7] of Double = (-1.8319397969392084011E+03,
    -1.2254078161378989535E+04, -7.2879702464464618998E+03,
    1.0341910641583726701E+04, 1.1725046279757103576E+04,
    4.4176707025325087628E+03, 7.4321196680624245801E+02,
    4.8591703355916499363E+01);

  Q2: array [0 .. 7] of Double = (-3.5783478026152301072E+05,
    2.4599102262586308984E+05, -8.4055062591169562211E+04,
    1.8680990008359188352E+04, -2.9458766545509337327E+03,
    3.3307310774649071172E+02, -2.5258076240801555057E+01, 1);

  PC: array [0 .. 5] of Double = (2.2779090197304684302E+04,
    4.1345386639580765797E+04, 2.1170523380864944322E+04,
    3.4806486443249270347E+03, 1.5376201909008354296E+02,
    8.8961548424210455236E-01);

  QC: array [0 .. 5] of Double = (2.2779090197304684318E+04,
    4.1370412495510416640E+04, 2.1215350561880115730E+04,
    3.5028735138235608207E+03, 1.5711159858080893649E+02, 1);

  PS: array [0 .. 5] of Double = (-8.9226600200800094098E+01,
    -1.8591953644342993800E+02, -1.1183429920482737611E+02,
    -2.2300261666214198472E+01, -1.2441026745835638459E+00,
    -8.8033303048680751817E-03);

  QS: array [0 .. 5] of Double = (5.7105024128512061905E+03,
    1.1951131543434613647E+04, 7.2642780169211018836E+03,
    1.4887231232283756582E+03, 9.0593769594993125859E+01, 1);

  CX1: Double = 2.4048255576957727686;
  CX2: Double = 5.5200781102863106496E+00;
  CX11: Double = 2.40625;
  CX12: Double = -1.42444230422723137837E-03;
  CX21: Double = 5.51953125;
  CX22: Double = 5.46860286310649596604E-04;

var
  Temp: Double;
  Cmplx: TComplex64;
begin
  // even function
  Value := Abs(Value);

  if Value = 0 then
    Result := 1
  else if Value <= 4 then // x in (0, 4]
  begin
    Assert(Length(P1) = Length(Q1));
    Result := (Value + CX1) * ((Value - CX11) - CX12) * EvaluateRational(P1, Q1,
      Sqr(Value));
  end
  else if Value <= 8 then // x in (4, 8]
  begin
    Assert(Length(P2) = Length(Q2));
    Result := (Value + CX2) * ((Value - CX21) - CX22) * EvaluateRational(P2, Q2,
      1 - Sqr(Value) / 64);
  end
  else // x in (8, \infty)
  begin
    Temp := 8 / Value;
    GetSinCos(Value - 0.25 * Pi, Cmplx.Im, Cmplx.Re);

    Assert(Length(PC) = Length(QC));
    Assert(Length(PS) = Length(QS));

    Result := sqrt(2 / (Value * Pi)) * (EvaluateRational(PC, QC, Sqr(Temp)) *
      Cmplx.Re - Temp * EvaluateRational(PS, QS, Sqr(Temp)) * Cmplx.Im);
  end;
end;

function BesselFirstKindOrderOne(Value: Double): Double;
const
  P1: array [0 .. 6] of Double = (-1.4258509801366645672E+11,
    6.6781041261492395835E+09, -1.1548696764841276794E+08,
    9.8062904098958257677E+05, -4.4615792982775076130E+03,
    1.0650724020080236441E+01, -1.0767857011487300348E-02);

  Q1: array [0 .. 6] of Double = (4.1868604460820175290E+12,
    4.2091902282580133541E+10, 2.0228375140097033958E+08,
    5.9117614494174794095E+05, 1.0742272239517380498E+03, 1, 0);

  P2: array [0 .. 7] of Double = (-1.7527881995806511112E+16,
    1.6608531731299018674E+15, -3.6658018905416665164E+13,
    3.5580665670910619166E+11, -1.8113931269860667829E+09,
    5.0793266148011179143E+06, -7.5023342220781607561E+03,
    4.6179191852758252278);

  Q2: array [0 .. 7] of Double = (1.7253905888447681194E+18,
    1.7128800897135812012E+16, 8.4899346165481429307E+13,
    2.7622777286244082666E+11, 6.4872502899596389593E+08,
    1.1267125065029138050E+06, 1.3886978985861357615E+03, 1);

  PC: array [0 .. 6] of Double = (-4.4357578167941278571E+06,
    -9.9422465050776411957E+06, -6.6033732483649391093E+06,
    -1.5235293511811373833E+06, -1.0982405543459346727E+05,
    -1.6116166443246101165E+03, 0);

  QC: array [0 .. 6] of Double = (-4.4357578167941278568E+06,
    -9.9341243899345856590E+06, -6.5853394797230870728E+06,
    -1.5118095066341608816E+06, -1.0726385991103820119E+05,
    -1.4550094401904961825E+03, 1);

  PS: array [0 .. 6] of Double = (3.3220913409857223519E+04,
    8.5145160675335701966E+04, 6.6178836581270835179E+04,
    1.8494262873223866797E+04, 1.7063754290207680021E+03,
    3.5265133846636032186E+01, 0);

  QS: array [0 .. 6] of Double = (7.0871281941028743574E+05,
    1.8194580422439972989E+06, 1.4194606696037208929E+06,
    4.0029443582266975117E+05, 3.7890229745772202641E+04,
    8.6383677696049909675E+02, 1);

  x1: Double = 3.8317059702075123156;
  x2: Double = 7.0155866698156187535;
  x11: Double = 9.810E+02;
  x12: Double = -3.2527979248768438556E-04;
  x21: Double = 1.7960E+03;
  x22: Double = -3.8330184381246462950E-05;

var
  Temp: Double;
  Cmplx: TComplex64;

begin

  if Value = 0 then
    Result := 0
  else if Abs(Value) <= 4 then // w in (0, 4]
  begin
    Assert(Length(P1) = Length(Q1));
    Result := Abs(Value) * (Abs(Value) + x1) * ((Abs(Value) - x11 / 256) - x12)
      * EvaluateRational(P1, Q1, Sqr(Value));
  end
  else if Abs(Value) <= 8 then // w in (4, 8]
  begin
    Assert(Length(P2) = Length(Q2));
    Result := Abs(Value) * (Abs(Value) + x2) * ((Abs(Value) - x21 / 256) - x22)
      * EvaluateRational(P2, Q2, Sqr(Value));
  end
  else // w in (8, \infty)
  begin
    Temp := 8 / Abs(Value);;
    GetSinCos(Abs(Value) - 0.75 * Pi, Cmplx.Im, Cmplx.Re);
    Assert(Length(PC) = Length(QC));
    Assert(Length(PS) = Length(QS));
    Result := sqrt(2 / (Abs(Value) * Pi)) *
      (EvaluateRational(PC, QC, Sqr(Temp)) * Cmplx.Re - Temp *
      EvaluateRational(PS, QS, Sqr(Temp)) * Cmplx.Im);
  end;

  // odd function
  if (Value < 0) then
    Result := -Result;
end;

end.
