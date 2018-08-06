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

unit DAV_InvMDCT;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types;

procedure InvMDCT(Input: PDAV1024SingleArray; Output: PDAV1024SingleArray; BlockType: Integer);

implementation

uses
  DAV_Common;

const
  CWin: array[0..3, 0..35] of Single = (
    (-1.6141214951E-2, -5.3603178919E-2, -1.0070713296E-1, -1.6280817573E-1,
     -4.9999999679E-1, -3.8388735032E-1, -6.2061144372E-1, -1.1659756083E+0,
     -3.8720752656E+0, -4.2256286556E+0, -1.5195289984E+0, -9.7416483388E-1,
     -7.3744074053E-1, -1.2071067773E+0, -5.1636156596E-1, -4.5426052317E-1,
     -4.0715656898E-1, -3.6969460527E-1, -3.3876269197E-1, -3.1242222492E-1,
     -2.8939587111E-1, -2.6880081906E-1, -5.0000000266E-1, -2.3251417468E-1,
     -2.1596714708E-1, -2.0004979098E-1, -1.8449493497E-1, -1.6905846094E-1,
     -1.5350360518E-1, -1.3758624925E-1, -1.2103922149E-1, -2.0710679058E-1,
     -8.4752577594E-2, -6.4157525656E-2, -4.1131172614E-2, -1.4790705759E-2),

    (-1.6141214951E-2, -5.3603178919E-2, -1.0070713296E-1, -1.6280817573E-1,
     -4.9999999679E-1, -3.8388735032E-1, -6.2061144372E-1, -1.1659756083E+0,
     -3.8720752656E+0, -4.2256286556E+0, -1.5195289984E+0, -9.7416483388E-1,
     -7.3744074053E-1, -1.2071067773E+0, -5.1636156596E-1, -4.5426052317E-1,
     -4.0715656898E-1, -3.6969460527E-1, -3.3908542600E-1, -3.1511810350E-1,
     -2.9642226150E-1, -2.8184548650E-1, -5.4119610000E-1, -2.6213228100E-1,
     -2.5387916537E-1, -2.3296291359E-1, -1.9852728987E-1, -1.5233534808E-1,
     -9.6496400054E-2, -3.3423828516E-2,  0, 0, 0, 0, 0, 0),

    (-4.8300800645E-2, -1.5715656932E-1, -2.8325045177E-1, -4.2953747763E-1,
     -1.2071067795E+0, -8.2426483178E-1, -1.1451749106E+0, -1.7695290101E+0,
     -4.5470225061E+0, -3.4890531002E+0, -7.3296292804E-1, -1.5076514758E-1,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),

    ( 0, 0, 0, 0, 0, 0, -1.5076513660E-1, -7.3296291107E-1, -3.4890530566E+0,
     -4.5470224727E+0, -1.7695290031E+0, -1.1451749092E+0, -8.3137738100E-1,
     -1.3065629650E+0, -5.4142014250E-1, -4.6528974900E-1, -4.1066990750E-1,
     -3.7004680800E-1, -3.3876269197E-1, -3.1242222492E-1, -2.8939587111E-1,
     -2.6880081906E-1, -5.0000000266E-1, -2.3251417468E-1, -2.1596714708E-1,
     -2.0004979098E-1, -1.8449493497E-1, -1.6905846094E-1, -1.5350360518E-1,
     -1.3758624925E-1, -1.2103922149E-1, -2.0710679058E-1, -8.4752577594E-2,
     -6.4157525656E-2, -4.1131172614E-2, -1.4790705759E-2));

procedure InvMDCT(Input: PDAV1024SingleArray; Output: PDAV1024SingleArray; BlockType: Integer);
var
  Temp       : array[0..17] of Single;
  WinBt      : PDAV1024SingleArray;
  i, p       : integer;
  SixInc     : Integer;
  pp         : array [0..1] of Single;
  Sum, Save  : Single;
  TmpX       : array [0..8] of Single;
  Tmp        : array [0..8] of Single;
  i0, i0p12  : Single;
  i6_, e, o  : Single;
begin
 if (BlockType = 2) then
  begin
   p := 0;
   while (p < 36) do
    begin
     Output[p    ] := 0;
     Output[p + 1] := 0;
     Output[p + 2] := 0;
     Output[p + 3] := 0;
     Output[p + 4] := 0;
     Output[p + 5] := 0;
     Output[p + 6] := 0;
     Output[p + 7] := 0;
     Output[p + 8] := 0;
     Inc(p, 9);
    end;

   SixInc := 0;
   for i := 0 to 2 do
    begin
     // 12 point IMDCT
     // Begin 12 point IDCT
     // Input aliasing for 12 pt IDCT
     Input[15 + i] := Input[15 + i] + Input[12 + i];
     Input[12 + i] := Input[12 + i] + Input[ 9 + i];
     Input[ 9 + i] := Input[ 9 + i] + Input[ 6 + i];
     Input[ 6 + i] := Input[ 6 + i] + Input[ 3 + i];
     Input[ 3 + i] := Input[ 3 + i] + Input[     i];

     // Input aliasing on odd indices (for 6 point IDCT)
     Input[15+i] := Input[15+i] + Input[9+i];
     Input[9+i]  := Input[9+i]  + Input[3+i];

     // 3 point IDCT on even indices
     pp[1] := Input[12 + i] * CHalf32;
     pp[0] := Input[ 6 + i] * 0.866025403;
     Sum := Input[i] + pp[1];
     Temp[1] := Input[i] - Input[12 + i];
     Temp[0] := Sum + pp[0];
     Temp[2] := Sum - pp[0];
     // End 3 point IDCT on even indices

     // 3 point IDCT on odd indices (for 6 point IDCT)
     pp[1] := Input[15 + i] * CHalf32;
     pp[0] := Input[ 9 + i] * 0.866025403;
     Sum := Input[3 + i] + pp[1];
     Temp[4] := Input[3+i] - Input[15+i];
     Temp[5] := Sum + pp[0];
     Temp[3] := Sum - pp[0];
     // End 3 point IDCT on odd indices

     // Twiddle factors on odd indices (for 6 point IDCT)
     Temp[3] := Temp[3] * 1.931851653;
     Temp[4] := Temp[4] * 0.707106781;
     Temp[5] := Temp[5] * 0.517638090;

     // Output butterflies on 2 3 point IDCT's (for 6 point IDCT)
     Save := Temp[0];
     Temp[0] := Temp[0] + Temp[5];
     Temp[5] := Save - Temp[5];
     Save := Temp[1];
     Temp[1] := Temp[1] + Temp[4];
     Temp[4] := Save - Temp[4];
     Save := Temp[2];
     Temp[2] := Temp[2] + Temp[3];
     Temp[3] := Save - Temp[3];
     // End 6 point IDCT

     // Twiddle factors on indices (for 12 point IDCT)
     Temp[0] := Temp[0] * 0.504314480;
     Temp[1] := Temp[1] * 0.541196100;
     Temp[2] := Temp[2] * 0.630236207;
     Temp[3] := Temp[3] * 0.821339815;
     Temp[4] := Temp[4] * 1.306562965;
     Temp[5] := Temp[5] * 3.830648788;
     // End 12 point IDCT

     // Shift to 12 point modified IDCT, multiply by window type 2
     Temp[8]  := -Temp[0] * 0.793353340;
     Temp[9]  := -Temp[0] * 0.608761429;
     Temp[7]  := -Temp[1] * 0.923879532;
     Temp[10] := -Temp[1] * 0.382683432;
     Temp[6]  := -Temp[2] * 0.991444861;
     Temp[11] := -Temp[2] * 0.130526192;

     Temp[0]  :=  Temp[3];
     Temp[1]  :=  Temp[4] * 0.382683432;
     Temp[2]  :=  Temp[5] * 0.608761429;

     Temp[3]  := -Temp[5] * 0.793353340;
     Temp[4]  := -Temp[4] * 0.923879532;
     Temp[5]  := -Temp[0] * 0.991444861;

     Temp[0]  :=  Temp[0] * 0.130526192;

     Output[SixInc + 6]  := Output[SixInc + 6]  + Temp[0];
     Output[SixInc + 7]  := Output[SixInc + 7]  + Temp[1];
     Output[SixInc + 8]  := Output[SixInc + 8]  + Temp[2];
     Output[SixInc + 9]  := Output[SixInc + 9]  + Temp[3];
     Output[SixInc + 10] := Output[SixInc + 10] + Temp[4];
     Output[SixInc + 11] := Output[SixInc + 11] + Temp[5];
     Output[SixInc + 12] := Output[SixInc + 12] + Temp[6];
     Output[SixInc + 13] := Output[SixInc + 13] + Temp[7];
     Output[SixInc + 14] := Output[SixInc + 14] + Temp[8];
     Output[SixInc + 15] := Output[SixInc + 15] + Temp[9];
     Output[SixInc + 16] := Output[SixInc + 16] + Temp[10];
     Output[SixInc + 17] := Output[SixInc + 17] + Temp[11];

     Inc(SixInc, 6);
    end;
  end
 else
  begin
   // 36 point IDCT
   // Input aliasing for 36 point IDCT
   Input[17] := Input[17] + Input[16];
   Input[16] := Input[16] + Input[15];
   Input[15] := Input[15] + Input[14];
   Input[14] := Input[14] + Input[13];
   Input[13] := Input[13] + Input[12];
   Input[12] := Input[12] + Input[11];
   Input[11] := Input[11] + Input[10];
   Input[10] := Input[10] + Input[9];
   Input[9]  := Input[9]  + Input[8];
   Input[8]  := Input[8]  + Input[7];
   Input[7]  := Input[7]  + Input[6];
   Input[6]  := Input[6]  + Input[5];
   Input[5]  := Input[5]  + Input[4];
   Input[4]  := Input[4]  + Input[3];
   Input[3]  := Input[3]  + Input[2];
   Input[2]  := Input[2]  + Input[1];
   Input[1]  := Input[1]  + Input[0];
   // 18 point IDCT for odd indices

   // Input aliasing for 18 point IDCT
   Input[17] := Input[17] + Input[15];
   Input[15] := Input[15] + Input[13];
   Input[13] := Input[13] + Input[11];
   Input[11] := Input[11] + Input[9];
   Input[9]  := Input[9]  + Input[7];
   Input[7]  := Input[7]  + Input[5];
   Input[5]  := Input[5]  + Input[3];
   Input[3]  := Input[3]  + Input[1];

   // Fast 9 Point Inverse Discrete Cosine Transform
   //
   // By  Francois-Raymond Boyer
   //         mailto:boyerf@iro.umontreal.ca
   //         http://www.iro.umontreal.ca/~boyerf
   //
   // The code has been optimized for Intel processors
   //  (takes a lot of time to convert float to and from iternal FPU representation)
   //
   // It is a simple "factorization" of the IDCT matrix.

   // 9 point IDCT on even indices
   // 5 points on odd indices (not realy an IDCT)
   i0 := Input[0] + Input[0];
   i0p12 := i0 + Input[12];

   TmpX[0] := i0p12 + Input[4] * 1.8793852415718  + Input[8] * 1.532088886238 + Input[16] * 0.34729635533386;
   TmpX[1] := i0    + Input[4]                    - Input[8] - Input[12] - Input[12] - Input[16];
   TmpX[2] := i0p12 - Input[4] * 0.34729635533386 - Input[8] * 1.8793852415718  + Input[16] * 1.532088886238;
   TmpX[3] := i0p12 - Input[4] * 1.532088886238   + Input[8] * 0.34729635533386 - Input[16] * 1.8793852415718;
   TmpX[4] := Input[0] - Input[4]                 + Input[8] - Input[12]          + Input[16];

   // 4 points on even indices
   i6_ := Input[6] * 1.732050808;  // Sqrt[3]

   TmpX[5] := Input[2] * 1.9696155060244  + i6_ + Input[10] * 1.2855752193731  + Input[14] * 0.68404028665134;
   TmpX[6] := (Input[2]                        - Input[10]                   - Input[14]) * 1.732050808;
   TmpX[7] := Input[2] * 1.2855752193731  - i6_ - Input[10] * 0.68404028665134 + Input[14] * 1.9696155060244;
   TmpX[8] := Input[2] * 0.68404028665134 - i6_ + Input[10] * 1.9696155060244  - Input[14] * 1.2855752193731;

   // 9 point IDCT on odd indices
   // 5 points on odd indices (not realy an IDCT)
   i0 := Input[1] + Input[1];
   i0p12 := i0 + Input[13];

   Tmp[0] := i0p12   + Input[5] * 1.8793852415718  + Input[9] * 1.532088886238 + Input[17] * 0.34729635533386;
   Tmp[1] := i0      + Input[5]                    - Input[9] - Input[13] - Input[13] - Input[17];
   Tmp[2] := i0p12   - Input[5] * 0.34729635533386 - Input[9] * 1.8793852415718 + Input[17] * 1.532088886238;
   Tmp[3] := i0p12   - Input[5] * 1.532088886238   + Input[9] * 0.34729635533386 - Input[17] * 1.8793852415718;
   Tmp[4] := (Input[1] - Input[5] + Input[9] - Input[13] + Input[17]) * 0.707106781;  // Twiddled

   // 4 points on even indices
   i6_ := Input[7] * 1.732050808;  // Sqrt[3]

   Tmp[5] := Input[3] * 1.9696155060244 + i6_ + Input[11] * 1.2855752193731  + Input[15] * 0.68404028665134;
   Tmp[6] := (Input[3]                        - Input[11]                   - Input[15]) * 1.732050808;
   Tmp[7] := Input[3] * 1.2855752193731 - i6_ - Input[11] * 0.68404028665134 + Input[15] * 1.9696155060244;
   Tmp[8] := Input[3] * 0.68404028665134 - i6_ + Input[11] * 1.9696155060244  - Input[15] * 1.2855752193731;

   // Twiddle factors on odd indices
   // and
   // Butterflies on 9 point IDCT's
   // and
   // twiddle factors for 36 point IDCT

   e := TmpX[0] + TmpX[5];
   o := (Tmp[0] + Tmp[5]) * 0.501909918;
   Temp[0] := e + o;
   Temp[17] := e - o;
   e := TmpX[1] + TmpX[6];
   o := (Tmp[1] + Tmp[6]) * 0.517638090;
   Temp[1] := e + o;
   Temp[16] := e - o;
   e := TmpX[2] + TmpX[7];
   o := (Tmp[2] + Tmp[7]) * 0.551688959;
   Temp[2] := e + o;
   Temp[15] := e - o;
   e := TmpX[3] + TmpX[8];
   o := (Tmp[3] + Tmp[8]) * 0.610387294;
   Temp[3] := e + o;
   Temp[14] := e - o;
   Temp[4] := TmpX[4] + Tmp[4];
   Temp[13] := TmpX[4] - Tmp[4];
   e := TmpX[3] - TmpX[8];
   o := (Tmp[3] - Tmp[8]) * 0.871723397;
   Temp[5] := e + o;
   Temp[12] := e - o;
   e := TmpX[2] - TmpX[7];
   o := (Tmp[2] - Tmp[7]) * 1.183100792;
   Temp[6] := e + o;
   Temp[11] := e - o;
   e := TmpX[1] - TmpX[6];
   o := (Tmp[1] - Tmp[6]) * 1.931851653;
   Temp[7] := e + o;
   Temp[10] := e - o;
   e := TmpX[0] - TmpX[5];
   o := (Tmp[0] - Tmp[5]) * 5.736856623;
   Temp[8] := e + o;
   Temp[9] := e - o;

   // end 36 point IDCT */

   // shift to modified IDCT
   WinBt := @CWin[BlockType];

   Output[0]  := -Temp[9]  * WinBt[0];
   Output[1]  := -Temp[10] * WinBt[1];
   Output[2]  := -Temp[11] * WinBt[2];
   Output[3]  := -Temp[12] * WinBt[3];
   Output[4]  := -Temp[13] * WinBt[4];
   Output[5]  := -Temp[14] * WinBt[5];
   Output[6]  := -Temp[15] * WinBt[6];
   Output[7]  := -Temp[16] * WinBt[7];
   Output[8]  := -Temp[17] * WinBt[8];

   Output[9]  :=  Temp[17] * WinBt[9];
   Output[10] :=  Temp[16] * WinBt[10];
   Output[11] :=  Temp[15] * WinBt[11];
   Output[12] :=  Temp[14] * WinBt[12];
   Output[13] :=  Temp[13] * WinBt[13];
   Output[14] :=  Temp[12] * WinBt[14];
   Output[15] :=  Temp[11] * WinBt[15];
   Output[16] :=  Temp[10] * WinBt[16];
   Output[17] :=  Temp[9]  * WinBt[17];
   Output[18] :=  Temp[8]  * WinBt[18];
   Output[19] :=  Temp[7]  * WinBt[19];
   Output[20] :=  Temp[6]  * WinBt[20];
   Output[21] :=  Temp[5]  * WinBt[21];
   Output[22] :=  Temp[4]  * WinBt[22];
   Output[23] :=  Temp[3]  * WinBt[23];
   Output[24] :=  Temp[2]  * WinBt[24];
   Output[25] :=  Temp[1]  * WinBt[25];
   Output[26] :=  Temp[0]  * WinBt[26];

   Output[27] :=  Temp[0]  * WinBt[27];
   Output[28] :=  Temp[1]  * WinBt[28];
   Output[29] :=  Temp[2]  * WinBt[29];
   Output[30] :=  Temp[3]  * WinBt[30];
   Output[31] :=  Temp[4]  * WinBt[31];
   Output[32] :=  Temp[5]  * WinBt[32];
   Output[33] :=  Temp[6]  * WinBt[33];
   Output[34] :=  Temp[7]  * WinBt[34];
   Output[35] :=  Temp[8]  * WinBt[35];
  end;
end;

end.
