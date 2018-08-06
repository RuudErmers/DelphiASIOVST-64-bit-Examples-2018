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

unit DAV_SynthFilter;

interface

{$I DAV_Compiler.inc}
{$DEFINE FastCalculation}

uses
  DAV_Types, DAV_Complex;

type
  TNewPCMSample = procedure(Sender: TObject; Sample: Single) of object;

  // A class for the synthesis filter bank:
  // This class does a fast downsampling from 32, 44.1 or 48 kHz to 8 kHz, if ULAW is defined.
  // Frequencies above 4 kHz are removed by ignoring higher subbands.
  TSynthesisFilter = class
  private
    FVector: array [0 .. 1, 0 .. 511] of Single;
    FActualVector: PDAV512SingleArray; // FVector[0] or FVector[1]
    FActualWritePos: Cardinal; // 0-15
    FSample: array [0 .. 31] of Single; // 32 new subband samples
    FOnNewPCMSample: TNewPCMSample;
    procedure ComputeNewVector;
    procedure ComputePCMSample;
  public
    constructor Create;
    procedure InputSample(Sample: Single; SubBandNumber: Cardinal);
    procedure CalculatePCMSamples; // calculate 32 PCM samples
    procedure Reset; // reset the synthesis filter

    property OnNewPCMSample: TNewPCMSample read FOnNewPCMSample
      write FOnNewPCMSample;
  end;

implementation

uses
  DAV_Math;

const
  // Note: These values are not in the same order
  // as in Annex 3-B.3 of the ISO/IEC DIS 11172-3
  CAnnex3B3Table: array [0 .. 511] of Single = (0, -0.000442505, 0.003250122,
    -0.007003784, 0.031082153, -0.078628540, 0.100311279, -0.572036743,
    1.144989014, 0.572036743, 0.100311279, 0.078628540, 0.031082153,
    0.007003784, 0.003250122, 0.000442505, -0.000015259, -0.000473022,
    0.003326416, -0.007919312, 0.030517578, -0.084182739, 0.090927124,
    -0.600219727, 1.144287109, 0.543823242, 0.108856201, 0.073059082,
    0.031478882, 0.006118774, 0.003173828, 0.000396729, -0.000015259,
    -0.000534058, 0.003387451, -0.008865356, 0.029785156, -0.089706421,
    0.080688477, -0.628295898, 1.142211914, 0.515609741, 0.116577148,
    0.067520142, 0.031738281, 0.005294800, 0.003082275, 0.000366211,
    -0.000015259, -0.000579834, 0.003433228, -0.009841919, 0.028884888,
    -0.095169067, 0.069595337, -0.656219482, 1.138763428, 0.487472534,
    0.123474121, 0.061996460, 0.031845093, 0.004486084, 0.002990723,
    0.000320435, -0.000015259, -0.000625610, 0.003463745, -0.010848999,
    0.027801514, -0.100540161, 0.057617188, -0.683914185, 1.133926392,
    0.459472656, 0.129577637, 0.056533813, 0.031814575, 0.003723145,
    0.002899170, 0.000289917, -0.000015259, -0.000686646, 0.003479004,
    -0.011886597, 0.026535034, -0.105819702, 0.044784546, -0.711318970,
    1.127746582, 0.431655884, 0.134887695, 0.051132202, 0.031661987,
    0.003005981, 0.002792358, 0.000259399, -0.000015259, -0.000747681,
    0.003479004, -0.012939453, 0.025085449, -0.110946655, 0.031082153,
    -0.738372803, 1.120223999, 0.404083252, 0.139450073, 0.045837402,
    0.031387329, 0.002334595, 0.002685547, 0.000244141, -0.000030518,
    -0.000808716, 0.003463745, -0.014022827, 0.023422241, -0.115921021,
    0.016510010, -0.765029907, 1.111373901, 0.376800537, 0.143264771,
    0.040634155, 0.031005859, 0.001693726, 0.002578735, 0.000213623,
    -0.000030518, -0.000885010, 0.003417969, -0.015121460, 0.021575928,
    -0.120697021, 0.001068115, -0.791213989, 1.101211548, 0.349868774,
    0.146362305, 0.035552979, 0.030532837, 0.001098633, 0.002456665,
    0.000198364, -0.000030518, -0.000961304, 0.003372192, -0.016235352,
    0.019531250, -0.125259399, -0.015228271, -0.816864014, 1.089782715,
    0.323318481, 0.148773193, 0.030609131, 0.029937744, 0.000549316,
    0.002349854, 0.000167847, -0.000030518, -0.001037598, 0.003280640,
    -0.017349243, 0.017257690, -0.129562378, -0.032379150, -0.841949463,
    1.077117920, 0.297210693, 0.150497437, 0.025817871, 0.029281616,
    0.000030518, 0.002243042, 0.000152588, -0.000045776, -0.001113892,
    0.003173828, -0.018463135, 0.014801025, -0.133590698, -0.050354004,
    -0.866363525, 1.063217163, 0.271591187, 0.151596069, 0.021179199,
    0.028533936, -0.000442505, 0.002120972, 0.000137329, -0.000045776,
    -0.001205444, 0.003051758, -0.019577026, 0.012115479, -0.137298584,
    -0.069168091, -0.890090942, 1.048156738, 0.246505737, 0.152069092,
    0.016708374, 0.027725220, -0.000869751, 0.002014160, 0.000122070,
    -0.000061035, -0.001296997, 0.002883911, -0.020690918, 0.009231567,
    -0.140670776, -0.088775635, -0.913055420, 1.031936646, 0.221984863,
    0.151962280, 0.012420654, 0.026840210, -0.001266479, 0.001907349,
    0.000106812, -0.000061035, -0.001388550, 0.002700806, -0.021789551,
    0.006134033, -0.143676758, -0.109161377, -0.935195923, 1.014617920,
    0.198059082, 0.151306152, 0.008316040, 0.025909424, -0.001617432,
    0.001785278, 0.000106812, -0.000076294, -0.001480103, 0.002487183,
    -0.022857666, 0.002822876, -0.146255493, -0.130310059, -0.956481934,
    0.996246338, 0.174789429, 0.150115967, 0.004394531, 0.024932861,
    -0.001937866, 0.001693726, 0.000091553, -0.000076294, -0.001586914,
    0.002227783, -0.023910522, -0.000686646, -0.148422241, -0.152206421,
    -0.976852417, 0.976852417, 0.152206421, 0.148422241, 0.000686646,
    0.023910522, -0.002227783, 0.001586914, 0.000076294, -0.000091553,
    -0.001693726, 0.001937866, -0.024932861, -0.004394531, -0.150115967,
    -0.174789429, -0.996246338, 0.956481934, 0.130310059, 0.146255493,
    -0.002822876, 0.022857666, -0.002487183, 0.001480103, 0.000076294,
    -0.000106812, -0.001785278, 0.001617432, -0.025909424, -0.008316040,
    -0.151306152, -0.198059082, -1.014617920, 0.935195923, 0.109161377,
    0.143676758, -0.006134033, 0.021789551, -0.002700806, 0.001388550,
    0.000061035, -0.000106812, -0.001907349, 0.001266479, -0.026840210,
    -0.012420654, -0.151962280, -0.221984863, -1.031936646, 0.913055420,
    0.088775635, 0.140670776, -0.009231567, 0.020690918, -0.002883911,
    0.001296997, 0.000061035, -0.000122070, -0.002014160, 0.000869751,
    -0.027725220, -0.016708374, -0.152069092, -0.246505737, -1.048156738,
    0.890090942, 0.069168091, 0.137298584, -0.012115479, 0.019577026,
    -0.003051758, 0.001205444, 0.000045776, -0.000137329, -0.002120972,
    0.000442505, -0.028533936, -0.021179199, -0.151596069, -0.271591187,
    -1.063217163, 0.866363525, 0.050354004, 0.133590698, -0.014801025,
    0.018463135, -0.003173828, 0.001113892, 0.000045776, -0.000152588,
    -0.002243042, -0.000030518, -0.029281616, -0.025817871, -0.150497437,
    -0.297210693, -1.077117920, 0.841949463, 0.032379150, 0.129562378,
    -0.017257690, 0.017349243, -0.003280640, 0.001037598, 0.000030518,
    -0.000167847, -0.002349854, -0.000549316, -0.029937744, -0.030609131,
    -0.148773193, -0.323318481, -1.089782715, 0.816864014, 0.015228271,
    0.125259399, -0.019531250, 0.016235352, -0.003372192, 0.000961304,
    0.000030518, -0.000198364, -0.002456665, -0.001098633, -0.030532837,
    -0.035552979, -0.146362305, -0.349868774, -1.101211548, 0.791213989,
    -0.001068115, 0.120697021, -0.021575928, 0.015121460, -0.003417969,
    0.000885010, 0.000030518, -0.000213623, -0.002578735, -0.001693726,
    -0.031005859, -0.040634155, -0.143264771, -0.376800537, -1.111373901,
    0.765029907, -0.016510010, 0.115921021, -0.023422241, 0.014022827,
    -0.003463745, 0.000808716, 0.000030518, -0.000244141, -0.002685547,
    -0.002334595, -0.031387329, -0.045837402, -0.139450073, -0.404083252,
    -1.120223999, 0.738372803, -0.031082153, 0.110946655, -0.025085449,
    0.012939453, -0.003479004, 0.000747681, 0.000015259, -0.000259399,
    -0.002792358, -0.003005981, -0.031661987, -0.051132202, -0.134887695,
    -0.431655884, -1.127746582, 0.711318970, -0.044784546, 0.105819702,
    -0.026535034, 0.011886597, -0.003479004, 0.000686646, 0.000015259,
    -0.000289917, -0.002899170, -0.003723145, -0.031814575, -0.056533813,
    -0.129577637, -0.459472656, -1.133926392, 0.683914185, -0.057617188,
    0.100540161, -0.027801514, 0.010848999, -0.003463745, 0.000625610,
    0.000015259, -0.000320435, -0.002990723, -0.004486084, -0.031845093,
    -0.061996460, -0.123474121, -0.487472534, -1.138763428, 0.656219482,
    -0.069595337, 0.095169067, -0.028884888, 0.009841919, -0.003433228,
    0.000579834, 0.000015259, -0.000366211, -0.003082275, -0.005294800,
    -0.031738281, -0.067520142, -0.116577148, -0.515609741, -1.142211914,
    0.628295898, -0.080688477, 0.089706421, -0.029785156, 0.008865356,
    -0.003387451, 0.000534058, 0.000015259, -0.000396729, -0.003173828,
    -0.006118774, -0.031478882, -0.073059082, -0.108856201, -0.543823242,
    -1.144287109, 0.600219727, -0.090927124, 0.084182739, -0.030517578,
    0.007919312, -0.003326416, 0.000473022, 0.000015259);

var
  GCosTable: array [0 .. 30] of Single;

  { TSynthesisFilter }

constructor TSynthesisFilter.Create;
begin
  Reset;
end;

procedure TSynthesisFilter.CalculatePCMSamples;
begin
  ComputeNewVector;
  ComputePCMSample;
  FActualWritePos := (FActualWritePos + 1) and $F;
  if (FActualVector = @FVector[0]) then
    FActualVector := @FVector[1]
  else
    FActualVector := @FVector[0];
  FillChar(FSample, Sizeof(FSample), 0);
end;

procedure TSynthesisFilter.ComputeNewVector;
var
  NewVec: array [0 .. 31] of Single;
  // new V[0-15] and V[33-48] of Figure 3-A.2 in ISO DIS 11172-3
  p: array [0 .. 15] of Single;
  pp: array [0 .. 15] of Single;
  x: array [0 .. 1] of PDAV512SingleArray;
  tmp: array [0 .. 1] of Single;
begin
  // compute new values via a fast cosine transform:
  x[0] := @FSample;

  p[0] := x[0, 0] + x[0, 31];
  p[1] := x[0, 1] + x[0, 30];
  p[2] := x[0, 2] + x[0, 29];
  p[3] := x[0, 3] + x[0, 28];
  p[4] := x[0, 4] + x[0, 27];
  p[5] := x[0, 5] + x[0, 26];
  p[6] := x[0, 6] + x[0, 25];
  p[7] := x[0, 7] + x[0, 24];
  p[8] := x[0, 8] + x[0, 23];
  p[9] := x[0, 9] + x[0, 22];
  p[10] := x[0, 10] + x[0, 21];
  p[11] := x[0, 11] + x[0, 20];
  p[12] := x[0, 12] + x[0, 19];
  p[13] := x[0, 13] + x[0, 18];
  p[14] := x[0, 14] + x[0, 17];
  p[15] := x[0, 15] + x[0, 16];

  pp[0] := p[0] + p[15];
  pp[1] := p[1] + p[14];
  pp[2] := p[2] + p[13];
  pp[3] := p[3] + p[12];
  pp[4] := p[4] + p[11];
  pp[5] := p[5] + p[10];
  pp[6] := p[6] + p[9];
  pp[7] := p[7] + p[8];
  pp[8] := (p[0] - p[15]) * GCosTable[14];
  pp[9] := (p[1] - p[14]) * GCosTable[13];
  pp[10] := (p[2] - p[13]) * GCosTable[12];
  pp[11] := (p[3] - p[12]) * GCosTable[11];
  pp[12] := (p[4] - p[11]) * GCosTable[10];
  pp[13] := (p[5] - p[10]) * GCosTable[9];
  pp[14] := (p[6] - p[9]) * GCosTable[8];
  pp[15] := (p[7] - p[8]) * GCosTable[7];

  p[0] := pp[0] + pp[7];
  p[1] := pp[1] + pp[6];
  p[2] := pp[2] + pp[5];
  p[3] := pp[3] + pp[4];
  p[4] := (pp[0] - pp[7]) * GCosTable[6];
  p[5] := (pp[1] - pp[6]) * GCosTable[5];
  p[6] := (pp[2] - pp[5]) * GCosTable[4];
  p[7] := (pp[3] - pp[4]) * GCosTable[3];
  p[8] := pp[8] + pp[15];
  p[9] := pp[9] + pp[14];
  p[10] := pp[10] + pp[13];
  p[11] := pp[11] + pp[12];
  p[12] := (pp[8] - pp[15]) * GCosTable[6];
  p[13] := (pp[9] - pp[14]) * GCosTable[5];
  p[14] := (pp[10] - pp[13]) * GCosTable[4];
  p[15] := (pp[11] - pp[12]) * GCosTable[3];

  pp[0] := p[0] + p[3];
  pp[1] := p[1] + p[2];
  pp[2] := (p[0] - p[3]) * GCosTable[2];
  pp[3] := (p[1] - p[2]) * GCosTable[1];
  pp[4] := p[4] + p[7];
  pp[5] := p[5] + p[6];
  pp[6] := (p[4] - p[7]) * GCosTable[2];
  pp[7] := (p[5] - p[6]) * GCosTable[1];
  pp[8] := p[8] + p[11];
  pp[9] := p[9] + p[10];
  pp[10] := (p[8] - p[11]) * GCosTable[2];
  pp[11] := (p[9] - p[10]) * GCosTable[1];
  pp[12] := p[12] + p[15];
  pp[13] := p[13] + p[14];
  pp[14] := (p[12] - p[15]) * GCosTable[2];
  pp[15] := (p[13] - p[14]) * GCosTable[1];

  p[0] := pp[0] + pp[1];
  p[1] := (pp[0] - pp[1]) * GCosTable[0];
  p[2] := pp[2] + pp[3];
  p[3] := (pp[2] - pp[3]) * GCosTable[0];
  p[4] := pp[4] + pp[5];
  p[5] := (pp[4] - pp[5]) * GCosTable[0];
  p[6] := pp[6] + pp[7];
  p[7] := (pp[6] - pp[7]) * GCosTable[0];
  p[8] := pp[8] + pp[9];
  p[9] := (pp[8] - pp[9]) * GCosTable[0];
  p[10] := pp[10] + pp[11];
  p[11] := (pp[10] - pp[11]) * GCosTable[0];
  p[12] := pp[12] + pp[13];
  p[13] := (pp[12] - pp[13]) * GCosTable[0];
  p[14] := pp[14] + pp[15];
  p[15] := (pp[14] - pp[15]) * GCosTable[0];

  NewVec[12] := p[7];
  NewVec[4] := NewVec[12] + p[5];
  NewVec[19] := -NewVec[4] - p[6];
  NewVec[27] := -p[6] - p[7] - p[4];
  NewVec[14] := p[15];
  NewVec[10] := NewVec[14] + p[11];
  NewVec[6] := NewVec[10] + p[13];
  NewVec[2] := p[15] + p[13] + p[9];
  NewVec[17] := -NewVec[2] - p[14];
  tmp[0] := -p[14] - p[15] - p[10] - p[11];
  NewVec[21] := tmp[0] - p[13];
  NewVec[29] := -p[14] - p[15] - p[12] - p[8];
  NewVec[25] := tmp[0] - p[12];
  NewVec[31] := -p[0];
  NewVec[0] := p[1];
  NewVec[8] := p[3];
  NewVec[23] := -NewVec[8] - p[2];

  p[0] := (x[0, 0] - x[0, 31]) * GCosTable[30];
  p[1] := (x[0, 1] - x[0, 30]) * GCosTable[29];
  p[2] := (x[0, 2] - x[0, 29]) * GCosTable[28];
  p[3] := (x[0, 3] - x[0, 28]) * GCosTable[27];
  p[4] := (x[0, 4] - x[0, 27]) * GCosTable[26];
  p[5] := (x[0, 5] - x[0, 26]) * GCosTable[25];
  p[6] := (x[0, 6] - x[0, 25]) * GCosTable[24];
  p[7] := (x[0, 7] - x[0, 24]) * GCosTable[23];
  p[8] := (x[0, 8] - x[0, 23]) * GCosTable[22];
  p[9] := (x[0, 9] - x[0, 22]) * GCosTable[21];
  p[10] := (x[0, 10] - x[0, 21]) * GCosTable[20];
  p[11] := (x[0, 11] - x[0, 20]) * GCosTable[19];
  p[12] := (x[0, 12] - x[0, 19]) * GCosTable[18];
  p[13] := (x[0, 13] - x[0, 18]) * GCosTable[17];
  p[14] := (x[0, 14] - x[0, 17]) * GCosTable[16];
  p[15] := (x[0, 15] - x[0, 16]) * GCosTable[15];

  pp[0] := p[0] + p[15];
  pp[1] := p[1] + p[14];
  pp[2] := p[2] + p[13];
  pp[3] := p[3] + p[12];
  pp[4] := p[4] + p[11];
  pp[5] := p[5] + p[10];
  pp[6] := p[6] + p[9];
  pp[7] := p[7] + p[8];
  pp[8] := (p[0] - p[15]) * GCosTable[14];
  pp[9] := (p[1] - p[14]) * GCosTable[13];
  pp[10] := (p[2] - p[13]) * GCosTable[12];
  pp[11] := (p[3] - p[12]) * GCosTable[11];
  pp[12] := (p[4] - p[11]) * GCosTable[10];
  pp[13] := (p[5] - p[10]) * GCosTable[9];
  pp[14] := (p[6] - p[9]) * GCosTable[8];
  pp[15] := (p[7] - p[8]) * GCosTable[7];

  p[0] := pp[0] + pp[7];
  p[1] := pp[1] + pp[6];
  p[2] := pp[2] + pp[5];
  p[3] := pp[3] + pp[4];
  p[4] := (pp[0] - pp[7]) * GCosTable[6];
  p[5] := (pp[1] - pp[6]) * GCosTable[5];
  p[6] := (pp[2] - pp[5]) * GCosTable[4];
  p[7] := (pp[3] - pp[4]) * GCosTable[3];
  p[8] := pp[8] + pp[15];
  p[9] := pp[9] + pp[14];
  p[10] := pp[10] + pp[13];
  p[11] := pp[11] + pp[12];
  p[12] := (pp[8] - pp[15]) * GCosTable[6];
  p[13] := (pp[9] - pp[14]) * GCosTable[5];
  p[14] := (pp[10] - pp[13]) * GCosTable[4];
  p[15] := (pp[11] - pp[12]) * GCosTable[3];

  pp[0] := p[0] + p[3];
  pp[1] := p[1] + p[2];
  pp[2] := (p[0] - p[3]) * GCosTable[2];
  pp[3] := (p[1] - p[2]) * GCosTable[1];
  pp[4] := p[4] + p[7];
  pp[5] := p[5] + p[6];
  pp[6] := (p[4] - p[7]) * GCosTable[2];
  pp[7] := (p[5] - p[6]) * GCosTable[1];
  pp[8] := p[8] + p[11];
  pp[9] := p[9] + p[10];
  pp[10] := (p[8] - p[11]) * GCosTable[2];
  pp[11] := (p[9] - p[10]) * GCosTable[1];
  pp[12] := p[12] + p[15];
  pp[13] := p[13] + p[14];
  pp[14] := (p[12] - p[15]) * GCosTable[2];
  pp[15] := (p[13] - p[14]) * GCosTable[1];

  p[0] := pp[0] + pp[1];
  p[1] := (pp[0] - pp[1]) * GCosTable[0];
  p[2] := pp[2] + pp[3];
  p[3] := (pp[2] - pp[3]) * GCosTable[0];
  p[4] := pp[4] + pp[5];
  p[5] := (pp[4] - pp[5]) * GCosTable[0];
  p[6] := pp[6] + pp[7];
  p[7] := (pp[6] - pp[7]) * GCosTable[0];
  p[8] := pp[8] + pp[9];
  p[9] := (pp[8] - pp[9]) * GCosTable[0];
  p[10] := pp[10] + pp[11];
  p[11] := (pp[10] - pp[11]) * GCosTable[0];
  p[12] := pp[12] + pp[13];
  p[13] := (pp[12] - pp[13]) * GCosTable[0];
  p[14] := pp[14] + pp[15];
  p[15] := (pp[14] - pp[15]) * GCosTable[0];

  NewVec[15] := p[15];
  NewVec[13] := NewVec[15] + p[7];
  NewVec[11] := NewVec[13] + p[11];
  NewVec[5] := NewVec[11] + p[5] + p[13];
  NewVec[9] := p[15] + p[11] + p[3];
  NewVec[7] := NewVec[9] + p[13];
  tmp[0] := p[13] + p[15] + p[9];
  NewVec[1] := tmp[0] + p[1];
  NewVec[16] := -NewVec[1] - p[14];
  NewVec[3] := tmp[0] + p[5] + p[7];
  NewVec[18] := -NewVec[3] - p[6] - p[14];

  tmp[0] := -p[10] - p[11] - p[14] - p[15];
  NewVec[22] := tmp[0] - p[13] - p[2] - p[3];
  NewVec[20] := tmp[0] - p[13] - p[5] - p[6] - p[7];
  NewVec[24] := tmp[0] - p[12] - p[2] - p[3];
  tmp[1] := p[4] + p[6] + p[7];
  NewVec[26] := tmp[0] - p[12] - tmp[1];
  tmp[0] := -p[8] - p[12] - p[14] - p[15];
  NewVec[30] := tmp[0] - p[0];
  NewVec[28] := tmp[0] - tmp[1];

  // insert V[0-15] (= NewVec[0-15]) into actual v:
  x[0] := @NewVec;
  x[1] := @FActualVector[FActualWritePos];
  x[1, 0] := x[0, 0];
  x[1, 16] := x[0, 1];
  x[1, 32] := x[0, 2];
  x[1, 48] := x[0, 3];
  x[1, 64] := x[0, 4];
  x[1, 80] := x[0, 5];
  x[1, 96] := x[0, 6];
  x[1, 112] := x[0, 7];
  x[1, 128] := x[0, 8];
  x[1, 144] := x[0, 9];
  x[1, 160] := x[0, 10];
  x[1, 176] := x[0, 11];
  x[1, 192] := x[0, 12];
  x[1, 208] := x[0, 13];
  x[1, 224] := x[0, 14];
  x[1, 240] := x[0, 15];

  // V[16] is always 0.0:
  x[1, 256] := 0;

  // insert V[17-31] (= -NewVec[15-1]) into actual v:
  x[1, 272] := -x[0, 15];
  x[1, 288] := -x[0, 14];
  x[1, 304] := -x[0, 13];
  x[1, 320] := -x[0, 12];
  x[1, 336] := -x[0, 11];
  x[1, 352] := -x[0, 10];
  x[1, 368] := -x[0, 9];
  x[1, 384] := -x[0, 8];
  x[1, 400] := -x[0, 7];
  x[1, 416] := -x[0, 6];
  x[1, 432] := -x[0, 5];
  x[1, 448] := -x[0, 4];
  x[1, 464] := -x[0, 3];
  x[1, 480] := -x[0, 2];
  x[1, 496] := -x[0, 1];

  // insert V[32] (= -NewVec[0]) into other v:
  if (FActualVector = @FVector[0]) then
    x[1] := @FVector[1, FActualWritePos]
  else
    x[1] := @FVector[0, FActualWritePos];

  x[1, 0] := -x[0, 0];

  // insert V[33-48] (= NewVec[16-31]) into other v:
  x[1, 16] := x[0, 16];
  x[1, 32] := x[0, 17];
  x[1, 48] := x[0, 18];
  x[1, 64] := x[0, 19];
  x[1, 80] := x[0, 20];
  x[1, 96] := x[0, 21];
  x[1, 112] := x[0, 22];
  x[1, 128] := x[0, 23];
  x[1, 144] := x[0, 24];
  x[1, 160] := x[0, 25];
  x[1, 176] := x[0, 26];
  x[1, 192] := x[0, 27];
  x[1, 208] := x[0, 28];
  x[1, 224] := x[0, 29];
  x[1, 240] := x[0, 30];
  x[1, 256] := x[0, 31];

  // insert V[49-63] (= NewVec[30-16]) into other v:
  x[1, 272] := x[0, 30];
  x[1, 288] := x[0, 29];
  x[1, 304] := x[0, 28];
  x[1, 320] := x[0, 27];
  x[1, 336] := x[0, 26];
  x[1, 352] := x[0, 25];
  x[1, 368] := x[0, 24];
  x[1, 384] := x[0, 23];
  x[1, 400] := x[0, 22];
  x[1, 416] := x[0, 21];
  x[1, 432] := x[0, 20];
  x[1, 448] := x[0, 19];
  x[1, 464] := x[0, 18];
  x[1, 480] := x[0, 17];
  x[1, 496] := x[0, 16];
end;

procedure TSynthesisFilter.ComputePCMSample;
var
  vp: PDAV512SingleArray;
  Coefficient: PDAV512SingleArray;
  PcmSample: Single;
const
  C2048 = 2048;
begin
  if not Assigned(FOnNewPCMSample) then
    exit;

  vp := FActualVector;
  case FActualWritePos of
    0:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[0] * Coefficient[0]) + (vp[15] * Coefficient[1]) +
            (vp[14] * Coefficient[2]) + (vp[13] * Coefficient[3]) +
            (vp[12] * Coefficient[4]) + (vp[11] * Coefficient[5]) +
            (vp[10] * Coefficient[6]) + (vp[9] * Coefficient[7]) +
            (vp[8] * Coefficient[8]) + (vp[7] * Coefficient[9]) +
            (vp[6] * Coefficient[10]) + (vp[5] * Coefficient[11]) +
            (vp[4] * Coefficient[12]) + (vp[3] * Coefficient[13]) +
            (vp[2] * Coefficient[14]) + (vp[1] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    1:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[1] * Coefficient[0]) + (vp[0] * Coefficient[1]) +
            (vp[15] * Coefficient[2]) + (vp[14] * Coefficient[3]) +
            (vp[13] * Coefficient[4]) + (vp[12] * Coefficient[5]) +
            (vp[11] * Coefficient[6]) + (vp[10] * Coefficient[7]) +
            (vp[9] * Coefficient[8]) + (vp[8] * Coefficient[9]) +
            (vp[7] * Coefficient[10]) + (vp[6] * Coefficient[11]) +
            (vp[5] * Coefficient[12]) + (vp[4] * Coefficient[13]) +
            (vp[3] * Coefficient[14]) + (vp[2] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    2:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[2] * Coefficient[0]) + (vp[1] * Coefficient[1]) +
            (vp[0] * Coefficient[2]) + (vp[15] * Coefficient[3]) +
            (vp[14] * Coefficient[4]) + (vp[13] * Coefficient[5]) +
            (vp[12] * Coefficient[6]) + (vp[11] * Coefficient[7]) +
            (vp[10] * Coefficient[8]) + (vp[9] * Coefficient[9]) +
            (vp[8] * Coefficient[10]) + (vp[7] * Coefficient[11]) +
            (vp[6] * Coefficient[12]) + (vp[5] * Coefficient[13]) +
            (vp[4] * Coefficient[14]) + (vp[3] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    3:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[3] * Coefficient[0]) + (vp[2] * Coefficient[1]) +
            (vp[1] * Coefficient[2]) + (vp[0] * Coefficient[3]) +
            (vp[15] * Coefficient[4]) + (vp[14] * Coefficient[5]) +
            (vp[13] * Coefficient[6]) + (vp[12] * Coefficient[7]) +
            (vp[11] * Coefficient[8]) + (vp[10] * Coefficient[9]) +
            (vp[9] * Coefficient[10]) + (vp[8] * Coefficient[11]) +
            (vp[7] * Coefficient[12]) + (vp[6] * Coefficient[13]) +
            (vp[5] * Coefficient[14]) + (vp[4] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    4:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[4] * Coefficient[0]) + (vp[3] * Coefficient[1]) +
            (vp[2] * Coefficient[2]) + (vp[1] * Coefficient[3]) +
            (vp[0] * Coefficient[4]) + (vp[15] * Coefficient[5]) +
            (vp[14] * Coefficient[6]) + (vp[13] * Coefficient[7]) +
            (vp[12] * Coefficient[8]) + (vp[11] * Coefficient[9]) +
            (vp[10] * Coefficient[10]) + (vp[9] * Coefficient[11]) +
            (vp[8] * Coefficient[12]) + (vp[7] * Coefficient[13]) +
            (vp[6] * Coefficient[14]) + (vp[5] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    5:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[5] * Coefficient[0]) + (vp[4] * Coefficient[1]) +
            (vp[3] * Coefficient[2]) + (vp[2] * Coefficient[3]) +
            (vp[1] * Coefficient[4]) + (vp[0] * Coefficient[5]) +
            (vp[15] * Coefficient[6]) + (vp[14] * Coefficient[7]) +
            (vp[13] * Coefficient[8]) + (vp[12] * Coefficient[9]) +
            (vp[11] * Coefficient[10]) + (vp[10] * Coefficient[11]) +
            (vp[9] * Coefficient[12]) + (vp[8] * Coefficient[13]) +
            (vp[7] * Coefficient[14]) + (vp[6] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    6:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[6] * Coefficient[0]) + (vp[5] * Coefficient[1]) +
            (vp[4] * Coefficient[2]) + (vp[3] * Coefficient[3]) +
            (vp[2] * Coefficient[4]) + (vp[1] * Coefficient[5]) +
            (vp[0] * Coefficient[6]) + (vp[15] * Coefficient[7]) +
            (vp[14] * Coefficient[8]) + (vp[13] * Coefficient[9]) +
            (vp[12] * Coefficient[10]) + (vp[11] * Coefficient[11]) +
            (vp[10] * Coefficient[12]) + (vp[9] * Coefficient[13]) +
            (vp[8] * Coefficient[14]) + (vp[7] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    7:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[7] * Coefficient[0]) + (vp[6] * Coefficient[1]) +
            (vp[5] * Coefficient[2]) + (vp[4] * Coefficient[3]) +
            (vp[3] * Coefficient[4]) + (vp[2] * Coefficient[5]) +
            (vp[1] * Coefficient[6]) + (vp[0] * Coefficient[7]) +
            (vp[15] * Coefficient[8]) + (vp[14] * Coefficient[9]) +
            (vp[13] * Coefficient[10]) + (vp[12] * Coefficient[11]) +
            (vp[11] * Coefficient[12]) + (vp[10] * Coefficient[13]) +
            (vp[9] * Coefficient[14]) + (vp[8] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    8:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[8] * Coefficient[0]) + (vp[7] * Coefficient[1]) +
            (vp[6] * Coefficient[2]) + (vp[5] * Coefficient[3]) +
            (vp[4] * Coefficient[4]) + (vp[3] * Coefficient[5]) +
            (vp[2] * Coefficient[6]) + (vp[1] * Coefficient[7]) +
            (vp[0] * Coefficient[8]) + (vp[15] * Coefficient[9]) +
            (vp[14] * Coefficient[10]) + (vp[13] * Coefficient[11]) +
            (vp[12] * Coefficient[12]) + (vp[11] * Coefficient[13]) +
            (vp[10] * Coefficient[14]) + (vp[9] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    9:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[9] * Coefficient[0]) + (vp[8] * Coefficient[1]) +
            (vp[7] * Coefficient[2]) + (vp[6] * Coefficient[3]) +
            (vp[5] * Coefficient[4]) + (vp[4] * Coefficient[5]) +
            (vp[3] * Coefficient[6]) + (vp[2] * Coefficient[7]) +
            (vp[1] * Coefficient[8]) + (vp[0] * Coefficient[9]) +
            (vp[15] * Coefficient[10]) + (vp[14] * Coefficient[11]) +
            (vp[13] * Coefficient[12]) + (vp[12] * Coefficient[13]) +
            (vp[11] * Coefficient[14]) + (vp[10] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    10:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[10] * Coefficient[0]) + (vp[9] * Coefficient[1]) +
            (vp[8] * Coefficient[2]) + (vp[7] * Coefficient[3]) +
            (vp[6] * Coefficient[4]) + (vp[5] * Coefficient[5]) +
            (vp[4] * Coefficient[6]) + (vp[3] * Coefficient[7]) +
            (vp[2] * Coefficient[8]) + (vp[1] * Coefficient[9]) +
            (vp[0] * Coefficient[10]) + (vp[15] * Coefficient[11]) +
            (vp[14] * Coefficient[12]) + (vp[13] * Coefficient[13]) +
            (vp[12] * Coefficient[14]) + (vp[11] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    11:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[11] * Coefficient[0]) + (vp[10] * Coefficient[1]) +
            (vp[9] * Coefficient[2]) + (vp[8] * Coefficient[3]) +
            (vp[7] * Coefficient[4]) + (vp[6] * Coefficient[5]) +
            (vp[5] * Coefficient[6]) + (vp[4] * Coefficient[7]) +
            (vp[3] * Coefficient[8]) + (vp[2] * Coefficient[9]) +
            (vp[1] * Coefficient[10]) + (vp[0] * Coefficient[11]) +
            (vp[15] * Coefficient[12]) + (vp[14] * Coefficient[13]) +
            (vp[13] * Coefficient[14]) + (vp[12] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    12:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[12] * Coefficient[0]) + (vp[11] * Coefficient[1]) +
            (vp[10] * Coefficient[2]) + (vp[9] * Coefficient[3]) +
            (vp[8] * Coefficient[4]) + (vp[7] * Coefficient[5]) +
            (vp[6] * Coefficient[6]) + (vp[5] * Coefficient[7]) +
            (vp[4] * Coefficient[8]) + (vp[3] * Coefficient[9]) +
            (vp[2] * Coefficient[10]) + (vp[1] * Coefficient[11]) +
            (vp[0] * Coefficient[12]) + (vp[15] * Coefficient[13]) +
            (vp[14] * Coefficient[14]) + (vp[13] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    13:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[13] * Coefficient[0]) + (vp[12] * Coefficient[1]) +
            (vp[11] * Coefficient[2]) + (vp[10] * Coefficient[3]) +
            (vp[9] * Coefficient[4]) + (vp[8] * Coefficient[5]) +
            (vp[7] * Coefficient[6]) + (vp[6] * Coefficient[7]) +
            (vp[5] * Coefficient[8]) + (vp[4] * Coefficient[9]) +
            (vp[3] * Coefficient[10]) + (vp[2] * Coefficient[11]) +
            (vp[1] * Coefficient[12]) + (vp[0] * Coefficient[13]) +
            (vp[15] * Coefficient[14]) + (vp[14] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    14:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[14] * Coefficient[0]) + (vp[13] * Coefficient[1]) +
            (vp[12] * Coefficient[2]) + (vp[11] * Coefficient[3]) +
            (vp[10] * Coefficient[4]) + (vp[9] * Coefficient[5]) +
            (vp[8] * Coefficient[6]) + (vp[7] * Coefficient[7]) +
            (vp[6] * Coefficient[8]) + (vp[5] * Coefficient[9]) +
            (vp[4] * Coefficient[10]) + (vp[3] * Coefficient[11]) +
            (vp[2] * Coefficient[12]) + (vp[1] * Coefficient[13]) +
            (vp[0] * Coefficient[14]) + (vp[15] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;

    15:
      begin
        Coefficient := @CAnnex3B3Table;
        while (Cardinal(Coefficient) < Cardinal(@CAnnex3B3Table) + C2048) do
        begin
          PcmSample := ((vp[15] * Coefficient[0]) + (vp[14] * Coefficient[1]) +
            (vp[13] * Coefficient[2]) + (vp[12] * Coefficient[3]) +
            (vp[11] * Coefficient[4]) + (vp[10] * Coefficient[5]) +
            (vp[9] * Coefficient[6]) + (vp[8] * Coefficient[7]) +
            (vp[7] * Coefficient[8]) + (vp[6] * Coefficient[9]) +
            (vp[5] * Coefficient[10]) + (vp[4] * Coefficient[11]) +
            (vp[3] * Coefficient[12]) + (vp[2] * Coefficient[13]) +
            (vp[1] * Coefficient[14]) + (vp[0] * Coefficient[15]));
          FOnNewPCMSample(Self, PcmSample);
          Coefficient := @Coefficient[16];
          vp := @vp[16];
        end;
      end;
  end;
end;

procedure TSynthesisFilter.InputSample(Sample: Single; SubBandNumber: Cardinal);
begin
  FSample[SubBandNumber] := Sample;
end;

procedure TSynthesisFilter.Reset;
begin
  FillChar(FVector[0], Sizeof(FVector[0]), 0);
  FillChar(FVector[1], Sizeof(FVector[1]), 0);
  FillChar(FSample, Sizeof(FSample), 0);
  FActualVector := @FVector[0];
  FActualWritePos := 15;
end;

procedure CalculateCosTable;
const
  COne64th = 1 / 64;
{$IFNDEF FastCalculation}
  COne32th = 1 / 32;
  COne16th = 1 / 16;
  COne8th = 1 / 8;
  COne4th = 1 / 4;
{$ELSE}
var
  Position, Offset: TComplex64;
{$ENDIF}
begin
{$IFDEF FastCalculation}
  Position.Re := 1;
  Position.Im := 0;
  GetSinCos(Pi * COne64th, Offset.Im, Offset.Re);

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[30] := 0.5 / Position.Re;
  GCosTable[15] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[14] := 0.5 / Position.Re;
  GCosTable[7] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[29] := 0.5 / Position.Re;
  GCosTable[16] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[6] := 0.5 / Position.Re;
  GCosTable[3] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[28] := 0.5 / Position.Re;
  GCosTable[17] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[13] := 0.5 / Position.Re;
  GCosTable[8] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[27] := 0.5 / Position.Re;
  GCosTable[18] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[2] := 0.5 / Position.Re;
  GCosTable[1] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[26] := 0.5 / Position.Re;
  GCosTable[19] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[12] := 0.5 / Position.Re;
  GCosTable[9] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[25] := 0.5 / Position.Re;
  GCosTable[20] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[5] := 0.5 / Position.Re;
  GCosTable[4] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[24] := 0.5 / Position.Re;
  GCosTable[21] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[11] := 0.5 / Position.Re;
  GCosTable[10] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[23] := 0.5 / Position.Re;
  GCosTable[22] := 0.5 / Position.Im;

  ComplexMultiplyInplace64(Position, Offset);
  GCosTable[0] := 0.5 / Position.Re;

{$ELSE}
  GCosTable[0] := 1 / (2 * cos(Pi * COne4th));
  GCosTable[1] := 1 / (2 * cos(Pi * 3 * COne8th));
  GCosTable[2] := 1 / (2 * cos(Pi * COne8th));
  GCosTable[3] := 1 / (2 * cos(Pi * 7 * COne16th));
  GCosTable[4] := 1 / (2 * cos(Pi * 5 * COne16th));
  GCosTable[5] := 1 / (2 * cos(Pi * 3 * COne16th));
  GCosTable[6] := 1 / (2 * cos(Pi * COne16th));
  GCosTable[7] := 1 / (2 * cos(Pi * 15 * COne32th));
  GCosTable[8] := 1 / (2 * cos(Pi * 13 * COne32th));
  GCosTable[9] := 1 / (2 * cos(Pi * 11 * COne32th));
  GCosTable[10] := 1 / (2 * cos(Pi * 9 * COne32th));
  GCosTable[11] := 1 / (2 * cos(Pi * 7 * COne32th));
  GCosTable[12] := 1 / (2 * cos(Pi * 5 * COne32th));
  GCosTable[13] := 1 / (2 * cos(Pi * 3 * COne32th));
  GCosTable[14] := 1 / (2 * cos(Pi * COne32th));
  GCosTable[15] := 1 / (2 * cos(Pi * 31 * COne64th));
  GCosTable[16] := 1 / (2 * cos(Pi * 29 * COne64th));
  GCosTable[17] := 1 / (2 * cos(Pi * 27 * COne64th));
  GCosTable[18] := 1 / (2 * cos(Pi * 25 * COne64th));
  GCosTable[19] := 1 / (2 * cos(Pi * 23 * COne64th));
  GCosTable[20] := 1 / (2 * cos(Pi * 21 * COne64th));
  GCosTable[21] := 1 / (2 * cos(Pi * 19 * COne64th));
  GCosTable[22] := 1 / (2 * cos(Pi * 17 * COne64th));
  GCosTable[23] := 1 / (2 * cos(Pi * 15 * COne64th));
  GCosTable[24] := 1 / (2 * cos(Pi * 13 * COne64th));
  GCosTable[25] := 1 / (2 * cos(Pi * 11 * COne64th));
  GCosTable[26] := 1 / (2 * cos(Pi * 9 * COne64th));
  GCosTable[27] := 1 / (2 * cos(Pi * 7 * COne64th));
  GCosTable[28] := 1 / (2 * cos(Pi * 5 * COne64th));
  GCosTable[29] := 1 / (2 * cos(Pi * 3 * COne64th));
  GCosTable[30] := 1 / (2 * cos(Pi * COne64th));
{$ENDIF}
end;

initialization

CalculateCosTable;

end.
