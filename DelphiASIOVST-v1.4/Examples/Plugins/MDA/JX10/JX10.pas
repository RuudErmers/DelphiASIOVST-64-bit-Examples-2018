unit JX10;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, DAV_Types, DAV_VSTEffect,
  DAV_VSTCustomModule, DAV_VSTModule;

const
  cNumVoices = 32;

type
  TJX10DataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    function VSTModuleOutputProperties(Sender: TObject; const index: Integer; var vLabel, shortLabel: string; var SpeakerArrangement: TVstSpeakerArrangementType; var Flags: TVstPinPropertiesFlags): Boolean;
    procedure VSTModuleProcessMidi(Sender: TObject; MidiEvent: TVstMidiEvent);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLFO            : Single;
    FModWheel       : Single;
    FFiltWheel      : Single;
    FPress          : Single;
    FMode           : Integer;
    FInvSampleRate  : Single;
    FNoiseMix       : Single;
    FVolTrim        : Single;
    FSemi           : Single;
    FCent           : Single;
    FOscMix         : Single;
    FDetune         : Single;
    FTune           : Single;
    FVibrato        : Single;
    FPWDepth        : Single;
    FLFOHz          : Single;
    FDeltaLFO       : Single;
    FFilterFreq     : Single;
    FFilterQ        : Single;
    FFilterLFO      : Single;
    FFilterEnv      : Single;
    FFilterVel      : Single;
    FVelOff         : Integer;
    FVolume         : Single;
    FResonanceWheel : Single;
    FAttack         : Single;
    FDecay          : Single;
    FRelease        : Single;
    FSustain        : Single;
    FPitchBend      : Single;
    FPitchBendInv   : Single;
    procedure Update;
    procedure noteOn(Note, Velocity: Integer);
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math;

procedure TJX10DataModule.Update;  // Parameter Change
begin
 FMode     := Round(7.9 * Parameter[3]);
 FNoiseMix := sqr(Parameter[21]);
 FVolTrim  := (3.2 - Parameter[0] - 1.5 * FNoiseMix) * (1.5 - 0.5 * Parameter[7]);
 FNoiseMix := FNoiseMix * 0.06;
 FOscMix   := Parameter[0];

 FSemi     := trunc(48 * Parameter[1]) - 24;
 FCent     := 15.876 * Parameter[2] - 7.938;
 FCent     := 0.1 * trunc(sqr(FCent) * FCent);
 FDetune   := Power(1.059463094359, - FSemi - 0.01 * FCent);
 FTune     := -23.376 - 2 * Parameter[23] - 12 * trunc(Parameter[22] * 4.9);
 FTune     := SampleRate * Power(1.059463094359, FTune);

 FVibrato  := 0.2 * (Parameter[20] - 0.5) * (Parameter[20] - 0.5);
 FPWDepth    := FVibrato;
 if Parameter[20] < 0.5 then FVibrato := 0;

 FLFOHz    := exp(7 * Parameter[19] - 4);
(*
 FDeltaLFO := FLFOHz * (FInvSampleRate * 2 * Pi * KMAX);
*)

 FFilterFreq := 8 * Parameter[6] - 1.5;
 FFilterQ    := sqr(1 - Parameter[7]);        ////// + 0.02;
 FFilterLFO  := 2.5 * sqr(Parameter[9]);
 FFilterEnv  := 12 * Parameter[8] - 6;
 FFilterVel  := 0.1 * Parameter[10] - 0.05;
 if (Parameter[10] < 0.05) then
   begin
    FVelOff := 1;
    FFilterVel := 0;
   end
  else FVelOff := 0;

 FAttack  := 1 - exp(-FInvSampleRate * exp(5.5 - 7.5 * Parameter[15]));
 FDecay   := 1 - exp(-FInvSampleRate * exp(5.5 - 7.5 * Parameter[16]));
 FSustain := Parameter[17];
 FRelease := 1 - exp(-FInvSampleRate * exp(5.5 - 7.5 * Parameter[18]));
 if Parameter[18] < 0.01 then FRelease := 0.1; //extra fast release

(*
 FInvSampleRate := FInvSampleRate * KMAX; //lower update rate...

 fAtt := 1 - exp(-FInvSampleRate * exp(5.5 - 7.5 * Parameter[11]));
 fDec := 1 - exp(-FInvSampleRate * exp(5.5 - 7.5 * Parameter[12]));
 fSus := sqr(Parameter[13]);
 fRel := 1 - exp(-FInvSampleRate * exp(5.5 - 7.5 * Parameter[14]));

 if (Parameter[4] < 0.02)
  then fGlide := 1.0
  else fGlide := 1 - exp(-FInvSampleRate * exp(6 - 7 * Parameter[4]));
 fGlidedisp   := (6.604 * Parameter[5] - 3.302);
 fGlidedisp   := fGlidedisp * sqr(fGlidedisp);
*)
end;


procedure TJX10DataModule.VSTModuleOpen(Sender: TObject);
var
  i, v : Integer;
begin
 i := 0;
 with Programs[0] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.37; Parameter[ 2] := 0.25;
   Parameter[ 3] := 0.3;  Parameter[ 4] := 0.32; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.9;  Parameter[ 7] := 0.6;  Parameter[ 8] := 0.12;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.9;
   Parameter[12] := 0.89; Parameter[13] := 0.9;  Parameter[14] := 0.73;
   Parameter[15] := 0.0;  Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.71; Parameter[19] := 0.81; Parameter[20] := 0.65;
   Parameter[21] := 0.0;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[2] do
  begin
   Parameter[ 0] := 0.88; Parameter[ 1] := 0.51; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.49; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.46; Parameter[ 7] := 0.76; Parameter[ 8] := 0.69;
   Parameter[ 9] := 0.1;  Parameter[10] := 0.69; Parameter[11] := 1.0;
   Parameter[12] := 0.86; Parameter[13] := 0.76; Parameter[14] := 0.57;
   Parameter[15] := 0.3;  Parameter[16] := 0.8;  Parameter[17] := 0.68;
   Parameter[18] := 0.66; Parameter[19] := 0.79; Parameter[20] := 0.13;
   Parameter[21] := 0.25; Parameter[22] := 0.45; Parameter[23] := 0.5;
  end;

 with Programs[3] do
  begin
   Parameter[ 0] := 0.88; Parameter[ 1] := 0.51; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.16; Parameter[ 4] := 0.49; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.49; Parameter[ 7] := 0.82; Parameter[ 8] := 0.66;
   Parameter[ 9] := 0.08; Parameter[10] := 0.89; Parameter[11] := 0.85;
   Parameter[12] := 0.69; Parameter[13] := 0.76; Parameter[14] := 0.47;
   Parameter[15] := 0.12; Parameter[16] := 0.22; Parameter[17] := 0.55;
   Parameter[18] := 0.66; Parameter[19] := 0.89; Parameter[20] := 0.34;
   Parameter[21] := 0.0;  Parameter[22] := 1.0;  Parameter[23] := 0.5;
  end;
 with Programs[4] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.26; Parameter[ 2] := 0.14;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.3;  Parameter[ 7] := 0.25; Parameter[ 8] := 0.7;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.63; Parameter[11] := 0.0;
   Parameter[12] := 0.35; Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.0;  Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.3;  Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.5;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[5] do
  begin
   Parameter[ 0] := 0.41; Parameter[ 1] := 0.5;  Parameter[ 2] := 0.79;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.08; Parameter[ 5] := 0.32;
   Parameter[ 6] := 0.49; Parameter[ 7] := 0.01; Parameter[ 8] := 0.34;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.93; Parameter[11] := 0.61;
   Parameter[12] := 0.87; Parameter[13] := 1.0;  Parameter[14] := 0.93;
   Parameter[15] := 0.11; Parameter[16] := 0.48; Parameter[17] := 0.98;
   Parameter[18] := 0.32; Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[6] do
  begin
   Parameter[ 0] := 0.29; Parameter[ 1] := 0.76; Parameter[ 2] := 0.26;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.18; Parameter[ 5] := 0.76;
   Parameter[ 6] := 0.35; Parameter[ 7] := 0.15; Parameter[ 8] := 0.77;
   Parameter[ 9] := 0.14; Parameter[10] := 0.54; Parameter[11] := 0.0;
   Parameter[12] := 0.42; Parameter[13] := 0.13; Parameter[14] := 0.21;
   Parameter[15] := 0.0;  Parameter[16] := 0.56; Parameter[17] := 0.0;
   Parameter[18] := 0.32; Parameter[19] := 0.2;  Parameter[20] := 0.58;
   Parameter[21] := 0.22; Parameter[22] := 0.53; Parameter[23] := 0.5;
  end;
 with Programs[7] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.65; Parameter[ 2] := 0.24;
   Parameter[ 3] := 0.4;  Parameter[ 4] := 0.34; Parameter[ 5] := 0.85;
   Parameter[ 6] := 0.65; Parameter[ 7] := 0.63; Parameter[ 8] := 0.75;
   Parameter[ 9] := 0.16; Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.3;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.17; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.03; Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.68; Parameter[23] := 0.5;
  end;
 with Programs[8] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.5;
   Parameter[ 3] := 1.0;  Parameter[ 4] := 0.46; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.51; Parameter[ 7] := 0.0;  Parameter[ 8] := 0.5;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.0;  Parameter[11] := 0.0;
   Parameter[12] := 0.3;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.37; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.38; Parameter[19] := 0.81; Parameter[20] := 0.62;
   Parameter[21] := 0.0;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[9] do
  begin
   Parameter[ 0] := 0.84; Parameter[ 1] := 0.51; Parameter[ 2] := 0.15;
   Parameter[ 3] := 0.45; Parameter[ 4] := 0.41; Parameter[ 5] := 0.42;
   Parameter[ 6] := 0.54; Parameter[ 7] := 0.01; Parameter[ 8] := 0.58;
   Parameter[ 9] := 0.21; Parameter[10] := 0.67; Parameter[11] := 0.0;
   Parameter[12] := 0.09; Parameter[13] := 1.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.2;  Parameter[16] := 0.85; Parameter[17] := 1.0;
   Parameter[18] := 0.3;  Parameter[19] := 0.83; Parameter[20] := 0.09;
   Parameter[21] := 0.4;  Parameter[22] := 0.49; Parameter[23] := 0.5;
  end;
 with Programs[10] do
  begin
   Parameter[ 0] := 0.71; Parameter[ 1] := 0.75; Parameter[ 2] := 0.53;
   Parameter[ 3] := 0.18; Parameter[ 4] := 0.24; Parameter[ 5] := 1.0;
   Parameter[ 6] := 0.56; Parameter[ 7] := 0.52; Parameter[ 8] := 0.69;
   Parameter[ 9] := 0.19; Parameter[10] := 0.7;  Parameter[11] := 1.0;
   Parameter[12] := 0.14; Parameter[13] := 0.65; Parameter[14] := 0.95;
   Parameter[15] := 0.07; Parameter[16] := 0.91; Parameter[17] := 1.0;
   Parameter[18] := 0.15; Parameter[19] := 0.84; Parameter[20] := 0.33;
   Parameter[21] := 0.0;  Parameter[22] := 0.49; Parameter[23] := 0.5;
  end;
 with Programs[11] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.43;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.71; Parameter[ 5] := 0.48;
   Parameter[ 6] := 0.23; Parameter[ 7] := 0.77; Parameter[ 8] := 0.8;
   Parameter[ 9] := 0.32; Parameter[10] := 0.63; Parameter[11] := 0.4;
   Parameter[12] := 0.18; Parameter[13] := 0.66; Parameter[14] := 0.14;
   Parameter[15] := 0.0;  Parameter[16] := 0.38; Parameter[17] := 0.65;
   Parameter[18] := 0.16; Parameter[19] := 0.48; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.67; Parameter[23] := 0.5;
  end;
 with Programs[12] do
  begin
   Parameter[ 0] := 0.62; Parameter[ 1] := 0.26; Parameter[ 2] := 0.51;
   Parameter[ 3] := 0.79; Parameter[ 4] := 0.35; Parameter[ 5] := 0.54;
   Parameter[ 6] := 0.64; Parameter[ 7] := 0.39; Parameter[ 8] := 0.51;
   Parameter[ 9] := 0.65; Parameter[10] := 0.0;  Parameter[11] := 0.07;
   Parameter[12] := 0.52; Parameter[13] := 0.24; Parameter[14] := 0.84;
   Parameter[15] := 0.13; Parameter[16] := 0.3;  Parameter[17] := 0.76;
   Parameter[18] := 0.21; Parameter[19] := 0.58; Parameter[20] := 0.3;
   Parameter[21] := 0.0;  Parameter[22] := 0.36; Parameter[23] := 0.5;
  end;
 with Programs[13] do
  begin
   Parameter[ 0] := 0.81; Parameter[ 1] := 1.0;  Parameter[ 2] := 0.21;
   Parameter[ 3] := 0.78; Parameter[ 4] := 0.15; Parameter[ 5] := 0.35;
   Parameter[ 6] := 0.39; Parameter[ 7] := 0.17; Parameter[ 8] := 0.69;
   Parameter[ 9] := 0.4;  Parameter[10] := 0.62; Parameter[11] := 0.0;
   Parameter[12] := 0.47; Parameter[13] := 0.19; Parameter[14] := 0.37;
   Parameter[15] := 0.0;  Parameter[16] := 0.5;  Parameter[17] := 0.2;
   Parameter[18] := 0.33; Parameter[19] := 0.38; Parameter[20] := 0.53;
   Parameter[21] := 0.0;  Parameter[22] := 0.12; Parameter[23] := 0.5;
  end;
 with Programs[14] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.51; Parameter[ 2] := 0.52;
   Parameter[ 3] := 0.96; Parameter[ 4] := 0.44; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.41; Parameter[ 7] := 0.46; Parameter[ 8] := 0.5;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.0;  Parameter[11] := 0.0;
   Parameter[12] := 0.0;  Parameter[13] := 1.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.15; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.32; Parameter[19] := 0.81; Parameter[20] := 0.49;
   Parameter[21] := 0.0;  Parameter[22] := 0.83; Parameter[23] := 0.5;
  end;
 with Programs[15] do
  begin
   Parameter[ 0] := 0.48; Parameter[ 1] := 0.51; Parameter[ 2] := 0.22;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.0;  Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.5;  Parameter[ 7] := 0.47; Parameter[ 8] := 0.73;
   Parameter[ 9] := 0.3;  Parameter[10] := 0.8;  Parameter[11] := 0.0;
   Parameter[12] := 0.1;  Parameter[13] := 0.0;  Parameter[14] := 0.07;
   Parameter[15] := 0.0;  Parameter[16] := 0.42; Parameter[17] := 0.0;
   Parameter[18] := 0.22; Parameter[19] := 0.21; Parameter[20] := 0.59;
   Parameter[21] := 0.16; Parameter[22] := 0.98; Parameter[23] := 0.5;
  end;
 with Programs[16] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.51; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.83; Parameter[ 4] := 0.49; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.55; Parameter[ 7] := 0.75; Parameter[ 8] := 0.69;
   Parameter[ 9] := 0.35; Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.56; Parameter[13] := 0.0;  Parameter[14] := 0.56;
   Parameter[15] := 0.0;  Parameter[16] := 0.8;  Parameter[17] := 1.0;
   Parameter[18] := 0.24; Parameter[19] := 0.26; Parameter[20] := 0.49;
   Parameter[21] := 0.0;  Parameter[22] := 0.07; Parameter[23] := 0.5;
  end;
 with Programs[17] do
  begin
   Parameter[ 0] := 0.75; Parameter[ 1] := 0.51; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.83; Parameter[ 4] := 0.49; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.55; Parameter[ 7] := 0.75; Parameter[ 8] := 0.69;
   Parameter[ 9] := 0.35; Parameter[10] := 0.5;  Parameter[11] := 0.14;
   Parameter[12] := 0.49; Parameter[13] := 0.0;  Parameter[14] := 0.39;
   Parameter[15] := 0.0;  Parameter[16] := 0.8;  Parameter[17] := 1.0;
   Parameter[18] := 0.24; Parameter[19] := 0.26; Parameter[20] := 0.49;
   Parameter[21] := 0.0;  Parameter[22] := 0.07; Parameter[23] := 0.5;
  end;
 with Programs[18] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.2;
   Parameter[ 3] := 0.81; Parameter[ 4] := 0.19; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.3;  Parameter[ 7] := 0.51; Parameter[ 8] := 0.85;
   Parameter[ 9] := 0.09; Parameter[10] := 0.0;  Parameter[11] := 0.0;
   Parameter[12] := 0.88; Parameter[13] := 0.0;  Parameter[14] := 0.21;
   Parameter[15] := 0.0;  Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.46; Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.27; Parameter[23] := 0.5;
  end;
 with Programs[19] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.2;
   Parameter[ 3] := 0.72; Parameter[ 4] := 0.19; Parameter[ 5] := 0.86;
   Parameter[ 6] := 0.48; Parameter[ 7] := 0.43; Parameter[ 8] := 0.94;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.8;  Parameter[11] := 0.0;
   Parameter[12] := 0.0;  Parameter[13] := 0.0;  Parameter[14] := 0.0;
   Parameter[15] := 0.0;  Parameter[16] := 0.61; Parameter[17] := 1.0;
   Parameter[18] := 0.32; Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.27; Parameter[23] := 0.5;
  end;
 with Programs[20] do
  begin
   Parameter[ 0] := 0.97; Parameter[ 1] := 0.26; Parameter[ 2] := 0.3;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.8;  Parameter[ 7] := 0.4;  Parameter[ 8] := 0.52;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.77; Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.0;  Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.3;  Parameter[19] := 0.81; Parameter[20] := 0.16;
   Parameter[21] := 0.0;  Parameter[22] := 0.0;  Parameter[23] := 0.5;
  end;
 with Programs[21] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.65; Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.33; Parameter[ 7] := 0.76; Parameter[ 8] := 0.53;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.3;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.0;  Parameter[16] := 0.55; Parameter[17] := 0.25;
   Parameter[18] := 0.3;  Parameter[19] := 0.81; Parameter[20] := 0.52;
   Parameter[21] := 0.0;  Parameter[22] := 0.14; Parameter[23] := 0.5;
  end;
 with Programs[22] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.26; Parameter[ 2] := 0.22;
   Parameter[ 3] := 0.64; Parameter[ 4] := 0.82; Parameter[ 5] := 0.59;
   Parameter[ 6] := 0.72; Parameter[ 7] := 0.47; Parameter[ 8] := 0.34;
   Parameter[ 9] := 0.34; Parameter[10] := 0.82; Parameter[11] := 0.2;
   Parameter[12] := 0.69; Parameter[13] := 1.0;  Parameter[14] := 0.15;
   Parameter[15] := 0.09; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.07; Parameter[19] := 0.81; Parameter[20] := 0.46;
   Parameter[21] := 0.0;  Parameter[22] := 0.24; Parameter[23] := 0.5;
  end;
 with Programs[23] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.26; Parameter[ 2] := 0.22;
   Parameter[ 3] := 0.71; Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.67; Parameter[ 7] := 0.7;  Parameter[ 8] := 0.26;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.48;
   Parameter[12] := 0.69; Parameter[13] := 1.0;  Parameter[14] := 0.15;
   Parameter[15] := 0.0;  Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.07; Parameter[19] := 0.81; Parameter[20] := 0.46;
   Parameter[21] := 0.0;  Parameter[22] := 0.24; Parameter[23] := 0.5;
  end;
 with Programs[24] do
  begin
   Parameter[ 0] := 0.49; Parameter[ 1] := 0.25; Parameter[ 2] := 0.66;
   Parameter[ 3] := 0.81; Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.36; Parameter[ 7] := 0.15; Parameter[ 8] := 0.75;
   Parameter[ 9] := 0.2;  Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.38; Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.0;  Parameter[16] := 0.6;  Parameter[17] := 1.0;
   Parameter[18] := 0.22; Parameter[19] := 0.19; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.17; Parameter[23] := 0.5;
  end;
 with Programs[25] do
  begin
   Parameter[ 0] := 0.37; Parameter[ 1] := 0.51; Parameter[ 2] := 0.77;
   Parameter[ 3] := 0.71; Parameter[ 4] := 0.22; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.33; Parameter[ 7] := 0.47; Parameter[ 8] := 0.71;
   Parameter[ 9] := 0.16; Parameter[10] := 0.59; Parameter[11] := 0.0;
   Parameter[12] := 0.0;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.04; Parameter[16] := 0.58; Parameter[17] := 0.0;
   Parameter[18] := 0.22; Parameter[19] := 0.15; Parameter[20] := 0.44;
   Parameter[21] := 0.33; Parameter[22] := 0.15; Parameter[23] := 0.5;
  end;
 with Programs[26] do
  begin
   Parameter[ 0] := 0.5;  Parameter[ 1] := 0.51; Parameter[ 2] := 0.17;
   Parameter[ 3] := 0.8;  Parameter[ 4] := 0.34; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.51; Parameter[ 7] := 0.0;  Parameter[ 8] := 0.58;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.67; Parameter[11] := 0.0;
   Parameter[12] := 0.09; Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.2;  Parameter[16] := 0.85; Parameter[17] := 0.0;
   Parameter[18] := 0.3;  Parameter[19] := 0.81; Parameter[20] := 0.7;
   Parameter[21] := 0.0;  Parameter[22] := 0.0;  Parameter[23] := 0.5;
  end;
 with Programs[27] do
  begin
   Parameter[ 0] := 0.23; Parameter[ 1] := 0.51; Parameter[ 2] := 0.38;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.33; Parameter[ 7] := 1.0;  Parameter[ 8] := 0.5;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.29; Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.68; Parameter[16] := 0.39; Parameter[17] := 0.58;
   Parameter[18] := 0.36; Parameter[19] := 0.81; Parameter[20] := 0.64;
   Parameter[21] := 0.38; Parameter[22] := 0.92; Parameter[23] := 0.5;
  end;
 with Programs[28] do
  begin
   Parameter[ 0] := 0.39; Parameter[ 1] := 0.51; Parameter[ 2] := 0.27;
   Parameter[ 3] := 0.38; Parameter[ 4] := 0.12; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.35; Parameter[ 7] := 0.78; Parameter[ 8] := 0.5;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.3;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.35; Parameter[16] := 0.5;  Parameter[17] := 0.8;
   Parameter[18] := 0.7;  Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[29] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.23; Parameter[ 7] := 0.2;  Parameter[ 8] := 0.75;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.22; Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.0;  Parameter[16] := 0.47; Parameter[17] := 0.0;
   Parameter[18] := 0.3;  Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.8;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[30] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.51; Parameter[ 2] := 0.24;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.0;  Parameter[ 5] := 0.35;
   Parameter[ 6] := 0.42; Parameter[ 7] := 0.26; Parameter[ 8] := 0.75;
   Parameter[ 9] := 0.14; Parameter[10] := 0.69; Parameter[11] := 0.0;
   Parameter[12] := 0.67; Parameter[13] := 0.55; Parameter[14] := 0.97;
   Parameter[15] := 0.82; Parameter[16] := 0.7;  Parameter[17] := 1.0;
   Parameter[18] := 0.42; Parameter[19] := 0.84; Parameter[20] := 0.67;
   Parameter[21] := 0.3;  Parameter[22] := 0.47; Parameter[23] := 0.5;
  end;
 with Programs[31] do
  begin
   Parameter[ 0] := 0.75; Parameter[ 1] := 0.51; Parameter[ 2] := 0.29;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.49; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.55; Parameter[ 7] := 0.16; Parameter[ 8] := 0.69;
   Parameter[ 9] := 0.08; Parameter[10] := 0.2;  Parameter[11] := 0.76;
   Parameter[12] := 0.29; Parameter[13] := 0.76; Parameter[14] := 1.0;
   Parameter[15] := 0.46; Parameter[16] := 0.8;  Parameter[17] := 1.0;
   Parameter[18] := 0.39; Parameter[19] := 0.79; Parameter[20] := 0.27;
   Parameter[21] := 0.0;  Parameter[22] := 0.68; Parameter[23] := 0.5;
  end;
 with Programs[32] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.5;  Parameter[ 2] := 0.53;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.13; Parameter[ 5] := 0.39;
   Parameter[ 6] := 0.38; Parameter[ 7] := 0.74; Parameter[ 8] := 0.54;
   Parameter[ 9] := 0.2;  Parameter[10] := 0.0;  Parameter[11] := 0.0;
   Parameter[12] := 0.55; Parameter[13] := 0.52; Parameter[14] := 0.31;
   Parameter[15] := 0.0;  Parameter[16] := 0.17; Parameter[17] := 0.73;
   Parameter[18] := 0.28; Parameter[19] := 0.87; Parameter[20] := 0.24;
   Parameter[21] := 0.0;  Parameter[22] := 0.29; Parameter[23] := 0.5;
  end;
 with Programs[33] do
  begin
   Parameter[ 0] := 0.5;  Parameter[ 1] := 0.77; Parameter[ 2] := 0.52;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.44; Parameter[ 7] := 0.5;  Parameter[ 8] := 0.65;
   Parameter[ 9] := 0.16; Parameter[10] := 0.0;  Parameter[11] := 0.0;
   Parameter[12] := 0.0;  Parameter[13] := 0.18; Parameter[14] := 0.0;
   Parameter[15] := 0.0;  Parameter[16] := 0.75; Parameter[17] := 0.8;
   Parameter[18] := 0.0;  Parameter[19] := 0.81; Parameter[20] := 0.49;
   Parameter[21] := 0.0;  Parameter[22] := 0.44; Parameter[23] := 0.5;
  end;
 with Programs[34] do
  begin
   Parameter[ 0] := 0.89; Parameter[ 1] := 0.91; Parameter[ 2] := 0.37;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.51; Parameter[ 7] := 0.62; Parameter[ 8] := 0.54;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.0;  Parameter[11] := 0.0;
   Parameter[12] := 0.37; Parameter[13] := 0.0;  Parameter[14] := 1.0;
   Parameter[15] := 0.04; Parameter[16] := 0.08; Parameter[17] := 0.72;
   Parameter[18] := 0.04; Parameter[19] := 0.77; Parameter[20] := 0.49;
   Parameter[21] := 0.0;  Parameter[22] := 0.58; Parameter[23] := 0.5;
  end;
 with Programs[35] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.51; Parameter[ 2] := 0.51;
   Parameter[ 3] := 0.37; Parameter[ 4] := 0.0;  Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.51; Parameter[ 7] := 0.1;  Parameter[ 8] := 0.5;
   Parameter[ 9] := 0.11; Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.0;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.35; Parameter[16] := 0.65; Parameter[17] := 0.65;
   Parameter[18] := 0.32; Parameter[19] := 0.79; Parameter[20] := 0.49;
   Parameter[21] := 0.2;  Parameter[22] := 0.35; Parameter[23] := 0.5;
  end;
 with Programs[36] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.51; Parameter[ 2] := 0.51;
   Parameter[ 3] := 0.82; Parameter[ 4] := 0.06; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.57; Parameter[ 7] := 0.0;  Parameter[ 8] := 0.32;
   Parameter[ 9] := 0.15; Parameter[10] := 0.5;  Parameter[11] := 0.21;
   Parameter[12] := 0.15; Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.24; Parameter[16] := 0.6;  Parameter[17] := 0.8;
   Parameter[18] := 0.1;  Parameter[19] := 0.75; Parameter[20] := 0.55;
   Parameter[21] := 0.25; Parameter[22] := 0.69; Parameter[23] := 0.5;
  end;
 with Programs[37] do
  begin
   Parameter[ 0] := 0.12; Parameter[ 1] := 0.9;  Parameter[ 2] := 0.67;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.5;  Parameter[ 7] := 0.21; Parameter[ 8] := 0.29;
   Parameter[ 9] := 0.12; Parameter[10] := 0.6;  Parameter[11] := 0.0;
   Parameter[12] := 0.35; Parameter[13] := 0.36; Parameter[14] := 0.25;
   Parameter[15] := 0.08; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.27; Parameter[19] := 0.83; Parameter[20] := 0.51;
   Parameter[21] := 0.1;  Parameter[22] := 0.25; Parameter[23] := 0.5;
  end;
 with Programs[38] do
  begin
   Parameter[ 0] := 0.43; Parameter[ 1] := 0.76; Parameter[ 2] := 0.23;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.28; Parameter[ 5] := 0.36;
   Parameter[ 6] := 0.5;  Parameter[ 7] := 0.0;  Parameter[ 8] := 0.59;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.24;
   Parameter[12] := 0.16; Parameter[13] := 0.91; Parameter[14] := 0.08;
   Parameter[15] := 0.17; Parameter[16] := 0.5;  Parameter[17] := 0.8;
   Parameter[18] := 0.45; Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.0;  Parameter[22] := 0.58; Parameter[23] := 0.5;
  end;
 with Programs[39] do
  begin
   Parameter[ 0] := 0.4;  Parameter[ 1] := 0.51; Parameter[ 2] := 0.25;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.3;  Parameter[ 5] := 0.28;
   Parameter[ 6] := 0.39; Parameter[ 7] := 0.15; Parameter[ 8] := 0.75;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.39;
   Parameter[12] := 0.3;  Parameter[13] := 0.82; Parameter[14] := 0.25;
   Parameter[15] := 0.33; Parameter[16] := 0.74; Parameter[17] := 0.76;
   Parameter[18] := 0.41; Parameter[19] := 0.81; Parameter[20] := 0.47;
   Parameter[21] := 0.23; Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[40] do
  begin
   Parameter[ 0] := 0.68; Parameter[ 1] := 0.5;  Parameter[ 2] := 0.93;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.31; Parameter[ 5] := 0.62;
   Parameter[ 6] := 0.26; Parameter[ 7] := 0.07; Parameter[ 8] := 0.85;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.66; Parameter[11] := 0.0;
   Parameter[12] := 0.83; Parameter[13] := 0.0;  Parameter[14] := 0.05;
   Parameter[15] := 0.0;  Parameter[16] := 0.75; Parameter[17] := 0.54;
   Parameter[18] := 0.32; Parameter[19] := 0.76; Parameter[20] := 0.37;
   Parameter[21] := 0.29; Parameter[22] := 0.56; Parameter[23] := 0.5;
  end;
 with Programs[41] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.27; Parameter[ 2] := 0.22;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.82; Parameter[ 7] := 0.13; Parameter[ 8] := 0.75;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.0;  Parameter[11] := 0.24;
   Parameter[12] := 0.3;  Parameter[13] := 0.88; Parameter[14] := 0.34;
   Parameter[15] := 0.0;  Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.48; Parameter[19] := 0.71; Parameter[20] := 0.37;
   Parameter[21] := 0.0;  Parameter[22] := 0.35; Parameter[23] := 0.5;
  end;
 with Programs[42] do
  begin
   Parameter[ 0] := 0.76; Parameter[ 1] := 0.51; Parameter[ 2] := 0.35;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.49; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.87; Parameter[ 7] := 0.67; Parameter[ 8] := 1.0;
   Parameter[ 9] := 0.32; Parameter[10] := 0.09; Parameter[11] := 0.95;
   Parameter[12] := 0.56; Parameter[13] := 0.72; Parameter[14] := 1.0;
   Parameter[15] := 0.04; Parameter[16] := 0.76; Parameter[17] := 0.11;
   Parameter[18] := 0.46; Parameter[19] := 0.88; Parameter[20] := 0.72;
   Parameter[21] := 0.0;  Parameter[22] := 0.38; Parameter[23] := 0.5;
  end;
 with Programs[43] do
  begin
   Parameter[ 0] := 0.75; Parameter[ 1] := 0.51; Parameter[ 2] := 0.24;
   Parameter[ 3] := 0.45; Parameter[ 4] := 0.16; Parameter[ 5] := 0.48;
   Parameter[ 6] := 0.38; Parameter[ 7] := 0.58; Parameter[ 8] := 0.75;
   Parameter[ 9] := 0.16; Parameter[10] := 0.81; Parameter[11] := 0.0;
   Parameter[12] := 0.3;  Parameter[13] := 0.4;  Parameter[14] := 0.31;
   Parameter[15] := 0.37; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.54; Parameter[19] := 0.85; Parameter[20] := 0.83;
   Parameter[21] := 0.43; Parameter[22] := 0.46; Parameter[23] := 0.5;
  end;
 with Programs[44] do
  begin
   Parameter[ 0] := 0.31; Parameter[ 1] := 0.51; Parameter[ 2] := 0.43;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.34; Parameter[ 7] := 0.26; Parameter[ 8] := 0.53;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.63; Parameter[11] := 0.0;
   Parameter[12] := 0.22; Parameter[13] := 0.0;  Parameter[14] := 0.39;
   Parameter[15] := 0.0;  Parameter[16] := 0.8;  Parameter[17] := 0.0;
   Parameter[18] := 0.44; Parameter[19] := 0.81; Parameter[20] := 0.51;
   Parameter[21] := 0.0;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[45] do
  begin
   Parameter[ 0] := 0.72; Parameter[ 1] := 0.82; Parameter[ 2] := 1.0;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.37; Parameter[ 7] := 0.47; Parameter[ 8] := 0.54;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.0;
   Parameter[12] := 0.45; Parameter[13] := 0.0;  Parameter[14] := 0.39;
   Parameter[15] := 0.0;  Parameter[16] := 0.39; Parameter[17] := 0.0;
   Parameter[18] := 0.48; Parameter[19] := 0.81; Parameter[20] := 0.6;
   Parameter[21] := 0.0;  Parameter[22] := 0.71; Parameter[23] := 0.5;
  end;
 with Programs[46] do
  begin
   Parameter[ 0] := 0.81; Parameter[ 1] := 0.76; Parameter[ 2] := 0.19;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.18; Parameter[ 5] := 0.7;
   Parameter[ 6] := 0.4;  Parameter[ 7] := 0.3;  Parameter[ 8] := 0.54;
   Parameter[ 9] := 0.17; Parameter[10] := 0.4;  Parameter[11] := 0.0;
   Parameter[12] := 0.42; Parameter[13] := 0.23; Parameter[14] := 0.47;
   Parameter[15] := 0.12; Parameter[16] := 0.48; Parameter[17] := 0.0;
   Parameter[18] := 0.49; Parameter[19] := 0.53; Parameter[20] := 0.36;
   Parameter[21] := 0.34; Parameter[22] := 0.56; Parameter[23] := 0.5;
  end;
 with Programs[58] do
  begin
   Parameter[ 0] := 0.57; Parameter[ 1] := 0.49; Parameter[ 2] := 0.31;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.46; Parameter[ 7] := 0.0;  Parameter[ 8] := 0.68;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.46;
   Parameter[12] := 0.3;  Parameter[13] := 1.0;  Parameter[14] := 0.23;
   Parameter[15] := 0.3;  Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.31; Parameter[19] := 1.0;  Parameter[20] := 0.38;
   Parameter[21] := 0.0;  Parameter[22] := 0.5;  Parameter[23] := 0.5;
  end;
 with Programs[59] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.08; Parameter[ 7] := 0.36; Parameter[ 8] := 0.69;
   Parameter[ 9] := 1.0;  Parameter[10] := 0.5;  Parameter[11] := 1.0;
   Parameter[12] := 1.0;  Parameter[13] := 0.0;  Parameter[14] := 1.0;
   Parameter[15] := 0.96; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.92; Parameter[19] := 0.97; Parameter[20] := 0.5;
   Parameter[21] := 1.0;  Parameter[22] := 0.0;  Parameter[23] := 0.5;
  end;
 with Programs[60] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.16; Parameter[ 7] := 0.85; Parameter[ 8] := 0.5;
   Parameter[ 9] := 0.28; Parameter[10] := 0.5;  Parameter[11] := 0.37;
   Parameter[12] := 0.3;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.89; Parameter[16] := 0.5;  Parameter[17] := 1.0;
   Parameter[18] := 0.89; Parameter[19] := 0.24; Parameter[20] := 0.5;
   Parameter[21] := 1.0;  Parameter[22] := 1.0;  Parameter[23] := 0.5;
  end;
 with Programs[61] do
  begin
   Parameter[ 0] := 1.0;  Parameter[ 1] := 0.37; Parameter[ 2] := 0.51;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.35; Parameter[ 5] := 0.5;
   Parameter[ 6] := 0.0;  Parameter[ 7] := 1.0;  Parameter[ 8] := 0.97;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.02;
   Parameter[12] := 0.2;  Parameter[13] := 0.0;  Parameter[14] := 0.2;
   Parameter[15] := 0.0;  Parameter[16] := 0.46; Parameter[17] := 0.0;
   Parameter[18] := 0.3;  Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.78; Parameter[22] := 0.48; Parameter[23] := 0.5;
  end;
 with Programs[62] do
  begin
   Parameter[ 0] := 0.0;  Parameter[ 1] := 0.25; Parameter[ 2] := 0.5;
   Parameter[ 3] := 0.0;  Parameter[ 4] := 0.76; Parameter[ 5] := 0.94;
   Parameter[ 6] := 0.3;  Parameter[ 7] := 0.33; Parameter[ 8] := 0.76;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.68; Parameter[11] := 0.0;
   Parameter[12] := 0.59; Parameter[13] := 0.0;  Parameter[14] := 0.59;
   Parameter[15] := 0.1;  Parameter[16] := 0.5;  Parameter[17] := 0.0;
   Parameter[18] := 0.5;  Parameter[19] := 0.81; Parameter[20] := 0.5;
   Parameter[21] := 0.7;  Parameter[22] := 0.0;  Parameter[23] := 0.5;
  end;
 with Programs[63] do
  begin
   Parameter[ 0] := 0.5;  Parameter[ 1] := 0.41; Parameter[ 2] := 0.23;
   Parameter[ 3] := 0.45; Parameter[ 4] := 0.77; Parameter[ 5] := 0.0;
   Parameter[ 6] := 0.4;  Parameter[ 7] := 0.65; Parameter[ 8] := 0.95;
   Parameter[ 9] := 0.0;  Parameter[10] := 0.5;  Parameter[11] := 0.33;
   Parameter[12] := 0.5;  Parameter[13] := 0.0;  Parameter[14] := 0.25;
   Parameter[15] := 0.0;  Parameter[16] := 0.7;  Parameter[17] := 0.65;
   Parameter[18] := 0.18; Parameter[19] := 0.32; Parameter[20] := 1.0;
   Parameter[21] := 0.0;  Parameter[22] := 0.06; Parameter[23] := 0.5;
  end;

 //initialise...
 for v := 0 to cNumVoices - 1 do
  begin
(*
   fVoices[v].dp    := 1;
   fVoices[v].dp2   := 1;
   fVoices[v].saw   := 0;
   fVoices[v].p     := 0;
   fVoices[v].p2    := 0;
   fVoices[v].env   := 0;
   fVoices[v].envd  := 0;
   fVoices[v].envl  := 0;
   fVoices[v].fenv  := 0;
   fVoices[v].fenvd := 0;
   fVoices[v].fenvl := 0;
   fVoices[v].f0    := 0;
   fVoices[v].f1    := 0;
   fVoices[v].f2    := 0;
   fVoices[v].note  := 0;
*)
  end;
(*
  notes[0] := EVENTS_DONE;
*)
  FLFO          := 0;
  FModWheel     := 0;
  FFiltWheel    := 0;
  FPress        := 0;
(*
  fzip          := 0;
  fRezWheel     := 1;
  FPitchBend    := 1;
  ipbend        := 1;
  FVolume       := 0.0005;
  K             := 0;
  FMode         := 0;
  fLastNote     := 0;
  FSustain      := 0;
  fActiveVoices := 0;
  fNoise        := 22222;
*)

 Update;
 VSTModuleSuspend(Sender);
end;

function TJX10DataModule.VSTModuleOutputProperties(Sender: TObject;
  const Index : Integer; var vLabel, shortLabel: string;
  var SpeakerArrangement: TVstSpeakerArrangementType;
  var Flags: TVstPinPropertiesFlags): Boolean;
begin
 result := False;
 if (index < numOutputs) then
  begin
    vLabel := 'JX10 Channel ' + IntToStr(Index + 1);
    Flags  := [vppIsActive];
    if index < 2 then Flags := Flags + [vppIsStereo]; //make channel 1+2 stereo
    result := True;
  end;
end;

procedure TJX10DataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 Update;
end;

procedure TJX10DataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  event, frame, frames, v     : Integer;
  o, e, vib, pwm, pb, ipb, gl : Single;
  x, y, hpf, min, w, ww       : Single;
  ff, fe, fq, fx, fz          : Single;
  k                           : Integer;
  r                           : Cardinal;
begin
(*
  float* out1 = outputs[0];
  float* out2 = outputs[1];
  long event=0, frame=0, frames, v;
  pb   := FPitchBend;
  ipb  := ipbend;
  gl   := fGlide;
  hpf  := 0.997;
  min  := 1;
  w    := 0;
  ww   := FNoiseMix;
  fe   := FFilterEnv;
  fq   := FFilterQ * fRezWheel;
  fx   := 1.97 - 0.85 * fq;
  fz   := fzip;
  k    := K;
*)
  vib := sin(FLFO);
  ff  := FFilterFreq + FFiltWheel + (FFilterLFO + FPress) * vib; //have to do again here as way that
  pwm := 1 + vib * (FModWheel + FPWDepth);           //below triggers on k was too cheap!
  vib := 1 + vib * (FModWheel + FVibrato);

(*
  if ((fActiveVoices > 0) or (notes[event] < sampleFrames))
   begin
    while (frame < sampleFrames) do
     begin
      frames := notes[event++];
      if (frames > sampleFrames)
       then frames := sampleFrames;
      frames := frames - frame;
      frame  := frame + frames;

      while (--frames >= 0) do
       begin
        fVoices *V = fVoices;
        o := 0.0;

        fNoise := (fNoise * 196314165) + 907633515;
        r := (fNoise and 0x7FFFFF) + 0x40000000; //generate fNoise + fast convert to float
        w := *(float * )&r;
        w := ww * (w - 3);

        dec(k);
        if (k < 0) then
         begin
          FLFO := FLFO + FDeltaLFO;
          if FLFO > PI then FLFO := FLFO - 2 * Pi;
          vib := sin(FLFO);
          ff  := FFilterFreq + FFiltWheel + (FFilterLFO + FPress) * vib;
          pwm := 1 + vib * (FModWheel + FPWDepth);
          vib := 1 + vib * (FModWheel + FVibrato);
          k   := KMAX;
         end;

        for v := 0 to cNumVoices - 1 do  //for each voices
         begin
          e := V.env;
          if (e > cSilence)
          begin // Sinc-Loop Oscillator
            x := V.p + V.dp;
            if (x > min) then
             begin
              if(x > V.pmax) then
               begin
                x := V.pmax + V.pmax - x;
                V.dp := -V.dp;
               end;
              V.p := x;
              x   := V.sin0 * V.sinx - V.sin1; //sine osc
              V.sin1 := V.sin0;
              V.sin0 := x;
              x := x / V.p;
             end;
            else
             begin
              x      := - x;
              V.p    := x;
              V.dp   := V.period * vib * pb; //set period for next cycle
              V.pmax := trunc(0.5 + V.dp) - 0.5;
              V.dc   := -0.5 * V.lev / V.pmax;
              V.pmax := V.pmax * PI;
              V.dp   := V.pmax / V.dp;
              V.sin0 := V.lev * sin(x);
              V.sin1 := V.lev * sin(x - V.dp);
              V.sinx := 2 * cos(V.dp);
              if (x * x > 0.1)                     // was 0.01;
               then x := V.sin0 / x
               else x := V.lev;
             end;

            y := V.p2 + V.dp2; //osc2
            if (y > min) then
             begin
              if (y > V.pmax2) then
               begin
                y := V.pmax2 + V.pmax2 - y;
                V.dp2 := -V.dp2;
               end;
              V.p2 := y;
              y := V.sin02 * V.sinx2 - V.sin12;
              V.sin12 := V.sin02;
              V.sin02 := y;
              y := y / V.p2;
             end
            else
             begin
              y       := - y;
              V.p2    := y;
              V.dp2   := V.period * V.FDetune * pwm * pb;
              V.pmax2 := trunc(0.5 + V.dp2) - 0.5;
              V.dc2   := -0.5 * V.lev2 / V.pmax2;
              V.pmax2 := V.pmax2 * PI;
              V.dp2   := V.pmax2 / V.dp2;
              V.sin02 := V.lev2 * sin(y);
              V.sin12 := V.lev2 * sin(y - V.dp2);
              V.sinx2 := 2 * cos(V.dp2);
              if (y * y > 0.1)
               then y := V.sin02 / y
               else y := V.lev2;
             end;
            V.saw := V.saw * hpf + V.dc + x - V.dc2 - y;  //integrated sinc = saw
            x := V.saw + w;
            V.env := V.env + V.envd * (V.envl - V.env);

            if (k = KMAX) then //filter freq update at FLFO rate
             begin
              if V.env + V.envl > 3 then
               begin
                V.envd := FDecay;
                V.envl := FSustain;
               end; //envelopes
              V.fenv := V.fenv + V.fenvd * (V.fenvl - V.fenv);
             if V.fenv + V.fenvl > 3 then
              begin
               V.fenvd := fdec;
               V.fenvl := fsus;
              end;

             fz := fz + 0.005 * (ff - fz); // Filter zipper fNoise filter
             y := V.fc * exp(fz + fe * V.fenv) * ipb; // Filter cutoff
             if (y < 0.005) then y := 0.005;
             V.ff := y;
 
             V.period := V.period + gl * (V.target - V.period); // Glide
             if V.target < V.period
              then V.period := V.period + gl * (V.target - V.period);
            end;

            if V.ff > fx then V.ff := fx; // stability limit

            V.f0 := V.f0 + V.ff * V.f1; //state-variable filter
            V.f1 := V.f1 - V.ff * (V.f0 + fq * V.f1 - x - V.f2);
            V.f1 := V.f1 - 0.2 * sqr(V.f1) * V.f1; //soft limit

            V.f2 := x;

            o := o + V.env * V.f0;
          end;
         inc(V);
        end;

        *out1++ = o;
        *out2++ = o;
      end;

      if (frame < sampleFrames) then
       begin
        long note = notes[event++];
        long vel  = notes[event++];
        noteOn(note, vel);
       end;
    end;
  
    fActiveVoices := cNumVoices;
    for v := 0 to cNumVoices - 1 do
     begin
      if fVoices[v].env < cSilence then  // choke voices
       begin
        fVoices[v].env  := 0;
        fVoices[v].envl := 0;
        fVoices[v].f0   := 0;
        fVoices[v].f1   := 0;
        fVoices[v].f2   := 0;
        dec(fActiveVoices);
       end;
     end;
   end
  else //empty block
   begin
    while (--sampleFrames >= 0) do
     begin
      *out1++ = 0.0;
      *out2++ = 0.0;
     end;
   end;
  notes[0] = EVENTS_DONE;  //mark events buffer as done
  fzip = fz;
  K = k;
*)
end;

procedure TJX10DataModule.VSTModuleProcessMidi(Sender: TObject;
  MidiEvent: TVstMidiEvent);
begin
 with MidiEvent do
  case MidiData[0] and $F0 of     // status byte (all channels)
   $80: begin  // Note off
(*
         notes[npos++] = event.deltaFrames; //delta
         notes[npos++] = midiData[1] & $7F; //note
         notes[npos++] = 0;                  //vel
*)
        end;

   $90: begin // Note on
(*
         notes[npos++] = event.deltaFrames; //delta
         notes[npos++] = midiData[1] & $7F; //note
         notes[npos++] = midiData[2] & $7F; //vel
*)
        end;

   $B0: case midiData[1] of  // Controller
         $01: FModWheel := 0.000005 * sqr(midiData[2]);         // Mod Wheel
         $02,
         $4A: FFiltWheel :=  0.02 * (midiData[2]);              // Filter +
         $03: FFiltWheel := -0.03 * (midiData[2]);              // Filter -
         $07: FVolume    := 0.00000005 * sqr(midiData[2]);      // Volume
         $10,
         $47: FResonanceWheel  := 0.0065 * (154 - midiData[2]); // Resonance
         $40: begin // Sustain
               FSustain := midiData[2] and $40;
               if (FSustain = 0) then
                begin
(*
                 notes[npos++] = event.deltaFrames;
                 notes[npos++] = FSustain; //end all sustained notes
                 notes[npos++] = 0;
*)               end;
              end;

         else  //all notes off
          if (midiData[1] > $7A) then
           begin
(*
            for(long v=0; v<NVOICES; v++)
             begin
              fVoices[v].envl = fVoices[v].env = 0.0;
              fVoices[v].envd = 0.99;
              fVoices[v].note = 0;
              //could probably reset some more stuff here for safety!
             end;
*)
            FSustain := 0;
           end;
        end;

   $C0: if (midiData[1] < NumPrograms)
         then setProgram(midiData[1]);        // Program Change

   $D0: FPress := 0.00001 * sqr(midiData[1]); // Channel Aftertouch
   $E0: begin //pitch bend
         FPitchBendInv := exp(0.000014102 * (midiData[1] + 128 * midiData[2] - 8192));
         FPitchBend := 1 / FPitchBendInv;
        end;
  end;
end;

procedure TJX10DataModule.VSTModuleResume(Sender: TObject);
begin
 wantEvents(1);
end;

procedure TJX10DataModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
// FDeltaLFO := FLFOHz * (2 * Pi * KMAX) / SampleRate;
end;

procedure TJX10DataModule.VSTModuleSuspend(Sender: TObject);
var
  v : Integer;
begin
 for v := 0 to cNumVoices - 1 do
  begin
(*
   fVoices[v].envl := 0;
   fVoices[v].env  := 0;
   fVoices[v].envd := 0.99;
   fVoices[v].note := 0;
   fVoices[v].f0   := 0;
   fVoices[v].f1   := 0;
   fVoices[v].f2   := 0;
*)
  end;
end;

procedure TJX10DataModule.noteOn(Note, Velocity: Integer);
var
  p, l         : Single;
  v, tmp, held : Integer;
begin
 l    := 100; //louder than any envelope!
 v    := 0;
 held := 0;

 if (velocity > 0) then // Note on
  begin
    if FVelOff > 0 then Velocity := 80;

    if (FMode and 4) > 0 then // Monophonic
     begin
(*      if (fVoices[0].note > 0) // Legato Pitch Change
       begin
        for tmp := (cNumVoices - 1) downto 0   // queue any held notes
         do fVoices[tmp].note := fVoices[tmp - 1].note;
        p := FTune * exp(-0.05776226505 * (note + ANALOG * v));
        while (p < 3) or ((p * FDetune)<3) do p := p + p;
        fVoices[v].target := p;
        if (FMode and 2) = 0 then fVoices[v].period := p;
        fVoices[v].fc   := exp(FFilterVel * (velocity - 64)) / p;
        fVoices[v].env  := fVoices[v].env + 2 * cSilence; ///was missed out below if returned?
        fVoices[v].note := note;
        exit;
       end;
*)
     end
    else //polyphonic
     for tmp := 0 to cNumVoices - 1 do  //replace quietest fVoices not in attack
      begin
(*
       if (fVoices[tmp].note > 0) then inc(held);
       if (fVoices[tmp].env  < l) and (fVoices[tmp].envl < 2) then
        begin
         l := fVoices[tmp].env;
         v := tmp;
        end;
*)
      end;
(*
    p := FTune * exp(-0.05776226505 * (note + ANALOG * v));
    while (p < 3) or ((p * FDetune) < 3) do p := p + p;
    fVoices[v].target  := p;
    fVoices[v].FDetune := FDetune;

    tmp := 0;
    if (FMode and 2) > 0 then
     if ((FMode and 1 > 0) or (held > 0)) then tmp := note - fLastNote; // Glide
    fVoices[v].period := p * Power(1.059463094359, tmp - fGlidedisp);
    if fVoices[v].period < 3
     then fVoices[v].period := 3.0; //limit min period

    fVoices[v].note := note;
    fLastNote       := note;

    fVoices[v].fc   := exp(FFilterVel * (velocity - 64)) / p;  // filter tracking

    fVoices[v].lev  := FVolTrim * FVolume * (0.004 * sqr(velocity + 64) - 8);
    fVoices[v].lev2 := fVoices[v].lev * FOscMix;
*)

    if (Parameter[20] < 0.5) then //force 180 deg phase difference for PWM
     begin
(*
      if fVoices[v].dp > 0 then
       begin
        p := fVoices[v].pmax + fVoices[v].pmax - fVoices[v].p;
        fVoices[v].dp2 := -fVoices[v].dp;
       end
      else
       begin
        p := fVoices[v].p;
        fVoices[v].dp2 := fVoices[v].dp;
       end;
      fVoices[v].p2    := p + PI * fVoices[v].period;
      fVoices[v].pmax2 := fVoices[v].p2;

      fVoices[v].dc2   := 0;
      fVoices[v].sin02 := 0
      fVoices[v].sinx2 := 0
      fVoices[v].sin12 := 0;
*)
     end;

(*
    if (FMode and 4) > 0    // monophonic retriggering
     then fVoices[v].env := fVoices[v].env + 2 * cSilence
     else
      begin
       //if(Parameter[15] < 0.28)
       //begin
       //  fVoices[v].f0 = fVoices[v].f1 = fVoices[v].f2 = 0.0; //reset filter
       //  fVoices[v].env = cSilence + cSilence;
       //  fVoices[v].fenv = 0.0;
       //end;
       //else
         fVoices[v].env := fVoices[v].env + 2 * cSilence; //anti-glitching trick
      end;
    fVoices[v].envl  := 2;
    fVoices[v].envd  := FAttack;
    fVoices[v].fenvl := 2;
    fVoices[v].fenvd := fatt;
*)
  end
 else //note off
  begin
(*
   if (FMode and 4 > 0) and (fVoices[0].note = Note) then //monophonic (and current note)
    begin
      for v := cNumVoices - 1 downto 0 do
       if fVoices[v].Note > 0 then held := v; //any other notes queued?
      if held > 0 then
       begin
        fVoices[v].note    := fVoices[held].note;
        fVoices[held].note := 0;

        p := FTune * exp(-0.05776226505 * (fVoices[v].note + ANALOG * (double)v));
        while (p < 3) or ((p * FDetune) < 3) do p := p + p;
        fVoices[v].target := p;
        if (FMode and 2) = 0
         then fVoices[v].period := p;
        fVoices[v].fc := 1 / p;
       end
      else
       begin
        fVoices[v].envl  := 0.0;
        fVoices[v].envd  := FRelease;
        fVoices[v].fenvl := 0.0;
        fVoices[v].fenvd := frel;
        fVoices[v].note  := 0;
       end;
    end
   else //polyphonic
    begin
     for v := 0 to cNumVoices - 1 do
      if fVoices[v].note = Note then //any voices playing that note?
       if FSustain = 0 then
        begin
         fVoices[v].envl  := 0.0;
         fVoices[v].envd  := FRelease;
         fVoices[v].fenvl := 0.0;
         fVoices[v].fenvd := frel;
         fVoices[v].note  := 0;
        end
       else fVoices[v].note := FSustain;
    end;
*)
  end;
end;

end.

(*
mdaJX10Program::mdaJX10Program()
begin
  Parameter[0] := 0.00; // OSC Mix
  Parameter[1] := 0.25; // OSC FTune
  Parameter[2] := 0.50; // OSC Fine

  Parameter[3] := 0.00; // OSC FMode
  Parameter[4] := 0.35; // OSC Rate
  Parameter[5] := 0.50; // OSC Bend

  Parameter[6] := 1.00; // VCF Freq
  Parameter[7] := 0.15; // VCF Reso
  Parameter[8] := 0.75; // VCF <Env

  Parameter[9] := 0.00; // VCF <FLFO
  Parameter[10] = 0.50; // VCF <Vel
  Parameter[11] = 0.00; // VCF FAttack

  Parameter[12] = 0.30; // VCF FDecay
  Parameter[13] = 0.00; // VCF FSustain
  Parameter[14] = 0.25; // VCF FRelease

  Parameter[15] = 0.00; // ENV FAttack
  Parameter[16] = 0.50; // ENV FDecay
  Parameter[17] = 1.00; // ENV FSustain

  Parameter[18] = 0.30; // ENV FRelease
  Parameter[19] = 0.81; // LFO Rate
  Parameter[20] = 0.50; // Vibrato

  Parameter[21] = 0.00; // Noise   - not present in original patches
  Parameter[22] = 0.50; // Octave
  Parameter[23] = 0.50; // Tuning
  strcpy (name, "Empty Patch");
end;


void mdaJX10::getParameterDisplay(VstInt32 index, char *text)
begin
 case index of
   0: sprintf(string, "%4.0f:%2.0f", 100.0-50.0*Parameter[index], 50.0*Parameter[index]); break;
   1: sprintf(string, "%.0f", FSemi); break;
   2: sprintf(string, "%.1f", FCent); break;
   3: switch(FMode)
              begin case  0:
              1: strcpy(string, "POLY    "); break;
              2: strcpy(string, "P-LEGATO"); break;
              3: strcpy(string, "P-GLIDE "); break;
              4:
              5: strcpy(string, "MONO    "); break;
              6: strcpy(string, "M-LEGATO"); break;
                default: strcpy(string, "M-GLIDE "); break; end; break;
   5: sprintf(string, "%.2f", fGlidedisp); break;
   6: sprintf(string, "%.1f", 100.0 * Parameter[index]); break;
   8:
  23: sprintf(string, "%.1f", 200.0 * Parameter[index] - 100.0); break;
  10: if(Parameter[index]<0.05) strcpy(string, "   OFF  ");
                else sprintf(string, "%.0f", 200.0 * Parameter[index] - 100.0); break;
  19: sprintf(string, "%.3f", FLFOHz); break;
  20: if(Parameter[index]<0.5) sprintf(string, "PWM %3.0f", 100.0 - 200.0 * Parameter[index]);
                else sprintf(string, "%7.0f", 200.0 * Parameter[index] - 100.0); break;
  22: sprintf(string, "%d", (long)(Parameter[index] * 4.9) - 2); break;
  else sprintf(string, "%.0f", 100.0 * Parameter[index]);
 end;
 string[8] := 0;
 strcpy(text, (char * )string);
end;

*)
