unit DitherDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, DAV_Types, DAV_VSTModule;

type
  TDitherDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDitherDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FWordLength : Integer;
    FBits       : Single;
    FGain       : Single;
    FOffset     : Single;
    FDither     : Single;
    FShaper     : Single;
    FRandom     : array [0..1] of Integer;
    FShapeState : array [0..3] of Single;
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

procedure TDitherDataModule.ParamDitherDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
   0 : PreDefined := 'OFF';
   1 : PreDefined := 'TRI';
   2 : PreDefined := 'HP-TRI';
  else PreDefined := 'N.SHAPE';
 end;
end;

procedure TDitherDataModule.VSTModuleOpen(Sender: TObject);
begin
 FShapeState[0] := 0;
 FShapeState[1] := 0;
 FShapeState[2] := 0;
 FShapeState[3] := 0;
 FRandom[0]     := 0;
 FRandom[1]     := 0;
end;

procedure TDitherDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 //calcs here
 FGain := 1;
 FBits := 8 + 2 * trunc(8.9 * Parameter[0]);

 if (Parameter[4] > 0.1) then //zoom to 6 bit & fade out audio
  begin
   FWordLength := 32;
   FGain := sqr(1 - Parameter[4]);
  end
 else FWordLength := Round(Power(2, FBits - 1)); //word length in quanta

 //Using WaveLab 2.01 (unity gain) as a reference:
 //  16-bit output is floor(floating_point_value * 32768)

 FOffset := (4 * Parameter[3] - 1.5) / FWordLength;   //DC offset (plus 0.5 to round dither not truncate)
 FDither := 2 * Parameter[2] / (FWordLength * 32767);
 FShaper := 0;

 case Round(Parameter[0]) of // dither mode:
  0: FDither := 0;           // - off
  3: FShaper := 0.5;         // - noise shaping
 end;
end;

procedure TDitherDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample             : Integer;
  a, b, aa, bb       : Single;
  sl, s1, s2, s3, s4 : Single;  // shaping level, buffers
  dl                 : Single;  // dither level
  o, w, wi           : Single;  // DC offset, word length & inverse
  g                  : Single;  // gain for Zoom mode
  r1, r2, r3, r4     : Integer; // random numbers for dither
  m                  : Integer; // dither mode
begin
  sl := FShaper;
  s1 := FShapeState[0];
  s2 := FShapeState[1];
  s3 := FShapeState[2];
  s4 := FShapeState[3];
  dl := FDither;
  o  := FOffset;
  w  := FWordLength;
  wi := 1 / w;
  g  := FGain;
  r1 := FRandom[0];
  r3 := FRandom[1];
  m := 1;
(*
  if (Round(fParam1 * 3.9) = 1)
   then m := 0;
*)

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := Inputs[0, Sample];
   Outputs[1, Sample] := Inputs[1, Sample];
   (*
    a = *++in1;
    b = *++in2;

    r2 := r1;
    r4 := r3; //HP-TRI dither (also used when noise shaping)
    if (m=0) { r4=rand() & 0x7FFF; r2=(r4 & 0x7F)<<8; } //TRI dither
               r1=rand() & 0x7FFF; r3=(r1 & 0x7F)<<8;   //Assumes RAND_MAX=32767?

    a  := g * a + sl * (s1 + s1 - s2);    // target level + error feedback
    aa := a + o + dl * (r1 - r2);         //              + offset + dither
    if (aa < 0) then aa := aa - wi;       // (long) truncates towards zero!
    aa := wi * (float)(long)(w * aa);     // truncate
    s2 := s1;
    s1 := a - aa;                         // error feedback: 2nd order noise shaping

    b  := g * b + sl * (s3 + s3 - s4);
    bb := b + o + dl * (r3 - r4);
    if (bb < 0) then bb := bb - wi;
    bb := wi * (float)(long)(w * bb);
    s4 := s3;
    s3 := b - bb;

    *++out1 = aa;
    *++out2 = bb;
   *)
  end;

  FShapeState[0]  := s1;
  FShapeState[1]  := s2;
  FShapeState[2]  := s3;
  FShapeState[3]  := s4;   // doesn't actually matter if these are
  FRandom[0] := r1;
  FRandom[1] := r3;        // saved or not as effect is so small !
end;

end.

(*
void mdaDither::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)bits, text); break;
    case 2: float2strng(4.0f * fParam2, text); break;
    case 3: float2strng(4.0f * fParam3 - 2.0f, text); break;
    case 4: if(fParam4>0.1f) 
            if(gain<0.0001f) strcpy(text, "-80");
                        else long2string((long)(20.0 * log10(gain)), text);
                        else strcpy(text, "OFF"); break;
  }
}
*)
