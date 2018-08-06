unit DelayDM;

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
  TDelayDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FBuffer    : PDAVSingleFixedArray;
    FSize      : Integer;
    FWet       : Single;
    FDry       : Single;
    FFeedback  : Single;
    FLowMix    : Single;
    FHighMix   : Single;
    FFilter    : Single;
    FFilter0   : Single;
    FIntPos    : Integer;
    ldel, rdel : Integer;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TDelayDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSize   := 32766;  //set max delay time at max sample rate
 GetMem(FBuffer, FSize * SizeOf(Single));

(*
 FIntPos = 0;
 fil0 = 0.0;

 // inits here!
 Parameter[0] := 0.50; // Left Delay
 Parameter[1] := 0.27; // Right Ratio
 Parameter[2] := 0.70; // Feedback
 Parameter[3] := 0.50; // Tone
 Parameter[4] := 0.33; // Wet mix
 Parameter[5] := 0.50; // Output

 suspend();    //flush FBuffer
 setParameter(0, 0.5);
*)
end;

procedure TDelayDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FBuffer)
  then Dispose(FBuffer);
end;

procedure TDelayDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  tmp : Single;
begin
 //calcs here
 ldel := Round(FSize * sqr(Parameter[0]));
 if (ldel < 4) then ldel := 4;

 case Round(Parameter[1] * 17.9) of //fixed left/right ratios
   17: tmp := 0.5;
   16: tmp := 0.6667;
   15: tmp := 0.75;
   14: tmp := 0.8333;
   13: tmp := 1;
   12: tmp := 1.2;
   11: tmp := 1.3333;
   10: tmp := 1.5;
    9: tmp := 2;
  else tmp := 4 * Parameter[1]; //variable ratio
 end;

 rdel := Round(FSize * sqr(Parameter[0]) * tmp);
 if (rdel > FSize) then rdel := FSize;
 if (rdel < 4) then rdel := 4;

(*
 fil := Parameter[3];

 if (Parameter[3] > 0.5)  //simultaneously change crossover frequency & high/low mix
  begin
   fil  := 0.5 * fil - 0.25;
   lmix := -2.0 * fil;
   hmix := 1.0;
  end;
 else
  begin
   hmix := 2 * fil;
   lmix := 1 - hmix;
  end;
 fil := exp(-6.2831853 * Power(10, 2.2 + 4.5 * fil) / SampleRate);
*)

 FFeedback := 0.495 * Parameter[2];
 FWet      := 1 - Parameter[4];
 FWet      := Parameter[5] * (1 - sqr(FWet)); // -3dB at 50% mix
 FDry      := Parameter[5] * 2 * (1 - sqr(Parameter[4]));

 //if(Parameter[2] > 0.99) { fbk=0.5; FWet=0.0; } //freeze
end;

procedure TDelayDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample      : Integer;
  i, l, r, s  : Integer;
  fb, lx, hx,
  f, f0, tmp  : Single;
begin
 fb := FFeedback;
 lx := FLowMix;
 hx := FHighMix;
 f  := FFilter;
 f0 := FFilter0;

 i  := FIntPos;

 s  := FSize;
 l  := (i + ldel) mod (s + 1);
 r  := (i + rdel) mod (s + 1);

 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FBuffer[l]; //delay outputs
   Outputs[1, Sample] := FBuffer[r];

   tmp := FWet * ( Inputs[0, Sample] +  Inputs[1, Sample]) +
          fb   * (Outputs[0, Sample] + Outputs[1, Sample]);   // mix input & feedback
   f0  := f * (f0 - tmp) + tmp;                             // low-pass filter
   FBuffer[i] := lx * f0 + hx * tmp;                        // delay input

   dec(i); if (i < 0) then i := s;
   dec(l); if (l < 0) then l := s;
   dec(r); if (r < 0) then r := s;

   Outputs[0, Sample] := FDry * Inputs[0, Sample] + Outputs[0, Sample]; //mix FWet & FDry
   Outputs[1, Sample] := FDry * Inputs[1, Sample] + Outputs[1, Sample];
  end;

 FIntPos := i;
 if abs(f0) < 1E-10
  then FFilter0 := 0
  else FFilter0 := f0; //trap denormals
end;

procedure TDelayDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(FBuffer, FSize * SizeOf(Single), 0);
end;

end.

(*
void mdaDelay::getParameterDisplay(VstInt32 index, char *text)
{
  switch(index)
  {
    case 0: long2string((long)(ldel * 1000.0 / SampleRate), text); break;
    case 1: long2string((long)(100 * rdel / ldel), text); break;
    case 2: long2string((long)(99 * Parameter[2]), text); break;
    case 3: long2string((long)(200 * Parameter[3] - 100), text); break;
    case 4: long2string((long)(100 * Parameter[4]), text); break;
    case 5: long2string((long)(20 * log10(2.0 * Parameter[5])), text); break;
  }
}

*)
