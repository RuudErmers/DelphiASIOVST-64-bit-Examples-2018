unit DegradeDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, DAV_Types, DAV_VSTModule;

type
  TDegradeDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterNonLinearChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPostFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FBuffer : array [0..9] of Single;
    FLin    : Single;
    FLin2   : Single;
    FClip   : Single;
    FFreq   : Single;
    FMode   : Integer;
    FTCount : Integer;
    FGain   : array [0..2] of Single;
    fo2     : Single;
    fi2     : Single;
    tn      : Integer;
    function FilterFreq(Hz: Single): Single;
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

procedure TDegradeDataModule.ParameterNonLinearChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Value > 0.5) then
  begin
   FLin  := Power(10, 0.3 * (0.5 - Value));
   FLin2 := FLin;
  end
 else
  begin
   FLin  := Power(10, 0.3 * (Value - 0.5));
   FLin2 := 1;
  end;
end;

procedure TDegradeDataModule.ParameterOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain[2] := Power(10, 2 * Value - 1.0);
end;

procedure TDegradeDataModule.ParameterPostFilterChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fo2 := FilterFreq(Power(10, 2.30104 + 2 * Value));
 fi2 := sqr(sqr(1 - fo2));
end;

function TDegradeDataModule.FilterFreq(Hz: Single): Single;
var
  j, k, r : Single;
begin
 r := 0.999;
 j := sqr(r) - 1;
 k := (2 - 2 * sqr(r) * cos(0.647 * Hz / SampleRate));
 result := (sqrt(k * k - 4 * j * j) - k) / (2 * j);
end;

procedure TDegradeDataModule.VSTModuleOpen(Sender: TObject);
begin
 //inits here!
 Parameter[0] := 0.8;  // Clip
 Parameter[1] := 0.50; // Bits
 Parameter[2] := 0.65; // Rate
 Parameter[3] := 0.9;  // Postfilt
 Parameter[4] := 0.58; // Non-lin
 Parameter[5] := 0.5;  // Level

 FillChar(FBuffer[0], SizeOf(FBuffer), 0);
end;

procedure TDegradeDataModule.VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  g1 : Single;
begin
 //calcs here
 if (Parameter[2] > 0.5) then
  begin
   FFreq := Parameter[2] - 0.5;
   FMode := 1;
  end
 else
  begin
   FFreq := 0.5 - Parameter[2];
   FMode := 0;
  end;

 tn := Round(exp(18 * FFreq));

 FTCount := 1;
 FClip   := Power(10, (-1.5 + 1.5 * Parameter[0]));

 g1      := Power(2, 2 + Round(Parameter[1] * 12));
 FGain[1] := 1 / (2 * g1);
 if (Parameter[2] > 0.5)
  then FGain[0] := -g1 / tn
  else FGain[0] := -g1;
end;

procedure TDegradeDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample     : Integer;
  l, l2      : Single;
  cl, i2, o2 : Single;
  b          : array [0..9] of Single;
  gi, go     : Single;
  ga, m      : Single;
  n, t       : Integer;
begin
 b[0] := FBuffer[0];
 b[1] := FBuffer[1];
 b[2] := FBuffer[2];
 b[3] := FBuffer[3];
 b[4] := FBuffer[4];
 b[5] := FBuffer[5];
 b[6] := FBuffer[6];
 b[7] := FBuffer[7];
 b[8] := FBuffer[8];
 b[9] := FBuffer[9];
 l    := FLin;
 l2   := FLin2;
 cl   := FClip;
 m    := FMode;
 i2   := fi2;
 o2   := fo2;
 gi   := FGain[0];
 go   := FGain[1];
 ga   := FGain[2];
 n    := tn;

 t    := FTCount;
 for Sample := 0 to SampleFrames - 1 do
  begin
   b[0] := (Inputs[0, Sample] + Inputs[1, Sample]) + m * b[0];

   if (t = n) then
    begin
     t  := 0;
     b[5] := (go * int(b[0] * gi));
     if (b[5] > 0)
      then begin b[5] :=  Power( b[5], l2); if (b[5] > cl) then b[5] :=  cl; end
      else begin b[5] := -Power(-b[5],  l); if (b[5] <-cl) then b[5] := -cl; end;
     b[0] := 0;
    end;
   t := t + 1;

   b[1] := i2 * (b[5] * ga) + o2 * b[1];
   b[2] :=      b[1] + o2 * b[2];
   b[3] :=      b[2] + o2 * b[3];
   b[4] :=      b[3] + o2 * b[4];
   b[6] := i2 * b[4] + o2 * b[6];
   b[7] :=      b[6] + o2 * b[7];
   b[8] :=      b[7] + o2 * b[8];
   b[9] :=      b[8] + o2 * b[9];

   Outputs[0, Sample] := b[9];
   Outputs[1, Sample] := b[9];
  end;

 if (abs(b[1]) < 1E-10) then 
  begin
   FBuffer[0] := 0;
   FBuffer[1] := 0;
   FBuffer[2] := 0;
   FBuffer[3] := 0;
   FBuffer[4] := 0;
   FBuffer[5] := 0;
   FBuffer[6] := 0;
   FBuffer[7] := 0;
   FBuffer[8] := 0;
   FBuffer[9] := 0;
  end
 else
  begin
   FBuffer[0] := b[0];
   FBuffer[1] := b[1];
   FBuffer[2] := b[2];
   FBuffer[3] := b[3];
   FBuffer[4] := b[4];
   FBuffer[5] := b[5];
   FBuffer[6] := b[6];
   FBuffer[7] := b[7];
   FBuffer[8] := b[8];
   FBuffer[9] := b[9];
   FTCount := t;
  end;
end;

end.
