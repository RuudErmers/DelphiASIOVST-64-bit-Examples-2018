unit TalkBoxDM;

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

const
  CMaxBufferSize = 1600;
  CMaxOrder      =   50;

type
  TTalkBoxDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterCarrierDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParameterCarierSelectChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDryChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FBuf       : array [0..1] of PDAVSingleFixedArray;
    FCar       : array [0..1] of PDAVSingleFixedArray;
    FWindow    : PDAVSingleFixedArray;
    FWinSize   : Integer;
    FEmphasis  : Single;
    FFX        : Single;
    FWet, FDry : Single;
    FPos       : Integer;
    FSwap      : Integer;
    FK, FO     : Integer;
    FD, FU     : array [0..4] of Single;
    procedure LPC(buf, car: PDAVSingleFixedArray; n, o: Integer);
    procedure LPCDurbin(r : PDAVSingleFixedArray; p : Integer; k : PDAVSingleFixedArray; var g: Single);
    procedure ResetStates;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common;

procedure TTalkBoxDataModule.VSTModuleOpen(Sender: TObject);
begin
 ///initialise...
 GetMem(FBuf[0], CMaxBufferSize * SizeOf(Single));
 GetMem(FBuf[1], CMaxBufferSize * SizeOf(Single));
 GetMem(FWindow, CMaxBufferSize * SizeOf(Single));
 GetMem(FCar[0], CMaxBufferSize * SizeOf(Single));
 GetMem(FCar[1], CMaxBufferSize * SizeOf(Single));
 FWinSize := 1; //trigger window recalc
 FK := 0;

 // default parameters
 Parameter[0] := 100; // Wet  [%]
 Parameter[1] := 0;   // Dry  [%]
 Parameter[2] := 0;   // Swap
 Parameter[3] := 1;   // Quality

 VSTModuleSuspend(Sender);
end;

procedure TTalkBoxDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FBuf[0]) then Dispose(FBuf[0]);
 if Assigned(FBuf[1]) then Dispose(FBuf[1]);
 if Assigned(FWindow) then Dispose(FWindow);
 if Assigned(FCar[0]) then Dispose(FCar[0]);
 if Assigned(FCar[1]) then Dispose(FCar[1]);
end;

procedure TTalkBoxDataModule.ParameterCarrierDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Left'
  else PreDefined := 'Right';
end;

procedure TTalkBoxDataModule.LPC(buf, car : PDAVSingleFixedArray; n, o : Integer);
var
  z, r, k  : array [0..CMaxOrder - 1] of Single;
  G, x     : Single;
  i, j, nn : Integer;
  min      : Single;
begin
 nn := n;

 for j := 0 to o do  //buf[] is already emphasized and windowed
  begin
   z[j] := 0;
   r[j] := 0;
   for i := 0 to nn - 1
    do r[j] := r[j] + Buf[i] * Buf[i + j]; //autocorrelation
   dec(nn);
  end;
 r[0] := 1.001 * r[0];  //stability fix

 min := 0.00001;
 if r[0] < min then
  begin
   for i := 0 to n - 1
    do buf[i] := 0.0;
   exit;
  end;

 LPCDurbin(@r, o, @k, G);  //calc reflection coeffs

 for i := 0 to o do
  begin
   if (k[i] > 0.995)
    then k[i] := 0.995 else
   if (k[i] < -0.995) then k[i] := -0.995;
  end;

 for i := 0 to n - 1 do
  begin
   x := G * car[i];
   for j := o downto 1 do  //lattice filter
    begin
     x    := x - k[j] * z[j - 1];
     z[j] := z[j - 1] + k[j] * x;
    end;
   buf[i] := x;
   z[0]   := x;  //output buf[] will be windowed elsewhere
  end;
end;


procedure TTalkBoxDataModule.LPCDurbin(r : PDAVSingleFixedArray; p : Integer; k : PDAVSingleFixedArray; var g: Single);
var
  i, j  : Integer;
  a, at : array [0..CMaxOrder - 1] of Single;
  e     : Single;
begin
 e := r[0];

 for i := 0 to p do
  begin
   a[i]  := 0;
   at[i] := 0;
  end;  //probably don't need to clear at[] or k[]

 for i := 1 to p do
  begin
   k[i] := -r[i];

   for j := 1 to i - 1 do
    begin
     at[j] := a[j];
     k[i] := k[i] - (a[j] * r[i-j]);
    end;
   if (abs(e) < 1E-20) then
    begin
     e := 0;
     break;
    end;

   k[i] := k[i] / e;

   a[i] := k[i];
   for j := 1 to i - 1
    do a[j] := at[j] + k[i] * at[i-j];

    e := e * (1 - sqr(k[i]));
  end;

 if (e < 1E-20) then e := 0;
 g := sqrt(e);
end;

procedure TTalkBoxDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample        : Integer;
  p, q, dr, FX  : Single;
  e, w, o, x    : Single;
  p0, p1        : Integer;
const
  den : Single = 1E-10;
  h0  : Single = 0.3;
  h1  : Single = 0.77;
begin
 p0 := FPos;
 p1 := (FPos + FWinSize div 2) mod FWinSize;
 e  := FEmphasis;
 FX := FFX;

 for Sample := 0 to SampleFrames - 1 do
  begin
   o  := Inputs[    FSwap, Sample];
   x  := Inputs[1 - FSwap, Sample];
   dr := o;

   p := FD[0] + h0 *  x;    FD[0] := FD[1];  FD[1] := x  - h0 * p;
   q := FD[2] + h1 * FD[4]; FD[2] := FD[3];  FD[3] := FD[4] - h1 * q;
   FD[4] := x;
   x := p + q;

   if FK > 0 then
    begin
     FK := 0;

     FCar[0][p0] := x;
     FCar[1][p1] := x;              // carrier input

     x := o - e;
     e := o;                        // 6dB / oct pre-emphasis

     w := FWindow[p0];
     FFX := FBuf[0, p0] * w;
     FBuf[0, p0] := x * w;          // 50% overlapping hanning windows

     inc(p0);
     if p0 >= FWinSize then
      begin
       LPC(FBuf[0], FCar[0], FWinSize, FO);
       p0 := 0;
      end;

     w := 1 - w;
     FFX := FFX + FBuf[1][p1] * w;
     FBuf[1][p1] := x * w;

     inc(p1);
     if p1 >= FWinSize then
      begin
       lpc(FBuf[1], FCar[1], FWinSize, FO);
       p1 := 0;
      end;
    end;
   inc(FK);

   p := FU[0] + h0 * FFX;   FU[0] := FU[1];  FU[1] := FFX - h0 * p;
   q := FU[2] + h1 * FU[4]; FU[2] := FU[3];  FU[3] := FU[4] - h1 * q;
   FU[4] := FFX;
   x := p + q;

   o := FWet * x + FDry * dr;

   Outputs[0, Sample] := o;
   Outputs[1, Sample] := o;
  end;

  FEmphasis := e;
  FPos := p0;
  FFX := FX;

  if (abs(FD[0]) < den) then FD[0] := 0; //anti-denormal (doesn't seem necessary but P4?)
  if (abs(FD[1]) < den) then FD[1] := 0;
  if (abs(FD[2]) < den) then FD[2] := 0;
  if (abs(FD[3]) < den) then FD[3] := 0;
  if (abs(FU[0]) < den) then FU[0] := 0;
  if (abs(FU[1]) < den) then FU[1] := 0;
  if (abs(FU[2]) < den) then FU[2] := 0;
  if (abs(FU[3]) < den) then FU[3] := 0;
end;

procedure TTalkBoxDataModule.ParameterCarierSelectChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSwap := Integer(Parameter[2] > 0.5);
end;

procedure TTalkBoxDataModule.ParameterWetChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FWet := 0.5 * Sqr(0.01 * Parameter[0]);
end;

procedure TTalkBoxDataModule.ParameterDryChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDry := 2   * Sqr(0.01 * Parameter[1]);
end;

procedure TTalkBoxDataModule.VSTModuleResume(Sender: TObject);
var
  fs    : Single;
  n     : Integer;
  p, dp : Single;
begin
 fs := Limit(SampleRate, 8000, 96000);

 n := Round(0.01633 * fs);
 if (n > CMaxBufferSize)
  then n := CMaxBufferSize;

 //FO = Round(0.0005 * fs);
 FO := Round((0.0001 + 0.0004 * Parameter[3]) * fs);

 if (n <> FWinSize) then //recalc hanning window
  begin
   FWinSize := n;
   dp := 2 * Pi / FWinSize;
   p  := 0;
   for n := 0 to N - 1 do
    begin
     FWindow[n] := 0.5 - 0.5 * cos(p);
     p := p + dp;
    end;
  end;
end;

procedure TTalkBoxDataModule.VSTModuleSuspend(Sender: TObject);
begin
 ResetStates;
end;

procedure TTalkBoxDataModule.ResetStates;
begin
 FPos := 0;
 FK   := 0;
 FU[0] := 0;
 FU[1] := 0;
 FU[2] := 0;
 FU[3] := 0;
 FU[4] := 0;
 FD[0] := 0;
 FD[1] := 0;
 FD[2] := 0;
 FD[3] := 0;
 FD[4] := 0;

 FEmphasis := 0;
 FFX := 0;

 FillChar(FBuf[0]^, CMaxBufferSize * SizeOf(Single), 0);
 FillChar(FBuf[1]^, CMaxBufferSize * SizeOf(Single), 0);
 FillChar(FCar[0]^, CMaxBufferSize * SizeOf(Single), 0);
 FillChar(FCar[1]^, CMaxBufferSize * SizeOf(Single), 0);
end;

end.

(*
void mdaTalkBox::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
   begin
    case 3: sprintf(string, "%4.0f", 5.0 + 95.0 * Parameter[index] * Parameter[index]); break;
    default: sprintf(string, "%4.0f %%", 200.0 * Parameter[index]);
   end;
end;
*)
