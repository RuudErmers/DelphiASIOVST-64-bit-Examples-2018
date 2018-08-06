unit ThruZeroDM;

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
  cBUFMAX = $7FF;

type
  TThruZeroDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleResume(Sender: TObject);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject;const SampleRate: Single);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDepthModChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDepthDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleClose(Sender: TObject);
  private
    FBuffer    : array [0..1] of PDAVSingleFixedArray;
    FRate      : Single;
    FPhi, FDem : Single;
    FDeps      : Single;
    FDepth     : Single;
    FWet, FDry : Single;
    FBufPos    : Integer;
    FFeedback  : array [0..2] of Single;
    procedure RateChanged;
    procedure DepthChanged;
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

procedure TThruZeroDataModule.VSTModuleOpen(Sender: TObject);
begin
  Parameter[0] := 0.30;  // Rate
  Parameter[1] := 0.43;  // Depth
  Parameter[2] :=  47;   // Mix
  Parameter[3] := -40;   // Feedback
  Parameter[4] := 100;   // Minimum delay to stop LF buildup with feedback

  ///differences from default program...
  with Programs[1] do
   begin
    Parameter[0] := 0.50;
    Parameter[1] := 0.20;
    Parameter[2] :=   47;
    Parameter[3] :=  -40;
    Parameter[4] :=  100;
   end;

  with Programs[2] do
   begin
    Parameter[0] := 0.60;
    Parameter[1] := 0.60;
    Parameter[2] :=   35;
    Parameter[3] :=  -40;
    Parameter[4] :=   70;
   end;

  with Programs[3] do
   begin
    Parameter[0] := 0.75;
    Parameter[1] := 1.00;
    Parameter[2] :=   50;
    Parameter[3] :=   50;
    Parameter[4] :=  100;
   end;

 ///initialise...
 FBufPos := 0;
 GetMem(FBuffer[0], cBUFMAX * SizeOf(Single));
 GetMem(FBuffer[1], cBUFMAX * SizeOf(Single));
 FPhi         := 0;
 FFeedback[0] := 0;
 FFeedback[1] := 0;
 FFeedback[2] := 0;
 FDeps        := 0;

 VSTModuleSuspend(Sender);
end;

procedure TThruZeroDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FBuffer[0]) then Dispose(FBuffer[0]);
 if Assigned(FBuffer[1]) then Dispose(FBuffer[1]);
end;

procedure TThruZeroDataModule.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample                  : Integer;
  a, b, f, f1, f2, ph     : Single;
  ra, de, we, dr, ds, dm  : Single;
  tmp, tmpi, bp           : Integer;
  tmpf, dpt               : Single;
begin
 f  := FFeedback[0];
 f1 := FFeedback[1];
 f2 := FFeedback[2];
 ph := FPhi;
 ra := FRate;
 de := FDepth;
 we := FWet;
 dr := FDry;
 ds := FDeps;
 dm := FDem;
 bp := FBufPos;

 for Sample := 0 to SampleFrames - 1 do
  begin
    a := Inputs[0, Sample];
    b := Inputs[1, Sample];

    ph := ph + ra;
    if ph > 1
     then ph := ph - 2;

    dec(bp);
    bp := bp and cBUFMAX;
    FBuffer[0, bp] := a + f * f1;
    FBuffer[1, bp] := b + f * f2;

    // ds := 0.995 * (ds - de) + de;           // smoothed depth change ...try inc not mult
    dpt  := dm + de * (1 - sqr(ph));           // delay mod shape
    tmpf := dpt;
    tmp  := Round(tmpf);
    tmpf := tmpf - tmp;
    tmp  := (tmp + bp) and $7FF;
    tmpi := (tmp + 1) and $7FF;

    f1 := FBuffer[0, tmp];                     // try adding a constant to reduce denormalling
    f2 := FBuffer[1, tmp];
    f1 := tmpf * (FBuffer[0, tmpi] - f1) + f1; // linear interpolation
    f2 := tmpf * (FBuffer[1, tmpi] - f2) + f2;

    a := a * dr - f1 * we;
    b := b * dr - f2 * we;

    Outputs[0, Sample] := a;
    Outputs[1, Sample] := b;
  end;

 if abs(f1) > 1E-10 then
  begin
   FFeedback[1] := f1;
   FFeedback[2] := f2;
  end
 else
  begin
   FFeedback[1] := 0;
   FFeedback[2] := 0; //catch denormals
  end;
 FPhi    := ph;
 FDeps   := ds;
 FBufPos := bp;
end;

procedure TThruZeroDataModule.VSTModuleResume(Sender: TObject);
begin
 DepthChanged;
end;

procedure TThruZeroDataModule.DepthChanged;
begin
 FDepth := 2000 * sqr(Parameter[1]);
 FDem := sqr(FDepth) * 0.01 * Parameter[4];
 FDepth := FDepth - FDem;
end;

procedure TThruZeroDataModule.ParameterMixChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FWet := 0.01 * Value;
 FDry := 1 - FWet;
end;

procedure TThruZeroDataModule.ParameterDepthModChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FFeedback[0] := 0.0095 * Value;
 FPhi         := 0.0;             //reset cycle
end;

procedure TThruZeroDataModule.ParameterRateChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 RateChanged;
 if Value < 0.01 then
  begin
   FRate := 0;
   FPhi  := 0;
  end;
end;

procedure TThruZeroDataModule.RateChanged;
begin
 FRate := Power(10, 3 * Parameter[0] - 2) * 2 / SampleRate;
end;

procedure TThruZeroDataModule.ParameterFeedbackChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 DepthChanged;
end;

procedure TThruZeroDataModule.ParameterDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 DepthChanged;
end;

procedure TThruZeroDataModule.ParameterRateDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if Parameter[0] < 0.01
  then PreDefined := '-'
  else PreDefined := FloatToStrF(Power(10, 2 - 3 * Parameter[index]), ffGeneral, 4, 4);
end;

procedure TThruZeroDataModule.ParameterDepthDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(1000 * FDepth / SampleRate, ffGeneral, 4, 4);
end;

procedure TThruZeroDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 RateChanged;
end;

procedure TThruZeroDataModule.VSTModuleSuspend(Sender: TObject);
begin
 if Assigned(FBuffer[0]) then FillChar(FBuffer[0, 0], cBUFMAX * SizeOf(Single), 0);
 if Assigned(FBuffer[1]) then FillChar(FBuffer[1, 0], cBUFMAX * SizeOf(Single), 0);
end;

end.
