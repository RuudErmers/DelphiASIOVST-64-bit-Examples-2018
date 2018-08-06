unit StereoDM;

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
  TStereoDataModule = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWidthLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterModDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
  private
    FPhi, FDel : Single;
    FDeltaPhi  : Single;
    FMod       : Single;
    FSize      : Integer;
    FBufferPos : Integer;
    FDelay     : Integer;
    FRi, FRd   : Single;
    FLi, FLd   : Single;
    FBuffer    : PDAVSingleFixedArray;
    procedure DeltaPhiChanged;
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

procedure TStereoDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FBuffer) then Dispose(FBuffer);
end;

procedure TStereoDataModule.VSTModuleOpen(Sender: TObject);
begin
(*
 //inits here!
 Parameter[0] = 0.78; // Haas/Comb width
 Parameter[1] = 0.43; // Delay
 Parameter[2] = 0.50; // Balance
 Parameter[3] = 0.00; // Mod
 Parameter[4] = 0.50; // Rate
*)

 FSize      := 4800;
 FBufferPos := 0;
 GetMem(FBuffer, FSize * SizeOf(Single));
end;

procedure TStereoDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if (Parameter[2] > 0.5) then
  begin
   FLi := FLi * 2 * (1 - Parameter[2]);
   FLd := FLd * 2 * (1 - Parameter[2]);
  end
 else
  begin
   FRi := FRi * (2 * Parameter[2]);
   FRd := FRd * (2 * Parameter[2]);
  end;

 FRi := FRi * abs(Parameter[0]);
 FRd := FRd * abs(Parameter[0]);
 FLi := FLi * abs(Parameter[0]);
 FLd := FLd * abs(Parameter[0]);
end;

procedure TStereoDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Sample  : Integer;
  a, b    : Double;
  li, ld,
  ri, rd,
  del, ph : Double;
  dph, md : Double;
  tmp, bp : Integer;
begin
 ph  := FPhi;
 dph := FDeltaPhi;
 bp  := FBufferPos;

 li  := FLi;
 ld  := FLd;
 ri  := FRi;
 rd  := FRd;
 del := FDel;

 if FMod > 0 then //modulated delay
  for Sample := 0 to SampleFrames - 1 do
   begin
    a := Inputs[0, Sample] + Inputs[1, Sample]; //sum to mono

    FBuffer[bp] := a; //write
    tmp := (bp + Round(del + abs(FMod * sin(ph)))) mod 4410;
    b   := FBuffer[tmp];

    Outputs[0, Sample] := (Inputs[0, Sample] * li) - (Inputs[1, Sample] * ld); // output
    Outputs[1, Sample] := (Inputs[0, Sample] * ri) - (Inputs[1, Sample] * rd);

    dec(bp);
    if (bp < 0)
     then bp := 4410; //FBuffer position

    ph := ph + dph;
   end
 else
  for Sample := 0 to SampleFrames - 1 do
   begin
    a := Inputs[0, Sample] + Inputs[1, Sample]; //sum to mono

    FBuffer[bp] := a; //write
    tmp := (bp + Round(del)) mod 4410;
    b   := FBuffer[tmp];

    Outputs[0, Sample] := (Inputs[0, Sample] * li) - (Inputs[1, Sample] * ld); // output
    Outputs[1, Sample] := (Inputs[0, Sample] * ri) - (Inputs[1, Sample] * rd);

    dec(bp);
    if (bp < 0)
     then bp := 4410; //FBuffer position

    ph := ph + dph;
   end;
 FBufferPos := bp;
 FPhi       := f_mod(ph, 2 * Pi);
end;

procedure TStereoDataModule.VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
begin
 DeltaPhiChanged;
end;

procedure TStereoDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(FBuffer, FSize * SizeOf(Single), 0);
end;

procedure TStereoDataModule.ParameterWidthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if (Parameter[0] < 0.5) then
  begin
   FLi := (0.25 + (1.5 * Parameter[0]));
   FLd := 0;
   FRi := (2 * Parameter[0]);
   FRd := (1 - FRi);
  end
 else
  begin
   FLi := (1.5 - Parameter[0]);
   FLd := (Parameter[0] - 0.5);
   FRi := FLi;
   FRd := -FLd;
  end;
end;

procedure TStereoDataModule.ParameterWidthLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then Predefined := 'Haas'
  else Predefined := 'Comb';
end;

procedure TStereoDataModule.ParameterModDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if FMod > 0
  then PreDefined := FloatToStrF(1000 * FMod / SampleRate, ffGeneral, 2, 2)
  else PreDefined := 'OFF';
end;

procedure TStereoDataModule.ParameterModChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMod := (2100 * Power(Value, 2));
end;

procedure TStereoDataModule.ParameterRateChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 DeltaPhiChanged;
end;

procedure TStereoDataModule.DeltaPhiChanged;
begin
 FDeltaPhi := (Pi * Power(10, -2 + 3 * Parameter[3]) / SampleRate);
end;

procedure TStereoDataModule.ParameterDelayChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDelay := Round(20 + 2080 * Power(Value, 2));
end;

end.

(*
void mdaStereo::getParameterDisplay(VstInt32 index, char *text)
begin
  switch(index)
  begin
    case 0: long2string(Round(200 * abs(Parameter[0] - 0.5)), text);break;
    case 1: float2strng((1000 * FDel / SampleRate), text); break;
    case 2: long2string(Round(200 * (Parameter[2] - 0.5)), text); break;
    case 4: float2strng(Power(10, 2 - 3 * Parameter[4]), text); break;
  end;
end;
*)
