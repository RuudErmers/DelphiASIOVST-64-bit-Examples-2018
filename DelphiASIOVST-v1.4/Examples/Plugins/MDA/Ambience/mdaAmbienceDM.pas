unit mdaAmbienceDM;

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
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

const
  CBufferSize = 1024;
  CFeedBack = 0.8;

type
  TmdaAmbienceDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSuspend(Sender: TObject);
    procedure ParamSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHFDampChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleClose(Sender: TObject);
  private
    FBuffers      : array [0..3] of PDAVSingleFixedArray;
    FPos          : Integer;
    FHfDampState  : Single;
    FDamp         : Single;
    FOutputFactor : Single;
    FDry, FWet    : Single;
    FRoomsize     : Double;
    FReady, FDen  : Boolean;
    procedure CalculateDryWet;
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

procedure TmdaAmbienceDataModule.ParamHFDampChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FDamp := 0.05 + 0.9 * 0.01 * Value;
end;

procedure TmdaAmbienceDataModule.ParamMixChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 CalculateDryWet;
end;

procedure TmdaAmbienceDataModule.ParamOutputChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FOutputFactor := Power(10.0, Parameter[3] * 0.05);
 CalculateDryWet;
end;

procedure TmdaAmbienceDataModule.CalculateDryWet;
begin
 FDry := FOutputFactor - sqr(0.01 * Parameter[2]) * FOutputFactor;
 FWet := 0.01 * 0.8 * Parameter[2] * FOutputFactor;
end;

procedure TmdaAmbienceDataModule.ParamSizeChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  temp : Double;
begin
 temp := 0.025 + 0.2665 * Value;
 if (FRoomsize <> temp)
  then FReady := False;  // need to flush buffer
 FRoomsize := temp;
end;

procedure TmdaAmbienceDataModule.VSTModuleOpen(Sender: TObject);
begin
 GetMem(FBuffers[0], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[1], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[2], CBufferSize * SizeOf(Single));
 GetMem(FBuffers[3], CBufferSize * SizeOf(Single));

 FHfDampState := 0.0;
 FDen := FPos = 0;

 VSTModuleSuspend(Sender);  // Flush buffer

 // Initial Parameters
 Parameter[0] := 0.7; // Size
 Parameter[1] := 0.7; // HF
 Parameter[2] := 0.9; // Mix
 Parameter[3] := 0.5; // Output

 // Default Preset
 with Programs[0] do
  begin
   Parameter[0] := 0.7; // Size
   Parameter[1] := 0.7; // HF
   Parameter[2] := 0.9; // Mix
   Parameter[3] := 0.5; // Output
  end;
end;

procedure TmdaAmbienceDataModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FBuffers[0]) then Dispose(FBuffers[0]);
 if Assigned(FBuffers[1]) then Dispose(FBuffers[1]);
 if Assigned(FBuffers[2]) then Dispose(FBuffers[2]);
 if Assigned(FBuffers[3]) then Dispose(FBuffers[3]);
end;

procedure TmdaAmbienceDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  r    : Double;
  t, f,
  dmp,
  y, w : Double;
  i, p : Integer;
  d    : array [0..3] of Integer;
begin
 f   := FHfDampState;
 dmp := FDamp;
 y   := FDry;
 w   := FWet;
 p   := FPos;

 if (FReady = False)
  then VSTModuleSuspend(nil);

 d[0] := p + Round(107 * FRoomsize) and 1023;
 d[1] := p + Round(142 * FRoomsize) and 1023;
 d[2] := p + Round(277 * FRoomsize) and 1023;
 d[3] := p + Round(379 * FRoomsize) and 1023;

 for i := 0 to SampleFrames - 1 do
  begin
   f := f + dmp * (w * (Inputs[0, i] + Inputs[1, i]) - f);  // HF damping
   r := f;

   t := FBuffers[0]^[p];
   r := r - CFeedBack * t;
   FBuffers[0]^[d[0]] := r; // Allpass
   r := r + t;

   t := FBuffers[1]^[p];
   r := r - CFeedBack * t;
   FBuffers[1]^[d[1]] := r; // Allpass
   r := r + t;

   t := FBuffers[2]^[p];
   r := r - CFeedBack * t;
   FBuffers[2]^[d[2]] := r; // Allpass
   r := r + t;
   Outputs[0, i] := y * Inputs[0, i] + r - f; // Left Output

   t := FBuffers[3]^[p];
   r := r - CFeedBack * t;
   FBuffers[3]^[d[3]] := r; // Allpass
   r := r + t;
   Outputs[1, i] := y * Inputs[1, i] + r - f; // Right Output

   p    := (p    + 1) and 1023;
   d[0] := (d[0] + 1) and 1023;
   d[1] := (d[1] + 1) and 1023;
   d[2] := (d[2] + 1) and 1023;
   d[3] := (d[3] + 1) and 1023;
  end;

 FPos := p;
 if (abs(f) > 1E-10) then
  begin   // Catch Denormals
   FHfDampState := f;
   FDen := False;
  end
 else
  begin
   FHfDampState := 0;
   if FDen = False then
    begin
     FDen := True;
     VSTModuleSuspend(nil);
    end;
  end;
end;

procedure TmdaAmbienceDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  r    : Double;
  t, f,
  dmp,
  y, w : Double;
  i, p : Integer;
  d    : array [0..3] of Integer;
begin
 f   := FHfDampState;
 dmp := FDamp;
 y   := FDry;
 w   := FWet;
 p   := FPos;

 if (FReady = False)
  then VSTModuleSuspend(nil);

 d[0] := p + Round(107 * FRoomsize) and 1023;
 d[1] := p + Round(142 * FRoomsize) and 1023;
 d[2] := p + Round(277 * FRoomsize) and 1023;
 d[3] := p + Round(379 * FRoomsize) and 1023;

 for i := 0 to SampleFrames - 1 do
  begin
   f := f + dmp * (w * (Inputs[0, i] + Inputs[1, i]) - f);  //HF Damping
   r := f;

   t := FBuffers[0]^[p];
   r := r - CFeedBack * t;
   FBuffers[0]^[d[0]] := r; // Allpass
   r := r + t;

   t := FBuffers[1]^[p];
   r := r - CFeedBack * t;
   FBuffers[1]^[d[1]] := r; // Allpass
   r := r + t;

   t := FBuffers[2]^[p];
   r := r - CFeedBack * t;
   FBuffers[2]^[d[2]] := r; // Allpass
   r := r + t;
   Outputs[0, i] := y * Inputs[0, i] + r - f; // Left Output

   t := FBuffers[3]^[p];
   r := r - CFeedBack * t;
   FBuffers[3]^[d[3]] := r; // Allpass
   r := r + t;
   Outputs[1, i] := y * Inputs[1, i] + r - f; // Right Output

   p    := (p    + 1) and 1023;
   d[0] := (d[0] + 1) and 1023;
   d[1] := (d[1] + 1) and 1023;
   d[2] := (d[2] + 1) and 1023;
   d[3] := (d[3] + 1) and 1023;
  end;

 FPos := p;
 if (abs(f) > 1E-10) then
  begin   //catch denormals
   FHfDampState := f;
   FDen := False;
  end
 else
  begin
   FHfDampState := 0;
   if FDen = False then
    begin
     FDen := True;
     VSTModuleSuspend(nil);
    end;
  end;
end;

procedure TmdaAmbienceDataModule.VSTModuleSuspend(Sender: TObject);
begin
 FillChar(FBuffers[0]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[1]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[2]^[0], CBufferSize * SizeOf(Single), 0);
 FillChar(FBuffers[3]^[0], CBufferSize * SizeOf(Single), 0);
 FReady := True;
end;

end.
