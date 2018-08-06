unit VibratoDSP;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows,{$ENDIF} Messages, SysUtils,
  Classes, Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspVibrato,
  DAV_VSTCustomModule;

type
  TVibratoModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FVibrato         : array [0..1] of TDspVibrato32;
    FCriticalSection : TCriticalSection;
    function GetVibrato(Index: Integer): TDspVibrato32;
  public
    property Vibrato[Index: Integer]: TDspVibrato32 read GetVibrato;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Approximations;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';


{ TVibratoModule }

procedure TVibratoModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TVibratoModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TVibratoModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to 1 do
  begin
   FVibrato[Channel] := TDspVibrato32.Create;
   FVibrato[Channel].SampleRate := SampleRate;
  end;

 // initialize parameters
 Parameter[0] :=  0.2;
 Parameter[1] :=  2;
 with Programs[0] do
  begin
   Parameter[0] :=  0.2;
   Parameter[1] :=  2;
  end;
 with Programs[1] do
  begin
   Parameter[0] :=  0.02;
   Parameter[1] :=  1;
  end;
 with Programs[2] do
  begin
   Parameter[0] :=  0.04;
   Parameter[1] :=  4;
  end;
 with Programs[3] do
  begin
   Parameter[0] :=  0.62;
   Parameter[1] :=  4;
  end;
 with Programs[4] do
  begin
   Parameter[0] :=  1.3;
   Parameter[1] :=  8;
  end;
 with Programs[5] do
  begin
   Parameter[0] :=  2.5;
   Parameter[1] :=  10;
  end;
 with Programs[6] do
  begin
   Parameter[0] :=  2.5;
   Parameter[1] :=  10;
  end;
 with Programs[7] do
  begin
   Parameter[0] :=  0.33;
   Parameter[1] :=  12;
  end;
end;

procedure TVibratoModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FVibrato[0]);
 FreeAndNil(FVibrato[1]);
end;

function TVibratoModule.GetVibrato(Index: Integer): TDspVibrato32;
begin
 if Index in [0..1]
  then Result := FVibrato[Index]
  else raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

procedure TVibratoModule.ParamSpeedChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FVibrato[0]) then FVibrato[0].Speed := Value;
  if Assigned(FVibrato[1]) then FVibrato[1].Speed := Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVibratoModule.ParamDepthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FVibrato[0]) then FVibrato[0].Depth := 0.01 * Value;
  if Assigned(FVibrato[1]) then FVibrato[1].Depth := 0.01 * Value;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVibratoModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FVibrato[Channel].ProcessSample32(Inputs[Channel, Sample])
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVibratoModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  Channel, Sample : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FastTanhContinousError4(FVibrato[Channel].ProcessSample32(Inputs[Channel, Sample]))
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TVibratoModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Abs(SampleRate) = 0 then Exit;

 FCriticalSection.Enter;
 try
  if Assigned(FVibrato[0]) then FVibrato[0].SampleRate := Abs(SampleRate);
  if Assigned(FVibrato[1]) then FVibrato[1].SampleRate := Abs(SampleRate);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
