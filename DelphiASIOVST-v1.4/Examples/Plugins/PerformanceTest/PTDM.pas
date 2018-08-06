unit PTDM;

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
  FastMove, {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, 
  Classes, Forms, DAV_Types, DAV_VSTEffect, DAV_VSTModule;

type
  TPerformanceTestModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; SampleFrames: Integer);
  private
    function GetProcessorCycles: Double;
  public
    Cycles       : Double;
    CycleCounter : Integer;
    procedure PatchProcessCalls;
    property ProcessorCycles: Double read GetProcessorCycles;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  PTGUI, DAV_VSTBasicModule;

var
  StartStop: function: Int64;

function RDTSC: Int64;
asm
    DW  0310Fh
end;

function NoRDTSC: Int64;
begin
  Result := 0;
end;

function IsRDTSCPresent: Boolean;

  function HasRDTSC: Boolean; assembler;
  asm
      PUSH  EBX
      PUSHFD
      PUSHFD
      POP   EAX
      MOV   EDX,EAX
      XOR   EAX,0040000h
      PUSH  EAX
      POPFD
      PUSHFD
      POP   EAX
      XOR   EAX,EDX
      JZ   @@1
      PUSHFD
      POP   EAX
      MOV   EDX,EAX
      XOR   EAX,0200000h
      PUSH  EAX
      POPFD
      PUSHFD
      POP   EAX
      XOR   EAX,EDX
      @@1:  POPFD
      TEST  EAX,EAX
      JZ   @@2
      MOV   EAX,1
      DW   0A20Fh
      TEST  EDX,010h
      SETNZ  AL
      @@2:  POP   EBX
  end;

begin
   try
    Result := HasRDTSC;
    if Result then
      RDTSC;
   except
    Result := False;
   end;
end;

procedure ProcessFunc(Effect: PVSTEffect; Inputs, Outputs: PPSingle;
  SampleFrames: Integer); cdecl;
var
  Start, Stop: Int64;
begin
  with TPerformanceTestModule(Effect^.vObject) do
   begin
    Start := StartStop;
    HostCallProcess(Inputs, Outputs, SampleFrames);
    Stop := StartStop;
    Cycles := CycleCounter / (CycleCounter + 1) * Cycles +
      (Stop - Start) / ((CycleCounter + 1) * SampleFrames);
    Inc(CycleCounter);
   end;
end;

procedure ProcessReplacingFunc(Effect: PVSTEffect;
  Inputs, Outputs: PPSingle; SampleFrames: Integer); cdecl;
var
  Start, Stop: Int64;
begin
  with TPerformanceTestModule(Effect^.vObject) do
   begin
    Start := StartStop;
    HostCallProcessReplacing(Inputs, Outputs, SampleFrames);
    Stop := StartStop;
    Cycles := CycleCounter / (CycleCounter + 1) * Cycles +
      (Stop - Start) / ((CycleCounter + 1) * SampleFrames);
    Inc(CycleCounter);
   end;
end;

procedure ProcessDoubleReplacingFunc(Effect: PVSTEffect;
  Inputs, Outputs: PPDouble; SampleFrames: Integer); cdecl;
var
  Start, Stop: Int64;
begin
  with TPerformanceTestModule(Effect^.vObject) do
   begin
    Start := StartStop;
    HostCallProcessDoubleReplacing(Inputs, Outputs, SampleFrames);
    Stop := StartStop;
    Cycles := CycleCounter / (CycleCounter + 1) * Cycles +
      (Stop - Start) / ((CycleCounter + 1) * SampleFrames);
    Inc(CycleCounter);
   end;
end;

function TPerformanceTestModule.GetProcessorCycles: Double;
begin
  Result := Cycles;
end;

procedure TPerformanceTestModule.PatchProcessCalls;
begin
  CycleCounter := 0;
  Cycles := 0;
  FEffect.Process := ProcessFunc;
  FEffect.ProcessReplacing := ProcessReplacingFunc;
  FEffect.ProcessDoubleReplacing := ProcessDoubleReplacingFunc;
end;

procedure TPerformanceTestModule.VSTModuleCreate(Sender: TObject);
var
  DataTest   : TDAVDoubleDynArray;
  DataGetmem : PSingle;
begin
 {$IFDEF Delphi10_UP}
  SetMinimumBlockAlignment(mba8Byte);
 {$ENDIF}
  DataGetmem := GetMemory(8192 * SizeOf(Single));
  SetLength(DataTest, 8192);
  FindMaximum(PSingle(@DataTest[0]), 8192);
  FindMaximum(DataGetmem, 8192);
end;

procedure TPerformanceTestModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmPerformanceTest.Create(Self);
end;

procedure TPerformanceTestModule.VSTModuleProcess(
  const Inputs, Outputs: TDAVArrayOfSingleFixedArray; SampleFrames: Integer);
begin
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TPerformanceTestModule.VSTModuleProcessDoubleReplacing(
  const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; SampleFrames: Integer);
begin
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Double));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Double));
end;

initialization
 if IsRDTSCPresent
  then StartStop := RDTSC
  else StartStop := NoRDTSC;

end.
