unit SortedDisorderDM;

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
  Forms, SyncObjs, DAV_Types, DAV_VSTModule, DAV_DspBlockDistribution;

type
  TSortedDisorderModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray;
      const SampleFrames: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParameterBlocksizeChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FBuildingBuffer  : array of TBlockDisorder32;
  public
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  SortedDisorderGui;

procedure TSortedDisorderModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = numOutputs);
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSortedDisorderModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSortedDisorderModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 SetLength(FBuildingBuffer, numInputs);
 for Channel := 0 to Length(FBuildingBuffer) - 1 do
  begin
   FBuildingBuffer[Channel] := TBlockDisorder32.Create;
   FBuildingBuffer[Channel].BlockSize := 1 shl 8;
  end;

 // initialize parameter
 Parameter[0] := 256;

 // set editor form class
 EditorFormClass := TFmSortedDisorder;
end;

procedure TSortedDisorderModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FBuildingBuffer) - 1
  do FreeAndNil(FBuildingBuffer[Channel]);
end;

procedure TSortedDisorderModule.ParameterBlocksizeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FBuildingBuffer) - 1
   do FBuildingBuffer[Channel].BlockSize := 2 * Round(0.5 * Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSortedDisorderModule.ParameterFilterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FBuildingBuffer) - 1
   do FBuildingBuffer[Channel].FilterOrder := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSortedDisorderModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
(*
  for Channel := 0 to Length(FBuildingBuffer) - 1
   do FBuildingBuffer[Channel].ProcessBlock32(@Inputs[Channel, 0],
    @Outputs[Channel, 0], SampleFrames * SizeOf(Single));
*)
  for Channel := 0 to Length(FBuildingBuffer) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FBuildingBuffer[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
