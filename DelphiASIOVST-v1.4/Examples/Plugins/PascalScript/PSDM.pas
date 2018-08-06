unit PSDM;

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
  Forms, DAV_Types, DAV_VSTModule, DAV_VSTPrograms, uPSRuntime;

type
  TVSTProcessSample = procedure (Channel : Integer; var Data : Double) of object;

  TPascalScriptDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VPSStoreChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
    procedure VPSLoadChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
  private
    FPascalScriptExecuter : TPSExec;
    FByteCode             : AnsiString;
    FScriptCode           : AnsiString;
    FVSTProcessSample     : TVSTProcessSample;
    procedure SetByteCode(const Value: AnsiString);
  public
    property ByteCode: AnsiString read FByteCode write SetByteCode;
    property ScriptCode: AnsiString read FScriptCode write FScriptCode;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  PSGUI, DAV_VSTCustomModule;

procedure TPascalScriptDataModule.VSTModuleCreate(Sender: TObject);
begin
 FPascalScriptExecuter := TPSExec.Create;
end;

procedure TPascalScriptDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FPascalScriptExecuter);
end;

procedure TPascalScriptDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmPascalScript.Create(Self);
 if Length(FScriptCode) > 0 then
  with TFmPascalScript(GUI)
   do SynEdit.LineText := string(FScriptCode);
end;

procedure TPascalScriptDataModule.VPSLoadChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
var
  AtomName : array [0..3] of Char;
  AtomSize : Integer;
begin
 with (Sender as TVstProgram).Chunk do
  begin
   Position := 0;
   if Size < 8 then exit;
   Read(AtomName, 4);
   Read(AtomSize, 4);
   if AtomSize > 0 then
    begin
     SetLength(FScriptCode, AtomSize);
     Read(FScriptCode[1], AtomSize);
     if Assigned(EditorForm) then
      with TFmPascalScript(EditorForm)
       do SynEdit.Lines.Text := string(ScriptCode);
    end;
  end;
end;

procedure TPascalScriptDataModule.VPSStoreChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
var
  AtomName : array [0..3] of Char;
  AtomSize : Integer;
begin
 with (Sender as TVstProgram).Chunk do
  begin
   Position := 0;
   AtomName := 'VPSS';
   Write(AtomName, 4);
   AtomSize := Length(FScriptCode);
   Write(AtomSize, 4);
   Write(FScriptCode[1], AtomSize);
  end;
end;

procedure TPascalScriptDataModule.SetByteCode(const Value: AnsiString);
begin
 if FByteCode <> Value then
  begin
   FByteCode := Value;
   try
    if FPascalScriptExecuter.LoadData(FByteCode)
     then FVSTProcessSample := TVSTProcessSample(FPascalScriptExecuter.GetProcAsMethodN('VSTProcessSample'));
   except
    FVSTProcessSample := nil;
   end;
  end;
end;

procedure TPascalScriptDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  i : Integer;
  d : Double;
begin
 if @FVSTProcessSample <> nil then
  for i := 0 to SampleFrames - 1 do
   begin
    d := Inputs[0, i]; FVSTProcessSample(0, d); Outputs[0, i] := d;
    d := Inputs[1, i]; FVSTProcessSample(1, d); Outputs[1, i] := d;
   end
 else
  begin
   Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
   Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
  end;
end;

procedure TPascalScriptDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var i : Integer;
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
 if @FVSTProcessSample <> nil then
  for i := 0 to SampleFrames - 1 do
   begin
    FVSTProcessSample(0, Outputs[0, i]);
    FVSTProcessSample(1, Outputs[1, i]);
   end;
end;

end.
