unit PSGUI;

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
  Forms, Controls, ExtCtrls, StdCtrls, ActnList, ComCtrls, ToolWin, Dialogs, 
  DAV_Types, DAV_VSTModule, SynEdit, SynEditHighlighter, SynHighlighterPas, 
  uPSCompiler, uPSUtils;

type
  TFmPascalScript = class(TForm)
    ACCompile: TAction;
    ActionList: TActionList;
    BtLoadScript: TButton;
    BtRun: TButton;
    BtSaveScript: TButton;
    DebugBox: TListBox;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Splitter: TSplitter;
    SynEdit: TSynEdit;
    SynPasSyn: TSynPasSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACCompileExecute(Sender: TObject);
    procedure SynEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtLoadScriptClick(Sender: TObject);
    procedure BtSaveScriptClick(Sender: TObject);
  private
    FCompiler : TPSPascalCompiler;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  PSDM;

resourcestring
  STR_SUCCESSFULLY_COMPILED = 'Succesfully compiled';

function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
begin
 if Proc.Name = 'VSTPROCESSSAMPLE' then
  begin
   if not ExportCheck(Sender, Proc, [btReturnAddress, btS32, btDouble], [pmIn, pmInOut]) then // Check if the proc has the correct params.
    begin
     Sender.MakeError('', ecTypeMismatch, '');
     Result := False;
     Exit;
    end;
   Result := True;
  end
 else Result := True;
end;

procedure TFmPascalScript.ACCompileExecute(Sender: TObject);
var
  str : AnsiString;
  i   : Integer;
begin
 DebugBox.Items.Clear;
 if not FCompiler.Compile(SynEdit.Lines.Text) then
  for i := 0 to FCompiler.MsgCount - 1
   do DebugBox.Items.Add(FCompiler.Msg[i].MessageToString)
 else
  begin
   FCompiler.GetOutput(str);
   TPascalScriptDataModule(Owner).ByteCode := str;
   TPascalScriptDataModule(Owner).ScriptCode := AnsiString(SynEdit.Lines.Text);
   DebugBox.Items.Add(STR_SUCCESSFULLY_COMPILED);
  end;
end;

procedure TFmPascalScript.BtLoadScriptClick(Sender: TObject);
begin
 if OpenDialog.Execute
  then SynEdit.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TFmPascalScript.BtSaveScriptClick(Sender: TObject);
begin
 if SaveDialog.Execute
  then SynEdit.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TFmPascalScript.FormCreate(Sender: TObject);
begin
 FCompiler := TPSPascalCompiler.Create; // create an instance of the compiler.
 with FCompiler do
  begin
   OnExportCheck := ScriptOnExportCheck; // Assign the onExportCheck event.
   AllowNoBegin := True;
   AllowNoEnd := True; // AllowNoBegin and AllowNoEnd allows it that begin and end are not required in a script.
  end;
end;

procedure TFmPascalScript.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FCompiler);
end;

procedure TFmPascalScript.SynEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if Key = 120 then ACCompileExecute(Sender);
end;

end.
