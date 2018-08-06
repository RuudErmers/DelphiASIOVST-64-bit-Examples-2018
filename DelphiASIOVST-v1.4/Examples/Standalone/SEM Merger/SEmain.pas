unit SEmain;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, DAV_SEModule, DAV_SEHost;

type
  TFmSEModuleExplorer = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MISaveAs: TMenuItem;
    LBSEMs: TListBox;
    MINew: TMenuItem;
    MiSettings: TMenuItem;
    MiAddMerged: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MINewClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MiAddMergedClick(Sender: TObject);
  private
    procedure SaveModule(Filename: TFileName);
  end;

var
  FmSEModuleExplorer: TFmSEModuleExplorer;

implementation

uses
  IniFiles, DAV_DLLResources, DAV_SECommon;

{$R *.dfm}

procedure TFmSEModuleExplorer.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.sem';
   Filter := 'SE Modules (*.sem)|*.sem;*.sep';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Select SE Module';
   if Execute then
    begin
     LBSEMs.Items.Add(FileName);
     MISaveAs.Enabled := True;
    end;
  end;
end;

procedure TFmSEModuleExplorer.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  begin
   DefaultExt := '.sem';
   Filter := 'SE Modules (*.sem)|*.sem';
   Options := [ofHideReadOnly, ofEnableSizing];
   Title := 'Save as SE Module';
   if Execute then SaveModule(FileName);
  end;
end;

procedure TFmSEModuleExplorer.SaveModule(Filename: TFileName);
var
  RS : TResourceStream;
  RM : TPEResourceModule;
  RD : TResourceDetails;
  i  : Integer;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try
   // load template
   RS := TResourceStream.Create(HInstance, 'SEMerger', 'SEM');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;

   // store SE modules
   for i := 0 to LBSEMs.Count - 1 do
    with TMemoryStream.Create do
     try
      Assert(FileExists(LBSEMs.Items[i]));
      LoadFromFile(LBSEMs.Items[i]);
      if MiAddMerged.Checked
       then RD := TResourceDetails.CreateResourceDetails(RM, 0, 'SEMmerged' + IntToStr(i), 'SEM', Size, Memory)
       else RD := TResourceDetails.CreateResourceDetails(RM, 0, 'SEM' + IntToStr(i), 'SEM', Size, Memory);
      AddResource(RD);
     finally
      Free;
     end;

   SortResources;
   SaveToFile(Filename);
   ShowMessage('Merged SEM successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

procedure TFmSEModuleExplorer.MINewClick(Sender: TObject);
begin
 LBSEMs.Clear;
 MISaveAs.Enabled := False;
end;

procedure TFmSEModuleExplorer.MiAddMergedClick(Sender: TObject);
begin
 MiAddMerged.Checked := not MiAddMerged.Checked; 
end;

procedure TFmSEModuleExplorer.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
