unit SPCmain;

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
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Messages, SysUtils, Classes,
  Controls, Forms, Dialogs, StdCtrls, DAV_GuiBaseControl, DAV_ChunkClasses,
  DAV_ChunkPluginGUI;

type
  TFmSplitPluginCreator = class(TForm)
    BtClearA: TButton;
    BtClearB: TButton;
    BtCreate: TButton;
    BtOpenA: TButton;
    BtOpenB: TButton;
    EdPluginA: TEdit;
    EdPluginB: TEdit;
    LbPluginA: TLabel;
    LbPluginB: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtClearAClick(Sender: TObject);
    procedure BtClearBClick(Sender: TObject);
    procedure BtCreateClick(Sender: TObject);
    procedure BtOpenAClick(Sender: TObject);
    procedure BtOpenBClick(Sender: TObject);
    procedure EdPluginChange(Sender: TObject);
    procedure EdPluginClick(Sender: TObject);
  private
    procedure SavePlugin(FileName: TFileName);
  end;

var
  FmSplitPluginCreator: TFmSplitPluginCreator;

implementation

uses
  IniFiles, ShellAPI, DAV_DLLResources;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmSplitPluginCreator.FormCreate(Sender: TObject);
begin
 {$IFDEF Registered}
 EdPluginB.Enabled := True;
 {$ENDIF}
 with TIniFile.Create('SplitPluginCreator.ini') do
  try
   EdPluginA.Text := ReadString('Last State','Plugin A', EdPluginA.Text);
   if EdPluginB.Enabled
    then EdPluginB.Text := ReadString('Last State','Plugin B', EdPluginB.Text)
    else EdPluginB.Text := 'please donate to unlock this feature';
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.FormDestroy(Sender: TObject);
begin
 with TIniFile.Create('SplitPluginCreator.ini') do
  try
   WriteString('Last State','Plugin A', EdPluginA.Text);
   WriteString('Last State','Plugin B', EdPluginB.Text);
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.SavePlugin(FileName: TFileName);
var
  RS  : TResourceStream;
  RM  : TPEResourceModule;
  RD  : TResourceDetails;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try
   RS := TResourceStream.Create(HInstance, 'SplitTemplate', 'DLL');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;
//       LoadFromFile(FileName);

   // store VST Plugins
   with TMemoryStream.Create do
    try
     LoadFromFile(EdPluginA.Text);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST1', 'DLL', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   // store VST Plugins
   if EdPluginB.Enabled and FileExists(EdPluginB.Text) then
    with TMemoryStream.Create do
     try
      LoadFromFile(EdPluginB.Text);
      RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST2', 'DLL', Size, Memory);
      AddResource(RD);
     finally
      Free;
     end;

   SortResources;
   SaveToFile(FileName);
   ShowMessage('Plugin successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

procedure TFmSplitPluginCreator.BtCreateClick(Sender: TObject);
begin
 if not FileExists(EdPluginA.Text) then
  begin
   BtCreate.Enabled := False;
   exit;
  end;
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Title := 'Save As VST DLL';
   if Execute then SavePlugin(FileName);
  finally
   Free;
  end;
end;

procedure TFmSplitPluginCreator.BtClearAClick(Sender: TObject);
begin
 EdPluginA.Text := '';
end;

procedure TFmSplitPluginCreator.BtClearBClick(Sender: TObject);
begin
 if EdPluginB.Enabled
  then EdPluginB.Text := '';
end;

procedure TFmSplitPluginCreator.BtOpenAClick(Sender: TObject);
begin
 EdPluginClick(EdPluginA);
end;

procedure TFmSplitPluginCreator.BtOpenBClick(Sender: TObject);
begin
 if EdPluginB.Enabled
  then EdPluginClick(EdPluginB)
  else ShellExecute(Handle, 'open', PChar('http://delphiasiovst.sourceforge.net'), nil, nil, SW_SHOWNORMAL);
end;

procedure TFmSplitPluginCreator.EdPluginChange(Sender: TObject);
begin
 BtCreate.Enabled := FileExists(EdPluginA.Text);
end;

procedure TFmSplitPluginCreator.EdPluginClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Title := 'Choose a plugin that should be used for splitting';
   Options := Options + [ofFileMustExist];
   if Execute then
    with Sender as TEdit
     do Text := FileName;
  finally
   Free;
  end;
end;

end.
