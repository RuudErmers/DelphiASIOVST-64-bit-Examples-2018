unit ECImain;

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
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, DAV_DLLResources;

type
  TFmSemEmbedConvolutionIR = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MIOpenSEM: TMenuItem;
    MIAddIR: TMenuItem;
    MISaveAs: TMenuItem;
    N2: TMenuItem;
    MISave: TMenuItem;
    OpenDialogSEM: TOpenDialog;
    OpenDialogWAV: TOpenDialog;
    ListBox: TListBox;
    SaveDialogSEM: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenSEMClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MIAddIRClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFileName : TFileName;
  public
    FSEModule : TPEResourceModule;
  end;

var
  FmSemEmbedConvolutionIR: TFmSemEmbedConvolutionIR;

implementation

{$R *.dfm}

procedure TFmSemEmbedConvolutionIR.FormCreate(Sender: TObject);
var
  RS : TResourceStream;
begin
 FSEModule := TPEResourceModule.Create;
 RS := TResourceStream.Create(HInstance, 'ConvolutionModule', 'DLL');
 try
  FSEModule.LoadFromStream(RS);
 finally
  FreeAndNil(RS);
 end;
end;

procedure TFmSemEmbedConvolutionIR.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FSEModule);
end;

procedure TFmSemEmbedConvolutionIR.MIOpenSEMClick(Sender: TObject);
var
  i : Integer;
begin
 with OpenDialogSEM do
  if Execute then
   try
    ListBox.Clear;

    // clear existing resources
    while FSEModule.ResourceCount > 0
     do FSEModule.DeleteResource(0);

    // load file
    FSEModule.LoadFromFile(FileName);

    for i := 0 to FSEModule.ResourceCount - 1 do
     if FSEModule.ResourceDetails[i].ResourceType = 'IR'
      then ListBox.Items.Add(FSEModule.ResourceDetails[i].ResourceName);

    FFileName := FileName;
   finally
    MISave.Enabled := FileExists(FFileName);
   end;
end;

procedure TFmSemEmbedConvolutionIR.MISaveAsClick(Sender: TObject);
begin
 with SaveDialogSEM do
  if Execute then
   try
    FSEModule.SortResources;
    FSEModule.SaveToFile(FileName);
    FFileName := FileName;
   finally
    MISave.Enabled := FileExists(FFileName);
   end;
end;

procedure TFmSemEmbedConvolutionIR.MISaveClick(Sender: TObject);
begin
 if FFileName <> ''
  then FSEModule.SaveToFile(FFileName)
  else MISave.Enabled := False;
end;

procedure TFmSemEmbedConvolutionIR.MIAddIRClick(Sender: TObject);
var
  Index : Integer;
  RD    : TResourceDetails;
begin
 with OpenDialogWAV do
  if Execute then
   for Index := 0 to Files.Count - 1 do
    begin
     with TMemoryStream.Create do
      try
       ListBox.Items.Add(ExtractFileName(Files[Index]));
       LoadFromFile(Files[Index]);
       RD := TResourceDetails.CreateResourceDetails(FSEModule, 0, 'IR' + IntToStr(Index), 'IR', Size, Memory);
       FSEModule.AddResource(RD);
      finally
       Free;
      end;
    end;
end;

procedure TFmSemEmbedConvolutionIR.MIExitClick(Sender: TObject);
begin
 Close;
end;

end.
