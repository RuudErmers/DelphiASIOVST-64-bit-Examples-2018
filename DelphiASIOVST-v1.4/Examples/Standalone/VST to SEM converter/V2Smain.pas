unit V2Smain;

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
  Graphics, Controls, Forms, Dialogs, Menus, ComCtrls, StdCtrls, XPMan, 
  DAV_VSTHost, DAV_SEHost;

type
  TFmVST2SEM = class(TForm)
    LbName: TLabel;
    LbSemAbout: TLabel;
    LbSemId: TLabel;
    LbVSTName: TLabel;
    MainMenu: TMainMenu;
    MemoAbout: TMemo;
    MemoID: TMemo;
    MemoInfo: TMemo;
    MemoName: TMemo;
    MIBatchConvert: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    MISaveAs: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    PC: TPageControl;
    TSSEMProperties: TTabSheet;
    TSVSTPlugin: TTabSheet;
    VstHost: TVstHost;
    VstName: TEdit;
    XPManifest: TXPManifest;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MISaveClick(Sender: TObject);
    procedure MIBatchConvertClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FVSTPluginDLL : TFileName;
    procedure SaveModule(Filename: TFileName);
    procedure OpenVSTFile(FileName: TFileName);
  end;

var
  FmVST2SEM: TFmVST2SEM;

implementation

uses
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} FileCtrl,
  DAV_DLLResources;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmVST2SEM.FormShow(Sender: TObject);
begin
 if UpperCase(InputBox('Confirmation',
  'Some plugins might come with a license, that doesn''t allow them to be wrapped in this kind of wrapper.' + #13#10#13#10 + 
  'By typing ''I CONFIRM'' you have to confirm, that you read the license of the plugins you wrap carefully ' +
  'and that you won''t create converted SE modules if the license does not allow this.', '')) <> 'I CONFIRM'
   then exit;
end;

procedure TFmVST2SEM.MIBatchConvertClick(Sender: TObject);
var
  Dir : string;
  SR  : TSearchRec;
begin
 Dir := '';
 SelectDirectory('Select a directory', '', Dir);
 if Dir = '' then exit;
 if FindFirst(Dir + '\' + '*.dll', faAnyFile, SR) = 0 then
  begin
   repeat
    try
     OpenVSTFile(Dir + '\' + SR.Name);
     SaveModule(Dir + '\' + SR.Name + '.sem');
    finally
     Application.ProcessMessages;
    end;
   until FindNext(SR) <> 0;

   // Must free up resources used by these successful finds
   FindClose(SR);
  end;
 ShowMessage('Wrapped SEMs successfully created!');
end;

procedure TFmVST2SEM.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmVST2SEM.MIOpenClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  begin
   DefaultExt := '.dll';
   Filter := 'VST Plugin (*.DLL)|*.DLL';
   Options := Options + [ofFileMustExist];
   Title := 'Select a VST Plugin';
   if Execute then OpenVSTFile(FileName)
  end;
end;

procedure TFmVST2SEM.OpenVSTFile(FileName: TFileName);
var
  str : AnsiString;
begin
 try
  FVSTPluginDLL := FileName;
  if VstHost[0].Active then
   begin
    VstHost[0].Close;
    VstHost[0].UnLoad;
   end;
  VstHost[0].LoadFromFile(FileName);
  VstHost[0].Open;
  str := Trim(VstHost[0].GetEffectName);
  if str = '' then
   begin
    str := AnsiString(ExtractFileName(FVSTPluginDLL));
    if Pos(AnsiChar('.'), str) > 1 then SetLength(str, Pos(AnsiChar('.'), str) - 1);
   end;
  while Pos(AnsiChar(' '), str) > 0 do str[Pos(AnsiChar(' '), str)] := '_';
  VstName.Text := string(Uppercase(str));
  MemoName.Clear;
  MemoName.Lines.Add('DAV VST-Wrapper - ' + VstName.Text);
  MemoID.Clear;
  MemoID.Lines.Add('VST2SEM - ' + VstName.Text);
  MemoInfo.Clear;
  MemoInfo.Lines.Add('Inputs: ' + IntToStr(VstHost[0].numInputs));
  MemoInfo.Lines.Add('Outputs: ' + IntToStr(VstHost[0].numOutputs));
  MemoInfo.Lines.Add('Parameters: ' + IntToStr(VstHost[0].numParams));
  MISaveAs.Enabled := True;
 except
  MISaveAs.Enabled := False;
 end;
end;

procedure TFmVST2SEM.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  begin
   DefaultExt := '.SEM';
   Filter     := 'SE Module (*.SEM)|*.SEM';
   Title      := 'Save as SEM Module';
   FileName   := FVSTPluginDLL + '.sem';
   if Execute then
    begin
     SaveModule(FileName);
//     ShowMessage('Wrapped SEM successfully created!');
    end;
  end;
end;

procedure TFmVST2SEM.MISaveClick(Sender: TObject);
begin
 SaveModule(FVSTPluginDLL + '.sem');
end;

procedure TFmVST2SEM.SaveModule(Filename: TFileName);
var
  RS : TResourceStream;
  RM : TPEResourceModule;
  RD : TResourceDetails;
begin
 assert(FileExists(FVSTPluginDLL));

 RM := TPEResourceModule.Create;
 with RM do
  try
   // load template
   RS := TResourceStream.Create(HInstance, 'VST2SEM', 'SEM');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;

   while ResourceCount > 0 do DeleteResource(0);

   // store SE modules
   with TMemoryStream.Create do
    try
     LoadFromFile(FVSTPluginDLL);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, VstName.Text, 'VST', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   SortResources;
   SaveToFile(Filename);
  finally
   FreeAndNil(RM);
  end;
end;

end.
