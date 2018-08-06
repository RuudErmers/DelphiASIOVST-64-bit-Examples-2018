unit PPmain;

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
  Graphics, Controls, Forms, Dialogs, Menus, StdCtrls, DAV_VSTHost;

type
  TFmPresetParser = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    MIOpenVSTPlugin: TMenuItem;
    MIExit: TMenuItem;
    N1: TMenuItem;
    MISaveAs: TMenuItem;
    Memo: TMemo;
    VstHost: TVstHost;
    MISettings: TMenuItem;
    MIDetectBufferOverflows: TMenuItem;
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenVSTPluginClick(Sender: TObject);
    procedure MISaveAsClick(Sender: TObject);
    procedure MIDetectBufferOverflowsClick(Sender: TObject);
  end;

var
  FmPresetParser: TFmPresetParser;

implementation

{$R *.dfm}

procedure TFmPresetParser.MIDetectBufferOverflowsClick(Sender: TObject);
begin
 MIDetectBufferOverflows.Checked := not MIDetectBufferOverflows.Checked;
 VSTHost.CheckStringLengths := MIDetectBufferOverflows.Checked;
end;

procedure TFmPresetParser.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmPresetParser.MIOpenVSTPluginClick(Sender: TObject);
var
  prgs, params : Integer;
  str          : string;
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
   Title := 'Load VST Plugin';
   if Execute then
    begin
     with VstHost.VstPlugIns.Add do
      try
       LoadFromFile(FileName);
       Active := True;
       Memo.Lines.Clear;
       for prgs := 0 to numPrograms - 1 do
        begin
         CurrentProgram := prgs;
         str := 'Program: ' + ProgramName;
         Memo.Lines.Add(str);
         FillChar(str[1], Length(str), '-');
         Memo.Lines.Add(str);
         for params := 0 to numParams - 1 do
          try
           Memo.Lines.Add('Parameter ' + IntToStr(params +  1) + ': ' +
             ParameterName[params] + ' = ' + ParameterDisplay[params] + ' ' +
             ParameterLabel[params]);
          except
           Memo.Lines.Add('Parameter ' + IntToStr(params +  1) + ': Error!');  
          end;
         Memo.Lines.Add('');
        end;
       Active := False;
      finally
       VstHost.VstPlugIns.Clear;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmPresetParser.MISaveAsClick(Sender: TObject);
begin
 with TSaveDialog.Create(Self) do
  try
   DefaultExt := '.dll';
   Filter := 'Text (*.txt)|*.txt';
   Title := 'Save as...';
   if Execute
    then Memo.Lines.SaveToFile(FileName);
  finally
   Free;
  end;
end;

end.
