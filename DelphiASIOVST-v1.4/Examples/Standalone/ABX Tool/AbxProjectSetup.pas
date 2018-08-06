unit AbxProjectSetup;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, Dialogs, StdCtrls, DAV_AudioFile, DAV_AudioFileWAV;

type
  TFmProjectSetup = class(TForm)
    BtCancel: TButton;
    BtOk: TButton;
    BtSelectA: TButton;
    BtSelectB: TButton;
    CBProtectSettings: TCheckBox;
    EdFilenameA: TEdit;
    EdFilenameB: TEdit;
    LbSelectA: TLabel;
    LbSelectB: TLabel;
    procedure BtOkClick(Sender: TObject);
    procedure BtSelectAClick(Sender: TObject);
    procedure BtSelectBClick(Sender: TObject);
    procedure EdFilenameChange(Sender: TObject);
  private
  public
  end;

implementation

uses
  Inifiles, AbxMain;

{$R *.dfm}

resourcestring
  RCStrFileDoesNotExist = 'File does not exist anymore!';
  RCStrWaveFileFilter = 'Wave File (*.wav)|*.wav';
  RCStrIdenticalFiles = 'You must choose two different files for comparison';
  RCStrInvalidWavFile = 'File %s is not a valid WAV file!';
  RCStrSamplerateMismatch = 'Samplerate mismatch!';

procedure TFmProjectSetup.BtOkClick(Sender: TObject);
var
  SR : Single;
begin
 try
  if not (FileExists(EdFilenameA.Text) and FileExists(EdFilenameB.Text))
   then raise Exception.Create(RCStrFileDoesNotExist);
  if EdFilenameA.Text = EdFilenameB.Text
   then raise Exception.Create(RCStrIdenticalFiles);

  with TAudioFileWAV.Create do
   try
    LoadFromFile(EdFilenameA.Text);
    SR := SampleRate;
    LoadFromFile(EdFilenameB.Text);
    if SR <> SampleRate
     then raise Exception.Create(RCStrSamplerateMismatch);
   finally
    Free;
   end;
 except
  on E: Exception do
   begin
    ModalResult := mrNone;
    MessageDlg('Error: ' + E.Message, mtError, [mbOK], 0);
   end;
 end;
end;

procedure TFmProjectSetup.BtSelectAClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   Filter := RCStrWaveFileFilter;
   Options := Options + [ofFileMustExist];
   with TIniFile.Create(FmAbxMain.IniFileName) do
    try
     InitialDir := ReadString('Recent', 'Audio Directory', InitialDir);
    finally
     Free;
    end;
   if Execute then
    begin
     if not TCustomAudioFileWAV.CanLoad(FileName)
      then raise Exception.CreateFmt(RCStrInvalidWavFile, [FileName]);

     EdFilenameA.Text := FileName;
     InitialDir := ExtractFileDir(FileName);
     with TIniFile.Create(FmAbxMain.IniFileName) do
      try
       WriteString('Recent', 'Audio Directory', InitialDir);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmProjectSetup.BtSelectBClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := '.wav';
   Filter := RCStrWaveFileFilter;
   Options := Options + [ofFileMustExist];
   with TIniFile.Create(FmAbxMain.IniFileName) do
    try
     InitialDir := ReadString('Recent', 'Audio Directory', InitialDir);
    finally
     Free;
    end;
   if Execute then
    begin
     if not TCustomAudioFileWAV.CanLoad(FileName)
      then raise Exception.CreateFmt(RCStrInvalidWavFile, [FileName]);

     EdFilenameB.Text := FileName;
     InitialDir := ExtractFileDir(FileName);
     with TIniFile.Create(FmAbxMain.IniFileName) do
      try
       WriteString('Recent', 'Audio Directory', InitialDir);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmProjectSetup.EdFilenameChange(Sender: TObject);
begin
 BtOk.Enabled := FileExists(EdFilenameA.Text) and
                 FileExists(EdFilenameB.Text);
end;

end.
