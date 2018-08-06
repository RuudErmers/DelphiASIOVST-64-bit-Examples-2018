unit AbxMain;

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
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Graphics, 
  Forms, Controls, Menus, StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, 
  ComCtrls, StdActns, ActnList, ToolWin, ImgList, DAV_ASIOHost, DAV_AudioFile, 
  DAV_AudioFileWav;

type
  TFmAbxMain = class(TForm)
    ACAudioSettings: TAction;
    ACCompileExecutable: TAction;
    ACExport: TAction;
    ACFileClose: TWindowClose;
    ACFileExit: TAction;
    ACFileNew: TAction;
    ACFileOpen: TAction;
    ACFileSave: TAction;
    ACFileSaveAs: TAction;
    ACHelpAbout: TAction;
    ACMergeTestResults: TAction;
    ACResultTableSetup: TAction;
    ACTestRun: TAction;
    ActionList: TActionList;
    ACWindowArrangeAll: TWindowArrange;
    ACWindowCascade: TWindowCascade;
    ACWindowMinimizeAll: TWindowMinimizeAll;
    ACWindowTileHorizontal: TWindowTileHorizontal;
    ACWindowTileVertical: TWindowTileVertical;
    ASIOHost: TASIOHost;
    HelpAboutItem: TMenuItem;
    ImageList: TImageList;
    MainMenu: TMainMenu;
    MIAudioSettings: TMenuItem;
    MIExport: TMenuItem;
    MIFile: TMenuItem;
    MIFileClose: TMenuItem;
    MIFileExit: TMenuItem;
    MIFileNew: TMenuItem;
    MIFileOpen: TMenuItem;
    MIFileSave: TMenuItem;
    MIFileSaveAs: TMenuItem;
    MIHelp: TMenuItem;
    MIOptions: TMenuItem;
    MIRecentFiles: TMenuItem;
    MIResultTableSetup: TMenuItem;
    MIStandaloneTest: TMenuItem;
    MIStandaloneTestCompileExecutable: TMenuItem;
    MIStandaloneTestMergeResults: TMenuItem;
    MIWindow: TMenuItem;
    MIWindowOverlap: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    StatusBar: TStatusBar;
    TBCascade: TToolButton;
    TBMeasure: TToolButton;
    TBNew: TToolButton;
    TBOpen: TToolButton;
    TBSave: TToolButton;
    TBSplitter1: TToolButton;
    TBSplitter2: TToolButton;
    TBTileHorizontal: TToolButton;
    TBTileVertical: TToolButton;
    ToolBar2: TToolBar;
    WindowArrangeItem: TMenuItem;
    WindowMinimizeItem: TMenuItem;
    WindowTileItem: TMenuItem;
    WindowTileItem2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ACAudioSettingsExecute(Sender: TObject);
    procedure ACCompileExecutableExecute(Sender: TObject);
    procedure ACExportExecute(Sender: TObject);
    procedure ACFileExitExecute(Sender: TObject);
    procedure ACFileNewExecute(Sender: TObject);
    procedure ACFileOpenExecute(Sender: TObject);
    procedure ACFileSaveAsExecute(Sender: TObject);
    procedure ACFileSaveExecute(Sender: TObject);
    procedure ACHelpAboutExecute(Sender: TObject);
    procedure ACMergeTestResultsExecute(Sender: TObject);
    procedure ACResultTableSetupExecute(Sender: TObject);
    procedure ACTestRunExecute(Sender: TObject);
    procedure CBDriversChange(Sender: TObject);
    procedure MIRecentFilesClick(Sender: TObject);
  private
    FIniFileName: TFileName;
    procedure LoadFile(FileName: TFileName);
  public
    procedure BuildRecentFileList(FileName: TFileName); virtual;
    procedure BuildRecentFilesMenuItems; virtual;
  published
    property IniFileName: TFileName read FIniFileName;
  end;

var
  FmAbxMain: TFmAbxMain;

implementation

{$R *.dfm}

uses
  Inifiles, AbxProject, AbxAbout, AbxAudio, AbxProjectSetup, AbxTestSetup,
  AbxResultTableSetup;

resourcestring
  RCStrNewABXTest = 'New ABX Test';
  RCStrSamplerateMismatch = 'Samplerate mismatch, please check your ASIO con' +
  'figuration!';

procedure TFmAbxMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i, j : Integer;
begin
 with TIniFile.Create(IniFileName) do
  try
   i := 0;
   j := 0;
   while i < MDIChildCount do
    begin
     assert(MDIChildren[i] is TFmProject);
     if FileExists(TFmProject(MDIChildren[i]).Filename) then
      begin
       inc(j);
       WriteString('Open Files', 'File ' + IntToStr(j), TFmProject(MDIChildren[i]).Filename);
      end;
     inc(i);
    end;
  finally
   Free;
  end;
end;

procedure TFmAbxMain.FormCreate(Sender: TObject);
begin
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'Abx.ini';
end;

procedure TFmAbxMain.FormShow(Sender: TObject);
var
  i      : Integer;
  StrLst : TStringList;
begin
 with TIniFile.Create(IniFileName) do
  try
   StrLst := TStringList.Create;
   try
    ReadSectionValues('Open Files', StrLst);
    EraseSection('Open Files');
    for i := 0 to StrLst.Count - 1
     do LoadFile(StrLst.Values[StrLst.Names[i]]);
   finally
    FreeAndNil(StrLst);
   end;
  finally
   Free;
  end;
 BuildRecentFilesMenuItems;
end;

procedure TFmAbxMain.CBDriversChange(Sender: TObject);
begin
 if assigned(FmAudioSettings)
  then FmAudioSettings.CBDriversChange(Sender);
end;

procedure TFmAbxMain.ACFileNewExecute(Sender: TObject);
var
  Child: TFmProject;
begin
 with TFmProjectSetup.Create(Self) do
  try
   if ShowModal = mrOk then
    begin
     // create new project
     Child := TFmProject.Create(Application);
     with Child do
      begin
       Caption := RCStrNewABXTest + ' '  + IntToStr(MDIChildCount + 1);
       EdTestTitle.Text := Caption;
       Width := 370;
       Height := 220;

       // load files
       AdcA.LoadFromFile(EdFilenameA.Text);
       AdcB.LoadFromFile(EdFilenameB.Text);

       // check if both files have the same length and truncate if necessary
       if AdcA.SampleFrames < AdcB.SampleFrames
        then AdcB.SampleFrames := AdcA.SampleFrames
        else AdcA.SampleFrames := AdcB.SampleFrames;

       StoreAudioData := CBProtectSettings.Checked;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmAbxMain.ACFileOpenExecute(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   DefaultExt := 'Abx';
   Filter := 'Abx files (*.Abx)|*.Abx';
   with TIniFile.Create(IniFileName) do
    try
     InitialDir := ReadString('Recent', 'Project Directory', InitialDir);
    finally
     Free;
    end;

   if Execute then
    begin
     LoadFile(FileName);
     BuildRecentFileList(FileName);

     InitialDir := ExtractFileDir(FileName);
     with TIniFile.Create(IniFileName) do
      try
       WriteString('Recent', 'Project Directory', InitialDir);
      finally
       Free;
      end;
    end;
  finally
   Free;
  end;
end;

procedure TFmAbxMain.LoadFile(FileName: TFileName);
var
  Child : TFmProject;
  i     : Integer;
begin
 for i := 0 to MDIChildCount - 1 do
  if TFmProject(MDIChildren[i]).Filename = FileName then
   begin
    TFmProject(MDIChildren[i]).BringToFront;
    Exit;
   end;
  
 Child := TFmProject.Create(Application);
 with Child do
  begin
   Caption := RCStrNewABXTest + ' '  + IntToStr(MDIChildCount + 1);
   EdTestTitle.Text := Caption;
   Width := 370;
   Height := 220;
  end;
 Child.LoadFromFile(FileName);
end;

procedure TFmAbxMain.MIRecentFilesClick(Sender: TObject);
var
  StrLst : TStringList;
begin
 with TIniFile.Create(IniFileName) do
  try
   StrLst := TStringList.Create;
   try
    ReadSectionValues('Recent Files', StrLst);
    LoadFile(StrLst.Values[StrLst.Names[TMenuItem(Sender).Tag]]);
   finally
    FreeAndNil(StrLst);
   end;
  finally
   Free;
  end;
end;

procedure TFmAbxMain.BuildRecentFileList(FileName: TFileName);
var
  i, j   : Integer;
  StrLst : TStringList;
begin
 with TIniFile.Create(IniFileName) do
  try
   StrLst := TStringList.Create;
   try
    ReadSectionValues('Recent Files', StrLst);
    EraseSection('Recent Files');
    while StrLst.IndexOf(FileName) >= 0
     do StrLst.Delete(StrLst.IndexOf(FileName));

    StrLst.Insert(0, FileName);

    i := 0;
    j := 0;
    while i < MDIChildCount do
     begin
      assert(MDIChildren[i] is TFmProject);
      if FileExists(TFmProject(MDIChildren[i]).Filename) then
       begin
        inc(j);
        WriteString('Recent Files', 'File ' + IntToStr(j), TFmProject(MDIChildren[i]).Filename);
       end;
      inc(i);
     end;

   finally
    FreeAndNil(StrLst);
   end;
  finally
   Free;
  end;
 BuildRecentFilesMenuItems;
end;

procedure TFmAbxMain.BuildRecentFilesMenuItems;
var
  i      : Integer;
  StrLst : TStringList;
  MI     : TMenuItem;
begin
 with MIRecentFiles do
  begin
   Clear;

   with TIniFile.Create(IniFileName) do
    try
     StrLst := TStringList.Create;
     try
      ReadSectionValues('Recent Files', StrLst);

      for i := 0 to StrLst.Count - 1 do
       begin
        MI := TMenuItem.Create(MIRecentFiles);
        MI.Caption := StrLst.Values[StrLst.Names[i]];
        MI.Tag := i;
        MI.OnClick := MIRecentFilesClick;
        Add(MI);
       end;

     finally
      FreeAndNil(StrLst);
     end;
    finally
     Free;
    end;

   Visible := Count > 0;
  end;
end;

procedure TFmAbxMain.ACFileSaveAsExecute(Sender: TObject);
begin
 if ActiveMDIChild is TFmProject then
  with TSaveDialog.Create(Self) do
   try
    Filter := 'ABX Files (*.abx)|*.abx';
    DefaultExt := 'abx';
    with TIniFile.Create(IniFileName) do
     try
      InitialDir := ReadString('Recent', 'Project Directory', InitialDir);
     finally
      Free;
     end;
    if Execute then
     begin
      TFmProject(ActiveMDIChild).SaveToFile(FileName);
      InitialDir := ExtractFileDir(FileName);
      with TIniFile.Create(IniFileName) do
       try
        WriteString('Recent', 'Project Directory', InitialDir);
       finally
        Free;
       end;
      BuildRecentFileList(FileName);
     end;
   finally
    Free;
   end;
end;

procedure TFmAbxMain.ACFileSaveExecute(Sender: TObject);
begin
 if ActiveMDIChild is TFmProject
  then TFmProject(ActiveMDIChild).Save;
end;

procedure TFmAbxMain.ACHelpAboutExecute(Sender: TObject);
begin
 FmAboutBox.ShowModal;
end;

procedure TFmAbxMain.ACMergeTestResultsExecute(Sender: TObject);
begin
 if MDIChildCount > 0 then
  with TOpenDialog.Create(Self) do
   try
    DefaultExt := '.csv';
    Filter := 'ABX Test Results (*.csv)|*.csv';
    if Execute
     then TFmProject(ActiveMDIChild).MergeTestResults(FileName);
   finally
    Free;
   end;
end;

procedure TFmAbxMain.ACResultTableSetupExecute(Sender: TObject);
begin
 FmResultTableSetup.ShowModal;
end;

procedure TFmAbxMain.ACTestRunExecute(Sender: TObject);
begin
 if MDIChildCount > 0 then
  with TFmTestSetup.Create(ActiveMDIChild) do
   try
    try
     assert(Self.ActiveMDIChild is TFmProject);
     assert(assigned(TFmProject(Self.ActiveMDIChild).AdcA));
     assert(assigned(TFmProject(Self.ActiveMDIChild).AdcB));

     with TFmProject(Self.ActiveMDIChild) do
      if (AdcA.SampleRate <> ASIOHost.SampleRate) or
         (AdcB.SampleRate <> ASIOHost.SampleRate)
       then raise Exception.Create(RCStrSamplerateMismatch);
     ShowModal;
    except
     on E: Exception do MessageDlg('Error: ' + E.Message, mtError, [mbOK], 0);
    end;
   finally
    Free;
   end;
end;

procedure TFmAbxMain.ACAudioSettingsExecute(Sender: TObject);
begin
 FmAudioSettings.ShowModal;
end;

procedure TFmAbxMain.ACCompileExecutableExecute(Sender: TObject);
begin
 if MDIChildCount > 0 then
  with TSaveDialog.Create(Self) do
   try
    DefaultExt := '.exe';
    Filter := 'Executable (*.exe)|*.exe';
    if Execute
     then TFmProject(ActiveMDIChild).CompileStandalone(FileName);
   finally
    Free;
   end;
end;

procedure TFmAbxMain.ACExportExecute(Sender: TObject);
begin
 if MDIChildCount > 0 then
  with TSaveDialog.Create(Self) do
   try
    DefaultExt := 'xls';
    Filter := 'Excel (*.xls)|*.xls|CSV (*.csv)|*.csv|HTML (*.html)|*.html|RTF (*.rtf)|*.rtf|TXT (*.txt)|*.txt';
    if Execute
     then TFmProject(ActiveMDIChild).ExportData(FileName);
   finally
    Free;
   end;
end;

procedure TFmAbxMain.ACFileExitExecute(Sender: TObject);
begin
 Close;
end;

end.
