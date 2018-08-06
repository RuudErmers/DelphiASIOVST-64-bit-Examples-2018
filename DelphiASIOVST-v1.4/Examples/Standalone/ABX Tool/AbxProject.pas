unit AbxProject;

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

{-$DEFINE AddDummyData}
{-$DEFINE ExcelExport}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Classes, Graphics, SysUtils, 
  Forms, Controls, StdCtrls, ComCtrls, Grids, DB, kbmMemTable, 
  kbmMemBinaryStreamFormat, kbmMemCSVStreamFormat, rs, rs_std, 
  {$IFDEF ExcelExport}Dataset2Excel, {$ENDIF} DAV_Types, DAV_AudioData, 
  DAV_AudioFile, DAV_AudioFileWAV, AbxChunks, Menus;

type
  TFmProject = class(TForm)
    AdcA: TAudioDataCollection32;
    AdcB: TAudioDataCollection32;
    Database: TkbmMemTable;
    DatabaseDate: TDateField;
    DatabaseFieldRating: TFloatField;
    DatabaseNameID: TStringField;
    DatabaseTrials: TIntegerField;
    DataSource: TDataSource;
    DBGridPro: TDBGridPro;
    EdTestTitle: TEdit;
    kbmBinaryStreamFormat: TkbmBinaryStreamFormat;
    kbmCSVStreamFormat: TkbmCSVStreamFormat;
    LbMaterialInfo: TLabel;
    LbMaterialTitle: TLabel;
    LiteDesigner: TLiteDesigner;
    MemoTestInfo: TMemo;
    PageControl: TPageControl;
    TSMeasurementDatabase: TTabSheet;
    TSTestInformation: TTabSheet;
    PUDatabase: TPopupMenu;
    MIDeleteRecord: TMenuItem;
    N1: TMenuItem;
    MIAllowEdit: TMenuItem;
    N2: TMenuItem;
    MIFilterDate: TMenuItem;
    N3: TMenuItem;
    MIUndo: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure DBGridProGetRowAttrs(Sender: TObject; RowAttrs: TgpRowAttrs);
    procedure DatabaseAfterPost(DataSet: TDataSet);
    procedure EdTestTitleChange(Sender: TObject);
    procedure MemoTestInfoChange(Sender: TObject);
    procedure MIDeleteRecordClick(Sender: TObject);
    procedure MIAllowEditClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MIFilterDateClick(Sender: TObject);
    procedure DBGridProTitleStateChange(Sender: TObject; Column: TgpColumn);
    procedure DBGridProDblClick(Sender: TObject);
    procedure MIUndoClick(Sender: TObject);
  private
    FAbxContainer     : TAbxContainer;
    FFileName         : TFileName;
    {$IFDEF ExcelExport}
    FDataset2Excel    : TDataset2Excel;
    {$ENDIF}
    FStoreAudioData   : Boolean;
    FSomethingChanged : Boolean;
    procedure SetStoreAudioData(const Value: Boolean);
    procedure RecoverSortState;
  public
    procedure ExportData(FileName: TFileName);
    procedure CompileStandalone(FileName: TFileName);
    procedure MergeTestResults(FileName: TFileName);
    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
    procedure Save;

    property SomethingChanged: Boolean read FSomethingChanged;
    property Filename: TFilename read FFileName;
    property StoreAudioData: Boolean read FStoreAudioData write SetStoreAudioData;
  end;

implementation

uses
  Dialogs, IniFiles, DAV_DLLResources, AbxMain, AbxResultTableSetup;

{$R *.dfm}

resourcestring
  RCStrNoFilenameSpecified = 'No filename specified!';

{ TFmProject }

procedure TFmProject.FormCreate(Sender: TObject);
{$IFDEF AddDummyData}
var
  RcNo   : Integer;
  Trials : Integer;
{$ENDIF}
begin
 // create ABX container (without any data)
 FAbxContainer := TAbxContainer.Create;

 // set empty filename
 FFileName := '';

 // store audio data
 FStoreAudioData := True;

 {$IFDEF AddDummyData}
 // populate database
 if FmAbxMain.MDIChildCount = 1 then
  begin
   EdTestTitle.Text := 'Dummy';
   MemoTestInfo.Lines.Add('This is just some dummy data');

   for RcNo := 0 to 9 do
    begin
     Database.Insert;
     Database.Fields[0].AsDateTime := Now;
     Database.Fields[1].AsString := 'Test subject ' + IntToStr((RcNo div 2) + 1);
     Trials := 5 + 5 * random(3);
     Database.Fields[2].AsFloat := 2 * (random(Trials) / Trials) - 1;
     Database.Fields[3].AsInteger := Trials;
     Database.Post;
    end;
  end;
 FSomethingChanged := True;
 {$ENDIF}

 {$IFDEF ExcelExport}
 FDataset2Excel := TDataset2Excel.Create(Self);
 FDataset2Excel.Dataset := Database;

 // eventually enable the excel export
 FmAbxMain.ACExport.Enabled := FmAbxMain.MDIChildCount >= 1;
 {$ENDIF}

 // eventually enable 'save as' action
 FmAbxMain.ACFileSaveAs.Enabled := FmAbxMain.ActiveMDIChild = Self;
 FmAbxMain.ACCompileExecutable.Enabled := FmAbxMain.ActiveMDIChild = Self;
 FmAbxMain.ACExport.Enabled := FmAbxMain.ActiveMDIChild = Self;
end;

procedure TFmProject.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FAbxContainer);
end;

procedure TFmProject.FormShow(Sender: TObject);
begin
 EdTestTitle.Width := ClientWidth - (EdTestTitle.Left + 8);
 MemoTestInfo.Width := ClientWidth - (MemoTestInfo.Left + 8);
 MemoTestInfo.Height := TSTestInformation.Height - (MemoTestInfo.Top);
end;

procedure TFmProject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TFmProject.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 if SomethingChanged then
  case MessageDlg('Something has changed' + #13#10 +
                  'Save file?', mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
   mrYes : if FFileName <> ''
            then Save else
             begin
              with TSaveDialog.Create(Self) do
               try
                Filter := 'ABX Files (*.abx)|*.abx';
                DefaultExt := 'abx';
                with TIniFile.Create(FmAbxMain.IniFileName) do
                 try
                  InitialDir := ReadString('Recent', 'Project Directory', InitialDir);
                 finally
                  Free;
                 end;
                if Execute then
                 begin
                  SaveToFile(FileName);
                  InitialDir := ExtractFileDir(FileName);
                  with TIniFile.Create(FmAbxMain.IniFileName) do
                   try
                    WriteString('Recent', 'Project Directory', InitialDir);
                   finally
                    Free;
                   end;
                  FmAbxMain.BuildRecentFileList(FileName);
                 end;
               finally
                Free;
               end;

             end;
   mrCancel : CanClose := False;          
  end;
end;

procedure TFmProject.FormActivate(Sender: TObject);
begin
 with FmAbxMain do
  begin
   ACTestRun.Enabled := True;
   ACFileSaveAs.Enabled := True;
   ACFileSave.Enabled := FFileName <> '';
   ACExport.Enabled := True;
   ACCompileExecutable.Enabled := True;
   ACMergeTestResults.Enabled := False;
  end;
end;

procedure TFmProject.FormDeactivate(Sender: TObject);
begin
 with FmAbxMain do
  begin
   ACTestRun.Enabled := False;
   ACFileSave.Enabled := False;
   ACFileSaveAs.Enabled := False;
   ACExport.Enabled := False;
   ACCompileExecutable.Enabled := False;
   ACMergeTestResults.Enabled := False;
  end;
end;

procedure TFmProject.DatabaseAfterPost(DataSet: TDataSet);
begin
 FSomethingChanged := True;
end;

procedure TFmProject.RecoverSortState;
var
  b : Byte;
begin
 with DBGridPro do
  for b := 0 to Columns.Count - 1 do
   if Columns[b].TitleState <> tsSortedNone then
    begin
     Database.SortFields := Columns[b].FieldName;
     if Columns[b].TitleState = tsSortedUp
      then Database.SortOptions := [mtcoDescending]
      else Database.SortOptions := [];
     Database.SortDefault;
     Break;
    end;
end;

procedure TFmProject.DBGridProDblClick(Sender: TObject);
var
  i, o, r : Integer;
  b       : Boolean;
begin
 Database.DisableControls;
 try
  r := Database.RecNo;
  b := DBGridPro.AutoColumnFit;
  o := 7;
  with TControlCanvas.Create do
   try
    Font := DBGridPro.Font;
    Control := Self;
    with DBGridPro do
     try
      AutoColumnFit := False;
      Database.First;
      for i := 0 to Columns.VisibleCount - 1 do
       if Columns.Visible[i].Field <> nil
        then Columns.Visible[i].Width := o + TextWidth(Columns.Visible[i].Field.DisplayLabel);
      while not Database.Eof do
       with Columns do
        begin
         for i := 0 to VisibleCount - 1 do
          if (Visible[i].Field <> nil) then
           if o + TextWidth(Visible[i].Field.DisplayText) > Visible[i].Width
            then Visible[i].Width := o + TextWidth(Visible[i].Field.DisplayText);
         Database.Next;
        end;
    finally
     DBGridPro.AutoColumnFit := b;
     Database.RecNo := r;
     Database.EnableControls;
    end;
  finally
   Free;
  end;
 finally
  Database.EnableControls;
 end;

 RecoverSortState;
end;

procedure TFmProject.DBGridProGetRowAttrs(Sender: TObject;
  RowAttrs: TgpRowAttrs);
begin
 inherited;
 with RowAttrs do
  begin
   if FmResultTableSetup.RBColorEverySecond.Checked then
    if (StripIndex = 1)
     then BgColor := clCream
     else BgColor := clNone
   else if FmResultTableSetup.RBColorAboveThreshold.Checked then
    if DataSet['Rating'] > FmResultTableSetup.RatingThreshold
     then BgColor := $00F8FFF8
     else BgColor := clNone
   else if FmResultTableSetup.RBColorAboveThreshold.Checked then
    if DataSet['Rating'] < FmResultTableSetup.RatingThreshold
     then BgColor := $00F8F8FF
     else BgColor := clNone;

//   if Selected then BgColor := clHighlight;
  end;
end;

procedure TFmProject.DBGridProTitleStateChange(Sender: TObject;
  Column: TgpColumn);
begin
 Database.SortFields := Column.FieldName;
 if Column.TitleState = tsSortedUp
  then Database.SortOptions := [mtcoDescending]
  else Database.SortOptions := [];
 Database.SortDefault;
end;

procedure TFmProject.EdTestTitleChange(Sender: TObject);
begin
 FSomethingChanged := True;
end;

procedure TFmProject.ExportData(FileName: TFileName);
begin
 {$IFDEF ExcelExport}
 FDataset2Excel.ExportDataset;
 FDataset2Excel.SaveAs(FileName);
 {$ENDIF}
end;

procedure TFmProject.LoadFromFile(FileName: TFileName);
begin
 // set filename as internal filename
 FFileName := FileName;
 FmAbxMain.ACFileSave.Enabled := True;

 // actually load file
 FAbxContainer.LoadFromFile(FFileName);

 // load database from stream
 Database.LoadFromStream(FAbxContainer.DatabaseStream);

 // assign title from ABX container
 EdTestTitle.Text := FAbxContainer.Title;

 // assign notes from ABX container
 MemoTestInfo.Lines.Text := FAbxContainer.Notes;

 // eventually load audio stream
 if assigned(FAbxContainer.AudioStreamA)
  then AdcA.LoadFromStream(FAbxContainer.AudioStreamA);

 // eventually load audio stream
 if assigned(FAbxContainer.AudioStreamB)
  then AdcB.LoadFromStream(FAbxContainer.AudioStreamB);

 FSomethingChanged := False;
end;

procedure TFmProject.MemoTestInfoChange(Sender: TObject);
begin
 FSomethingChanged := True;
end;

procedure TFmProject.MergeTestResults(FileName: TFileName);
begin
 // not yet implemented
end;

procedure TFmProject.MIAllowEditClick(Sender: TObject);
begin
 DBGridPro.ReadOnly := MIAllowEdit.Checked;
 MIAllowEdit.Checked := not MIAllowEdit.Checked;
end;

procedure TFmProject.MIDeleteRecordClick(Sender: TObject);
begin
 Database.Delete;
end;

procedure TFmProject.MIFilterDateClick(Sender: TObject);
begin
(*
 MMD_FilterName := Database['Date'];
 Database.Filtered := False;
 MIFilterDate.Checked := not MIFilterDate.Checked;
 if MIFilterDate.Checked
  then MMDFilterType := MMDFilterType + [dfName]
  else MMDFilterType := MMDFilterType - [dfName];
 CheckUnfiltered;
*)
end;

procedure TFmProject.MIUndoClick(Sender: TObject);
begin
 with Database do
  if (UpdateStatus = usModified)
   then try Undo; except end;
end;

procedure TFmProject.SaveToFile(FileName: TFileName);
begin
 // set filename as internal filename
 FFileName := FileName;
 FmAbxMain.ACFileSave.Enabled := True;

 // eventually enable save action
 FmAbxMain.ACFileSave.Enabled := FmAbxMain.ActiveMDIChild = Self;

 // actually save file
 Save;
end;

procedure TFmProject.SetStoreAudioData(const Value: Boolean);
begin
 if FStoreAudioData <> Value then
  begin
   FStoreAudioData := True; //Value;
  end;
end;

procedure TFmProject.Save;
begin
 if FFileName = ''
  then raise Exception.Create(RCStrNoFilenameSpecified);

 // assign title to ABX container
 FAbxContainer.Title := EdTestTitle.Text;

 // assign notes to ABX container
 FAbxContainer.Notes := MemoTestInfo.Lines.Text;

 // store database to stream
 Database.SaveToStream(FAbxContainer.DatabaseStream);

 if StoreAudioData then
  begin
   // eventually add audio stream
   FAbxContainer.AddAudioStreamChunk;

   // eventually save audio stream
   if assigned(FAbxContainer.AudioStreamA) and
      (AdcA.ChannelCount > 0) and (AdcA.SampleFrames > 0)
    then AdcA.SaveToStream(FAbxContainer.AudioStreamA);

   // eventually save audio stream
   if assigned(FAbxContainer.AudioStreamB) and
      (AdcB.ChannelCount > 0) and (AdcB.SampleFrames > 0)
    then AdcB.SaveToStream(FAbxContainer.AudioStreamB);
  end;

 // actually save file
 FAbxContainer.SaveToFile(FFileName);

 FSomethingChanged := False;
end;

procedure TFmProject.CompileStandalone(FileName: TFileName);
var
  Standalone : TPEResourceModule;
  RS         : TResourceStream;
  RD         : TResourceDetails;
  MS         : TMemoryStream;
begin
 Standalone := TPEResourceModule.Create;
 try
  RS := TResourceStream.Create(HInstance, 'Standalone', 'EXE');
  try
   Standalone.LoadFromStream(RS);
  finally
   FreeAndNil(RS);
  end;

  MS := TMemoryStream.Create;
  with MS do
   try
    FAbxContainer.SaveToStream(MS);
    RD := TResourceDetails.CreateResourceDetails(Standalone, 0, 'ABX', 'ABX', Size, Memory);
    Standalone.AddResource(RD);
   finally
    FreeAndNil(MS);
   end;

  Standalone.SortResources;
  Standalone.SaveToFile(FileName);
 finally
  FreeAndNil(Standalone);
 end;
end;

end.
