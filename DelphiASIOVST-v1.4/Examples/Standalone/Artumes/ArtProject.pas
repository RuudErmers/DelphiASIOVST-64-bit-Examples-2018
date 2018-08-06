unit ArtProject;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

{$I Artumes.inc}

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Classes, SysUtils, Graphics, 
  Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus, Dialogs, ArtItemSource, 
  ArtItemAnalysis, ArtItemDestination, DAV_AudioData;

type
  TFmProject = class(TForm)
    MiDestinationCSV: TMenuItem;
    MiDestinationExcel: TMenuItem;
    MiDestinationWAV: TMenuItem;
    MiOctave: TMenuItem;
    MiSourceAddFile: TMenuItem;
    MiSourceDelete: TMenuItem;
    MiThirdOctave: TMenuItem;
    OpenDialog: TOpenDialog;
    PuAnalysis: TPopupMenu;
    PuDestination: TPopupMenu;
    PuSource: TPopupMenu;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    TvAnalysis: TTreeView;
    TvDestination: TTreeView;
    TvFilter: TTreeView;
    TvSource: TTreeView;
    TvStatistic: TTreeView;
    N1: TMenuItem;
    N2: TMenuItem;
    MiSourceAddFolder: TMenuItem;
    N3: TMenuItem;
    MiAnalysisDelete: TMenuItem;
    MiDestinationDelete: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    MiAnalysisAddFolder: TMenuItem;
    N6: TMenuItem;
    MiDestinationAddFolder: TMenuItem;
    MiSourceRename: TMenuItem;
    MiAnalysisRename: TMenuItem;
    MiDestinationRename: TMenuItem;
    N7: TMenuItem;
    MiSourceProperties: TMenuItem;
    N8: TMenuItem;
    MiAnalysisProperties: TMenuItem;
    N9: TMenuItem;
    MiDestinationProperties: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure MiSourceAddFileClick(Sender: TObject);
    procedure MiThirdOctaveClick(Sender: TObject);
    procedure MiOctaveClick(Sender: TObject);
    procedure MiDestinationExcelClick(Sender: TObject);
    procedure MiDestinationCSVClick(Sender: TObject);
    procedure MiDestinationWAVClick(Sender: TObject);
    procedure MiSourceDeleteClick(Sender: TObject);
    procedure MiAnalysisDeleteClick(Sender: TObject);
    procedure MiDestinationDeleteClick(Sender: TObject);
    procedure MiSourceAddFolderClick(Sender: TObject);
    procedure MiAnalysisAddFolderClick(Sender: TObject);
    procedure MiDestinationAddFolderClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure MiSourceRenameClick(Sender: TObject);
    procedure MiAnalysisRenameClick(Sender: TObject);
    procedure MiDestinationRenameClick(Sender: TObject);
    procedure MiAnalysisPropertiesClick(Sender: TObject);
    procedure TvAnalysisChange(Sender: TObject; Node: TTreeNode);
  private
    FWidthRatios : array [0..4] of Single;
    procedure ValidateNewWidthRatios;
    procedure AddSourceFile(Filename: TFilename);
    procedure AddAnalysisOctave;
    procedure AddAnalysisThirdOctave;
    procedure AddDestinationCSV;
    procedure AddDestinationWAV;
    procedure SelectAnalysisProperties(Node: TTreeNode);
    procedure ProcessAudioChannel(AudioChannel: TAudioChannel32);
    {$IFDEF Excel}
    procedure AddDestinationExcel;
    {$ENDIF}
  protected
    procedure CustomAlignPosition(Control: TControl; var NewLeft, NewTop,
      NewWidth, NewHeight: Integer; var AlignRect: TRect;
      AlignInfo: TAlignInfo); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromFile(const FileName: TFileName);
    procedure Calculate;
  end;

implementation

uses
  Math, DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF, DAV_AudioFileAU,
  ArtMain, ArtPropertiesAnalysis;

{$R *.dfm}


{ TFmProject }

constructor TFmProject.Create(AOwner: TComponent);
begin
 FWidthRatios[0] := 0.2;
 FWidthRatios[1] := 0.2;
 FWidthRatios[2] := 0.2;
 FWidthRatios[3] := 0.2;
 FWidthRatios[4] := 0.2;
 inherited;
end;

procedure TFmProject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 Action := caFree;
end;

procedure TFmProject.FormActivate(Sender: TObject);
begin
 FmArtumes.AcCalculate.Enabled := True;
end;

procedure TFmProject.FormDeactivate(Sender: TObject);
begin
 FmArtumes.AcCalculate.Enabled := False;
end;

procedure TFmProject.Calculate;
var
  Source    : Integer;
  Channel   : Integer;
  AudioData : TAudioDataCollection32;
begin
 for Source := 0 to TvSource.Items.Count - 1 do
  begin
   if TObject(TvSource.Items[Source].Data) is TSourceFile then
    with TSourceFile(TvSource.Items[Source].Data) do
     begin
      AudioData := TAudioDataCollection32.Create(Self);
      AudioData.LoadFromFile(FileName);
      for Channel := 0 to AudioData.ChannelCount - 1
       do ProcessAudioChannel(TAudioChannel32(AudioData.Channels[Channel]));
     end;
  end;
end;

procedure TFmProject.ProcessAudioChannel(AudioChannel: TAudioChannel32);
var
  Analysis : Integer;
begin
 with AudioChannel do
  for Analysis := 0 to TvAnalysis.Items.Count - 1 do
   begin


   end;
end;

procedure TFmProject.LoadFromFile(const FileName: TFileName);
begin
 raise Exception.Create('not implemented yet');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmProject.MiAnalysisDeleteClick(Sender: TObject);
begin
 with TvAnalysis do
  if (Selected <> nil) and (Selected <> Items[0]) then
   begin
    // delete selected item object
    if Selected.Data <> nil
     then TObject(Selected.Data).Free;

    // delete selected item
    Items.Delete(Selected);
   end;
end;

procedure TFmProject.MiDestinationDeleteClick(Sender: TObject);
begin
 with TvDestination do
  if (Selected <> nil) and (Selected <> Items[0]) then
   begin
    // delete selected item object
    if Selected.Data <> nil
     then TObject(Selected.Data).Free;

    // delete selected item
    Items.Delete(Selected);
   end;
end;

procedure TFmProject.MiSourceDeleteClick(Sender: TObject);
begin
 with TvSource do
  if (Selected <> nil) and (Selected <> Items[0]) then
   begin
    // delete selected item object
    if Selected.Data <> nil
     then TObject(Selected.Data).Free;

    // delete selected item
    Items.Delete(Selected);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmProject.MiAnalysisPropertiesClick(Sender: TObject);
begin
 with TvAnalysis do
  if (Selected <> nil) and (Selected.Data <> nil) then
   begin
    // set parent
    FmAnalysisProperties.Parent := FmArtumes;

    FmAnalysisProperties.Show;
    SelectAnalysisProperties(Selected);
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmProject.MiSourceRenameClick(Sender: TObject);
begin
 with TvSource do
  begin
   if (Selected <> nil) and (Selected <> Items[0]) then
    if Selected.EditText
     then // todo
  end;
end;

procedure TFmProject.MiAnalysisRenameClick(Sender: TObject);
begin
 with TvAnalysis do
  begin
   if (Selected <> nil) and (Selected <> Items[0]) then
    if Selected.EditText
     then // todo
  end;
end;

procedure TFmProject.MiDestinationRenameClick(Sender: TObject);
begin
 with TvDestination do
  begin
   if (Selected <> nil) and (Selected <> Items[0]) then
    if Selected.EditText
     then // todo
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmProject.MiAnalysisAddFolderClick(Sender: TObject);
var
  Node : TTreeNode;
begin
 with TvAnalysis do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'Folder')
    else Node := Items.AddChild(Items[0], 'Folder');

   Node.Parent.Expand(True);
  end;
end;

procedure TFmProject.MiSourceAddFolderClick(Sender: TObject);
var
  Node : TTreeNode;
begin
 with TvSource do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'Folder')
    else Node := Items.AddChild(Items[0], 'Folder');

   Node.Parent.Expand(True);
  end;
end;

procedure TFmProject.MiDestinationAddFolderClick(Sender: TObject);
var
  Node : TTreeNode;
begin
 with TvDestination do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'Folder')
    else Node := Items.AddChild(Items[0], 'Folder');

   Node.Parent.Expand(True);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmProject.MiSourceAddFileClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute
   then AddSourceFile(OpenDialog.FileName)
end;

procedure TFmProject.MiDestinationCSVClick(Sender: TObject);
begin
 AddDestinationCSV;
end;

procedure TFmProject.MiDestinationExcelClick(Sender: TObject);
begin
 {$IFDEF Excel}
 AddDestinationExcel;
 {$ELSE}
 MessageDlg('Excel export is not supported in the open source edition',
   mtError, [mbOK], 0);
 {$ENDIF}
end;

procedure TFmProject.MiDestinationWAVClick(Sender: TObject);
begin
 AddDestinationWAV;
end;

procedure TFmProject.MiOctaveClick(Sender: TObject);
begin
 AddAnalysisOctave;
end;

procedure TFmProject.MiThirdOctaveClick(Sender: TObject);
begin
 AddAnalysisThirdOctave;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TFmProject.AddSourceFile(Filename: TFilename);
var
  Node       : TTreeNode;
  SF         : TSourceFile;
  AudioClass : TAudioFileClass;
begin
 with TvSource do
  begin
   AudioClass := FileNameToFormat(Filename);

   if Assigned(AudioClass) and AudioClass.CanLoad(Filename) then
    begin
     if Selected <> nil
      then Node := Items.AddChild(Selected, ExtractFileName(Filename))
      else Node := Items.AddChild(Items[0], ExtractFileName(Filename));

     Node.Parent.Expand(True);

     // create source file
     SF := TSourceFile.Create;
     SF.FileName := Filename;

     Node.Data := SF;
    end;
  end;
end;

procedure TFmProject.AddAnalysisOctave;
var
  Node : TTreeNode;
  Ana  : TCustomAnalysis;
begin
 with TvAnalysis do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'Octave')
    else Node := Items.AddChild(Items[0], 'Octave');
   Node.Parent.Expand(True);

   // create analysis
   Ana := TAnalysisOctave.Create;

   Node.Data := Ana;
  end;
end;

procedure TFmProject.AddAnalysisThirdOctave;
var
  Node : TTreeNode;
  Ana  : TCustomAnalysis;
begin
 with TvAnalysis do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'Third Octave')
    else Node := Items.AddChild(Items[0], 'Third Octave');
   Node.Parent.Expand(True);

   // create analysis
   Ana := TAnalysisThirdOctave.Create;

   Node.Data := Ana;
  end;
end;

procedure TFmProject.AddDestinationCSV;
var
  Node : TTreeNode;
  Dest : TCustomDestination;
begin
 with TvDestination do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'CSV')
    else Node := Items.AddChild(Items[0], 'CSV');
   Node.Parent.Expand(True);

   // create destination
   Dest := TDestinationCSV.Create;

   Node.Data := Dest;
  end;
end;

procedure TFmProject.AddDestinationWAV;
var
  Node : TTreeNode;
  Dest : TCustomDestination;
begin
 with TvDestination do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'WAV')
    else Node := Items.AddChild(Items[0], 'WAV');
   Node.Parent.Expand(True);

   // create destination
   Dest := TDestinationWAV.Create;

   Node.Data := Dest;
  end;
end;

{$IFDEF Excel}
procedure TFmProject.AddDestinationExcel;
var
  Node : TTreeNode;
  Dest : TCustomDestination;
begin
 with TvDestination do
  begin
   if Selected <> nil
    then Node := Items.AddChild(Selected, 'Excel')
    else Node := Items.AddChild(Items[0], 'Excel');
   Node.Parent.Expand(True);

   // create destination
   Dest := TDestinationExcel.Create;

   Node.Data := Dest;
  end;
end;
{$ENDIF}

procedure TFmProject.SplitterCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
var
  Sum : Single;
begin
 if Sender is TSplitter then
  with TSplitter(Sender) do
   begin
    Sum := FWidthRatios[Tag] + FWidthRatios[Tag + 1];
    FWidthRatios[Tag    ] := NewSize / Self.ClientWidth;
    FWidthRatios[Tag + 1] := Max(0.05, Sum - FWidthRatios[Tag]);
    ValidateNewWidthRatios;
   end;
end;

procedure TFmProject.TvAnalysisChange(Sender: TObject; Node: TTreeNode);
begin
 if FmAnalysisProperties.Visible then
  begin
   SelectAnalysisProperties(Node);
  end;
end;

procedure TFmProject.SelectAnalysisProperties(Node: TTreeNode);
begin
 if Assigned(Node) and Assigned(Node.Data) and
   (TObject(Node.Data) is TCustomAnalysis)
  then FmAnalysisProperties.Analysis := TCustomAnalysis(Node.Data)
  else FmAnalysisProperties.Analysis := nil;
end;

procedure TFmProject.ValidateNewWidthRatios;
var
  InvSum : Single;
begin
 InvSum := 1 / (FWidthRatios[0] + FWidthRatios[1] + FWidthRatios[2] +
   FWidthRatios[3] + FWidthRatios[4]);
 FWidthRatios[0] := FWidthRatios[0] * InvSum;
 FWidthRatios[1] := FWidthRatios[1] * InvSum;
 FWidthRatios[2] := FWidthRatios[2] * InvSum;
 FWidthRatios[3] := FWidthRatios[3] * InvSum;
 FWidthRatios[4] := FWidthRatios[4] * InvSum;
end;

procedure TFmProject.CustomAlignPosition(Control: TControl; var NewLeft,
  NewTop, NewWidth, NewHeight: Integer; var AlignRect: TRect;
  AlignInfo: TAlignInfo);
begin
 if Control = TvSource then
  begin
   NewTop := 0;
   NewLeft := 0;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[0] * ClientWidth);

   Splitter1.BoundsRect := Rect(NewWidth, 0, NewWidth +
     Splitter1.Width, ClientHeight);
  end else
 if Control = TvFilter then
  begin
   NewTop := 0;
   NewLeft := TvSource.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[1] * ClientWidth);

   Splitter2.BoundsRect := Rect(NewLeft + NewWidth, 0,
     NewLeft + NewWidth + Splitter2.Width, ClientHeight);
  end else
 if Control = TvAnalysis then
  begin
   NewTop := 0;
   NewLeft := TvFilter.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[2] * ClientWidth);

   Splitter3.BoundsRect := Rect(NewLeft + NewWidth, 0,
     NewLeft + NewWidth + Splitter3.Width, ClientHeight);
  end else
 if Control = TvStatistic then
  begin
   NewTop := 0;
   NewLeft := TvAnalysis.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[3] * ClientWidth);

   Splitter4.BoundsRect := Rect(NewLeft + NewWidth, 0,
     NewLeft + NewWidth + Splitter4.Width, ClientHeight);
  end else
 if Control = TvDestination then
  begin
   NewTop := 0;
   NewLeft := TvStatistic.BoundsRect.Right + 3;
   NewHeight := ClientHeight;
   NewWidth := Round(FWidthRatios[4] * ClientWidth);
  end;
end;

end.
