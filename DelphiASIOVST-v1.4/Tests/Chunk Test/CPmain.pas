unit CPmain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExtCtrls, ComCtrls, DAV_Common, DAV_Types, DAV_ChunkClasses;

type
  TFmChunkParser = class(TForm)
    MainMenu: TMainMenu;
    MIFile: TMenuItem;
    MIOpen: TMenuItem;
    N1: TMenuItem;
    MIExit: TMenuItem;
    OD: TOpenDialog;
    TreeView: TTreeView;
    Splitter: TSplitter;
    Memo: TMemo;
    procedure MIOpenClick(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure MIExitClick(Sender: TObject);
  private
    procedure LoadFile(FileName: TFileName);
  public
    FChunkContainer : TUnknownChunkContainer;
  end;

var
  FmChunkParser: TFmChunkParser;

implementation

{$R *.dfm}

function ChunknameValidASCII(ChunkName: TChunkName): Boolean;
var
  CharPos : Byte;
begin
 Result := True;
 for CharPos := 0 to 3 do
  case ChunkName[CharPos] of
   #0, #$21..#$7F : ;
   else
    begin
     Result := False;
     Exit;
    end;
  end;
end;

function ValidASCIIString(Value: string): Boolean;
var
  CharPos : Byte;
begin
 Result := True;
 for CharPos := 1 to Length(Value) do
  case Value[CharPos] of
   #$20..#$7F : ;
   else
    begin
     Result := False;
     if Value[CharPos] = #12 then Exit;
    end;
  end;
end;

procedure TFmChunkParser.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmChunkParser.MIOpenClick(Sender: TObject);
begin
 if OD.Execute
  then LoadFile(OD.Filename);
end;

procedure TFmChunkParser.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  Str : string;
begin
 if TObject(Node.Data) is TUnknownChunk then
  with TUnknownChunk(Node.Data) do
   begin
    Memo.Clear;
    Memo.Lines.Add('chunk name: ' + ChunkName);
    Memo.Lines.Add('chunk size: ' + IntToStr(DataStream.Size));
    DataStream.Position := 0;
    SetLength(Str, DataStream.Size);
    DataStream.Read(Str[1], DataStream.Size);
    if ValidASCIIString(Str)
     then Memo.Lines.Add('chunk data as string: ' + Str);
   end;
end;

procedure TFmChunkParser.LoadFile(FileName: TFileName);
var
  ChunkName    : TChunkName;
  ChunkSize    : Cardinal;
  UnknwonChunk : TUnknownChunk;
  FS           : TFileStream;

  procedure MirrorChunkToTreeview(Chunk: TUnknownChunkContainer; TreeNode: TTreeNode);
  var
    i  : Integer;
    nt : TTreeNode;
  begin
   for i := 0 to Chunk.Count - 1 do
    begin
     nt := TTreeView(TreeNode.TreeView).Items.AddChildObject(TreeNode, Chunk.SubChunk[i].ChunkName, Chunk.SubChunk[i]);
     if Chunk.SubChunk[i] is TUnknownChunkContainer
      then MirrorChunkToTreeview(TUnknownChunkContainer(Chunk.SubChunk[i]), nt);
    end;
  end;

begin
 FS := TFileStream.Create(Filename, fmOpenRead);
 with FS do
  try
   if Size < 8
    then raise Exception.CreateFmt('Filesize too small (%d)', [Size]);

   // read chunk name
   Read(ChunkName, 4);

   if (ChunkName = #$89'PNG') then
    begin
     // read chunk name
     Read(ChunkName, 4);

     if ChunkName <> #$0D#$0A#$1A#$0A
      then raise Exception.Create('Not a valid PNG file');

     ChunkName := #$89'PNG';

     // clear old data
     TreeView.Items.Clear;
     FreeAndNil(FChunkContainer);

     // actually load chunk file
     Position := 0;
     FChunkContainer := TPNGChunkContainer.Create;
     FChunkContainer.ChunkFlags := FChunkContainer.ChunkFlags +
       [cfReversedByteOrder, cfSizeFirst];
     FChunkContainer.LoadFromStream(FS);

     TreeView.Items.AddObject(TTreeNode.Create(TreeView.Items), ChunkName, FChunkContainer);
     MirrorChunkToTreeview(FChunkContainer, TreeView.Items[0]);
    end else
   if (ChunkName = 'MThd') then
    begin
     // read chunk size
     Read(ChunkSize, 4);
     Flip32(ChunkSize);
     FS.Position := FS.Position + ChunkSize;

     // clear old data
     TreeView.Items.Clear;
     FreeAndNil(FChunkContainer);

     while FS.Position < FS.Size do
      begin
       UnknwonChunk := TUnknownChunk.Create;
       UnknwonChunk.ChunkFlags := [cfReversedByteOrder];
       UnknwonChunk.LoadFromStream(FS);

       TreeView.Items.AddObject(TTreeNode.Create(TreeView.Items),
         UnknwonChunk.ChunkName, UnknwonChunk);
      end;
    end
   else
    begin
     // read chunk size
     Read(ChunkSize, 4);

     if (ChunkName = 'FORM')
      then Flip32(ChunkSize);

     if (ChunkName[1] = 'W') and (ChunkName[2] = 'S')
      then ChunkSize := ChunkSize - 8;

     // generic header
     if ChunkSize = $FFFFFFFF
      then raise Exception.Create('Not supported yet');
     if ChunkSize + 8 <> Size then
      if ChunknameValidASCII(ChunkName)
       then raise Exception.CreateFmt('Invalid chunk size found (%d)', [Chunksize])
       else raise Exception.CreateFmt('Not a chunk format or invalid chunk size found (%d)', [Chunksize]);

     // clear old data
     TreeView.Items.Clear;
     FreeAndNil(FChunkContainer);

     // actually load chunk file
     FChunkContainer := TUnknownChunkContainer.Create;
     if ChunkName = 'FORM' then
      with FChunkContainer
       do ChunkFlags := ChunkFlags + [cfReversedByteOrder, cfPadSize];
     Position := 0;
     if (ChunkName[1] = 'W') and (ChunkName[2] = 'S')
      then FChunkContainer.ChunkFlags := FChunkContainer.ChunkFlags + [cfIncludeChunkInSize]; 
     FChunkContainer.LoadFromStream(FS);

     TreeView.Items.AddObject(TTreeNode.Create(TreeView.Items), ChunkName, FChunkContainer);
     MirrorChunkToTreeview(FChunkContainer, TreeView.Items[0]);
    end;
  finally
   FreeAndNil(FS);
  end;
end;

end.
