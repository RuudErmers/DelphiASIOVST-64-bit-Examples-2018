unit AbxChunks;

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
  Classes, SysUtils, DAV_Types, DAV_ChunkClasses;

type
  TAbxTitle = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TAbxNotes = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TAbxMemoryStream = class(TCustomMemoryStreamChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TAbxAudioStreams = class(TChunkContainer)
  protected
    FAudioStreams : array [0..1] of TAbxMemoryStream;
  public
    class function GetClassChunkName: TChunkName; override;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;

    property AudioStreamA: TAbxMemoryStream read FAudioStreams[0];
    property AudioStreamB: TAbxMemoryStream read FAudioStreams[1];
  end;

  TAbxDatabaseStream = class(TCustomMemoryStreamChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TAbxContainer = class(TChunkContainer)
  private
    function GetNotes: string;
    function GetTitle: string;
    function GetAudioStreamA: TMemoryStream;
    function GetAudioStreamB: TMemoryStream;
    function GetDatabaseStream: TMemoryStream;
    procedure SetDatabaseStream(const Value: TMemoryStream);
    procedure SetNotes(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetAudioStreamA(const Value: TMemoryStream);
    procedure SetAudioStreamB(const Value: TMemoryStream);
  protected
    FDatabaseStreamChunk : TAbxDatabaseStream;
    FTitleChunk          : TAbxTitle;
    FNotesChunk          : TAbxNotes;
    FAudioStreamChunk    : TAbxAudioStreams;
    procedure Clear; virtual;
    procedure ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream: TStream); override;
  public
    class function GetClassChunkName: TChunkName; override;
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure AddAudioStreamChunk;

    property Title: string read GetTitle write SetTitle;
    property Notes: string read GetNotes write SetNotes;
    property DatabaseStream: TMemoryStream read GetDatabaseStream write SetDatabaseStream;
    property AudioStreamA: TMemoryStream read GetAudioStreamA write SetAudioStreamA;
    property AudioStreamB: TMemoryStream read GetAudioStreamB write SetAudioStreamB;
  end;

implementation

resourcestring
  RCStrChunkAlreadyExists = 'Chunk already exists';

{ TAbxTitle }

class function TAbxTitle.GetClassChunkName: TChunkName;
begin
 result := 'ABXT';
end;

{ TAbxNotes }

class function TAbxNotes.GetClassChunkName: TChunkName;
begin
 result := 'ABXN';
end;

{ TAbxDatabaseStream }

class function TAbxDatabaseStream.GetClassChunkName: TChunkName;
begin
 result := 'ABXD';
end;

{ TAbxMemoryStream }

class function TAbxMemoryStream.GetClassChunkName: TChunkName;
begin
 result := 'ABXM';
end;

{ TAbxAudioStreams }

constructor TAbxAudioStreams.Create;
begin
 inherited;
 RegisterChunkClass(TAbxMemoryStream);

 // create audio streams
 FAudioStreams[0] := TAbxMemoryStream.Create;
 FAudioStreams[1] := TAbxMemoryStream.Create;

 // add audiostreams
 FChunkList.Add(FAudioStreams[0]);
 FChunkList.Add(FAudioStreams[1]);

 // evaluate chunk size
 FChunkSize := GetChunkSize;
end;

class function TAbxAudioStreams.GetClassChunkName: TChunkName;
begin
 result := 'ABXA';
end;

procedure TAbxAudioStreams.LoadFromStream(Stream: TStream);
begin
 inherited;

 if FChunkList.Count >= 4 then
  begin
   // make sure the chunks are memory streams
   assert(FChunkList[2] is TAbxMemoryStream);
   assert(FChunkList[3] is TAbxMemoryStream);

   // assign new chunks
   FAudioStreams[0] := TAbxMemoryStream(FChunkList[2]);
   FAudioStreams[1] := TAbxMemoryStream(FChunkList[3]);

   // delete old chunks
   FChunkList.Delete(0);
   FChunkList.Delete(0);
  end;
end;

{ TAbxContainer }

constructor TAbxContainer.Create;
begin
 inherited;
 RegisterChunkClasses([TAbxTitle, TAbxNotes, TAbxDatabaseStream, TAbxAudioStreams]);

 // create and add database stream sub-chunk
 FDatabaseStreamChunk := TAbxDatabaseStream.Create;
 FDatabaseStreamChunk.ChunkFlags := ChunkFlags;
 AddChunk(FDatabaseStreamChunk);

 // evaluate chunk size
 FChunkSize := GetChunkSize;
end;

function TAbxContainer.GetAudioStreamA: TMemoryStream;
begin
 if assigned(FAudioStreamChunk) and assigned(FAudioStreamChunk.AudioStreamA)
  then result := FAudioStreamChunk.AudioStreamA.MemoryStream
  else result := nil;
end;

function TAbxContainer.GetAudioStreamB: TMemoryStream;
begin
 if assigned(FAudioStreamChunk) and assigned(FAudioStreamChunk.AudioStreamB)
  then result := FAudioStreamChunk.AudioStreamB.MemoryStream
  else result := nil;
end;

class function TAbxContainer.GetClassChunkName: TChunkName;
begin
 result := 'ABXC';
end;

function TAbxContainer.GetDatabaseStream: TMemoryStream;
begin
 result := FDatabaseStreamChunk.MemoryStream;
end;

function TAbxContainer.GetNotes: string;
begin
 if Assigned(FNotesChunk)
  then result := FNotesChunk.Text
  else result := '';
end;

function TAbxContainer.GetTitle: string;
begin
 if Assigned(FTitleChunk)
  then result := FTitleChunk.Text
  else result := '';
end;

procedure TAbxContainer.AddAudioStreamChunk;
begin
 if not Assigned(FAudioStreamChunk) then
  begin
   FAudioStreamChunk := TAbxAudioStreams.Create;
   AddChunk(FAudioStreamChunk);
  end;
end;

procedure TAbxContainer.Clear;
var
  i : Integer;
begin
 i := 0;
 while i < FChunkList.Count do
  if (FChunkList[i] = FDatabaseStreamChunk)
   then inc(i)
   else FChunkList.Delete(i);
end;

procedure TAbxContainer.LoadFromStream(Stream: TStream);
begin
 // remove chunk references
 FNotesChunk := nil;
 FTitleChunk := nil;
 FAudioStreamChunk := nil;

 // clear all existing chunks
 Clear;

 inherited;
end;

procedure TAbxContainer.ConvertStreamToChunk(ChunkClass: TCustomChunkClass;
  Stream: TStream);
begin
 if ChunkClass = TAbxTitle then
  begin
   if assigned(FTitleChunk)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FTitleChunk := TAbxTitle.Create;
   FTitleChunk.ChunkFlags := ChunkFlags;
   FTitleChunk.LoadFromStream(Stream);
   AddChunk(FTitleChunk);
  end else
 if ChunkClass = TAbxNotes then
  begin
   if assigned(FNotesChunk)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FNotesChunk := TAbxNotes.Create;
   FNotesChunk.ChunkFlags := ChunkFlags;
   FNotesChunk.LoadFromStream(Stream);
   AddChunk(FNotesChunk);
  end else
 if ChunkClass = TAbxAudioStreams then
  begin
   if assigned(FAudioStreamChunk)
    then raise Exception.Create(RCStrChunkAlreadyExists);
   FAudioStreamChunk := TAbxAudioStreams.Create;
   FAudioStreamChunk.ChunkFlags := ChunkFlags;
   FAudioStreamChunk.LoadFromStream(Stream);
   AddChunk(FAudioStreamChunk);
  end else
 if ChunkClass = TAbxDatabaseStream
  then FDatabaseStreamChunk.LoadFromStream(Stream)
  else inherited;
end;

procedure TAbxContainer.SaveToStream(Stream: TStream);
begin
 if assigned(FDatabaseStreamChunk)
  then FDatabaseStreamChunk.FChunkSize := FDatabaseStreamChunk.GetChunkSize;
 inherited;
end;

procedure TAbxContainer.SetAudioStreamA(const Value: TMemoryStream);
begin
 if AudioStreamA <> Value then
  begin
   // ensure audio stream chunk is available
   AddAudioStreamChunk;

   // copy streams
   with FAudioStreamChunk.AudioStreamA.MemoryStream do
    begin
     Position := 0;
     Value.Position := 0;
     CopyFrom(Value, Value.Size);
    end;
  end;
end;

procedure TAbxContainer.SetAudioStreamB(const Value: TMemoryStream);
begin
 if AudioStreamB <> Value then
  begin
   // ensure audio stream chunk is available
   if not Assigned(FAudioStreamChunk) then
    begin
     FAudioStreamChunk := TAbxAudioStreams.Create;
     AddChunk(FAudioStreamChunk);
    end;

   // copy streams
   with FAudioStreamChunk.AudioStreamB.MemoryStream do
    begin
     Position := 0;
     Value.Position := 0;
     CopyFrom(Value, Value.Size);
    end;
  end;
end;

procedure TAbxContainer.SetDatabaseStream(const Value: TMemoryStream);
begin
 with FDatabaseStreamChunk.MemoryStream do
  begin
   Position := 0;
   CopyFrom(Value, Value.Size);
  end;
end;

procedure TAbxContainer.SetNotes(const Value: string);
begin
 if Notes <> Value then
  begin
   if not Assigned(FNotesChunk) then
    begin
     FNotesChunk := TAbxNotes.Create;
     AddChunk(FNotesChunk);
    end;
   FNotesChunk.Text := Value;
  end;
end;

procedure TAbxContainer.SetTitle(const Value: string);
begin
 if Title <> Value then
  begin
   if not Assigned(FTitleChunk) then
    begin
     FTitleChunk := TAbxTitle.Create;
     AddChunk(FTitleChunk);
    end;
   FTitleChunk.Text := Value;
  end;
end;

end.
