unit DAV_ChunkClasses;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, SysUtils, DAV_Types;

type
  TChunkFlag = (cfSizeFirst, cfReversedByteOrder, cfPadSize,
    cfIncludeChunkInSize);
  TChunkFlags = set of TChunkFlag;
  {$IFDEF DELPHI5}
  TCustomChunk = class(TPersistent)
  {$ELSE}
  TCustomChunk = class(TInterfacedPersistent, IStreamPersist)
  {$ENDIF}
  protected
    FChunkName  : TChunkName;
    FChunkSize  : Cardinal;
    FChunkFlags : TChunkFlags;
    function GetChunkName: AnsiString; virtual;
    function GetChunkSize: Cardinal; virtual;
    function CalculateZeroPad: Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetChunkName(const Value: AnsiString); virtual;
    procedure CheckAddZeroPad(Stream: TStream);
  public
    constructor Create; virtual;
    procedure LoadFromStream(Stream : TStream); virtual;
    procedure SaveToStream(Stream : TStream); virtual;
    procedure LoadFromFile(FileName : TFileName); virtual;
    procedure SaveToFile(FileName : TFileName); virtual;
    property ChunkName: AnsiString read GetChunkName write SetChunkName;
    property ChunkSize: Cardinal read GetChunkSize;
    property ChunkFlags: TChunkFlags read FChunkFlags write FChunkFlags default [];
  end;

  TCustomChunkClass = class of TCustomChunk;

  TDummyChunk = class(TCustomChunk)
  public
    procedure LoadFromStream(Stream : TStream); override;
  end;

  TUnknownChunk = class(TCustomChunk)
  private
    function GetData(Index: Integer): Byte;
    procedure SetData(Index: Integer; const Value: Byte);
  protected
    FDataStream : TMemoryStream;
    function CalculateChecksum: Integer;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;

    property Data[Index : Integer]: Byte read GetData write SetData;
    property DataStream: TMemoryStream read FDataStream;
  end;

  TDefinedChunk = class(TCustomChunk)
  protected
    FFilePosition : Cardinal;
    procedure SetChunkName(const Value: AnsiString); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    class function GetClassChunkName : TChunkName; virtual; abstract;
  published
    property FilePosition : Cardinal read FFilePosition;
  end;

  TDefinedChunkClass = class of TDefinedChunk;

  TFixedDefinedChunk = class(TDefinedChunk)
  private
    function GetStartAddress: Pointer;
    procedure SetStartAddress(const Value: Pointer);
  protected
    FStartAddresses : array of Pointer;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
    property StartAddress: Pointer read GetStartAddress write SetStartAddress;
  public
    class function GetClassChunkSize : Integer; virtual; abstract;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TChunkList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TCustomChunk;
    procedure SetItem(Index: Integer; AChunk: TCustomChunk);
  public
    function Add(AChunk: TCustomChunk): Integer;
    function Extract(Item: TCustomChunk): TCustomChunk;
    function Remove(AChunk: TCustomChunk): Integer;
    function IndexOf(AChunk: TCustomChunk): Integer;
    procedure Insert(Index: Integer; AChunk: TCustomChunk);
    function First: TCustomChunk;
    function Last: TCustomChunk;
    property Items[Index: Integer]: TCustomChunk read GetItem write SetItem; default;
  end;

  TCustomChunkContainer = class(TDefinedChunk)
  private
    function GetSubChunk(Index: Integer): TCustomChunk;
    function GetCount: Integer;
  protected
    FChunkList : TChunkList;
    function GetChunkClass(ChunkName : TChunkName): TCustomChunkClass; virtual; abstract;
    function GetChunkSize: Cardinal; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream: TStream); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AddChunk(Chunk : TCustomChunk); virtual;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    property SubChunk[Index : Integer]: TCustomChunk read GetSubChunk;
    property Count : Integer read GetCount;
  end;

  TChunkContainer = class(TCustomChunkContainer)
  protected
    FRegisteredChunks : array of TDefinedChunkClass;
    function GetChunkClass(ChunkName : TChunkName): TCustomChunkClass; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure RegisterChunkClass(ChunkClass : TDefinedChunkClass);
    procedure RegisterChunkClasses; overload;
    procedure RegisterChunkClasses(ChunkClasses: array of TDefinedChunkClass); overload;
  published
    property Count;
  end;

  TUnknownChunkContainer = class(TUnknownChunk)
  private
    function GetSubChunk(Index: Integer): TCustomChunk;
    function GetCount: Integer;
    function ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream: TStream): TCustomChunk; virtual;
  protected
    FChunkList : TChunkList;
    function CheckForSubchunks: Boolean; virtual;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    property SubChunk[Index : Integer] : TCustomChunk read GetSubChunk;
  published
    property Count : Integer read GetCount;
  end;

  TPNGChunkContainer = class(TUnknownChunkContainer)
  protected
    function CheckForSubchunks: Boolean; override;
  public
    procedure LoadFromStream(Stream : TStream); override;
  end;

  TCustomBinaryChunk = class(TDefinedChunk)
  protected
    FBinaryData : Array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TCustomTextChunk = class(TDefinedChunk)
  protected
    FText : AnsiString;
    procedure SetText(const Value: AnsiString);
    procedure AssignTo(Dest: TPersistent); override;
    property Text: AnsiString read FText write SetText;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TCustomStreamChunk = class(TDefinedChunk)
  protected
    FStream : TStream;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TCustomMemoryStreamChunk = class(TCustomStreamChunk)
  private
    function GetMemoryStream: TMemoryStream;
  public
    constructor Create; override;
    property MemoryStream: TMemoryStream read GetMemoryStream;
  end;

const
  CZeroPad: Integer = 0;

function CompareChunkNames(ChunkNameA, ChunkNameB: TChunkName): Boolean;

implementation

uses
  DAV_Common;

function CompareChunkNames(ChunkNameA, ChunkNameB: TChunkName): Boolean;
begin
 Result := False;
 if ChunkNameA[0] <> ChunkNameB[0] then Exit;
 if ChunkNameA[1] <> ChunkNameB[1] then Exit;
 if ChunkNameA[2] <> ChunkNameB[2] then Exit;
 if ChunkNameA[3] <> ChunkNameB[3] then Exit;
 Result := True;
end;


{ TCustomChunk }

function TCustomChunk.CalculateZeroPad: Integer;
begin
 Result := (2 - (FChunkSize and 1)) and 1;
end;

procedure TCustomChunk.CheckAddZeroPad(Stream: TStream);
begin
 // insert pad byte if necessary
 if cfPadSize in ChunkFlags
  then Stream.Write(CZeroPad, CalculateZeroPad);
end;

constructor TCustomChunk.Create;
begin
 FChunkName := '';
 FChunkSize := 0;
end;

procedure TCustomChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChunk then
  begin
   TCustomChunk(Dest).FChunkName := FChunkName;
   TCustomChunk(Dest).FChunkSize := FChunkSize;
  end
 else inherited;
end;

function TCustomChunk.GetChunkName: AnsiString;
begin
 Result := AnsiString(FChunkName);
end;

function TCustomChunk.GetChunkSize: Cardinal;
begin
 Result := FChunkSize;
end;

procedure TCustomChunk.LoadFromFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   LoadFromStream(FileStream);
  finally
   Free;
  end;
end;

procedure TCustomChunk.SaveToFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmCreate);
 with FileStream do
  try
   SaveToStream(FileStream);
  finally
   Free;
  end;
end;

procedure TCustomChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   Assert(Position <= Size + 8);
   if cfSizeFirst in ChunkFlags then
    begin
     // order known from PNG
     Read(FChunkSize, 4);
     Read(FChunkName, 4);
    end
   else
    begin
     // order known from WAVE, AIFF, etc.
     Read(FChunkName, 4);
     Read(FChunkSize, 4);
    end;
  end;

 // eventually flip bytes
 if cfReversedByteOrder in ChunkFlags
  then Flip32(FChunkSize);
end;

procedure TCustomChunk.SaveToStream(Stream: TStream);
var
  TempSize : Cardinal;
begin
 TempSize := FChunkSize;

 // eventually flip bytes
 if cfReversedByteOrder in ChunkFlags
  then Flip32(TempSize);

 with Stream do
  if cfSizeFirst in ChunkFlags then
   begin
    // order known from PNG
    Write(TempSize, 4);
    Write(FChunkName[0], 4);
   end
  else
   begin
    // order known from WAVE, AIFF, etc.
    Write(FChunkName[0], 4);
    Write(TempSize, 4);
   end;
end;

procedure TCustomChunk.SetChunkName(const Value: AnsiString);
var
  ChunkNameSize : Integer;
begin
 ChunkNameSize := Length(Value);
 if ChunkNameSize > 3 then ChunkNameSize := 4;
 Move(Value[1], FChunkName[0], ChunkNameSize);
end;


{ TDummyChunk }

procedure TDummyChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   inherited;
   Position := Position + FChunkSize;
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;


{ TUnknownChunk }

function TUnknownChunk.CalculateChecksum: Integer;
var
  b : Byte;
begin
 with FDataStream do
  begin
   Position := 0;
   Result := 0;
   while Position < Size do
    begin
     Read(b, 1);
     Result := Result + b;
    end;
  end;
end;

constructor TUnknownChunk.Create;
begin
 inherited;
 FDataStream := TMemoryStream.Create;
end;

destructor TUnknownChunk.Destroy;
begin
 FreeAndNil(FDataStream);
 inherited;
end;

procedure TUnknownChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TUnknownChunk then
  begin
   TUnknownChunk(Dest).FDataStream.CopyFrom(FDataStream, FDataStream.Size);
  end;
end;

function TUnknownChunk.GetData(Index: Integer): Byte;
begin
 if (Index >= 0) and (Index < FDataStream.Size)
  then
   with FDataStream do
    begin
     Position := Index;
     Read(Result, 1);
    end
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TUnknownChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   inherited;
   Assert(FChunkSize <= Size);
   Assert(FChunkName <> #0#0#0#0);
   FDataStream.Clear;
   FDataStream.Size := FChunkSize;
   FDataStream.Position := 0;
   if FChunkSize > 0 then
    if cfIncludeChunkInSize in ChunkFlags
     then FDataStream.CopyFrom(Stream, FChunkSize - 8)
     else FDataStream.CopyFrom(Stream, FChunkSize);

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;

procedure TUnknownChunk.SaveToStream(Stream: TStream);
begin
 with Stream do
  begin
   FChunkSize := FDataStream.Size; //Length(FData);
   inherited;
   FDataStream.Position := 0;
   CopyFrom(FDataStream, FDataStream.Position);

   // check and eventually add zero pad
   CheckAddZeroPad(Stream);
  end;
end;

procedure TUnknownChunk.SetData(Index: Integer; const Value: Byte);
begin
 if (Index >= 0) and (Index < FDataStream.Size)
  then
   with FDataStream do
    begin
     Position := Index;
     Write(Value, 1);
    end
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;


{ TDefinedChunk }

constructor TDefinedChunk.Create;
begin
 inherited;
 FFilePosition := 0;
 FChunkName := GetClassChunkName;
end;

procedure TDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDefinedChunk
  then TDefinedChunk(Dest).FFilePosition := FFilePosition;
end;

procedure TDefinedChunk.LoadFromStream(Stream: TStream);
var
  TempChunkName : TChunkName;
begin
 with Stream do
  begin
   if cfSizeFirst in ChunkFlags then
    begin
     // Assume chunk name fits the defined one
     Position := Position + 4;
     Read(TempChunkName, 4);
     Assert(TempChunkName = FChunkName);
     Position := Position - 8;
    end
   else
    begin
     // Assume chunk name fits the defined one
     Read(TempChunkName, 4);
     Assert(TempChunkName = FChunkName);
     Position := Position - 4;
    end;
   inherited;
  end;
end;

procedure TDefinedChunk.SetChunkName(const Value: AnsiString);
begin
 inherited;
 if Value <> FChunkName
  then raise Exception.Create('Chunk name must always be ''' +
    string(AnsiString(FChunkName)) + '''');
end;


{ TFixedDefinedChunk }

constructor TFixedDefinedChunk.Create;
begin
 inherited;
 SetLength(FStartAddresses, 1);
 FChunkSize := GetClassChunkSize;
end;

procedure TFixedDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TFixedDefinedChunk then
  begin
   SetLength(TFixedDefinedChunk(Dest).FStartAddresses, Length(FStartAddresses));
   Move(FStartAddresses[0], TFixedDefinedChunk(Dest).FStartAddresses[0], Length(FStartAddresses) * SizeOf(Pointer));
  end;
end;

function TFixedDefinedChunk.GetChunkSize: Cardinal;
begin
 Result := GetClassChunkSize;
end;

function TFixedDefinedChunk.GetStartAddress: Pointer;
begin
 Result := FStartAddresses[0];
end;

procedure TFixedDefinedChunk.SetStartAddress(const Value: Pointer);
begin
 FStartAddresses[0] := Value;
end;

procedure TFixedDefinedChunk.LoadFromStream(Stream: TStream);
var
  BytesReaded : Integer;
begin
 inherited;
 with Stream do
  begin
   if FChunkSize <= Cardinal(GetClassChunkSize)
    then Read(FStartAddresses[0]^, FChunkSize)
    else
     begin
      BytesReaded := Read(FStartAddresses[0]^, GetClassChunkSize);
      Assert(BytesReaded = GetClassChunkSize);
      Position := Position + FChunkSize - GetClassChunkSize;
     end;
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;

procedure TFixedDefinedChunk.SaveToStream(Stream: TStream);
var
  BytesWritten: Cardinal;
begin
 FChunkSize := GetClassChunkSize;
 inherited;
 try
  BytesWritten := Stream.Write(FStartAddresses[0]^, GetClassChunkSize);
  Assert(BytesWritten = FChunkSize);

  // check and eventually add zero pad
  CheckAddZeroPad(Stream);
 except
  raise Exception.Create('Wrong Start Addess of Chunk: ' + string(ChunkName));
 end;
end;


{ TChunkList }

function TChunkList.Add(AChunk: TCustomChunk): Integer;
begin
 Result := inherited Add(TObject(AChunk));
end;

function TChunkList.Extract(Item: TCustomChunk): TCustomChunk;
begin
 Result := TCustomChunk(inherited Extract(TObject(Item)));
end;

function TChunkList.First: TCustomChunk;
begin
 Result := TCustomChunk(inherited First);
end;

function TChunkList.GetItem(Index: Integer): TCustomChunk;
begin
 Result := TCustomChunk(inherited GetItem(Index));
end;

function TChunkList.IndexOf(AChunk: TCustomChunk): Integer;
begin
 Result := inherited IndexOf(TObject(AChunk));
end;

procedure TChunkList.Insert(Index: Integer; AChunk: TCustomChunk);
begin
 inherited Insert(Index, TObject(AChunk));
end;

function TChunkList.Last: TCustomChunk;
begin
 Result := TCustomChunk(inherited Last);
end;

function TChunkList.Remove(AChunk: TCustomChunk): Integer;
begin
 Result := inherited Remove(TObject(AChunk));
end;

procedure TChunkList.SetItem(Index: Integer; AChunk: TCustomChunk);
begin
 inherited SetItem(Index, TObject(AChunk));
end;


{ TCustomChunkContainer }

constructor TCustomChunkContainer.Create;
begin
 inherited;
 FChunkList := TChunkList.Create;
end;

destructor TCustomChunkContainer.Destroy;
begin
 FreeAndNil(FChunkList);
 inherited;
end;

procedure TCustomChunkContainer.AssignTo(Dest: TPersistent);
{$IFDEF DELPHI5}
var
  i : Integer;
{$ENDIF}
begin
 inherited;
 if Dest is TCustomChunkContainer then
  begin
   {$IFDEF DELPHI5}
   for i := 0 to TCustomChunkContainer(Dest).FChunkList.Count - 1
    do TCustomChunk(TCustomChunkContainer(Dest).FChunkList[i]).Assign(TCustomChunk(FChunkList[i]));
   {$ELSE}
   TCustomChunkContainer(Dest).FChunkList.Assign(FChunkList);
   {$ENDIF}
  end;
end;

procedure TCustomChunkContainer.AddChunk(Chunk: TCustomChunk);
begin
 FChunkList.Add(Chunk);
end;

function TCustomChunkContainer.GetCount: Integer;
begin
 Result := FChunkList.Count;
end;

function TCustomChunkContainer.GetSubChunk(Index: Integer): TCustomChunk;
begin
 if (Index >= 0) and (Index < FChunkList.Count)
  then Result := FChunkList[Index]
  else Result := nil;
end;

procedure TCustomChunkContainer.LoadFromStream(Stream: TStream);
var
  ChunkEnd  : Integer;
  ChunkName : TChunkName;
begin
 inherited;
 with Stream do
  begin
   ChunkEnd := Position + FChunkSize;
   Assert(ChunkEnd <= Stream.Size);
   while Position < ChunkEnd do
    begin
     if cfSizeFirst in ChunkFlags then
      begin
       Position := Position + 4;
       Read(ChunkName, 4);
       Position := Position - 8;
      end
     else
      begin
       Read(ChunkName, 4);
       Position := Position - 4;
      end;
     ConvertStreamToChunk(GetChunkClass(ChunkName), Stream);
    end;
   if Position <> ChunkEnd
    then Position := ChunkEnd;

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then Position := Position + CalculateZeroPad;
  end;
end;

procedure TCustomChunkContainer.ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream : TStream);
var
  Chunk : TCustomChunk;
begin
 Chunk := ChunkClass.Create;
 Chunk.ChunkFlags := ChunkFlags;
 Chunk.LoadFromStream(Stream);
 AddChunk(Chunk);
end;

function TCustomChunkContainer.GetChunkSize: Cardinal;
var
  i : Integer;
begin
 Result := 0;
 for i := 0 to FChunkList.Count - 1
  do Inc(Result, FChunkList[i].ChunkSize + 8); // Chunk Size + Chunk Frame (8)
end;

procedure TCustomChunkContainer.SaveToStream(Stream: TStream);
var
  i : Integer;
begin
 FChunkSize := GetChunkSize;
 inherited;
 for i := 0 to FChunkList.Count - 1
  do FChunkList[i].SaveToStream(Stream);

 // insert pad byte if necessary
 if cfPadSize in ChunkFlags
  then Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TChunkContainer }

procedure TChunkContainer.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TChunkContainer then
  begin
   SetLength(TChunkContainer(Dest).FRegisteredChunks, Length(FRegisteredChunks));
   Move(FRegisteredChunks, TChunkContainer(Dest).FRegisteredChunks, Length(FRegisteredChunks) * SizeOf(TCustomChunkClass));
  end;
end;

function TChunkContainer.GetChunkClass(ChunkName: TChunkName): TCustomChunkClass;
var
  Index : Integer;
begin
 Result := TUnknownChunk;
 for Index := 0 to Length(FRegisteredChunks) - 1 do
  if CompareChunkNames(FRegisteredChunks[Index].GetClassChunkName, ChunkName) then
   begin
    Result := FRegisteredChunks[Index];
    Exit;
   end;
end;

procedure TChunkContainer.RegisterChunkClass(ChunkClass: TDefinedChunkClass);
var
  i : Integer;
begin
 // Check if the chunk class is already in the list
 for i := 0 to Length(FRegisteredChunks) - 1 do
  if FRegisteredChunks[i] = ChunkClass then Exit;

 // If not, add chunk class to the list
 SetLength(FRegisteredChunks, Length(FRegisteredChunks) + 1);
 FRegisteredChunks[Length(FRegisteredChunks) - 1] := ChunkClass;
end;

procedure TChunkContainer.RegisterChunkClasses(ChunkClasses: array of TDefinedChunkClass);
var
  i : Integer;
begin
 for i := 0 to Length(ChunkClasses) - 1
  do RegisterChunkClass(ChunkClasses[i]);
end;

procedure TChunkContainer.RegisterChunkClasses;
var
  i : Integer;
begin
 for i := 0 to FChunkList.Count - 1
  do RegisterChunkClass(TDefinedChunkClass(FChunkList[i].ClassType));
end;


{ TUnknownChunkContainer }

constructor TUnknownChunkContainer.Create;
begin
 inherited;
 FChunkList := TChunkList.Create;
end;

destructor TUnknownChunkContainer.Destroy;
begin
 FreeAndNil(FChunkList);
 inherited;
end;

function TUnknownChunkContainer.ConvertStreamToChunk(ChunkClass: TCustomChunkClass; Stream : TStream): TCustomChunk;
begin
 Result := ChunkClass.Create;
 Result.ChunkFlags := ChunkFlags;
 Result.LoadFromStream(Stream);
 FChunkList.Add(Result);
end;

function TUnknownChunkContainer.CheckForSubchunks: Boolean;
var
  TempSize : Cardinal;
  TempName : TChunkName;
begin
 Result := False;
 if (ChunkName = 'RIFF') or (ChunkName = 'FORM') or (ChunkName = 'MTrk')
  then FDataStream.Position := 4
  else FDataStream.Position := 0;
 while FDataStream.Position + 8 < FChunkSize do
  begin
   if cfSizeFirst in ChunkFlags then
    begin
     // read chunk size
     FDataStream.Read(TempSize, 4);

     // read chunk name
     FDataStream.Read(TempName, 4);
    end
   else
    begin
     // read chunk name
     FDataStream.Read(TempName, 4);

     // read chunk size
     FDataStream.Read(TempSize, 4);
    end;

   // eventually reverse byte order
   if cfReversedByteOrder in ChunkFlags
    then Flip32(TempSize);

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then TempSize := TempSize + (2 - (TempSize and 1)) and 1;

   if (FDataStream.Position + TempSize) <= FChunkSize
    then
     begin
      FDataStream.Position := FDataStream.Position + TempSize;
      Result := FDataStream.Position = FChunkSize;
      if Result then break;
     end
    else Exit;
  end;
end;

procedure TUnknownChunkContainer.LoadFromStream(Stream: TStream);
begin
 inherited;

 if CheckForSubchunks then
  begin
   if (ChunkName = 'RIFF') or (ChunkName = 'FORM')
    then FDataStream.Position := 4
    else FDataStream.Position := 0;
   while FDataStream.Position + 8 < FChunkSize
    do ConvertStreamToChunk(TUnknownChunkContainer, FDataStream);
  end;
end;

procedure TUnknownChunkContainer.SaveToStream(Stream: TStream);
var
  i : Integer;
begin
 FChunkSize := GetChunkSize;
 inherited;
 for i := 0 to FChunkList.Count - 1
  do FChunkList[i].SaveToStream(Stream);

 // insert pad byte if necessary
 if cfPadSize in ChunkFlags
  then Stream.Write(CZeroPad, CalculateZeroPad);
end;

function TUnknownChunkContainer.GetChunkSize: Cardinal;
var
  i : Integer;
begin
 Result := 0;
 for i := 0 to FChunkList.Count - 1
  do Inc(Result, FChunkList[i].ChunkSize + 8); // Chunk Size + Chunk Frame (8)
end;

function TUnknownChunkContainer.GetCount: Integer;
begin
 Result := FChunkList.Count;
end;

function TUnknownChunkContainer.GetSubChunk(Index: Integer): TCustomChunk;
begin
 if (Index >= 0) and (Index < FChunkList.Count)
  then Result := FChunkList[Index]
  else Result := nil;
end;


{ TPNGChunkContainer }

function TPNGChunkContainer.CheckForSubchunks: Boolean;
var
  TempSize : Cardinal;
  TempName : TChunkName;
begin
 Result := False;
 FDataStream.Position := 0;
 while FDataStream.Position + 8 < FChunkSize do
  begin
   if cfSizeFirst in ChunkFlags then
    begin
     // read chunk size
     FDataStream.Read(TempSize, 4);

     // read chunk name
     FDataStream.Read(TempName, 4);
    end
   else
    begin
     // read chunk name
     FDataStream.Read(TempName, 4);

     // read chunk size
     FDataStream.Read(TempSize, 4);
    end;

   // eventually reverse byte order
   if cfReversedByteOrder in ChunkFlags
    then Flip32(TempSize);

   // eventually skip padded zeroes
   if cfPadSize in ChunkFlags
    then TempSize := TempSize + (2 - (TempSize and 1)) and 1;

   // checksum 
   TempSize := TempSize + 4;

   if (FDataStream.Position + TempSize) <= FChunkSize
    then
     begin
      FDataStream.Position := FDataStream.Position + TempSize;
      Result := FDataStream.Position = FChunkSize;
      if Result then break;
     end
    else Exit;
  end;
end;

procedure TPNGChunkContainer.LoadFromStream(Stream: TStream);
var
  PngMagic : TChunkName;
  CheckSum : Integer;
  SubChunk : TUnknownChunkContainer;
begin
 with Stream do
  begin
   Read(FChunkName, 4);
   Read(PngMagic, 4);
   if PngMagic <> #$0D#$0A#$1A#$0A
    then Exception.Create('Not a valid PNG file');
   FChunkSize := Stream.Size - 8;

   FDataStream.Clear;
   FDataStream.Size := FChunkSize;
   FDataStream.Position := 0;
   FDataStream.CopyFrom(Stream, FChunkSize);
  end;

 if CheckForSubchunks then
  begin
   FDataStream.Position := 0;
   while FDataStream.Position + 8 < FChunkSize do
    begin
     SubChunk := TUnknownChunkContainer(ConvertStreamToChunk(TUnknownChunkContainer, FDataStream));

     // read checksum
     FDataStream.Read(CheckSum, 4);
     if Checksum <> SubChunk.CalculateChecksum
      then raise Exception.Create('Checksum Error');
    end;
  end;
end;


{ TCustomBinaryChunk }

procedure TCustomBinaryChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomBinaryChunk then
  begin
   SetLength(TCustomBinaryChunk(Dest).FBinaryData, Length(FBinaryData));
   Move(FBinaryData, TCustomBinaryChunk(Dest).FBinaryData, SizeOf(FBinaryData));
  end;
end;

procedure TCustomBinaryChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(FBinaryData, FChunkSize);
 Stream.Read(FBinaryData[0], Length(FBinaryData));
end;

procedure TCustomBinaryChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := Length(FBinaryData);
 inherited;
 Stream.Write(FBinaryData[0], FChunkSize);
end;


{ TCustomTextChunk }

procedure TCustomTextChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomTextChunk then
  begin
   TCustomTextChunk(Dest).FText  := FText;
  end;
end;

procedure TCustomTextChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(FText, FChunkSize);
 Stream.Read(FText[1], Length(FText));

 // eventually skip padded zeroes
 if cfPadSize in ChunkFlags
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TCustomTextChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := Length(FText);

 inherited;
 Stream.Write(FText[1], FChunkSize);

 // eventually skip padded zeroes
 if (cfPadSize in ChunkFlags)
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TCustomTextChunk.SetText(const Value: AnsiString);
begin
 if FText <> Value then
  begin
   FText := Value;
   FChunkSize := Length(FText);
  end;
end;


{ TCustomStreamChunk }

procedure TCustomStreamChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomStreamChunk then
  begin
   FStream.Position := 0;
   TCustomStreamChunk(Dest).FStream.Position := 0;
   TCustomStreamChunk(Dest).FStream.CopyFrom(FStream, FStream.Size); 
  end;
end;

destructor TCustomStreamChunk.Destroy;
begin
 FreeAndNil(FStream);
 inherited;
end;

function TCustomStreamChunk.GetChunkSize: Cardinal;
begin
 FChunkSize := FStream.Size;
 Result := inherited GetChunkSize;
end;

procedure TCustomStreamChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 FStream.Position := 0;
 FStream.CopyFrom(Stream, FChunkSize);
 FStream.Position := 0;

 // eventually skip padded zeroes
 if cfPadSize in ChunkFlags
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TCustomStreamChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := FStream.Size;
 inherited;
 FStream.Position := 0;
 Stream.CopyFrom(FStream, FStream.Size);

 // eventually skip padded zeroes
 if (cfPadSize in ChunkFlags)
  then Stream.Position := Stream.Position + CalculateZeroPad;
end;


{ TCustomMemoryStreamChunk }

constructor TCustomMemoryStreamChunk.Create;
begin
 inherited;
 FStream := TMemoryStream.Create;
end;

function TCustomMemoryStreamChunk.GetMemoryStream: TMemoryStream;
begin
 Result := TMemoryStream(FStream);
end;

end.