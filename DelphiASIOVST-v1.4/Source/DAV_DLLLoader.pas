{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_DLLLoader;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LCLType, LResources, Dynlibs,
  {$IFDEF MSWINDOWS} Windows, {$ENDIF} {$ELSE} Windows, Messages, {$ENDIF}
  Classes;

const
  IMPORTED_NAME_OFFSET = $00000002;
  IMAGE_ORDINAL_FLAG32 = $80000000;
  IMAGE_ORDINAL_MASK32 = $0000FFFF;

  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGH = 1;
  IMAGE_REL_BASED_LOW = 2;
  IMAGE_REL_BASED_HIGHLOW = 3;
  IMAGE_REL_BASED_HIGHADJ = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR = 5;
  IMAGE_REL_BASED_SECTION = 6;
  IMAGE_REL_BASED_REL32 = 7;

type
  PPLongWord = ^PLongWord;
  PPWord = ^PWord;
  PPointer = ^Pointer;

  PWordArray = ^TWordArray;
  TWordArray = array[0..(2147483647 div SizeOf(Word)) - 1] of Word;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array [0..(2147483647 div SizeOf(LongWord)) - 1] of LongWord;

  PNativeUIntArray = ^TNativeUIntArray;
  TNativeUIntArray = array [0..(2147483647 div SizeOf(NativeUInt)) - 1] of NativeUInt;

  PImageDOSHeader = ^TImageDOSHeader;
  TImageDOSHeader = packed record
    Signature : Word;
    PartPag   : Word;
    PageCnt   : Word;
    ReloCnt   : Word;
    HdrSize   : Word;
    MinMem    : Word;
    MaxMem    : Word;
    ReloSS    : Word;
    ExeSP     : Word;
    ChkSum    : Word;
    ExeIP     : Word;
    ReloCS    : Word;
    TablOff   : Word;
    Overlay   : Word;
    Reserved  : packed array [0..3] of Word;
    OEMID     : Word;
    OEMInfo   : Word;
    Reserved2 : packed array [0..9] of Word;
    LFAOffset : LongWord;
  end;

  TISHMisc = packed record
    case Byte of
      0: (PhysicalAddress: NativeInt);
      1: (VirtualSize: NativeInt);
  end;

  PImageSectionHeaders = ^TImageSectionHeaders;
  TImageSectionHeaders =
    array[0..(2147483647 div SizeOf(TImageSectionHeader)) - 1] of
    TImageSectionHeader;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    OriginalFirstThunk : LongWord;
    TimeDateStamp      : LongWord;
    ForwarderChain     : LongWord;
    Name               : LongWord;
    FirstThunk         : LongWord;
  end;

  PImageBaseRelocation = ^TImageBaseRelocation;
  TImageBaseRelocation = packed record
    VirtualAddress : LongWord;
    SizeOfBlock    : LongWord;
  end;

  PSection = ^TSection;
  TSection = packed record
    Base            : Pointer;
    RVA             : LongWord;
    Size            : LongWord;
    Characteristics : LongWord;
  end;
  TSections = array of TSection;

  TDLLEntryProc = function(hinstDLL: HMODULE; dwReason: LongWord;
    lpvReserved: Pointer): Boolean; stdcall;

  TNameOrID = (niName, niID);

  TExternalLibrary = record
    LibraryName   : string;
    LibraryHandle : HINST;
  end;

  TExternalLibrarys = array of TExternalLibrary;

  PDLLFunctionImport = ^TDLLFunctionImport;

  TDLLFunctionImport = record
    NameOrID : TNameOrID;
    Name     : string;
    ID       : Integer;
  end;

  PDLLImport = ^TDLLImport;

  TDLLImport = record
    LibraryName   : string;
    LibraryHandle : HINST;
    Entries       : array of TDLLFunctionImport;
  end;

  TImports = array of TDLLImport;

  PDLLFunctionExport = ^TDLLFunctionExport;

  TDLLFunctionExport = record
    Name            : string;
    Index           : Integer;
    FunctionPointer : Pointer;
  end;

  TExports = array of TDLLFunctionExport;

  TExportTreeLink = Pointer;

  PExportTreeNode = ^TExportTreeNode;

  TExportTreeNode = record
    TheChar   : Char;
    Link      : TExportTreeLink;
    LinkExist : Boolean;
    Prevoius,
    Next,
    Up, Down  : PExportTreeNode;
  end;

  TExportTree = class(TPersistent)
  private
    FRoot: PExportTreeNode;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Dump;
    function Add(FunctionName: string; Link: TExportTreeLink): Boolean;
    function Delete(FunctionName: string): Boolean;
    function Find(FunctionName: string; var Link: TExportTreeLink): Boolean;
  end;

  TDLLLoader = class(TPersistent)
  private
    FImageBase            : Pointer;
    FImageBaseDelta       : NativeInt;
    FDLLProc              : TDLLEntryProc;
    FExternalLibraryArray : TExternalLibrarys;
    FImportArray          : TImports;
    FExportArray          : TExports;
    FSections             : TSections;
    FExportTree           : TExportTree;
    function FindExternalLibrary(LibraryName: string): Integer;
    function LoadExternalLibrary(LibraryName: string): Integer;
    function GetExternalLibraryHandle(LibraryName: string): HInst;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(Stream: TStream): Boolean;
    function Unload: Boolean;
    function FindExport(FunctionName: string): Pointer;
    function FindExportPerIndex(FunctionIndex: Integer): Pointer;
    function GetExportList: TStringList;
  end;

implementation

uses
  SysUtils;

function CreateExportTreeNode(AChar: Char): PExportTreeNode;
begin
  GetMem(Result, SizeOf(TExportTreeNode));
  with Result^ do
   begin
    TheChar   := AChar;
    Link      := nil;
    LinkExist := False;
    Prevoius  := nil;
    Next      := nil;
    Up        := nil;
    Down      := nil;
   end;
end;

procedure DestroyExportTreeNode(Node: PExportTreeNode);
begin
  if Assigned(Node) then
   begin
    DestroyExportTreeNode(Node^.Next);
    DestroyExportTreeNode(Node^.Down);
    FreeMem(Node);
   end;
end;

{ TExportTree }

procedure TExportTree.AssignTo(Dest: TPersistent);
begin
 if Dest is TExportTree then
  begin
   FRoot^ := Self.FRoot^;
  end
 else inherited;
end;

constructor TExportTree.Create;
begin
  inherited Create;
  FRoot := nil;
end;

destructor TExportTree.Destroy;
begin
  DestroyExportTreeNode(FRoot);
  inherited Destroy;
end;

procedure TExportTree.Dump;
var
  Ident: Integer;

  procedure DumpNode(Node: PExportTreeNode);
  var
    SubNode: PExportTreeNode;
    IdentCounter, IdentOld: Integer;
  begin
    for IdentCounter := 1 to Ident do Write(' ');
    Write(Node^.TheChar);
    IdentOld := Ident;
    SubNode := Node^.Next;
    while Assigned(SubNode) do
     begin
      Write(SubNode^.TheChar);
      if not Assigned(SubNode^.Next) then break;
      Inc(Ident);
      SubNode := SubNode^.Next;
     end;
    writeLN;
    Inc(Ident);
    while Assigned(SubNode) and (SubNode <> Node) do
     begin
      if Assigned(SubNode^.Down) then DumpNode(SubNode^.Down);
      SubNode := SubNode^.Prevoius;
      Dec(Ident);
     end;
    Ident := IdentOld;
    if Assigned(Node^.Down) then DumpNode(Node^.Down);
  end;

begin
  Ident := 0;
  DumpNode(FRoot);
end;

function TExportTree.Add(FunctionName: string; Link: TExportTreeLink): Boolean;
var
  stringLength, Position, PositionCounter: Integer;
  NewNode, LastNode, Node: PExportTreeNode;
  stringChar, NodeChar: Char;
begin
  Result := False;
  stringLength := Length(FunctionName);
  if stringLength > 0 then
   begin
    LastNode := nil;
    Node := FRoot;
    for Position := 1 to stringLength do
     begin
      stringChar := FunctionName[Position];
      if Assigned(Node) then
       begin
        NodeChar := Node^.TheChar;
        if NodeChar = stringChar then
         begin
          LastNode := Node;
          Node := Node^.Next;
         end else
         begin
          while (NodeChar < stringChar) and Assigned(Node^.Down) do
           begin
            Node := Node^.Down;
            NodeChar := Node^.TheChar;
           end;
          if NodeChar = stringChar then
           begin
            LastNode := Node;
            Node := Node^.Next;
           end else
           begin
            NewNode := CreateExportTreeNode(stringChar);
            if NodeChar < stringChar then
             begin
              NewNode^.Down := Node^.Down;
              NewNode^.Up := Node;
              if Assigned(NewNode^.Down) then
                NewNode^.Down^.Up := NewNode;
              NewNode^.Prevoius := Node^.Prevoius;
              Node^.Down := NewNode;
             end else if NodeChar > stringChar then
             begin
              NewNode^.Down := Node;
              NewNode^.Up := Node^.Up;
              if Assigned(NewNode^.Up) then
                NewNode^.Up^.Down := NewNode;
              NewNode^.Prevoius := Node^.Prevoius;
              if not Assigned(NewNode^.Up) then
                if Assigned(NewNode^.Prevoius)
                 then NewNode^.Prevoius^.Next := NewNode
                 else FRoot := NewNode;
              Node^.Up := NewNode;
             end;
            LastNode := NewNode;
            Node := LastNode^.Next;
           end;
         end;
       end else
       begin
        for PositionCounter := Position to stringLength do
         begin
          NewNode := CreateExportTreeNode(FunctionName[PositionCounter]);
          if Assigned(LastNode) then
           begin
            NewNode^.Prevoius := LastNode;
            LastNode^.Next := NewNode;
            LastNode := LastNode^.Next;
           end else if not Assigned(FRoot) then
           begin
            FRoot := NewNode;
            LastNode := FRoot;
           end;
         end;
        break;
       end;
     end;
    if Assigned(LastNode) then
      if not LastNode^.LinkExist then
       begin
        LastNode^.Link := Link;
        LastNode^.LinkExist := True;
        Result := True;
       end;
   end;
end;

function TExportTree.Delete(FunctionName: string): Boolean;
var
  stringLength, Position : Integer;
  Node                   : PExportTreeNode;
  stringChar, NodeChar   : Char;
begin
  Result := False;
  stringLength := Length(FunctionName);
  if stringLength > 0 then
   begin
    Node := FRoot;
    for Position := 1 to stringLength do
     begin
      stringChar := FunctionName[Position];
      if Assigned(Node) then
       begin
        NodeChar := Node^.TheChar;
        while (NodeChar <> stringChar) and Assigned(Node^.Down) do
         begin
          Node := Node^.Down;
          NodeChar := Node^.TheChar;
         end;
        if NodeChar = stringChar then
         begin
          if (Position = stringLength) and Node^.LinkExist then
           begin
            Node^.LinkExist := False;
            Result := True;
            break;
           end;
          Node := Node^.Next;
         end;
       end else
        break;
     end;
   end;
end;

function TExportTree.Find(FunctionName: string;
  var Link: TExportTreeLink): Boolean;
var
  stringLength, Position : Integer;
  Node                   : PExportTreeNode;
  stringChar, NodeChar   : Char;
begin
  Result := False;
  stringLength := Length(FunctionName);
  if stringLength > 0 then
   begin
    Node := FRoot;
    for Position := 1 to stringLength do
     begin
      stringChar := FunctionName[Position];
      if Assigned(Node) then
       begin
        NodeChar := Node^.TheChar;
        while (NodeChar <> stringChar) and Assigned(Node^.Down) do
         begin
          Node := Node^.Down;
          NodeChar := Node^.TheChar;
         end;
        if NodeChar = stringChar then
         begin
          if (Position = stringLength) and Node^.LinkExist then
           begin
            Link := Node^.Link;
            Result := True;
            Break;
           end;
          Node := Node^.Next;
         end;
       end else
        break;
     end;
   end;
end;

{ TDLLLoader }

constructor TDLLLoader.Create;
begin
  inherited Create;
  FImageBase            := nil;
  FDLLProc              := nil;
  FExternalLibraryArray := nil;
  FImportArray          := nil;
  FExportArray          := nil;
  FSections             := nil;
  FExportTree           := nil;
end;

destructor TDLLLoader.Destroy;
begin
  if @FDLLProc <> nil then Unload;
  if Assigned(FExportTree)
   then FreeAndNil(FExportTree);
  FExportTree := nil;
  inherited Destroy;
end;

procedure TDLLLoader.AssignTo(Dest: TPersistent);
begin
 if Dest is TDLLLoader then
  with TDLLLoader(Dest) do
   begin
    FImageBase            := Self.FImageBase;
    FDLLProc              := Self.FDLLProc;
    FExternalLibraryArray := Self.FExternalLibraryArray;
    FImportArray          := Self.FImportArray;
    FExportArray          := Self.FExportArray;
    FSections             := Self.FSections;
    FExportTree.Assign(Self.FExportTree);
   end
 else inherited;
end;

function TDLLLoader.FindExternalLibrary(LibraryName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FExternalLibraryArray) - 1 do
    if FExternalLibraryArray[I].LibraryName = LibraryName then
     begin
      Result := I;
      Exit;
     end;
end;

function TDLLLoader.LoadExternalLibrary(LibraryName: string): Integer;
begin
  Result := FindExternalLibrary(LibraryName);
  if Result < 0 then
   begin
    Result := Length(FExternalLibraryArray);
    SetLength(FExternalLibraryArray, Length(FExternalLibraryArray) + 1);
    FExternalLibraryArray[Result].LibraryName := LibraryName;
    FExternalLibraryArray[Result].LibraryHandle :=
      LoadLibrary(PChar(LibraryName));
//    Assert(FExternalLibraryArray[Result].LibraryHandle <> 0);
   end;
end;

function TDLLLoader.GetExternalLibraryHandle(LibraryName: string): HInst;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(FExternalLibraryArray) - 1 do
    if FExternalLibraryArray[I].LibraryName = LibraryName then
     begin
      Result := FExternalLibraryArray[I].LibraryHandle;
      Exit;
     end;
end;

(*
function LoadResource(hModule: HINST; hResInfo: HRSRC): HGLOBAL; stdcall;
begin
 result := Windows.LoadResource(hModule, hResInfo);
end;

function SizeofResource(hModule: HINST; hResInfo: HRSRC): DWORD; stdcall;
begin
 result := Windows.SizeofResource(hModule, hResInfo);
end;
*)

{$IFNDEF FPC}
function GetModuleFileNameA(hModule: HINST; lpFilename: PAnsiChar; nSize: DWORD): DWORD; stdcall;
begin
 Result := Windows.GetModuleFileNameA(hModule, lpFilename, nSize);
 if (Result = 0) and (hModule <> 0) then
  begin
   StrCopy(lpFilename, @ParamStr(0)[1]);
   Result := Length(ParamStr(0));
  end;
end;
{$ENDIF}

function GetModuleFileNameW(hModule: HINST; lpFilename: PWideChar; nSize: DWORD): DWORD; stdcall;
begin
 result := Windows.GetModuleFileNameW(hModule, lpFilename, nSize);
end;

(*
var
  LastHMODULE : HMODULE;

function GetInternalProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
begin
 if LastHMODULE <> hModule
  then
   begin
    result := GetProcAddress(hModule, lpProcName);
    LastHMODULE := hModule;
   end
  else result := GetProcAddress(hModule, lpProcName);
end;
*)

function TDLLLoader.Load(Stream: TStream): Boolean;
var
  ImageDOSHeader: TImageDOSHeader;
  ImageNTHeaders: TImageNTHeaders;
  OldProtect: LongWord;

  function ConvertPointer(RVA: NativeUInt): Pointer;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Length(FSections) - 1 do
      if (RVA < (FSections[I].RVA + FSections[I].Size)) and
        (RVA >= FSections[I].RVA) then
       begin
        Result := Pointer(NativeUInt((RVA - NativeUInt(FSections[I].RVA)) +
          NativeUInt(FSections[I].Base)));
        Exit;
       end;
  end;

  function ReadImageHeaders: Boolean;
  begin
    Result := False;
    if Stream.Size > 0 then
     begin
      FillChar(ImageNTHeaders, SizeOf(TImageNTHeaders), #0);
      if Stream.Read(ImageDOSHeader, SizeOf(TImageDOSHeader)) <> SizeOf(TImageDOSHeader) then Exit;
      if ImageDOSHeader.Signature <> $5A4D then Exit;
      if Stream.Seek(ImageDOSHeader.LFAOffset, soFrombeginning) <> LongInt(ImageDOSHeader.LFAOffset) then Exit;
      if Stream.Read(ImageNTHeaders.Signature, SizeOf(LongWord)) <> SizeOf(LongWord) then Exit;
      if ImageNTHeaders.Signature <> $00004550 then Exit;
      if Stream.Read(ImageNTHeaders.FileHeader, SizeOf(TImageFileHeader)) <> SizeOf(TImageFileHeader) then Exit;
      {$IFDEF CPU32}
      if ImageNTHeaders.FileHeader.Machine <> $14C then Exit;
      {$ELSE}
      if ImageNTHeaders.FileHeader.Machine <> $8664 then Exit;
      {$ENDIF}
      if Stream.Read(ImageNTHeaders.OptionalHeader, ImageNTHeaders.FileHeader.SizeOfOptionalHeader) <> ImageNTHeaders.FileHeader.SizeOfOptionalHeader then Exit;
      Result := True;
     end;
  end;

  function InitializeImage: Boolean;
  var
    SectionBase: Pointer;
    OldPosition: Integer;
  begin
    Result := False;
    with ImageNTHeaders do
      if FileHeader.NumberOfSections > 0 then
       begin
        FImageBase := VirtualAlloc(nil, OptionalHeader.SizeOfImage, MEM_RESERVE, PAGE_NOACCESS);
        FImageBaseDelta := NativeUInt(FImageBase) - OptionalHeader.ImageBase;
        SectionBase := VirtualAlloc(FImageBase, OptionalHeader.SizeOfHeaders, MEM_COMMIT, PAGE_READWRITE);
        OldPosition := Stream.Position;
        Stream.Seek(0, soFrombeginning);
        Stream.Read(SectionBase^, OptionalHeader.SizeOfHeaders);
        VirtualProtect(SectionBase, OptionalHeader.SizeOfHeaders, PAGE_READONLY, OldProtect);
        Stream.Seek(OldPosition, soFrombeginning);
        Result := True;
       end;
  end;

  function ReadSections: Boolean;
  var
    I              : Integer;
    Section        : TImageSectionHeader;
    SectionHeaders : PImageSectionHeaders;
  begin
    Result := False;
    if ImageNTHeaders.FileHeader.NumberOfSections > 0 then
     begin
      GetMem(SectionHeaders, ImageNTHeaders.FileHeader.NumberOfSections * SizeOf(TImageSectionHeader));
      if Stream.Read(SectionHeaders^, (ImageNTHeaders.FileHeader.NumberOfSections * SizeOf(TImageSectionHeader))) <> (ImageNTHeaders.FileHeader.NumberOfSections * SizeOf(TImageSectionHeader))
       then Exit;
      SetLength(FSections, ImageNTHeaders.FileHeader.NumberOfSections);
      for I := 0 to ImageNTHeaders.FileHeader.NumberOfSections - 1 do
       begin
        Section := SectionHeaders^[I];
        FSections[I].RVA := Section.VirtualAddress;
        FSections[I].Size := Section.SizeOfRawData;
        if FSections[I].Size < Section.Misc.VirtualSize
         then FSections[I].Size := Section.Misc.VirtualSize;
        FSections[I].Characteristics := Section.Characteristics;
        FSections[I].Base := VirtualAlloc(Pointer(NativeUInt(FSections[I].RVA +
          NativeUInt(FImageBase))), FSections[I].Size, MEM_COMMIT, PAGE_READWRITE);
        FillChar(FSections[I].Base^, FSections[I].Size, #0);
        if Section.PointerToRawData <> 0 then
         begin
          Stream.Seek(Section.PointerToRawData, soFrombeginning);
          if Stream.Read(FSections[I].Base^, Section.SizeOfRawData) <> LONGINT(Section.SizeOfRawData) then Exit;
         end;
       end;
      FreeMem(SectionHeaders);
      Result := True;
     end;
  end;

  function ProcessRelocations: Boolean;
  var
    Relocations: PAnsiChar;
    Position: NativeUInt;
    BaseRelocation: PImageBaseRelocation;
    Base: Pointer;
    NumberOfRelocations: LongWord;
    Relocation: PWordArray;
    RelocationCounter: LONGINT;
    RelocationPointer: Pointer;
    RelocationType: LongWord;
  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[
      IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress <> 0 then
     begin
      Result := False;
      Relocations := ConvertPointer(
        ImageNTHeaders.OptionalHeader.DataDirectory[
        IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress);
      Position := 0;
      while Assigned(Relocations) and (Position <
          ImageNTHeaders.OptionalHeader.DataDirectory[
          IMAGE_DIRECTORY_ENTRY_BASERELOC].Size) do
       begin
        BaseRelocation := PImageBaseRelocation(Relocations);
        Base := ConvertPointer(BaseRelocation^.VirtualAddress);
        if not Assigned(Base) then
          Exit;
        NumberOfRelocations :=
          (BaseRelocation^.SizeOfBlock - SizeOf(TImageBaseRelocation)) div
          SizeOf(Word);
        Relocation := Pointer(NativeUInt(NativeUInt(BaseRelocation) +
          SizeOf(TImageBaseRelocation)));
        for RelocationCounter := 0 to NumberOfRelocations - 1 do
         begin
          RelocationPointer :=
            Pointer(NativeUInt(NativeUInt(Base) +
            (Relocation^[RelocationCounter] and $FFF)));
          RelocationType := Relocation^[RelocationCounter] shr 12;
          case RelocationType of
            IMAGE_REL_BASED_ABSOLUTE : ;
            IMAGE_REL_BASED_HIGH : PWord(RelocationPointer)^ :=
                (NativeUInt(
                ((NativeUInt(PWord(RelocationPointer)^ + NativeUInt(FImageBase) -
                ImageNTHeaders.OptionalHeader.ImageBase)))) shr 16) and $FFFF;
            IMAGE_REL_BASED_LOW : PWord(RelocationPointer)^ :=
                NativeUInt(((NativeUInt(PWord(RelocationPointer)^
                + NativeUInt(FImageBase) - ImageNTHeaders.OptionalHeader.
                ImageBase)))) and $FFFF;
            IMAGE_REL_BASED_HIGHLOW : PPointer(RelocationPointer)^ :=
                Pointer((NativeUInt(NativeUInt(PPointer(RelocationPointer)^) +
                NativeUInt(FImageBase) -
                ImageNTHeaders.OptionalHeader.ImageBase)));
            IMAGE_REL_BASED_HIGHADJ : ; // ???
            IMAGE_REL_BASED_MIPS_JMPADDR : ; // Only for MIPS CPUs ;)
           end;
         end;
        Relocations := Pointer(
          NativeUInt(NativeUInt(Relocations) + BaseRelocation^.SizeOfBlock));
        Inc(Position, BaseRelocation^.SizeOfBlock);
       end;
     end;
    Result := True;
  end;

  function ProcessImports: Boolean;
  var
    ImportDescriptor: PImageImportDescriptor;
    ThunkData: PLongWord;
    Name: PAnsiChar;
    DLLImport: PDLLImport;
    DLLFunctionImport: PDLLFunctionImport;
    FunctionPointer: Pointer;
  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[
      IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress <> 0 then
     begin
      ImportDescriptor := ConvertPointer(
        ImageNTHeaders.OptionalHeader.DataDirectory[
        IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
      if Assigned(ImportDescriptor) then
       begin
        SetLength(FImportArray, 0);
        while ImportDescriptor^.Name <> 0 do
         begin
          Name := ConvertPointer(ImportDescriptor^.Name);
          SetLength(FImportArray, Length(FImportArray) + 1);
          LoadExternalLibrary(string(Name));
          DLLImport := @FImportArray[Length(FImportArray) - 1];
          DLLImport^.LibraryName := string(Name);
          DLLImport^.LibraryHandle := GetExternalLibraryHandle(string(Name));
          DLLImport^.Entries := nil;
          if ImportDescriptor^.TimeDateStamp = 0
           then ThunkData := ConvertPointer(ImportDescriptor^.FirstThunk)
           else ThunkData := ConvertPointer(ImportDescriptor^.OriginalFirstThunk);

          while ThunkData^ <> 0 do
           begin
            SetLength(DLLImport^.Entries, Length(DLLImport^.Entries) + 1);
            DLLFunctionImport :=
              @DLLImport^.Entries[Length(DLLImport^.Entries) - 1];
            if (ThunkData^ and IMAGE_ORDINAL_FLAG32) <> 0 then
             begin
              DLLFunctionImport^.NameOrID := niID;
              DLLFunctionImport^.ID := ThunkData^ and IMAGE_ORDINAL_MASK32;
              DLLFunctionImport^.Name := '';
              FunctionPointer :=
                GetProcAddress(DLLImport^.LibraryHandle,
                PChar(ThunkData^ and IMAGE_ORDINAL_MASK32));
             end
            else
             begin
              Name := ConvertPointer(NativeUInt(ThunkData^) +
                IMPORTED_NAME_OFFSET);
              DLLFunctionImport^.NameOrID := niName;
              DLLFunctionImport^.ID := 0;
              DLLFunctionImport^.Name := string(Name);
              if Name = 'GetModuleFileNameA'
               then FunctionPointer := @GetModuleFileNameA else
              if Name = 'GetModuleFileNameW'
               then FunctionPointer := @GetModuleFileNameW
               else FunctionPointer := GetProcAddress(DLLImport^.LibraryHandle, Name);
             end;
            PPointer(Thunkdata)^ := FunctionPointer;
            Inc(ThunkData);
           end;
          Inc(ImportDescriptor);
         end;
       end;
     end;
    Result := True;
  end;

  function ProtectSections: Boolean;
  var
    I               : Integer;
    Characteristics : LongWord;
    Flags           : LongWord;
  begin
    Result := False;
    if ImageNTHeaders.FileHeader.NumberOfSections > 0 then
     begin
      for I := 0 to ImageNTHeaders.FileHeader.NumberOfSections - 1 do
       begin
        Characteristics := FSections[I].Characteristics;
        Flags := 0;
        if (Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
         begin
          if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
           begin
            if (Characteristics and IMAGE_SCN_MEM_write) <> 0
             then Flags := Flags or PAGE_EXECUTE_READwrite
             else Flags := Flags or PAGE_EXECUTE_READ;
           end else if (Characteristics and IMAGE_SCN_MEM_write) <> 0
            then Flags := Flags or PAGE_EXECUTE_writeCOPY
            else Flags := Flags or PAGE_EXECUTE;
         end else if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
         begin
          if (Characteristics and IMAGE_SCN_MEM_write) <> 0
           then Flags := Flags or PAGE_READWRITE
           else Flags := Flags or PAGE_READONLY;
         end else
        if (Characteristics and IMAGE_SCN_MEM_write) <> 0
         then Flags := Flags or PAGE_WRITECOPY
         else Flags := Flags or PAGE_NOACCESS;
        if (Characteristics and IMAGE_SCN_MEM_not_CACHED) <> 0
         then Flags := Flags or PAGE_NOCACHE;
        VirtualProtect(FSections[I].Base, FSections[I].Size, Flags, OldProtect);
       end;
      Result := True;
     end;
  end;

  function InitializeLibrary: Boolean;
  begin
    Result := False;
    @FDLLProc := ConvertPointer(ImageNTHeaders.OptionalHeader.AddressOfEntryPoint);
    if FDLLProc(HMODULE(FImageBase), DLL_PROCESS_ATTACH, nil)
     then Result := True;
  end;

  function ProcessExports: Boolean;
  var
    I                      : Integer;
    ExportDirectory        : PImageExportDirectory;
    ExportDirectorySize    : LongWord;
    FunctionNamePointer    : Pointer;
    FunctionName           : PAnsiChar;
    FunctionIndexPointer   : Pointer;
    FunctionIndex          : LongWord;
    FunctionPointer        : Pointer;
    ForwarderCharPointer   : PChar;
    Forwarderstring        : string;
    ForwarderLibrary       : string;
    ForwarderLibraryHandle : HINST;

    function ParseStringToNumber(Astring: string): LongWord;
    var
      CharCounter: Integer;
    begin
      Result := 0;
      for CharCounter := 0 to Length(Astring) - 1 do
        {$IFDEF DELPHI12_UP}
        if CharInSet(Astring[CharCounter], ['0'..'9']) then
        {$ELSE}
        if Astring[CharCounter] in ['0'..'9'] then
        {$ENDIF}
          Result := (Result * 10) +
            Byte(Byte(Astring[CharCounter]) - Byte('0'))
        else
          Exit;
    end;

  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[
      IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress <> 0 then
     begin
      FExportTree := TExportTree.Create;
      ExportDirectory := ConvertPointer(
        ImageNTHeaders.OptionalHeader.DataDirectory[
        IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
      if Assigned(ExportDirectory) then
       begin
        ExportDirectorySize :=
          ImageNTHeaders.OptionalHeader.DataDirectory[
          IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
        SetLength(FExportArray, ExportDirectory^.NumberOfNames);
        for I := 0 to ExportDirectory^.NumberOfNames - 1 do
         begin
          FunctionNamePointer := ConvertPointer(NativeUInt(ExportDirectory^.AddressOfNames));
          FunctionNamePointer := ConvertPointer(PNativeUIntArray(FunctionNamePointer)^[I]); // TODO: problematic!!! PLongWordArray
          FunctionName := FunctionNamePointer;
          FunctionIndexPointer := ConvertPointer(NativeUInt(ExportDirectory^.AddressOfNameOrdinals));
          FunctionIndex := PWordArray(FunctionIndexPointer)^[I];
          FunctionPointer := ConvertPointer(NativeUInt(ExportDirectory^.AddressOfFunctions));
          FunctionPointer := ConvertPointer(PNativeUIntArray(FunctionPointer)^[FunctionIndex]); // TODO: problematic!!! PLongWordArray
          FExportArray[I].Name := string(FunctionName);
          FExportArray[I].Index := FunctionIndex;
          if (NativeUInt(ExportDirectory) < NativeUInt(FunctionPointer)) and
            (NativeUInt(FunctionPointer) <
            (NativeUInt(ExportDirectory) + ExportDirectorySize)) then
           begin
            ForwarderCharPointer := FunctionPointer;
            Forwarderstring := ForwarderCharPointer;
            while ForwarderCharPointer^ <> '.' do Inc(ForwarderCharPointer);
            ForwarderLibrary := COPY(Forwarderstring, 1, POS('.', Forwarderstring) - 1);
            LoadExternalLibrary(ForwarderLibrary);
            ForwarderLibraryHandle := GetExternalLibraryHandle(ForwarderLibrary);
            if ForwarderCharPointer^ = '#' then
             begin
              Inc(ForwarderCharPointer);
              Forwarderstring := ForwarderCharPointer;
              ForwarderCharPointer := ConvertPointer(ParsestringToNumber(Forwarderstring));
              Forwarderstring := ForwarderCharPointer;
             end
            else
             begin
              Forwarderstring := ForwarderCharPointer;
              FExportArray[I].FunctionPointer := GetProcAddress(ForwarderLibraryHandle, PChar(Forwarderstring));
             end;
           end else FExportArray[I].FunctionPointer := FunctionPointer;
          FExportTree.Add(FExportArray[I].Name, FExportArray[I].FunctionPointer);
         end
       end;
     end;
    Result := True;
  end;

begin
  Result := False;
  if Assigned(Stream) then
   begin
    Stream.Seek(0, soFromBeginning);
    if Stream.Size > 0 then
      if ReadImageHeaders then
        if InitializeImage then
          if ReadSections then
            if ProcessRelocations then
              if ProcessImports then
                if ProtectSections then
                  if InitializeLibrary then
                    if ProcessExports then
                      Result := True;
   end;
end;



function TDLLLoader.Unload: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if @FDLLProc <> nil then
   begin
    FDLLProc(NativeUInt(FImageBase), DLL_PROCESS_DETACH, nil);
    FDLLProc := nil;
   end;

  for I := 0 to Length(FSections) - 1 do
    if Assigned(FSections[I].Base) then
      VirtualFree(FSections[I].Base, 0, MEM_RELEASE);

  SetLength(FSections, 0);
  for I := 0 to Length(FExternalLibraryArray) - 1 do
   begin
    FExternalLibraryArray[I].LibraryName := '';
    FreeLibrary(FExternalLibraryArray[I].LibraryHandle);
   end;

  SetLength(FExternalLibraryArray, 0);
  for I := 0 to Length(FImportArray) - 1 do
   begin
    for J := 0 to Length(FImportArray[I].Entries) - 1 do
      FImportArray[I].Entries[J].Name := '';
    SetLength(FImportArray[I].Entries, 0);
   end;

  SetLength(FImportArray, 0);
  for I := 0 to Length(FExportArray) - 1 do
    FExportArray[I].Name := '';

  SetLength(FExportArray, 0);
  VirtualFree(FImageBase, 0, MEM_RELEASE);
  if Assigned(FExportTree) then
   begin
    FExportTree.Destroy;
    FExportTree := nil;
   end;
end;

function TDLLLoader.FindExport(FunctionName: string): Pointer;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(FExportTree) then
    FExportTree.Find(FunctionName, Result)
  else
    for I := 0 to Length(FExportArray) - 1 do
      if FExportArray[I].Name = FunctionName then
       begin
        Result := FExportArray[I].FunctionPointer;
        Exit;
       end;
end;

function TDLLLoader.FindExportPerIndex(FunctionIndex: Integer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for i := 0 to Length(FExportArray) - 1 do
    if FExportArray[i].Index = FunctionIndex then
     begin
      Result := FExportArray[i].FunctionPointer;
      Exit;
     end;
end;

function TDLLLoader.GetExportList: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Length(FExportArray) - 1 do
    Result.Add(FExportArray[I].Name);
  Result.Sort;
end;

end.
