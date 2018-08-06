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

unit DAV_DLLResources;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$ELSE} Windows, {$ENDIF} Classes, SysUtils, ConTnrs, ImageHlp;

type
  TPEModule = class;
  TResourceDetails = class;
  TResourceDetailsClass = class of TResourceDetails;

  {$IFDEF DELPHI10_UP} {$region 'TResourceModule class'} {$ENDIF}

  ///////////////////////////
  // TResourceModule class //
  ///////////////////////////

  TResourceModule = class
  private
    FDirty  : Boolean;
    FBackup : Boolean;
    function GetDirty: Boolean;
  protected
    function GetResourceCount: Integer; virtual; abstract;
    function GetResourceDetails(idx: Integer): TResourceDetails;
      virtual; abstract;
    procedure ClearDirty;
  public
    procedure DeleteResource(idx: Integer); virtual;
    procedure InsertResource(idx: Integer; Details: TResourceDetails); virtual; abstract;
    function AddResource(Details: TResourceDetails): Integer; virtual;
    function IndexOfResource(Details: TResourceDetails): Integer; virtual; abstract;
    function GetUniqueResourceName(const tp: WideString): WideString;

    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;

    procedure SaveToFile(const FileName: TFileName); virtual;
    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure SortResources; virtual; abstract;

    function FindResource(const tp, Name: WideString; ALanguage: Integer): TResourceDetails;

    property ResourceCount: Integer read GetResourceCount;
    property ResourceDetails[idx: Integer]: TResourceDetails read GetResourceDetails;
    property Dirty: Boolean read GetDirty write FDirty;
    property Backup: Boolean read FBackup write FBackup;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TResourceDetails class'} {$ENDIF}
  ////////////////////////////
  // TResourceDetails class //
  ////////////////////////////

  TResourceDetails = class
  private
    FParent                : TResourceModule;
    FData                  : TMemoryStream;
    FCodePage              : Integer;
    FResourceLanguage      : LCID;
    FResourceName          : WideString;
    FResourceType          : WideString;

    FMemoryFlags           : Word;          // Resource memory flags
    FDataVersion, FVersion : DWORD;         // Resource header version info
    FCharacteristics       : DWORD;
    FDirty                 : Boolean;
    FTag                   : Integer;
    procedure SetResourceType(const Value: WideString);
                                           // Resource header Characteristics
  protected
    constructor Create(AParent: TResourceModule; ALanguage: Integer; const AName, AType: WideString; ASize: Integer; AData: Pointer); virtual;
    procedure InitNew; virtual;
    procedure SetResourceName(const Value: WideString); virtual;
    class function SupportsRCData(const AName: AnsiString; Size: Integer; Data: Pointer): Boolean; virtual;
    class function SupportsData(Size: Integer; Data: Pointer): Boolean; virtual;
  public
    class function CreateResourceDetails(AParent: TResourceModule; ALanguage: Integer; const AName, AType: WideString; ASize: Integer; AData: Pointer): TResourceDetails;
    class function GetBaseType: WideString; virtual;

    constructor CreateNew(AParent: TResourceModule; ALanguage: Integer; const AName: WideString); virtual;
    destructor Destroy; override;
    procedure BeforeDelete; virtual;

    procedure ChangeData(newData: TMemoryStream); virtual;

    property Parent: TResourceModule read FParent;
    property Data: TMemoryStream read FData;
    property ResourceName: WideString read FResourceName write SetResourceName;
    property ResourceType: WideString read FResourceType write SetResourceType;
    property ResourceLanguage: LCID read FResourceLanguage write FResourceLanguage;

    property CodePage: Integer read FCodePage write FCodePage;
    property Characteristics: DWORD read FCharacteristics write FCharacteristics;
    property Version: DWORD read FVersion write FDataVersion;
    property DataVersion: DWORD read FDataVersion write FDataVersion;
    property MemoryFlags: WORD read FMemoryFlags write FMemoryFlags;

    property Dirty: Boolean read FDirty write FDirty;
    property Tag: Integer read FTag write FTag;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TImageSection class'} {$ENDIF}
  /////////////////////////
  // TImageSection class //
  /////////////////////////

  TImageSection = class
  private
    FParent: TPEModule;
    FSectionHeader: TImageSectionHeader;
    FRawData: TMemoryStream;
    FUninitializedDataSize: Integer;

    function GetSectionName: AnsiString;
  public
    constructor Create(AParent: TPEModule;
      const AHeader: TImageSectionHeader; rawData: pointer);
    destructor Destroy; override;
    property Parent: TPEModule read FParent;

    property SectionName: AnsiString read GetSectionName;
    property SectionHeader: TImageSectionHeader read FSectionHeader;
    property RawData: TMemoryStream read FRawData;
  end;

  TImageImportDirectory = packed record
    Characteristics : DWORD; // This is an RVA to a list of pointers. Each of these points to there function name
    TimeDateStamp   : DWORD; // The time/date stamp indicating when the file was built
    ForwarderChain  : DWORD; // This field relates to forwarding. Forwarding involves one DLL sending on references to one of its functions to another DLL
    Name            : DWORD; // This is an RVA to a NULL-terminated ASCII string containing the imported DLL's name
    FirstThunk      : DWORD; // Another RVA to a list pointers. Each of these points to their function name
  end;
  PImageImportDirectory = ^TImageImportDirectory;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TPEModule class'} {$ENDIF}
  /////////////////////
  // TPEModule class //
  /////////////////////

  TPEModule = class(TResourceModule)
  private
    FDOSHeader      : TImageDosHeader;
    FCOFFHeader     : TImageFileHeader;
    {$IFDEF CPU64}
    FOptionalHeader : PImageOptionalHeader64;
    {$ELSE}
    FOptionalHeader : PImageOptionalHeader32;
    {$ENDIF}
    FSectionList    : TObjectList;           // List of TImageSection objects
    FDOSStub        : TMemoryStream;
    FCommentBlock   : PAnsiChar;
    FCommentSize    : Integer;
    FEndComment     : PAnsiChar;
    FEndCommentSize : Integer;

    function GetOptionalHeader: TImageOptionalHeader;
    function GetImageSection(index: Integer): TImageSection;
    function GetImageSectionCount: Integer;
    function GetDataDictionary(index: Integer): PImageDataDirectory;
    function GetDataDictionaryCount: Integer;
    function GetDOSHeader: TImageDosHeader;
    function GetCOFFHeader: TImageFileHeader;
    function GetExportCount: Integer;
    function GetImportCount: Integer;
    function GetResourceSection(var Offset: Integer): TImageSection;
    function GetImportSection(var Offset: Integer): TImageSection;
    function GetExportSection(var Offset: Integer): TImageSection;
    function GetImport(idx: Integer): PImageImportDirectory;
    function GetImportSectionData: PAnsiChar;
    function GetExportSectionData: PAnsiChar;

  protected
    procedure Decode(memory: pointer; exeSize: Integer); virtual;
    procedure Encode; virtual;
    {$IFDEF CPU64}
    property OptionalHeaderPtr: PImageOptionalHeader64 read FOptionalHeader;
    {$ELSE}
    property OptionalHeaderPtr: PImageOptionalHeader32 read FOptionalHeader;
    {$ENDIF}
    function FindDictionaryEntrySection(entryNo: Integer; var Offset: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property DOSHeader: TImageDosHeader read GetDOSHeader;
    property COFFHeader: TImageFileHeader read GetCOFFHeader;
    property OptionalHeader: TImageOptionalHeader read GetOptionalHeader;

    property ImageSectionCount: Integer read GetImageSectionCount;
    property ImageSection[index: Integer]: TImageSection read GetImageSection;

    property DataDictionaryCount: Integer read GetDataDictionaryCount;
    property DataDictionary[index: Integer]: PImageDataDirectory read GetDataDictionary;

    property ImportCount: Integer read GetImportCount;
    property Import[idx: Integer]: PImageImportDirectory read GetImport;
    property ImportSectionData: PAnsiChar read GetImportSectionData;
    property ExportSectionData: PAnsiChar read GetExportSectionData;
    property ExportCount: Integer read GetExportCount;

    procedure GetExportDetails(idx: Integer; var Name: AnsiString; var Ordinal: DWORD);

    procedure LoadFromStream(s: TStream); override;
    procedure LoadFromFile(const Name: TFileName); override;

    procedure SaveToStream(Stream: TStream); override;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TResourceDirectory records'} {$ENDIF}
  ////////////////////////////////////
  // TResourceDirectoryTable record //
  ////////////////////////////////////

  TResourceDirectoryTable = packed record
    Characteristics : DWORD; // Resource flags, reserved for future use; currently set to zero.
    TimeDateStamp   : DWORD; // Time the resource data was created by the resource compiler.
    VersionMajor    : WORD;  // Major version number, set by the user.
    VersionMinor    : WORD;  // Minor version number.
    CNameEntries    : WORD;  // Number of directory entries, immediately following the Table, that use strings to identify Type, Name, or Language (depending on the level of the Table).
    CIDEntries      : WORD;  // Number of directory entries, immediately following the Name entries, that use numeric identifiers for Type, Name, or Language.
  end;
  PResourceDirectoryTable = ^TResourceDirectoryTable;

  //////////////////////
  // TPEModule record //
  //////////////////////

  TResourceDirectoryEntry = packed record
    Name : DWORD; // RVA Address of integer or string that gives the Type, Name, or Language identifier, depending on level of Table.
    RVA  : DWORD; // RVA High bit 0. Address of a Resource Data Entry (a Leaf).
                  // RVA High bit 1. Lower 31 bits are the address of another Resource Directory Table (the next level down).
  end;
  PResourceDirectoryEntry = ^TResourceDirectoryEntry;

  ////////////////////////////////////
  // TResourceDirectoryEntry record //
  ////////////////////////////////////

  TResourceDataEntry = packed record
    OffsetToData : DWORD;
    Size         : DWORD;
    CodePage     : DWORD;
    Reserved     : DWORD;
  end;
  PResourceDataEntry = ^TResourceDataEntry;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TPEResourceModule class'} {$ENDIF}
  /////////////////////////////
  // TPEResourceModule class //
  /////////////////////////////

  TPEResourceModule = class(TPEModule)
  private
    FDetailList: TObjectList;             // List of TResourceDetails objects

  protected
    procedure Decode(memory: pointer; exeSize: Integer); override;
    procedure Encode; override;
    function GetResourceCount: Integer; override;
    function GetResourceDetails(idx: Integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ResourceCount: Integer read GetResourceCount;
    property ResourceDetails[idx: Integer]: TResourceDetails read GetResourceDetails;
    procedure DeleteResource(resourceNo: Integer); override;
    procedure InsertResource(idx: Integer; Details: TResourceDetails); override;
    function AddResource(Details: TResourceDetails): Integer; override;
    function IndexOfResource(Details: TResourceDetails): Integer; override;
    procedure SortResources; override;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  EPEException = class(Exception);

  // Global function definitions
  function ResourceWideCharToStr(var wstr: PWideChar; codePage: Integer): AnsiString;
  function ResourceWideCharToWideStr(var wstr: PWideChar): WideString;
  procedure ResourceStrToWideChar(const s: AnsiString; var p: PWideChar; codePage: Integer);
  procedure ResourceWideStrToWideChar(const s: WideString; var p: PWideChar);
  function ResourceNameToInt(const s: AnsiString): Integer;
  function WideResourceNameToInt(const s: WideString): Integer;
  function CompareDetails(p1, p2: Pointer): Integer;

implementation

{$IFDEF DELPHI10_UP} {$region 'Local Declarations and Functions'} {$ENDIF}

{$IFDEF DELPHI14_UP}
//uses
//  AnsiStrings;
{$ENDIF}

resourcestring
  RCStrNoBaseType = 'Can''t register resource details class with no base type';
  RCStrNoStreaming = 'Module doesn''t support streaming';
  RCStrInvalidDOSSignature = 'Invalid DOS signature';
  RCStrInvalidCOFFSignature = 'Invalid COFF signature';
  RCStrInvalidOptionalHeader = 'Invalid Windows Image';
  RCStrBadDictionaryIndex = 'Index exceeds data dictionary count';
  RCStrBadLangID = 'Unsupported non-integer language ID in resource';
  RCStrEncode = 'Error encoding module';

type
  TResourceNode = class
    Count: Integer;
    Nodes: array of record
      Id: AnsiString;
      IntID: Boolean;
      case Leaf: Boolean of
        False: (Next: TResourceNode);
        True: (Data: TMemoryStream;
          CodePage: DWORD)
    end;

    constructor Create(const AType, AName: AnsiString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateNameNode(const AName: AnsiString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateLangNode(ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure Add(const AType, AName: AnsiString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddName(const AName: AnsiString; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddLang(ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    function IsID(idx: Integer): Boolean;
    destructor Destroy; override;
  end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure ResourceWideCharToStr ()                                        //
//                                                                            //
//  Convert Pascal-style WideChar array to a string                           //
//                                                                            //
//  Parameters:                                                               //
//    WStr : PWChar             The characters                                //
//    codePage : Integer        Code page to use in conversion                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function ResourceWideCharToStr(var wstr: PWideChar; codePage: Integer): AnsiString;
var
  Len: word;
begin
  Len := word(wstr^);
  SetLength(Result, Len);
  Inc(wstr);
  WideCharToMultiByte(codePage, 0, WStr, Len, PAnsiChar(Result),
    Len + 1, nil, nil);
  Inc(wstr, Len);
  Result := PAnsiChar(Result);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure ResourceWideCharToWideStr ()                                    //
//                                                                            //
//  Convert Pascal-style WideChar array to a WideString                       //
//                                                                            //
//  Parameters:                                                               //
//    WStr : PWChar             The characters                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function ResourceWideCharToWideStr(var wstr: PWideChar): WideString;
var
  Len: word;
begin
  Len := word(wstr^);
  SetLength(Result, Len);
  Inc(wstr);
  Move(wstr^, PWideChar(Result)^, Len * SizeOf(WideChar));
  Inc(wstr, Len);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// procedure ResourceStrToWideChar ()                                         //
//                                                                            //
// Convert a string to a Pascal style Wide char array                         //
//                                                                            //
// Parameters:                                                                //
//   s : string                The string                                     //
//   var p : PWideChar         [in]  Points to the start of the receiving buf //
//                             [out] Points after the characters.             //
//   codePage : Integer        Code page to use in conversion                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
procedure ResourceStrToWideChar(const s: AnsiString; var p: PWideChar;
  codePage: Integer);
var
  Buffer: PWideChar;
  Len, Size: word;
begin
  Len := Length(s);
  Size := (Length(s) + 1) * SizeOf(WideChar);
  GetMem(Buffer, Size);
   try
    MultiByteToWideChar(codePage, 0, PAnsiChar(s), -1, Buffer, Size);
    p^ := WideChar(Len);
    Inc(p);
    Move(Buffer^, p^, Len * SizeOf(WideChar));
    Inc(p, Len)
   finally
    FreeMem(Buffer)
   end
end;

////////////////////////////////////////////////////////////////////////////////
// procedure ResourceWideStrToWideChar ()                                     //
//                                                                            //
// Convert a wide string to a Pascal style Wide char array                    //
//                                                                            //
// Parameters:                                                                //
//   s : string                The string                                     //
//   var p : PWideChar         [in]  Points to the start of the receiving buf //
//                             [out] Points after the characters.             //
////////////////////////////////////////////////////////////////////////////////
procedure ResourceWideStrToWideChar(const s: WideString; var p: PWideChar);
var
  Len: word;
begin
  Len := Length(s);
  p^ := WideChar(Len);
  Inc(p);
  Move(PWideChar(s)^, p^, Len * SizeOf(WideChar));
  Inc(p, Len)
end;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure ResourceNameToInt                                               //
//                                                                            //
//  Get integer value of resource name (or type).  Return -1 if it's not      //
//  numeric.                                                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
function ResourceNameToInt(const s: AnsiString): Integer;
var
  isNumeric : Boolean;
  i         : Integer;
begin
 isNumeric := Length(s) > 0;
 for i := 1 to Length(s) do
  if not (s[i] in ['0'..'9']) then
   begin
    isNumeric := False;
    Break;
   end;

 if isNumeric
  then Result := StrToInt(string(s))
  else Result := -1
end;

function WideResourceNameToInt(const s: WideString): Integer;
begin
 Result := ResourceNameToInt(AnsiString(s));
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function CompareDetails                                                   //
//                                                                            //
//  'Compare' function used when sorting resources.  p1 and p2 must be        //
//  TResourceDetails references.  Returns > 0 if Details at p1 are >          //
//  Details at p2.                                                            //
//                                                                            //
//   *  Compare resource types.  If they match then compare names.            //
//   *  'Integer' ids or names must come *after* non integer ids or names.    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function CompareDetails(p1, p2: Pointer): Integer;
var
  d1: TResourceDetails;
  d2: TResourceDetails;
  i1, i2: Integer;
begin
  d1 := TResourceDetails(p1);
  d2 := TResourceDetails(p2);

  i1 := ResourceNameToInt(AnsiString(d1.ResourceType));
  i2 := ResourceNameToInt(AnsiString(d2.ResourceType));

  if i1 >= 0 then
   if i2 >= 0
    then Result := i1 - i2 // Compare two integer ids
    else Result := 1       // id1 is int, so it's greater than non-int id2
  else
   if i2 >= 0
    then Result := -1      // id2 is int, so it's less than non-int id1
    else Result := CompareText(d1.ResourceType, d2.ResourceType); // Compare two string resource ids

  if Result = 0 then        // If they match, do the same with the names
   begin
    i1 := ResourceNameToInt(AnsiString(d1.ResourceName));
    i2 := ResourceNameToInt(AnsiString(d2.ResourceName));

    if i1 >= 0 then
     if i2 >= 0
      then Result := i1 - i2
      else Result := 1
    else
     if i2 >= 0
      then Result := -1
      else Result := CompareText(d1.ResourceName, d2.ResourceName)
   end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function LCIDTOCodePage                                                   //
//                                                                            //
//  Get the ANSI code page for a given language Id                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result := StrToIntDef(Buffer, GetACP);
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TResourceDetails implementation'} {$ENDIF}

{ TResourceDetails }

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.BeforeDelete                                             //
//                                                                            //
//  Can override this to clear up before deleting.  Eg. deleting an           //
//  icon removes it from the icon group it's in.  Deleting an icon group      //
//  removes the individual icon resources, etc.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TResourceDetails.BeforeDelete;
begin
  // Stub
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.ChangeData                                               //
//                                                                            //
//  Change all the data.  Handy for implementing 'undo', etc.                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TResourceDetails.ChangeData(newData: TMemoryStream);
begin
  FData.Clear;
  FData.CopyFrom(newData, 0);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.Create                                                   //
//                                                                            //
//  Raw - protected - constructor for resource Details.                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: WideString; ASize: Integer;
  AData: Pointer);
begin
  FParent := AParent;
  FResourceLanguage := ALanguage;
  FCodePage := LCIDToCodePage(FResourceLanguage);
  FResourceName := AName;
  FResourceType := AType;
  FData := TMemoryStream.Create;
  if AData <> nil
   then FData.Write(AData^, ASize)
   else InitNew;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.CreateNew                                                //
//                                                                            //
//  Constructor to be used when adding new resources to a module.             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TResourceDetails.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const aName: WideString);
begin
  FParent := AParent;
  FResourceLanguage := ALanguage;
  FCodePage := LCIDToCodePage(FResourceLanguage);
  FResourceName := AName;
  FResourceType := GetBaseType;
  if Assigned(AParent)
   then AParent.AddResource(Self);
  FData := TMemoryStream.Create;
  InitNew
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.CreateResourceDetails                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TResourceDetails.CreateResourceDetails(
  AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: WideString; ASize: Integer;
  AData: pointer): TResourceDetails;
begin
 Result := TResourceDetails.Create(AParent, ALanguage, AName, AType, ASize, AData)
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.Destroy                                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

destructor TResourceDetails.Destroy;
begin
 FreeAndNil(FData);
 inherited;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.GetBaseType                                              //
//                                                                            //
//  Return the base type for the resource Details.  This is overridden        //
//  in derived classes.                                                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TResourceDetails.GetBaseType: WideString;
begin
  Result := '0';
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.InitNew                                                  //
//                                                                            //
//  Override this to initialize a new resource being added to a module.       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TResourceDetails.InitNew;
begin
// Stub
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.SetResourceName                                          //
//                                                                            //
//  Set the resource name.                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TResourceDetails.SetResourceName(const Value: WideString);
begin
  if FResourceName <> Value then
   begin
    FResourceName := Value;
    FDirty := True
   end
end;

procedure TResourceDetails.SetResourceType(const Value: WideString);
begin
  if FResourceType <> Value then
   begin
    FResourceType := Value;
    FDirty := True
   end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.SupportsData                                             //
//                                                                            //
//  Can be overridden to support a custom resource class, where you can       //
//  determine the custom class from the data - eg. RIFF data, etc.            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TResourceDetails.SupportsData(Size: Integer;
  Data: Pointer): Boolean;
begin
  Result := False; // stub
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceDetails.SupportsData                                             //
//                                                                            //
//  Can be overridden to support RC data where you can determine the          //
//  type from the data and name - eg. the Delphi splash screen JPEG           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

class function TResourceDetails.SupportsRCData(const AName: AnsiString;
  Size: Integer; Data: Pointer): Boolean;
begin
  Result := False; // stub
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TResourceModule implementation'} {$ENDIF}
{ TResourceModule }

function TResourceModule.AddResource(Details: TResourceDetails): Integer;
begin
  Result := -1
 // Stub
end;

procedure TResourceModule.ClearDirty;
var
  Index : Integer;
begin
 FDirty := False;
 for Index := 0 to ResourceCount - 1
  do ResourceDetails[Index].Dirty := False
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceModule.DeleteResource                                            //
//                                                                            //
//  Must be overridden to remove the resource Details object from             //
//  wherever it's stored.  The overriding method must call                    //
//  inherited                                                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TResourceModule.DeleteResource(idx: Integer);
begin
  FDirty := True;
  ResourceDetails[idx].BeforeDelete;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceModule.FindResource                                              //
//                                                                            //
//  Find a resource with a given type/name                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TResourceModule.FindResource(const tp, Name: WideString;
  ALanguage: Integer): TResourceDetails;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ResourceCount - 1 do
    if (ResourceDetails[i].FResourceType = tp) and
      (ResourceDetails[i].FResourceName = Name) and
      (Integer(ResourceDetails[i].FResourceLanguage) = ALanguage) then
     begin
      Result := ResourceDetails[i];
      break
     end;

  if not Assigned(Result) then
    for i := 0 to ResourceCount - 1 do
      if (ResourceDetails[i].FResourceType = tp) and
        (ResourceDetails[i].FResourceName = Name) and
        (ResourceDetails[i].FResourceLanguage = 0) then
       begin
        Result := ResourceDetails[i];
        break
       end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceModule.GetDirty                                                  //
//                                                                            //
//  Returns true if the module or it's resources are 'dirty'                  //
//                                                                            //
//  nb. FDirty is only set if resources have been deleted.                    //
//      After adding a resource make sure the resource's Dirty is set to      //
//      true.                                                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TResourceModule.GetDirty: Boolean;
var
  i: Integer;
begin
  Result := FDirty;
  if not FDirty then
    for i := 0 to ResourceCount - 1 do
      if ResourceDetails[i].Dirty then
       begin
        Result := True;
        break
       end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceModule.GetUniqueResourceName                                     //
//                                                                            //
//  Generate a unique resource name for a given type.  Names start at         //
//  1 (though string lists downgrade that to '0')                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TResourceModule.GetUniqueResourceName(
  const tp: WideString): WideString;
var
  i       : Integer;
  n, n1   : Integer;
  Details : TResourceDetails;
begin
  n := 0;

  for i := 0 to ResourceCount - 1 do
   begin
    Details := ResourceDetails[i];
    if Details.ResourceType = tp then
     begin
      n1 := ResourceNametoInt(AnsiString(Details.ResourceName));
      if n1 > n then n := n1
     end
   end;

  Result := IntToStr(n + 1);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceModule.LoadFromFile                                              //
//                                                                            //
//  Load from file.  This can be overriden but usually isn't as it            //
//  relies on LoadFromStream, which must be.                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TResourceModule.LoadFromFile(const FileName: TFileName);
var
  s: TFileStream;
begin
 s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
 try
  LoadFromStream(s);
 finally
  s.Free
 end;
end;

procedure TResourceModule.LoadFromStream(stream: TStream);
begin
 raise Exception.Create(RCStrNoStreaming);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  TResourceModule.SaveToFile                                                //
//                                                                            //
//  Save to file.  This can be overriden but usually isn't as it              //
//  relies on SaveToStream, which must be.                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TResourceModule.SaveToFile(const FileName: TFileName);
var
  s          : TFileStream;
  BackupName : TFileName;
begin
 // eventually do a backup
 if FBackup then
  begin
   ChangeFileExt(BackupName, '~' + ExtractFileExt(FileName));
   if FileExists(BackupName) then DeleteFile(BackupName);
   RenameFile(FileName, BackupName);
  end;

 try
  s := TFileStream.Create(FileName, fmCreate);
   try
    SaveToStream(s);
    ClearDirty;
   finally
    FreeAndNil(s)
   end
 except
  // Failed
  DeleteFile(FileName);

  // eventually rename old file back.
  if FBackup
   then RenameFile(BackupName, FileName);
  raise
 end
end;

procedure TResourceModule.SaveToStream(Stream: TStream);
begin
  raise Exception.Create(RCStrNoStreaming);
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TPEModule implementation'} {$ENDIF}


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  constructor TPEModule.Create                                              //
//                                                                            //
//  Constructor for TPEModule instance.  Create empty Section list            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TPEModule.Create;
begin
  inherited Create;
  FSectionList := TObjectList.Create;
  FDOSStub := TMemoryStream.Create;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure TPEModule.Decode                                                // 
//                                                                            //
//  Decode the PE file.  Load the DOS header, the COFF header and the         //
//  'optional' header, then load each Section into FSectionList               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEModule.Decode(Memory: pointer; exeSize: Integer);
var
  Offset: LongInt;
  i: Integer;
  sectionHeader: PImageSectionHeader;
  commentOffset: Integer;
begin
  FSectionList.Clear;
                                // Check it's really a PE file.
  if PWORD(Memory)^ <> IMAGE_DOS_SIGNATURE then
    raise EPEException.Create(RCStrInvalidDOSSignature);

                                // Load the DOS header
  FDOSHeader := PImageDosHeader(Memory)^;

  Offset := FDOSHeader._lfanew;
  FDOSStub.Write((PAnsiChar(Memory) + SizeOf(FDOSHeader))^,
    FDOSHeader._lfanew - SizeOf(FDOSHeader));

                                // Check the COFF signature
  if PDWORD(PAnsiChar(Memory) + Offset)^ <> IMAGE_NT_SIGNATURE then
    raise EPEException.Create(RCStrInvalidCOFFSignature);

                                // Load the COFF header
  Inc(Offset, SizeOf(DWORD));
  FCOFFHeader := PImageFileHEader(PAnsiChar(Memory) + Offset)^;

  Inc(Offset, SizeOf(FCOFFHeader));

                                // Check the Optional Header signature.  nb
                                // the optional header is compulsory for
                                // 32 bit windows modules!
  if PWORD(PAnsiChar(Memory) + Offset)^ <> IMAGE_NT_OPTIONAL_HDR_MAGIC then
    raise EPEException.Create(RCStrInvalidOptionalHeader);

                                // Save the 'optional' header
  ReallocMem(FOptionalHeader, FCOFFHeader.SizeOfOptionalHeader);
  Move((PAnsiChar(Memory) + Offset)^, FOptionalHeader^,
    FCOFFHeader.SizeOfOptionalHeader);

  Inc(Offset, FCOFFHeader.SizeOfOptionalHeader);

  sectionHeader := PImageSectionHeader(PAnsiChar(memory) + Offset);
  commentOffset := Offset + FCOFFHeader.NumberOfSections *
    SizeOf(TImageSectionHeader);

// Save padding between the end of the Section headers, and the start of the
// 1st Section.  TDump reports this as 'comment', and it seems to be important
// to MS clock.exe...

  FCommentSize := Integer(sectionHeader^.PointerToRawData) - commentOffset;

  if FCommentSize > 0 then
   begin
    GetMem(FCommentBlock, FCommentSize);
    Move((PAnsiChar(memory) + commentOffset)^, FCommentBlock^, FCommentSize)
   end;
                                // Now save each image Section in the FSectionList
  for i := 0 to FCOFFHeader.NumberOfSections - 1 do
   begin
    sectionHeader := PImageSectionHeader(PAnsiChar(memory) + Offset);
    FSectionList.Add(TImageSection.Create(self, sectionHeader^,
      PAnsiChar(memory) + sectionHeader^.PointertoRawData));
    Inc(Offset, SizeOf(TImageSectionHeader));
   end;

  i := sectionHeader^.PointerToRawData + sectionHeader^.SizeOfRawData;

// Save the padding between the last Section and the end of the file.
// This appears to hold debug info and things ??

  FEndCommentSize := exeSize - i;
  if FEndCommentSize > 0 then
   begin
    GetMem(FEndComment, FEndCommentSize);
    Move((PAnsiChar(memory) + i)^, FEndComment^, FEndCommentSize)
   end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  destructor TPEModule.Destroy                                              //
//                                                                            //
//  Destructor for TPEModule instance.                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

destructor TPEModule.Destroy;
begin
  ReallocMem(FOptionalHeader, 0);
  FreeAndNil(FSectionList);
  FreeAndNil(FDOSStub);
  ReallocMem(FCommentBlock, 0);
  ReallocMem(FEndComment, 0);
  inherited;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure TPEModule.Encode                                                //
//                                                                            //
//  Fix up the data prior to writing to stream.                               //
//                                                                            //
//  Ensure that the headers match what we've got...                           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEModule.Encode;
var
  Offset: DWORD;
  i: NativeInt;
  Section: TImageSection;
  Align: Integer;
  AddrAlign: Integer;
  Address: NativeInt;
  AlignedSize, AddrAlignedSize: Integer;
  CodeSize, IDataSize, UDataSize, iSize: Integer;
begin
  CodeSize := 0;
  IDataSize := 0;
  UDataSize := 0;

  // Use the DOS stub from their .EXE
  FDOSHeader._lfanew := SizeOf(FDOSHeader) + FDOSStub.Size;

  // Fixup sections count
  FCOFFHeader.NumberOfSections := FSectionList.Count;

  iSize := FDOSHeader._lfanew +  // File Offset for start of sections
    SizeOf(DWORD) +              // NT signature
    SizeOf(FCOFFHeader) +
    FCOFFHeader.SizeOfOptionalHeader + FSectionList.Count *
    SizeOf(TImageSectionHeader);

  Offset := iSize + FCommentSize;

  Align := FOptionalHeader^.FileAlignment;
  AddrAlign := FOptionalHeader^.SectionAlignment;

  Address := AddrAlign;
  Offset := DWORD((NativeInt(Offset) + Align - 1) div Align * Align);

  // First Section starts at $1000 (when loaded)
  // and at 'offset' in file.

  FOptionalHeader^.SizeOfHeaders :=
    DWORD((integer(iSize) + Align - 1) div Align * Align);

  FOptionalHeader^.BaseOfCode := $ffffffff;
  FOptionalHeader^.CheckSum := 0;
               // Calculate it during 'SaveToStream' when
                                                // we've got all the info.

  iSize := DWORD((integer(iSize) + AddrAlign - 1) div
    AddrAlign * AddrAlign);

  for i := 0 to FSectionList.Count - 1 do
      // Recalculate the Section offsets
   begin
    Section := TImageSection(FSectionList[i]);

    Section.FSectionHeader.PointerToRawData := Offset;
    Section.FSectionHeader.VirtualAddress := Address;

// Virtual Size is Size of data in memory, and is not padded to an 'alignment'.

// SizeOfRawData is Size of data in file, padded to (file) alignment.

// 1.  If VirtualSize < SizeOfRawData, that's simply because the raw data is aligned, and virt data isn't.

// 2.  If VirtualSize > SizeOfRawData, the additional memory is filled with Zeros when it's loaded.

// Because SizeOfRawData is padded it's impossible to tell how much Virtual Memory is really required.

// We do our best by saving the original difference in '2.' above in fUninitializeDataSize

    Section.FSectionHeader.Misc.VirtualSize :=
      Section.FRawData.Size + Section.FUninitializedDataSize;
    Section.FSectionHeader.SizeOfRawData :=
      (Section.FRawData.Size + Align - 1) div Align * Align;

    AlignedSize := (Integer(Section.FSectionHeader.Misc.VirtualSize) +
      Align - 1) div Align * Align;
    addrAlignedSize := (Integer(Section.FSectionHeader.Misc.VirtualSize) +
      AddrAlign - 1) div AddrAlign * AddrAlign;

    if (Section.FSectionHeader.Characteristics and
      IMAGE_SCN_MEM_EXECUTE) <> 0 then
     begin
      Inc(CodeSize, AlignedSize);
      if DWORD(Address) < FOptionalHeader^.BaseOfCode then
        FOptionalHeader^.BaseOfCode := Address
     end
    else
    if (Section.FSectionHeader.Characteristics and
      IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
      Inc(IDataSize, AlignedSize)
    else
    if (Section.FSectionHeader.Characteristics and
      IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
      Inc(UDataSize, AlignedSize);

    Inc(iSize, addrAlignedSize);
    Inc(Offset, Section.FSectionHeader.SizeOfRawData);
    Inc(Address, (Integer(Section.FSectionHeader.Misc.VirtualSize) +
      AddrAlign - 1) div AddrAlign * AddrAlign);
   end;

  FOptionalHeader^.SizeOfCode := CodeSize;
  FOptionalHeader^.SizeOfInitializedData := IDataSize;
  FOptionalHeader^.SizeOfUninitializedData := UDataSize;

  i := SizeOf(DWORD) +                   // NT signature
    SizeOf(FCOFFHeader) + FCOFFHeader.SizeOfOptionalHeader +
    CodeSize;

  i := (i + AddrAlign - 1) div AddrAlign * AddrAlign;

  //////////////////////////////////////////////////////////////////////////////
  // With explorer.exe, CodeSize is $14800, i is 148E8, so aligned 'i' is $15000
  // .. so BaseOfData should be $15000 + BaseOfCode ($1000) = $16000.
  //
  // ... but it's not - it's $15000, which means that the last $8e8 bytes of code
  // should be stampled over by the data!
  //
  // But obviously explorer.exe works, so I'm, missing a trick here.  Never mind - it
  // doesn't do any harm making it $16000 instead, and the formula works for everything
  // else I've tested...
  //////////////////////////////////////////////////////////////////////////////

  {$IFNDEF CPU64}
  FOptionalHeader^.BaseOfData := FOptionalHeader.BaseOfCode + DWORD(i);
  {$ENDIF}

  FOptionalHeader^.SizeOfImage := iSize;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.FindDictionaryEntrySection                             //
//                                                                            //
//  Return the index of the specified Section.  The 'entryNo' to find         //
//  should be a 'IMAGE_DIRECTORY_ENTRY_xxxx' constant defined in              //
//  Windows.pas.                                                              // 
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.FindDictionaryEntrySection(entryNo: Integer;
  var Offset: Integer): Integer;
var
  i: Integer;
  p: PImageDataDirectory;
begin
  Result := -1;
  p := DataDictionary[entryNo];
                                // Find Section with matching virt address.
  for i := 0 to ImageSectionCount - 1 do
    if (p^.VirtualAddress >=
      ImageSection[i].FSectionHeader.VirtualAddress) and
      (p^.VirtualAddress < ImageSection[i].FSectionHeader.VirtualAddress +
      ImageSection[i].FSectionHeader.Misc.VirtualSize) then
     begin
      Result := i;
      Offset := p^.VirtualAddress -
        ImageSection[i].FSectionHeader.VirtualAddress;
      break
     end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.GetCOFFHeader                                          // 
//                                                                            //
//  Return COFF header                                                        // 
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.GetCOFFHeader: TImageFileHeader;
begin
  Result := FCOFFHeader;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.GetDataDictionary                                      //
//                                                                            //
//  Return the data dictionary for a specified                                //
//  IMAGE_DIRECTORY_ENTRY_xxxx  index                                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.GetDataDictionary(index: Integer): PImageDataDirectory;
var
  p: PImageDataDirectory;
begin
  if index < DataDictionaryCount then
   begin
    p := @FOptionalHeader.DataDirectory[0];
    Inc(p, index);
    Result := p
   end
  else
    raise ERangeError.Create(RCStrBadDictionaryIndex);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.GetDataDictionaryCount                                 //
//                                                                            //
//  Return no of entries in the Data Directory                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.GetDataDictionaryCount: Integer;
begin
  Result := FOptionalHeader^.NumberOfRvaAndSizes
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.GetDosHeader                                           // 
//                                                                            //
//  Return DOS header                                                         //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.GetDOSHeader: TImageDosHeader;
begin
  Result := FDOSHeader;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.GetImageSection () : TImageSection                     // 
//                                                                            //
//  Get the specified image Section                                           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.GetExportCount: Integer;
var
  ExportSection: PImageExportDirectory;
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetExportSection(Offset);
  if Assigned(Section) then
   begin
    ExportSection := PImageExportDirectory(
      PAnsiChar(Section.FRawData.memory) + Offset);
    Result := ExportSection^.NumberOfNames
   end
  else
    Result := 0;
end;

procedure TPEModule.GetExportDetails(idx: Integer; var Name: AnsiString;
  var Ordinal: DWORD);
var
  ExportSection: PImageExportDirectory;
  Section: TImageSection;
  Offset: Integer;
  po: DWORD;
  pw: PWORD;
  p: PDWORD;
  Data: PAnsiChar;
begin
  Section := GetExportSection(Offset);
  if Assigned(Section) then
   begin
    Data := GetExportSectionData;
    ExportSection := PImageExportDirectory(
      PAnsiChar(Section.FRawData.memory) + Offset);
    po := DWORD(ExportSection^.AddressOfNameOrdinals);
    pw := PWORD(Data + po);
    Inc(pw, idx);
    ordinal := pw^;

    po := DWORD(ExportSection^.AddressOfNames);
    p := PDWORD(Data + po);
    Inc(p, idx);
    Name := Data + p^
   end
end;

function TPEModule.GetExportSection(var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  Offset := 0;
  idx := FindDictionaryEntrySection(IMAGE_DIRECTORY_ENTRY_EXPORT, Offset);
  if idx = -1 then
    Result := nil
  else
    Result := ImageSection[idx]
end;

function TPEModule.GetExportSectionData: PAnsiChar;
var
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetExportSection(Offset);
  Result := PAnsiChar(Section.FRawData.Memory) -
    Section.FSectionHeader.VirtualAddress;
end;

function TPEModule.GetImageSection(index: Integer): TImageSection;
begin
  Result := TImageSection(FSectionList[index]);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.GetImageSectionCount                                   //
//                                                                            //
//  Return no of image sections                                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.GetImageSectionCount: Integer;
begin
  Result := FSectionList.Count
end;

function DirValid(dir: PImageImportDirectory): Boolean;
begin
  DirValid := (dir^.Characteristics <> 0) or (dir^.TimeDateStamp <> 0) or
    (dir^.ForwarderChain <> 0) or (dir^.Name <> 0) or
    (dir^.FirstThunk <> 0)
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEModule.GetImageSectionCount                                   //
//                                                                            //
//  Get the optional header                                                   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEModule.GetImport(idx: Integer): PImageImportDirectory;
var
  ImportSection: PImageImportDirectory;
  Section: TImageSection;
  Offset: Integer;

begin
  Section := GetImportSection(Offset);
  Result := nil;
  if Assigned(Section) then
   begin
    ImportSection := PImageImportDirectory(
      PAnsiChar(Section.FRawData.memory) + Offset);

    while DirValid(ImportSection) and (idx > 0) do
     begin
      Inc(ImportSection);
      Dec(idx)
     end;

    if DirValid(ImportSection) then
      Result := ImportSection
   end
end;

function TPEModule.GetImportCount: Integer;
var
  ImportSection: PImageImportDirectory;
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetImportSection(Offset);
  Result := 0;
  if Assigned(Section) then
   begin
    ImportSection := PImageImportDirectory(
      PAnsiChar(Section.FRawData.memory) + Offset);

    while DirValid(ImportSection) do
     begin
      Inc(Result);
      Inc(ImportSection)
     end
   end
end;

function TPEModule.GetImportSection(var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDictionaryEntrySection(IMAGE_DIRECTORY_ENTRY_IMPORT, Offset);
  if idx = -1 then
    Result := nil
  else
    Result := ImageSection[idx]
end;

function TPEModule.GetImportSectionData: PAnsiChar;
var
  Section: TImageSection;
  Offset: Integer;
begin
  Section := GetImportSection(Offset);
  Result := PAnsiChar(Section.FRawData.Memory) -
    Section.FSectionHeader.VirtualAddress;
end;

function TPEModule.GetOptionalHeader: TImageOptionalHeader;
begin
  Result := FOptionalHeader^
end;

function TPEModule.GetResourceSection(var Offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDictionaryEntrySection(IMAGE_DIRECTORY_ENTRY_RESOURCE, Offset);
  if idx = -1
   then Result := nil
   else Result := ImageSection[idx]
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure TPEModule.LoadFromFile                                          //
//                                                                            //
//  Load the module from a file                                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEModule.LoadFromFile(const Name: TFileName);
var
  f: TFileStream;
begin
  f := TFileStream.Create(Name, fmOpenRead or fmShareDenyNone);
   try
    LoadFromStream(f)
   finally
    f.Free
   end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure TPEModule.LoadFromFile                                          //
//                                                                            //
//  Load the module from a stream                                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEModule.LoadFromStream(s: TStream);
begin
 with TMemoryStream.Create do
  try
   CopyFrom(s, 0);
   Decode(Memory, Size)
  finally
   Free
  end
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure TPEModule.SaveToFile                                            //
//                                                                            //
//  Save the module to a file                                                 //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

(*
procedure TPEModule.SaveToFile(const name: string);
var
  f : TFileStream;
begin
 f := TFileStream.Create (name, fmCreate);
 try
  SaveToStream(f)
 finally
  f.Free
 end
end;
*)

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure TPEModule.SaveToStream                                          //
//                                                                            //
//  Save the module to a stream                                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEModule.SaveToStream(Stream: TStream);
var
  NTSignature : DWORD;
  i           : Integer;
  Section     : TImageSection;
  PaddingSize : Integer;
  PaddingLen  : Integer;
  Padding     : PAnsiChar;
  OldCheckSum : DWORD;
  NewCheckSum : DWORD;
  NtHeaders   : PImageNtHeaders;
  CkOffset    : DWORD;
begin
 // Encode the data.
 Encode;

 NTSignature := IMAGE_NT_SIGNATURE;

 // Write the DOS stub
 Stream.Write(FDOSHeader, SizeOf(FDOSHeader));
 Stream.CopyFrom(FDOSStub, 0);

 // Write NT sig and COFF header
 Stream.Write(NTSignature, SizeOf(NTSignature));
 Stream.Write(FCOFFHeader, SizeOf(FCOFFHeader));
 CkOffset := Stream.Position + Integer(@FOptionalHeader^.CheckSum) -
   Integer(@FOptionalHeader^);
 Stream.Write(FOptionalHeader^, FCOFFHeader.SizeOfOptionalHeader);

                               // Write the Section headers
 for i := 0 to FSectionList.Count - 1 do
  begin
   Section := TImageSection(FSectionList[i]);
   Stream.Write(Section.FSectionHeader, SizeOf(Section.FSectionHeader))
  end;

 // Save the 'comment' Section.  See 'Decode' for Details
 if FCommentSize > 0
  then Stream.Write(FCommentBlock^, FCommentSize);

 // Write the sections
 Padding := nil;
 PaddingLen := 0;
 try
  for i := 0 to FSectionList.Count - 1 do
   begin
    // Write Padding up to file Offset of the Section
    Section := TImageSection(FSectionList[i]);
    PaddingSize := Section.FSectionHeader.PointerToRawData - DWORD(Stream.Position);

    if PaddingSize > PaddingLen then
     begin
      PaddingLen := PaddingSize + 65536;
      ReallocMem(Padding, PaddingLen);
      ZeroMemory(Padding, PaddingLen);
     end;

    // Put our signature at the start of the first
    if PaddingSize > 0 then Stream.Write(Padding^, PaddingSize);

    // Write the Section data.
    Stream.CopyFrom(Section.FRawData, 0);

    // Write data
    with Section.FSectionHeader
     do PaddingSize := SizeOfRawData - misc.VirtualSize;

    // Pad data
    if PaddingSize > PaddingLen then
     begin
      PaddingLen := PaddingSize + 65536;
      ReallocMem(Padding, PaddingLen);
      ZeroMemory(Padding, PaddingLen);
     end;

    if PaddingSize > 0
     then Stream.Write(Padding^, PaddingSize)
   end;

  // Save the debug info.
  if FEndCommentSize > 0
   then Stream.Write(FEndComment^, FEndCommentSize)
 finally
  ReallocMem(Padding, 0)
 end;

 // Now calculate the checksum....
 with TMemoryStream.Create do
  try
   Stream.Seek(0, soFromBeginning);
   LoadFromStream(Stream);
   NtHeaders := ChecksumMappedFile(Memory, Size, @OldCheckSum, @NewCheckSum);

   if Assigned(NtHeaders) then
    begin
     Stream.Seek(CkOffset, soFromBeginning);
     Stream.Write(NewCheckSum, SizeOf(NewCheckSum))
    end
  finally
   Free
  end;

 Stream.Seek(0, soFromEnd);
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TImageSection implementation'} {$ENDIF}
{ TImageSection }


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  constructor TImageSection.Create                                          //
//                                                                            //
//  Constructor for TImageSection.                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TImageSection.Create(AParent: TPEModule;
  const AHeader: TImageSectionHeader; rawData: pointer);
begin
  FSectionHeader := AHeader;
  FRawData := TMemoryStream.Create;

//  nb.  SizeOfRawData is usually bigger than VirtualSize because it's padded,
//       and VirtualSize isn't.


  if FSectionHeader.Misc.VirtualSize <= FSectionHeader.SizeOfRawData then
   begin

// Some linkers (?) set VirtualSize to 0 - which isn't correct.  Work round it.
// (Encountered in MCL Link Lite HHT software )

    if FSectionHeader.Misc.VirtualSize = 0 then
      FSectionHeader.Misc.VirtualSize := FSectionHeader.SizeOfRawData;
    FRawData.Write(rawData^, FSectionHeader.Misc.VirtualSize)
   end
  else

// nb.  If VirtualSize is bigger than SizeOfRawData it implies that extra padding is required.
//      Save the amount, so we can get all the COFF header values right.  See 'Encode' above.
   begin
    FRawData.Write(rawData^, FSectionHeader.SizeOfRawData);
    FUninitializedDataSize :=
      FSectionHeader.Misc.VirtualSize - FSectionHeader.SizeOfRawData;
   end;

  FParent := AParent;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TImageSection.GetSectionName                                     //
//                                                                            //
//  Return the Section name - eg. .data                                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TImageSection.GetSectionName: AnsiString;
begin
 Result := PAnsiChar(@FSectionHeader.Name)
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  destructor TImageSection.Destroy                                          //
//                                                                            //
//  destructor for TImageSection.                                             //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

destructor TImageSection.Destroy;
begin
 FreeAndNil(FRawData);
 inherited;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TPEResourceModule implementation'} {$ENDIF}

{ TPEResourceModule }

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  procedure TPEResourceModule.DeleteResource                                //
//                                                                            //
//  Delete the specified resource (by index)                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEResourceModule.DeleteResource(resourceNo: Integer);
var
  res: TResourceDetails;
begin
 res := ResourceDetails[resourceNo];
 inherited;
 resourceNo := IndexOfResource(Res);
 if resourceNo <> -1
  then FDetailList.Delete(resourceNo);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  constructor TPEResourceModule.Create                                      //
//                                                                            //
//  Constructor for TPEResourceModule                                         // 
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

constructor TPEResourceModule.Create;
begin
 inherited Create;
 FDetailList := TObjectList.Create;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  destructor TPEResourceModule.Destroy                                      // 
//                                                                            //
//  Destructor for TPEResourceModule                                          // 
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

destructor TPEResourceModule.Destroy;
begin
 FreeAndNil(FDetailList);
 inherited;
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEResourceModule.Decode                                         //
//                                                                            //
//  Decode the Section's resource tree into a list of resource details        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEResourceModule.Decode;
var
  Section: TImageSection;
  tp, Name: AnsiString;
  lang: Integer;
  Offset: Integer;

  // Get string resource name
  function GetResourceStr(IdorName: Boolean; Section: TImageSection;
    n: DWORD): AnsiString;
  var
    p: PWideChar;
  begin
    if IdorName
     then Result := AnsiString(IntToStr(n))
     else
      begin
       p := PWideChar(PAnsiChar(Section.FRawData.Memory) + (n and $7fffffff));
       Result := ResourceWideCharToStr(p, CP_ACP)
      end
  end;

  // (recursively) get resources
  procedure GetResource(Offset, level: Integer);
  var
    Entry: PResourceDirectoryEntry;
    i, Count: Integer;
    IDorName: Boolean;
    DataEntry: PResourceDataEntry;
    Table: PResourceDirectoryTable;
    Details: TResourceDetails;
  begin
    Table := PResourceDirectoryTable(
      PAnsiChar(Section.FRawData.memory) + Offset);
    with Table^ do
      Count := CNameEntries + CIDEntries;

    Entry := PResourceDirectoryEntry(PAnsiChar(Section.FRawData.memory) +
      Offset + SizeOf(TResourceDirectoryTable));
    for i := 0 to Count - 1 do
     begin
      idOrName := i >= Table^.CNameEntries;
      case level of
        0 : tp := GetResourceStr(IDOrName, Section, Entry^.Name);
        1 :
          Name := GetResourceStr(IDOrName, Section, Entry^.Name);
        2 :
         begin
          if not IdOrName then
            raise EPEException.Create(RCStrBadLangID);

          lang := Entry^.Name
         end
       end;

      if (Entry^.RVA and $80000000) > 0 then
 // Not a Leaf node - traverse the tree
        GetResource(Entry^.RVA and $7fffffff, level + 1)
      else
       begin
                                             // It's a leaf node - create resource details
        DataEntry := PResourceDataEntry(PAnsiChar(Section.FRawData.Memory) +
          Entry^.RVA);
        Details := TResourceDetails.CreateResourceDetails(Self,
          lang, string(Name), string(tp), DataEntry^.Size,
          PAnsiChar(Section.FRawData.Memory) + DataEntry^.OffsetToData -
          Section.FSectionHeader.VirtualAddress);
        Details.CodePage := DataEntry^.CodePage;
        Details.Characteristics := Table^.Characteristics;
        Details.DataVersion :=
          DWORD(Table^.VersionMajor) * 65536 + DWORD(Table^.VersionMinor);
        FDetailList.Add(Details);

       end;

      Inc(Entry)
     end
  end;

begin
  inherited;
  Section := GetResourceSection(Offset);
  if Section <> nil then
    GetResource(Offset, 0)
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEResourceModule.GetResourceCount                               //
//                                                                            //
//  Return the number of resources in the resource Section                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEResourceModule.GetResourceCount: Integer;
begin
  Result := FDetailList.Count
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEResourceModule.GetResourceDetails                             //
//                                                                            //
//  Get the resource Details for the specified resource.                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEResourceModule.GetResourceDetails(idx: Integer):
TResourceDetails;
begin
  Result := TResourceDetails(FDetailList[idx]);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEResourceModule.IndexOfResource                                //
//                                                                            //
//  Return the index of the specified resource Details in the resource        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

function TPEResourceModule.IndexOfResource(Details: TResourceDetails): Integer;
begin
  Result := FDetailList.IndexOf(Details);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEResourceModule.InsertResource                                 //
//                                                                            //
//  Insert a resource in the list.                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEResourceModule.InsertResource(idx: Integer;
  Details: TResourceDetails);
begin
  FDetailList.Insert(idx, Details);
end;


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  function TPEResourceModule.Encode                                         //
//                                                                            //
//  Complicated?  I'll give you complicated ...                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure TPEResourceModule.Encode;
var
  i           : Integer;
  Details     : TResourceDetails;
  Section     : TImageSection;
  Root        : TResourceNode;
  versMajor   : Word;
  versMinor   : Word;
  TimeStamp   : DWORD;
  NameSize    : DWORD;
  NameOffset  : DWORD;
  NamePos     : DWORD;
  TableOffset : DWORD;
  deOffset    : DWORD;
  dePos       : DWORD;
  deSize      : DWORD;
  DataOffset  : DWORD;
  DataPos     : DWORD;
  DataSize    : DWORD;
  Offset      : Integer;

  nameTable   : PAnsiChar;
  deTable     : PAnsiChar;
  Data        : PAnsiChar;
  Zeros       : PAnsiChar;

  //------------------------------------------------------------------
  // Calculate Offset and Size of name Table and DirectoryEntry Table.
  // Calculate Size of data

  procedure GetNameTableSize(node: TResourceNode);
  var
    i: Integer;
  begin
    Inc(NameOffset, SizeOf(TResourceDirectoryTable));
    Inc(deOffset, SizeOf(TResourceDirectoryTable));

    for i := 0 to node.Count - 1 do
     begin
      Inc(NameOffset, SizeOf(TResourceDirectoryEntry));
      Inc(deOffset, SizeOf(TResourceDirectoryEntry));

      if not node.Nodes[i].IntID then
        Inc(NameSize, Length(node.Nodes[i].Id) * SizeOf(WideChar) +
          SizeOf(word));

      if not node.Nodes[i].Leaf then
        GetNameTableSize(node.Nodes[i].Next)
      else
       begin
        Inc(NameOffset, SizeOf(TResourceDataEntry));
        Inc(deSize, SizeOf(TResourceDataEntry));
        DataSize := (DataSize + DWORD(node.Nodes[i].Data.Size) + 3) div 4 * 4;
       end
     end
  end;

  //------------------------------------------------------------------
  // Save a node to Section.FRawData (and save it's child nodes recursively)

  procedure SaveToSection(Node: TResourceNode);
  var
    Table     : TResourceDirectoryTable;
    Entry     : TResourceDirectoryEntry;
    DataEntry : PResourceDataEntry;
    i, n      : Integer;
    w         : WideString;
    wl        : word;

  //------------------------------------------------------------------
  // Save Entry (i), and the child Nodes

    procedure SaveNode(i: Integer);
    begin
      if node.Nodes[i].IntID then // Id is a simple integer
        Entry.Name := StrToInt(string(node.Nodes[i].Id))
      else
       begin
        // Id is an offset to a name in the name table.
        Entry.Name := NameOffset + NamePos + $80000000;
        w := string(node.Nodes[i].Id);
        wl := Length(node.Nodes[i].Id);
        Move(wl, nameTable[NamePos], SizeOf(wl));
        Inc(NamePos, SizeOf(wl));
        Move(w[1], nameTable[NamePos], wl * SizeOf(WideChar));
        Inc(NamePos, wl * SizeOf(WideChar))
       end;

      if node.Nodes[i].Leaf then
       // RVA points to a TResourceDataEntry in the data entry table.
       begin                            
        Entry.RVA := deOffset + dePos;
        DataEntry := PResourceDataEntry(deTable + dePos);
        DataEntry^.CodePage := node.Nodes[i].CodePage;
        DataEntry^.Reserved := 0;
        DataEntry^.Size := node.Nodes[i].Data.Size;
        DataEntry^.OffsetToData :=
          DataOffset + DataPos + Section.FSectionHeader.VirtualAddress;

        Move(node.Nodes[i].Data.memory^, Data[DataPos], DataEntry^.Size);

        Inc(dePos, SizeOf(TResourceDataEntry));
        DataPos := (DataPos + DataEntry^.Size + 3) div 4 * 4;
        Section.FRawData.Write(Entry, SizeOf(Entry));
       end
      else // RVA points to another Table.
       begin
        Entry.RVA := $80000000 + TableOffset;
        Section.FRawData.Write(Entry, SizeOf(Entry));
        n := Section.FRawData.Position;
        SaveToSection(node.Nodes[i].Next);
        Section.FRawData.Seek(n, soFromBeginning);
       end
    end;

  begin { SaveToSection }
    Table.Characteristics := 0;
    Table.TimeDateStamp := TimeStamp;
    Table.VersionMajor := versMajor;
    Table.VersionMinor := versMinor;
    Table.CNameEntries := 0;
    Table.CIDEntries := 0;

    // Calculate no of integer and string IDs
    for i := 0 to node.Count - 1 do
     if node.Nodes[i].IntID
      then Inc(Table.CIDEntries)
      else Inc(Table.CNameEntries);

    Section.FRawData.Seek(TableOffset, soFromBeginning);
    Section.FRawData.Write(Table, SizeOf(Table));

    TableOffset := TableOffset + SizeOf(TResourceDirectoryTable) +
      DWORD(node.Count) * SizeOf(TResourceDirectoryEntry);

    // The docs suggest that you save the Nodes with string entries first.
    // Goodness knows why, but play along...

    for i := 0 to node.Count - 1 do
     if not node.Nodes[i].IntID then SaveNode(i);

    for i := 0 to node.Count - 1 do
     if node.Nodes[i].IntID then SaveNode(i);

    Section.FRawData.Seek(0, soFromEnd);
  end;


begin { Encode }
 Section := GetResourceSection(Offset);

 // Get the Details in a tree structure
 Root := nil;
 Data := nil;
 deTable := nil;
 Zeros := nil;

 try
  for i := 0 to FDetailList.Count - 1 do
   begin
    Details := TResourceDetails(FDetailList.Items[i]);
    if Root = nil
     then Root := TResourceNode.Create(AnsiString(Details.ResourceType),
            AnsiString(Details.ResourceName),
            Details.ResourceLanguage, Details.Data, Details.CodePage)
     else Root.Add(AnsiString(Details.ResourceType),
            AnsiString(Details.ResourceName), Details.ResourceLanguage,
            Details.Data, Details.CodePage)
   end;

  // Save elements of their original EXE
  versMajor := PResourceDirectoryTable(Section.FRawData.Memory)^.VersionMajor;
  versMinor := PResourceDirectoryTable(Section.FRawData.Memory)^.VersionMinor;
  TimeStamp := PResourceDirectoryTable(Section.FRawData.Memory)^.TimeDateStamp;

  // Clear the data.  We're gonna recreate  it from our resource details.
  Section.FRawData.Clear;

  NameSize   := 0;
  NameOffset := Offset;
  deSize     := 0;
  deOffset   := Offset;
  DataSize   := 0;

  GetNameTableSize(Root);  // Calculate sizes and offsets of the name table,
                           // the data entry table and the size of the data.

  // Calculate the data Offset.  Must be aligned.
  DataOffset := (NameOffset + NameSize + 15) div 16 * 16;

  // Initialize globals...

  // Offset of next entry in the string Table
  NamePos := 0;
  // Offset of next entry in the data entry Table
  dePos := 0;
  // Offset of next data block.
  DataPos := 0;
  // Offset of next TResourceDirectoryTable
  TableOffset := 0;

  GetMem(nameTable, NameSize);         // Allocate buffers for tables
  GetMem(Data, DataSize);
  GetMem(deTable, deSize);

  SaveToSection(Root);               // Do the work.

  // Save the tables
  Section.FRawData.Write(deTable^, deSize);
  Section.FRawData.Write(nameTable^, NameSize);

  // Add padding so the data goes on a
  // 16 byte boundary.
  if DWORD(Section.FRawData.Position) < DataOffset then
   begin
    GetMem(Zeros, DataOffset - DWORD(Section.FRawData.Position));
    ZeroMemory(Zeros, DataOffset - DWORD(Section.FRawData.Position));
    Section.FRawData.Write(Zeros^, DataOffset - DWORD(Section.FRawData.Position))
   end;

  // Write the data.
  Section.FRawData.Write(Data^, DataSize);

  inherited; // **** Must call inherited !

 finally       // Tidy up.
  Dispose(Zeros);
  FreeMem(nameTable);
  FreeMem(deTable);
  FreeMem(Data);
  FreeAndNil(Root);
 end
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TResourceNode implementation'} {$ENDIF}
{ TResourceNode }

procedure TResourceNode.Add(const AType, AName: AnsiString;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
var
  Index: Integer;
begin
 for Index := 0 to Count - 1 do
  if AType = Nodes[Index].Id then
   begin
    Nodes[Index].Next.AddName(AName, ALang, aData, codePage);
    Exit;
   end;

 Inc(Count);
 SetLength(Nodes, Count);
 Nodes[Count - 1].Id := AType;
 Nodes[Count - 1].IntID := isID(Count - 1);
 Nodes[Count - 1].Leaf := False;
 Nodes[Count - 1].Next := TResourceNode.CreateNameNode(AName, ALang, AData, codePage)
end;

procedure TResourceNode.AddLang(ALang: Integer; aData: TMemoryStream;
  codePage: DWORD);
var
  Index : Integer;
begin
 for Index := 0 to Count - 1 do
  if IntToStr(ALang) = string(Nodes[Index].Id) then
   begin
    Nodes[Index].Data := aData;
    Exit;
   end;

 Inc(Count);
 SetLength(Nodes, Count);
 Nodes[Count - 1].Id := AnsiString(IntToStr(ALang));
 Nodes[Count - 1].IntID := True;
 Nodes[Count - 1].Leaf := True;
 Nodes[Count - 1].Data := aData;
 Nodes[Count - 1].CodePage := codePage;
end;

procedure TResourceNode.AddName(const AName: AnsiString; ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
var
  Index : Integer;
begin
 for Index := 0 to Count - 1 do
  if AName = Nodes[Index].Id then
   begin
    Nodes[Index].Next.AddLang(ALang, aData, codePage);
    Exit;
   end;

 Inc(Count);
 SetLength(Nodes, Count);
 Nodes[Count - 1].Id := AName;
 Nodes[Count - 1].IntID := isID(Count - 1);
 Nodes[Count - 1].Leaf := False;
 Nodes[Count - 1].Next := TResourceNode.CreateLangNode(ALang, aData, codePage);
end;

constructor TResourceNode.Create(const AType, AName: AnsiString;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
begin
 Count := 1;
 SetLength(Nodes, 1);
 Nodes[0].Id := AType;
 Nodes[Count - 1].IntID := isID(Count - 1);
 Nodes[0].Leaf := False;
 Nodes[0].Next := TResourceNode.CreateNameNode(AName, ALang, aData, codePage);
end;

constructor TResourceNode.CreateLangNode(ALang: Integer; aData: TMemoryStream;
  codePage: DWORD);
begin
 Count := 1;
 SetLength(Nodes, 1);
 Nodes[0].Id := AnsiString(IntToStr(ALang));
 Nodes[Count - 1].IntID := True;
 Nodes[0].Leaf := True;
 Nodes[0].Data := aData;
 Nodes[0].CodePage := codePage
end;

constructor TResourceNode.CreateNameNode(const AName: AnsiString;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
begin
 Count := 1;
 SetLength(Nodes, 1);
 Nodes[0].Id := AName;
 Nodes[Count - 1].IntID := isID(Count - 1);

 Nodes[0].Leaf := False;
 Nodes[0].Next := TResourceNode.CreateLangNode(ALang, aData, codePage)
end;

destructor TResourceNode.Destroy;
var
  Index: Integer;
begin
 for Index := 0 to Count - 1 do
  if not Nodes[Index].Leaf
   then Nodes[Index].Next.Free;

 inherited;
end;

function TResourceNode.IsID(idx: Integer): Boolean;
var
  Index : Integer;
begin
 Result := True;
 for Index := 1 to Length(Nodes[idx].Id) do
  if not (Nodes[idx].Id[Index] in ['0'..'9']) then
   begin
    Result := False;
    Break
   end;

 if Result
  then Result := AnsiString(IntToStr(StrToInt(string(Nodes[idx].Id)))) = Nodes[idx].Id;
end;

function TPEResourceModule.AddResource(Details: TResourceDetails): Integer;
begin
 Result := FDetailList.Add(Details);
end;

procedure TPEResourceModule.SortResources;
begin
 FDetailList.Sort(CompareDetails);
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

end.
