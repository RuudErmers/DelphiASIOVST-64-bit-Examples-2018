unit DAV_AsioList;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Registry, Windows;

type
  TDavAsioDriverDescription = class
  private
    FGuid     : TGUID;
    FName     : string;
    FFilename : string;
  public
    constructor Create(nGuid: TGUID; nName: string; nFilename: string); overload;
    constructor Create(nGuid, nName, nFilename: string); overload;
    property Guid: TGUID read FGuid;
    property Name: string read FName;
    property Filename: string read FFilename;
  end;

  TDavAsioDriverList = class
  private
    FNameList      : TStrings;
    FIgnoreGuid    : TGuid;
    FHasIgnoreGuid : boolean;
    FList          : TList;
    procedure ClearList;
    procedure LoadList;
    function GetDriverFileName(DrvGuidStr: string): string;
    function GetItem(Index: Integer): TDavAsioDriverDescription;
    function GetCount: Integer;
  public
    constructor Create(Ignore: TGuid); reintroduce; overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure SetIgnoredDriver(ignore: TGuid);
    procedure UpdateList;
    function DriverNumberByName(DriverName: string): Integer;
    property Items[Index: Integer]: TDavAsioDriverDescription read GetItem;
    property Count: Integer read GetCount;
    property DriverNames: TStrings read FNameList;
  end;

implementation

const
  CAsioInprocServer = 'InprocServer32';
  CAsioPath         = 'software\asio';
  CAsioComClsId     = 'clsid';
  CAsioDescription  = 'description';


{ TDAVIntAsioDriverDesc }

constructor TDavAsioDriverDescription.Create(nGuid: TGUID; nName: string; nFilename: string);
begin
 FGuid := nGuid;
 FName := nName;
 FFilename := nFilename;
end;

constructor TDavAsioDriverDescription.Create(nGuid, nName, nFilename: string);
begin
 FGuid := StringToGUID(nGuid);
 FName := nName;
 FFilename := nFilename;
end;


{ TDAVIntAsioDriverList }

constructor TDavAsioDriverList.Create;
begin
 inherited;
 FHasIgnoreGuid := False;
 FNameList := TStringList.Create;
 FList := TList.Create;
end;

constructor TDavAsioDriverList.Create(Ignore: TGuid);
begin
 Create;
 SetIgnoredDriver(Ignore);
end;

destructor TDavAsioDriverList.Destroy;
begin
 ClearList;
 FreeAndNil(FList);
 FreeAndNil(FNameList);
 inherited;
end;

procedure TDavAsioDriverList.SetIgnoredDriver(Ignore: TGuid);
begin
 if (FIgnoreGuid.D1 <> Ignore.D1) and (FIgnoreGuid.D2 <> Ignore.D2) and
    (FIgnoreGuid.D3 <> Ignore.D3) then
  begin
   FIgnoreGuid := Ignore;
   FHasIgnoreGuid := True;
  end;
end;

procedure TDavAsioDriverList.ClearList;
var
  DriverIndex : Integer;
begin
 for DriverIndex := Count - 1 downto 0
  do Items[DriverIndex].Free;

 FNameList.Clear;
 FList.Clear;
end;

function TDavAsioDriverList.GetCount: Integer;
begin
 Result := FList.Count;
end;

function TDavAsioDriverList.GetDriverFileName(DrvGuidStr: string): string;
var
  Filename : string;
  DirStr   : PChar;
begin
 Result := '';
 if DrvGuidStr = '' then exit;

 with TRegistry.Create do
  try
   RootKey := HKEY_CLASSES_ROOT;
   if OpenKeyReadOnly(CAsioComClsId + '\' + Lowercase(DrvGuidStr) + '\' + CAsioInprocServer) then
    begin
     Result := ReadString('');
     Filename := ExtractFileName(Result);
     DirStr := StrAlloc(MAX_PATH);

     if not FileExists(Result) and (GetSystemDirectory(DirStr, MAX_PATH) <> 0)
      then Result := StrPas(DirStr) + '\' + Filename;

     if not FileExists(Result) and (GetWindowsDirectory(DirStr, MAX_PATH) <> 0)
      then Result := StrPas(DirStr) + '\' + Filename;

     if not FileExists(Result) then Result := '';

     StrDispose(DirStr);
     CloseKey;
    end;

 finally
  Free;
 end;
end;

function TDavAsioDriverList.GetItem(Index: Integer): TDavAsioDriverDescription;
begin
 Result := TDavAsioDriverDescription(FList.Items[Index]);
end;

procedure TDavAsioDriverList.LoadList;
var
  SubKeys     : TStringList;
  KeyNo       : Integer;
  DrvName     : string;
  DrvGuidStr  : string;
  DrvFile     : string;
  DrvGuid     : TGuid;
  DriverItem  : TDavAsioDriverDescription;
begin
 SubKeys := TStringList.Create;
 with TRegistry.Create do
  try
   RootKey := HKEY_LOCAL_MACHINE;
   if OpenKeyReadOnly(CAsioPath) then
    begin
     GetKeyNames(SubKeys);
     CloseKey;
    end;

   for KeyNo := 0 to SubKeys.Count - 1 do
    if OpenKeyReadOnly(CAsioPath + '\' + SubKeys[KeyNo]) then
     try
      DrvGuidStr := ReadString(CAsioComClsId);
      if DrvGuidStr <> '' then
       begin
        DrvGuid := StringToGUID(DrvGuidStr);

        DrvFile := GetDriverFileName(DrvGuidStr);
        if (DrvFile <> '') and not (FHasIgnoreGuid and IsEqualGUID(DrvGuid, FIgnoreGuid)) then
         begin
          DrvName := ReadString(CAsioDescription);
          if DrvName = '' then DrvName := SubKeys[KeyNo];

          DriverItem := TDavAsioDriverDescription.Create(DrvGuidStr, DrvName, DrvFile);

          FList.Add(DriverItem);
          FNameList.Add(DrvName);
         end;
       end
       {$IFNDEF IgnoreBrokenAsioList}
      else
       raise Exception.Create('Error loading GUID from ' + SubKeys[KeyNo])
      {$ENDIF};
     finally
      CloseKey;
     end;
 finally
  Free;
  FreeAndNil(SubKeys);
 end;
end;

procedure TDavAsioDriverList.UpdateList;
begin
 ClearList;
 LoadList;
end;

function TDavAsioDriverList.DriverNumberByName(DriverName: string): Integer;
var
  DriverIndex : Integer;
begin
 DriverName := LowerCase(DriverName);
 Result := -1;
 for DriverIndex := 0 to Count-1 do
  if LowerCase(Items[DriverIndex].Name) = DriverName then
   begin
    Result := DriverIndex;
    Break;
   end;
end;

end.
