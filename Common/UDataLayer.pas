unit UDataLayer;

interface

uses Classes;

type TDataLayerSection = class(TStringList)
  private
    FSaveEnabled:boolean;
  protected
    procedure Save;virtual;
  public
    function Add(const S: string): Integer; override;
    procedure AddStrings(Strings: TStrings); override;
    procedure SetAttribute(attrib,value:string);
    procedure SetAttributeI(attrib:string;value:integer);
    procedure SetAttributeB(attrib:string;value:boolean);
    function GetAttribute(attrib:string):string;
    function GetAttributeI(attrib:string;defValue:integer=0):integer;
    function GetAttributeB(attrib:string):boolean;
    function LoadSection(SectionName:string;sl:Tstrings):boolean;
    function SaveSection(SectionName:string;sl:Tstrings):boolean;
    property SaveEnabled:boolean read FSaveEnabled write FSaveEnabled;

end;
type TDataLayer = class  (TDataLayerSection)
  private
    FFilename:string;
  protected
    procedure Save;override;
  public

    constructor create(filename:string='');overload;
    constructor create(sl:TStringlist);overload;

end;

function SlSaveSection(data:TStringList;sectionname: string;sl:Tstrings): boolean;
function SlLoadSection(data:TStringList;sectionname: string;sl:Tstrings): boolean;


implementation

uses SysUtils;


procedure RemoveSection(data:TStringList;sectionname:string);
VAR i, istart,istop:integer;
begin
  istart:=-1;
  istop:=0;
  for i:=0 to data.Count-1 do
  begin
    if data.Strings[i]='['+sectionname+']' then Istart:=i;
    if data.Strings[i]='[~'+sectionname+']' then IStop:=i;
  end;
  if istart=-1 then exit;
  for i:=istop downto istart do data.Delete(i);
end;

procedure AddSection(data:TStringList;sectionname:string;sl:Tstrings);
VAR i:integer;
begin
  data.Add('['+sectionname+']');
  for i:=0 to sl.Count-1 do
    data.Add(sl[i]);
  data.Add('[~'+sectionname+']')
end;

constructor TDataLayer.create(filename: string);
begin
  inherited create;
  SaveEnabled:=false;
  FFileName:=filename;
  try
    LoadFromFile(Filename);
  except end;
  SaveEnabled:=true;
end;

function SlLoadSection(data:TStringList;sectionname: string;sl:Tstrings): boolean;
Var i:integer;
    InSection:Boolean;
begin
  sl.Clear;
  InSection:=false;
  for i:=0 to data.Count-1 do
  begin
    if UpperCase(data.Strings[i])=UpperCase('['+sectionname+']') then InSection:=true
    else if UpperCase(data.Strings[i])=UpperCase('[~'+sectionname+']') then InSection:=false
    else if InSection then sl.Add(data.Strings[i]);
  end;
end;

function SlSaveSection(data:TStringList;sectionname: string;sl:Tstrings): boolean;
begin
  RemoveSection(data,sectionname);
  AddSection(data,sectionname,sl);
end;

constructor TDataLayer.create(sl: TStringlist);
begin
  create;
  assign(sl);
end;

function TDataLayerSection.LoadSection(SectionName:string;sl:Tstrings):boolean;
begin
  SlLoadSection(self,SectionName,sl);
end;

procedure TDataLayerSection.Save;
begin
 //
end;

function TDataLayerSection.SaveSection(SectionName: string;  sl: Tstrings): boolean;
begin
  SaveEnabled:=false;
  SlSaveSection(self,SectionName,sl);
  SaveEnabled:=true;
  Save;
end;

procedure TDataLayer.Save;
begin
  if (FFileName<>'') and (SaveEnabled) then
  try
    SaveToFile(FFilename);
  except end;
end;

function TDataLayerSection.Add(const S: string): Integer;
begin
  result:=inherited Add(S);
  Save;
end;

procedure TDataLayerSection.SetAttribute(attrib,value:string);
VAR i:integer;
    InSection:boolean;
begin
  InSection:=false;
  for i:=0 to count-1 do
  begin
    if 1 = pos('[',self[i]) then
      InSection:= 1 <> pos('[~',self[i]);
    if not InSection and (1 = pos(UpperCase(attrib+'='),UpperCase(self[i]))) then
    begin
      self[i]:=attrib+'='+value;
      Save;
      exit;
    end;
  end;
  Add(attrib+'='+value);
end;

procedure TDataLayerSection.SetAttributeB(attrib: string; value: boolean);
begin
  SetAttribute(attrib,inttostr(ord(value)));
end;

procedure TDataLayerSection.SetAttributeI(attrib: string; value: integer);
begin
  SetAttribute(attrib,inttostr(value));
end;

procedure TDataLayerSection.AddStrings(Strings: TStrings);
begin
  inherited;
  Save;
end;

function TDataLayerSection.GetAttribute(attrib: string): string;
Var i:integer;
begin
  result:='';
  for i:=0 to count-1 do if
   1 = pos(UpperCase(attrib+'='),UpperCase(self[i])) then
   begin
     result:=Copy(self[i],length(attrib)+2);
     exit;
   end;
end;


function TDataLayerSection.GetAttributeB(attrib: string): boolean;
begin
  result:=GetAttribute(attrib)='1';
end;

function TDataLayerSection.GetAttributeI(attrib: string;
  defValue: integer): integer;
begin
  result:=StrToIntDef(GetAttribute(attrib),defValue)
end;

end.


