unit DAV_VSTParameters;

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, SysUtils, Forms,
  DAV_Types, DAV_VSTEffect, DAV_VSTBasicModule;

type
  TCustomVstParameterCategory = class(TCollectionItem)
  private
    FDisplayName : string;
    FParamsInCat : Integer;
    FVSTModule   : TBasicVSTModule;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const AValue: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
  published
    property DisplayName{$IFNDEF FPC}: string read FDisplayName write SetDisplayName{$ENDIF};
    property ParametersInCategory: Integer read FParamsInCat;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

  TCustomVstParameterCategories = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
  protected
    function GetItem(Index: Integer): TCustomVstParameterCategory; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomVstParameterCategory); virtual;
    property Items[Index: Integer]: TCustomVstParameterCategory read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function CategoryExists(const Value: AnsiString): Boolean;
    function CategoryIndex(const Value: AnsiString): Integer;
    function Add: TCustomVstParameterCategory;
    function Insert(const Index: Integer): TCustomVstParameterCategory;
    procedure CheckParametersInUse;
    procedure Delete(const Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

  TCurveType = (ctLinear, ctLogarithmic, ctExponential, ctFrequencyScale);

  TParameterChangeEvent        = procedure(Sender: TObject; const Index: Integer; var Value: Single) of object;
  TCustomParameterLabelEvent   = procedure(Sender: TObject; const Index: Integer; var PreDefined: AnsiString) of object;
  TCustomParameterDisplayEvent = procedure(Sender: TObject; const Index: Integer; var PreDefined: AnsiString) of object;
  TString2ParameterEvent       = procedure(Sender: TObject; const Index: Integer; const ParameterString: AnsiString; var Value: Single) of object;

  TCustomVstParameterProperty = class(TCollectionItem)
  private
    FSmoothStates        : TDAV2SingleArray;
    FMin, FMax           : Single;
    FCurve               : TCurveType;
    FCurveFactor         : Single;
    FInvCurveFactor      : Single;
    FDisplayName         : string;
    FUnits               : AnsiString;
    FSmoothingFactor     : Single;
    FCanBeAutomated      : Boolean;
    FV2Properties        : Boolean;
    FStepFloat           : Single;
    FSmallStepFloat      : Single;
    FLargeStepFloat      : Single;
    FFlags               : TVstParameterPropertiesFlags;
    FMinInteger          : Integer;
    FMaxInteger          : Integer;
    FStepInteger         : Integer;
    FLargeStepInteger    : Integer;
    FCC                  : Integer;
    FShortLabel          : AnsiString;
    FCategoryString      : AnsiString;
    FUseDefaultStr2Param : Boolean;

    FVSTModule     : TBasicVSTModule;
    FOnParamChange : TParameterChangeEvent;
    FOnCParamLabel : TCustomParameterLabelEvent;
    FOnCParamDisp  : TCustomParameterDisplayEvent;
    FOnStr2Param   : TString2ParameterEvent;

    function GetCategoryIndex: Integer;
    procedure SetShortLabel(const Value: AnsiString);
    procedure SetCurve(const Value: TCurveType);
    procedure SetCurveFactor(const Value: Single);
    procedure SetMax(const Value: Single);
    procedure SetMin(const Value: Single);
    procedure SetUnits(AUnits: AnsiString);
    procedure SetSmoothingFactor(const Value: Single);
    procedure SetCategoryString(const Value: AnsiString);
    procedure SetCategoryIndex(const Value: Integer);

    procedure ReadMaxProperty(Reader: TReader);
    procedure WriteMaxProperty(Writer: TWriter);
    procedure ReadStepFloatProperty(Reader: TReader);
    procedure WriteStepFloatProperty(Writer: TWriter);
    procedure ReadCurveFactorProperty(Reader: TReader);
    procedure WriteCurveFactorProperty(Writer: TWriter);
    procedure ReadSmallStepFloatProperty(Reader: TReader);
    procedure WriteSmallStepFloatProperty(Writer: TWriter);
    procedure ReadLargeStepFloatProperty(Reader: TReader);
    procedure WriteLargeStepFloatProperty(Writer: TWriter);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateCurveFactor; virtual;
    procedure CurveFactorChanged; virtual;
    procedure MaximumChanged; virtual;
    procedure MinimumChanged; virtual;
    procedure SetDisplayName(const AValue: string); override;
    procedure ShortLabelChanged; virtual;
    procedure CategoryStringChanged; virtual;
    procedure UnitsChanged; virtual;
    procedure DefineProperties(Filer: TFiler); override;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
    function Smooth(const Value: Single): Single;
    function VSTParameter2Parameter(const Value: Single): Single;
    function Parameter2VSTParameter(const Value: Single): Single;
  published
    property CanBeAutomated: Boolean read FCanBeAutomated write FCanBeAutomated default True;
    property CC: Integer read FCC write FCC default -1;
    property Curve: TCurveType read FCurve write SetCurve default ctLinear;
    property CurveFactor: Single read FCurveFactor write SetCurveFactor;
    property Category: AnsiString read FCategoryString write SetCategoryString;
    property CategoryIndex: Integer read GetCategoryIndex write SetCategoryIndex stored false;
    property DisplayName{$IFNDEF FPC}: string read FDisplayName write SetDisplayName{$ENDIF};
    property Flags: TVstParameterPropertiesFlags read FFlags write FFlags default [];
    property LargeStepFloat: Single read FLargeStepFloat write FLargeStepFloat;
    property LargeStepInteger: Integer read FLargeStepInteger write FLargeStepInteger default 10;
    property Max: Single read FMax write SetMax;
    property MaxInteger: Integer read FMaxInteger write FMaxInteger default 100;
    property Min: Single read FMin write SetMin;
    property MinInteger: Integer read FMinInteger write FMinInteger default 0;
    property ReportVST2Properties: Boolean read FV2Properties write FV2Properties default False;
    property ShortLabel: AnsiString read FShortLabel write SetShortLabel;
    property SmallStepFloat: Single read FSmallStepFloat write FSmallStepFloat;
    property SmoothingFactor: Single read FSmoothingFactor write SetSmoothingFactor;
    property StepFloat: Single read FStepFloat write FStepFloat;
    property StepInteger: Integer read FStepInteger write FStepInteger default 1;
    property Units: AnsiString read FUnits write SetUnits;
    property UseDefaultString2ParameterHandler: Boolean read FUseDefaultStr2Param write FUseDefaultStr2Param default False;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
    property OnParameterChange: TParameterChangeEvent read FOnParamChange write FOnParamChange;
    property OnCustomParameterLabel: TCustomParameterLabelEvent read FOnCParamLabel write FOnCParamLabel;
    property OnCustomParameterDisplay: TCustomParameterDisplayEvent read FOnCParamDisp write FOnCParamDisp;
    property OnStringToParameter: TString2ParameterEvent read FOnStr2Param write FOnStr2Param;
  end;

  TCustomVstParameterProperties = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
  protected
    function GetItem(Index: Integer): TCustomVstParameterProperty; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomVstParameterProperty); virtual;
    property Items[Index: Integer]: TCustomVstParameterProperty read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCustomVstParameterProperty;
    function Insert(Index: Integer): TCustomVstParameterProperty;
    procedure Delete(Index: Integer);
    procedure WriteVSTXML(FileName : TFileName); overload;
    procedure WriteVSTXML; overload;
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

  TVstParameterProperty = TCustomVstParameterProperty;
  TVstParameterProperties = TCustomVstParameterProperties;

implementation

uses
  Math, DAV_Common, DAV_VSTModuleWithPrograms;

{ TCustomVstParameterCategory }

{$IFDEF FPC}
constructor TCustomVstParameterCategory.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstParameterCategory.Create(Collection: TCollection);
{$ENDIF}
begin
 inherited;
 FDisplayName := 'Category ' + IntToStr(Collection.Count);
 FParamsInCat := 0;
 FVSTModule   := TCustomVstParameterCategories(Collection).VSTModule;
end;

destructor TCustomVstParameterCategory.Destroy;
begin
 inherited;
end;

procedure TCustomVstParameterCategory.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstParameterCategory then
  with TCustomVstParameterCategory(Dest) do
   try
    DisplayName := Self.DisplayName;
   except
    inherited;
   end
  else inherited;
end;

function TCustomVstParameterCategory.GetDisplayName: string;
begin
 Result := FDisplayName;
end;

procedure TCustomVstParameterCategory.SetDisplayName(const AValue: string);
var
  NewDisplayName : string;
begin
 NewDisplayName := AValue;
 if Length(NewDisplayName) > 24
  then SetLength(NewDisplayName, 24);

 if NewDisplayName <> FDisplayName then
  begin
   FDisplayName := NewDisplayName;
  end;
 inherited;
end;


{ TCustomVstParameterCategories }

constructor TCustomVstParameterCategories.Create(AOwner: TComponent);
begin
 inherited Create(AOwner, TCustomVstParameterCategory);
 FVSTModule := TVSTModuleWithPrograms(AOwner);
end;

destructor TCustomVstParameterCategories.Destroy;
begin
 while Count > 0 do Delete(0);
 inherited;
end;

function TCustomVstParameterCategories.Add: TCustomVstParameterCategory;
begin
 Result := TCustomVstParameterCategory(inherited Add);
end;

function TCustomVstParameterCategories.GetItem(Index: Integer): TCustomVstParameterCategory;
begin
 Result := TCustomVstParameterCategory(inherited GetItem(Index));
end;

function TCustomVstParameterCategories.Insert(const Index: Integer): TCustomVstParameterCategory;
begin
 Result := TCustomVstParameterCategory(inherited Insert(Index));
end;

procedure TCustomVstParameterCategories.Delete(const Index: Integer);
var
  i : Integer;
begin
 if VSTModule is TVSTModuleWithPrograms then
  with TVSTModuleWithPrograms(VSTModule) do
   if Assigned(ParameterProperties) then
    begin
     for i := 0 to ParameterProperties.Count - 1 do
      if string(ParameterProperties[i].Category) = Items[Index].DisplayName
       then ParameterProperties[i].Category := '';
    end;
 inherited Delete(Index);
end;

procedure TCustomVstParameterCategories.SetItem(Index: Integer; const Value: TCustomVstParameterCategory);
begin
  inherited SetItem(Index, Value);
end;

function TCustomVstParameterCategories.CategoryExists(const Value: AnsiString): Boolean;
var
  i : Integer;
begin
 Result := False;
 for i := 0 to Count - 1 do
  if Items[i].DisplayName = string(Value) then
   begin
    Result := True;
    exit;
   end;
end;

function TCustomVstParameterCategories.CategoryIndex(const Value: AnsiString): Integer;
var
  i : Integer;
begin
 Result := -1;
 for i := 0 to Count - 1 do
  if Items[i].DisplayName = string(Value) then
   begin
    Result := i;
    exit;
   end;
end;

procedure TCustomVstParameterCategories.CheckParametersInUse;
var
  c, p : Integer;
begin
 if VSTModule is TVSTModuleWithPrograms then
  with TVSTModuleWithPrograms(VSTModule) do
   if Assigned(ParameterProperties) then
    for c := 0 to Count - 1 do
     begin
      Items[c].FParamsInCat := 0;
      for p := 0 to ParameterProperties.Count - 1 do
       if string(ParameterProperties[p].Category) = Items[c].DisplayName
        then Inc(Items[c].FParamsInCat);
     end;
end;


{ TCustomVstParameterProperty }

{$IFDEF FPC}
constructor TCustomVstParameterProperty.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstParameterProperty.Create(Collection: TCollection);
{$ENDIF}
var
  i: Integer;
begin
  inherited;
  FMinInteger       := 0;
  FMaxInteger       := 100;
  FStepInteger      := 1;
  FCC               := -1;
  FCurve            := ctLinear;
  FFlags            := [];
  FLargeStepInteger := 10;
  FSmoothingFactor  := 0;
  FMin              := 0;
  FMax              := 1;
  FStepFloat        := 1;
  FCurveFactor      := 1;
  FInvCurveFactor   := 1;
  FSmallStepFloat   := 0.5;
  FLargeStepFloat   := 2;
  FCanBeAutomated   := True;
  FV2Properties     := False;
  FDisplayName      := 'Parameter ' + IntTostr(Collection.Count);

  Assert(Collection is TCustomVstParameterProperties);
  FVSTModule := TCustomVstParameterProperties(Collection).VSTModule;

  with FVSTModule as TVSTModuleWithPrograms do
   try
    Effect^.numParams := Collection.Count;
// RE <    if not (effFlagsProgramChunks in Effect^.EffectFlags) then
     if (Effect^.numPrograms > 0) then
      for i := 0 to Effect^.numPrograms - 1
       do Programs[i].SetParameterCount(Collection.Count)
     else SetParameterCount(Collection.Count);
   except
   end;
end;

destructor TCustomVstParameterProperty.Destroy;
var
  i: Integer;
begin
 try
  if VSTModule is TVSTModuleWithPrograms then
   with TVSTModuleWithPrograms(FVSTModule) do
    begin
// RE <      if not (effFlagsProgramChunks in Effect^.EffectFlags) then
      if Assigned(Programs) and (Programs.Count > 0) then
       for i := 0 to Programs.Count - 1 do
        if Assigned(Programs[i])
         then Programs[i].SetParameterCount(Collection.Count - 1)
      else SetParameterCount(Collection.Count - 1);

(*
     if (HostProduct <> 'Cubase VST') and
        (HostProduct <> 'Unknown') and
        (HostProduct <> '') then

     not necessary anymore ?!?
*)
     Effect^.numParams := Collection.Count - 1;
    end;
 finally
  inherited;
 end;
end;

procedure TCustomVstParameterProperty.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Max', ReadMaxProperty,
    WriteMaxProperty, Max = 0);
  Filer.DefineProperty('StepFloat', ReadStepFloatProperty,
    WriteStepFloatProperty, StepFloat = 0);
  Filer.DefineProperty('CurveFactor', ReadCurveFactorProperty,
    WriteCurveFactorProperty, CurveFactor = 0);
  Filer.DefineProperty('SmallStepFloat', ReadSmallStepFloatProperty,
    WriteSmallStepFloatProperty, SmallStepFloat = 0);
  Filer.DefineProperty('LargeStepFloat', ReadLargeStepFloatProperty,
    WriteLargeStepFloatProperty, LargeStepFloat = 0);
end;

function TCustomVstParameterProperty.VSTParameter2Parameter(const Value: Single): Single;
begin
 Result := Limit(Value, 0, 1);
 case Curve of
  ctLogarithmic: Result := (Exp(Result * Ln(FCurveFactor + 1)) - 1) * FInvCurveFactor;
  ctExponential: Result := Log2(FCurveFactor * Result + 1) / Log2(FCurveFactor + 1);
  ctFrequencyScale: Result := (Exp(Result * Ln((Max / Min) + 1)) - 1) / (Max / Min);
 else
 end;
 Result := Smooth(Result * (Max - Min) + Min);
end;

procedure TCustomVstParameterProperty.ReadCurveFactorProperty(Reader: TReader);
begin
 FCurveFactor := Reader.ReadFloat;
end;

procedure TCustomVstParameterProperty.ReadLargeStepFloatProperty(
  Reader: TReader);
begin
 FStepFloat := Reader.ReadFloat;
end;

procedure TCustomVstParameterProperty.ReadMaxProperty(Reader: TReader);
begin
 FMax := Reader.ReadFloat;
end;

procedure TCustomVstParameterProperty.ReadSmallStepFloatProperty(
  Reader: TReader);
begin
 FSmallStepFloat := Reader.ReadFloat;
end;

procedure TCustomVstParameterProperty.ReadStepFloatProperty(Reader: TReader);
begin
 FStepFloat := Reader.ReadFloat;
end;

procedure TCustomVstParameterProperty.WriteCurveFactorProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FCurveFactor);
end;

procedure TCustomVstParameterProperty.WriteLargeStepFloatProperty(
  Writer: TWriter);
begin
 Writer.WriteFloat(FLargeStepFloat);
end;

procedure TCustomVstParameterProperty.WriteMaxProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FMax);
end;

procedure TCustomVstParameterProperty.WriteSmallStepFloatProperty(
  Writer: TWriter);
begin
 Writer.WriteFloat(FSmallStepFloat);
end;

procedure TCustomVstParameterProperty.WriteStepFloatProperty(Writer: TWriter);
begin
 Writer.WriteFloat(FStepFloat);
end;

function TCustomVstParameterProperty.Parameter2VSTParameter(const Value: Single): Single;
begin
 Result := (Value - Min) / (Max - Min);
 case Curve of
  ctLogarithmic:
    begin
     Assert(FCurveFactor * Result + 1 >= 0);
     Result := Log2(FCurveFactor * Result + 1) / Log2(FCurveFactor + 1);
    end;
  ctExponential: Result := Exp(Result * Ln(FCurveFactor + 1)) - 1;
  ctFrequencyScale: if Min <> 0
                     then Result := Log2(Max / Min * Result + 1) / Log2(Max / Min)
                     else Result := Log2(Max * Result + 1) / Log2(Max);
  else
 end;
 Result := Limit(Result, 0, 1);
end;

function TCustomVstParameterProperty.Smooth(const Value: Single): Single;
begin
 // simple first order lowpass
 FSmoothStates[0] := Value + SmoothingFactor * (FSmoothStates[0] - Value);
 Result := FSmoothStates[0];
end;

procedure TCustomVstParameterProperty.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomVstParameterProperty then
  with TCustomVstParameterProperty(Dest) do
   try
    CanBeAutomated       := Self.CanBeAutomated;
    CC                   := Self.CC;
    Curve                := Self.Curve;
    CurveFactor          := Self.CurveFactor;
    Flags                := Self.Flags;
    FSmoothStates        := Self.FSmoothStates;
    LargeStepFloat       := Self.LargeStepFloat;
    LargeStepInteger     := Self.LargeStepInteger;
    Max                  := Self.Max;
    MaxInteger           := Self.MaxInteger;
    Min                  := Self.Min;
    MinInteger           := Self.MinInteger;
    ReportVST2Properties := Self.ReportVST2Properties;
    ShortLabel           := Self.ShortLabel;
    SmallStepFloat       := Self.SmallStepFloat;
    SmoothingFactor      := Self.SmoothingFactor;
    StepFloat            := Self.StepFloat;
    StepInteger          := Self.StepInteger;
    Units                := Self.Units;
    DisplayName          := Self.DisplayName;
   except
    inherited;
   end
  else inherited;
end;

procedure TCustomVstParameterProperty.SetCategoryIndex(const Value: Integer);
begin
 if VSTModule is TVSTModuleWithPrograms then
  with TVSTModuleWithPrograms(VSTModule) do
   if Assigned(ParameterCategories) then
    if (Value > 0) and (Value <= ParameterCategories.Count)
     then Category := AnsiString(ParameterCategories[Value - 1].DisplayName)
     else Category := '';
end;

procedure TCustomVstParameterProperty.SetCategoryString(const Value: AnsiString);
var
  catndx : Integer;
begin
 if Category <> Value then
  if VSTModule is TVSTModuleWithPrograms then
   with TVSTModuleWithPrograms(VSTModule) do
    if Assigned(ParameterCategories) then
     with ParameterCategories do
      begin
       if (Value = '') then
        begin
         catndx := CategoryIndex(FCategoryString);
         if (catndx >= 0) and (catndx < Count - 1) then
          begin
           Dec(Items[catndx].FParamsInCat);
           Assert(Items[catndx].FParamsInCat >= 0);
          end;
         FCategoryString := Value;
         CategoryStringChanged;
        end
       else
        begin
         FCategoryString := Value;
         CategoryStringChanged;

         catndx := CategoryIndex(FCategoryString);
         if catndx < 0 then
          with Add do
           begin
            DisplayName := string(FCategoryString);
            Inc(FParamsInCat);
           end else
         if (catndx >= 0) and (catndx < Count - 1)
          then inc(Items[catndx].FParamsInCat);
        end;
      end;
end;

procedure TCustomVstParameterProperty.CategoryStringChanged;
begin
 if Length(FCategoryString) > 23
  then SetLength(FCategoryString, 23); 
end;

procedure TCustomVstParameterProperty.SetCurve(const Value: TCurveType);
begin
 if FCurve <> Value then
  begin
   FCurve := Value;
   case FCurve of
    ctLogarithmic : if FMin <> 0 then CurveFactor := FMax / FMin;
   end;
  end;
end;

procedure TCustomVstParameterProperty.SetCurveFactor(const Value: Single);
begin
 if FCurveFactor <> Value then
  begin
   FCurveFactor := Value;
   CurveFactorChanged;
  end;
end;

procedure TCustomVstParameterProperty.CurveFactorChanged;
begin
 CalculateCurveFactor;
end;

procedure TCustomVstParameterProperty.CalculateCurveFactor;
begin
 FInvCurveFactor := 1 / FCurveFactor;
end;

procedure TCustomVstParameterProperty.SetDisplayName(const AValue: string);
var
  NewDisplayName : string;
begin
 NewDisplayName := Copy(AValue, 1, Math.Min(64, Length(AValue)));
 if NewDisplayName <> FDisplayName then
  begin
   if (ShortLabel = '') or (string(ShortLabel) = FDisplayName)
    then ShortLabel := AnsiString(NewDisplayName);
   FDisplayName := NewDisplayName;
  end;
end;

procedure TCustomVstParameterProperty.SetMax(const Value: Single);
begin
 if FMax <> Value then
  begin
   FMax := Value;
   MaximumChanged;
  end;
end;

procedure TCustomVstParameterProperty.MaximumChanged;
begin
 // nothing todo yet;
end;

procedure TCustomVstParameterProperty.SetMin(const Value: Single);
begin
 if FMin <> Value then
  begin
   FMin := Value;
   MinimumChanged;
  end;
end;

procedure TCustomVstParameterProperty.MinimumChanged;
begin
 // nothing todo yet;
end;

function TCustomVstParameterProperty.GetCategoryIndex: Integer;
begin
 if VSTModule is TVSTModuleWithPrograms then
  with TVSTModuleWithPrograms(VSTModule) do
   if Assigned(ParameterCategories)
    then Result := ParameterCategories.CategoryIndex(Category) + 1
    else Result := 0
 else Result := 0;
end;

function TCustomVstParameterProperty.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TCustomVstParameterProperty.SetUnits(AUnits: AnsiString);
begin
 if FUnits <> AUnits then
  begin
   FUnits := AUnits;
   UnitsChanged;
  end;
end;

procedure TCustomVstParameterProperty.UnitsChanged;
begin
 // nothing todo yet;
end;

procedure TCustomVstParameterProperty.SetShortLabel(const Value: AnsiString);
begin
 if FShortLabel <> Value then
  begin
   FShortLabel := Value;
   ShortLabelChanged;
  end;
end;

procedure TCustomVstParameterProperty.SetSmoothingFactor(const Value: Single);
begin
 // try..except will be removed soon, so please change all your projects
 // according to the new limits!
 try
  if FSmoothingFactor < 0
   then raise Exception.Create('SmoothingFactor needs to be above or equal zero');
  if FSmoothingFactor >= 1
   then raise Exception.Create('SmoothingFactor needs to be below one! (0 = no smoothing)');

  if FSmoothingFactor <> Value then
   begin
    FSmoothingFactor := Value;
   end;
 except
  FSmoothingFactor := 0
 end;
end;

procedure TCustomVstParameterProperty.ShortLabelChanged;
begin
 if Length(FShortLabel) > 7
  then SetLength(FShortLabel, 7);
end;


{ TCustomVstParameterProperties }

constructor TCustomVstParameterProperties.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCustomVstParameterProperty);
  FVSTModule := TVSTModuleWithPrograms(AOwner);
end;

destructor TCustomVstParameterProperties.Destroy;
begin
  while Count > 0 do Delete(0);
  inherited;
end;

procedure TCustomVstParameterProperties.WriteVSTXML;
{$IFNDEF FMX}
{$IFNDEF FPC}
var
  s : AnsiString;
  b : PAnsiChar;
{$ENDIF}
{$ENDIF}
begin
  {$IFNDEF FMX}
  {$IFNDEF FPC}
  GetMem(b, 255);
  try
   GetModuleFileNameA(Application.Handle, b, 255);
   s := b;
  finally
   FreeMem(b);
  end;
  WriteVSTXML(Copy(string(s), 1, Pos('.dll', string(s)) - 1) + '.VSTXML');
  {$ENDIF}
  {$ENDIF}
end;

procedure TCustomVstParameterProperties.WriteVSTXML(FileName: TFileName);
var
  i : Integer;
begin
  with TStringlist.Create do
  try
    SaveToFile(FileName);
    Add('<!-- =========================================================== -->');
    Add('<!-- XML definition of VST parameters ========================== -->');
    Add('<!-- Draft 0.1 ================================================= -->');
    Add('<!-- Date: ' + DateToStr(Now) + ' ========================================== -->');
    Add('<!-- =========================================================== -->');
    Add('<VSTPluginProperties>');
    Add('');
    Add(#9 + '<VSTParametersStructure>');
    Add(#9 + #9 + '<!--  Create Global Params================================== -->');
    for i := 0 to Count-1 do
    begin
      Add(#9 + #9 + '<Param name="' + string(Items[i].FDisplayName) + '"' + #9 +
                    'shortName="' + string(Items[i].FShortLabel) + '"' + #9 +
                    'id="' + IntToStr(i)+'"/>');
    end;
    Add(#9 + '</VSTParametersStructure>');
    Add('</VSTPluginProperties>');
  finally
    Free;
  end;
end;

function TCustomVstParameterProperties.Add: TCustomVstParameterProperty;
begin
  Result := TCustomVstParameterProperty(inherited Add);
end;

function TCustomVstParameterProperties.GetItem(Index: Integer): TCustomVstParameterProperty;
begin
  Result := TCustomVstParameterProperty(inherited GetItem(Index));
end;

function TCustomVstParameterProperties.Insert(Index: Integer): TCustomVstParameterProperty;
begin
  Result := TCustomVstParameterProperty(inherited Insert(Index));
end;

procedure TCustomVstParameterProperties.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TCustomVstParameterProperties.SetItem(Index: Integer; const Value: TCustomVstParameterProperty);
begin
  inherited SetItem(Index, Value);
end;

end.
