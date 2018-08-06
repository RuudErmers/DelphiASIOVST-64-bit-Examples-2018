unit VoiceList;

interface

uses
  Classes, RTLConsts, SineSynthVoice;

type
  TVoiceList = class(TObject)
  private
    FList        : PPointerList;
    FCount       : Integer;
    FCapacity    : Integer;
    FOwnsObjects : Boolean;
  protected
    function Get(Index: Integer): TSineSynthVoice;
    procedure Grow; virtual;
    procedure Put(Index: Integer; SineSynthVoice: TSineSynthVoice);
    procedure Notify(SineSynthVoice: TSineSynthVoice; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create; overload; virtual;
    constructor Create(AOwnsObjects: Boolean); overload; virtual;
    destructor Destroy; override;
    function Add(SineSynthVoice: TSineSynthVoice): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TVoiceList;
    function Extract(SineSynthVoice: TSineSynthVoice): TSineSynthVoice;
    function First: TSineSynthVoice;
    function IndexOf(SineSynthVoice: TSineSynthVoice): Integer;
    procedure Insert(Index: Integer; SineSynthVoice: TSineSynthVoice);
    function Last: TSineSynthVoice;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(SineSynthVoice: TSineSynthVoice): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    procedure Assign(ListA: TVoiceList; AOperator: TListAssignOp = laCopy; ListB: TVoiceList = nil);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: TSineSynthVoice read Get write Put; default;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property List: PPointerList read FList;
  end;

implementation

{ TVoiceList }

constructor TVoiceList.Create;
begin
 inherited Create;
 FOwnsObjects := True;
end;

constructor TVoiceList.Create(AOwnsObjects: Boolean);
begin
 inherited Create;
 FOwnsObjects := AOwnsObjects;
end;

destructor TVoiceList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

function TVoiceList.Add(SineSynthVoice: TSineSynthVoice): Integer;
begin
 Result := FCount;
 if Result = FCapacity then Grow;
 FList^[Result] := SineSynthVoice;
 Inc(FCount);
 if SineSynthVoice <> nil then Notify(SineSynthVoice, lnAdded);
end;

procedure TVoiceList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TVoiceList.Delete(Index: Integer);
var
  Temp: TSineSynthVoice;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Temp := Items[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

class procedure TVoiceList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

class procedure TVoiceList.Error(Msg: PResStringRec; Data: Integer);
begin
  TVoiceList.Error(LoadResString(Msg), Data);
end;

procedure TVoiceList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TVoiceList.Expand: TVoiceList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TVoiceList.First: TSineSynthVoice;
begin
  Result := Get(0);
end;

function TVoiceList.Get(Index: Integer): TSineSynthVoice;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

procedure TVoiceList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TVoiceList.IndexOf(SineSynthVoice: TSineSynthVoice): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> SineSynthVoice) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TVoiceList.Insert(Index: Integer; SineSynthVoice: TSineSynthVoice);
begin
 if (Index < 0) or (Index > FCount)
  then Error(@SListIndexError, Index);
 if FCount = FCapacity
  then Grow;
 if Index < FCount
  then System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(Pointer));
 FList^[Index] := SineSynthVoice;
 Inc(FCount);
 if SineSynthVoice <> nil
  then Notify(SineSynthVoice, lnAdded);
end;

function TVoiceList.Last: TSineSynthVoice;
begin
  Result := Get(FCount - 1);
end;

procedure TVoiceList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TVoiceList.Put(Index: Integer; SineSynthVoice: TSineSynthVoice);
var
  Temp: Pointer;
begin
 if (Index < 0) or (Index >= FCount)
  then Error(@SListIndexError, Index);
 if SineSynthVoice <> FList^[Index] then
  begin
   Temp := FList^[Index];
   FList^[Index] := SineSynthVoice;
   if Temp <> nil then Notify(Temp, lnDeleted);
   if SineSynthVoice <> nil then Notify(SineSynthVoice, lnAdded);
  end;
end;

function TVoiceList.Remove(SineSynthVoice: TSineSynthVoice): Integer;
begin
 Result := IndexOf(SineSynthVoice);
 if Result >= 0 then Delete(Result);
end;

procedure TVoiceList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

procedure TVoiceList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TVoiceList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
 if (NewCount < 0) or (NewCount > MaxListSize)
  then Error(@SListCountError, NewCount);
 if NewCount > FCapacity then SetCapacity(NewCount);
 if NewCount > FCount
  then FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
 FCount := NewCount;
end;

procedure QuickSort(SorTVoiceList: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
 repeat
  I := L;
  J := R;
  P := SorTVoiceList^[(L + R) shr 1];
  repeat
   while SCompare(SorTVoiceList^[I], P) < 0 do Inc(I);
   while SCompare(SorTVoiceList^[J], P) > 0 do Dec(J);
   if I <= J then
    begin
     T := SorTVoiceList^[I];
     SorTVoiceList^[I] := SorTVoiceList^[J];
     SorTVoiceList^[J] := T;
     Inc(I);
     Dec(J);
    end;
  until I > J;
  if L < J then QuickSort(SorTVoiceList, L, J, SCompare);
  L := I;
 until I >= R;
end;

procedure TVoiceList.Sort(Compare: TListSortCompare);
begin
 if (FList <> nil) and (Count > 0) then QuickSort(FList, 0, Count - 1, Compare);
end;

function TVoiceList.Extract(SineSynthVoice: TSineSynthVoice): TSineSynthVoice;
var I: Integer;
begin
 Result := nil;
 I := IndexOf(SineSynthVoice);
 if I >= 0 then
  begin
   Result := SineSynthVoice;
   FList^[I] := nil;
   Delete(I);
   Notify(Result, lnExtracted);
  end;
end;

procedure TVoiceList.Notify(SineSynthVoice: TSineSynthVoice; Action: TListNotification);
begin
 if OwnsObjects then if Action = lnDeleted then SineSynthVoice.Free;
end;

procedure TVoiceList.Assign(ListA: TVoiceList; AOperator: TListAssignOp; ListB: TVoiceList);
var
  I: Integer;
  LTemp, LSource: TVoiceList;
begin
 // ListB given?
 if ListB <> nil then
  begin
   LSource := ListB;
   Assign(ListA);
  end
 else LSource := ListA;

 // on with the show
 case AOperator of

    // 12345, 346 = 346 : only those in the new list
    laCopy:
      begin
        Clear;
        Capacity := LSource.Capacity;
        for I := 0 to LSource.Count - 1 do
          Add(LSource[I]);
      end;

    // 12345, 346 = 34 : intersection of the two lists
    laAnd:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) = -1 then
          Delete(I);

    // 12345, 346 = 123456 : union of the two lists
    laOr:
      for I := 0 to LSource.Count - 1 do
        if IndexOf(LSource[I]) = -1 then
          Add(LSource[I]);

    // 12345, 346 = 1256 : only those not in both lists
    laXor:
      begin
        LTemp := TVoiceList.Create(False); // Temp holder of 4 byte values
        try
          LTemp.Capacity := LSource.Count;
          for I := 0 to LSource.Count - 1 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          for I := Count - 1 downto 0 do
            if LSource.IndexOf(Items[I]) <> -1 then
              Delete(I);
          I := Count + LTemp.Count;
          if Capacity < I then
            Capacity := I;
          for I := 0 to LTemp.Count - 1 do
            Add(LTemp[I]);
        finally
          LTemp.Free;
        end;
      end;

    // 12345, 346 = 125 : only those unique to source
    laSrcUnique:
      for I := Count - 1 downto 0 do
        if LSource.IndexOf(Items[I]) <> -1 then
          Delete(I);

    // 12345, 346 = 6 : only those unique to dest
    laDestUnique:
      begin
        LTemp := TVoiceList.Create(False);
        try
          LTemp.Capacity := LSource.Count;
          for I := LSource.Count - 1 downto 0 do
            if IndexOf(LSource[I]) = -1 then
              LTemp.Add(LSource[I]);
          Assign(LTemp);
        finally
          LTemp.Free;
        end;
      end;
  end;
end;

end.
