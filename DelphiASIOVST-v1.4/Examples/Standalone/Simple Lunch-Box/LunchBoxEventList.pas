unit LunchBoxEventList;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, RTLConsts, LunchBoxEvent;

type
  TLunchBoxEventList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
    FCapacity: Integer;
    FOwnsObjects: Boolean;
    function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
  protected
    function Get(Index: Integer): TLunchBoxSample;
    procedure Grow; virtual;
    procedure Put(Index: Integer; SimpleSamplerVoice: TLunchBoxSample);
    procedure Notify(SimpleSamplerVoice: TLunchBoxSample; Action: TListNotification); virtual;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    constructor Create; overload; virtual;
    constructor Create(AOwnsObjects: Boolean); overload; virtual;
    destructor Destroy; override;
    function Add(SimpleSamplerVoice: TLunchBoxSample): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TLunchBoxEventList;
    function Extract(SimpleSamplerVoice: TLunchBoxSample): TLunchBoxSample;
    function First: TLunchBoxSample;
    function IndexOf(SimpleSamplerVoice: TLunchBoxSample): Integer;
    procedure Insert(Index: Integer; SimpleSamplerVoice: TLunchBoxSample);
    function Last: TLunchBoxSample;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(SimpleSamplerVoice: TLunchBoxSample): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    {$IFNDEF FPC}
    procedure Assign(ListA: TLunchBoxEventList; AOperator: TListAssignOp = laCopy; ListB: TLunchBoxEventList = nil);
    {$ENDIF}
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: TLunchBoxSample read Get write Put; default;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property List: PPointerList read FList;
  end;

implementation

{ TLunchBoxEventList }

constructor TLunchBoxEventList.Create;
begin
 inherited Create;
 FOwnsObjects := True;
end;

constructor TLunchBoxEventList.Create(AOwnsObjects: Boolean);
begin
 inherited Create;
 FOwnsObjects := AOwnsObjects;
end;

destructor TLunchBoxEventList.Destroy;
begin
 inherited Destroy;
 Clear;
end;

function TLunchBoxEventList.Add(SimpleSamplerVoice: TLunchBoxSample): Integer;
begin
 Result := FCount;
 if Result = FCapacity then Grow;
 FList^[Result] := SimpleSamplerVoice;
 Inc(FCount);
 if SimpleSamplerVoice <> nil then Notify(SimpleSamplerVoice, lnAdded);
end;

procedure TLunchBoxEventList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TLunchBoxEventList.Delete(Index: Integer);
var Temp: TLunchBoxSample;
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

class procedure TLunchBoxEventList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

class procedure TLunchBoxEventList.Error(Msg: PResStringRec; Data: Integer);
begin
  TLunchBoxEventList.Error(LoadResString(Msg), Data);
end;

procedure TLunchBoxEventList.Exchange(Index1, Index2: Integer);
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

function TLunchBoxEventList.Expand: TLunchBoxEventList;
begin
 if FCount = FCapacity then Grow;
 Result := Self;
end;

function TLunchBoxEventList.First: TLunchBoxSample;
begin
  Result := Get(0);
end;

function TLunchBoxEventList.Get(Index: Integer): TLunchBoxSample;
begin
 if (Index < 0) or (Index >= FCount)
  then Error(@SListIndexError, Index);
 Result := FList^[Index];
end;

procedure TLunchBoxEventList.Grow;
var Delta: Integer;
begin
 if FCapacity > 64
  then Delta := FCapacity div 4
  else if FCapacity > 8 then Delta := 16 else Delta := 4;
 SetCapacity(FCapacity + Delta);
end;

function TLunchBoxEventList.IndexOf(SimpleSamplerVoice: TLunchBoxSample): Integer;
begin
 Result := 0;
 while (Result < FCount) and (FList^[Result] <> SimpleSamplerVoice)
  do Inc(Result);
 if Result = FCount
  then Result := -1;
end;

procedure TLunchBoxEventList.Insert(Index: Integer; SimpleSamplerVoice: TLunchBoxSample);
begin
 if (Index < 0) or (Index > FCount)
  then Error(@SListIndexError, Index);
 if FCount = FCapacity
  then Grow;
 if Index < FCount
  then System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(Pointer));
 FList^[Index] := SimpleSamplerVoice;
 Inc(FCount);
 if SimpleSamplerVoice <> nil
  then Notify(SimpleSamplerVoice, lnAdded);
end;

function TLunchBoxEventList.Last: TLunchBoxSample;
begin
 Result := Get(FCount - 1);
end;

procedure TLunchBoxEventList.Move(CurIndex, NewIndex: Integer);
var Item: Pointer;
begin
 if CurIndex <> NewIndex then
  begin
   if (NewIndex < 0) or (NewIndex >= FCount)
    then Error(@SListIndexError, NewIndex);
   Item := Get(CurIndex);
   FList^[CurIndex] := nil;
   Delete(CurIndex);
   Insert(NewIndex, nil);
   FList^[NewIndex] := Item;
  end;
end;

procedure TLunchBoxEventList.Put(Index: Integer; SimpleSamplerVoice: TLunchBoxSample);
var Temp: Pointer;
begin
 if (Index < 0) or (Index >= FCount)
  then Error(@SListIndexError, Index);
 if SimpleSamplerVoice <> FList^[Index] then
  begin
   Temp := FList^[Index];
   FList^[Index] := SimpleSamplerVoice;
   if Temp <> nil then Notify(Temp, lnDeleted);
   if SimpleSamplerVoice <> nil then Notify(SimpleSamplerVoice, lnAdded);
  end;
end;

function TLunchBoxEventList.Remove(SimpleSamplerVoice: TLunchBoxSample): Integer;
begin
 Result := IndexOf(SimpleSamplerVoice);
 if Result >= 0 then Delete(Result);
end;

procedure TLunchBoxEventList.Pack;
var I: Integer;
begin
 for I := FCount - 1 downto 0 do
  if Items[I] = nil
   then Delete(I);
end;

procedure TLunchBoxEventList.SetCapacity(NewCapacity: Integer);
begin
 if (NewCapacity < FCount) or (NewCapacity > MaxListSize)
  then Error(@SListCapacityError, NewCapacity);
 if NewCapacity <> FCapacity then
  begin
   ReallocMem(FList, NewCapacity * SizeOf(Pointer));
   FCapacity := NewCapacity;
  end;
end;

procedure TLunchBoxEventList.SetCount(NewCount: Integer);
var I: Integer;
begin
 if (NewCount < 0) or (NewCount > MaxListSize)
  then Error(@SListCountError, NewCount);
 if NewCount > FCapacity then SetCapacity(NewCount);
 if NewCount > FCount
  then FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else for I := FCount - 1 downto NewCount do Delete(I);
 FCount := NewCount;
end;

procedure QuickSort(SorTLunchBoxEventList: PPointerList; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
 repeat
  I := L;
  J := R;
  P := SorTLunchBoxEventList^[(L + R) shr 1];
  repeat
   while SCompare(SorTLunchBoxEventList^[I], P) < 0 do Inc(I);
   while SCompare(SorTLunchBoxEventList^[J], P) > 0 do Dec(J);
   if I <= J then
    begin
     T := SorTLunchBoxEventList^[I];
     SorTLunchBoxEventList^[I] := SorTLunchBoxEventList^[J];
     SorTLunchBoxEventList^[J] := T;
     Inc(I);
     Dec(J);
    end;
  until I > J;
  if L < J then QuickSort(SorTLunchBoxEventList, L, J, SCompare);
  L := I;
 until I >= R;
end;

procedure TLunchBoxEventList.Sort(Compare: TListSortCompare);
begin
 if (FList <> nil) and (Count > 0) then QuickSort(FList, 0, Count - 1, Compare);
end;

function TLunchBoxEventList.Extract(SimpleSamplerVoice: TLunchBoxSample): TLunchBoxSample;
var I: Integer;
begin
 Result := nil;
 I := IndexOf(SimpleSamplerVoice);
 if I >= 0 then
  begin
   Result := SimpleSamplerVoice;
   FList^[I] := nil;
   Delete(I);
   Notify(Result, lnExtracted);
  end;
end;

procedure TLunchBoxEventList.Notify(SimpleSamplerVoice: TLunchBoxSample; Action: TListNotification);
begin
 if OwnsObjects then if Action = lnDeleted then SimpleSamplerVoice.Free;
end;

{$IFNDEF FPC}
procedure TLunchBoxEventList.Assign(ListA: TLunchBoxEventList; AOperator: TListAssignOp; ListB: TLunchBoxEventList);
var
  I: Integer;
  LTemp, LSource: TLunchBoxEventList;
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
        LTemp := TLunchBoxEventList.Create(False); // Temp holder of 4 byte values
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
        LTemp := TLunchBoxEventList.Create(False);
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
{$ENDIF}

function TLunchBoxEventList.FindInstanceOf(AClass: TClass; AExact: Boolean;
  AStartAt: Integer): Integer;
var I: Integer;
begin
 Result := -1;
 for I := AStartAt to Count - 1 do
  if (AExact and (Items[I].ClassType = AClass)) or
     (not AExact and Items[I].InheritsFrom(AClass))
   then
    begin
     Result := I;
     break;
    end;
end;

end.
