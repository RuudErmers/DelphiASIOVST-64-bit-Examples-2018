unit DAV_SECommon;

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
//  SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{$ALIGN ON}
{$MINENUMSIZE 4}

const
  CSeSdkVersion : Integer = 2230;

  // the 'magic number' that identifies a SynthEdit module (spells SEPL)
  CSepMagic  = $5345504C;
  CSepMagic2 = CSepMagic + 1;

type
  // plug datatype
  TSEPlugDataType = ({$IFDEF DELPHI6_UP}dtNone = -1, {$ENDIF}dtEnum, dtText,
       dtMidi2, dtDouble, dtBoolean, dtFSample, dtSingle, dtVstParam,
       dtInteger, dtBlob {$IFDEF DELPHI6_UP}, dtExperimental,
       dtFilterReference = $666{$ENDIF});

  // plug direction (use drNone in order to create a 32bit type)
  TSEDirection = (drIn {$IFDEF DELPHI6_UP}= 0 {$ENDIF}, drOut, drContainerIO,
    drParameter {$IFDEF DELPHI6_UP}, drNone = $7FFFFFFF, drFeature = drIn,
    drCntrl = drOut{$ENDIF});

  TSeSdkString = string;
  TSeSdkString2 = WideString; 

  PEnumEntry = ^TEnumEntry;  
  TEnumEntry = record
    Index : Integer;
    Value : Integer;
    Text  : TSeSdkString2;
  end;

  // TODO:
  //  - make more like STL
  //  - defer extracting text unless CurrentItem called
  //  - hold pointer-to string, not copy-of. }

  TItEnumList = class(TObject)
  private
    FEnumList  : TSeSdkString2;
    FCurrent   : TEnumEntry;
    FRangeMode : Boolean;
    FRangeLo   : Integer; // also used as current position within string
    FRangeHi   : Integer;
  public
    constructor Create(var AEnumList: TSeSdkString2); virtual;
    function CurrentItem: PEnumEntry;
    function IsDone: Boolean;
    procedure Next;
//    TItEnumList &operator++{Next;return *this;};//Prefix increment
    procedure First;
    function Size: Integer;
    function FindValue(AValue: Integer): Boolean;
    function FindIndex(AIndex: Integer): Boolean;
    class function IsValidValue(var AEnumList: TSeSdkString2; AValue: Integer): Boolean;
    class function ForceValidValue(var AEnumList: TSeSdkString2; AValue: Integer): Integer;
    function IsRange: Boolean;
    function RangeHi: Integer;
    function RangeLo: Integer;
  end;

implementation

uses
  SysUtils;

constructor TItEnumList.Create(var AEnumList: TSeSdkString2);
var
  list : TSeSdkString2;
  lval : TSeSdkString2;
  p    : Integer;
begin
 FEnumList := AEnumList;

// two formats are allowed, a list of values or a range

 if AnsiPos('range', FEnumList) = 1 then // how about upper/mixed case?
  try
   FRangeMode := True;
   List := FEnumList;     // List = 'range 3, 6'

   Delete(List, 1, 5);    // List = ' 3, 6'

   p := Pos(',', List);
   assert(p > 0);         // no range specified

   lval := List;          // lval = ' 3, 6'
   Copy(lval, 1, p);      // lval = ' 3'
   Trim(lval);            // lval = '3'
   FRangeLo := StrToInt(lval);

   Delete(List, 1, p);    // List = ' 6'
   Trim(lval);            // List = '6'
   FRangeHi := StrToInt(list);
  except
   FRangeMode := False;
  end
 else FRangeMode := False;
end;

procedure TItEnumList.First;
begin
 // two formats are allowed, a list of values or a range
 if FRangeMode then
  begin
   FCurrent.Value := FRangeLo - 1;
//   FCurrent.text.Format(_T('%d'), FRangeLo);
//   FCurrent.index = 0;
  end
 else
  begin
   FRangeLo := 0;
   FCurrent.Value := -1;
  end;
 FCurrent.Index := -1; // anticipate initial next
 Next;
end;

procedure TItEnumList.Next;
var
  p, st, en       : Integer;
  SubStringLength : Integer;
  p_eq            : Integer;
  id_str          : TSeSdkString2;
begin
  Inc(FCurrent.Index);
  Inc(FCurrent.Value);

  if FRangeMode then
   begin
    FCurrent.text := IntToStr(FCurrent.Value);
    if(FCurrent.Value > FRangeHi) then
     begin
      FCurrent.index := -1;
      exit;
     end;
   end
  else
   begin
//    begin
    // find next comma
//    Integer p = FEnumList.Find(',', FRangeLo);
    p := Pos(FEnumList, ',');
    if (p = -1) // none left
     then
      begin
       p := Length(FEnumList);
       if FRangeLo >= p then // then we are done
        begin
         FCurrent.Index := -1;
         exit;
        end;
     end;

    SubStringLength := p - FRangeLo;
    FCurrent.text := Copy(FEnumList, FRangeLo, SubStringLength);

    p_eq := Pos('=', FCurrent.Text);
    if p_eq > 0 then
     begin
      id_str := Copy(FCurrent.text, p_eq + 1, Length(FCurrent.text) - 1);
      FCurrent.text := Copy(FCurrent.text, 0, p_eq);
      FCurrent.Value := StrToInt(id_str);
     end;

    FRangeLo := p + 1;

    // Trim spaces from start and end of text
    st := Length(Trim(FCurrent.text));

    if st < 0 // nothing but spaces?
     then FCurrent.text := ''
     else
      begin
       en := Length(FCurrent.text);
       while (en > 0) and (FCurrent.text[en - 1] = ' ')
        do Dec(en);

       if (st > 0) or (en < Length(FCurrent.text))
        then FCurrent.text := Copy(FCurrent.text, st, en - st);
      end;
  end;
end;

function TItEnumList.Size: Integer;
var
  i, sz : Integer;
begin
 if FRangeMode
  then result := 1 + abs(FRangeHi - FRangeLo)
  else
   begin
    // count number of commas
    sz := 1;
    for i := Length(FEnumList) - 1 downto 1 do
     if FEnumList[i] = ',' then inc(sz);
    result := sz;
   end;
end;

function TItEnumList.FindValue(AValue: Integer): Boolean;
begin
 // could be specialied for ranges
 First;
 result := True;
 while not IsDone do
  if (CurrentItem.Value = AValue)
   then Exit
   else Next;
 result := False;
end;

function TItEnumList.FindIndex(AIndex: Integer): Boolean;
begin
 First;
 result := True;
 while not IsDone do
  if (CurrentItem.index = AIndex)
   then Exit
   else Next;
 result := False;
end;

class function TItEnumList.IsValidValue(var AEnumList: TSeSdkString2; AValue: Integer): Boolean;
var
  itr : TItEnumList;
begin
  itr := TItEnumList.Create(AEnumList);
  result := itr.FindValue(AValue);
end;

// ensure a Value is one of the valid choices, if not return first item, if no items avail return 0
class function TItEnumList.ForceValidValue(var AEnumList: TSeSdkString2; AValue :Integer): Integer;
var
  itr : TItEnumList;
begin
 itr := TItEnumList.Create(AEnumList);
 if itr.FindValue(AValue)
  then begin result := AValue; exit; end;

 itr.First;
 if not itr.IsDone
  then result := itr.CurrentItem.Value
  else result := 0;
end;

(*
function TItEnumList.ValueToNormalised( Integer AValue ): Single;
var
  NumberOfValues : Single;
  Index          : Integer;
begin
 NumberOfValues := itr.Size;
 index := floor(AValue * NumberOfValues + 0.5); // floor important for -ve numbers
 itr.FindIndex(index);
 assert(not itr.IsDone);
 setValue(IntToString(itr.CurrentItem.Value));
end;
*)

function TItEnumList.IsRange: Boolean;
begin
 result := FRangeMode;
end;

function TItEnumList.RangeHi: Integer;
begin
 result := FRangeHi;
end;

function TItEnumList.RangeLo: Integer;
begin
 result := FRangeLo;
end;

function TItEnumList.CurrentItem: PEnumEntry;
begin
 assert(not IsDone);
 result := @FCurrent;
end;

function TItEnumList.IsDone: Boolean;
begin
 result := FCurrent.index = -1;
end;

end.
