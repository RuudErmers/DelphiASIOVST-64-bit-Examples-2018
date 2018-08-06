unit DAV_GuiFixedPoint;

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
  DAV_FixedPoint;

type
  TFixed8Dot24Point = record
    X, Y : TFixed8Dot24;
  {$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
    {$IFNDEF FPC}
    constructor Create(X, Y: TFixed8Dot24); overload;
    constructor Create(X, Y: Integer); overload;
    constructor Create(X, Y: Single); overload;
    {$ENDIF}

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed8Dot24Point): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed8Dot24Point): Boolean;
    class operator Add(const Lhs, Rhs: TFixed8Dot24Point): TFixed8Dot24Point;
    class operator Subtract(const Lhs, Rhs: TFixed8Dot24Point): TFixed8Dot24Point;

    class function Zero: TFixed8Dot24Point; inline; static;
  {$ENDIF}
  end;
  PFixed8Dot24Point = ^TFixed8Dot24Point;

  TFixed8Dot24Rect = record
  case Integer of
    0: (Left, Top, Right, Bottom: TFixed8Dot24);
    1: (TopLeft, BottomRight: TFixed8Dot24Point);
  end;
  PFixed8Dot24Rect = ^TFixed8Dot24Rect;

  TFixed8Dot24PointArray = array [0..0] of TFixed8Dot24Point;
  PFixed8Dot24PointArray = ^TFixed8Dot24PointArray;
  TArrayOfFixed8Dot24Point = array of TFixed8Dot24Point;
  PArrayOfFixed8Dot24Point = ^TArrayOfFixed8Dot24Point;
  TArrayOfArrayOfFixed8Dot24Point = array of TArrayOfFixed8Dot24Point;
  PArrayOfArrayOfFixed8Dot24Point = ^TArrayOfArrayOfFixed8Dot24Point;

  TFixed16Dot16Point = record
    X, Y : TFixed16Dot16;
  {$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
    {$IFNDEF FPC}
    constructor Create(X, Y: TFixed16Dot16); overload;
    constructor Create(X, Y: Integer); overload;
    constructor Create(X, Y: Single); overload;
    {$ENDIF}

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator Add(const Lhs, Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
    class operator Subtract(const Lhs, Rhs: TFixed16Dot16Point): TFixed16Dot16Point;

    class function Zero: TFixed16Dot16Point; inline; static;
  {$ENDIF}
  end;
  PFixed16Dot16Point = ^TFixed16Dot16Point;

  TFixed16Dot16Rect = record
  case Integer of
    0: (Left, Top, Right, Bottom: TFixed16Dot16);
    1: (TopLeft, BottomRight: TFixed16Dot16Point);
  end;
  PFixed16Dot16Rect = ^TFixed16Dot16Rect;

  TFixed16Dot16PointArray = array [0..0] of TFixed16Dot16Point;
  PFixed16Dot16PointArray = ^TFixed16Dot16PointArray;
  TArrayOfFixed16Dot16Point = array of TFixed16Dot16Point;
  PArrayOfFixed16Dot16Point = ^TArrayOfFixed16Dot16Point;
  TArrayOfArrayOfFixed16Dot16Point = array of TArrayOfFixed16Dot16Point;
  PArrayOfArrayOfFixed16Dot16Point = ^TArrayOfArrayOfFixed16Dot16Point;

  TFixed24Dot8Point = record
    X, Y : TFixed24Dot8;
  {$IFDEF SUPPORTS_ENHANCED_RECORDS}
  public
    {$IFNDEF FPC}
    constructor Create(X, Y: TFixed24Dot8); overload;
    constructor Create(X, Y: Integer); overload;
    constructor Create(X, Y: Single); overload;
    {$ENDIF}

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator Add(const Lhs, Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
    class operator Subtract(const Lhs, Rhs: TFixed24Dot8Point): TFixed24Dot8Point;

    class function Zero: TFixed24Dot8Point; inline; static;
  {$ENDIF}
  end;
  PFixed24Dot8Point = ^TFixed24Dot8Point;

  TFixed24Dot8Rect = record
  case Integer of
    0: (Left, Top, Right, Bottom: TFixed24Dot8);
    1: (TopLeft, BottomRight: TFixed24Dot8Point);
  end;
  PFixed24Dot8Rect = ^TFixed24Dot8Rect;

  TFixed24Dot8PointArray = array [0..0] of TFixed24Dot8Point;
  PFixed24Dot8PointArray = ^TFixed24Dot8PointArray;
  TArrayOfFixed24Dot8Point = array of TFixed24Dot8Point;
  PArrayOfFixed24Dot8Point = ^TArrayOfFixed24Dot8Point;
  TArrayOfArrayOfFixed24Dot8Point = array of TArrayOfFixed24Dot8Point;
  PArrayOfArrayOfFixed24Dot8Point = ^TArrayOfArrayOfFixed24Dot8Point;

function Fixed8Dot24Point(X, Y: TFixed8Dot24): TFixed8Dot24Point; overload;
function Fixed16Dot16Point(X, Y: TFixed16Dot16): TFixed16Dot16Point; overload;
function Fixed24Dot8Point(X, Y: TFixed24Dot8): TFixed24Dot8Point; overload;

function Fixed8Dot24Point(X, Y: Single): TFixed8Dot24Point; overload;
function Fixed16Dot16Point(X, Y: Single): TFixed16Dot16Point; overload;
function Fixed24Dot8Point(X, Y: Single): TFixed24Dot8Point; overload;

function Fixed8Dot24Point(X, Y: Integer): TFixed8Dot24Point; overload;
function Fixed16Dot16Point(X, Y: Integer): TFixed16Dot16Point; overload;
function Fixed24Dot8Point(X, Y: Integer): TFixed24Dot8Point; overload;

function Fixed8Dot24Rect(Left, Top, Right, Bottom: TFixed8Dot24): TFixed8Dot24Rect; overload;
function Fixed16Dot16Rect(Left, Top, Right, Bottom: TFixed16Dot16): TFixed16Dot16Rect; overload;
function Fixed24Dot8Rect(Left, Top, Right, Bottom: TFixed24Dot8): TFixed24Dot8Rect; overload;

function Fixed8Dot24Rect(Left, Top, Right, Bottom: Single): TFixed8Dot24Rect; overload;
function Fixed16Dot16Rect(Left, Top, Right, Bottom: Single): TFixed16Dot16Rect; overload;
function Fixed24Dot8Rect(Left, Top, Right, Bottom: Single): TFixed24Dot8Rect; overload;

function Fixed8Dot24Rect(Left, Top, Right, Bottom: Integer): TFixed8Dot24Rect; overload;
function Fixed16Dot16Rect(Left, Top, Right, Bottom: Integer): TFixed16Dot16Rect; overload;
function Fixed24Dot8Rect(Left, Top, Right, Bottom: Integer): TFixed24Dot8Rect; overload;


implementation

function Fixed8Dot24Point(X, Y: TFixed8Dot24): TFixed8Dot24Point;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Fixed16Dot16Point(X, Y: TFixed16Dot16): TFixed16Dot16Point;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Fixed24Dot8Point(X, Y: TFixed24Dot8): TFixed24Dot8Point;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Fixed8Dot24Point(X, Y: Single): TFixed8Dot24Point; overload;
begin
  Result.X := ConvertToFixed8Dot24(X);
  Result.Y := ConvertToFixed8Dot24(Y);
end;

function Fixed16Dot16Point(X, Y: Single): TFixed16Dot16Point; overload;
begin
  Result.X := ConvertToFixed16Dot16(X);
  Result.Y := ConvertToFixed16Dot16(Y);
end;

function Fixed24Dot8Point(X, Y: Single): TFixed24Dot8Point; overload;
begin
  Result.X := ConvertToFixed24Dot8(X);
  Result.Y := ConvertToFixed24Dot8(Y);
end;

function Fixed8Dot24Point(X, Y: Integer): TFixed8Dot24Point; overload;
begin
  Result.X := ConvertToFixed8Dot24(X);
  Result.Y := ConvertToFixed8Dot24(Y);
end;

function Fixed16Dot16Point(X, Y: Integer): TFixed16Dot16Point; overload;
begin
  Result.X := ConvertToFixed16Dot16(X);
  Result.Y := ConvertToFixed16Dot16(Y);
end;

function Fixed24Dot8Point(X, Y: Integer): TFixed24Dot8Point; overload;
begin
  Result.X := ConvertToFixed24Dot8(X);
  Result.Y := ConvertToFixed24Dot8(Y);
end;

function Fixed8Dot24Rect(Left, Top, Right, Bottom: TFixed8Dot24): TFixed8Dot24Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function Fixed16Dot16Rect(Left, Top, Right, Bottom: TFixed16Dot16): TFixed16Dot16Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function Fixed24Dot8Rect(Left, Top, Right, Bottom: TFixed24Dot8): TFixed24Dot8Rect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

function Fixed8Dot24Rect(Left, Top, Right, Bottom: Single): TFixed8Dot24Rect;
begin
  Result.Left := ConvertToFixed8Dot24(Left);
  Result.Top := ConvertToFixed8Dot24(Top);
  Result.Right := ConvertToFixed8Dot24(Right);
  Result.Bottom := ConvertToFixed8Dot24(Bottom);
end;

function Fixed16Dot16Rect(Left, Top, Right, Bottom: Single): TFixed16Dot16Rect;
begin
  Result.Left := ConvertToFixed16Dot16(Left);
  Result.Top := ConvertToFixed16Dot16(Top);
  Result.Right := ConvertToFixed16Dot16(Right);
  Result.Bottom := ConvertToFixed16Dot16(Bottom);
end;

function Fixed24Dot8Rect(Left, Top, Right, Bottom: Single): TFixed24Dot8Rect;
begin
  Result.Left := ConvertToFixed24Dot8(Left);
  Result.Top := ConvertToFixed24Dot8(Top);
  Result.Right := ConvertToFixed24Dot8(Right);
  Result.Bottom := ConvertToFixed24Dot8(Bottom);
end;

function Fixed8Dot24Rect(Left, Top, Right, Bottom: Integer): TFixed8Dot24Rect;
begin
  Result.Left := ConvertToFixed8Dot24(Left);
  Result.Top := ConvertToFixed8Dot24(Top);
  Result.Right := ConvertToFixed8Dot24(Right);
  Result.Bottom := ConvertToFixed8Dot24(Bottom);
end;

function Fixed16Dot16Rect(Left, Top, Right, Bottom: Integer): TFixed16Dot16Rect;
begin
  Result.Left := ConvertToFixed16Dot16(Left);
  Result.Top := ConvertToFixed16Dot16(Top);
  Result.Right := ConvertToFixed16Dot16(Right);
  Result.Bottom := ConvertToFixed16Dot16(Bottom);
end;

function Fixed24Dot8Rect(Left, Top, Right, Bottom: Integer): TFixed24Dot8Rect;
begin
  Result.Left := ConvertToFixed24Dot8(Left);
  Result.Top := ConvertToFixed24Dot8(Top);
  Result.Right := ConvertToFixed24Dot8(Right);
  Result.Bottom := ConvertToFixed24Dot8(Bottom);
end;


{$IFDEF SUPPORTS_ENHANCED_RECORDS}

{ TFixed8Dot24Point }

{$IFNDEF FPC}
constructor TFixed8Dot24Point.Create(X, Y: TFixed8Dot24);
begin
  Self.X := X;
  Self.Y := Y;
end;

constructor TFixed8Dot24Point.Create(X, Y: Integer);
begin
  Self.X := ConvertToFixed8Dot24(X);
  Self.Y := ConvertToFixed8Dot24(Y);
end;

constructor TFixed8Dot24Point.Create(X, Y: Single);
begin
  Self.X := ConvertToFixed8Dot24(X);
  Self.Y := ConvertToFixed8Dot24(Y);
end;
{$ENDIF}

class operator TFixed8Dot24Point.Equal(const Lhs,
  Rhs: TFixed8Dot24Point): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TFixed8Dot24Point.NotEqual(const Lhs,
  Rhs: TFixed8Dot24Point): Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TFixed8Dot24Point.Add(const Lhs,
  Rhs: TFixed8Dot24Point): TFixed8Dot24Point;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TFixed8Dot24Point.Subtract(const Lhs,
  Rhs: TFixed8Dot24Point): TFixed8Dot24Point;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class function TFixed8Dot24Point.Zero: TFixed8Dot24Point;
begin
  Result.X.Fixed := 0;
  Result.Y.Fixed := 0;
end;


{ TFixed16Dot16Point }

{$IFNDEF FPC}
constructor TFixed16Dot16Point.Create(X, Y: TFixed16Dot16);
begin
  Self.X := X;
  Self.Y := Y;
end;

constructor TFixed16Dot16Point.Create(X, Y: Integer);
begin
  Self.X := ConvertToFixed16Dot16(X);
  Self.Y := ConvertToFixed16Dot16(Y);
end;

constructor TFixed16Dot16Point.Create(X, Y: Single);
begin
  Self.X := ConvertToFixed16Dot16(X);
  Self.Y := ConvertToFixed16Dot16(Y);
end;
{$ENDIF}

class operator TFixed16Dot16Point.Equal(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TFixed16Dot16Point.NotEqual(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TFixed16Dot16Point.Add(const Lhs,
  Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TFixed16Dot16Point.Subtract(const Lhs,
  Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result.X := Lhs.X - Rhs.X;
  Result.Y := Lhs.Y - Rhs.Y;
end;

class function TFixed16Dot16Point.Zero: TFixed16Dot16Point;
begin
  Result.X.Fixed := 0;
  Result.Y.Fixed := 0;
end;


{ TFixed24Dot8Point }

{$IFNDEF FPC}
constructor TFixed24Dot8Point.Create(X, Y: TFixed24Dot8);
begin
  Self.X := X;
  Self.Y := Y;
end;

constructor TFixed24Dot8Point.Create(X, Y: Integer);
begin
  Self.X := ConvertToFixed24Dot8(X);
  Self.Y := ConvertToFixed24Dot8(Y);
end;

constructor TFixed24Dot8Point.Create(X, Y: Single);
begin
  Self.X := ConvertToFixed24Dot8(X);
  Self.Y := ConvertToFixed24Dot8(Y);
end;
{$ENDIF}

class operator TFixed24Dot8Point.Equal(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := (Lhs.X = Rhs.X) and (Lhs.Y = Rhs.Y);
end;

class operator TFixed24Dot8Point.NotEqual(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := (Lhs.X <> Rhs.X) or (Lhs.Y <> Rhs.Y);
end;

class operator TFixed24Dot8Point.Add(const Lhs,
  Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result.X := Lhs.X + Rhs.X;
  Result.Y := Lhs.Y + Rhs.Y;
end;

class operator TFixed24Dot8Point.Subtract(const Lhs,
  Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result.X := Lhs.X - Rhs.X;
  Result.Y := Lhs.Y - Rhs.Y;
end;

class function TFixed24Dot8Point.Zero: TFixed24Dot8Point;
begin
  Result.X.Fixed := 0;
  Result.Y.Fixed := 0;
end;
{$ENDIF}

end.
