unit TestMemoryUtils;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Classes, {$IFDEF FPC}fpcunit, testutils, testregistry; {$ELSE}
  TestFramework; {$ENDIF}

type
  TTestMemoryUtils = class(TTestCase)
  public
    {$IFNDEF FPC}
    constructor Create(MethodName: string); override;
    {$ENDIF}
  published
    procedure TestGetAlignedMemory;
    procedure TestReallocateAlignedMemory;
    procedure TestSmallBlocks;
  end;

implementation

uses
  SysUtils, DAV_MemoryUtils;

{$IFNDEF FPC}
constructor TTestMemoryUtils.Create(MethodName: string);
begin
 inherited;
 FailsOnMemoryLeak := True;
 FailsOnMemoryRecovery := True;
end;
{$ENDIF}

procedure TTestMemoryUtils.TestGetAlignedMemory;
var
  Index : Integer;
  Data  : Pointer;
begin
 for Index := 0 to $1FF do
  begin
   GetAlignedMemory(Data, Index);
   CheckEquals(0, Integer(Data) and $F);
   FreeAlignedMemory(Data);
  end;
end;

procedure TTestMemoryUtils.TestReallocateAlignedMemory;
var
  Index : Integer;
  Data  : Pointer;
begin
 Data := nil;
 ReallocateAlignedMemory(Data, $A);
 for Index := 0 to $1FF do
  begin
   ReallocateAlignedMemory(Data, Index);
   CheckEquals(0, Integer(Data) and $F);
  end;
 ReallocateAlignedMemory(Data, 0);
end;

procedure TTestMemoryUtils.TestSmallBlocks;
var
  Data  : Pointer;
begin
 Data := nil;
 GetAlignedMemory(Data, $10);
 ReallocateAlignedMemory(Data, $20);
 FreeAlignedMemory(Data);
end;

initialization
  // Alle Testfälle beim Test-Runner registrieren
  {$IFDEF FPC}
  RegisterTest(TTestMemoryUtils);
  {$ELSE}
  RegisterTest(TTestMemoryUtils.Suite);
  {$ENDIF}

end.
