unit TestDAV_BlockRoutines;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, TestFramework, DAV_Complex, DAV_Types, DAV_Math, DAV_BlockProcessing;

type
  TestMixBuffer = class(TTestCase)
  published
    procedure TestMixBuffers32;
    procedure TestMixBuffers64;
  end;

  TestComplexMultiplyBlock = class(TTestCase)
  published
    procedure TestComplexMultiplyBlock32;
    procedure TestComplexMultiplyBlock64;
  end;

implementation

{ TestMixBuffer }

procedure TestMixBuffer.TestMixBuffers32;
var
  Mix         : TDAVSingleDynArray;
  Data        : TDAVSingleDynArray;
  SampleIndex : Integer;
const
  DataSize : Cardinal = 256;
begin
 SetLength(Mix, DataSize);
 SetLength(Data, DataSize);

 for SampleIndex := 0 to DataSize - 1 do
  begin
   Mix[SampleIndex]  := Random;
   Data[SampleIndex] := 1 - Mix[SampleIndex];
  end;

 MixBuffers32(@Mix[0], @Data[0], DataSize);

 for SampleIndex := 0 to DataSize - 1
  do CheckEquals(1, Data[SampleIndex]);
end;

procedure TestMixBuffer.TestMixBuffers64;
var
  Mix         : TDAVDoubleDynArray;
  Data        : TDAVDoubleDynArray;
  SampleIndex : Integer;
const
  DataSize : Cardinal = 256;
begin
 SetLength(Mix, DataSize);
 SetLength(Data, DataSize);

 for SampleIndex := 0 to DataSize - 1 do
  begin
   Mix[SampleIndex]  := Random;
   Data[SampleIndex] := 1 - Mix[SampleIndex];
  end;

 MixBuffers64(@Mix[0], @Data[0], DataSize);

 for SampleIndex := 0 to DataSize - 1
  do CheckEquals(1, Data[SampleIndex]);
end;


{ TestComplexMultiplyBlock }

procedure TestComplexMultiplyBlock.TestComplexMultiplyBlock32;
var
  Value       : Double;
  Filter      : TDAVComplexSingleDynArray;
  Inplace     : TDAVComplexSingleDynArray;
  SampleIndex : Integer;
const
  CDataSize  : Cardinal = 256;
  CThreshold : Single = {$IFDEF PUREPASCAL}1E-5 {$ELSE} 1E-6{$ENDIF};
begin
 SetLength(Filter, CDataSize);
 SetLength(Inplace, CDataSize);

 Filter[0].Re := Random;
 Filter[0].Im := Random;
 Inplace[0].Re := 1 / Filter[0].Re;
 Inplace[0].Im := 1 / Filter[0].Im;

 for SampleIndex := 1 to CDataSize - 1 do
  begin
   Value := Pi * Random;
   GetSinCos( Value, Filter[SampleIndex].Im, Filter[SampleIndex].Re);
   GetSinCos(-Value, Inplace[SampleIndex].Im, Inplace[SampleIndex].Re);
  end;

 ComplexMultiplyBlock32(@Inplace[0], @Filter[0], CDataSize);

 // check DC and Nyquist
 CheckTrue(Abs(Inplace[0].Re - 1) < CThreshold);
 CheckTrue(Abs(Inplace[0].Im - 1) < CThreshold);

 // check other bins
 for SampleIndex := 1 to CDataSize - 1 do
  begin
   CheckTrue(Abs(Inplace[SampleIndex].Re - 1) < CThreshold);
   CheckTrue(Abs(Inplace[SampleIndex].Im    ) < CThreshold);
  end;
end;

procedure TestComplexMultiplyBlock.TestComplexMultiplyBlock64;
var
  Value       : Double;
  Filter      : TDAVComplexDoubleDynArray;
  Inplace     : TDAVComplexDoubleDynArray;
  SampleIndex : Integer;
const
  CDataSize : Cardinal = 256;
  CThreshold : Single = {$IFDEF PUREPASCAL}1E-5 {$ELSE} 1E-6{$ENDIF};
begin
 SetLength(Filter, CDataSize);
 SetLength(Inplace, CDataSize);

 Filter[0].Re := Random;
 Filter[0].Im := Random;
 Inplace[0].Re := 1 / Filter[0].Re;
 Inplace[0].Im := 1 / Filter[0].Im;

 for SampleIndex := 1 to CDataSize - 1 do
  begin
   Value := Pi * Random;
   GetSinCos( Value, Filter[SampleIndex].Im, Filter[SampleIndex].Re);
   GetSinCos(-Value, Inplace[SampleIndex].Im, Inplace[SampleIndex].Re);
  end;

 ComplexMultiplyBlock64(@Inplace[0], @Filter[0], CDataSize);

 // check DC and Nyquist
 CheckTrue(Abs(Inplace[0].Re - 1) < CThreshold);
 CheckTrue(Abs(Inplace[0].Im - 1) < CThreshold);

 // check other bins
 for SampleIndex := 1 to CDataSize - 1 do
  begin
   CheckTrue(Abs(Inplace[SampleIndex].Re - 1) < CThreshold);
   CheckTrue(Abs(Inplace[SampleIndex].Im    ) < CThreshold);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure RegisterTestSuite;
var
  TS : TTestSuite;
begin
 TS := TTestSuite.Create('Block Routines');
 TS.AddSuite(TestMixBuffer.Suite);
 TS.AddSuite(TestComplexMultiplyBlock.Suite);
 RegisterTest(TS);
end;

initialization
  RegisterTestSuite;

end.
