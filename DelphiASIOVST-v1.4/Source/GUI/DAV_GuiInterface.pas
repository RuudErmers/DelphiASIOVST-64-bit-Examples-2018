unit DAV_GuiInterface;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2011-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_GuiCommon;

type
  IPixel32Access = interface(IInterface)
    ['{5CF0D35F-E4E6-46E2-A313-ACD9906544B0}']
    function GetDataPointer: PPixel32Array;
    function GetPixel(X, Y: Integer): TPixel32;
    function GetPixelPointer(X, Y: Integer): PPixel32;
    function GetScanLine(Y: Integer): PPixel32Array;
    procedure SetPixel(X, Y: Integer; const Value: TPixel32);

    property DataPointer: PPixel32Array read GetDataPointer;
    property Pixel[X, Y: Integer]: TPixel32 read GetPixel write SetPixel;
    property PixelPointer[X, Y: Integer]: PPixel32 read GetPixelPointer;
    property ScanLine[Y: Integer]: PPixel32Array read GetScanLine;
  end;

implementation

end.

