unit DAV_Types;

{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

interface

{$I DAV_Compiler.inc}

{$IFDEF UseNativeTypes}
uses
  Types;
{$ENDIF}

type
  {$IFNDEF DELPHI7_UP}
    TDAVSingleDynarray = array of Single;
    TDAVDoubleDynArray = array of Double;
  {$ELSE}
    {$IFDEF UseNativeTypes}
      TDAVSingleDynArray = Types.TSingleDynArray;
      TDAVDoubleDynArray = Types.TDoubleDynArray;
    {$ELSE}
      TDAVSingleDynArray = array of Single;
      TDAVDoubleDynArray = array of Double;
    {$ENDIF}
  {$ENDIF}

  PDAVSingleDynArray = ^TDAVSingleDynArray;
  PDAVDoubleDynArray = ^TDAVDoubleDynArray;

  TDAVSingleFixedArray = array [0..{$IFDEF ZeroArray}0{$ELSE}MaxInt div SizeOf(Single) - 1{$ENDIF}] of Single;
  PDAVSingleFixedArray = ^TDAVSingleFixedArray;
  TDAVDoubleFixedArray = array [0..{$IFDEF ZeroArray}0{$ELSE}MaxInt div SizeOf(Double) - 1{$ENDIF}] of Double;
  PDAVDoubleFixedArray = ^TDAVDoubleFixedArray;

  TDAVArrayOfSingleDynArray = array of TDAVSingleDynArray;
  PDAVArrayOfSingleDynArray = ^TDAVArrayOfSingleDynArray;
  TDAVArrayOfDoubleDynArray = array of TDAVDoubleDynArray;
  PDAVArrayOfDoubleDynArray = ^TDAVArrayOfDoubleDynArray;

  TDAVArrayOfSingleFixedArray = array of PDAVSingleFixedArray;
  PDAVArrayOfSingleFixedArray = ^TDAVArrayOfSingleFixedArray;
  TDAVArrayOfDoubleFixedArray = array of PDAVDoubleFixedArray;
  PDAVArrayOfDoubleFixedArray = ^TDAVArrayOfDoubleFixedArray;

  TDAVSingleDynMatrix = TDAVArrayOfSingleDynArray;
  PDAVSingleDynMatrix = ^TDAVSingleDynMatrix;
  TDAVDoubleDynMatrix = TDAVArrayOfDoubleDynArray;
  PDAVDoubleDynMatrix = ^TDAVDoubleDynMatrix;

  TDAVSingleFixedMatrix = array [0..0, 0..0] of Single;
  PDAVSingleFixedMatrix = ^TDAVSingleFixedMatrix;
  TDAVDoubleFixedMatrix = array [0..0, 0..0] of Double;
  PDAVDoubleFixedMatrix = ^TDAVDoubleFixedMatrix;

  TDAVSingleFixedPointerArray = array [0..0] of PDAVSingleFixedArray;
  PDAVSingleFixedPointerArray = ^TDAVSingleFixedPointerArray;
  TDAVDoubleFixedPointerArray = array [0..0] of PDAVDoubleFixedArray;
  PDAVDoubleFixedPointerArray = ^TDAVDoubleFixedPointerArray;

  TDAV2SingleArray = array [0..1] of Single;
  PDAV2SingleArray = ^TDAV2SingleArray;
  TDAV2DoubleArray = array [0..1] of Double;
  PDAV2DoubleArray = ^TDAV2DoubleArray;

  TDAV3SingleArray = array [0..2] of Single;
  PDAV3SingleArray = ^TDAV3SingleArray;
  TDAV3DoubleArray = array [0..2] of Double;
  PDAV3DoubleArray = ^TDAV3DoubleArray;

  TDAV4SingleArray = array [0..3] of Single;
  PDAV4SingleArray = ^TDAV4SingleArray;
  TDAV4DoubleArray = array [0..3] of Double;
  PDAV4DoubleArray = ^TDAV4DoubleArray;

  TDAV6SingleArray = array [0..5] of Single;
  PDAV6SingleArray = ^TDAV6SingleArray;
  TDAV6DoubleArray = array [0..5] of Double;
  PDAV6DoubleArray = ^TDAV6DoubleArray;

  TDAV8SingleArray = array [0..7] of Single;
  PDAV8SingleArray = ^TDAV8SingleArray;
  TDAV8DoubleArray = array [0..7] of Double;
  PDAV8DoubleArray = ^TDAV8DoubleArray;

  TDAV16SingleArray = array [0..15] of Single;
  PDAV16SingleArray = ^TDAV16SingleArray;
  TDAV16DoubleArray = array [0..15] of Double;
  PDAV16DoubleArray = ^TDAV16DoubleArray;

  PDAV512SingleArray = ^TDAV1024SingleArray;
  TDAV512SingleArray = array[0..512] of Single;
  PDAV512DoubleArray = ^TDAV1024DoubleArray;
  TDAV512DoubleArray = array[0..512] of Double;

  PDAV1024SingleArray = ^TDAV1024SingleArray;
  TDAV1024SingleArray = array[0..1024] of Single;
  PDAV1024DoubleArray = ^TDAV1024DoubleArray;
  TDAV1024DoubleArray = array[0..1024] of Double;

  TDAVMinMaxSingle = record
    min : Single;
    max : Single;
  end;
  TDAVMinMaxDouble = record
    min : Double;
    max : Double;
  end;

  TChunkName = array [0..3] of AnsiChar;

  TPrecision = (pcHalf, pcSingle, pcDouble); 

  {$IFDEF Delphi5}
  PCardinal = ^Cardinal;
//  TValueSign = set of (-1, 0, 1);
  PSingle = ^Single;
  PDouble = ^Double;
  {$ENDIF}

  TStrArray = array of string;

  TDAVMidiEvent = record
    MidiData        : array[0..3] of Byte;  // 1 thru 3 midi Bytes; midiData[3] is reserved (zero)
    DeltaFrames     : LongInt;              // sample frames related to the current block start sample position
    NoteOffset      : LongInt;              // offset into note from note start if available, else 0
    NoteLength      : LongInt;              // (in sample frames) of entire note, if available, else 0
    Detune          : Byte;                 // -64 to +63 cents; for scales other than 'well-tempered' ('microtuning')
    NoteOffVelocity : Byte;
  end;

implementation

end.
