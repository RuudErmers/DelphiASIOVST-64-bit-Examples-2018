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

unit DAV_CRC;

interface

{$I DAV_Compiler.inc}

type
  TCRC16 = class
  private
    FCRC: Word;
    function GetCRC: Word;
  public
    constructor Create;
    procedure AddBits(BitString: Cardinal; Length: Cardinal);
    procedure Clear;

    property Checksum: Word read GetCRC;
  end;

implementation

const
  CPolynomial: Word = $8005;

  { TCRC16 }

constructor TCRC16.Create;
begin
  Clear;
end;

function TCRC16.GetCRC: Word;
begin
  result := FCRC;
  Clear;
end;

// erase checksum for the next call of AddBits()
procedure TCRC16.Clear;
begin
  FCRC := $FFFF;
end;

// feed a bitstring to the crc calculation (0 < length <= 32)
procedure TCRC16.AddBits(BitString, Length: Cardinal);
var
  BitMask: Cardinal;
begin
  BitMask := 1 shl (Length - 1);
  repeat
    if ((FCRC and $8000 = 0) xor (BitString and BitMask = 0)) then
    begin
      FCRC := FCRC shl 1;
      FCRC := FCRC xor CPolynomial;
    end
    else
      FCRC := FCRC shl 1;
    BitMask := BitMask shr 1;
  until (BitMask = 0);
end;

end.
