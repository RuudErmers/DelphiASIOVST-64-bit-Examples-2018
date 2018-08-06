unit DAV_Semaphore;

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

{.$UNDEF Win32}

uses
  {$IFDEF Win32}Windows, {$ENDIF} Classes;

type
  TSemaphore = class
  private
    {$IFDEF Win32}
    FSemaphore : THandle;
    {$ELSE}
    FSemaphore : Integer;
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;

implementation

{ TSemaphore }

constructor TSemaphore.Create;
begin
 {$IFDEF Win32}
 FSemaphore := CreateSemaphore(nil, 1, 1, nil);
 {$ELSE}
 FSemaphore := 0;
 {$ENDIF}
end;

destructor TSemaphore.Destroy;
begin
 {$IFDEF Win32}
 CloseHandle(FSemaphore);
 {$ENDIF}
 inherited;
end;

procedure TSemaphore.Enter;
 {$IFDEF Win32}
var
  ws : DWORD;
begin
 repeat
  ws := WaitForSingleObject(FSemaphore, 0);
//  if ws = WAIT_TIMEOUT then exit;
 until ws = WAIT_OBJECT_0;
 {$ELSE}
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 {$ENDIF}
end;

procedure TSemaphore.Leave;
begin
 {$IFDEF Win32}
 ReleaseSemaphore(FSemaphore, 1, nil);
 {$ELSE}
 Dec(FSemaphore);
 {$ENDIF}
end;

end.
