unit DAV_SampleRateSource;

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

uses
  Classes;

type
  {$IFDEF DELPHI10_UP} {$region 'SampleRateSource classes'} {$ENDIF}
  TCustomSampleRateSource = class(TComponent)
  private
    procedure SetSampleRate(const Value: Double);
  protected
    FSampleRate     : Double;
    FSampleRateReci : Double;
    procedure SampleRateChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TComponent); override;
    property SampleRateReciprocal: Double read FSampleRateReci;
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

  TSampleRateSource = class(TCustomSampleRateSource)
  published
    property SampleRate;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

implementation

uses
  SysUtils;

resourcestring
  RCStrInvalidSampleRate = 'SampleRate must be larger than 0!';

{$IFDEF DELPHI10_UP} {$region 'SampleRateSource implementation'} {$ENDIF}

{ TCustomSampleRateSource }

procedure TCustomSampleRateSource.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomSampleRateSource then
  begin
   TCustomSampleRateSource(Dest).FSampleRate     := FSampleRate;
   TCustomSampleRateSource(Dest).FSampleRateReci := FSampleRateReci;
  end
 else inherited;
end;

constructor TCustomSampleRateSource.Create(AOwner: TComponent);
begin
 inherited;
 FSampleRate := 44100;
end;

procedure TCustomSampleRateSource.SetSampleRate(const Value: Double);
begin
 if FSampleRate <> abs(Value) then
  begin
   if Value = 0
    then raise Exception.Create(RCStrInvalidSampleRate);
   FSampleRate := abs(Value);
   SampleRateChanged;
  end;
end;

procedure TCustomSampleRateSource.SampleRateChanged;
begin
 FSampleRateReci := 1 / FSampleRate;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

end.
