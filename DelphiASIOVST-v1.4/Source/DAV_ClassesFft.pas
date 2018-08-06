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

unit DAV_ClassesFft;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspFftReal2Complex;

type
  TDspSampleRateFftPersistent = class(TDspSampleRatePersistent)
  private
    function GetFftOrder: Byte;
    procedure SetFftOrder(const Value: Byte);
  protected
    FFFT: TFftReal2Complex;
    FFFTSize: Integer;
    FFFTSizeHalf: Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure CalculateFftSizeVariables; virtual;
    procedure FFTOrderChanged; virtual;
  public
    destructor Destroy; override;

    property FFTOrder: Byte read GetFftOrder write SetFftOrder;
    property FFTSize: Integer read FFFTSize;
  end;

implementation

uses
  SysUtils;

{ TDspSampleRateFftPersistent }

destructor TDspSampleRateFftPersistent.Destroy;
begin
  FreeAndNil(FFFT);
  inherited;
end;

procedure TDspSampleRateFftPersistent.AssignTo(Dest: TPersistent);
begin
  if Dest is TDspSampleRateFftPersistent then
    with TDspSampleRateFftPersistent(Dest) do
    begin
      FFFT.Assign(Self.FFFT);
      FFFTSize := Self.FFFTSize;
      FFFTSizeHalf := Self.FFFTSizeHalf;
    end
  else
    inherited;
end;

procedure TDspSampleRateFftPersistent.CalculateFftSizeVariables;
begin
  FFFTSize := FFFT.FFTSize;
  FFFTSizeHalf := FFFTSize shr 1;
end;

function TDspSampleRateFftPersistent.GetFftOrder: Byte;
begin
  Result := FFFT.Order;
end;

procedure TDspSampleRateFftPersistent.FFTOrderChanged;
begin
  CalculateFftSizeVariables;
  FFFT.AutoScaleType := astDivideBySqrtN;
end;

procedure TDspSampleRateFftPersistent.SetFftOrder(const Value: Byte);
begin
  if FFFT.Order <> Value then
  begin
    FFFT.Order := Value;
    FFTOrderChanged;
  end;
end;

end.
