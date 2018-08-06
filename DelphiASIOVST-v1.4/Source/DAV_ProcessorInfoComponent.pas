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

unit DAV_ProcessorInfoComponent;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_ProcessorInfo;

type
  TProcessorInfoComponent = class(TComponent)
  private
    function GetAPICID: Byte;
    function GetBrandID: Byte;
    function GetCPUName: string;
    function GetCPUType: TCPUType;
    function GetExtendedFamily: Byte;
    function GetExtendedModel: Byte;
    function GetFamily: Byte;
    function GetFlushLineSize: Byte;
    function GetHas3DNow: Boolean;
    function GetHasCacheInfo: Boolean;
    function GetHasEx3DNow: Boolean;
    function GetHasExMMX: Boolean;
    function GetHasMMX: Boolean;
    function GetHyperThreading: Boolean;
    function GetLogicalCore: Byte;
    function GetModel: Byte;
    function GetProcessorType: Byte;
    function GetStepping: Byte;
    function GetSupportsSSE: TSSESupports;
    function GetVendorString: string;
    function GetHasConditionalMov: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property VendorString: string read GetVendorString;

    // TODO: add caching!

    property CPUName: string read GetCPUName;
    property CPUType: TCPUType read GetCPUType;

    property Family: Byte read GetFamily;
    property Model: Byte read GetModel;
    property ExtendedFamily: Byte read GetExtendedFamily;
    property ExtendedModel: Byte read GetExtendedModel;
    property ProcessorType: Byte read GetProcessorType;
    property Stepping: Byte read GetStepping;

    property HasConditionalMov: Boolean read GetHasConditionalMov;
    property Has3DNow: Boolean read GetHas3DNow;
    property HasCacheInfo: Boolean read GetHasCacheInfo;
    property HasEx3DNow: Boolean read GetHasEx3DNow;
    property HasExMMX: Boolean read GetHasExMMX;
    property HasMMX: Boolean read GetHasMMX;
    property SupportsSSE: TSSESupports read GetSupportsSSE;

    property HyperThreading: Boolean read GetHyperThreading;
    property LogicalCore: Byte read GetLogicalCore;

    property BrandID: Byte read GetBrandID;
    property FlushLineSize: Byte read GetFlushLineSize;
    property APICID: Byte read GetAPICID;
  end;

implementation

{ TProcessorInfoComponent }

constructor TProcessorInfoComponent.Create(AOwner: TComponent);
begin
  inherited;

end;

function TProcessorInfoComponent.GetAPICID: Byte;
begin
  Result := ProcessorInfo.APICID;
end;

function TProcessorInfoComponent.GetBrandID: Byte;
begin
  Result := ProcessorInfo.BrandID;
end;

function TProcessorInfoComponent.GetCPUName: string;
begin
  Result := ProcessorInfo.CPUName;
end;

function TProcessorInfoComponent.GetCPUType: TCPUType;
begin
  Result := ProcessorInfo.CPUType;
end;

function TProcessorInfoComponent.GetExtendedFamily: Byte;
begin
  Result := ProcessorInfo.ExtendedFamily;
end;

function TProcessorInfoComponent.GetExtendedModel: Byte;
begin
  Result := ProcessorInfo.ExtendedModel;
end;

function TProcessorInfoComponent.GetFamily: Byte;
begin
  Result := ProcessorInfo.Family;
end;

function TProcessorInfoComponent.GetFlushLineSize: Byte;
begin
  Result := ProcessorInfo.FlushLineSize;
end;

function TProcessorInfoComponent.GetHas3DNow: Boolean;
begin
  Result := ProcessorInfo.Has3DNow;
end;

function TProcessorInfoComponent.GetHasCacheInfo: Boolean;
begin
  Result := ProcessorInfo.HasCacheInfo;
end;

function TProcessorInfoComponent.GetHasConditionalMov: Boolean;
begin
  Result := ProcessorInfo.HasConditionalMove;
end;

function TProcessorInfoComponent.GetHasEx3DNow: Boolean;
begin
  Result := ProcessorInfo.HasEx3DNow;
end;

function TProcessorInfoComponent.GetHasExMMX: Boolean;
begin
  Result := ProcessorInfo.HasExMMX;
end;

function TProcessorInfoComponent.GetHasMMX: Boolean;
begin
  Result := ProcessorInfo.HasMMX;
end;

function TProcessorInfoComponent.GetHyperThreading: Boolean;
begin
  Result := ProcessorInfo.HyperThreading;
end;

function TProcessorInfoComponent.GetLogicalCore: Byte;
begin
  Result := ProcessorInfo.LogicalCore;
end;

function TProcessorInfoComponent.GetModel: Byte;
begin
  Result := ProcessorInfo.Model;
end;

function TProcessorInfoComponent.GetProcessorType: Byte;
begin
  Result := ProcessorInfo.ProcessorType;
end;

function TProcessorInfoComponent.GetStepping: Byte;
begin
  Result := ProcessorInfo.Stepping;
end;

function TProcessorInfoComponent.GetSupportsSSE: TSSESupports;
begin
  Result := ProcessorInfo.SupportsSSE;
end;

function TProcessorInfoComponent.GetVendorString: string;
begin
  Result := ProcessorInfo.VendorString;
end;

end.
