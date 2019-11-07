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

unit DAV_ProcessorInfo;

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF DELPHI20_UP}AnsiStrings,{$ENDIF}
  SysUtils;

type
  TCPUType = (
    ctUnknown   = 0,
    ctIntel     = 1,
    ctCyrix     = 2,
    ctAMD       = 3,
    ctTransmeta = 4,
    ctVIA       = 5
  );

  TSSESupport = (ssSSE, ssSSE2, ssSSE3, ssSSE3x, ssSSE4A, ssSSE4B, ssSSE5,
    ssAVX);
  TSSESupports = set of TSSESupport;

////////////////////////////////////////////////////////////////////////////////

  TCacheFamily = (cfInstructionTLB, cfDataTLB, cfL1InstructionCache,
    cfL1DataCache, cfL2Cache, cfL2TLB, cfL3Cache, cfTrace, cfOther);

  TTLBInformation = (tiEntries, tiAssociativity);

  TCacheUnitInformation = class
  protected
    FAssociativity : Byte;
    FLineSize      : Byte;     // in Byte
    FSize          : Cardinal; // in kByte
  public
    constructor Create(Associativity, LineSize: Byte; Size: Cardinal); virtual;
    property Associativity: Byte read FAssociativity;
    property LineSize: Byte read FLineSize;
    property Size: Cardinal read FSize;
  end;

  TCustomCacheInformation = class
  public
    constructor Create(ExtendedFeatureID: Cardinal); virtual; abstract;
  end;

  TCustomL1L2CacheInformation = class(TCustomCacheInformation)
  protected
    FL1DataCache : TCacheUnitInformation;
    FL1CodeCache : TCacheUnitInformation;
    FL2Cache     : TCacheUnitInformation;
  public
    destructor Destroy; override;

    property L1DataCache: TCacheUnitInformation read FL1DataCache;
    property L1InstructionCache: TCacheUnitInformation read FL1CodeCache;
    property L2Cache: TCacheUnitInformation read FL2Cache;
  end;

  TCustomL3CacheInformation = class(TCustomL1L2CacheInformation)
  protected
    FL3Cache : TCacheUnitInformation;
  public
    destructor Destroy; override;

    property L3Cache: TCacheUnitInformation read FL3Cache;
  end;

  TIntelCacheInformation = class(TCustomL3CacheInformation)
  protected
    FCacheDescriptors : array [0..15] of Byte;
    FL3LinesPerSector : Byte;
    FL2CacheInfo      : Cardinal;
  public
    constructor Create(ExtendedFeatureID: Cardinal); override;
  end;

  TAMDCacheInformation = class(TCustomL3CacheInformation)
  protected
    // do not split L1 MByte TLB
    FL1MByteInstructionTLB   : array [TTLBInformation] of Byte;
    FL1MByteDataTLB          : array [TTLBInformation] of Byte;

    // do not split L1 KByte TLB
    FL1KByteInstructionTLB   : array [TTLBInformation] of Byte;
    FL1KByteDataTLB          : array [TTLBInformation] of Byte;

    // do not split L2 MByte TLB
    FL2MByteInstructionTLB   : array [TTLBInformation] of Byte; // L2 TLB for 2-MByte and 4-MByte pages
    FL2MByteDataTLB          : array [TTLBInformation] of Byte; // L2 TLB for 2-MByte and 4-MByte pages

    // do not split L2 KByte TLB
    FL2KByteDataTLB          : array [TTLBInformation] of Byte; // L2 TLB for 4-KByte pages
    FL2KByteInstructionTLB   : array [TTLBInformation] of Byte; // L2 TLB for 4-KByte pages

    FL2CacheInfo             : Cardinal;
    FL3CacheInfo             : Cardinal;
  public
    constructor Create(ExtendedFeatureID: Cardinal); override;
  end;

  TCyrixCacheInformation = class(TCustomCacheInformation)
  protected
    FL1CacheInfo : array [0..3] of Byte;
    FTLBInfo     : array [0..3] of Byte;
  public
    constructor Create(ExtendedFeatureID: Cardinal); override;
  end;

  TViaCacheInformation = class(TCustomL1L2CacheInformation)
  protected
    FDataTLB     : array [TTLBInformation] of Byte;
    FCodeTLB     : array [TTLBInformation] of Byte;
    FL2DataCache : Cardinal;
  public
    constructor Create(ExtendedFeatureID: Cardinal); override;
  end;

  TTransmetaCacheInformation = class(TCustomL1L2CacheInformation)
  protected
    FDataTLB     : array [TTLBInformation] of Byte;
    FCodeTLB     : array [TTLBInformation] of Byte;
    FL2CacheInfo : Cardinal;
  public
    constructor Create(ExtendedFeatureID: Cardinal); override;
  end;

////////////////////////////////////////////////////////////////////////////////

  TCustomProcessorFeatures = class
  private
    function GetCPUName: string;
    function GetHyperThreading: Boolean;
    function GetHasCacheInfo: Boolean;
  protected
    FCPUName          : array [0..47] of AnsiChar;
    FCPUType          : TCPUType;
    FCacheInfo        : TCustomCacheInformation;
    FExFeatures       : Cardinal;
    FExtendedFamily   : Byte;
    FExtendedModel    : Byte;
    FFamily           : Byte;

    FFeatures         : Cardinal;
    FLogicalCore      : Byte;
    FModel            : Byte;
    FProcessorType    : Byte;
    FStepping         : Byte;

    FBrandID          : Byte;
    FFlushLineSize    : Byte;
    FAPICID           : Byte;
    class function GetManufacturer: string; virtual; abstract;
  public
    constructor Create(FeatureID: Cardinal); virtual;
    destructor Destroy; override;

    property CPUName: string read GetCPUName;
    property CPUType: TCPUType read FCPUType;

    property Family: Byte read FFamily;
    property Model: Byte read FModel;
    property ExtendedFamily: Byte read FExtendedFamily;
    property ExtendedModel: Byte read FExtendedModel;
    property ProcessorType: Byte read FProcessorType;
    property Features: Cardinal read FFeatures;
    property Stepping: Byte read FStepping;

    property HasCacheInfo: Boolean read GetHasCacheInfo;

    property HyperThreading: Boolean read GetHyperThreading;
    property LogicalCore: Byte read FLogicalCore;

    property BrandID: Byte read FBrandID;
    property FlushLineSize: Byte read FFlushLineSize;
    property APICID: Byte read FAPICID;

    property ExFeatures: Cardinal read FExFeatures;
  end;
  TProcessorFeaturesClass = class of TCustomProcessorFeatures;

  TProcessorFeaturesStandard = class(TCustomProcessorFeatures)
  protected
    class function GetManufacturer: string; override;
  public
    constructor Create(FeatureID: Cardinal); override;
  end;

  TCustomProcessorExtendedFeatures = class(TCustomProcessorFeatures)
  protected
    FHasExtendedInfo : Boolean;
    FHas3DNow        : Boolean;
    FHasEx3DNow      : Boolean;
    FHasExMMX        : Boolean;
    FSupportsSSE     : TSSESupports;

    function GetHasMMX: Boolean; virtual;
    function GetHasFPU: Boolean; virtual;
    function GetHasCMOV: Boolean; virtual;
  public
    property Has3DNow: Boolean read FHas3DNow;
    property HasEx3DNow: Boolean read FHasEx3DNow;
    property HasExMMX: Boolean read FHasExMMX;
    property HasMMX: Boolean read GetHasMMX;
    property HasFPU: Boolean read GetHasFPU;
    property HasCMOV: Boolean read GetHasCMOV;
    property HasExtendedInfo: Boolean read FHasExtendedInfo;
    property SupportsSSE: TSSESupports read FSupportsSSE;
  end;

  TProcessorFeaturesIntel = class(TCustomProcessorExtendedFeatures)
  protected
    FEx64Features         : Cardinal;
    FEx64Features2        : Cardinal;
    FPhysicalAddressBits  : Byte;
    FVirtualAddressBits   : Byte;

    // Intel & AMD
    FIs64Bits             : Boolean;
    FDepCapable           : Boolean;
    FPhysicalCore         : Byte;
    class function GetManufacturer: string; override;
  public
    constructor Create(FeatureID: Cardinal); override;
  end;

  TProcessorFeaturesAMD = class(TCustomProcessorExtendedFeatures)
  protected
    FFeatures2               : Cardinal;
    FExFeatures2             : Cardinal;
    FExBrandID               : Word;

    FAdvancedPowerManagement : Cardinal;
    FPhysicalAddressSize     : Byte;
    FVirtualAddressSize      : Byte;

    // Intel & AMD
    FIs64Bits                : Boolean;
    FDepCapable              : Boolean;
    FPhysicalCore            : Byte;
    class function GetManufacturer: string; override;
  public
    constructor Create(FeatureID: Cardinal); override;
  end;

  TProcessorFeaturesCyrix = class(TCustomProcessorExtendedFeatures)
  protected
    class function GetManufacturer: string; override;
  public
    constructor Create(FeatureID: Cardinal); override;
  end;

  TProcessorFeaturesVIA = class(TCustomProcessorExtendedFeatures)
  protected
    class function GetManufacturer: string; override;
  public
    constructor Create(FeatureID: Cardinal); override;
  end;
  TProcessorFeaturesCentaur = TProcessorFeaturesVIA;

  TProcessorFeaturesTransmeta = class(TCustomProcessorExtendedFeatures)
  protected
    FRevisionABCD          : Cardinal;
    FRevisionXXXX          : Cardinal;
    FFrequency             : Cardinal;
    FCodeMorphingABCD      : Cardinal;
    FCodeMorphingXXXX      : Cardinal;
    FTransmetaFeatures     : Cardinal;
    FTransmetaInformations : array [0..64] of Char;
    FCurrentVoltage        : Cardinal;
    FCurrentFrequency      : Cardinal;
    FCurrentPerformance    : Cardinal;
    class function GetManufacturer: string; override;
  public
    constructor Create(FeatureID: Cardinal); override;
  end;

  TProcessorInfo = class
  private
    FVendorString : string;
    FFeatures     : TCustomProcessorFeatures;
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
    function GetExFeatures: Cardinal;
    function GetSupportsSSE: TSSESupports;
    function GetHasConditionalMove: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property VendorString: string read FVendorString;

    // TODO: add caching!

    property CPUName: string read GetCPUName;
    property CPUType: TCPUType read GetCPUType;

    property Family: Byte read GetFamily;
    property Model: Byte read GetModel;
    property ExtendedFamily: Byte read GetExtendedFamily;
    property ExtendedModel: Byte read GetExtendedModel;
    property ProcessorType: Byte read GetProcessorType;
    property Stepping: Byte read GetStepping;

    property HasCacheInfo: Boolean read GetHasCacheInfo;
    property HasConditionalMove: Boolean read GetHasConditionalMove;
    property Has3DNow: Boolean read GetHas3DNow;
    property HasEx3DNow: Boolean read GetHasEx3DNow;
    property HasExMMX: Boolean read GetHasExMMX;
    property HasMMX: Boolean read GetHasMMX;
    property SupportsSSE: TSSESupports read GetSupportsSSE;

    property HyperThreading: Boolean read GetHyperThreading;
    property LogicalCore: Byte read GetLogicalCore;

    property BrandID: Byte read GetBrandID;
    property FlushLineSize: Byte read GetFlushLineSize;
    property APICID: Byte read GetAPICID;

    property ExFeatures: Cardinal read GetExFeatures;
  end;

var
  ProcessorInfo: TProcessorInfo;

implementation

type
  TCacheSizeInfo = record
    Size          : Cardinal;
    LineSize      : Byte;
    Associativity : Byte;
  end;

  TCacheInfoStructure = (ciLineSize, ciLinesPerTag, ciAssociativity, ciSize);

  TCacheInfo = record
    D: Byte;
    Family: TCacheFamily;
    Size: Cardinal;
    WaysOfAssoc: Byte;
    LineSize: Byte;       // for Normal Cache
    LinePerSector: Byte;  // for L3 Normal Cache
    Entries: Cardinal;    // for TLB
  end;

const
  CIntelCacheDescription: array [0..101] of TCacheInfo = (
    (D: $00; Family: cfOther;              Size: 0;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $01; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 32 ),
    (D: $02; Family: cfInstructionTLB;     Size: 4096;  WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 2  ),
    (D: $03; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 64 ),
    (D: $04; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 8  ),
    (D: $05; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 32 ),
    (D: $06; Family: cfL1InstructionCache; Size: 8;     WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $08; Family: cfL1InstructionCache; Size: 16;    WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $09; Family: cfL1InstructionCache; Size: 32;    WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $0A; Family: cfL1DataCache;        Size: 8;     WaysOfAssoc: 2;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $0B; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 4  ),
    (D: $0C; Family: cfL1DataCache;        Size: 16;    WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $0D; Family: cfL1DataCache;        Size: 16;    WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $0E; Family: cfL1DataCache;        Size: 24;    WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $21; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $22; Family: cfL3Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $23; Family: cfL3Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $25; Family: cfL3Cache;            Size: 2048;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $29; Family: cfL3Cache;            Size: 4096;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $2C; Family: cfL1DataCache;        Size: 32;    WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $30; Family: cfL1InstructionCache; Size: 32;    WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $39; Family: cfL2Cache;            Size: 128;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $3A; Family: cfL2Cache;            Size: 192;   WaysOfAssoc: 6;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $3B; Family: cfL2Cache;            Size: 128;   WaysOfAssoc: 2;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $3C; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $3D; Family: cfL2Cache;            Size: 384;   WaysOfAssoc: 6;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $3E; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $40; Family: cfOther;              Size: 0;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $41; Family: cfL2Cache;            Size: 128;   WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $42; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $43; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $44; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $45; Family: cfL2Cache;            Size: 2048;  WaysOfAssoc: 4;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $46; Family: cfL3Cache;            Size: 4096;  WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $47; Family: cfL3Cache;            Size: 8192;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $48; Family: cfL2Cache;            Size: 3072;  WaysOfAssoc: 12; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $49; Family: cfL2Cache;            Size: 4096;  WaysOfAssoc: 16; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $4A; Family: cfL3Cache;            Size: 6144;  WaysOfAssoc: 12; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $4B; Family: cfL3Cache;            Size: 8192;  WaysOfAssoc: 16; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $4C; Family: cfL3Cache;            Size: 12288; WaysOfAssoc: 12; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $4D; Family: cfL3Cache;            Size: 16384; WaysOfAssoc: 16; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $4E; Family: cfL3Cache;            Size: 6144;  WaysOfAssoc: 24; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $4F; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 32 ),
    (D: $50; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 64 ),
    (D: $51; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 128),
    (D: $52; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 256),
    (D: $55; Family: cfInstructionTLB;     Size: 2048;  WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 7  ),
    (D: $56; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 16 ),
    (D: $57; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 16 ),
    (D: $59; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 16 ),
    (D: $5A; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 32 ),
    (D: $5B; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 64 ),
    (D: $5C; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 128),
    (D: $5D; Family: cfDataTLB;            Size: 4096;  WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 256),
    (D: $60; Family: cfL1DataCache;        Size: 16;    WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $66; Family: cfL1DataCache;        Size: 8;     WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $67; Family: cfL1DataCache;        Size: 16;    WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $68; Family: cfL1DataCache;        Size: 32;    WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $70; Family: cfTrace;              Size: 12;    WaysOfAssoc: 8;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $71; Family: cfTrace;              Size: 16;    WaysOfAssoc: 8;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $72; Family: cfTrace;              Size: 32;    WaysOfAssoc: 8;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $73; Family: cfTrace;              Size: 64;    WaysOfAssoc: 8;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $78; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $79; Family: cfL2Cache;            Size: 128;   WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $7A; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $7B; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $7C; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 2; Entries: 0  ),
    (D: $7D; Family: cfL2Cache;            Size: 2048;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $7F; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 2;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $80; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $82; Family: cfL2Cache;            Size: 256;   WaysOfAssoc: 8;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $83; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 8;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $84; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $85; Family: cfL2Cache;            Size: 2048;  WaysOfAssoc: 8;  LineSize: 32; LinePerSector: 0; Entries: 0  ),
    (D: $86; Family: cfL2Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $87; Family: cfL2Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $B0; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 128),
    (D: $B1; Family: cfInstructionTLB;     Size: 2048;  WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 8  ),
    (D: $B2; Family: cfInstructionTLB;     Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 64 ),
    (D: $B3; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 128),
    (D: $B4; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 256),
    (D: $BA; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 64 ),
    (D: $C0; Family: cfDataTLB;            Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 8  ),
    (D: $CA; Family: cfL2TLB;              Size: 4;     WaysOfAssoc: 4;  LineSize: 0;  LinePerSector: 0; Entries: 512),
    (D: $D0; Family: cfL3Cache;            Size: 512;   WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $D1; Family: cfL3Cache;            Size: 1024;  WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $D2; Family: cfL3Cache;            Size: 2048;  WaysOfAssoc: 4;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $D6; Family: cfL3Cache;            Size: 1024;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $D7; Family: cfL3Cache;            Size: 2048;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $D8; Family: cfL3Cache;            Size: 4096;  WaysOfAssoc: 8;  LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $DC; Family: cfL3Cache;            Size: 1536;  WaysOfAssoc: 12; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $DD; Family: cfL3Cache;            Size: 3072;  WaysOfAssoc: 12; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $DE; Family: cfL3Cache;            Size: 6144;  WaysOfAssoc: 12; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $E2; Family: cfL3Cache;            Size: 2048;  WaysOfAssoc: 16; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $E3; Family: cfL3Cache;            Size: 4096;  WaysOfAssoc: 16; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $E4; Family: cfL3Cache;            Size: 8192;  WaysOfAssoc: 16; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $EA; Family: cfL3Cache;            Size: 12288; WaysOfAssoc: 24; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $EB; Family: cfL3Cache;            Size: 18432; WaysOfAssoc: 24; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $EC; Family: cfL3Cache;            Size: 24576; WaysOfAssoc: 24; LineSize: 64; LinePerSector: 0; Entries: 0  ),
    (D: $F0; Family: cfOther;              Size: 0;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $F1; Family: cfOther;              Size: 0;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 0  ),
    (D: $FF; Family: cfOther;              Size: 0;     WaysOfAssoc: 0;  LineSize: 0;  LinePerSector: 0; Entries: 0  )
  );

////////////////////////////////////////////////////////////////////////////////

{$WARNINGS ON}
function HasCPUIDInstruction: Boolean;
const
  CFlag = $200000;
asm
    {$IFDEF CPU32}
    PUSHFD                 // save EFLAGS to stack
    POP     EAX            // store EFLAGS in EAX
    MOV     ECX, EAX       // save in ECX for later testing
    XOR     EAX, CFlag     // flip ID bit in EFLAGS
    AND     ECX, CFlag
    PUSH    EAX            // save new EFLAGS Value on stack
    POPFD                  // replace current EFLAGS Value
    PUSHFD                 // get new EFLAGS
    POP     EAX            // store new EFLAGS in EAX
    AND     EAX, CFlag
    XOR     EAX, ECX       // check if ID bit changed
    SETNZ   Result         // set result
    {$ENDIF CPU32}

    {$IFDEF CPU64}
    MOV       EDX, False
    PUSHFQ
    POP       RAX
    MOV       ECX, EAX
    XOR       EAX, $00200000
    PUSH      RAX
    POPFQ
    PUSHFQ
    POP       RAX
    XOR       ECX, EAX
    JZ        @NotFound
    MOV       EDX, True
@NotFound:
    PUSH      RAX
    POPFQ
    MOV       EAX, EDX
    {$ENDIF CPU64}
end;

procedure CallCPUID(ValueEAX, ValueECX: Cardinal; out ReturnedEAX, ReturnedEBX,
  ReturnedECX, ReturnedEDX);
asm
    {$IFDEF CPU32}
    // save context
    PUSH    EDI
    PUSH    EBX
    MOV     EDI, ReturnedEAX

    // init parameters
    MOV     ECX, ValueECX

    // CPUID
    DB      0FH
    DB      0A2H

    // store results
    MOV     Cardinal PTR [EDI], EAX
    MOV     EDI, ReturnedEBX
    MOV     Cardinal PTR [EDI], EBX
    MOV     EDI, ReturnedECX
    MOV     Cardinal PTR [EDI], ECX
    MOV     EDI, ReturnedEDX
    MOV     Cardinal PTR [EDI], EDX

    // restore context
    POP     EBX
    POP     EDI
    {$ENDIF CPU32}

    // yet to be tested...
    {$IFDEF CPU64}
    // save context
    PUSH    RDI
    PUSH    RBX

    // init parameters
    MOV     EAX, ValueEAX
    MOV     ECX, ValueECX

    // CPUID
    CPUID

    // store results
    MOV     [R8], EAX
    MOV     [R9], EBX
    MOV     RDI, ReturnedECX
    MOV     [RDI], ECX
    MOV     RDI, ReturnedEDX
    MOV     [RDI], EDX

    // restore context
    POP     RBX
    POP     RDI
    {$ENDIF CPU64}
end;

////////////////////////////////////////////////////////////////////////////////

function IsFPU_Available: Boolean;
var
  _FCW, _FSW: Word;
asm
    MOV     EAX, False     // initialize return register
    MOV     _FSW, $5A5A    // store a non-zero Value
    FNINIT                 // must use non-wait form
    FNSTSW  _FSW           // store the status
    CMP     _FSW, 0        // was the correct status read?
    JNE     @Exit          // no, FPU not available
    FNSTCW  _FCW           // yes, now save control word
    MOV     DX, _FCW       // get the control word
    AND     DX, $103F      // mask the proper status bits
    CMP     DX, $3F        // is a numeric processor installed?
    JNE     @Exit          // no, FPU not installed
    MOV     EAX, True      // yes, FPU is installed
@Exit:
end;

function TestFDIVInstruction: Boolean;
{$IFDEF CPU32}
var
  TopNum    : Double;
  BottomNum : Double;
  One       : Double;
  ISOK      : Boolean;
begin
  TopNum := 2658955;
  BottomNum := PI;
  One := 1;
  asm
      PUSH    EAX
      FLD     [TopNum]
      FDIV    [BottomNum]
      FMUL    [BottomNum]
      FSUBR   [TopNum]
      FCOMP   [One]
      FSTSW   AX
      SHR     EAX, 8
      AND     EAX, 01H
      MOV     ISOK, AL
      POP     EAX
  end;
  Result := ISOK;
end;
{$ENDIF CPU32}
{$IFDEF CPU64}
begin
  Result := True;
end;
{$ENDIF CPU64}

////////////////////////////////////////////////////////////////////////////////

{ TProcessorInfo }

constructor TProcessorInfo.Create;
var
  VendorStr    : array [0..11] of AnsiChar;
  FeatureID    : Cardinal;
  FeatureClass : TProcessorFeaturesClass;
begin
 if HasCPUIDInstruction then
  begin
   CallCPUID(0, 0, FeatureID, VendorStr[0], VendorStr[8], VendorStr[4]);
   FVendorString := string(VendorStr);

   if FVendorString = 'GenuineIntel' then FeatureClass := TProcessorFeaturesIntel else
   if FVendorString = 'AuthenticAMD' then FeatureClass := TProcessorFeaturesAMD else
   if FVendorString = 'CyrixInstead' then FeatureClass := TProcessorFeaturesCyrix else
   if FVendorString = 'CentaurHauls' then FeatureClass := TProcessorFeaturesCentaur else
   if FVendorString = 'VIA VIA VIA ' then FeatureClass := TProcessorFeaturesVIA else

   // yet unsupported
   if FVendorString = 'Geode by NSC' then FeatureClass := TProcessorFeaturesStandard else
   if FVendorString = 'NexGenDriven' then FeatureClass := TProcessorFeaturesStandard else
   if FVendorString = 'RiseRiseRise' then FeatureClass := TProcessorFeaturesStandard else
   if FVendorString = 'SiS SiS SiS ' then FeatureClass := TProcessorFeaturesStandard else
   if FVendorString = 'UMC UMC UMC ' then FeatureClass := TProcessorFeaturesStandard else

   if FVendorString = 'TransmetaCPU' then FeatureClass := TProcessorFeaturesTransmeta else
   if FVendorString = 'GenuineTMx86'
    then FeatureClass := TProcessorFeaturesTransmeta
    else FeatureClass := TProcessorFeaturesStandard;

   FFeatures := FeatureClass.Create(FeatureID);
  end
 else
  begin
   FVendorString := '';
  end;
end;

destructor TProcessorInfo.Destroy;
begin
 FreeAndNil(FFeatures);
 inherited;
end;

function TProcessorInfo.GetAPICID: Byte;
begin
 Result := FFeatures.APICID;
end;

function TProcessorInfo.GetBrandID: Byte;
begin
 Result := FFeatures.BrandID
end;

function TProcessorInfo.GetCPUName: string;
begin
 Result := FFeatures.CPUName;
end;

function TProcessorInfo.GetCPUType: TCPUType;
begin
 Result := FFeatures.CPUType;
end;

function TProcessorInfo.GetExFeatures: Cardinal;
begin
 Result := FFeatures.ExFeatures;
end;

function TProcessorInfo.GetExtendedFamily: Byte;
begin
 Result := FFeatures.ExtendedFamily;
end;

function TProcessorInfo.GetExtendedModel: Byte;
begin
 Result := FFeatures.ExtendedModel;
end;

function TProcessorInfo.GetFamily: Byte;
begin
 Result := FFeatures.Family;
end;

function TProcessorInfo.GetFlushLineSize: Byte;
begin
 Result := FFeatures.FlushLineSize;
end;

function TProcessorInfo.GetHas3DNow: Boolean;
begin
 if FFeatures is TCustomProcessorExtendedFeatures
  then Result := TCustomProcessorExtendedFeatures(FFeatures).Has3DNow
  else Result := False;
end;

function TProcessorInfo.GetHasCacheInfo: Boolean;
begin
 Result := FFeatures.HasCacheInfo;
end;

function TProcessorInfo.GetHasConditionalMove: Boolean;
begin
 if FFeatures is TCustomProcessorExtendedFeatures
  then Result := TCustomProcessorExtendedFeatures(FFeatures).HasCMOV
  else Result := False;
end;

function TProcessorInfo.GetHasEx3DNow: Boolean;
begin
 if FFeatures is TCustomProcessorExtendedFeatures
  then Result := TCustomProcessorExtendedFeatures(FFeatures).HasEx3DNow
  else Result := False;
end;

function TProcessorInfo.GetHasExMMX: Boolean;
begin
 if FFeatures is TCustomProcessorExtendedFeatures
  then Result := TCustomProcessorExtendedFeatures(FFeatures).HasExMMX
  else Result := False;
end;

function TProcessorInfo.GetHasMMX: Boolean;
begin
 if FFeatures is TCustomProcessorExtendedFeatures
  then Result := TCustomProcessorExtendedFeatures(FFeatures).HasMMX
  else Result := False;
end;

function TProcessorInfo.GetHyperThreading: Boolean;
begin
 Result := FFeatures.HyperThreading;
end;

function TProcessorInfo.GetLogicalCore: Byte;
begin
 Result := FFeatures.LogicalCore;
end;

function TProcessorInfo.GetModel: Byte;
begin
 Result := FFeatures.Model;
end;

function TProcessorInfo.GetProcessorType: Byte;
begin
 Result := FFeatures.ProcessorType;
end;

function TProcessorInfo.GetStepping: Byte;
begin
 Result := FFeatures.Stepping;
end;

function TProcessorInfo.GetSupportsSSE: TSSESupports;
begin
 if FFeatures is TCustomProcessorExtendedFeatures
  then Result := TCustomProcessorExtendedFeatures(FFeatures).SupportsSSE
  else Result := [];
end;


{ TCustomProcessorFeatures }

constructor TCustomProcessorFeatures.Create(FeatureID: Cardinal);
var
  VersionInfo, AdditionalInfo, ExFeatures: Cardinal;
begin
 // clear variables
 FillChar(FCPUName, SizeOf(FCPUName), 0);

 if FeatureID >= 1 then
  begin
   CallCPUID(1, 0, VersionInfo, AdditionalInfo, ExFeatures, FFeatures);

   FProcessorType := (VersionInfo and $00003000) shr 12;
   FFamily := (VersionInfo and $00000F00) shr 8;
   FModel := (VersionInfo and $000000F0) shr 4;
   FStepping := (VersionInfo and $0000000F);
   FExtendedModel := (VersionInfo and $000F0000) shr 16;
   FExtendedFamily := (VersionInfo and $0FF00000) shr 20;

   if FCpuType = ctIntel then
    begin
     FExFeatures := ExFeatures;
     FBrandID := AdditionalInfo and $000000FF;
     FFlushLineSize := (AdditionalInfo and $0000FF00) shr 8;
     FAPICID := (AdditionalInfo and $FF000000) shr 24;

     if HyperThreading then
      begin
       FLogicalCore := (AdditionalInfo and $00FF0000) shr 16;
       if FLogicalCore = 0
        then FLogicalCore := 1;
      end;
    end;
  end
 else
  begin
   FFeatures      := 0;
   FExFeatures    := 0;
   FBrandID       := 0;
   FFlushLineSize := 0;
   FAPICID        := 0;
   FLogicalCore   := 0;
  end;
end;

destructor TCustomProcessorFeatures.Destroy;
begin
 FreeAndNil(FCacheInfo);
 inherited;
end;

function TCustomProcessorFeatures.GetCPUName: string;
begin
 Result := string(FCPUName);
end;

function TCustomProcessorFeatures.GetHasCacheInfo: Boolean;
begin
 Result := FCacheInfo <> nil;
end;

function TCustomProcessorFeatures.GetHyperThreading: Boolean;
begin
 Result := (FFeatures and $10000000) <> 0;
end;


{ TCustomProcessorExtendedFeatures }

function TCustomProcessorExtendedFeatures.GetHasCMOV: Boolean;
begin
 Result := (FFeatures and $00008000) <> 0;
end;

function TCustomProcessorExtendedFeatures.GetHasFPU: Boolean;
begin
 Result := (FFeatures and $00000001) <> 0;
end;

function TCustomProcessorExtendedFeatures.GetHasMMX: Boolean;
begin
 Result := (FFeatures and $00800000) <> 0;
end;


{ TCustomL1L2CacheInformation }

destructor TCustomL1L2CacheInformation.Destroy;
begin
 FreeAndNil(FL1DataCache);
 FreeAndNil(FL1CodeCache);
 FreeAndNil(FL2Cache);
 inherited;
end;


{ TCustomL3CacheInformation }

destructor TCustomL3CacheInformation.Destroy;
begin
 FreeAndNil(FL3Cache);
 inherited;
end;


{ TIntelCacheInformation }

constructor TIntelCacheInformation.Create(ExtendedFeatureID: Cardinal);
var
  Unused   : Cardinal;
  L1I, L1D : TCacheSizeInfo;
  L2, L3   : TCacheSizeInfo;
  I, J     : Integer;
begin
 inherited;

 CallCPUID(2, 0, FCacheDescriptors[0], FCacheDescriptors[4],
   FCacheDescriptors[8], FCacheDescriptors[12]);

 if ExtendedFeatureID >= $80000006
  then CallCPUID($80000006, 0, Unused, Unused, FL2CacheInfo, Unused);

 if (FL2CacheInfo <> 0) then
  begin
   L2.Size := FL2CacheInfo shr 16;
   L2.LineSize := FL2CacheInfo and $FF;
   L2.Associativity := (FL2CacheInfo shr 12) and $F;
  end;

 for I := Low(FCacheDescriptors) to High(FCacheDescriptors) do
  if FCacheDescriptors[I] <> 0 then
   for J := Low(CIntelCacheDescription) to High(CIntelCacheDescription) do
    if CIntelCacheDescription[J].D = FCacheDescriptors[I] then
     with CIntelCacheDescription[J] do
      case Family of
        cfL1InstructionCache:
          begin
           Inc(L1I.Size, Size);
           L1I.LineSize := LineSize;
           L1I.Associativity := WaysOfAssoc;
          end;
        cfL1DataCache:
          begin
           Inc(L1D.Size, Size);
           L1D.LineSize := LineSize;
           L1D.Associativity := WaysOfAssoc;
          end;
        cfL2Cache:
          if (FL2CacheInfo = 0) then
           begin
            Inc(L2.Size, Size);
            L2.LineSize := LineSize;
            L2.Associativity := WaysOfAssoc;
           end;
        cfL3Cache:
          begin
            Inc(L3.Size,Size);
            L3.LineSize := LineSize;
            L3.Associativity := WaysOfAssoc;
            FL3LinesPerSector := LinePerSector;
          end;
      end;

 FL1DataCache := TCacheUnitInformation.Create(L1D.Associativity, L1D.LineSize, L1D.Size);
 FL1CodeCache := TCacheUnitInformation.Create(L1I.Associativity, L1I.LineSize, L1I.Size);
 FL2Cache := TCacheUnitInformation.Create(L2.Associativity, L2.LineSize, L2.Size);
 FL3Cache := TCacheUnitInformation.Create(L3.Associativity, L3.LineSize, L3.Size);
end;


{ TAMDCacheInformation }

constructor TAMDCacheInformation.Create(ExtendedFeatureID: Cardinal);
var
  L1DataCache : array [TCacheInfoStructure] of Byte;
  L1CodeCache : array [TCacheInfoStructure] of Byte;
begin
  inherited;

 CallCPUID($80000005, 0, FL1MByteInstructionTLB, FL1KByteInstructionTLB,
   L1DataCache, L1CodeCache);

 if ExtendedFeatureID >= $80000006
  then CallCPUID($80000006, 0, FL2MByteInstructionTLB, FL2KByteInstructionTLB,
    FL2CacheInfo, FL3CacheInfo);

 FL1DataCache := TCacheUnitInformation.Create(
   L1DataCache[ciAssociativity], L1DataCache[ciLineSize],
   L1DataCache[ciSize]);

 FL1CodeCache := TCacheUnitInformation.Create(
   L1CodeCache[ciAssociativity], L1CodeCache[ciLineSize],
   L1CodeCache[ciSize]);

 FL2Cache := TCacheUnitInformation.Create(
   (FL2CacheInfo shr 12) and $F, FL2CacheInfo and $FF,
   FL2CacheInfo shr 16);

 FL3Cache := TCacheUnitInformation.Create(
   (FL3CacheInfo shr 12) and $F, FL3CacheInfo and $FF,
   FL3CacheInfo shr 19);
end;


{ TCyrixCacheInformation }

constructor TCyrixCacheInformation.Create(ExtendedFeatureID: Cardinal);
var
  Unused : Cardinal;
begin
 CallCPUID($80000005, 0, Unused, FTLBInfo, FL1CacheInfo, Unused);
end;


{ TViaCacheInformation }

constructor TViaCacheInformation.Create(ExtendedFeatureID: Cardinal);
var
  Unused      : Cardinal;
  L1DataCache : array [TCacheInfoStructure] of Byte;
  L1CodeCache : array [TCacheInfoStructure] of Byte;
begin
 CallCPUID($80000005, 0, Unused, FCodeTLB, L1DataCache,
   L1CodeCache);

 if ExtendedFeatureID >= $80000006
  then CallCPUID($80000006, 0, Unused, Unused, FL2DataCache, Unused);

 FL1DataCache := TCacheUnitInformation.Create(
   L1DataCache[ciAssociativity], L1DataCache[ciLineSize],
   L1DataCache[ciSize]);

 FL1CodeCache := TCacheUnitInformation.Create(
   L1CodeCache[ciAssociativity], L1CodeCache[ciLineSize],
   L1CodeCache[ciSize]);

 FL2Cache := TCacheUnitInformation.Create(
   (FL2DataCache shr 12) and $F, FL2DataCache and $FF,
    FL2DataCache shr 16);
end;


{ TTransmetaInformation }

constructor TTransmetaCacheInformation.Create(ExtendedFeatureID: Cardinal);
var
  Unused      : Cardinal;
  L1DataCache : array [TCacheInfoStructure] of Byte;
  L1CodeCache : array [TCacheInfoStructure] of Byte;
begin
 CallCPUID($80000005, 0, Unused, FCodeTLB, FL1DataCache, FL1CodeCache);

 if ExtendedFeatureID >= $80000006
  then CallCPUID($80000006, 0, Unused, Unused, FL2CacheInfo, Unused);

 FL1DataCache := TCacheUnitInformation.Create(
   L1DataCache[ciAssociativity], L1DataCache[ciLineSize],
   L1DataCache[ciSize]);

 FL1CodeCache := TCacheUnitInformation.Create(
   L1CodeCache[ciAssociativity], L1CodeCache[ciLineSize],
   L1CodeCache[ciSize]);

 FL2Cache := TCacheUnitInformation.Create(
   (FL2CacheInfo shr 12) and $F, FL2CacheInfo and $FF,
    FL2CacheInfo shr 16);
end;

{ TCacheUnitInformation }

constructor TCacheUnitInformation.Create(Associativity, LineSize: Byte;
  Size: Cardinal);
begin
 FAssociativity := Associativity;
 FLineSize := LineSize;
 FSize := Size;
end;


{ TProcessorFeaturesStandard }

constructor TProcessorFeaturesStandard.Create(FeatureID: Cardinal);
begin
 FCpuType := ctUnknown;
 inherited;
end;

class function TProcessorFeaturesStandard.GetManufacturer: string;
begin
 Result := 'Unknown';
end;


{ TProcessorFeaturesIntel }

constructor TProcessorFeaturesIntel.Create(FeatureID: Cardinal);
var
  ExFeatID, Unused : Cardinal;
  PhysicalCoreInfo : Cardinal;
  AddressSize      : Cardinal;
begin
 FCpuType := ctIntel;

 inherited Create(FeatureID);

 if FeatureID >= 4 then
  begin
   CallCPUID(4, 0, PhysicalCoreInfo, Unused, Unused, Unused);
   FPhysicalCore := ((PhysicalCoreInfo and $FC000000) shr 26) + 1;
  end;

 // check Intel extended
 CallCPUID($80000000, 0, ExFeatID, Unused, Unused, Unused);

 if ExFeatID >= $80000001 then
  begin
   FHasExtendedInfo := True;
   CallCPUID($80000001, 0, Unused, Unused, FEx64Features2,
     FEx64Features);
  end;

   // get CPU name
 if ExFeatID >= $80000002
  then CallCPUID($80000002, 0, FCPUName[0], FCPUName[4], FCPUName[8], FCPUName[12]);
 if ExFeatID >= $80000003
  then CallCPUID($80000003, 0, FCPUName[16], FCPUName[20], FCPUName[24], FCPUName[28]);
 if ExFeatID >= $80000004
  then CallCPUID($80000004, 0, FCPUName[32], FCPUName[36], FCPUName[40], FCPUName[44]);

 if ExFeatID >= $80000008 then
  begin
   CallCPUID($80000008, 0, AddressSize, Unused, Unused, Unused);
   FPhysicalAddressBits := AddressSize and $000000FF;
   FVirtualAddressBits := (AddressSize and $0000FF00) shr 8;
  end;

 if FeatureID >= 2
  then FCacheInfo := TIntelCacheInformation.Create(ExFeatID);

 if not FHasExtendedInfo then
  begin
   case FFamily of
     4: case FModel of
            1: FCPUName := 'Intel 486DX Processor';
            2: FCPUName := 'Intel 486SX Processor';
            3: FCPUName := 'Intel DX2 Processor';
            4: FCPUName := 'Intel 486 Processor';
            5: FCPUName := 'Intel SX2 Processor';
            7: FCPUName := 'Write-Back Enhanced Intel DX2 Processor';
            8: FCPUName := 'Intel DX4 Processor';
          else FCPUName := 'Intel 486 Processor';
        end;
     5: FCPUName := 'Pentium';
     6: case FModel of
            1: FCPUName := 'Pentium Pro';
            3: FCPUName := 'Pentium II';
            5: if (FCacheInfo is TCustomL1L2CacheInformation) and
                  Assigned(TCustomL1L2CacheInformation(FCacheInfo).L2Cache) then
                case TCustomL1L2CacheInformation(FCacheInfo).L2Cache.Size of
                    0: FCPUName := 'Celeron';
                 1024: FCPUName := 'Pentium II Xeon';
                 2048: FCPUName := 'Pentium II Xeon';
                 else  FCPUName := 'Pentium II';
                end
               else FCPUName := 'Celeron';
            6: if (FCacheInfo is TCustomL1L2CacheInformation) and
                  Assigned(TCustomL1L2CacheInformation(FCacheInfo).L2Cache) then
                case TCustomL1L2CacheInformation(FCacheInfo).L2Cache.Size of
                   0: FCPUName := 'Celeron';
                 128: FCPUName := 'Celeron';
                 else FCPUName := 'Pentium II';
                end
               else FCPUName := 'Celeron';
            7: if (FCacheInfo is TCustomL1L2CacheInformation) and
                  Assigned(TCustomL1L2CacheInformation(FCacheInfo).L2Cache) then
                case TCustomL1L2CacheInformation(FCacheInfo).L2Cache.Size of
                 1024: FCPUName := 'Pentium III Xeon';
                 2048: FCPUName := 'Pentium III Xeon';
                 else  FCPUName := 'Pentium III';
                end
               else FCPUName := 'Pentium III';
            8: case FBrandID of
                   1: FCPUName := 'Celeron';
                   2: FCPUName := 'Pentium III';
                   3: FCPUName := 'Pentium III Xeon';
                   4: FCPUName := 'Pentium III';
                else FCPUName := 'Pentium III';
               end;
           10: FCPUName := 'Pentium III Xeon';
           11: FCPUName := 'Pentium III';
         else {$IFDEF DELPHI20_UP}AnsiStrings.{$ENDIF}StrPCopy(FCPUName, AnsiString(Format('P6 (Model %d)', [FModel])));
        end;
    15: case FBrandID of
            1: FCPUName := 'Celeron';
            8: FCPUName := 'Pentium 4';
           14: FCPUName := 'Xeon';
         else  FCPUName := 'Pentium 4';
        end;
    else
      {$IFDEF DELPHI20_UP}AnsiStrings.{$ENDIF}StrPCopy(FCPUName, AnsiString(Format('P%d', [FFamily])));
    end;
  end;

 // detect MMX, SSE & Co.
 FSupportsSSE := [];
 if (FFeatures and $02000000) <> 0 then Include(FSupportsSSE, ssSSE);
 if (FFeatures and $04000000) <> 0 then Include(FSupportsSSE, ssSSE2);
 if (FExFeatures and $00000001) <> 0 then Include(FSupportsSSE, ssSSE3);
 if (FExFeatures and $00000200) <> 0 then Include(FSupportsSSE, ssSSE3X);
 if (FExFeatures and $00080000) <> 0 then Include(FSupportsSSE, ssSSE4A);
 if (FExFeatures and $00100000) <> 0 then Include(FSupportsSSE, ssSSE4B);
 if (FExFeatures and $10000000) <> 0 then Include(FSupportsSSE, ssAVX);

 // detect misc. features
 FIs64Bits := FHasExtendedInfo and ((FEx64Features and $20000000) <> 0);
 FDepCapable := FHasExtendedInfo and ((FEx64Features and $00100000) <> 0);
end;

class function TProcessorFeaturesIntel.GetManufacturer: string;
begin
 Result := 'Intel';
end;


{ TProcessorFeaturesAMD }

constructor TProcessorFeaturesAMD.Create(FeatureID: Cardinal);
var
  ExFeatID, Unused, VersionInfo, AdditionalInfo: Cardinal;
begin
 FCpuType := ctAMD;

 // check AMD extended
 if FeatureID >= 1 then
  begin
   CallCPUID(1, 0, VersionInfo, AdditionalInfo, FFeatures2, FFeatures);

   FBrandID := AdditionalInfo and $000000FF;
   FFlushLineSize := (AdditionalInfo and $0000FF00) shr 8;
   FAPICID := (AdditionalInfo and $FF000000) shr 24;
   if HyperThreading then
    begin
     FLogicalCore := (AdditionalInfo and $00FF0000) shr 16;
     if FLogicalCore = 0
      then FLogicalCore := 1;
    end;
  end;

 CallCPUID($80000000, 0, ExFeatID, Unused, Unused, Unused);

 if ExFeatID <> 0 then
  begin
   // AMD only
   FHasExtendedInfo := True;

   if ExFeatID >= $80000001 then
    begin
     CallCPUID($80000001, 0, VersionInfo, AdditionalInfo, FExFeatures2, FExFeatures);
     FFamily := (VersionInfo and $00000F00) shr 8;
     FModel := (VersionInfo and $000000F0) shr 4;
     FStepping := (VersionInfo and $0000000F);
     FExtendedModel := (VersionInfo and $000F0000) shr 16;
     FExtendedFamily := (VersionInfo and $0FF00000) shr 20;
     FExBrandID := AdditionalInfo and $0000FFFF;
    end;

   // get CPU name
   if ExFeatID >= $80000002
    then CallCPUID($80000002, 0, FCPUName[0], FCPUName[4], FCPUName[8], FCPUName[12]);
   if ExFeatID >= $80000003
    then CallCPUID($80000003, 0, FCPUName[16], FCPUName[20], FCPUName[24], FCPUName[28]);
   if ExFeatID >= $80000004
    then CallCPUID($80000004, 0, FCPUName[32], FCPUName[36], FCPUName[40], FCPUName[44]);

   if ExFeatID >= $80000005
    then FCacheInfo := TAMDCacheInformation.Create(ExFeatID);

   if ExFeatID >= $80000007
    then CallCPUID($80000007, 0, Unused, Unused, Unused, FAdvancedPowerManagement);

   if ExFeatID >= $80000008 then
    begin
     CallCPUID($80000008, 0, Unused, VersionInfo, AdditionalInfo, Unused);
     FPhysicalAddressSize := VersionInfo and $000000FF;
     FVirtualAddressSize := (VersionInfo and $0000FF00) shr 8;
     FPhysicalCore := (AdditionalInfo and $000000FF) + 1;
    end;
  end
 else
  begin
   inherited Create(FeatureID);

   case FFamily of
     4: FCPUName := 'Am486(R) or Am5x86';
     5: case FModel of
          0: FCPUName := 'AMD-K5 (Model 0)';
          1: FCPUName := 'AMD-K5 (Model 1)';
          2: FCPUName := 'AMD-K5 (Model 2)';
          3: FCPUName := 'AMD-K5 (Model 3)';
          6: FCPUName := 'AMD-K6® (Model 6)';
          7: FCPUName := 'AMD-K6® (Model 7)';
          8: FCPUName := 'AMD-K6®-2 (Model 8)';
          9: FCPUName := 'AMD-K6®-III (Model 9)';
        else {$IFDEF DELPHI20_UP}AnsiStrings.{$ENDIF}StrFmt(FCPUName, PAnsiChar(AnsiString('Unknown AMD (Model %d)')), [FModel]);
       end;
     6: case FModel of
          1: FCPUName := 'AMD Athlon™ (Model 1)';
          2: FCPUName := 'AMD Athlon™ (Model 2)';
          3: FCPUName := 'AMD Duron™ (Model 3)';
          4: FCPUName := 'AMD Athlon™ (Model 4)';
          6: FCPUName := 'AMD Athlon™ XP (Model 6)';
          7: FCPUName := 'AMD Duron™ (Model 7)';
          8: FCPUName := 'AMD Athlon™ XP (Model 8)';
         10: FCPUName := 'AMD Athlon™ XP (Model 10)';
        else {$IFDEF DELPHI20_UP}AnsiStrings.{$ENDIF}StrFmt(FCPUName, PAnsiChar(AnsiString('Unknown AMD (Model %d)')), [FModel]);
       end;
     8:

     else
       FCPUName := 'Unknown AMD Chip';
   end;
 end;

 // hier ist noch etwas komisch!!!
 FHasExMMX := FHasExtendedInfo and ((FExFeatures and $00400000) <> 0);
 FHas3DNow := FHasExtendedInfo and ((FExFeatures and $80000000) <> 0);
 FHasEx3DNow := FHasExtendedInfo and ((FExFeatures and $40000000) <> 0);
 FSupportsSSE := [];
 if (FFeatures and $02000000) <> 0 then Include(FSupportsSSE, ssSSE);
 if (FFeatures and $04000000) <> 0 then Include(FSupportsSSE, ssSSE2);
 if (FFeatures2 and $00000001) <> 0 then Include(FSupportsSSE, ssSSE3);
 if FHasExtendedInfo then
  begin
   if (FExFeatures2 and $00000040) <> 0
    then Include(FSupportsSSE, ssSSE4A);
   if (FExFeatures2 and $00000800) <> 0
    then Include(FSupportsSSE, ssSSE5);
  end;

 FIs64Bits := FHasExtendedInfo and ((FExFeatures and $20000000) <> 0);
 FDEPCapable := FHasExtendedInfo and ((FExFeatures and $00100000) <> 0);
end;

class function TProcessorFeaturesAMD.GetManufacturer: string;
begin
 Result := 'AMD';
end;


{ TProcessorFeaturesCyrix }

constructor TProcessorFeaturesCyrix.Create(FeatureID: Cardinal);
var
  ExFeatID, Unused, VersionInfo, AdditionalInfo: Cardinal;
begin
 FCpuType := ctCyrix;

 // check Cyrix extended
 CallCPUID($80000000, 0, ExFeatID, Unused, Unused, Unused);
 if ExFeatID <> 0 then
  begin
   // Cyrix only
   FHasExtendedInfo := True;
   if ExFeatID >= $80000001 then
    begin
     CallCPUID($80000001, 0, VersionInfo, AdditionalInfo, Unused, FFeatures);
     FProcessorType := (VersionInfo and $0000F000) shr 12;
     FFamily := (VersionInfo and $00000F00) shr 8;
     FModel := (VersionInfo and $000000F0) shr 4;
     FStepping := (VersionInfo and $0000000F);
    end;

   if ExFeatID >= $80000002
    then CallCPUID($80000002, 0, FCPUName[0], FCPUName[4], FCPUName[8], FCPUName[12]);
   if ExFeatID >= $80000003
    then CallCPUID($80000003, 0, FCPUName[16], FCPUName[20], FCPUName[24], FCPUName[28]);
   if ExFeatID >= $80000004
    then CallCPUID($80000004, 0, FCPUName[32], FCPUName[36], FCPUName[40], FCPUName[44]);

   if ExFeatID >= $80000005
    then FCacheInfo := TCyrixCacheInformation.Create(ExFeatID);
  end
 else
  begin
   inherited Create(FeatureID);
   case FFamily of
    4: FCPUName := 'Cyrix MediaGX';
    5: case FModel of
         2: FCPUName := 'Cyrix 6x86';
         4: FCPUName := 'Cyrix GXm';
       end;
    6: FCPUName := '6x86MX';
    else {$IFDEF DELPHI20_UP}AnsiStrings.{$ENDIF}StrPCopy(FCPUName, AnsiString(Format('%dx86', [FFamily])));
   end;
  end;
end;

class function TProcessorFeaturesCyrix.GetManufacturer: string;
begin
 Result := 'Cyrix';
end;


{ TProcessorFeaturesVIA }

constructor TProcessorFeaturesVIA.Create(FeatureID: Cardinal);
var
  ExFeatID, Unused, VersionInfo: Cardinal;
begin
 FCpuType := ctVIA;

 // check VIA extended
 CallCPUID($80000000, 0, ExFeatID, Unused, Unused, Unused);
 if ExFeatID <> 0 then
  begin
   if ExFeatID >= $80000001 then
    begin
     FHasExtendedInfo := True;
     CallCPUID($80000001, 0, VersionInfo, Unused, Unused, FExFeatures);
     FProcessorType := (VersionInfo and $00003000) shr 12;
     FFamily := (VersionInfo and $00000F00) shr 8;
     FModel := (VersionInfo and $000000F0) shr 4;
     FStepping := (VersionInfo and $0000000F);
    end;
   if ExFeatID >= $80000002
    then CallCPUID($80000002, 0, FCPUName[0], FCPUName[4], FCPUName[8], FCPUName[12]);
   if ExFeatID >= $80000003
    then CallCPUID($80000003, 0, FCPUName[16], FCPUName[20], FCPUName[24], FCPUName[28]);
   if ExFeatID >= $80000004
    then CallCPUID($80000004, 0, FCPUName[32], FCPUName[36], FCPUName[40], FCPUName[44]);

   if ExFeatID >= $80000005
    then FCacheInfo := TViaCacheInformation.Create(ExFeatID);

   CallCPUID($C0000000, 0, ExFeatID, Unused, Unused, Unused);
   if ExFeatID >= $C0000001
    then CallCPUID($C0000001, 0, Unused, Unused, Unused, FExFeatures);
  end
   else inherited Create(FeatureID);

 if not FHasExtendedInfo
  then FCPUName := 'C3';

 FSupportsSSE := [];
 if (FFeatures and $02000000) <> 0 then Include(FSupportsSSE, ssSSE);
 FHas3DNow := (FFeatures and $80000000) <> 0;
end;

class function TProcessorFeaturesVIA.GetManufacturer: string;
begin
 Result := 'VIA';
end;


{ TProcessorFeaturesTransmeta }

constructor TProcessorFeaturesTransmeta.Create(FeatureID: Cardinal);
var
  ExFeatID, Unused, VersionInfo: Cardinal;
begin
 FCpuType := ctTransmeta;

 if (FeatureID >= 1) then
  begin
   CallCPUID(1, 0, VersionInfo, Unused, Unused, FFeatures);
   FProcessorType := (VersionInfo and $00003000) shr 12;
   FFamily := (VersionInfo and $00000F00) shr 8;
   FModel := (VersionInfo and $000000F0) shr 4;
   FStepping := (VersionInfo and $0000000F);
  end;

 // small CPU description, overriden if ExFeatID >= 80000002
 CallCPUID($80000000, 0, ExFeatID, FCPUName[0], FCPUName[8], FCPUName[4]);
 if ExFeatID <> 0 then
  begin
   FHasExtendedInfo := True;

   if ExFeatID >= $80000001
    then CallCPUID($80000001, 0, Unused, Unused, Unused, FExFeatures);
   if ExFeatID >= $80000002
    then CallCPUID($80000002, 0, FCPUName[0], FCPUName[4], FCPUName[8], FCPUName[12]);
   if ExFeatID >= $80000003
    then CallCPUID($80000003, 0, FCPUName[16], FCPUName[20], FCPUName[24], FCPUName[28]);
   if ExFeatID >= $80000004
    then CallCPUID($80000004, 0, FCPUName[32], FCPUName[36], FCPUName[40], FCPUName[44]);

   if ExFeatID >= $80000005
    then FCacheInfo := TTransmetaCacheInformation.Create(ExFeatID);
  end
 else FCPUName := 'Crusoe';

  CallCPUID($80860000, 0, ExFeatID, Unused, Unused, Unused);
  if ExFeatID <> 0 then
  begin
    if ExFeatID >= $80860001 then
      CallCPUID($80860001, 0, Unused, FRevisionABCD, FRevisionXXXX,
        FTransmetaFeatures);
    if ExFeatID >= $80860002 then
      CallCPUID($80860002, 0, Unused, FCodeMorphingABCD, FCodeMorphingXXXX, Unused);
    if ExFeatID >= $80860003 then
      CallCPUID($80860003, 0, FTransmetaInformations[0], FTransmetaInformations[4],
        FTransmetaInformations[8], FTransmetaInformations[12]);
    if ExFeatID >= $80860004 then
      CallCPUID($80860004, 0, FTransmetaInformations[16], FTransmetaInformations[20],
        FTransmetaInformations[24], FTransmetaInformations[28]);
    if ExFeatID >= $80860005 then
      CallCPUID($80860005, 0, FTransmetaInformations[32], FTransmetaInformations[36],
        FTransmetaInformations[40], FTransmetaInformations[44]);
    if ExFeatID >= $80860006 then
      CallCPUID($80860006, 0, FTransmetaInformations[48], FTransmetaInformations[52],
        FTransmetaInformations[56], FTransmetaInformations[60]);
    if (ExFeatID >= $80860007) and ((FTransmetaFeatures and $00000002) <> 0) then
      CallCPUID($80860007, 0, FCurrentFrequency, FCurrentVoltage,
        FCurrentPerformance, Unused);
  end;
end;

class function TProcessorFeaturesTransmeta.GetManufacturer: string;
begin
 Result := 'Transmeta';
end;

initialization
  ProcessorInfo := TProcessorInfo.Create;

finalization
  FreeAndNil(ProcessorInfo);

end.
