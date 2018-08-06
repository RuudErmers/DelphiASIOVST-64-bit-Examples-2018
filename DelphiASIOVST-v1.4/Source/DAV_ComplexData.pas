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

unit DAV_ComplexData;

interface

{$I DAV_Compiler.inc}

uses
  Classes, DAV_Classes, DAV_Complex;

type
  TCustomComplexData = class;
  TCustomComplexChannels = class(TOwnedCollection);
  TComplexChannels32 = class(TCustomComplexChannels);
  TComplexChannels64 = class(TCustomComplexChannels);

  TCustomComplexChannel = class(TCollectionItem)
  private
    FChannelData: TCustomComplexChannels;
    FBinCount: Cardinal;
    function GetComplexData: TCustomComplexData;
  protected
{$IFNDEF FPC}
    FDisplayName: string;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
{$ENDIF}
    procedure AssignTo(Dest: TPersistent); override;
    procedure BinCountChanged; virtual;
    property ComplexData: TCustomComplexData read GetComplexData;
  public
    constructor Create(ACollection: TCollection); override;

    // some processing functions
    procedure Clear; virtual; abstract;

    property BinCount: Cardinal read FBinCount;
  published
    property DisplayName;
  end;

  TComplexChannel32 = class(TCustomComplexChannel)
  private
    FChannelDataPtr: PDAVComplexSingleFixedArray;
    function GetChannelDataImaginary(Bin: Int64): Single;
    function GetChannelDataMagnitude(Bin: Int64): Single;
    function GetChannelDataPhase(Bin: Int64): Single;
    function GetChannelDataReal(Bin: Int64): Single;
    procedure SetChannelDataImaginary(Bin: Int64; const Value: Single);
    procedure SetChannelDataMagnitude(Bin: Int64; const Value: Single);
    procedure SetChannelDataPhase(Bin: Int64; const Value: Single);
    procedure SetChannelDataReal(Bin: Int64; const Value: Single);
  protected
    procedure BinCountChanged; override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Clear; override;

    // data access properties
    property ChannelDataReal[Sample: Int64]: Single read GetChannelDataReal
      write SetChannelDataReal;
    property ChannelDataImaginary[Sample: Int64]: Single
      read GetChannelDataImaginary write SetChannelDataImaginary;
    property ChannelDataMagnitude[Sample: Int64]: Single
      read GetChannelDataMagnitude write SetChannelDataMagnitude;
    property ChannelDataPhase[Sample: Int64]: Single read GetChannelDataPhase
      write SetChannelDataPhase;
    property ChannelDataPointer: PDAVComplexSingleFixedArray
      read FChannelDataPtr;
  end;

  TComplexChannel64 = class(TCustomComplexChannel)
  private
    FChannelDataPtr: PDAVComplexDoubleFixedArray;
    function GetChannelDataImaginary(Bin: Int64): Double;
    function GetChannelDataMagnitude(Bin: Int64): Double;
    function GetChannelDataPhase(Bin: Int64): Double;
    function GetChannelDataReal(Bin: Int64): Double;
    procedure SetChannelDataImaginary(Bin: Int64; const Value: Double);
    procedure SetChannelDataMagnitude(Bin: Int64; const Value: Double);
    procedure SetChannelDataPhase(Bin: Int64; const Value: Double);
    procedure SetChannelDataReal(Bin: Int64; const Value: Double);
  protected
    procedure BinCountChanged; override;
  public
    destructor Destroy; override;

    // some processing functions
    procedure Clear; override;

    // data acces properties
    property ChannelDataReal[Sample: Int64]: Double read GetChannelDataReal
      write SetChannelDataReal;
    property ChannelDataImaginary[Sample: Int64]: Double
      read GetChannelDataImaginary write SetChannelDataImaginary;
    property ChannelDataMagnitude[Sample: Int64]: Double
      read GetChannelDataMagnitude write SetChannelDataMagnitude;
    property ChannelDataPhase[Sample: Int64]: Double read GetChannelDataPhase
      write SetChannelDataPhase;
    property ChannelDataPointer: PDAVComplexDoubleFixedArray
      read FChannelDataPtr;
  end;

  TCustomComplexData = class(TAudioComponent)
  private
    FBinCount: Cardinal;
    procedure SetBinCount(const Value: Cardinal);
  protected
    FChannels: TCustomComplexChannels;
    procedure BinCountChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BinCount: Cardinal read FBinCount write SetBinCount;
    property Channels: TCustomComplexChannels read FChannels write FChannels;
  end;

  TComplexData32 = class(TCustomComplexData)
  private
    function GetComplexChannel(index: Integer): TComplexChannel32; virtual;
  protected
    property ChannelList[index: Integer]: TComplexChannel32
      read GetComplexChannel; default;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Channels;
    property BinCount;
    property SampleRate;
    property SampleRateSource;
  end;

  TComplexData64 = class(TCustomComplexData)
  private
    function GetComplexChannel(index: Integer): TComplexChannel64; virtual;
  protected
    property ChannelList[index: Integer]: TComplexChannel64
      read GetComplexChannel; default;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Channels;
    property BinCount;
    property SampleRate;
    property SampleRateSource;
  end;

implementation

uses
  SysUtils, Math;

resourcestring
  RStrChannel = 'Channel';

  { TCustomComplexChannel }

procedure TCustomComplexChannel.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomComplexChannel then
  begin
{$IFNDEF FPC}
    TCustomComplexChannel(Dest).FDisplayName := FDisplayName;
{$ENDIF}
    TCustomComplexChannel(Dest).FChannelData := FChannelData;
  end
  else
    inherited;
end;

constructor TCustomComplexChannel.Create(ACollection: TCollection);
begin
  inherited;
  DisplayName := RStrChannel + ' ' + IntToStr(ACollection.Count);
  BinCountChanged;
end;

function TCustomComplexChannel.GetComplexData: TCustomComplexData;
begin
  assert(Collection is TCustomComplexChannels);
  assert(TCustomComplexChannels(Collection).GetOwner is TCustomComplexData);
  result := TCustomComplexData(TCustomComplexChannels(GetOwner).GetOwner);
end;

{$IFNDEF FPC}

function TCustomComplexChannel.GetDisplayName: string;
begin
  result := FDisplayName;
end;

procedure TCustomComplexChannel.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
  inherited;
end;
{$ENDIF}

procedure TCustomComplexChannel.BinCountChanged;
begin
  FBinCount := ComplexData.BinCount;
end;

{ TComplexChannel32 }

procedure TComplexChannel32.Clear;
begin
  FillChar(FChannelDataPtr^, BinCount * SizeOf(TComplex32), 0);
end;

destructor TComplexChannel32.Destroy;
begin
  if assigned(FChannelDataPtr) then
  begin
    Dispose(FChannelDataPtr);
    FChannelDataPtr := nil;
  end;
  inherited;
end;

function TComplexChannel32.GetChannelDataReal(Bin: Int64): Single;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := FChannelDataPtr^[Bin].Re
  else
    raise Exception.Create('Bin out of range');
end;

function TComplexChannel32.GetChannelDataImaginary(Bin: Int64): Single;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := FChannelDataPtr^[Bin].Im
  else
    raise Exception.Create('Bin out of range');
end;

function TComplexChannel32.GetChannelDataMagnitude(Bin: Int64): Single;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := sqrt(sqr(FChannelDataPtr^[Bin].Re) +
      sqr(FChannelDataPtr^[Bin].Im))
  else
    raise Exception.Create('Bin out of range');
end;

function TComplexChannel32.GetChannelDataPhase(Bin: Int64): Single;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := arctan2(FChannelDataPtr^[Bin].Im, FChannelDataPtr^[Bin].Re)
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.BinCountChanged;
begin
  ReallocMem(FChannelDataPtr, ComplexData.BinCount * SizeOf(TComplex32));

  // check if new length is longer than the old length and fill with zeroes if necessary
  if ComplexData.BinCount > BinCount then
    FillChar(FChannelDataPtr^[BinCount], (ComplexData.BinCount - BinCount) *
      SizeOf(TComplex32), 0);

  inherited;
end;

procedure TComplexChannel32.SetChannelDataReal(Bin: Int64; const Value: Single);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    FChannelDataPtr^[Bin].Re := Value
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.SetChannelDataImaginary(Bin: Int64;
  const Value: Single);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    FChannelDataPtr^[Bin].Im := Value
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.SetChannelDataMagnitude(Bin: Int64;
  const Value: Single);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    raise Exception.Create('Not supported yet!')
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel32.SetChannelDataPhase(Bin: Int64;
  const Value: Single);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    raise Exception.Create('Not supported yet!')
  else
    raise Exception.Create('Bin out of range');
end;

{ TComplexChannel64 }

procedure TComplexChannel64.Clear;
begin
  FillChar(FChannelDataPtr^, ComplexData.BinCount * SizeOf(TComplex64), 0);
end;

destructor TComplexChannel64.Destroy;
begin
  if assigned(FChannelDataPtr) then
  begin
    Dispose(FChannelDataPtr);
    FChannelDataPtr := nil;
  end;
  inherited;
end;

function TComplexChannel64.GetChannelDataReal(Bin: Int64): Double;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := FChannelDataPtr^[Bin].Re
  else
    raise Exception.Create('Bin out of range');
end;

function TComplexChannel64.GetChannelDataImaginary(Bin: Int64): Double;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := FChannelDataPtr^[Bin].Im
  else
    raise Exception.Create('Bin out of range');
end;

function TComplexChannel64.GetChannelDataMagnitude(Bin: Int64): Double;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := sqrt(sqr(FChannelDataPtr^[Bin].Re) +
      sqr(FChannelDataPtr^[Bin].Im))
  else
    raise Exception.Create('Bin out of range');
end;

function TComplexChannel64.GetChannelDataPhase(Bin: Int64): Double;
begin
  if (Bin >= 0) and (Bin < BinCount) then
    result := arctan2(FChannelDataPtr^[Bin].Im, FChannelDataPtr^[Bin].Re)
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataReal(Bin: Int64; const Value: Double);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    FChannelDataPtr^[Bin].Re := Value
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataImaginary(Bin: Int64;
  const Value: Double);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    FChannelDataPtr^[Bin].Im := Value
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataMagnitude(Bin: Int64;
  const Value: Double);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    raise Exception.Create('Not supported yet!')
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.SetChannelDataPhase(Bin: Int64;
  const Value: Double);
begin
  if (Bin >= 0) and (Bin < ComplexData.BinCount) then
    raise Exception.Create('Not supported yet!')
  else
    raise Exception.Create('Bin out of range');
end;

procedure TComplexChannel64.BinCountChanged;
begin
  ReallocMem(FChannelDataPtr, ComplexData.BinCount * SizeOf(TComplex64));

  // check if new length is longer than the old length and fill with zeroes if necessary
  if ComplexData.BinCount > BinCount then
    FillChar(FChannelDataPtr^[BinCount], (ComplexData.BinCount - BinCount) *
      SizeOf(TComplex64), 0);

  inherited;
end;

{ TCustomComplexData }

constructor TCustomComplexData.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCustomComplexData.Destroy;
begin
  if assigned(FChannels) then
    FreeAndNil(FChannels);
  inherited;
end;

procedure TCustomComplexData.BinCountChanged;
var
  ch: Integer;
begin
  for ch := 0 to FChannels.Count - 1 do
  begin
    assert(FChannels.Items[ch] is TCustomComplexChannel);
    if TCustomComplexChannel(FChannels.Items[ch]).BinCount <> FBinCount then
      TCustomComplexChannel(FChannels.Items[ch]).BinCountChanged;
  end;
end;

procedure TCustomComplexData.SetBinCount(const Value: Cardinal);
begin
  if FBinCount <> Value then
  begin
    FBinCount := Value;
    BinCountChanged;
  end;
end;

{ TComplexData32 }

constructor TComplexData32.Create(AOwner: TComponent);
begin
  inherited;
  FChannels := TCustomComplexChannels.Create(Self, TComplexChannel32);
end;

function TComplexData32.GetComplexChannel(index: Integer): TComplexChannel32;
begin
  if (Index < 0) or (Index >= FChannels.Count) then
    raise Exception.Create('Index out of bounds')
  else
    result := TComplexChannel32(FChannels.Items[index]);
end;

{ TComplexData64 }

constructor TComplexData64.Create(AOwner: TComponent);
begin
  inherited;
  FChannels := TCustomComplexChannels.Create(Self, TComplexChannel64);
end;

function TComplexData64.GetComplexChannel(index: Integer): TComplexChannel64;
begin
  if (Index < 0) or (Index >= FChannels.Count) then
    raise Exception.Create('Index out of bounds')
  else
    result := TComplexChannel64(FChannels.Items[index]);
end;

end.
