unit DAV_DspSawtoothOscillator;

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
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterButterworth,
  DAV_DspFilterChebyshevType1, DAV_DspPolyphaseDownsampler;

type                                    
  TCustomSawtoothOscillator = class(TDspSampleRatePersistent, IDspGenerator32)
  private
    FFrequency : Single;
    FIntOffset : Cardinal;
    FIntValue  : Cardinal;
    Scale      : Double;
    procedure SetFrequency(const Value: Single);
  protected
    procedure CalculateIntegerOffset; virtual;
    procedure FrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;

    function ProcessSample32: Single;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleFrames: Integer);

    property Frequency: Single read FFrequency write SetFrequency;
  end;

  TCustomOversampledSawtoothOscillator = class(TCustomSawtoothOscillator)
  private
    FOSFactor : Integer;
    procedure SetOversamplingFactor(const Value: Integer);
  protected
    procedure CalculateIntegerOffset; override;
    procedure OversamplingFactorChanged; virtual;
  public
    property OversamplingFactor: Integer read FOSFactor write SetOversamplingFactor;
  end;

  TPolyphaseOversampledSawtoothOscillator = class(TCustomSawtoothOscillator)
  private
    FOsOrder     : Integer;
    FDownsampler : array of TPolyphaseDownsampler32;
    procedure SetOversamplingOrder(const Value: Integer);
  protected
    procedure CalculateIntegerOffset; override;
    procedure OversamplingOrderChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample32: Single;

    property OversamplingOrder: Integer read FOSOrder write SetOversamplingOrder;
  end;

  TCustomOversampledFilterSawtoothOscillator = class(TCustomOversampledSawtoothOscillator)
  private
    FFilterOrder : Integer;
    procedure SetFilterOrder(const Value: Integer);
  protected
    FFilter : TCustomOrderFilter;
    procedure FilterOrderChanged; virtual;
    procedure OversamplingFactorChanged; override;
    procedure SampleRateChanged; override;
    procedure UpdateOversamplingFilter; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ProcessSample32: Single;
  published
    property FilterOrder: Integer read FFilterOrder write SetFilterOrder;
  end;

  TButterworthOversampledSawtoothOscillator = class(TCustomOversampledFilterSawtoothOscillator)
  public
    constructor Create; override;
  end;

  TChebyshevOversampledSawtoothOscillator = class(TCustomOversampledFilterSawtoothOscillator)
  private
    FRipple: Single;
    procedure SetRipple(const Value: Single);
  protected
    procedure RippleChanged; virtual;
  public
    constructor Create; override;
  published
    property Ripple: Single read FRipple write SetRipple;
  end;

  TSawtoothOscillator = class(TCustomSawtoothOscillator)
  published
    property Frequency;
  end;

  TTriangleOscillator = class(TCustomSawtoothOscillator)
  public
    function ProcessSample32: Single;
  published
    property Frequency;
  end;

  TRectangleOscillator = class(TCustomSawtoothOscillator)
  public
    function ProcessSample32: Single;
  published
    property Frequency;
  end;

implementation

uses
  SysUtils, Math, DAV_Common;

resourcestring
  RCStrPositiveValueOnly = 'Value must be larger than 0!';

{ TCustomSawtoothOscillator }

constructor TCustomSawtoothOscillator.Create;
begin
 inherited;
 Scale := 1 / MaxInt;
end;

procedure TCustomSawtoothOscillator.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomSawtoothOscillator.FrequencyChanged;
begin
 CalculateIntegerOffset;
end;

procedure TCustomSawtoothOscillator.ProcessBlock32(
  const Data: PDAVSingleFixedArray; SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Data^[Sample] := 2 * FIntValue * Scale - 1;
   Inc(FIntValue, FIntOffset);
  end;
end;

function TCustomSawtoothOscillator.ProcessSample32: Single;
begin
 Result := 2 * FIntValue * Scale - 1;
 Inc(FIntValue, FIntOffset);
end;

procedure TCustomSawtoothOscillator.SampleRateChanged;
begin
 CalculateIntegerOffset;
 inherited;
end;

procedure TCustomSawtoothOscillator.CalculateIntegerOffset;
begin
 FIntOffset := Round(0.5 * FFrequency / SampleRate * High(Cardinal));
end;


{ TCustomOversampledSawtoothOscillator }

procedure TCustomOversampledSawtoothOscillator.CalculateIntegerOffset;
begin
 inherited;
 FIntOffset := Round(0.5 * FFrequency / FOSFactor / SampleRate * High(Cardinal));
end;

procedure TCustomOversampledSawtoothOscillator.OversamplingFactorChanged;
begin
 CalculateIntegerOffset;
end;

procedure TCustomOversampledSawtoothOscillator.SetOversamplingFactor(
  const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create(RCStrPositiveValueOnly);

 if FOSFactor <> Value then
  begin
   FOSFactor := Value;
   OversamplingFactorChanged
  end;
end;


{ TPolyphaseOversampledSawtoothOscillator }

constructor TPolyphaseOversampledSawtoothOscillator.Create;
begin
 inherited;
 FOsOrder := 1;
 OversamplingOrderChanged;
end;

destructor TPolyphaseOversampledSawtoothOscillator.Destroy;
var
  i : Integer;
begin
 for i := 0 to Length(FDownsampler) - 1
  do FreeAndNil(FDownsampler);

 inherited;
end;

procedure TPolyphaseOversampledSawtoothOscillator.CalculateIntegerOffset;
begin
 inherited;
 FIntOffset := Round(0.5 * FFrequency / (1 shl FOsOrder) /
   SampleRate * High(Cardinal));
end;

procedure TPolyphaseOversampledSawtoothOscillator.SetOversamplingOrder(
  const Value: Integer);
begin
 if Value <= 0
  then raise Exception.Create(RCStrPositiveValueOnly);

 if FOSOrder <> Value then
  begin
   FOSOrder := Value;
   OversamplingOrderChanged
  end;
end;

procedure TPolyphaseOversampledSawtoothOscillator.OversamplingOrderChanged;
var
  i : Integer;
begin
 CalculateIntegerOffset;

 for i := FOsOrder to Length(FDownsampler) - 1
  do FreeAndNil(FDownsampler[i]);

 SetLength(FDownsampler, FOsOrder);

 for i := 0 to Length(FDownsampler) - 1 do
  if not assigned(FDownsampler[i])
   then FDownsampler[i] := TPolyphaseDownsampler32.Create;
end;

function TPolyphaseOversampledSawtoothOscillator.ProcessSample32: Single;
var
  Stage : Integer;
  Data  : array of TDAV2SingleArray;
begin
 Result := 0;
 // not yet done!!!
 SetLength(Data, FOsOrder);
 for Stage := 0 to (1 shl FOsOrder) - 1 do
  begin
   Result := 2 * FIntValue * Scale - 1;
   Inc(FIntValue, FIntOffset);
  end;
end;


{ TCustomOversampledFilterSawtoothOscillator }

constructor TCustomOversampledFilterSawtoothOscillator.Create;
begin
 inherited;
 FFilter.SampleRate := SampleRate;
 FFilterOrder := FFilter.Order;
end;

destructor TCustomOversampledFilterSawtoothOscillator.Destroy;
begin
 FreeAndNil(FFilter);
 inherited;
end;

procedure TCustomOversampledFilterSawtoothOscillator.OversamplingFactorChanged;
begin
 inherited;
 UpdateOversamplingFilter;
end;

procedure TCustomOversampledFilterSawtoothOscillator.SampleRateChanged;
begin
 inherited;
 UpdateOversamplingFilter;
end;

procedure TCustomOversampledFilterSawtoothOscillator.SetFilterOrder(
  const Value: Integer);
begin
 if FFilterOrder <> Value then
  begin
   FFilterOrder := Value;
   FilterOrderChanged;
  end;
end;

procedure TCustomOversampledFilterSawtoothOscillator.FilterOrderChanged;
begin
 FFilter.Order := FFilterOrder;
end;

procedure TCustomOversampledFilterSawtoothOscillator.UpdateOversamplingFilter;
begin
 FFilter.Frequency := 0.5 * SampleRate / FOSFactor;
end;

function TCustomOversampledFilterSawtoothOscillator.ProcessSample32: Single;
var
  Stage : Integer;
begin
 Result := 0;
 for Stage := 0 to FOSFactor - 1 do
  begin
   Result := FFilter.ProcessSample32(2 * FIntValue * Scale - 1);
   Inc(FIntValue, FIntOffset);
  end;
end;


{ TButterworthOversampledSawtoothOscillator }

constructor TButterworthOversampledSawtoothOscillator.Create;
begin
 FFilter := TButterworthLowPassFilter.Create;
 inherited;
end;


{ TChebyshevOversampledSawtoothOscillator }

constructor TChebyshevOversampledSawtoothOscillator.Create;
begin
 FFilter := TChebyshev1LowpassFilter.Create;
 inherited;
 FRipple := TChebyshev1LowpassFilter(FFilter).Ripple;
end;

procedure TChebyshevOversampledSawtoothOscillator.SetRipple(
  const Value: Single);
begin
 if FRipple <> Value then
  begin
   FRipple := Value;
   RippleChanged;
  end;
end;

procedure TChebyshevOversampledSawtoothOscillator.RippleChanged;
begin
 TChebyshev1LowpassFilter(FFilter).Ripple := FRipple;
end;

{ TTriangleOscillator }

function TTriangleOscillator.ProcessSample32: Single;
begin
 Result := 2 * abs(inherited ProcessSample32) - 1;
end;


{ TRectangleOscillator }

function TRectangleOscillator.ProcessSample32: Single;
begin
 Result := Sign(inherited ProcessSample32);
end;

end.
