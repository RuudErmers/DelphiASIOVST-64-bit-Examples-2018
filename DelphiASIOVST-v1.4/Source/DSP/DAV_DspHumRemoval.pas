unit DAV_DspHumRemoval;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2005-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Classes, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspFilterButterworth, DAV_DspFilterChebyshevType1,
  DAV_DspFilterChebyshevType2;

type
  TDspHumRemoval = class(TDspSampleRatePersistent, IDspProcessor32)
  private
    FHighpassFilter       : TCustomOrderFilter;
    FHighpassFilterType   : TOrderFilterClass;
    FNotchFilterCount     : Integer;
    FNotchFilters         : array of TBasicPeakFilter;
    FFundamentalFrequency : Single;
    FBandwidth            : Single;
    FDcFilterActive       : Boolean;
    FAttenuation          : Single;
    procedure SetHighpassFilterType(const Value: TOrderFilterClass);
    procedure SetNotchFilterCount(const Value: Integer);
    procedure SetFundamentalFrequency(const Value: Single);
    procedure SetBandwidth(const Value: Single);
    procedure SetDcFilterActive(const Value: Boolean);
    procedure DcFilterActiveChanged;
    procedure SetAttenuation(const Value: Single);
    procedure AttenuationChanged;
  protected
    procedure BandwidthChanged; virtual;
    procedure FundamentalFrequencyChanged; virtual;
    procedure HighpassFilterTypeChanged; virtual;
    procedure NotchFilterCountChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Clear;

    function ProcessSample32(Input: Single): Single;
    function ProcessSample64(Input: Double): Double;
    procedure ProcessBlock32(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    procedure ProcessBlock64(const Data: PDAVSingleFixedArray; SampleCount: Integer);
    function Magnitude_dB(const Frequency: Double): Double;
    function MagnitudeSquared(const Frequency: Double):Double;

    property HighpassFilterActive: Boolean read FDcFilterActive write SetDcFilterActive;
    property HighpassFilterType: TOrderFilterClass read FHighpassFilterType write SetHighpassFilterType;
    property HighpassFilter: TCustomOrderFilter read FHighpassFilter;
    property NotchFilterCount: Integer read FNotchFilterCount write SetNotchFilterCount;
    property FundamentalFrequency: Single read FFundamentalFrequency write SetFundamentalFrequency;
    property Bandwidth: Single read FBandwidth write SetBandwidth;
    property Attenuation: Single read FAttenuation write SetAttenuation;
  end;

implementation

uses
  Math, SysUtils;

{ TDspHumRemoval }

constructor TDspHumRemoval.Create;
begin
 inherited;
 FHighpassFilterType := TButterworthLowCutFilter;
 FHighpassFilter := FHighpassFilterType.Create(2);
 FFundamentalFrequency := 50;
 FBandwidth := 0.1;
 FAttenuation := 48;
 FNotchFilterCount := 8;
 NotchFilterCountChanged;
end;

destructor TDspHumRemoval.Destroy;
var
  FilterIndex : Integer;
begin
 FreeAndNil(FHighpassFilter);
 for FilterIndex := 0 to Length(FNotchFilters) - 1
  do FreeAndNil(FNotchFilters[FilterIndex]);
 inherited;
end;

procedure TDspHumRemoval.Clear;
var
  FilterIndex : Integer;
begin
 FHighpassFilter.ResetStates;
 for FilterIndex := 0 to Length(FNotchFilters) - 1
  do FNotchFilters[FilterIndex].ResetStates;
end;

procedure TDspHumRemoval.SetAttenuation(const Value: Single);
begin
 if FAttenuation <> Value then
  begin
   FAttenuation := Value;
   AttenuationChanged;
  end;
end;

procedure TDspHumRemoval.SetBandwidth(const Value: Single);
begin
 if FBandwidth <> Value then
  begin
   FBandwidth := Value;
   BandwidthChanged;
  end;
end;

procedure TDspHumRemoval.SetDcFilterActive(const Value: Boolean);
begin
 if FDcFilterActive <> Value then
  begin
   FDcFilterActive := Value;
   DcFilterActiveChanged;
  end;
end;

procedure TDspHumRemoval.SetFundamentalFrequency(const Value: Single);
begin
 if FFundamentalFrequency <> Value then
  begin
   FFundamentalFrequency := Value;
   FundamentalFrequencyChanged;
  end;
end;

procedure TDspHumRemoval.SetHighpassFilterType(const Value: TOrderFilterClass);
begin
 if FHighpassFilterType <> Value then
  begin
   FHighpassFilterType := Value;
   HighpassFilterTypeChanged;
  end;
end;

procedure TDspHumRemoval.SetNotchFilterCount(const Value: Integer);
begin
 if FNotchFilterCount <> Value then
  begin
   FNotchFilterCount := Value;
   NotchFilterCountChanged;
  end;
end;

procedure TDspHumRemoval.BandwidthChanged;
var
  FilterNo: Integer;
begin
 for FilterNo := 0 to Length(FNotchFilters) - 1
  do FNotchFilters[FilterNo].Bandwidth := Sqr(FBandwidth);
 Changed;
end;

procedure TDspHumRemoval.AttenuationChanged;
var
  FilterNo: Integer;
begin
 for FilterNo := 0 to Length(FNotchFilters) - 1
  do FNotchFilters[FilterNo].Gain := -FAttenuation + FAttenuation * FilterNo / Length(FNotchFilters);
 Changed;
end;

procedure TDspHumRemoval.DcFilterActiveChanged;
begin
 Changed;
end;

procedure TDspHumRemoval.FundamentalFrequencyChanged;
var
  FilterNo: Integer;
begin
 for FilterNo := 1 to Length(FNotchFilters)
  do FNotchFilters[FilterNo - 1].Frequency := FFundamentalFrequency * FilterNo;
 Changed;
end;

procedure TDspHumRemoval.NotchFilterCountChanged;
var
  FilterNo: Integer;
begin
 // free unused filters
 for FilterNo := FNotchFilterCount to Length(FNotchFilters) - 1
  do FreeAndNil(FNotchFilters[FilterNo]);

 // change notch filter array length
 SetLength(FNotchFilters, FNotchFilterCount);

 // check and eventually add new filters
 for FilterNo := 0 to Length(FNotchFilters) - 1 do
  begin
   if not assigned(FNotchFilters[FilterNo]) then
    begin
     FNotchFilters[FilterNo] := TBasicPeakFilter.Create;
     with FNotchFilters[FilterNo] do
      begin
       Frequency := FFundamentalFrequency * (FilterNo + 1);
       Bandwidth := FBandwidth;
      end;
    end;
   FNotchFilters[FilterNo].Gain := -FAttenuation + FAttenuation * FilterNo / Length(FNotchFilters);
  end;
 Changed;
end;

procedure TDspHumRemoval.HighpassFilterTypeChanged;
var
  OldFilter : TCustomOrderFilter;
begin
 OldFilter := FHighpassFilter;
 FHighpassFilter := FHighpassFilterType.Create(OldFilter.Order);

 // setup filter for the chebyshev case
 if FHighpassFilter is TCustomChebyshev1Filter then
  with TCustomChebyshev1Filter(FHighpassFilter) do
   begin
    Ripple := 0.2;
   end else
 if FHighpassFilter is TCustomChebyshev2Filter then
  with TCustomChebyshev2Filter(FHighpassFilter) do
   begin
    FixFrequency := True;
    Stopband := -FAttenuation;
   end;

 FHighpassFilter.Frequency := OldFilter.Frequency;
 FreeAndNil(OldFilter);

 Changed;
end;

function TDspHumRemoval.MagnitudeSquared(const Frequency: Double): Double;
var
  FilterNo: Integer;
begin
 if FDcFilterActive
  then Result := FHighpassFilter.MagnitudeSquared(Frequency)
  else Result := 1;

 for FilterNo := 0 to Length(FNotchFilters) - 1
  do Result := Result * FNotchFilters[FilterNo].MagnitudeSquared(Frequency);
end;

function TDspHumRemoval.Magnitude_dB(const Frequency: Double): Double;
begin
 Result := 10 * Log10(MagnitudeSquared(Frequency));
end;

procedure TDspHumRemoval.ProcessBlock32(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample32(Data[Sample]);
end;

procedure TDspHumRemoval.ProcessBlock64(const Data: PDAVSingleFixedArray;
  SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := ProcessSample64(Data[Sample]);
end;

function TDspHumRemoval.ProcessSample32(Input: Single): Single;
var
  FilterNo: Integer;
begin
 if FDcFilterActive
  then Result := FHighpassFilter.ProcessSample64(Input)
  else Result := Input;
 for FilterNo := 0 to Length(FNotchFilters) - 1
  do Result := FNotchFilters[FilterNo].ProcessSample64(Result);
end;

function TDspHumRemoval.ProcessSample64(Input: Double): Double;
var
  FilterNo: Integer;
begin
 if FDcFilterActive
  then Result := FHighpassFilter.ProcessSample64(Input)
  else Result := Input;
 for FilterNo := 0 to Length(FNotchFilters) - 1
  do Result := FNotchFilters[FilterNo].ProcessSample64(Result);
end;

procedure TDspHumRemoval.SampleRateChanged;
begin
 FHighpassFilter.SampleRate := SampleRate;
 inherited;
end;

end.
