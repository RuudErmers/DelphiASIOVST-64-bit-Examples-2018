unit DAV_DspFilterLinkwitzRiley;

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
  Classes, DAV_Common, DAV_Classes, DAV_DspFilter, DAV_DspFilterButterworth;

type
  TLinkwitzRiley = class(TDspSampleRatePersistent, IDspSplitter32,
    IDspSplitter64)
  private
    FLowpass    : TButterworthLowpassFilter;
    FHighpass   : TButterworthHighpassFilter;
    FSplit      : TButterworthSplitBandFilter;
    FFrequency  : Single;
    FOrder      : Integer;
    FSign       : Single;
    procedure SetFrequency(const Value: Single);
    procedure SetOrder(const Value: Integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FrequencyChanged; virtual;
    procedure OrderChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create(const Order: Integer = 4); reintroduce; virtual;
    destructor Destroy; override;

    procedure ProcessSample32(Input: Single; out Low, High: Single); virtual;
    procedure ProcessSample64(Input: Double; out Low, High: Double); virtual;
    procedure ResetStates; virtual;
  published
    property Frequency: Single read FFrequency write SetFrequency;
    property Order: Integer read FOrder write SetOrder;
    property SampleRate;
  end;

implementation

uses
  SysUtils;

{ TLinkwitzRiley }

constructor TLinkwitzRiley.Create(const Order: Integer = 4);
begin
 inherited Create;
 FLowpass    := TButterworthLowpassFilter.Create;
 FHighpass   := TButterworthHighpassFilter.Create;
 FSplit      := TButterworthSplitBandFilter.Create;
 FOrder      := Order;
 FSign       := 1;
 FFrequency  := 1000;
 FrequencyChanged;
 OrderChanged;
end;

destructor TLinkwitzRiley.Destroy;
begin
 FreeAndNil(FLowpass);
 FreeAndNil(FHighpass);
 FreeAndNil(FSplit);
 inherited;
end;

procedure TLinkwitzRiley.AssignTo(Dest: TPersistent);
begin
 if Dest is TLinkwitzRiley then
  with TLinkwitzRiley(Dest) do
   begin
    inherited;
    FLowpass.Assign(Self.FLowpass);
    FHighpass.Assign(Self.FHighpass);
    FSplit.Assign(Self.FSplit);

    FFrequency  := Self.FFrequency;
    FOrder      := Self.FOrder;
    FSign       := Self.FSign;
   end
 else inherited;
end;

procedure TLinkwitzRiley.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TLinkwitzRiley.SetOrder(const Value: Integer);
begin
 if FOrder <> Value then
  begin
   FOrder := Value;
   OrderChanged;
  end;
end;

procedure TLinkwitzRiley.FrequencyChanged;
begin
 FLowpass.Frequency  := FFrequency;
 FHighpass.Frequency := FFrequency;
 FSplit.Frequency    := FFrequency;
end;

procedure TLinkwitzRiley.OrderChanged;
begin
 FLowpass.Order  := FOrder;
 FHighpass.Order := FOrder;
 FSplit.Order    := FOrder;
 FSign := 1 - 2 * (FOrder mod 2);
end;

procedure TLinkwitzRiley.ProcessSample32(Input: Single; out Low,
  High: Single);
begin
 FSplit.ProcessSample32(Input, Low, High);
 Low  := FLowpass.ProcessSample64(Low - CDenorm32);
 High := FHighpass.ProcessSample64(FSign * High - CDenorm32);
end;

procedure TLinkwitzRiley.ProcessSample64(Input: Double; out Low,
  High: Double);
begin
 FSplit.ProcessSample64(Input, Low, High);
 Low  := FLowpass.ProcessSample64(Low);
 High := FHighpass.ProcessSample64(FSign * High);
end;

procedure TLinkwitzRiley.ResetStates;
begin
 FLowpass.ResetStates;
 FHighpass.ResetStates;
end;

procedure TLinkwitzRiley.SampleRateChanged;
begin
 FLowpass.SampleRate  := SampleRate;
 FHighpass.SampleRate := SampleRate;
 FSplit.SampleRate    := SampleRate;
end;

end.
