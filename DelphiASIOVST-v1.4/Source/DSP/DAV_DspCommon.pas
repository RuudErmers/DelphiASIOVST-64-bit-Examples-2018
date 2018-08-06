unit DAV_DspCommon;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Common;

type
  TDspPersistent = class(TInterfacedPersistent);

  TDspSampleRatePersistent = class(TDspPersistent)
  private
    FSampleRate : Double;
    procedure SetSampleRate(const Value: Double);
  protected
    procedure SampleRateChanged; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    property SampleRate: Double read FSampleRate write SetSampleRate;
  end;

//  TDspComponent = class(TComponent);

  // some interfaces

  {$IFDEF DELPHI7_UP}
  IDspSink32 = interface(IInterface)
    procedure ProcessSample32(Input: Single);
  end;

  IDspSink64 = interface(IInterface)
    procedure ProcessSample64(Input: Double);
  end;

  IDspProcessor32 = interface(IInterface)
    function ProcessSample32(Input: Single): Single;
  end;

  IDspProcessor64 = interface(IInterface)
    function ProcessSample64(Input: Double): Double;
  end;

  IDspGenerator32 = interface(IInterface)
    function ProcessSample32: Single;
  end;

  IDspGenerator64 = interface(IInterface)
    function ProcessSample64: Double;
  end;
  {$ENDIF}

implementation

uses
  SysUtils;

resourcestring
  RCStrInvalidSamplerate = 'Invalid Samplerate!';

{ TDspSampleRateObject }

procedure TDspSampleRatePersistent.AssignTo(Dest: TPersistent);
begin
 if Dest is TDspSampleRatePersistent
  then TDspSampleRatePersistent(Dest).FSampleRate := FSampleRate
  else inherited;
end;

constructor TDspSampleRatePersistent.Create;
begin
 inherited;
 FSampleRate := 44100;
end;

procedure TDspSampleRatePersistent.SetSampleRate(const Value: Double);
begin
 if Value = 0
  then raise Exception.Create(RCStrInvalidSamplerate);

 if FSampleRate <> abs(Value) then
  begin
   FSampleRate := abs(Value);
   SampleRateChanged;
  end;
end;

procedure TDspSampleRatePersistent.SampleRateChanged;
begin
 // nothing here yet
end;

end.
