unit DAV_AsioCore32;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LclType, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Controls, DAV_AsioInterface, DAV_Types, DAV_Asio,
  DAV_AsioConvert, DAV_AsioHostCore, DAV_BlockConvert32;

type
  {$IFDEF SUPPORTS_REGION} {$region 'TAsioChannel'} {$ENDIF}
  TAsioChannelInput32 = class(TAsioChannelInput)
  protected
    FConverter : TBlockConvertToFloat32;
  public
    procedure UpdateChannelInfo; override;
  end;

  TAsioChannelOutput32 = class(TAsioChannelOutput)
  protected
    FConverter : TBlockConvertFromFloat32;
  public
    procedure UpdateChannelInfo; override;
  end;
  {$IFDEF SUPPORTS_REGION} {$endregion 'TAsioChannel'} {$ENDIF}

  TAsioHostCore32 = class(TAsioHostCore)
  private
  protected
    class function GetInputChannelClass: TAsioChannelInputClass; override;
    class function GetOutputChannelClass: TAsioChannelOutputClass; override;
    procedure BufferSizeChange; override;
    procedure BufferSwitch(Index: Integer); override;
    procedure BufferSwitchTimeInfo(Index: Integer; const Params: TAsioTime); override;
    procedure ClearBuffers; override;
  public
    constructor Create(ID: TGUID); override;
    destructor Destroy; override;
  end;

implementation

function GetInputConverter32(ConverterType: TAsioSampleType): TBlockConvertToFloat32;
begin
 case ConverterType of
  CAsioSTInt16MSB   : Result := BlockConvertInt16MSBToFloat32;
  CAsioSTInt24MSB   : Result := BlockConvertInt24MSBToFloat32;
  CAsioSTInt32MSB   : Result := BlockConvertInt32MSBToFloat32;
//  CAsioSTFloat32MSB : Result := FromSingleMSB;
//  CAsioSTFloat64MSB : Result := FromDoubleMSB;
  CAsioSTInt32MSB16 : Result := BlockConvertInt32MSB16ToFloat32;
  CAsioSTInt32MSB18 : Result := BlockConvertInt32MSB18ToFloat32;
  CAsioSTInt32MSB20 : Result := BlockConvertInt32MSB20ToFloat32;
  CAsioSTInt32MSB24 : Result := BlockConvertInt32MSB24ToFloat32;
  CAsioSTInt16LSB   : Result := BlockConvertInt16LSBToFloat32;
  CAsioSTInt24LSB   : Result := BlockConvertInt24LSBToFloat32;
  CAsioSTInt32LSB   : Result := BlockConvertInt32LSBToFloat32;
//  CAsioSTFloat32LSB : Result := FromSingleLSB;
//  CAsioSTFloat64LSB : Result := FromDoubleLSB;
  CAsioSTInt32LSB16 : Result := BlockConvertInt32LSB16ToFloat32;
  CAsioSTInt32LSB18 : Result := BlockConvertInt32LSB18ToFloat32;
  CAsioSTInt32LSB20 : Result := BlockConvertInt32LSB20ToFloat32;
  CAsioSTInt32LSB24 : Result := BlockConvertInt32LSB24ToFloat32;
//  else raise EAsioHost.Create(RStrConverterTypeUnknown);
 end;
end;

function GetOutputConverter32(ConverterType: TAsioSampleType): TBlockConvertFromFloat32;
begin
 case ConverterType of
  CAsioSTInt16MSB   : Result := BlockConvertInt16MSBFromFloat32;
  CAsioSTInt24MSB   : Result := BlockConvertInt24MSBFromFloat32;
  CAsioSTInt32MSB   : Result := BlockConvertInt32MSBFromFloat32;
//  CAsioSTFloat32MSB : Result := FromSingleMSB;
//  CAsioSTFloat64MSB : Result := FromDoubleMSB;
  CAsioSTInt32MSB16 : Result := BlockConvertInt32MSB16FromFloat32;
  CAsioSTInt32MSB18 : Result := BlockConvertInt32MSB18FromFloat32;
  CAsioSTInt32MSB20 : Result := BlockConvertInt32MSB20FromFloat32;
  CAsioSTInt32MSB24 : Result := BlockConvertInt32MSB24FromFloat32;
  CAsioSTInt16LSB   : Result := BlockConvertInt16LSBFromFloat32;
  CAsioSTInt24LSB   : Result := BlockConvertInt24LSBFromFloat32;
  CAsioSTInt32LSB   : Result := BlockConvertInt32LSBFromFloat32;
//  CAsioSTFloat32LSB : Result := FromSingleLSB;
//  CAsioSTFloat64LSB : Result := FromDoubleLSB;
  CAsioSTInt32LSB16 : Result := BlockConvertInt32LSB16FromFloat32;
  CAsioSTInt32LSB18 : Result := BlockConvertInt32LSB18FromFloat32;
  CAsioSTInt32LSB20 : Result := BlockConvertInt32LSB20FromFloat32;
  CAsioSTInt32LSB24 : Result := BlockConvertInt32LSB24FromFloat32;
//  else raise EAsioHost.Create(RStrConverterTypeUnknown);
 end;
end;


{ TAsioChannelInput32 }

procedure TAsioChannelInput32.UpdateChannelInfo;
begin
 inherited;

 FConverter := GetInputConverter32(SampleType);
end;

{ TAsioChannelOutput32 }

procedure TAsioChannelOutput32.UpdateChannelInfo;
begin
 inherited;

 FConverter := GetOutputConverter32(SampleType);
end;


{ TAsioHostCore32 }

constructor TAsioHostCore32.Create(ID: TGUID);
begin

end;

destructor TAsioHostCore32.Destroy;
begin

  inherited;
end;

procedure TAsioHostCore32.BufferSizeChange;
begin

end;

procedure TAsioHostCore32.BufferSwitch(Index: Integer);
begin

end;

procedure TAsioHostCore32.BufferSwitchTimeInfo(Index: Integer;
  const Params: TAsioTime);
begin

end;

procedure TAsioHostCore32.ClearBuffers;
begin

end;

class function TAsioHostCore32.GetInputChannelClass: TAsioChannelInputClass;
begin
 Result := TAsioChannelInput32;
end;

class function TAsioHostCore32.GetOutputChannelClass: TAsioChannelOutputClass;
begin
 Result := TAsioChannelOutput32;
end;

end.
