unit DAV_DspDownsampleScheduler;

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
  Classes, DAV_Common, DAV_Classes;

type
  TDownsampleProcess32 = function(Sender: TObject; Stage: Integer; Input: Single): Single of object;

  TCustomDownsampleScheduler = class(TDspPersistent, IDspSink32)
  private
    FMaxDSStages          : Integer;
    FDownSampleCount      : Integer;
    FDownSampleMax        : Integer;
    FOnProcessDownsampled : TDownsampleProcess32;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure ProcessSample32(Input: Single); virtual;

    property MaximumDownsampleStages: Integer read FMaxDSStages write FMaxDSStages;
    property OnProcessDownsampled: TDownsampleProcess32 read FOnProcessDownsampled write FOnProcessDownsampled;
  end;

  TDownsampleScheduler = class(TCustomDownsampleScheduler)
  published
    property MaximumDownsampleStages;
    property OnProcessDownsampled;
  end;


implementation

{ TCustomDownsampleScheduler }

procedure TCustomDownsampleScheduler.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomDownsampleScheduler then
  with TCustomDownsampleScheduler(Dest) do
   begin
    FMaxDSStages          := Self.FMaxDSStages;
    FDownSampleCount      := Self.FDownSampleCount;
    FDownSampleMax        := Self.FDownSampleMax;
    FOnProcessDownsampled := Self.FOnProcessDownsampled;
   end
 else inherited;
end;

constructor TCustomDownsampleScheduler.Create;
begin
 FMaxDSStages := 8;
 FDownSampleMax := 1 shl FMaxDSStages;
 FDownSampleCount := 0;
end;

procedure TCustomDownsampleScheduler.ProcessSample32(Input: Single);
var
  Stage: Integer;
begin
 for Stage := 0 to FMaxDSStages - 1 do
  begin
   if (FDownSampleCount mod (1 shl Stage)) <> 0
    then Break;
   Input := FOnProcessDownsampled(Self, Stage, Input);
  end;

 Inc(FDownSampleCount);
 if FDownSampleCount >= FDownSampleMax
  then FDownSampleCount := 0;
end;

end.
