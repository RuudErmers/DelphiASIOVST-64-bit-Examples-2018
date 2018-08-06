unit ASIOVSTGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule, DAV_ASIOHost;

type
  TFmASIOVST = class(TForm)
    CbASIO: TComboBox;
    CBShortCircuit: TCheckBox;
    LbAsioOutput: TLabel;
    Memo: TMemo;
    procedure FormShow(Sender: TObject);
    procedure CbASIOChange(Sender: TObject);
    procedure CBShortCircuitClick(Sender: TObject);
    procedure LbAsioOutputClick(Sender: TObject);
  public
    procedure DisplayASIOInformation;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  ASIOVSTModule;

{ TFmASIOVST }

procedure TFmASIOVST.FormShow(Sender: TObject);
begin
  CbASIO.Items := TASIOVSTModule(Owner).ASIOHost.DriverList;
  DisplayASIOInformation;
end;

procedure TFmASIOVST.CBShortCircuitClick(Sender: TObject);
begin
 with TASIOVSTModule(Owner) do
  begin
   if not CBShortCircuit.Checked
    then AsioHost.OnBufferSwitch32 := AHBufferSwitch32
    else AsioHost.OnBufferSwitch32 := AHShortCircuit;
  end;
end;

procedure TFmASIOVST.CbASIOChange(Sender: TObject);
begin
 with TASIOVSTModule(Owner) do
  begin
   Parameter[0] := CbASIO.ItemIndex;
   DisplayASIOInformation;
  end;
end;

procedure TFmASIOVST.DisplayASIOInformation;
begin
 with TASIOVSTModule(Owner), AsioHost, Memo do
  begin
   Clear;
   Lines.Add('Driver: ' + DriverName);
   Lines.Add('Buffersize: ' + IntToStr(BufferSize));
   Lines.Add('Latency: ' + IntToStr(InputLatency + OutputLatency + Integer(BufferSize)));
   if OutputChannelCount > 0 then
    begin
     Lines.Add('Channel 1: ' + OutputChannelInfos[0].name);
     Lines.Add('Channel 2: ' + OutputChannelInfos[1].name);
     Lines.Add('Format In 1: ' + ChannelTypeToString(OutputChannelInfos[0].SampleType));
     Lines.Add('Format In 2: ' + ChannelTypeToString(OutputChannelInfos[1].SampleType));
     Lines.Add('Format Out 1: ' + ChannelTypeToString(OutputChannelInfos[0].SampleType));
     Lines.Add('Format Out 2: ' + ChannelTypeToString(OutputChannelInfos[1].SampleType));
    end;
  end;
end;

procedure TFmASIOVST.LbAsioOutputClick(Sender: TObject);
begin
 TASIOVSTModule(Owner).AsioHost.ControlPanel;
end;

end.
