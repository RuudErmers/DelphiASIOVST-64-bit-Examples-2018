unit VOLGUI;

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

{$I DAV_Compiler.inc}

uses 
  {$IFDEF FPC} LCLIntf, {$IFDEF MSWINDOWS} Windows,  {$ENDIF}
  {$ELSE} Windows, Messages, {$ENDIF} SysUtils, Classes, Forms, StdCtrls,
  Controls, Dialogs, DAV_Types, DAV_VSTModule;

type
  TFmVOL = class(TForm)
    MOpcodeLog: TMemo;
    BtClear: TButton;
    BtUpdate: TButton;
    CBAutoUpdates: TCheckBox;
    BtSaveAs: TButton;
    SaveDialog: TSaveDialog;
    Sb1: TScrollBar;
    LbParameter: TLabel;
    LbParameterValue: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BtUpdateClick(Sender: TObject);
    procedure BtClearClick(Sender: TObject);
    procedure BtSaveAsClick(Sender: TObject);
    procedure Sb1Change(Sender: TObject);
  public
    procedure UpdateParameter;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  VOLDM;

procedure TFmVOL.BtClearClick(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   OpcodeLog.Clear;
   MOpcodeLog.Lines.Assign(OpcodeLog);
  end;
end;

procedure TFmVOL.BtUpdateClick(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   MOpcodeLog.Lines.Assign(OpcodeLog);
  end;
end;

procedure TFmVOL.FormShow(Sender: TObject);
begin
 UpdateParameter;
end;

procedure TFmVOL.Sb1Change(Sender: TObject);
begin
 with TVOLDataModule(Owner) do
  begin
   Parameter[1] := 0.01 * Sb1.Position;
  end;
end;

procedure TFmVOL.UpdateParameter;
begin
 with TVOLDataModule(Owner) do
  begin
   if Sb1.Position <> Round(100 * Parameter[1])
    then Sb1.Position := Round(100 * Parameter[1]);
   LbParameterValue.Caption := FloatToStrF(Parameter[1], ffGeneral, 3, 3);
  end;
end;

procedure TFmVOL.BtSaveAsClick(Sender: TObject);
var
  FormatSettings : TFormatSettings;
begin
 with SaveDialog do
  begin
   {$IFDEF FPC}
   FormatSettings := DefaultFormatSettings;
   {$ELSE}
   {$IFDEF Compiler16_UP}
   FormatSettings := TFormatSettings.Create(SysLocale.DefaultLCID);
   {$ELSE}
   GetLocaleFormatSettings(SysLocale.DefaultLCID, FormatSettings);
   {$ENDIF}
   {$ENDIF}
   FormatSettings.ShortDateFormat := 'yyyymmdd';
   FormatSettings.LongTimeFormat := 'yyyymmdd';
   FormatSettings.ShortTimeFormat := 'hhmmss';
   FormatSettings.LongTimeFormat := 'hhmmsss';
   FileName := 'OpcodeLog - ' + DateTimeToStr(Now, FormatSettings) + '.log';
   if Execute then
    begin
     MOpcodeLog.Lines.SaveToFile(FileName);
    end;
  end;
end;

end.
