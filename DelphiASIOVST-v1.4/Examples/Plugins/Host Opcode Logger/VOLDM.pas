unit VOLDM;

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
{-$DEFINE UseMessageDialogs}

uses 
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Forms, DAV_Types, DAV_VSTEffect, DAV_VSTModule;

type
  TVOLDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure ParamChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: NativeUInt);
  private
    FOpcodeLog   : TStringList;
    FLastOpcode  : TDispatcherOpcode;
    FLogFileName : TFileName;
    procedure SyncLogDisplay;
  protected
    function HostCallDispatchEffect(const opcode : TDispatcherOpcode;
      const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer;
      const opt: Single): TVstIntPtr; override;
    procedure HostCallSetParameter(const Index: Integer; const Value: Single); override;
    function HostCallGetParameter(const Index: Integer): Single; override;
  public
    property OpcodeLog: TStringList read FOpcodeLog;
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

uses
  {$IFDEF UseMessageDialogs}Dialogs, {$ENDIF}
  {$IFDEF HAS_UNIT_ANSISTRINGS} AnsiStrings, {$ENDIF} VOLGUI;

procedure TVOLDataModule.VSTModuleCreate(Sender: TObject);
var
  FormatSettings : TFormatSettings;
  Offset         : Integer;
begin
 FOpcodeLog := TStringList.Create;

 try
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
  FLogFileName := 'OpcodeLog - ' + DateTimeToStr(Now, FormatSettings) + '.log';
 except
  FLogFileName := 'OpcodeLog.log';
 end;

 FLogFileName := 'C:\' + FLogFileName;
 if FileExists(FLogFileName) then
  begin
   Offset := 1;
   while FileExists(Copy(FLogFileName, 0, Length(FLogFileName) - 4) + ' - ' +
     IntToStr(Offset) + '.log') and (Offset < 999)
    do Inc(Offset);
   FLogFileName := Copy(FLogFileName, 0, Length(FLogFileName) - 4) + ' - ' +
     IntToStr(Offset) + '.log';
  end;

 {$IFDEF UseMessageDialogs}
 ShowMessage('Start Logging (' + FLogFileName + ')');

 FOpcodeLog.SaveToFile(FLogFileName);
 ShowMessage('Logfile stored (' + FLogFileName + ')');
 {$ENDIF}
end;

procedure TVOLDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FOpcodeLog);
end;

procedure TVOLDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: NativeUInt);
begin
 GUI := TFmVOL.Create(Self);
 TFmVOL(GUI).MOpcodeLog.Lines.Assign(OpcodeLog);
end;

procedure TVOLDataModule.VSTModuleOpen(Sender: TObject);
begin
 {$IFDEF UseMessageDialogs}
 ShowMessage('Effect Open');
 {$ENDIF}

 if not Assigned(FOpcodeLog)
  then Exit;

 FOpcodeLog.Add('HostProduct: ' + string(HostProduct));
 FOpcodeLog.Add('HostVendor: ' + string(HostVendor));

 SyncLogDisplay;
end;

function TVOLDataModule.HostCallDispatchEffect(const Opcode: TDispatcherOpcode;
  const Index: Integer; const Value: TVstIntPtr; const ptr: Pointer;
  const opt: Single): TVstIntPtr;
var
  ChunkName : TChunkName;
begin
 if Assigned(FOpcodeLog) then
  begin
   case Opcode of
    effOpen,
    effEditClose,
    effStartProcess,
    effGetNumProgramCategories,
    effGetPlugCategory,
    effIdentify,
    effStopProcess   :
     begin
      FOpcodeLog.Add(Opcode2String(Opcode));
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
    effGetSpeakerArrangement,
    effSetSpeakerArrangement :
     begin
      FOpcodeLog.Add(Opcode2String(Opcode) + 'Input: ' + IntToHex(Integer(Value), 8) +
        'Output: ' + IntToHex(Integer(Ptr), 8));
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
    effSetSampleRate :
     begin
      FOpcodeLog.Add(Opcode2String(Opcode) + ' SampleRate: ' + FloatToStr(Single(Opt)));
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
    effSetBlockSize  :
     begin
      FOpcodeLog.Add(Opcode2String(Opcode) + ' BlockSize: ' + IntToStr(Value));
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
    effVendorSpecific :
     begin
      ChunkName := TChunkName(Index);
      if (ChunkName[0] in ['0'..'z']) and
         (ChunkName[1] in ['0'..'z']) and
         (ChunkName[2] in ['0'..'z']) and
         (ChunkName[3] in ['0'..'z'])
       then FOpcodeLog.Add(Opcode2String(Opcode) +
              ' Chunkname: ' + string(ChunkName) +
              ' Value: ' + IntToStr(Value) +
              ' Pointer: ' + IntToStr(Integer(ptr)) +
              ' Single: ' + FloatToStr(Single(Opt)))
       else FOpcodeLog.Add(Opcode2String(Opcode) +
              ' Index: ' + IntToStr(Index) +
              ' Value: ' + IntToStr(Value) +
              ' Pointer: ' + IntToStr(Integer(ptr)) +
              ' Single: ' + FloatToStr(Single(Opt)));
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
    effIdle:
      if FLastOpcode <> effIdle
       then FOpcodeLog.Add(Opcode2String(Opcode));
    effEditIdle:
      if FLastOpcode <> effEditIdle
       then FOpcodeLog.Add(Opcode2String(Opcode));
    effCanDo:
     begin
      FOpcodeLog.Add(Opcode2String(Opcode) + ' ''' + string(StrPas(PAnsiChar(Ptr))) + '''');
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
    effClose:
     begin
      FOpcodeLog.Add(Opcode2String(Opcode));
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
    else
     begin
      FOpcodeLog.Add(Opcode2String(Opcode) +
        ' Index: ' + IntToStr(Index) +
        ' Value: ' + IntToStr(Value) +
        ' Pointer: ' + IntToStr(Integer(ptr)) +
        ' Single: ' + FloatToStr(Single(Opt)));
      FOpcodeLog.SaveToFile(FLogFileName);
     end;
   end;

   FLastOpcode := Opcode;
   SyncLogDisplay;
  end;

 Result := inherited HostCallDispatchEffect(Opcode, Index, Value, ptr, opt);
end;

function TVOLDataModule.HostCallGetParameter(const Index: Integer): Single;
begin
 Result := inherited HostCallGetParameter(Index);

 if not Assigned(FOpcodeLog)
  then Exit;

 FOpcodeLog.Add('GetParameter' +
                ' Index: ' + IntToStr(Index) +
                ' Value: ' + FloatToStr(result));
 SyncLogDisplay;
end;

procedure TVOLDataModule.HostCallSetParameter(const Index: Integer;
  const Value: Single);
begin
 inherited;

 if not Assigned(FOpcodeLog)
  then Exit;

 FOpcodeLog.Add('GetParameter' +
                ' Index: ' + IntToStr(Index) +
                ' Value: ' + FloatToStr(Value));
 SyncLogDisplay;
end;

procedure TVOLDataModule.SyncLogDisplay;
begin
 if EditorForm is TFmVOL then
  with TFmVOL(EditorForm) do
   begin
    if CBAutoUpdates.Checked
     then MOpcodeLog.Lines.Assign(OpcodeLog);
   end;
end;

procedure TVOLDataModule.ParamChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TFmVOL
  then TFmVOL(EditorForm).UpdateParameter;
end;

end.
