unit DAV_AudioPluginModule;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2011-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Forms, Sysutils, DAV_Types;

{$IFDEF DebugLog}
const
  CDebugLogFile : string = 'C:\Debug.log';
{$ENDIF}

type
  TAudioPluginModuleClass = class of TAudioPluginModule;

  { TAudioPluginModule }

  TAudioPluginModule = class({$IFDEF UseDelphi}TDataModule{$ELSE}TComponent{$ENDIF})
  protected
    {$IFNDEF UseDelphi}
    FOnCreate           : TNotifyEvent;
    FOnDestroy          : TNotifyEvent;
    {$ENDIF}

    {$IFDEF DebugLog}
    FLog                : TStringList;
    FTmStmp             : TDateTime;
    procedure AddLogMessage(const Text: string);
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function GetStaticDescription: string; virtual;

    {$IFNDEF UseDelphi}
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    {$ENDIF}

    {$IFDEF DebugLog} property DebugLog: TStringList read FLog; {$ENDIF}
  end;

  EPluginError = class(Exception);

implementation

uses
  DAV_Common;


{ TAudioPluginModule }

constructor TAudioPluginModule.Create(AOwner: TComponent);
begin
 inherited;
 // yet todo
end;

destructor TAudioPluginModule.Destroy;
begin
 // yet todo
 inherited;
end;

class function TAudioPluginModule.GetStaticDescription: string;
begin
 Result := 'Audio Plugin';
end;

end.
