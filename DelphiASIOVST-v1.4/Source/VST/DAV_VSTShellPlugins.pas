unit DAV_VSTShellPlugins;

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
//  The initial developer of this code is Christian-W. Budde                  //                                      //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_VSTEffect, DAV_VSTBasicModule;

type
  TUIDInstantiateEvent = procedure(Sender: TObject; UID: TChunkName) of object;

  TCustomVstShellPlugin = class(TCollectionItem)
  private
    FDisplayName      : string;
    FNumInputs        : Integer;
    FNumOutputs       : Integer;
    FNumParams        : Integer;
    FNumPrograms      : Integer;
    FPlugCategory     : TVstPluginCategory;
    FUniqueID         : TChunkName;
    FVSTModule        : TBasicVSTModule;
    FOnInstanciate    : TUIDInstantiateEvent;
    procedure SetUniqueID(FID: AnsiString);
    function GetUniqueID: AnsiString;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    destructor Destroy; override;
  published
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName write SetDisplayName{$ENDIF};
    property NumInputs: Integer read FNumInputs write FNumInputs default -1;
    property NumOutputs: Integer read FNumOutputs write FNumOutputs default -1;
    property NumParams: Integer read FNumParams write FNumParams default -1;
    property NumPrograms: Integer read FNumPrograms write FNumPrograms default -1;
    property PlugCategory: TVstPluginCategory read FPlugCategory write FPlugCategory;
    property UniqueID: AnsiString read GetUniqueID write SetUniqueID;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
    property OnInstanciate: TUIDInstantiateEvent read FOnInstanciate write FOnInstanciate;
  end;

  TCustomVstShellPlugins = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
    function GetItem(Index: Integer): TCustomVstShellPlugin;
    procedure SetItem(Index: Integer; const Value: TCustomVstShellPlugin);
  protected
    property Items[Index: Integer]: TCustomVstShellPlugin read GetItem write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCustomVstShellPlugin;
    function Insert(Index: Integer): TCustomVstShellPlugin;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

implementation

{$IFDEF FPC}
constructor TCustomVstShellPlugin.Create(ACollection: TCollection);
{$ELSE}
constructor TCustomVstShellPlugin.Create(Collection: TCollection);
{$ENDIF}
begin
  inherited;
  FDisplayName  := 'Init'; // inherited GetDisplayName;
  FNumInputs    := -1;
  FNumOutputs   := -1;
  FNumPrograms  := -1;
  FNumParams    := -1;
  FPlugCategory := vpcUnknown;
  FVSTModule    := (Collection As TCustomVstShellPlugins).VSTModule;
end;

destructor TCustomVstShellPlugin.Destroy;
begin
  inherited;
end;

procedure TCustomVstShellPlugin.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomVstShellPlugin then
    with TCustomVstShellPlugin(Dest)
      do DisplayName := Self.DisplayName
  else inherited;
end;

function TCustomVstShellPlugin.GetUniqueID: AnsiString;
begin
 Result := AnsiString(FUniqueID);
end;

procedure TCustomVstShellPlugin.SetUniqueID(FID: AnsiString);
begin
 if Length(FID) < 4
  then Move(FID[1], FUniqueID[0], Length(FID))
  else Move(FID[1], FUniqueID[0], 4);
end;

procedure TCustomVstShellPlugin.SetDisplayName(const AValue: string);
begin
  FDisplayName := Copy(AValue, 0, 50);
end;

function TCustomVstShellPlugin.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

{ TCustomVstShellPlugins }

constructor TCustomVstShellPlugins.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCustomVstShellPlugin);
  assert(AOwner is TBasicVSTModule);
  FVSTModule := TBasicVSTModule(AOwner);
end;

destructor TCustomVstShellPlugins.Destroy;
begin
  while Count > 0 do Delete(0);
  inherited;
end;

function TCustomVstShellPlugins.Add: TCustomVstShellPlugin;
begin
  Result := TCustomVstShellPlugin(inherited Add);
end;

function TCustomVstShellPlugins.Insert(Index: Integer): TCustomVstShellPlugin;
begin
  Result := TCustomVstShellPlugin(inherited Insert(Index));
end;

procedure TCustomVstShellPlugins.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TCustomVstShellPlugins.GetItem(Index: Integer): TCustomVstShellPlugin;
begin
  Result := TCustomVstShellPlugin(inherited GetItem(Index));
end;

procedure TCustomVstShellPlugins.SetItem(Index: Integer; const Value: TCustomVstShellPlugin);
begin
  inherited SetItem(Index, Value);
end;

end.
