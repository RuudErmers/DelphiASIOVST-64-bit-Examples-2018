unit OversamplePlugin;

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

{$I DAV_Compiler.inc}

uses
  Windows, Classes, SysUtils, Graphics, ExtCtrls, Menus, ComServ, ComObj,
  ActiveX, ShlObj, ShellAPI, Registry;

type
  TContextMenuFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  TContextMenu = class(TComObject, IShellExtInit, IContextMenu)
  private
    function BuildSubMenu(Menu: HMENU; IndexMenu: Integer; var IDCmdFirst: Integer): HMENU;
    procedure SavePlugin;
  protected
    szFile: array [0..2 * MAX_PATH] of Char; // just to be sure

    // Required to disambiguate TComObject.Initialize otherwise a compiler warning will result.
    function IShellExtInit.Initialize = IShellExtInit_Initialize;
  public
    { IShellExtInit members }
    function IShellExtInit_Initialize(pidlFolder: PItemIDList; lpdobj: IDataObject; hKeyProgID: HKEY): HResult; stdcall;

    { IContextMenu }
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd: UINT_PTR; uFlags: UINT; pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

const
  Class_ContextMenu: TGUID = '{E6EE9FD1-5C55-4894-8017-3D97C9BB9110}';

implementation

uses
  DAV_DLLResources, Dialogs;

{ TContextMenuFactory }

procedure TContextMenuFactory.UpdateRegistry(Register: Boolean);
begin
 inherited UpdateRegistry(Register);

 // Register our global context menu handler
 if Register then
  begin
   CreateRegKey('*\ShellEx\ContextMenuHandlers\OversampleVSTPlugin', '', GUIDToString(Class_ContextMenu));
   CreateRegKey('CLSID\' + GUIDToString(ClassID) + '\' + ComServer.ServerKey, 'ThreadingModel', 'Apartment');
  end else DeleteRegKey('*\ShellEx\ContextMenuHandlers\OversampleVSTPlugin');
end;

{ TContextMenu }

{ Build a context menu using the existing Menu handle. If Menu is nil,
  we create a new menu handle and return it in the function's return
  value. Note that this function does not handle nested (recursive)
  menus. This exercise is left to the reader. }
function TContextMenu.BuildSubMenu(Menu: HMENU; IndexMenu: Integer;
  var IDCmdFirst: Integer): HMENU;
var
  menuItemInfo: TMenuItemInfo;
begin
  if Menu = 0
   then Result := CreateMenu
   else Result := Menu;

  // Build the menu items here
  with MenuItemInfo do
   begin
    // clear MenuItemInfo, just to be sure 
    FillChar(MenuItemInfo, SizeOf(TMenuItemInfo), 0);

    // common
    cbSize        := SizeOf(TMenuItemInfo);
    fMask         := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE or MIIM_CHECKMARKS;
    fType         := MFT_STRING;
    fState        := MFS_ENABLED;
    hSubMenu      := 0;
    hbmpChecked   := 0;
    hbmpUnchecked := 0;

    // separator
    wID   := IDCmdFirst;
    fType := MFT_SEPARATOR;
    InsertMenuItem(Result, IndexMenu, True, MenuItemInfo);
    Inc(IDCmdFirst);

    // item
    wID   := IDCmdFirst;
    fType := MFT_String;
    dwTypeData := PChar('Oversample VST Plugin' + #0);
    InsertMenuItem(Result, IndexMenu + 1, True, MenuItemInfo);
    Inc(IDCmdFirst);
   end;
end;

{ IShellExtInit }

procedure TContextMenu.SavePlugin;
var
  RS  : TResourceStream;
  RM  : TPEResourceModule;
  RD  : TResourceDetails;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try
   // load template
   RS := TResourceStream.Create(HInstance, 'OversampleTemplate', 'DLL');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;

   // store VST Plugins
   with TMemoryStream.Create do
    try
     LoadFromFile(szFile);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST1', 'DLL', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   SortResources;
   SaveToFile(ExtractFilePath(szFile) + 'Oversampled ' + ExtractFileName(szFile));
   ShowMessage('Plugin ' + '"Oversampled ' + ExtractFileName(szFile) + '" successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

function TContextMenu.IShellExtInit_Initialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
var
  Medium : TStgMedium;
  FE     : TFormatEtc;

begin
  // Fail the call if lpdobj is Nil.
  if lpdobj = nil then
   begin
    Result := E_FAIL;
    Exit;
   end;

  with FE do
   try
    // clear FE, just to be sure
    FillChar(FE, SizeOf(TFormatEtc), 0);

    cfFormat := CF_HDROP;
    ptd      := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex   := -1;
    tymed    := TYMED_HGLOBAL;
   except
    Result := E_FAIL;
    Exit;
   end;

  // Render the data referenced by the IDataObject pointer to an HGLOBAL
  // storage medium in CF_HDROP format.
  Result := lpdobj.GetData(FE, Medium);
  try
   if Failed(Result) then Exit;

   // If only one file is selected, retrieve the file name and store it in
   // szFile. Otherwise fail the call.
   if DragQueryFile(Medium.hGlobal, $FFFFFFFF, nil, 0) = 1 then
    try
     DragQueryFile(Medium.hGlobal, 0, @szFile[0], MAX_PATH);
     Result := NOERROR;
    except
     Result := E_FAIL;
    end
   else Result := E_FAIL;
  finally
   ReleaseStgMedium(medium);
  end;
end;

{ IContextMenu }

function TContextMenu.QueryContextMenu(Menu: HMENU;
  indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult;
var
  Extension     : string;
  idLastCommand : Integer;
begin
  Result := E_FAIL;
  idLastCommand := idCmdFirst;

  // extract the filename extension from the file dropped
  Extension := UpperCase(ExtractFileExt(szFile));

  if Extension = '.DLL' then
   try
    BuildSubMenu(Menu, indexMenu, idLastCommand);

    // Return value is number of items added to context menu
    Result := idLastCommand - idCmdFirst;
   except
    Result := E_FAIL;
   end;
end;

function TContextMenu.InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult;
var
  idCmd: UINT;
begin
  if HIWORD(Integer(lpici.lpVerb)) <> 0
   then Result := E_FAIL
   else
    try
     idCmd := LOWORD(lpici.lpVerb);
     Result := S_OK;

     case idCmd of
      1 : SavePlugin;
     else
      Result := E_FAIL;
     end;
    except
     Result := E_FAIL;
    end;
end;

function TContextMenu.GetCommandString(idCmd: UINT_PTR; uFlags: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;
begin
  Result := S_OK;
end;

initialization
  { Note that we create an instance of TContextMenuFactory here rather
    than TComObjectFactory. This is necessary so that we can add some
    custom registry entries by overriding the UpdateRegistry virtual
    function. }
  TContextMenuFactory.Create(ComServer, TContextMenu, Class_ContextMenu,
    'ContextMenu', 'Oversample VST Plugin', ciMultiInstance);

end.
