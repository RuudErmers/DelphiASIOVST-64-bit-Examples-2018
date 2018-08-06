unit VSTShellExt;

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

{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows, ActiveX, ComObj, VSTPropertySheet_TLB, ShlObj, CommCtrl, SysUtils,
  Forms, StdVcl;

type
  TVSTPluginPropertySheet = class(TTypedComObject, IVSTPluginPropertySheet,
                                  IShellExtInit, IShellPropSheetExt)
  private
    FPageForm : TForm;
    FFileName : PChar;
  protected
    function IShellExtInit.Initialize = ShellExtInitialize;
    function ShellExtInitialize(pidlFolder: PItemIDList; lpdobj: IDataObject; hKeyProgID: HKEY): HResult; stdcall;
    function AddPages(lpfnAddPage: TFNAddPropSheetPage; lParam: LPARAM): HResult; stdcall;
    function ReplacePage(uPageID: UINT; lpfnReplaceWith: TFNAddPropSheetPage; lParam: LPARAM): HResult; stdcall;
  end;

  TPSheetTestFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

implementation

uses
  VSTPropertySheet_Page, Dialogs, ComServ, Messages, ShellAPI;

{ TVSTPluginPropertySheet }

const
  IDD_PROPDLG = 100;

function propdlgproc(hDlg: HWND; uMsg: UINT; wp: WPARAM; lp: LPARAM): boolean; stdcall;
var str : string;
begin
  Result := true;
  case uMsg of
    WM_INITDIALOG:
      try
       if PPropSheetPage(lp)^.lParam = 0 then Exit;
       with TVSTPluginPropertySheet(PPropSheetPage(lp)^.lParam) do
        begin
         str := FFileName;
         FPageForm := TFmPage.Create(nil);
         with TFmPage(FPageForm) do
          begin
           BorderStyle := bsNone;
           WindowState := wsMaximized;
           ParentWindow := hDlg;
           Show;
           FileName := str;
          end;
        end;
      except
      end;
    else Result := false;
  end;
end;

function propcallback(Wnd: HWnd; Msg: Integer; PPSP: PPropSheetPageA): Integer; stdcall;
begin
 result := 1;
 try
  case Msg of
   PSPCB_RELEASE : with TVSTPluginPropertySheet(PPropSheetPage(PPSP)^.lParam)
                    do FreeAndNil(FPageForm);
  end;
 except
  result := 0;
 end;
end;


function TVSTPluginPropertySheet.AddPages(lpfnAddPage: TFNAddPropSheetPage;
  lParam: LPARAM): HResult;
var
  aPSP : TPropSheetPage;
  hPage : HPropSheetPage;
  DLLHnd : THandle;
begin
 Result := NOERROR;
 try
  DLLHnd := SafeLoadLibrary(FFileName,7);
  try
   if (GetProcAddress(DLLHnd, 'main') = nil) and
      (GetProcAddress(DLLHnd, 'VSTPluginMain') = nil)
    then result := E_NOTIMPL;
  finally
   FreeLibrary(DLLHnd);
  end;
 except
  result := E_FAIL;
 end;
 if Result <> NOERROR then exit;

 fillchar(aPSP, sizeof(TPropSheetPage),#0);
 aPSP.dwSize      := sizeof(TPropSheetPage);
 aPSP.dwFlags     := PSP_USETITLE + PSP_USECALLBACK;
 aPSP.hInstance   := hInstance;
 aPSP.pszTemplate := MakeIntResource(IDD_PROPDLG);
 aPSP.pszTitle    := 'VST Plugin';
 aPSP.pfnDlgProc  := @propdlgproc;
 aPSP.pfnCallback := @propcallback;
 aPSP.lParam      := Integer(Self);

 hPage := CreatePropertySheetPage(aPSP);
 if (hPage <> nil) then
  if (lpfnAddPage(hPage, lParam) = FALSE)
   then DestroyPropertySheetPage(hPage);

 Result := NOERROR;
end;

function TVSTPluginPropertySheet.ReplacePage(uPageID: UINT;
  lpfnReplaceWith: TFNAddPropSheetPage; lParam: LPARAM): HResult;
begin
 Result := E_NOTIMPL; // Dummy
end;

function TVSTPluginPropertySheet.ShellExtInitialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
var
  StgMedium : TStgMedium;
  FormatEtc : TFormatEtc;
begin
 if assigned(FPageForm) then FreeAndNil(FPageForm); 
 Result := E_INVALIDARG;
 if(lpdobj = nil) then exit;
 with FormatEtc do
  begin
   cfFormat := CF_HDROP;
   ptd      := nil;
   dwAspect := DVASPECT_CONTENT;
   lindex   := -1;
   tymed    := TYMED_HGLOBAL;
  end;
 Result := lpdobj.GetData(FormatEtc, StgMedium); if Failed(Result) then exit;
 if (DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, nil, 0) = 1) then
  begin
   if FFileName <> nil then FreeMem(FFileName);
   GetMem(FFileName, 256);
   DragQueryFile(StgMedium.hGlobal, 0, FFileName, 256);
   Result := NOERROR;
  end;
end;

{ TPSheetTestFactory }

procedure TPSheetTestFactory.UpdateRegistry(Register: Boolean);
const
  szTestExtension = 'dllfile\shellex\PropertySheetHandlers\';
begin
 inherited;
 if register
  then CreateRegKey(szTestExtension + ClassName,'',GUIDToString(ClassID))
  else DeleteRegKey(szTestExtension + ClassName);
end;

initialization
  TPSheetTestFactory.Create(ComServer, TVSTPluginPropertySheet, Class_VSTPluginPropertySheet,
    'PropertySheetTest', '', ciMultiInstance, tmApartment);

end.
