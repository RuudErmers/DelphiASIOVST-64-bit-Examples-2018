unit AShellExt;

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

{$I DAV_Compiler.inc}
{$WARN SYMBOL_PLATFORM OFF}

uses
  Windows, ActiveX, ComObj, AudioSheet_TLB, ShlObj,
  CommCtrl, SysUtils, Forms, StdVcl;

type
  TAudioSheet = class(TTypedComObject, IAudioSheet,
                       IShellExtInit, IShellPropSheetExt)
  private
    FFileName : PChar;
    FPageForm : TForm;
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
  AudioSheet_Page, Dialogs, ComServ, Messages, ShellAPI;

{ TAudioPropertySheet }

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
       with TAudioSheet(PPropSheetPage(lp)^.lParam) do
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
   PSPCB_RELEASE : with TAudioSheet(PPropSheetPage(PPSP)^.lParam)
                    do FreeAndNil(FPageForm);
  end;
 except
  result := 0;
 end;
end;


function TAudioSheet.AddPages(lpfnAddPage: TFNAddPropSheetPage;
  lParam: LPARAM): HResult;
var
  aPSP : TPropSheetPage;
  hPage : HPropSheetPage;
begin
 fillchar(aPSP, sizeof(TPropSheetPage),#0);
 aPSP.dwSize      := sizeof(TPropSheetPage);
 aPSP.dwFlags     := PSP_USETITLE + PSP_USECALLBACK;
 aPSP.hInstance   := hInstance;
 aPSP.pszTemplate := MakeIntResource(IDD_PROPDLG);
 aPSP.pszTitle    := 'Audio Sheet';
 aPSP.pfnDlgProc  := @propdlgproc;
 aPSP.pfnCallback := @propcallback;
 aPSP.lParam      := Integer(Self);

 hPage := CreatePropertySheetPage(aPSP);
 if (hPage <> nil) then
  if (lpfnAddPage(hPage, lParam) = FALSE)
   then DestroyPropertySheetPage(hPage);

 Result := NOERROR;
end;

function TAudioSheet.ReplacePage(uPageID: UINT;
  lpfnReplaceWith: TFNAddPropSheetPage; lParam: LPARAM): HResult;
begin
 Result := E_NOTIMPL; // Dummy
end;

function TAudioSheet.ShellExtInitialize(pidlFolder: PItemIDList;
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
  szTestExtensionWav  = 'SystemFileAssociations\.wav\shellex\PropertySheetHandlers\';
  szTestExtensionAU   = 'SystemFileAssociations\.au\shellex\PropertySheetHandlers\';
  szTestExtensionAIFF = 'SystemFileAssociations\.aiff\shellex\PropertySheetHandlers\';
  szTestExtensionDat  = 'datfile\shellex\PropertySheetHandlers\';
  szTestExtensionSpk  = 'spkfile\shellex\PropertySheetHandlers\';
begin
 inherited;
 if register then
  begin
   CreateRegKey('.dat', '', 'datfile');
   CreateRegKey('.spk', '', 'spkfile');
   CreateRegKey(szTestExtensionSpk  + ClassName,'',GUIDToString(ClassID));
   CreateRegKey(szTestExtensionWav  + ClassName,'',GUIDToString(ClassID));
   CreateRegKey(szTestExtensionDat  + ClassName,'',GUIDToString(ClassID));
   CreateRegKey(szTestExtensionAIFF + ClassName,'',GUIDToString(ClassID));
   CreateRegKey(szTestExtensionAU   + ClassName,'',GUIDToString(ClassID));
  end
 else
  begin
   DeleteRegKey(szTestExtensionSpk  + ClassName);
   DeleteRegKey(szTestExtensionWav  + ClassName);
   DeleteRegKey(szTestExtensionDat  + ClassName);
   DeleteRegKey(szTestExtensionAIFF + ClassName);
   DeleteRegKey(szTestExtensionAU   + ClassName);
  end;
end;

initialization
  TPSheetTestFactory.Create(ComServer, TAudioSheet, CLASS_AudioSheet_,
    'AudioSheet', '', ciMultiInstance, tmApartment);

end.
