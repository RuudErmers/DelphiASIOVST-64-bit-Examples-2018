unit WADSPVST;

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
  Windows, Classes, Forms, SysUtils, Registry, ExtCtrls, Controls,
  DAV_VSTHost, DecimatorModule;

type
  TSmallIntArray = array [0..20000] of Smallint;
  PSmallIntArray = ^TSmallIntArray;
  TShortIntArray = array [0..20000] of ShortInt;
  PShortIntArray = ^TShortIntArray;
  T3Bytes = array [0..2] of Byte;
  P3Bytes = ^T3Bytes;
  T3ByteArray = array [0..20000] of T3Bytes;
  P3ByteArray = ^T3ByteArray;
  PWinampDSPModule = ^TWinampDSPModule;
  PWinAmpDSPHeader = ^TWinAmpDSPheader;
  PFmWinAmpVST = ^TFmWinAmpVST;

  TWAGetHeader = function : PWinAmpDSPHeader; cdecl;
  TWAGetModule = function(Which : Integer ) : PWinAmpDSPModule; cdecl;
  TWAConfig = procedure(This_Mod : PWinAmpDSPModule ); cdecl;
  TWAInit = function(This_Mod : PWinAmpDSPModule ) : Integer; cdecl;
  TWAQuit = procedure(This_Mod : PWinAmpDSPModule ); cdecl;
  TWAModifySamples = function(This_Mod : PWinAmpDSPModule; Samples : Pointer; NumSamples, BitPerSample, nCh, sRate : Integer) : Integer; cdecl;

  TWinampDSPModule = record
                      Description   : Pchar;
                      HwndParent    : Hwnd;
                      hDLLinstance  : Hinst;
                      Config        : TWAConfig;
                      Init          : TWAInit;
                      ModifySamples : TWAModifySamples;
                      Quit          : TWAQuit;
                      UserData      : PFmWinAmpVST;
                     end;

  TWinAmpDSPHeader = record
                      Version      : Integer;
                      Description  : PChar;
                      GetModule    : TWAGetModule;
                     end;

  TFmWinAmpVST = class(TForm)
    VstHost: TVstHost;
    PnGUI: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    DecimatorModule : TVSTDecimator;
    procedure LoadVST;
    procedure ClosePlugin;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
  end;

function winampDSPGetHeader2 : PWinAmpDSPHeader; cdecl; export;
function GetModule(Which : Integer) : PWinAmpDSPModule; cdecl;
procedure Config(This_Mod : PWinAmpDSPModule); cdecl;
function Init(This_Mod : PWinAmpDSPModule) : Integer; cdecl;
function ModifySamples(This_Mod : PWinAmpDSPModule; Samples : Pointer; NumSamples, BitPerSample, nCh, sRate : Integer
                      ) : Integer; cdecl;
procedure Quit(This_Mod : PWinAmpDSPModule); cdecl;

implementation

uses Math, SyncObjs, DAV_Types, DAV_VSTEffect;

var WADSPHeader  : TWinAmpDSPheader =
                   (Version : $20;
                    Description : 'Decimator for WinAmp';
                    GetModule : GetModule);

    WADSPModule : TWinAmpDSPModule =
                   (Description : 'Decimator for WinAmp';
                    HwndParent : 0;
                    hDLLinstance : 0;
                    Config : Config;
                    Init : Init;
                    ModifySamples : ModifySamples;
                    Quit : Quit;
                    UserData : nil);
    FmWinAmpVST : TFmWinAmpVST  = nil;
    CriticalSection : TCriticalSection;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

exports winampDSPGetHeader2;

function winampDSPGetHeader2 : PWinAmpDSPHeader; cdecl;
begin
 try
  Result := @WADSPHeader;
 except
  Result := nil;
 end;
end;

function GetModule(Which : Integer) : PWinAmpDSPModule;
begin
 case Which of
   0 : Result := @WADSPModule;
 else
  Result := nil;
 end;
end;

function Init(This_Mod : PWinAmpDSPModule) : Integer;
begin
 CriticalSection := TCriticalSection.Create;
 CriticalSection.Enter;
 if not Assigned(FmWinAmpVST) then
  begin
   FmWinAmpVST := TFmWinAmpVST.Create(Application);
   This_Mod^.UserData := @FmWinAmpVST;
  end
 else
  begin
   Application.ProcessMessages; sleep(10);
   WADSPModule.UserData^.Visible := True;
  end;
 CriticalSection.Leave;
 Result := 0;
end;

procedure Config(This_Mod : PWinAmpDSPModule);
begin
 if Assigned(This_Mod^.UserData^)
  then This_Mod^.UserData^.Show;
end;

function ModifySamples(This_Mod : PWinAmpDSPModule; Samples : Pointer;
                       NumSamples, BitPerSample, nCh, sRate : Integer) : Integer;
var
  TmpData  : TDAVArrayOfSingleDynArray;
  i, j, ch : Integer;
  Temp     : Integer;
const
  DivFak8  : Single = 1/$80;     MulFak8:Single  = $7F;
  DivFak16 : Single = 1/$8000;   MulFak16:Single = $7FFF;
  DivFak24 : Single = 1/$800000; MulFak24:Single = $7FFFFF;
begin
 CriticalSection.Enter;
 try
  if Assigned(This_Mod^.UserData^) then
   case BitPerSample of
    8: with This_Mod^.UserData^ do if Assigned(VstHost) then
         if VstHost[0].Active then
          begin
           VstHost[0].SetBlockSizeAndSampleRate(NumSamples,sRate);
           ch := max(VstHost[0].numInputs,VstHost[0].numOutputs);
           ch := max(nCh,ch);
           SetLength(TmpData,ch);
           for i := 0 to ch-1 do
            begin
             SetLength(TmpData[i],NumSamples);
             if i<nCh then
              for j := 0 to NumSamples-1 do TmpData[i,j] := PShortIntArray(Samples)^[j*nCh+i]*DivFak8;
            end;

           VstHost[0].ProcessReplacing(@TmpData[0],@TmpData[0], NumSamples);
           for i := 0 to ch-1 do if i<nCh then
            for j := 0 to NumSamples-1
             do PShortIntArray(Samples)^[j*nCh+i] := Round(f_Limit(1.9*Tanh2b(TmpData[i,j]))*MulFak8);
          end;
    16: with This_Mod^.UserData^ do if Assigned(VstHost) then
         if VstHost[0].Active then
          begin
           VstHost[0].SetBlockSizeAndSampleRate(NumSamples,sRate);
           ch := max(VstHost[0].numInputs,VstHost[0].numOutputs);
           ch := max(nCh,ch);
           SetLength(TmpData,ch);
           for i := 0 to ch-1 do
            begin
             SetLength(TmpData[i],NumSamples);
             if i<nCh then
              for j := 0 to NumSamples-1 do TmpData[i,j] := PSmallIntArray(Samples)^[j*nCh+i]*DivFak16;
            end;

           VstHost[0].ProcessReplacing(@TmpData[0],@TmpData[0], NumSamples);
           for i := 0 to ch-1 do if i<nCh then
            for j := 0 to NumSamples-1
             do PSmallIntArray(Samples)^[j*nCh+i] := Round(f_Limit(1.9*Tanh2b(TmpData[i,j]))*MulFak16);
          end;
    24: with This_Mod^.UserData^ do if Assigned(VstHost) then
         if VstHost[0].Active then
          begin
           VstHost[0].SetBlockSizeAndSampleRate(NumSamples,sRate);
           ch := max(VstHost[0].numInputs,VstHost[0].numOutputs);
           ch := max(nCh,ch);
           SetLength(TmpData,ch);
           for i := 0 to ch-1 do
            begin
             SetLength(TmpData[i],NumSamples);
             if i<nCh then
              for j := 0 to NumSamples-1
               do TmpData[i,j] := ((ShortInt(P3ByteArray(Samples)^[j*nCh+i][2]) shl 16) +
                                 (P3ByteArray(Samples)^[j*nCh+i][1] shl 8)  +
                                 (P3ByteArray(Samples)^[j*nCh+i][0] )) * DivFak24;
            end;
           VstHost[0].ProcessReplacing(@TmpData[0],@TmpData[0], NumSamples);

           for i := 0 to ch-1 do if i<nCh then
            for j := 0 to NumSamples-1 do
             begin
              Temp := Round(f_Limit(1.9*Tanh2b(TmpData[i,j]))*MulFak24);
              P3ByteArray(Samples)^[j*nCh+i][2] := (Temp shr 16) and $FF;
              P3ByteArray(Samples)^[j*nCh+i][1] := (Temp shr 8 ) and $FF;
              P3ByteArray(Samples)^[j*nCh+i][0] := (Temp       ) and $FF;
             end;
          end;
   end;
  Result := NumSamples;
 finally
  CriticalSection.Leave;
 end;
end;

procedure Quit(This_Mod : PWinAmpDSPModule);
begin
 CriticalSection.Enter;
 try This_Mod^.UserData^.ClosePlugin; finally
  try FreeAndNil(This_Mod^.UserData^); finally FmWinAmpVST := nil; end; end;
 CriticalSection.Leave; FreeAndNil(CriticalSection);
end;

{ TFmWinAmpVST }

procedure TFmWinAmpVST.FormCreate(Sender: TObject);
begin
 with TRegistry.Create do
  try
   LoadVST;
   RootKey := HKEY_CURRENT_USER;
   if OpenKeyReadOnly('SOFTWARE\ASIOVST\WinAmp') then
    begin
     if ValueExists('Visible') then if ReadBool('Visible') then Show;
     if ValueExists('Left') then Left := ReadInteger('Left');
     if ValueExists('Visible') then Visible := ReadBool('Visible');
    end;
  finally
   CloseKey;
   Free;
  end;
end;

procedure TFmWinAmpVST.ClosePlugin;
begin
 with TRegistry.Create do
  try
   if OpenKey('Software\ASIOVST\WinAmp',True) then
    begin
     WriteBool('Visible',Visible);
     WriteInteger('Left',Left);
     WriteInteger('Top',Top);
    end;
  finally
   CloseKey;
   Free;
  end;
 Visible := False; 
 with VstHost[0] do
  try
   SavePreset(ExtractFilePath(Application.ExeName)+'Decimator.fxp');
   CloseEdit;
   Active := False;
   Unload;
  finally
   FreeAndNil(FmWinAmpVST);
  end;
end;

procedure TFmWinAmpVST.CreateParams(var Params: TCreateParams);
begin
 inherited CreateParams(Params);
 Params.WndParent := WADSPModule.HwndParent;
end;

procedure TFmWinAmpVST.TimerTimer(Sender: TObject);
begin
 VstHost[0].EditIdle;
// if not fColDetected then SetScheme;
end;

procedure TFmWinAmpVST.LoadVST;
var rct  : ERect;
begin
 with VstHost[0] do
  try
   CriticalSection.Enter;
   try CloseEdit; except end;
   Active := False;
   sleep(10);
   try Unload; except end;
   sleep(10);

   DecimatorModule := TVSTDecimator.Create(Application);
   DecimatorModule.Effect^.user := DecimatorModule;
   DecimatorModule.AudioMaster := audioMaster;
   LoadFromVSTEffect(DecimatorModule.Effect);

   Active := True;
   try
    ShowEdit(TForm(PnGUI));
    Idle;
    EditIdle;
   except
     raise
   end;
   Caption := 'ASIO-VST - Decimator';
  finally
   CriticalSection.Leave;
  end;

 rct := VSTHost[0].EditGetRect;
 ClientWidth := rct.right - rct.left;
 ClientHeight := rct.bottom - rct.Top;
end;

end.
