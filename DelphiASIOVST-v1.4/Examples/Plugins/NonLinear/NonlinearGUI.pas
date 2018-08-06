unit NonlinearGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2006-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} Classes, Messages,
  Forms, Controls, StdCtrls, DAV_Types, DAV_VSTModule;

type
  TVSTGUI = class(TForm)
    SBGain: TScrollBar;
    LbGain: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBGainChange(Sender: TObject);
    procedure LbGainClick(Sender: TObject);
  public
    procedure UpdateGain;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Dialogs, SysUtils, NonlinearDSP;

{ TVSTGUI }

procedure TVSTGUI.FormCreate(Sender: TObject);
begin
 LbGain.Caption  := 'OpAmp Gain';
 SbGain.Max      := 1000;
 SbGain.Min      := 100;
 SbGain.Position := 100;
end;

procedure TVSTGUI.FormShow(Sender: TObject);
begin
 UpdateGain;
end;

procedure TVSTGUI.LbGainClick(Sender: TObject);
//var
//  b : PChar;
begin
(*
 // Example on how to query the DLL name of the plugin
 GetMem(b, 255);
 FillChar(b^, 255, 0);
 try
  ShowMessage('Instance: ' + IntToStr(hInstance));
  GetModuleFileName(hInstance, b, 255);
  ShowMessage(StrPas(b));
 finally
  Dispose(b);
 end;
*)
end;

procedure TVSTGUI.UpdateGain;
begin
 with TVSTOpAmp(Owner) do
  begin
   if Round(10 * Parameter[0]) <> SBGain.Position
    then SBGain.Position := Round(10 * Parameter[0]);
  end;
end;

procedure TVSTGUI.SBGainChange(Sender: TObject);
begin
 with TVSTOpAmp(Owner) do
  begin
   if Parameter[0] <> SBGain.Position * 0.1
    then Parameter[0] := SBGain.Position * 0.1;
  end;
end;

end.
