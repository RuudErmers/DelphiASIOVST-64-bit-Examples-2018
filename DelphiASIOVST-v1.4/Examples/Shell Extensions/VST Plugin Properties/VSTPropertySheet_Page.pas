unit VSTPropertySheet_Page;

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

{$I ..\DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, DAV_VSTHost;

type
  TFmPage = class(TForm)
    VstHost: TVstHost;
    Memo: TListBox;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFileName  : TFileName;
    FGUIBitmap : TBitmap;
    procedure SetFileName(const Value: TFileName);
  published
  public
    property FileName : TFileName read FFileName write SetFileName;
  end;

implementation

{$R *.dfm}

uses
  DAV_VSTEffect;

{ TFmPage }

procedure TFmPage.FormCreate(Sender: TObject);
begin
 FGUIBitmap := TBitmap.Create;
end;

procedure TFmPage.FormDestroy(Sender: TObject);
begin
 if Assigned(FGUIBitmap)
  then FGUIBitmap.Free;
end;

procedure TFmPage.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 200, FGUIBitmap);
end;

procedure TFmPage.SetFileName(const Value: TFileName);
var r  : ERect;
    fm : TForm;
begin
 Memo.Clear;
 FFileName := Value;
 if FileExists(FFileName) then
  with VstHost[0] do
   try
    DLLFileName := FFileName;
    Active := True;
    with Memo.Items do
     begin
      Add('Effect Name: ' + GetEffectName {+ 'Unique ID: ' + PVstEffect^.UniqueID + ', '});
      Add('Vendor: ' + VendorString + ', Product: ' + ProductString);
      Add('VST Version: ' + IntToStr(GetVstVersion) + ', Category: ' + PlugCategory2String(GetPlugCategory));
      Add('Inputs: ' + IntToStr(numInputs) + ', Outputs: ' + IntToStr(numOutputs));
      Add('Parameters: ' + IntToStr(numParams));
      Add('Programs: ' + IntToStr(numPrograms));
      Add('Initial Delay: ' + IntToStr(InitialDelay) + ', Tail Size: ' + IntToStr(GetTailSize));

      if (effFlagsHasEditor in VstEffectPointer.EffectFlags) then
       begin
        fm := TForm.Create(nil);
        with fm do
         try
          ShowEdit(fm);
          EditIdle; Idle;
          Application.ProcessMessages;
          r := EditGetRect;
          ClientWidth := r.Right - r.Left;
          ClientHeight := r.Bottom - r.Top;
          FGUIBitmap.Width := Self.Width;
          FGUIBitmap.Height := (Height * FGUIBitmap.Width) div Width;
          Visible := True;
          Application.ProcessMessages;
          StretchBlt(FGUIBitmap.Canvas.Handle, 0, 0, FGUIBitmap.Width, FGUIBitmap.Height,
                     Canvas.Handle, r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top, cmSrcCopy);
         finally
          CloseEdit;
         end;
        fm.Free;
       end;
     end;
   finally
    if not Active
     then Memo.Items.Add('Error while loading');
    Active := False;
    UnLoad;
   end;
end;

end.
