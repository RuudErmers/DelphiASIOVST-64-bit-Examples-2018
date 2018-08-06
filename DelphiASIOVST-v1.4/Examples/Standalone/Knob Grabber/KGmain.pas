unit KGmain;

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

uses
  {$IFDEF FPC} LCLType, LMessages, {$ELSE} Windows, Messages, {$ENDIF} SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, Menus, ExtCtrls, DAV_VSTHost;

type
  TStitchType = (stHorizontal, stVertical);
  TFmKnobGrabber = class(TForm)
    MainMenu: TMainMenu;
    MIAutoStitch: TMenuItem;
    MIExit: TMenuItem;
    MIFile: TMenuItem;
    MIGrabKnobs: TMenuItem;
    MIHorizontalStitch: TMenuItem;
    MIOpen: TMenuItem;
    MIStitch: TMenuItem;
    MIVerticalStitch: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    PnGUI: TPanel;
    VstHost: TVstHost;
    procedure FormCreate(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MIOpenClick(Sender: TObject);
    procedure MIGrabKnobsClick(Sender: TObject);
  private
    FFileName             : TFileName;
    FCheckParameterString : Boolean;
    procedure LoadVstPlugin(FileName: TFileName);
    function FindKnobBounds(ParameterNo: Integer): TRect;
    procedure GrabKnob(ParameterNo: Integer; rct: TRect; FileName: string);
  public
    property CheckParameterString: Boolean read FCheckParameterString;
  end;

var
  FmKnobGrabber: TFmKnobGrabber;

implementation

{$R *.dfm}

uses
  Types, FileCtrl, PngImage, DAV_GuiCommon;

procedure TFmKnobGrabber.FormCreate(Sender: TObject);
begin
 FCheckParameterString := True;
 if FileExists(ParamStr(1))
  then LoadVstPlugin(ParamStr(1));
end;

procedure TFmKnobGrabber.MIExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmKnobGrabber.MIGrabKnobsClick(Sender: TObject);
var
  Dir : string;
  rct : TRect;
  Prs : Integer;
  PNm : string;
begin
 Dir := ExtractFileDir(FFileName);
 SelectDirectory('Select a directory', '', Dir);
(*
*)
 PNm := ExtractFileName(FFileName);
 if Pos('.', PNm) > 0
  then PNm := Dir + Copy(PNm, 1, Pos('.', PNm) - 1);

 with VstHost[0] do
  for Prs := 0 to numParams - 1 do
   begin
    rct := FindKnobBounds(Prs);
    if (rct.Right - rct.Left > 0) and (rct.Bottom - rct.Top > 0)
     then GrabKnob(Prs, rct, PNm + IntToStr(Prs) + '.png');
   end;
end;

function TFmKnobGrabber.FindKnobBounds(ParameterNo: Integer): TRect;
var
  Bmp   : array [0..2] of TBitmap;
  Param : Single;
  PrmSt : array [0..1] of string;
  rct   : TRect;
  x, y  : Integer;
  Scln  : PIntegerArray;
begin
 with VstHost[0] do
  if Active then
   begin
    // create temp bitmaps
    Bmp[0] := TBitmap.Create;
    Bmp[1] := TBitmap.Create;
    Bmp[2] := TBitmap.Create;
    try
     // set dimensions
     rct := GetRect;
     Bmp[0].Width := rct.Right - rct.Left;
     Bmp[0].Height := rct.Bottom - rct.Top;
     Bmp[2].Assign(Bmp[0]);
     Bmp[2].Canvas.Brush.Color := clBlack;
     Bmp[2].Canvas.FillRect(ClientRect);
     Bmp[2].Canvas.CopyMode := cmSrcPaint; // copy mode = OR (= accumulate)

     // render basic image
     Parameter[ParameterNo] := 0;
     EditIdle;
     Idle;
     Application.ProcessMessages;
     RenderEditorToBitmap(Bmp[0]);
     Application.ProcessMessages;

     // assign first temp bitmap and set copy mode to XOR (only changes)
     Bmp[1].Assign(Bmp[0]);
     Bmp[1].Canvas.CopyMode := cmSrcInvert;

     Param := 0.001; PrmSt[1] := '';
     while Param <= 1 do
      begin
       // change parameter and idle to ensure drawing
       Parameter[ParameterNo] := Param;
       EditIdle; Idle;
       Application.ProcessMessages;
       if CheckParameterString then
        begin
         PrmSt[0] := ParameterDisplay[ParameterNo];
         if (PrmSt[0] = PrmSt[1]) then
          begin
           Param := Param + 0.001;
           Continue;
          end;
         PrmSt[1] := PrmSt[0];
        end;
       RenderEditorToBitmap(Bmp[1]);
       Application.ProcessMessages;

       // XOR to Bmp[1] for changes and accumulate these to Bmp[2]
       Bmp[1].Canvas.Draw(0, 0, Bmp[0]);
       Bmp[2].Canvas.Draw(0, 0, Bmp[1]);

       // advance parameter
       Param := Param + 0.001;
      end;

     // find rect
     Bmp[2].PixelFormat := pf32bit;
     Result := Rect(Bmp[2].Width, Bmp[2].Height, 0, 0);
     for y := 0 to Bmp[2].Height - 1 do
      begin
       Scln := PIntegerArray(Bmp[2].ScanLine[y]);
       for x := 0 to Bmp[2].Width - 1 do
        if Scln^[x] <> clBlack then
         begin
          if x < Result.Left then Result.Left := x;
          if x > Result.Right then Result.Right := x;
          if y < Result.Top then Result.Top := y;
          if y > Result.Bottom then Result.Bottom := y;
         end;
      end;

     Result.Right := Result.Right + 1;
     Result.Bottom := Result.Bottom + 1;

(*
     // draw rect
     Bmp[2].Canvas.Brush.Color := clWhite;
     Bmp[2].Canvas.FrameRect(Result);
*)
    finally
     // dispose temp bitmaps
     FreeAndNil(Bmp[0]);
     FreeAndNil(Bmp[1]);
     FreeAndNil(Bmp[2]);
    end;
   end else Result := Rect(0, 0, 0, 0);
end;

procedure TFmKnobGrabber.GrabKnob(ParameterNo: Integer; rct: TRect; FileName: string);
var
  Png   : TPNGObject;
  Bmp   : array [0..2] of TBitmap;
  Param : Single;
  PrmSt : array [0..1] of string;
  vrct  : TRect;
  x, y  : Integer;
  Scln  : array [0..1] of PIntegerArray;
  sttyp : TStitchType;

label
  next;
begin
 Png := TPNGObject.Create;
 try
  with VstHost[0] do
   if Active then
    begin
     // create temp bitmaps
     Bmp[0] := TBitmap.Create;
     Bmp[1] := TBitmap.Create;
     Bmp[2] := TBitmap.Create;
     try
      // set dimensions
      vrct := GetRect;
      Bmp[0].Width := vrct.Right - vrct.Left;
      Bmp[0].Height := vrct.Bottom - vrct.Top;

      Bmp[1].Width := rct.Right - rct.Left;
      Bmp[1].Height := rct.Bottom - rct.Top;
      Bmp[1].PixelFormat := pf32bit;

      Bmp[2].Width := rct.Right - rct.Left;
      Bmp[2].Height := rct.Bottom - rct.Top;
      Bmp[2].PixelFormat := pf32bit;

      // define stitch type
      if MIHorizontalStitch.Checked then sttyp := stHorizontal else
      if MIVerticalStitch.Checked then sttyp := stVertical
       else sttyp := TStitchType(Bmp[1].Width > Bmp[1].Height);

      // render basic image
      Parameter[ParameterNo] := 0;
      EditIdle;
      Idle;
      Application.ProcessMessages;
      RenderEditorToBitmap(Bmp[0]);
      Application.ProcessMessages;

      // copy knob area and assign to PNG
      Bmp[1].Canvas.CopyRect(Rect(0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top), Bmp[0].Canvas, rct);
      Png.Assign(Bmp[1]);

      Param := 0.001; PrmSt[1] := '';
      while Param <= 1 do
       begin
        Parameter[ParameterNo] := Param;
        EditIdle; Idle;
        Application.ProcessMessages;
        if CheckParameterString then
         begin
          PrmSt[0] := ParameterDisplay[ParameterNo];
          if (PrmSt[0] = PrmSt[1]) then goto next;
          PrmSt[1] := PrmSt[0];
         end;
        RenderEditorToBitmap(Bmp[0]);
        Application.ProcessMessages;

        // copy knob area
        Bmp[2].Canvas.CopyRect(Rect(0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top), Bmp[0].Canvas, rct);

        // check for changes
        for y := 0 to Bmp[2].Height - 1 do
         begin
          Scln[0] := PIntegerArray(Bmp[2].ScanLine[y]);
          Scln[1] := PIntegerArray(Bmp[1].ScanLine[y]);
          for x := 0 to Bmp[2].Width - 1 do
           if Scln[0]^[x] <> Scln[1]^[x] then
            begin
             // a change found, copy to PNG and break
             case sttyp of
              stHorizontal :
               begin

                Png.Resize(Png.Width + (rct.Right - rct.Left), Png.Height);
                Png.Canvas.CopyRect(rect(Png.Width - (rct.Right - rct.Left), 0, Png.Width, Png.Height),
                  Bmp[2].Canvas, Rect(0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top));
                Bmp[1].Assign(Bmp[2]);
               end;
              stVertical :
               begin
                Png.Resize(Png.Width, Png.Height + (rct.Bottom - rct.Top));
                Png.Canvas.CopyRect(rect(0, Png.Height - (rct.Bottom - rct.Top), Png.Width, Png.Height),
                  Bmp[2].Canvas, Rect(0, 0, rct.Right - rct.Left, rct.Bottom - rct.Top));
                Bmp[1].Assign(Bmp[2]);
               end;
             end;

             Application.ProcessMessages;
             goto next;
            end;
         end;

next:
        // advance parameter
        Param := Param + 0.001;
       end;

     finally
      // dispose temporary bitmaps
      FreeAndNil(Bmp[0]);
      FreeAndNil(Bmp[1]);
      FreeAndNil(Bmp[2]);
     end;
     Png.SaveToFile(FileName);
    end;
 finally
  FreeAndNil(Png);
 end;
end;

procedure TFmKnobGrabber.MIOpenClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute
   then LoadVstPlugin(FileName);
end;

procedure TFmKnobGrabber.LoadVstPlugin(FileName: TFileName);
var
  rct : TRect;
begin
 with VstHost[0] do
  begin
   LoadFromFile(FileName);
   Active := True;
   ShowEdit(PnGUI);
   rct := GetRect;
   ClientWidth := rct.Right - rct.Left;
   ClientHeight := (rct.Bottom - rct.Top);
//   Image.Height := (rct.Bottom - rct.Top);
   FFileName := FileName;
   MIGrabKnobs.Enabled := True;
  end;
end;

end.
