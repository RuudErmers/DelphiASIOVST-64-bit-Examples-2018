unit DAV_GuiGraphicControl;

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

{$I ..\DAV_Compiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, Types, {$ELSE} Windows, Messages, {$ENDIF}
  Classes, Graphics, Forms, SysUtils, Controls, DAV_GuiCommon, DAV_GuiPixelMap;

type
  TCustomGuiGraphicControl = class(TGraphicControl)
  private
    FUpdateBackBuffer : Boolean;
    FUpdateBuffer     : Boolean;
    FTransparent      : Boolean;
    procedure SetTransparent(const Value: Boolean);
  protected
    FBuffer           : TGuiCustomPixelMap;
    FBackBuffer       : TGuiCustomPixelMap;
    FOnPaint          : TNotifyEvent;

    {$IFNDEF FPC}
    {$IFNDEF COMPILER10_UP}
    FOnMouseLeave     : TNotifyEvent;
    FOnMouseEnter     : TNotifyEvent;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    {$ENDIF}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    {$ELSE}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
    {$ENDIF}

    procedure BufferChanged;
    procedure BackBufferChanged;
    procedure UpdateBackBuffer; virtual;
    procedure UpdateBuffer; virtual;
    procedure TransparentChanged; virtual;

    procedure Loaded; override;
    procedure Resize; override;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update; override;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Transparent: Boolean read FTransparent write SetTransparent default False;

    {$IFNDEF FPC}
    {$IFNDEF COMPILER10_UP}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
    {$ENDIF}
  end;

implementation

{ TCustomGuiGraphicControl }

constructor TCustomGuiGraphicControl.Create(AOwner: TComponent);
begin
 inherited;
 FBuffer        := TGuiPixelMapMemory.Create;
 FBackBuffer    := TGuiPixelMapMemory.Create;
 FUpdateBuffer  := False;
 ControlStyle   := ControlStyle + [csOpaque];
 {$IFDEF FPC}
// DoubleBuffered := True;
 {$ENDIF}
end;

destructor TCustomGuiGraphicControl.Destroy;
begin
 FreeAndNil(FBuffer);
 FreeAndNil(FBackBuffer);
 inherited;
end;

{$IFNDEF FPC}
{$IFNDEF COMPILER10_UP}
procedure TCustomGuiGraphicControl.CMMouseEnter(var Message: TMessage);
begin
 if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TCustomGuiGraphicControl.CMMouseLeave(var Message: TMessage);
begin
 if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;
{$ENDIF}
{$ENDIF}

procedure TCustomGuiGraphicControl.CMColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
 inherited;
 if not FTransparent
  then BackBufferChanged;
end;

procedure TCustomGuiGraphicControl.Loaded;
begin
 inherited;
 Resize;
end;

procedure TCustomGuiGraphicControl.Resize;
begin
 if Assigned(FBuffer)
  then FBuffer.SetSize(Width, Height);

 if Assigned(FBackBuffer) then
  begin
   FBackBuffer.SetSize(Width, Height);
   BackBufferChanged;
  end;

 inherited;
end;

procedure TCustomGuiGraphicControl.SetTransparent(const Value: Boolean);
begin
 if FTransparent <> Value then
  begin
   FTransparent := Value;
   TransparentChanged;
  end;
end;

procedure TCustomGuiGraphicControl.TransparentChanged;
begin
 BackBufferChanged;
end;

procedure TCustomGuiGraphicControl.Paint;
begin
 inherited;

 // check whether the buffer exists
 if not Assigned(FBuffer) then Exit;

 // eventually update the back buffer
 if FUpdateBackBuffer
  then UpdateBackBuffer;

 // eventuall update the buffer
 if FUpdateBuffer
  then UpdateBuffer;

 // eventuall call the OnPaint event
 if Assigned(FOnPaint)
  then FOnPaint(Self);

 // draw buffer to canvas
 FBuffer.PaintTo(Canvas);
end;

procedure TCustomGuiGraphicControl.BufferChanged;
begin
 FUpdateBuffer := True;
 Invalidate;
end;

procedure TCustomGuiGraphicControl.BackBufferChanged;
begin
 FUpdateBackBuffer := True;
 Invalidate;
end;

procedure TCustomGuiGraphicControl.Update;
begin
  inherited;

end;

procedure TCustomGuiGraphicControl.UpdateBackBuffer;
var
  PixelColor32 : TPixel32;
begin
 FUpdateBackBuffer := False;
 {$IFNDEF FPC}
 if FTransparent then FBackBuffer.CopyParentImage(Self) else
 {$ENDIF}
  begin
   PixelColor32 := ConvertColor(Color);
   FBackBuffer.FillRect(ClientRect, PixelColor32);
  end;
 FBackBuffer.MakeOpaque;

 FUpdateBuffer := True;
end;

procedure TCustomGuiGraphicControl.UpdateBuffer;
begin
 FUpdateBuffer := False;

 // check whether a buffer or a back buffer is assigned and that all
 Assert(Assigned(FBuffer) and Assigned(FBackBuffer));
 Assert((FBackBuffer.Width = FBuffer.Width) and (FBackBuffer.Height = FBuffer.Height));

 // copy back buffer to buffer
 Move(FBackBuffer.DataPointer^, FBuffer.DataPointer^, FBuffer.Height *
   FBuffer.Width * SizeOf(TPixel32));
end;

end.
