unit MainUnit;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SyncObjs, AppEvnts, DAV_GuiPixelMap, DAV_GuiVector,
  DAV_GuiFixedPoint, DAV_GuiVectorPixel, DAV_GuiVectorPixelLine,
  DAV_GuiVectorPixelCircle, DAV_GuiVectorPixelRectangle;

type
  TFmVectorGraphicTest = class(TForm)
    LbTestType: TLabel;
    PaintBox: TPaintBox;
    CbTestType: TComboBox;
    CbDraft: TCheckBox;
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure CbTestTypeChange(Sender: TObject);
    procedure CbDraftClick(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure PaintBoxClick(Sender: TObject);
  private
    FPixelMap        : TGuiPixelMapMemory;
    FPrimitiveClass  : TCustomGuiPixelPrimitiveClass;
    FPrimitives      : array of TCustomGuiPixelPrimitive;
    FCriticalSection : TCriticalSection;
    FIniFileName     : TFileName;
    FOffset          : Integer;
  protected
    procedure BuildRandomPrimitives;
  public
    procedure Render;
  end;

var
  FmVectorGraphicTest: TFmVectorGraphicTest;

implementation

{$R *.dfm}

uses
  Math, IniFiles, DAV_Math, DAV_Complex;

procedure TFmVectorGraphicTest.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
 FPrimitiveClass := TGuiPixelFilledRectangle;
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'VectorGraphicText.ini';
 ControlStyle := ControlStyle + [csOpaque];
 FCriticalSection := TCriticalSection.Create;
 FOffset := 0;
 Randomize;
end;

procedure TFmVectorGraphicTest.FormDestroy(Sender: TObject);
var
  Index : Integer;
begin
 FreeAndNil(FPixelMap);
 FreeAndNil(FCriticalSection);

 for Index := 0 to Length(FPrimitives) - 1
  do FreeAndNil(FPrimitives[Index]);
end;

procedure TFmVectorGraphicTest.FormPaint(Sender: TObject);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPixelMap)
   then FPixelMap.PaintTo(PaintBox.Canvas);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TFmVectorGraphicTest.FormResize(Sender: TObject);
begin
 if Assigned(FPixelMap) then
  begin
   FPixelMap.SetSize(PaintBox.Width, PaintBox.Height);
   FPixelMap.Clear;
   BuildRandomPrimitives;
   Render;
  end;
end;

procedure TFmVectorGraphicTest.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FIniFileName) do
  try
   Left := ReadInteger('Layout', 'Left', Left);
   Top := ReadInteger('Layout', 'Top', Top);
   Width := ReadInteger('Layout', 'Width', Width);
   Height := ReadInteger('Layout', 'Height', Height);
   CbTestType.ItemIndex := CbTestType.Items.IndexOf(
     ReadString('Recent', 'Primitive', CbTestType.Text));
   CbTestTypeChange(Self);
   CbDraft.Checked := ReadBool('Recent', 'Draft', CbDraft.Checked);
  finally
   Free;
  end;
end;

procedure TFmVectorGraphicTest.PaintBoxClick(Sender: TObject);
begin
 ApplicationEvents.OnIdle := nil;
 BuildRandomPrimitives;
 Inc(FOffset);
 FPixelMap.Clear;
 Render;
end;

procedure TFmVectorGraphicTest.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 ApplicationEvents.OnIdle := nil;
 Application.ProcessMessages;
 with TIniFile.Create(FIniFileName) do
  try
   WriteInteger('Layout', 'Left', Left);
   WriteInteger('Layout', 'Top', Top);
   WriteInteger('Layout', 'Width', Width);
   WriteInteger('Layout', 'Height', Height);
   WriteString('Recent', 'Primitive', CbTestType.Text);
   WriteBool('Recent', 'Draft', CbDraft.Checked);
  finally
   Free;
  end;
end;

procedure TFmVectorGraphicTest.FormClick(Sender: TObject);
begin
 ApplicationEvents.OnIdle := nil;
 FPixelMap.Clear;
 Render;
end;

procedure TFmVectorGraphicTest.FormDblClick(Sender: TObject);
begin
 ApplicationEvents.OnIdle := ApplicationEventsIdle;
end;

procedure TFmVectorGraphicTest.CbDraftClick(Sender: TObject);
begin
 FPixelMap.Clear;
 Render;
end;

procedure TFmVectorGraphicTest.CbTestTypeChange(Sender: TObject);
const
  CPrimitiveClasses : array [0..11] of TCustomGuiPixelPrimitiveClass = (
    TGuiPixelFilledRectangle, TGuiPixelFilledRoundedRectangle,
    TGuiPixelFilledCircle, TGuiPixelFilledCircleSector, TGuiPixelFilledEllipse,
    TGuiPixelFrameRectangle, TGuiPixelFrameRoundedRectangle,
    TGuiPixelFrameCircle, TGuiPixelFrameCircleSector, TGuiPixelFrameEllipse,
    TGuiPixelThinLine, TGuiPixelLine);
begin
 Assert(CbTestType.Items.Count = Length(CPrimitiveClasses));
 FPrimitiveClass := CPrimitiveClasses[CbTestType.ItemIndex];
 BuildRandomPrimitives;
 FPixelMap.Clear;
 Render;
end;

procedure TFmVectorGraphicTest.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
 BuildRandomPrimitives;
 FPixelMap.Clear;
 Render;
 Done := False;
end;

procedure TFmVectorGraphicTest.BuildRandomPrimitives;
var
  Index : Integer;
  Cmplx : TComplex32;
begin
 if not Assigned(FPixelMap.DataPointer) then Exit;

 for Index := 0 to Length(FPrimitives) - 1 do
  if Assigned(FPrimitives[Index])
   then FreeAndNil(FPrimitives[Index]);

 SetLength(FPrimitives, 10);

 for Index := 0 to Length(FPrimitives) - 1 do
  begin
   FPrimitives[Index] := FPrimitiveClass.Create;

   // set color and alpha value
   if FPrimitives[Index] is TCustomGuiPixelSimplePrimitive then
    with TCustomGuiPixelSimplePrimitive(FPrimitives[Index]) do
     begin
      Color := Random($7FFF);
      Alpha := Random($FF);
     end;

   with FPrimitives[Index] do
    if GeometricShape is TGuiCircle then
     with FPixelMap, TGuiCircle(GeometricShape) do
      begin
       CenterX := ConvertToFixed24Dot8(Random(Width - 1) + Random);
       CenterY := ConvertToFixed24Dot8(Random(Height - 1) + Random);
       Radius := ConvertToFixed24Dot8(Random(Width div 4) + Random);
      end else
    if GeometricShape is TGuiEllipse then
     with FPixelMap, TGuiEllipse(GeometricShape) do
      begin
       CenterX := ConvertToFixed24Dot8(Random(Width - 1) + Random);
       CenterY := ConvertToFixed24Dot8(Random(Height - 1) + Random);
       RadiusX := ConvertToFixed24Dot8(Random(Width div 4) + Random);
       RadiusY := ConvertToFixed24Dot8(Random(Height div 4) + Random);
      end else
    if GeometricShape is TGuiRectangle then
     with FPixelMap, TGuiRectangle(GeometricShape) do
      begin
       Left := ConvertToFixed24Dot8(Random(2 * (FPixelMap.Width - 1)) - FPixelMap.Width div 2 + Random);
       Top := ConvertToFixed24Dot8(Random(2 * (FPixelMap.Height - 1)) - FPixelMap.Height div 2 + Random);
       Right := FixedAdd(Left, ConvertToFixed24Dot8(Random(FPixelMap.Width - 1)));
       Bottom := FixedAdd(Top, ConvertToFixed24Dot8(Random(FPixelMap.Height - 1)));
       if GeometricShape is TGuiRoundedRectangle
        then TGuiRoundedRectangle(GeometricShape).BorderRadius := ConvertToFixed24Dot8(0.5 * Height * Random);
      end else
    if GeometricShape is TGuiLine then
     with FPixelMap, TGuiLine(GeometricShape) do
      begin

(*
       if 2 * Index >= Length(FPrimitives) then
        begin
         GetSinCos(4 * Pi * Index / Length(FPrimitives), Cmplx.Re, Cmplx.Im);
         XA := ConvertToFixed24Dot8(0.5 * Width * (1 + 0.2 * Cmplx.Re));
         YA := ConvertToFixed24Dot8(0.5 * Height * (1 + 0.2 * Cmplx.Im));
         XB := ConvertToFixed24Dot8(0.5 * Width * (1 + 0.8 * Cmplx.Re));
         YB := ConvertToFixed24Dot8(0.5 * Height * (1 + 0.8 * Cmplx.Im));
        end
       else
        begin
         GetSinCos(4 * Pi * Index / Length(FPrimitives), Cmplx.Re, Cmplx.Im);
         XA := ConvertToFixed24Dot8(0.5 * Width * (1 + 0.8 * Cmplx.Re));
         YA := ConvertToFixed24Dot8(0.5 * Height * (1 + 0.8 * Cmplx.Im));
         XB := ConvertToFixed24Dot8(0.5 * Width * (1 + 0.9 * Cmplx.Re));
         YB := ConvertToFixed24Dot8(0.5 * Height * (1 + 0.9 * Cmplx.Im));
        end;
*)

(*
       XA := ConvertToFixed24Dot8(10 + 5 * Index + Index / 10);
       XB := XA;
       YA := ConvertToFixed24Dot8(10);
       YB := ConvertToFixed24Dot8(Height - 10);
*)

       XA := ConvertToFixed24Dot8(Random(2 * (Width - 1)) - Width div 2 + Random);
       YA := ConvertToFixed24Dot8(Random(2 * (Height - 1)) - Height div 2 + Random);
       if Index = 0
        then XB := XA
        else XB := ConvertToFixed24Dot8(Random(2 * (Width - 1)) - Width div 2 + Random);
       if Index = 1
        then YB := YA
        else YB := ConvertToFixed24Dot8(Random(2 * (Height - 1)) - Height div 2 + Random);
      end;
  end;
end;

procedure TFmVectorGraphicTest.Render;
var
  Index : Integer;
begin
 FCriticalSection.Enter;
 try
  FPixelMap.MakeOpaque;
  if CbDraft.Checked then
   for Index := 0 to Length(FPrimitives) - 1
    do FPrimitives[Index].DrawDraft(FPixelMap)
  else
   for Index := 0 to Length(FPrimitives) - 1
    do FPrimitives[Index].Draw(FPixelMap);

  FPixelMap.MakeOpaque;
  Invalidate;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
