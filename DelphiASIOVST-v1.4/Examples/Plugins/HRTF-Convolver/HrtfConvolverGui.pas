unit HrtfConvolverGui;

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
{$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Forms, Dialogs, ComCtrls, Controls, Spin, StdCtrls, GLScene, GLObjects,
  GLVectorFileObjects, GLWin32Viewer, GLCoordinates, GLCrossPlatform,
  BaseClasses, DAV_Types, DAV_VSTModule, DAV_GuiAudioDataDisplay, DAV_AudioData;

type
  TFmHrtfConvolver = class(TForm)
    AudioDataDisplay: TGuiAudioDataDisplay;
    AudioDataDisplayIR: TGuiAudioDataDisplay;
    BtLoadHrtfFile: TButton;
    EdHrtfSet: TEdit;
    Gb3D: TGroupBox;
    GbImpulseResponse: TGroupBox;
    GbImpulseResponses: TGroupBox;
    GBPosition: TGroupBox;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLHead: TGLFreeForm;
    GLHRTFs: TGLPoints;
    GLLight: TGLLightSource;
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    LbAzimuth: TLabel;
    LbElevation: TLabel;
    LbHrtfSet: TLabel;
    LbRadius: TLabel;
    OpenDialog: TOpenDialog;
    PCConvolutionSelect: TPageControl;
    SEAzimuth: TSpinEdit;
    SEElevation: TSpinEdit;
    SERadius: TSpinEdit;
    TSHrtf: TTabSheet;
    TSReverb: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure BtLoadHrtfFileClick(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SEAzimuthChange(Sender: TObject);
    procedure SEElevationChange(Sender: TObject);
  private
    FOldMousePoint: TPoint;
    procedure Zoom(Value: Single);
  public
    procedure AzimuthChanged;
    procedure ElevationChanged;
    procedure RadiusChanged;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, VectorGeometry, MeshUtils, Jpeg, TGA, GLFile3DS, GLFileObj,
  VectorLists, DAV_DspHrtf, HrtfConvolverDM,
  DAV_VSTModuleWithPrograms;

procedure TFmHrtfConvolver.FormCreate(Sender: TObject);
var
  ResourceStream: TResourceStream;
  i: Integer;
  Tris, Norms: TAffineVectorList;
  Tex, Buf: TAffineVectorList;
  MorphTris: TAffineVectorList;
  MorphNorms: TAffineVectorList;
  Indices: TIntegerList;
  TexIndices: TIntegerList;
  FirstRemap: TIntegerList;
  SubdivideRemap: TIntegerList;
  BufRemap: TIntegerList;
begin
  ResourceStream := TResourceStream.Create(hInstance, 'Head', '3DS');
  with ResourceStream do
    try
      GLHead.LoadFromStream('Head.3DS', ResourceStream);
      for i := 0 to GLHead.MeshObjects.Count - 1 do
      begin
        Tex := TAffineVectorList.Create;
        try
          with GLHead.MeshObjects[i] do
            Tris := ExtractTriangles(Tex);
          try
            Indices := BuildVectorCountOptimizedIndices(Tris);
            try
              FirstRemap := TIntegerList(Indices.CreateClone);
              RemapAndCleanupReferences(Tris, Indices);
              Norms := BuildNormals(Tris, Indices);

              // subdivide geometry
              SubdivideTriangles(0.6, Tris, Indices, Norms);
              TexIndices := BuildVectorCountOptimizedIndices(Tex);
              RemapAndCleanupReferences(Tex, TexIndices);

              // subdivide texture space
              SubdivideTriangles(0, Tex, TexIndices);

              // Re-expand everything
              Buf := TAffineVectorList.Create;
              try
                ConvertIndexedListToList(Tris, Indices, Buf);
                Tris.Assign(Buf);
                Buf.Count := 0;
                ConvertIndexedListToList(Norms, Indices, Buf);
                Norms.Assign(Buf);
                Buf.Count := 0;
                ConvertIndexedListToList(Tex, TexIndices, Buf);
                Tex.Assign(Buf);
              finally
                FreeAndNil(Buf);
              end;

              // Pack & Optimize the expanded stuff
              FreeAndNil(Indices);
              Indices := BuildVectorCountOptimizedIndices(Tris, Norms, Tex);
              SubdivideRemap := TIntegerList(Indices.CreateClone);
              RemapReferences(Norms, Indices);
              RemapReferences(Tex, Indices);
              RemapAndCleanupReferences(Tris, Indices);

              IncreaseCoherency(Indices, 13);

              with GLHead.MeshObjects[i] do
              begin
                BufRemap := TIntegerList.Create;
                try
                  MorphTris := ExtractTriangles;
                  try
                    BufRemap.Assign(FirstRemap);
                    RemapAndCleanupReferences(MorphTris, BufRemap);

                    MorphNorms := MeshUtils.BuildNormals(MorphTris, BufRemap);
                    try
                      SubdivideTriangles(0.7, MorphTris, BufRemap, MorphNorms);
                      Buf := TAffineVectorList.Create;
                      try
                        ConvertIndexedListToList(MorphTris, BufRemap, Buf);
                        MorphTris.Assign(Buf);
                        ConvertIndexedListToList(MorphNorms, BufRemap, Buf);
                        MorphNorms.Assign(Buf);
                      finally
                        FreeAndNil(Buf);
                      end;
                      RemapReferences(MorphTris, SubdivideRemap);
                      RemapReferences(MorphNorms, SubdivideRemap);
                    finally
                      FreeAndNil(MorphNorms);
                    end;
                  finally
                    FreeAndNil(MorphTris);
                  end;
                finally
                  FreeAndNil(BufRemap);
                end;

                Vertices := Tris;
                Normals := Norms;
                TexCoords := Tex;
                FaceGroups.Clear;
                with TFGVertexIndexList.CreateOwned(FaceGroups) do
                begin
                  VertexIndices := Indices;
                  Mode := fgmmTriangles;
                end;
              end;
              FreeAndNil(TexIndices);
              FreeAndNil(SubdivideRemap);
              FreeAndNil(FirstRemap);
              FreeAndNil(Norms);
            finally
              FreeAndNil(Indices);
            end;
          finally
            FreeAndNil(Tris);
          end;
        finally
          FreeAndNil(Tex);
        end;
      end;
      GLHead.StructureChanged;
    finally
      Free;
    end;

  with THrtfConvolverDataModule(Owner) do
  begin
    AudioDataDisplay.AudioDataCollection := AudioDataCollectionHRTF;
  end;
end;

procedure TFmHrtfConvolver.BtLoadHrtfFileClick(Sender: TObject);
begin
  with THrtfConvolverDataModule(Self.Owner), OpenDialog do
    if Execute then
    begin
      HRTFs.LoadFromFile(FileName);
      HrtfChanged;
    end;
end;

procedure TFmHrtfConvolver.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FOldMousePoint.X := X;
  FOldMousePoint.Y := Y;
end;

procedure TFmHrtfConvolver.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  Scale = 1 / 40;
var
  OriginalT2C, NormalT2C, NormalCameraRight, NewPos: TVector;
  PitchNow, Dist: Single;
begin
  if ssLeft in Shift then
  begin
    with GLSceneViewer.Camera do
    begin
      OriginalT2C := VectorSubtract(AbsolutePosition,
        GLDummyCube.AbsolutePosition);
      SetVector(NormalT2C, OriginalT2C);
      Dist := VectorLength(NormalT2C);
      NormalizeVector(NormalT2C);
      NormalCameraRight := VectorCrossProduct(AbsoluteUp, NormalT2C);
      if VectorLength(NormalCameraRight) < 0.001 then
        SetVector(NormalCameraRight, XVector) // arbitrary vector
      else
        NormalizeVector(NormalCameraRight);
      PitchNow := Math.ArcCos(VectorDotProduct(AbsoluteUp, NormalT2C));
      if not(ssAlt in Shift) then
        PitchNow := ClampValue(PitchNow + VectorGeometry.DegToRad(FOldMousePoint.Y - Y), 0.002,
          PI - 0.77);
      SetVector(NormalT2C, AbsoluteUp);
      RotateVector(NormalT2C, NormalCameraRight, -PitchNow);
      if not(ssShift in Shift) then
        RotateVector(NormalT2C, AbsoluteUp, -VectorGeometry.DegToRad(FOldMousePoint.X - X));
      ScaleVector(NormalT2C, Dist);
      NewPos := VectorAdd(AbsolutePosition, VectorSubtract(NormalT2C,
        OriginalT2C));
      if Assigned(Parent) then
        NewPos := Parent.AbsoluteToLocal(NewPos);
      Position.AsVector := NewPos;

      case GLLight.Position.Style of
        csPoint:
          GLLight.Position.SetPoint(NewPos);
        csVector:
          GLLight.Position.SetVector(NewPos);
      end;
    end;
    FOldMousePoint.X := X;
    FOldMousePoint.Y := Y;
  end
  else if ssRight in Shift then
  begin
    Zoom(Power(0.995, (FOldMousePoint.Y - Y)));
    FOldMousePoint.X := X;
    FOldMousePoint.Y := Y;
  end;
end;

procedure TFmHrtfConvolver.GLSceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
const
  Scale = 1 / 120;
begin
  Zoom(Power(0.9, WheelDelta * Scale));
  Handled := true
end;

procedure TFmHrtfConvolver.SEAzimuthChange(Sender: TObject);
begin
  if SEAzimuth.Value > 180 then
    SEAzimuth.Value := SEAzimuth.Value - 360
  else if SEAzimuth.Value < -180 then
    SEAzimuth.Value := SEAzimuth.Value + 360;

  with THrtfConvolverDataModule(Owner) do
  begin
    if Parameter[0] <> SEAzimuth.Value then
      Parameter[0] := SEAzimuth.Value;
  end;
end;

procedure TFmHrtfConvolver.SEElevationChange(Sender: TObject);
begin
  if SEElevation.Value > 90 then
    SEElevation.Value := SEElevation.Value - 180
  else if SEElevation.Value < -90 then
    SEElevation.Value := SEElevation.Value + 180;

  with THrtfConvolverDataModule(Owner) do
  begin
    if Parameter[1] <> SEElevation.Value then
      Parameter[1] := SEElevation.Value;
  end;
end;

procedure TFmHrtfConvolver.AzimuthChanged;
begin
  with THrtfConvolverDataModule(Owner) do
  begin
    if SEAzimuth.Value <> Parameter[0] then
      SEAzimuth.Value := Round(Parameter[0]);
    AudioDataDisplay.Invalidate;
  end;
end;

procedure TFmHrtfConvolver.ElevationChanged;
begin
  with THrtfConvolverDataModule(Owner) do
  begin
    if SEElevation.Value <> Parameter[1] then
      SEElevation.Value := Round(Parameter[1]);
    AudioDataDisplay.Invalidate;
  end;
end;

procedure TFmHrtfConvolver.RadiusChanged;
begin
  with THrtfConvolverDataModule(Owner) do
  begin
    if SERadius.Value <> Parameter[2] then
      SERadius.Value := Round(Parameter[2]);
    AudioDataDisplay.Invalidate;
  end;
end;

procedure TFmHrtfConvolver.Zoom(Value: Single);
var
  Vect: TVector;
begin
  if GLSceneViewer.Camera = GLCamera then
    with GLCamera do
      if Assigned(TargetObject) then
      begin
        Vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
        if ((VectorLength(Vect) > 1.2) or (Value > 1)) and
          ((VectorLength(Vect) < 10) or (Value < 1)) then
        begin
          ScaleVector(Vect, Value - 1);
          AddVector(Vect, AbsolutePosition);
          if Assigned(Parent) then
            Vect := Parent.AbsoluteToLocal(Vect);
          Position.AsVector := Vect;
        end;
      end
end;

end.
