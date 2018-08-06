unit HRTF3DGUI;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, 
  Forms, Controls, Dialogs, StdCtrls, DAV_Types, DAV_VSTModule, BaseClasses, 
  GLScene, GLObjects, GLTexture, GLFile3DS, GLWin32Viewer,
  GLVectorFileObjects, GLCoordinates, GLCrossPlatform;

type
  TVSTGUI = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLHead: TGLFreeForm;
    GLLight: TGLLightSource;
    GLHRTFs: TGLPoints;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FOldMousePoint : TPoint;
    procedure Zoom(Value: Single);
  public  
    procedure UpdateAzimuth;
    procedure UpdatePolar;
    procedure UpdateRadius;
  end;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  Math, VectorGeometry, MeshUtils, TGA, GLFileObj, VectorLists, HRTF3DModule,
  DAV_DspHrtf;

procedure TVSTGUI.FormCreate(Sender: TObject);
var
  rs             : TResourceStream;
  i              : Integer;
  tris, norms    : TAffineVectorList;
  tex, buf       : TAffineVectorList;
  morphTris      : TAffineVectorList;
  morphNorms     : TAffineVectorList;
  indices        : TIntegerList;
  texIndices     : TIntegerList;
  firstRemap     : TIntegerList;
  subdivideRemap : TIntegerList;
  bufRemap       : TIntegerList;
begin
 rs := TResourceStream.Create(hInstance, 'Head', '3DS');
 with rs do
  try
   GLHead.LoadFromStream('Head.3DS',rs);
   for i := 0 to GLHead.MeshObjects.Count-1 do
    begin
     tex := TAffineVectorList.Create;
     try
      with GLHead.MeshObjects[i]
       do tris := ExtractTriangles(tex);
      try
       indices := BuildVectorCountOptimizedIndices(tris);
       try
        firstRemap := TIntegerList(indices.CreateClone);
        RemapAndCleanupReferences(tris, indices);
        norms := BuildNormals(tris, indices);

        // subdivide geometry
        SubdivideTriangles(0.6, tris, indices, norms);
        texIndices := BuildVectorCountOptimizedIndices(tex);
        RemapAndCleanupReferences(tex, texIndices);

        // subdivide texture space
        SubdivideTriangles(0, tex, texIndices);

        // Re-expand everything
        buf := TAffineVectorList.Create;
        try
         ConvertIndexedListToList(tris, indices, buf);
         tris.Assign(buf);
         buf.Count := 0;
         ConvertIndexedListToList(norms, indices, buf);
         norms.Assign(buf);
         buf.Count := 0;
         ConvertIndexedListToList(tex, texIndices, buf);
         tex.Assign(buf);
        finally
         FreeAndNil(buf);
        end;

        // Pack & Optimize the expanded stuff
        FreeAndNil(indices);
        indices := BuildVectorCountOptimizedIndices(tris, norms, tex);
        subdivideRemap := TIntegerList(indices.CreateClone);
        RemapReferences(norms, indices);
        RemapReferences(tex, indices);
        RemapAndCleanupReferences(tris, indices);

        IncreaseCoherency(indices, 13);

        with GLHead.MeshObjects[i] do
         begin
          bufRemap := TIntegerList.Create;
          try
           morphTris := ExtractTriangles;
           try
            bufRemap.Assign(firstRemap);
            RemapAndCleanupReferences(morphTris, bufRemap);

            morphNorms := MeshUtils.BuildNormals(morphTris, bufRemap);
            try
             SubdivideTriangles(0.7, morphTris, bufRemap, morphNorms);
             buf := TAffineVectorList.Create;
             try
              ConvertIndexedListToList(morphTris, bufRemap, buf);
              morphTris.Assign(buf);
              ConvertIndexedListToList(morphNorms, bufRemap, buf);
              morphNorms.Assign(buf);
             finally
              FreeAndNil(buf);
             end;
             RemapReferences(morphTris, subdivideRemap);
             RemapReferences(morphNorms, subdivideRemap);
            finally
             FreeAndNil(morphNorms);
            end;
           finally
            FreeAndNil(morphTris);
           end;
          finally
           FreeAndNil(bufRemap);
          end;

          Vertices := tris;
          Normals := norms;
          TexCoords := tex;
          FaceGroups.Clear;
          with TFGVertexIndexList.CreateOwned(FaceGroups) do
           begin
            VertexIndices := indices;
            Mode := fgmmTriangles;
           end;
         end;
        FreeAndNil(texIndices);
        FreeAndNil(subdivideRemap);
        FreeAndNil(firstRemap);
        FreeAndNil(norms);
       finally
        FreeAndNil(indices);
       end;
      finally
       FreeAndNil(tris);
      end;
     finally
      FreeAndNil(tex);
     end;
    end;
   GLHead.StructureChanged;
  finally
   Free;
  end;
end;

procedure TVSTGUI.Zoom(Value: Single);
var
  vect : TVector;
begin
 if GLSceneViewer.Camera = GLCamera then
  with GLCamera do
   if Assigned(TargetObject) then
    begin
     vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
     if ((VectorLength(vect) > 1.2) or (Value > 1)) and
        ((VectorLength(vect) < 10)  or (Value < 1)) then
      begin
       ScaleVector(vect, Value - 1);
       AddVector(vect, AbsolutePosition);
       if Assigned(Parent)
        then vect := Parent.AbsoluteToLocal(vect);
       Position.AsVector := vect;
      end;
    end
end;

procedure TVSTGUI.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
const
  Scale = 1/120;
begin
 Zoom(Power(0.9, WheelDelta * Scale));
 Handled := true
end;

procedure TVSTGUI.FormShow(Sender: TObject);
var
  HrtfNr : Integer;
begin
 GLHRTFs.Visible := TVSTHRTF3DModule(Owner).Parameter[4] > 0.5;
 if GLHRTFs.Visible then
  begin
   GLHRTFs.Positions.Clear;
   with TVSTHRTF3DModule(Owner) do
    for HrtfNr := 0 to HRTFs.HrirCount - 1 do
     with HRTFs.Hrir[HrtfNr].Position do
      begin
       GLHRTFs.Positions.Add(cos(Azimuth) * sin(Polar + 0.25 * Pi), sin(Azimuth) * sin(Polar + 0.25 * Pi), cos(Polar + 0.25 * Pi));
  //     GLHRTFs.Positions.Add(cos(Polar) * sin(Azimuth), sin(Polar) * sin(Azimuth), cos(Azimuth));
      end;
   GLHRTFs.StructureChanged;
  end;
end;

procedure TVSTGUI.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  Scale = 1/40;
var
   originalT2C, normalT2C, normalCameraRight, newPos : TVector;
   pitchNow, dist: Single;
begin
 if ssLeft in Shift then
  begin
   with GLSceneViewer.Camera do
    begin
     originalT2C := VectorSubtract(AbsolutePosition, GLDummyCube.AbsolutePosition);
     SetVector(normalT2C, originalT2C);
     dist := VectorLength(normalT2C);
     NormalizeVector(normalT2C);
     normalCameraRight := VectorCrossProduct(AbsoluteUp, normalT2C);
     if VectorLength(normalCameraRight) < 0.001
      then SetVector(normalCameraRight, XVector) // arbitrary vector
      else NormalizeVector(normalCameraRight);
     pitchNow := Math.ArcCos(VectorDotProduct(AbsoluteUp, normalT2C));
     if not (ssAlt in Shift)
      then pitchNow := ClampValue(pitchNow + Math.DegToRad(FOldMousePoint.Y - Y), 0.002, PI - 0.77);
     SetVector(normalT2C, AbsoluteUp);
     RotateVector(normalT2C, normalCameraRight, -pitchNow);
     if not (ssShift in Shift)
      then RotateVector(normalT2C, AbsoluteUp, -Math.DegToRad(FOldMousePoint.X - X));
     ScaleVector(normalT2C, dist);
     newPos := VectorAdd(AbsolutePosition, VectorSubtract(normalT2C, originalT2C));
     if Assigned(Parent) then newPos := Parent.AbsoluteToLocal(newPos);
     Position.AsVector := newPos;
     GLLight.Position.SetVector(newPos);
     with TVSTHRTF3DModule(Self.Owner) do
      begin
       Parameter[0] := Math.RadToDeg(Math.ArcTan2(Position.Y, Position.X));
       Parameter[1] := 90 - Math.RadToDeg(pitchNow);
      end;
    end;
   FOldMousePoint.X := X;
   FOldMousePoint.Y := Y;
  end else
 if ssRight in Shift then
  begin
   Zoom(Power(0.995, (FOldMousePoint.Y - Y)));
   FOldMousePoint.X := X;
   FOldMousePoint.Y := Y;
  end;
end;

procedure TVSTGUI.UpdateAzimuth;
const
  Scale = 1/40;
var
   originalT2C, normalT2C, normalCameraRight, newPos : TVector;
   turnNow, pitchNow, dist: Single;
begin
 with GLSceneViewer.Camera do
  begin
   originalT2C := VectorSubtract(AbsolutePosition, GLDummyCube.AbsolutePosition);
   SetVector(normalT2C, originalT2C);
   dist := VectorLength(normalT2C);
   NormalizeVector(normalT2C);
   normalCameraRight := VectorCrossProduct(AbsoluteUp, normalT2C);
   if VectorLength(normalCameraRight) < 0.001
    then SetVector(normalCameraRight, XVector) // arbitrary vector
    else NormalizeVector(normalCameraRight);
   pitchNow := Math.ArcCos(VectorDotProduct(AbsoluteUp, normalT2C));
   SetVector(normalT2C, AbsoluteUp);
   RotateVector(normalT2C, normalCameraRight, -pitchNow);
   RotateVector(normalT2C, AbsoluteUp, Math.DegToRad(TVSTHRTF3DModule(Self.Owner).Parameter[0]));
   ScaleVector(normalT2C, dist);
   newPos := VectorAdd(AbsolutePosition, VectorSubtract(normalT2C, originalT2C));
   if Assigned(Parent)
    then newPos := Parent.AbsoluteToLocal(newPos);
//   Position.AsVector := newPos;
//   GLLight.Position.SetVector(newPos);
  end;
end;

procedure TVSTGUI.UpdatePolar;
begin

end;

procedure TVSTGUI.UpdateRadius;
begin

end;

procedure TVSTGUI.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FOldMousePoint.X := X;
 FOldMousePoint.Y := Y;
end;

end.
