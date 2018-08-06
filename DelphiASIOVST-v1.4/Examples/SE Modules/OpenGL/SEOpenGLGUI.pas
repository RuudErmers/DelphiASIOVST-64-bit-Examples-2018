unit SEOpenGLGUI;

interface

uses
  Windows, Classes, Controls, SysUtils, DAV_SEModule, DAV_SEGUI, SEOpenGLModule,
  GLScene, GLObjects, GLWin32Viewer, GLVectorFileObjects, GLFile3DS,
  GLFileB3D, GLFileGL2, GLFileGTS, GLFileLWO, GLFileMD2, GLFileMD3, GLFileMD5,
  GLFileMDC, GLFileMS3D, GLFileNMF, GLFileNurbs, GLFileObj, GLFileOCT, GLFilePLY,
  GLFileQ3BSP, GLFileSMD, GLFileSTL, GLFileTIN, GLFileVRML, GLStarRecord,
  GLVfsPAK;

type
  TSEOpenGLGui = class(TSEGUIBase)
  private
    function InvalidateControl: Integer;
    procedure SetFileName(const Value: TFileName);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    FFileName      : TFileName;
    FGLSceneViewer : TGLSceneViewer;
    FGLScene       : TGLScene;
    FOldPos        : TPoint;
    procedure GuiWindowOpen(WI: PSEWndInfo); override;
    procedure GuiWindowClose(WI: PSEWndInfo); override;

    procedure GuiPaint(hDC: HDC; wi: PSEWndInfo); override;
    procedure GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer); override;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); override;
//    function GuiIdle: Boolean; override;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer); override;
    destructor Destroy; override;
    function GetWindowHandle: THandle;
    property FileName: TFileName read FFileName write SetFileName;
  end;

implementation

uses
  Math, Graphics, DAV_Types, DAV_GuiBaseControl;

constructor TSEOpenGLGui.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
 inherited;
// FGuiDial.Parent
 CallHost(seGuiHostSetWindowSize, 64, 64);
 CallHost(seGuiHostSetWindowType, 1); // 0 = Draw on SE's window (default), 1 = HWND based

// CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE or HWF_NO_CUSTOM_GFX_ON_STRUCTURE));
 CallHost(seGuiHostSetWindowFlags, Integer(HWF_RESIZEABLE));
end;

destructor TSEOpenGLGui.Destroy;
begin
 inherited;
 if Assigned(FGLScene)
  then FreeAndNil(FGLScene);
 if Assigned(FGLSceneViewer)
  then FreeAndNil(FGLSceneViewer);
end;

procedure TSEOpenGLGui.GuiWindowOpen(WI: PSEWndInfo);
begin
 FGLSceneViewer := TGLSceneViewer.Create(nil);
 FGLScene := TGLScene.Create(FGLSceneViewer);
 FGLScene.Objects.AddChild(TGLFreeForm.Create(FGLScene));
 FGLScene.Cameras.AddChild(TGLCamera.Create(FGLScene));
 with TGLCamera(FGLScene.Cameras[0]) do
  begin
   TargetObject := FGLScene.Objects[0];
   Position.X := 4;
   Position.Y := 4;
   Position.Z := 4;
  end;
 FGLScene.Cameras[0].AddChild(TGLLightSource.Create(FGLScene));
 with FGLSceneViewer do
  begin
   Align := alClient;
   Width := Wi.Width;
   Height := Wi.Height;
   ParentWindow := CallHost(seGuiHostGetWindowHandle, WI.ContextHandle);
   Camera := TGLCamera(FGLScene.Cameras[0]);
   OnMouseWheel := GLSceneViewerMouseWheel;
   OnMouseDown := GLSceneViewerMouseDown;
   OnMouseMove := GLSceneViewerMouseMove;
  end;
 inherited;
end;

procedure TSEOpenGLGui.GuiWindowClose(WI: PSEWndInfo);
begin
 inherited;
 if Assigned(FGLScene)
  then FreeAndNil(FGLScene);
 if Assigned(FGLSceneViewer)
  then FreeAndNil(FGLSceneViewer);
end;

procedure TSEOpenGLGui.GuiPaint(hDC: HDC; wi :PSEWndInfo);
begin
 if Assigned(FGLSceneViewer) then
  with FGLSceneViewer do
   begin
    SetFocus;
    if Width <> wi.Width then Width := wi.Width;
    if Height <> wi.Height then Height := wi.Height;
   end;
end;

procedure TSEOpenGLGui.GuiPinValueChange(CurrentPin: TSEGuiPin);
begin
 inherited;
 case CurrentPin.PinIndex of
  0 : FileName := CurrentPin.ValueAsString;
 end;
end;

procedure TSEOpenGLGui.GuiModuleMsg(AUserMsgID, ALength: Integer; AData: Pointer);
begin
 InvalidateControl;
end;

function TSEOpenGLGui.GetWindowHandle: THandle;
begin
 Result := HWND(CallHost(seGuiHostGetWindowHandle, CallHost(seGuiHostGetHandle)));
end;

function TSEOpenGLGui.InvalidateControl: Integer;
begin
 Result := CallHost(seGuiHostRequestRepaint);
end;

procedure TSEOpenGLGui.SetFileName(const Value: TFileName);
begin
 if FFileName <> Value then
  begin
   FFileName := Value;
   if FileExists(FFileName)
    then TGLFreeForm(FGLScene.Objects[0]).LoadFromFile(FFileName)
    else TGLFreeForm(FGLScene.Objects[0]).MeshObjects.Clear;
  end;
end;

procedure TSEOpenGLGui.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FOldPos.X := X;
 FOldPos.Y := Y;
end;

procedure TSEOpenGLGui.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if ssLeft in Shift then
  begin
   if ssCtrl in Shift
    then TGLCamera(FGLScene.Cameras[0]).AdjustDistanceToTarget(Power(1.1, FOldPos.Y - Y))
    else TGLCamera(FGLScene.Cameras[0]).MoveAroundTarget(FOldPos.Y - y, FOldPos.X - X);
   FOldPos.X := X; FOldPos.Y := Y;
  end;
end;

procedure TSEOpenGLGui.GLSceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
 TGLCamera(FGLScene.Cameras[0]).AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

end.
