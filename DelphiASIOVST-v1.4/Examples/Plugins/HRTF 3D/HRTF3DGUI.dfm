object VSTGUI: TVSTGUI
  Left = 386
  Top = 228
  BorderStyle = bsNone
  Caption = 'HRTF 3D'
  ClientHeight = 284
  ClientWidth = 307
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GLSceneViewer: TGLSceneViewer
    Left = 0
    Top = 0
    Width = 307
    Height = 284
    Camera = GLCamera
    Buffer.FogEnvironment.FogStart = 10.000000000000000000
    Buffer.FogEnvironment.FogEnd = 10.000000000000000000
    Buffer.FogEnvironment.FogDistance = fdEyePlane
    Buffer.BackgroundColor = clBlack
    FieldOfView = 109.691642761230500000
    Align = alClient
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
    OnMouseWheel = FormMouseWheel
    TabOrder = 0
  end
  object GLScene: TGLScene
    Left = 16
    Top = 16
    object GLDummyCube: TGLDummyCube
      CubeSize = 1.000000000000000000
      EdgeColor.Color = {B1A8A83EB1A8A83EB1A8A83E0000803F}
    end
    object GLHead: TGLFreeForm
      Material.BackProperties.Ambient.Color = {CDCC4C3DCDCC4C3DCDCC4C3D0000803F}
      Material.BackProperties.Diffuse.Color = {9A99193E9A99193E9A99193E0000803F}
      Material.BackProperties.Emission.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
      Material.FrontProperties.Ambient.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
      Material.FrontProperties.Emission.Color = {CDCCCC3DCDCCCC3DCDCCCC3D0000803F}
      Material.Texture.ImageClassName = 'TGLBlankImage'
      Material.Texture.Image.ColorFormat = 6408
      Material.FaceCulling = fcNoCull
      Direction.Coordinates = {0000000000000000FFFF7F3F00000000}
      Position.Coordinates = {0000000000000000CDCCCCBD0000803F}
      RollAngle = 90.000000000000000000
      Scale.Coordinates = {00000040000000400000004000000000}
      Up.Coordinates = {000080BF2EBD3BB30000000000000000}
    end
    object GLLight: TGLLightSource
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {83C02A3FFED4383F60E5703F0000803F}
      Position.Coordinates = {0000A04000000000000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLHRTFs: TGLPoints
      NoZWrite = False
      Static = False
      size = 3.000000000000000000
      Style = psSmooth
      PointParameters.Enabled = True
      PointParameters.PointParams = {00000000000000400000803F}
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 100.000000000000000000
      NearPlaneBias = 0.009999999776482582
      TargetObject = GLDummyCube
      Position.Coordinates = {0000A04000000000000000000000803F}
      Direction.Coordinates = {000000000000803F0000008000000000}
      Up.Coordinates = {00000000000000000000803F00000000}
    end
  end
end
