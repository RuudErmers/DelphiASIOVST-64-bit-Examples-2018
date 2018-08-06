object FmHrtfConvolver: TFmHrtfConvolver
  Left = 261
  Top = 95
  BorderStyle = bsNone
  Caption = 'HRTF-Convolver'
  ClientHeight = 305
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PCConvolutionSelect: TPageControl
    Left = 0
    Top = 0
    Width = 590
    Height = 305
    ActivePage = TSReverb
    Align = alClient
    TabOrder = 0
    object TSHrtf: TTabSheet
      Caption = 'HRTF Convolution'
      DesignSize = (
        582
        277)
      object LbHrtfSet: TLabel
        Left = 11
        Top = 11
        Width = 49
        Height = 13
        Caption = 'HRTF Set:'
      end
      object EdHrtfSet: TEdit
        Left = 66
        Top = 11
        Width = 359
        Height = 13
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 0
        Text = 'internal'
      end
      object BtLoadHrtfFile: TButton
        Left = 431
        Top = 6
        Width = 148
        Height = 25
        Anchors = [akTop, akRight]
        Caption = '&Load HRTF File'
        TabOrder = 1
        OnClick = BtLoadHrtfFileClick
      end
      object GBPosition: TGroupBox
        Left = 11
        Top = 40
        Width = 326
        Height = 49
        Caption = 'Position'
        TabOrder = 2
        object LbRadius: TLabel
          Left = 221
          Top = 23
          Width = 36
          Height = 13
          Caption = 'Radius:'
        end
        object LbElevation: TLabel
          Left = 111
          Top = 23
          Width = 48
          Height = 13
          Caption = 'Elevation:'
        end
        object LbAzimuth: TLabel
          Left = 7
          Top = 23
          Width = 42
          Height = 13
          Caption = 'Azimuth:'
        end
        object SERadius: TSpinEdit
          Left = 263
          Top = 20
          Width = 50
          Height = 22
          MaxValue = 10
          MinValue = 1
          TabOrder = 0
          Value = 1
        end
        object SEElevation: TSpinEdit
          Left = 165
          Top = 20
          Width = 50
          Height = 22
          MaxValue = 90
          MinValue = -90
          TabOrder = 1
          Value = 0
          OnChange = SEElevationChange
        end
        object SEAzimuth: TSpinEdit
          Left = 55
          Top = 20
          Width = 50
          Height = 22
          MaxValue = 360
          MinValue = -360
          TabOrder = 2
          Value = 0
          OnChange = SEAzimuthChange
        end
      end
      object GbImpulseResponses: TGroupBox
        Left = 11
        Top = 95
        Width = 326
        Height = 178
        Caption = 'Impulse Responses'
        TabOrder = 3
        object AudioDataDisplay: TGuiAudioDataDisplay
          Left = 6
          Top = 17
          Width = 314
          Height = 155
          DisplayChannels = <
            item
              DisplayName = 'Channel 1'
              Color = clBlue
            end
            item
              DisplayName = 'Channel 2'
              Color = clRed
            end>
          LineWidth = 0
          Normalize = False
          XAxis.SampleUpper = 511
          XAxis.FractionalLower = -0.500000000000000000
          XAxis.FractionalUpper = 0.500000000000000000
        end
      end
      object Gb3D: TGroupBox
        Left = 343
        Top = 40
        Width = 236
        Height = 233
        Caption = '3D Representation'
        TabOrder = 4
        object GLSceneViewer: TGLSceneViewer
          Left = 6
          Top = 19
          Width = 224
          Height = 208
          Camera = GLCamera
          Buffer.FogEnvironment.FogStart = 10.000000000000000000
          Buffer.FogEnvironment.FogEnd = 10.000000000000000000
          Buffer.FogEnvironment.FogDistance = fdEyePlane
          Buffer.BackgroundColor = clBlack
          FieldOfView = 92.246604919433590000
          OnMouseDown = GLSceneViewerMouseDown
          OnMouseMove = GLSceneViewerMouseMove
          OnMouseWheel = GLSceneViewerMouseWheel
          TabOrder = 0
        end
      end
    end
    object TSReverb: TTabSheet
      Caption = 'Reverb Convolution'
      ImageIndex = 1
      object GbImpulseResponse: TGroupBox
        Left = 3
        Top = 80
        Width = 576
        Height = 194
        Caption = 'Impulse Responses'
        TabOrder = 0
        DesignSize = (
          576
          194)
        object AudioDataDisplayIR: TGuiAudioDataDisplay
          Left = 6
          Top = 17
          Width = 564
          Height = 171
          Anchors = [akLeft, akTop, akRight, akBottom]
          DisplayChannels = <
            item
              DisplayName = 'Channel 1'
              Color = clBlue
            end
            item
              DisplayName = 'Channel 2'
              Color = clRed
            end>
          LineWidth = 0
          Normalize = False
          XAxis.SampleUpper = 511
          XAxis.FractionalLower = -0.500000000000000000
          XAxis.FractionalUpper = 0.500000000000000000
        end
      end
    end
  end
  object GLScene: TGLScene
    Left = 384
    Top = 120
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
      Scale.Coordinates = {0000003F0000003F0000003F00000000}
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
  object OpenDialog: TOpenDialog
    DefaultExt = 'hrtf'
    Filter = 'HRTF files (*.HRTF)|*.hrtf'
    Title = 'Select an HRTF file'
    Left = 552
    Top = 40
  end
end
