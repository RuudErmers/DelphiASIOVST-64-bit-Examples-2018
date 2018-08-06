object TrackPlugModule: TTrackPlugModule
  OldCreateOrder = False
  Version = '1.0'
  EffectName = 'TrackPlug'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'TrPl'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      Category = 'DC Filter'
      DisplayName = 'DC Filter Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 200.000000000000000000
      MaxInteger = 200
      Min = 0.019999999552965160
      ReportVST2Properties = True
      ShortLabel = 'DC Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterDCFilterChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'DC Filter'
      DisplayName = 'DC Filter Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'DC Ord.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterDcFilterChangeOrder
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 1'
      DisplayName = 'EQ Filter 1 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ1Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 1'
      DisplayName = 'EQ Filter 1 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ1Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 1'
      DisplayName = 'EQ Filter 1 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ1 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 1'
      DisplayName = 'EQ Filter 1 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ1Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 2'
      DisplayName = 'EQ Filter 2 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ2Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 2'
      DisplayName = 'EQ Filter 2 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ2Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 2'
      DisplayName = 'EQ Filter 2 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ2 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 2'
      DisplayName = 'EQ Filter 2 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ2Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 3'
      DisplayName = 'EQ Filter 3 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ3Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 3'
      DisplayName = 'EQ Filter 3 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ3Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 3'
      DisplayName = 'EQ Filter 3 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ3 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 3'
      DisplayName = 'EQ Filter 3 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ3Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 4'
      DisplayName = 'EQ Filter 4 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ4Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 4'
      DisplayName = 'EQ Filter 4 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ4Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 4'
      DisplayName = 'EQ Filter 4 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ4 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 4'
      DisplayName = 'EQ Filter 4 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ4Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 5'
      DisplayName = 'EQ Filter 5 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ5Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 5'
      DisplayName = 'EQ Filter 5 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ5Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 5'
      DisplayName = 'EQ Filter 5 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ5 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 5'
      DisplayName = 'EQ Filter 5 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ5Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 6'
      DisplayName = 'EQ Filter 6 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ6Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 6'
      DisplayName = 'EQ Filter 6 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ6Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 6'
      DisplayName = 'EQ Filter 6 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ6 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 6'
      DisplayName = 'EQ Filter 6 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ6Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 7'
      DisplayName = 'EQ Filter 7 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ7Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 7'
      DisplayName = 'EQ Filter 7 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ7Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 7'
      DisplayName = 'EQ Filter 7 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ7 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 7'
      DisplayName = 'EQ Filter 7 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ7Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 8'
      DisplayName = 'EQ Filter 8 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ8Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 8'
      DisplayName = 'EQ Filter 8 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ8Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 8'
      DisplayName = 'EQ Filter 8 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ8 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 8'
      DisplayName = 'EQ Filter 8 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ8Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'EQ Filter 9'
      DisplayName = 'EQ Filter 9 Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ9Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 9'
      DisplayName = 'EQ Filter 9 Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ5Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'EQ Filter 9'
      DisplayName = 'EQ Filter 9 Bandwidth'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ9 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'EQ Filter 9'
      DisplayName = 'EQ Filter 9 Type'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ9Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Gate'
      DisplayName = 'Gate Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterGateAttackChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Gate'
      DisplayName = 'Gate Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 5000
      Min = 0.050000000745058060
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterGateReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Gate'
      DisplayName = 'Gate Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGateThresholdChange
      OnCustomParameterDisplay = ParameterThresholdDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Gate'
      DisplayName = 'Gate Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 1
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterGateRatioChange
      OnCustomParameterDisplay = ParameterRatioDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Gate Knee'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Knee'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGateKneeChange
      OnCustomParameterDisplay = ParameterKneeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorAttackChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 5000
      Min = 0.050000000745058060
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorThresholdChange
      OnCustomParameterDisplay = ParameterThresholdDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.009999999776482582
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterCompressorRatioChange
      OnCustomParameterDisplay = ParameterRatioDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 Knee'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Knee'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorKneeChange
      OnCustomParameterDisplay = ParameterKneeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 MakeUp Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 60.000000000000000000
      MaxInteger = 60
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorMakeUpGainChange
      OnCustomParameterDisplay = ParameterMakeUpGainDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 Auto Make Up Gain'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Auto MU'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterCompressorAutoMakeUpGainChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 1'
      DisplayName = 'Compressor 1 Mix'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorMixChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 Attack'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorAttackChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 5000
      Min = 0.050000000745058060
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorThresholdChange
      OnCustomParameterDisplay = ParameterThresholdDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 Ratio'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.009999999776482582
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterCompressorRatioChange
      OnCustomParameterDisplay = ParameterRatioDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 Knee'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Knee'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorKneeChange
      OnCustomParameterDisplay = ParameterKneeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 MakeUp Gain'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 60.000000000000000000
      MaxInteger = 60
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorMakeUpGainChange
      OnCustomParameterDisplay = ParameterMakeUpGainDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 Auto Make Up Gain'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Auto MU'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterCompressorAutoMakeUpGainChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Compressor 2'
      DisplayName = 'Compressor 2 Mix'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorMixChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'DC Filter'
      VSTModule = Owner
    end
    item
      DisplayName = 'EQ Filter 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'EQ Filter 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'EQ Filter 3'
      VSTModule = Owner
    end
    item
      DisplayName = 'EQ Filter 4'
      VSTModule = Owner
    end
    item
      DisplayName = 'EQ Filter 5'
      VSTModule = Owner
    end
    item
      DisplayName = 'Gate'
      VSTModule = Owner
    end
    item
      DisplayName = 'Compressor 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Compressor 2'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Height = 150
  Width = 215
end
