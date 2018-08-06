object PluginDataModule: TPluginDataModule
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsProgramChunks]
  Version = '1.0'
  EffectName = 'Notepad'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Entry #1'
  KeysRequired = True
  IORatio = 1.000000000000000000
  UniqueID = 'Nopa'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Entry #1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Entry #2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Entry #3'
      VSTModule = Owner
    end>
  ParameterProperties = <>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnAfterProgramChange = VSTModuleAfterProgramChange
  OnProcess32Replacing = VSTModuleProcess32Replacing
  Left = 392
  Top = 88
  Height = 150
  Width = 215
end
