library ASIOVI;

{$MODE Delphi}

{%TogetherDiagram 'ModelSupport_ASIOVI\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ASIOVI\ASIOVI\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ASIOVI\ASIOVIObject\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ASIOVI\ASIOVIObject\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ASIOVI\DelphiASIO\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ASIOVI\ASIOVI\default.txvpck'}
{%TogetherDiagram 'ModelSupport_ASIOVI\Registry\default.txaPackage'}
{%TogetherDiagram 'ModelSupport_ASIOVI\default.txvpck'}

uses
  {FastMM4,}
  ASIOVIObject in 'ASIOVIObject.pas',
  DelphiASIO in 'DelphiASIO.pas',
  Registry in 'Registry.pas';

exports ASIOControlPanel;
exports ASIOGetNumDevices;
exports ASIOInitDriver;
exports ASIOSetDriverIndex;
exports ASIOGetDriverName;
exports ASIOGetDriverNames;
exports ASIOCanSampleRate;
exports ASIOSetSampleRate;
exports ASIODriverStart;
exports ASIODriverStop;
exports ASIOGetBufferSize;
exports ASIOGetChannels;
exports ASIOOutputType;
exports ASIOGetOutputLevel;
exports ASIOGetInputLevel;
exports ASIOSetOutputVolume;
exports ASIOSetOutputVolumedB;
exports ASIOSetSineFrequency;
exports ASIOReadWriteSize;
exports ASIOReadWriteSizeFixed;
exports ASIOReadWrite;
exports ASIOReadWriteX;
exports ASIOAutopilot;
exports ASIOSetExtraBufferSize;
exports ASIOBufferUnderrun;
exports ASIOResetBufferUnderruns;
exports ASIOGetLoopCounts;
exports ASIOSetLoopCounts;
exports ASIOSetClipFunction;
exports ASIOCalcMeters;

begin
end.

