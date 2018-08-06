{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library Tetris;

{$MODE Delphi}

uses
  Interfaces,
  Forms,
  DVSTEffect,
  DVSTModule,
  VSTPluginLaz,
  TetrisUnit in 'TetrisUnit.pas',
  TetrisDM in 'TetrisDM.pas' {TetrisModule: TVSTModule},
  TetrisEditor in 'TetrisEditor.pas' {FmTetris};

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  TetrisModule: TTetrisModule;
begin
  try
    TetrisModule := TTetrisModule.Create(Application);
    TetrisModule.Effect^.user := TetrisModule;
    TetrisModule.AudioMaster := audioMaster;
    with TetrisModule do
    begin
      Result := Effect;
      Flags := [effFlagsHasEditor];
      Version := '1.0';
      EffectName := 'Tetris';
      ProductName := 'Tetris';
      VendorName := 'VST Example';
      VersionMajor := 1;
      VersionMinor := 0;
      VersionRelease := 0;
      PlugCategory := cgEffect;
      TailSize := 0;
      CanDos := [plugAsChannelInsert, plugAsSend, mixDryWet, _2in2out];
      SampleRate := 44100.0;
      CurrentProgram := -1;
      KeysRequired := True;
      UniqueID := 'Tetr';
      OnEditOpen := VSTModuleEditOpen;
      OnEditorKeyDown := VSTModuleEditorKeyDown;
      OnCheckKey := VSTModuleCheckKey;
    end;
  except
    Result := nil;
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
