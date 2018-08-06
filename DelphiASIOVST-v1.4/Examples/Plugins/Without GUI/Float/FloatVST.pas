unit FloatVST;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes,
  Forms, DAV_Types, DAV_VSTModule, DAV_HalfFloat, DAV_MiniFloat;

type
  TFloatModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess32_8(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32_16(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess32_32(const Inputs, Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64_8(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64_16(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64_32(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure VSTModuleProcess64_64(const Inputs, Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
    procedure ParameterFloatBitsDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterFloatBitsChange(Sender: TObject; const Index: Integer; var Value: Single);
  end;

implementation

{$IFDEF FPC}
{$R *.LFM}
{$ELSE}
{$R *.DFM}
{$ENDIF}

procedure TFloatModule.VSTModuleOpen(Sender: TObject);
begin
  Parameter[0] := 2;

  with Programs[0] do
  begin
    Parameter[0] := 2;
  end;

  with Programs[1] do
  begin
    Parameter[0] := 0;
  end;

  with Programs[2] do
  begin
    Parameter[0] := 1;
  end;

  with Programs[3] do
  begin
    Parameter[0] := 2;
  end;
end;

procedure TFloatModule.ParameterFloatBitsChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Parameter[Index] - 0.499) of
   0 :
     begin
       OnProcess32Replacing := VSTModuleProcess32_8;
       OnProcess64Replacing := VSTModuleProcess64_8;
     end;
   1 :
     begin
       OnProcess32Replacing := VSTModuleProcess32_16;
       OnProcess64Replacing := VSTModuleProcess64_16;
     end;
   2 :
     begin
       OnProcess32Replacing := VSTModuleProcess32_32;
       OnProcess64Replacing := VSTModuleProcess64_32;
     end
  else
     begin
       OnProcess32Replacing := VSTModuleProcess32_32;
       OnProcess64Replacing := VSTModuleProcess64_64;
     end;
 end;
 OnProcess := OnProcess32Replacing;
end;

procedure TFloatModule.ParameterFloatBitsDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index] - 0.499) of
   0 : PreDefined := '8-Bit';
   1 : PreDefined := '16-Bit';
   2 : PreDefined := '32-Bit';
  else PreDefined := '64-Bit';
 end;
end;

procedure TFloatModule.VSTModuleProcess32_8(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  Value       : TMiniFloat;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := SingleToMiniFloat(Inputs[0, SampleIndex]);
    Outputs[0, SampleIndex] := MiniFloatToSingle(Value);
    Value := SingleToMiniFloat(Inputs[1, SampleIndex]);
    Outputs[1, SampleIndex] := MiniFloatToSingle(Value);
  end;
end;

procedure TFloatModule.VSTModuleProcess64_8(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  Value       : TMiniFloat;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := SingleToMiniFloat(Inputs[0, SampleIndex]);
    Outputs[0, SampleIndex] := MiniFloatToSingle(Value);
    Value := SingleToMiniFloat(Inputs[1, SampleIndex]);
    Outputs[1, SampleIndex] := MiniFloatToSingle(Value);
  end;
end;

procedure TFloatModule.VSTModuleProcess32_16(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  Value       : THalfFloat;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := SingleToHalfFloat(Inputs[0, SampleIndex]);
    Outputs[0, SampleIndex] := FastHalfFloatToSingle(Value);
    Value := SingleToHalfFloat(Inputs[1, SampleIndex]);
    Outputs[1, SampleIndex] := FastHalfFloatToSingle(Value);
  end;
end;

procedure TFloatModule.VSTModuleProcess64_16(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Outputs[0, SampleIndex] := FastHalfFloatToSingle(SingleToHalfFloat(
      Inputs[0, SampleIndex]));
    Outputs[1, SampleIndex] := FastHalfFloatToSingle(SingleToHalfFloat(
      Inputs[1, SampleIndex]));
  end;
end;

procedure TFloatModule.VSTModuleProcess32_32(const Inputs,
  Outputs: TDAVArrayOfSingleFixedArray; const SampleFrames: Cardinal);
begin
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TFloatModule.VSTModuleProcess64_32(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
var
  SampleIndex : Integer;
  Value       : Single;
begin
  for SampleIndex := 0 to SampleFrames - 1 do
  begin
    Value := Inputs[0, SampleIndex];
    Outputs[0, SampleIndex] := Value;
    Value := Inputs[1, SampleIndex];
    Outputs[1, SampleIndex] := Value;
  end;
end;

procedure TFloatModule.VSTModuleProcess64_64(const Inputs,
  Outputs: TDAVArrayOfDoubleFixedArray; const SampleFrames: Cardinal);
begin
  Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
  Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

end.
