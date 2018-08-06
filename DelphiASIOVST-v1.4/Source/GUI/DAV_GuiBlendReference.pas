unit DAV_GuiBlendReference;

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
//  The look-up table code has been copied from the Graphics32 project which  //
//  is under the same license as this project.                                //
//  Please check the file GR32_Blend.pas at http://graphics32.org for         //
//  further copyright information!                                            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{-$DEFINE UseLookupTables}

uses
  SysUtils, DAV_GuiCommon, DAV_Bindings, DAV_GuiBlend;

function BlendPixelReference(Foreground, Background: TPixel32): TPixel32;
procedure BlendPixelInplaceReference(Foreground: TPixel32; var Background: TPixel32);
procedure BlendPixelLineReference(Foreground: TPixel32; Destination: PPixel32; Count: Cardinal);
procedure BlendLineReference(Source, Destination: PPixel32; Count: Integer);
function CombinePixelReference(ForeGround, Background: TPixel32; Weight: Cardinal): TPixel32;
procedure CombinePixelInplaceReference(ForeGround: TPixel32; var Background: TPixel32; Weight: Cardinal);
procedure CombineLineReference(Source, Destination: PPixel32; Count: Integer; Weight: Cardinal);
function MergePixelReference(Foreground, Background: TPixel32): TPixel32;
procedure MergePixelInplaceReference(Foreground: TPixel32; var Background: TPixel32);
procedure MergeLineReference(Source, Destination: PPixel32; Count: Cardinal);
procedure BindFunctions;

implementation

uses
  DAV_Common;

{$IFDEF UseLookupTables}
var
  GRcTable: array [Byte, Byte] of Byte;
  GDivTable: array [Byte, Byte] of Byte;
{$ENDIF}

const
  COne255th : Double = 1 / 255;

function BlendPixelReference(Foreground, Background: TPixel32): TPixel32;
{$IFDEF UseLookupTables}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
var
  Scale : array [0..1] of Double;
{$ENDIF}
begin
 if Foreground.A =   0 then
  begin
   Result := Background;
   Exit;
  end;

 if Foreground.A = $FF then
  begin
   Result := Foreground;
   Exit;
  end;

 with Background do
  begin
   {$IFDEF UseLookupTables}
   AlphaForeground := @GDivTable[Foreground.A];
   AlphaBackground := @GDivTable[not Foreground.A];
   R := AlphaForeground[Foreground.R] + AlphaBackground[R];
   G := AlphaForeground[Foreground.G] + AlphaBackground[G];
   B := AlphaForeground[Foreground.B] + AlphaBackground[B];
   {$ELSE}
   Scale[0] := Foreground.A * COne255th;
   Scale[1] := 1 - Scale[0];
   R := Limit(Round(Scale[1] * R + Scale[0] * Foreground.R), 0, $FF);
   G := Limit(Round(Scale[1] * G + Scale[0] * Foreground.G), 0, $FF);
   B := Limit(Round(Scale[1] * B + Scale[0] * Foreground.B), 0, $FF);
   {$ENDIF}
  end;
 Result := Background;
end;

procedure BlendPixelInplaceReference(Foreground: TPixel32; var Background: TPixel32);
{$IFDEF UseLookupTables}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
var
  Scale : array [0..1] of Double;
{$ENDIF}
begin
 if Foreground.A = 0 then Exit;

 if Foreground.A = $FF then
  begin
   Background := Foreground;
   Exit;
  end;

 with Background do
  begin
   {$IFDEF UseLookupTables}
   AlphaForeground := @GDivTable[Foreground.A];
   AlphaBackground := @GDivTable[not Foreground.A];
   R := AlphaForeground[Foreground.R] + AlphaBackground[R];
   G := AlphaForeground[Foreground.G] + AlphaBackground[G];
   B := AlphaForeground[Foreground.B] + AlphaBackground[B];
   {$ELSE}
   Scale[0] := Foreground.A * COne255th;
   Scale[1] := 1 - Scale[0];
   R := Limit(Round(Scale[1] * R + Scale[0] * Foreground.R), 0, $FF);
   G := Limit(Round(Scale[1] * G + Scale[0] * Foreground.G), 0, $FF);
   B := Limit(Round(Scale[1] * B + Scale[0] * Foreground.B), 0, $FF);
   {$ENDIF}
  end;
end;

procedure BlendPixelLineReference(Foreground: TPixel32; Destination: PPixel32; Count: Cardinal);
begin
 while Count > 0 do
  begin
   BlendPixelInplaceReference(Foreground, Destination^);
   Inc(Destination);
   Dec(Count);
  end;
end;

procedure BlendLineReference(Source, Destination: PPixel32; Count: Integer);
begin
 while Count > 0 do
  begin
   BlendPixelInplaceReference(Source^, Destination^);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
end;

function CombinePixelReference(ForeGround, Background: TPixel32; Weight: Cardinal): TPixel32;
{$IFDEF UseLookupTables}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
var
  Scale : array [0..1] of Double;
{$ENDIF}
begin
 if Weight = 0 then
  begin
   Result := Background;
   Exit;
  end;

 if Weight >= $FF then
  begin
   Result := ForeGround;
   Exit;
  end;

 with ForeGround do
  begin
   {$IFDEF UseLookupTables}
   AlphaForeground := @GDivTable[Weight];
   AlphaBackground := @GDivTable[255 - Weight];
   R := AlphaBackground[Background.R] + AlphaForeground[R];
   G := AlphaBackground[Background.G] + AlphaForeground[G];
   B := AlphaBackground[Background.B] + AlphaForeground[B];
   A := AlphaBackground[Background.A] + AlphaForeground[A];
   {$ELSE}
   Scale[0] := Weight * COne255th;
   Scale[1] := 1 - Scale[0];
   R := Limit(Round(Scale[1] * Background.R + Scale[0] * R), 0, $FF);
   G := Limit(Round(Scale[1] * Background.G + Scale[0] * G), 0, $FF);
   B := Limit(Round(Scale[1] * Background.B + Scale[0] * B), 0, $FF);
   A := Limit(Round(Scale[1] * Background.A + Scale[0] * A), 0, $FF);
   {$ENDIF}
  end;
 Result := ForeGround;
end;

procedure CombinePixelInplaceReference(ForeGround: TPixel32; var Background: TPixel32; Weight: Cardinal);
{$IFDEF UseLookupTables}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
{$ELSE}
var
  Scale : array [0..1] of Double;
{$ENDIF}
begin
 if Weight = 0
  then Exit;

 if Weight >= $FF then
  begin
   Background := ForeGround;
   Exit;
  end;

 with ForeGround do
  begin
   {$IFDEF UseLookupTables}
   AlphaForeground := @GDivTable[Weight];
   AlphaBackground := @GDivTable[255 - Weight];
   R := AlphaBackground[Background.R] + AlphaForeground[R];
   G := AlphaBackground[Background.G] + AlphaForeground[G];
   B := AlphaBackground[Background.B] + AlphaForeground[B];
   A := AlphaBackground[Background.A] + AlphaForeground[A];
   {$ELSE}
   Scale[0] := Weight * COne255th;
   Scale[1] := 1 - Scale[0];
   R := Limit(Round(Scale[1] * Background.R + Scale[0] * R), 0, $FF);
   G := Limit(Round(Scale[1] * Background.G + Scale[0] * G), 0, $FF);
   B := Limit(Round(Scale[1] * Background.B + Scale[0] * B), 0, $FF);
   A := Limit(Round(Scale[1] * Background.A + Scale[0] * A), 0, $FF);
   {$ENDIF}
  end;
 Background := ForeGround;
end;

procedure CombineLineReference(Source, Destination: PPixel32; Count: Integer;
  Weight: Cardinal);
begin
 while Count > 0 do
  begin
   CombinePixelInplace(Source^, Destination^, Weight);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
end;


function MergePixelReference(Foreground, Background: TPixel32): TPixel32;
{$IFDEF UseLookupTables}
var
  Fw, Bw : PByteArray;
  Wa     : Byte;
{$ELSE}
var
  Temp  : Double;
  Scale : Double;
{$ENDIF}
begin
 if Foreground.A = $FF then Result := Foreground else
 if Foreground.A = $0  then Result := Background else
 if Background.A = $0  then Result := Foreground else
 if Background.A = $FF
  then Result := BlendPixel(Foreground, Background)
  else
   begin
    {$IFDEF UseLookupTables}
    Result.A := GDivTable[Foreground.A xor 255, Background.A xor 255] xor 255;
    Wa := GRcTable[Result.A, Foreground.A];
    Fw := @GDivTable[Wa];
    Bw := @GDivTable[Wa xor $FF];
    Result.R := Fw[Foreground.R] + Bw[Background.R];
    Result.G := Fw[Foreground.G] + Bw[Background.G];
    Result.B := Fw[Foreground.B] + Bw[Background.B];
    {$ELSE}
    Temp := $FF - ($FF - Foreground.A) * (1 - Background.A * COne255th);
    Result.A := Round(Temp);
    Scale := Foreground.A / Temp;

    Result.R := Round(Background.R + Scale * (Foreground.R - Background.R));
    Result.G := Round(Background.G + Scale * (Foreground.G - Background.G));
    Result.B := Round(Background.B + Scale * (Foreground.B - Background.B));
    {$ENDIF}
   end;
end;

procedure MergePixelInplaceReference(Foreground: TPixel32; var Background: TPixel32);
begin
 Background := MergePixelReference(Foreground, Background);
end;

procedure MergeLineReference(Source, Destination: PPixel32; Count: Cardinal);
begin
 while Count > 0 do
  begin
   Destination^ := MergePixel(Source^, Destination^);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
end;


{ Global Functions }

{$IFDEF UseLookupTables}
procedure CreateTables;
var
  I, J : Integer;
begin
 for J := 0 to 255 do
  for I := 0 to 255 do
   begin
    GDivTable[I, J] := Round(I * J * COne255th);
    if I > 0
     then GRcTable[I, J] := Round(J * 255 / I)
     else GRcTable[I, J] := 0;
   end;
end;

procedure FreeTables;
begin
end;
{$ENDIF}

procedure BindFunctions;
begin
(*
 BindingBlendPixel.Add(@BlendPixelReference);
 BindingBlendPixelInplace.Add(@BlendPixelInplaceReference);
 BindingBlendLine.Add(@BlendLineReference);
 BindingCombinePixel.Add(@CombinePixelReference);
 BindingCombinePixelInplace.Add(@CombinePixelInplaceReference);
 BindingCombineLine.Add(@CombineLineReference);
 BindingMergePixel.Add(@MergePixelReference);
 BindingMergePixelInplace.Add(@MergePixelInplaceReference);
 BindingMergeLine.Add(@MergeLineReference);
*)
end;

{$IFDEF UseLookupTables}
initialization
  CreateTables;

finalization
  FreeTables;
{$ENDIF}

end.
