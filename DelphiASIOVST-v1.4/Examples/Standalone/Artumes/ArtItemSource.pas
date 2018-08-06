unit ArtItemSource;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010-2012        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

{$I Artumes.inc}

uses
  Classes, SysUtils, DAV_AudioData;

type
  TCustomSourceFile = class
  public
    constructor Create; virtual; abstract;
  end;

  TSourceFile = class(TCustomSourceFile)
  private
    FFileName : TFilename;
  public
    constructor Create; override;

    property FileName: TFilename read FFileName write FFileName;
  end;

  TSourceData = class(TCustomSourceFile)
  private
    FAudioData : TAudioData32;
  public
    constructor Create; override;

    property AudioData : TAudioData32 read FAudioData;
  end;

implementation

{ TSourceFile }

constructor TSourceFile.Create;
begin
 // nothing in here yet
end;


{ TSourceData }

constructor TSourceData.Create;
begin
 // nothing in here yet
end;

end.
