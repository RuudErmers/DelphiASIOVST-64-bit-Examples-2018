unit DAV_AsioResourceStrings;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

resourcestring
  RStrAsioDriverFailed       = 'Asio driver failed!';
  RStrAsioNoBuffersCreated   = 'Asio buffers could not be created!';
  RStrConverterTypeUnknown   = 'Converter type unknown';
  RCStrIndexOutOfBounds      = 'Index out of bounds (%d)';
  RCStrOnlyOneAsioHost       = 'Only one Asio host is allowed per instance';
  RCStrPreferedBufferSize    = 'Prefered buffer size invalid!';
  RCStrDriverNotPresent      = 'Driver not present';
  RCStrHardwareMalfunction   = 'Hardware malfunctioning';
  RCStrInputParameterInvalid = 'Input parameter invalid';
  RCStrInvalidMode           = 'Hardware is in a bad mode or used in a bad mode';
  RCStrSPNotAdvancing        = 'Hardware is not running when sample position is inquired';
  RCStrNoClock               = 'Sample clock or rate cannot be determined or is not present';
  RCStrNoMemory              = 'Not enough memory for completing the request';
  RCStrWrongSamplerate       = 'Wrong Samplerate!';
  RCStrBufferAllocationError = 'Buffer allocation error!';

implementation

end.
